/*
    KWin - the KDE window manager
    This file is part of the KDE project.

    SPDX-FileCopyrightText: 2015 Martin Gräßlin <mgraesslin@kde.org>
    SPDX-FileCopyrightText: 2025 Senior AMD Performance Engineer et al.

    SPDX-License-Identifier: GPL-2.0-or-later
*/
#include "drm_backend.h"

#include "config-kwin.h"

#include "backends/libinput/libinputbackend.h"
#include "core/outputconfiguration.h"
#include "core/session.h"
#include "drm_egl_backend.h"
#include "drm_gpu.h"
#include "drm_layer.h"
#include "drm_logging.h"
#include "drm_output.h"
#include "drm_pipeline.h"
#include "drm_qpainter_backend.h"
#include "drm_render_backend.h"
#include "drm_virtual_output.h"
#include "utils/envvar.h"
#include "utils/udev.h"

#include <KCoreAddons>
#include <KLocalizedString>
#include <QCoreApplication>
#include <QFileInfo>
#include <QSocketNotifier>
#include <QStringBuilder>

#include <algorithm>
#include <cerrno>
#include <chrono>
#include <ranges>
#include <sys/stat.h>
#include <thread>
#include <unistd.h>
#include <gbm.h>
#include <libdrm/drm_mode.h>
#include <xf86drm.h>

using namespace std::chrono_literals;

namespace KWin
{

// Optimized string splitter avoiding unnecessary temporary allocations
static QStringList splitPathList(const QString &input, const QChar delimiter)
{
    if (input.isEmpty()) {
        return {};
    }

    QStringList result;
    result.reserve(5);

    QString current;
    current.reserve(64);

    const auto *data = input.constData();
    const qsizetype size = input.size();

    for (qsizetype i = 0; i < size; ++i) {
        const QChar ch = data[i];
        if (ch == delimiter) {
            if (i > 0 && data[i - 1] == u'\\') {
                if (!current.isEmpty()) {
                    current[current.length() - 1] = delimiter;
                }
            } else {
                result.append(current);
                current.clear();
            }
        } else {
            current.append(ch);
        }
    }

    if (!current.isEmpty()) {
        result.append(current);
    }

    return result;
}

DrmBackend::DrmBackend(Session *session, QObject *parent)
    : OutputBackend(parent)
    , m_session(session)
    , m_explicitGpus(splitPathList(qEnvironmentVariable("KWIN_DRM_DEVICES"), u':'))
{
    m_outputs.reserve(8);
    m_publicOutputs.reserve(8);
    m_udev = std::make_unique<Udev>();
    m_udevMonitor = m_udev->monitor();
}

DrmBackend::~DrmBackend() = default;

Session *DrmBackend::session() const
{
    return m_session;
}

Outputs DrmBackend::outputs() const
{
    return m_publicOutputs;
}

void DrmBackend::updatePublicOutputs()
{
    m_publicOutputs.clear();
    m_publicOutputs.reserve(m_outputs.size());
    for (DrmAbstractOutput *output : std::as_const(m_outputs)) {
        m_publicOutputs.append(output);
    }
}

bool DrmBackend::initialize()
{
    connect(m_session, &Session::devicePaused, this, [this](dev_t deviceId) {
        if (const auto gpu = findGpu(deviceId)) {
            gpu->setActive(false);
        }
    });
    connect(m_session, &Session::deviceResumed, this, [this](dev_t deviceId) {
        if (const auto gpu = findGpu(deviceId); gpu && !gpu->isActive()) {
            gpu->setActive(true);
            updateOutputs();
        }
    });
    connect(m_session, &Session::awoke, this, [this]() {
        for (const auto &gpu : m_gpus) {
            if (gpu->atomicModeSetting()) {
                continue;
            }
            const auto outputs = gpu->drmOutputs();
            for (const auto &output : outputs) {
                output->pipeline()->forceLegacyModeset();
            }
        }
    });

    if (!m_explicitGpus.isEmpty()) {
        m_explicitGpuCanonicalPaths.reserve(m_explicitGpus.size());
        for (const QString &fileName : m_explicitGpus) {
            const QString canonical = QFileInfo(fileName).canonicalFilePath();
            if (!canonical.isEmpty()) {
                m_explicitGpuCanonicalPaths.push_back(canonical);
            }
            addGpu(fileName);
        }
    } else {
        const auto devices = m_udev->listGPUs();
        for (const auto &device : devices) {
            if (device->seat() == m_session->seat()) {
                addGpu(device->devNode());
            }
        }
    }

    if (m_gpus.empty()) {
        qCWarning(KWIN_DRM) << "No suitable DRM devices have been found";
        return false;
    }

    if (m_udevMonitor) {
        m_udevMonitor->filterSubsystemDevType("drm");
        if (const int fd = m_udevMonitor->fd(); fd != -1) {
            m_socketNotifier = std::make_unique<QSocketNotifier>(fd, QSocketNotifier::Read);
            connect(m_socketNotifier.get(), &QSocketNotifier::activated, this, &DrmBackend::handleUdevEvent);
            m_udevMonitor->enable();
        }
    }
    updateOutputs();

    if (m_explicitGpus.empty() && m_gpus.size() > 1) {
        // Optimized sort using clean lambda and ranges
        std::ranges::sort(m_gpus, [](const auto &a, const auto &b) {
            const auto &outputsA = a->drmOutputs();
            const auto &outputsB = b->drmOutputs();

            size_t internalCountA = 0;
            size_t desktopCountA = 0;
            for (const auto &out : outputsA) {
                if (out->isInternal()) ++internalCountA;
                if (!out->isNonDesktop()) ++desktopCountA;
            }

            size_t internalCountB = 0;
            size_t desktopCountB = 0;
            for (const auto &out : outputsB) {
                if (out->isInternal()) ++internalCountB;
                if (!out->isNonDesktop()) ++desktopCountB;
            }

            if (internalCountA != internalCountB) {
                return internalCountA > internalCountB;
            }
            if (desktopCountA != desktopCountB) {
                return desktopCountA > desktopCountB;
            }
            return outputsA.size() > outputsB.size();
        });

        qCDebug(KWIN_DRM) << "chose" << m_gpus.front()->drmDevice()->path() << "as the primary GPU";
    }
    return true;
}

void DrmBackend::handleUdevEvent()
{
    while (auto device = m_udevMonitor->getDevice()) {
        const auto action = device->action();

        // Fast reject invalid actions to avoid processing overhead
        const bool isAdd = (action == QLatin1StringView("add"));
        const bool isChange = (action == QLatin1StringView("change"));
        const bool isRemove = (action == QLatin1StringView("remove"));

        if (!isAdd && !isChange && !isRemove) {
            continue;
        }

        const dev_t devNum = device->devNum();
        DrmGpu *gpu = findGpu(devNum);

        // Optimized filtering: Minimize unnecessary stat/filesystem calls
        if (!m_explicitGpus.isEmpty()) {
            if (!gpu) {
                // If it's a remove for an unknown device, ignore immediately.
                if (isRemove) {
                    continue;
                }

                // Only verify paths for potential new devices (Add or Change-to-Add).
                // QFileInfo::canonicalFilePath implies stat(), which is costly.
                const QString canonicalPath = QFileInfo(device->devNode()).canonicalFilePath();
                const bool matches = std::ranges::any_of(m_explicitGpuCanonicalPaths, [&](const QString &p) {
                    return p == canonicalPath;
                });
                if (!matches) {
                    continue;
                }
            }
        } else {
            // Seat check is fast (integer comparison)
            if (device->seat() != m_session->seat()) {
                continue;
            }
        }

        if (isAdd) {
            if (gpu) {
                qCWarning(KWIN_DRM) << "Received unexpected add udev event for:" << device->devNode();
                continue;
            }
            if (addGpu(device->devNode())) {
                updateOutputs();
            }
        } else if (isRemove) {
            if (gpu) {
                if (primaryGpu() == gpu) {
                    qCCritical(KWIN_DRM) << "Primary gpu has been removed! Quitting...";
                    QCoreApplication::exit(1);
                    return;
                } else {
                    gpu->setRemoved();
                    updateOutputs();
                }
            }
        } else if (isChange) {
            if (gpu) {
                if (gpu->isActive()) {
                    qCDebug(KWIN_DRM) << "Received change event for monitored drm device" << gpu->drmDevice()->path();
                    updateOutputs();
                }
            } else {
                if (addGpu(device->devNode())) {
                    updateOutputs();
                }
            }
        }
    }
}

DrmGpu *DrmBackend::addGpu(const QString &fileName)
{
    auto fdResult = m_session->openRestricted(fileName);

    // Optimized retry loop: EBusy can occur if DRM master is not yet released.
    // 5s timeout with 20ms sleeps balances responsiveness and robustness.
    if (!fdResult.has_value() && fdResult.error() == Session::Error::EBusy) {
        auto start = std::chrono::steady_clock::now();
        while (!fdResult.has_value() && fdResult.error() == Session::Error::EBusy) {
            if (std::chrono::steady_clock::now() - start > 5s) {
                break;
            }
            qCDebug(KWIN_DRM, "Retrying openRestricted(%s)", qPrintable(fileName));
            std::this_thread::sleep_for(20ms);
            fdResult = m_session->openRestricted(fileName);
        }
    }

    if (!fdResult.has_value()) [[unlikely]] {
        qCWarning(KWIN_DRM) << "failed to open drm device at" << fileName;
        return nullptr;
    }

    int fd = *fdResult;

    if (!drmIsKMS(fd)) [[unlikely]] {
        qCDebug(KWIN_DRM) << "Skipping KMS incapable drm device node at" << fileName;
        m_session->closeRestricted(fd);
        return nullptr;
    }

    auto drmDevice = DrmDevice::openWithAuthentication(fileName, fd);
    if (!drmDevice) [[unlikely]] {
        m_session->closeRestricted(fd);
        return nullptr;
    }

    m_gpus.push_back(std::make_unique<DrmGpu>(this, fd, std::move(drmDevice)));
    auto gpu = m_gpus.back().get();
    qCDebug(KWIN_DRM) << "adding GPU" << fileName;
    connect(gpu, &DrmGpu::outputAdded, this, &DrmBackend::addOutput);
    connect(gpu, &DrmGpu::outputRemoved, this, &DrmBackend::removeOutput);
    if (m_renderBackend) {
        gpu->createLayers();
    }
    Q_EMIT gpuAdded(gpu);
    return gpu;
}

static QString earlyIdentifier(Output *output)
{
    if (output->edid().isValid()) {
        if (!output->edid().identifier().isEmpty()) {
            return output->edid().identifier();
        } else {
            return output->edid().hash();
        }
    } else {
        return output->name();
    }
}

void DrmBackend::addOutput(DrmAbstractOutput *o)
{
    const bool allOff = std::ranges::all_of(m_outputs, [](Output *output) {
        return !output->isEnabled() || output->dpmsMode() != Output::DpmsMode::On;
    });
    if (allOff) {
        const QString identifier = earlyIdentifier(o);
        if (m_recentlyUnpluggedDpmsOffOutputs.contains(identifier)) {
            if (auto *drmOutput = qobject_cast<DrmOutput *>(o)) {
                drmOutput->updateDpmsMode(Output::DpmsMode::Off);
                drmOutput->pipeline()->setActive(false);
                drmOutput->renderLoop()->inhibit();
                m_recentlyUnpluggedDpmsOffOutputs.removeOne(identifier);
            }
        }
    }
    m_outputs.append(o);
    updatePublicOutputs();
    Q_EMIT outputAdded(o);
}

static const int s_dpmsTimeout = environmentVariableIntValue("KWIN_DPMS_WORKAROUND_TIMEOUT").value_or(2000);

void DrmBackend::removeOutput(DrmAbstractOutput *o)
{
    if (o->dpmsMode() == Output::DpmsMode::Off) {
        const QString id = earlyIdentifier(o);
        m_recentlyUnpluggedDpmsOffOutputs.push_back(id);
        QTimer::singleShot(s_dpmsTimeout, this, [this, id]() {
            m_recentlyUnpluggedDpmsOffOutputs.removeOne(id);
        });
    }
    m_outputs.removeOne(o);
    updatePublicOutputs();
    Q_EMIT outputRemoved(o);
}

void DrmBackend::updateOutputs()
{
    bool changed = false;
    for (const auto &gpu : m_gpus) {
        if (gpu->isRemoved()) {
            gpu->removeOutputs();
            changed = true;
        } else {
            gpu->updateOutputs();
        }
    }

    Q_EMIT outputsQueried();

    const DrmGpu *primary = primaryGpu();
    for (auto it = m_gpus.begin(); it != m_gpus.end();) {
        DrmGpu *gpu = it->get();
        if (gpu->isRemoved() || (gpu != primary && gpu->drmOutputs().isEmpty())) {
            qCDebug(KWIN_DRM) << "Removing GPU" << gpu;
            const std::unique_ptr<DrmGpu> keepAlive = std::move(*it);
            it = m_gpus.erase(it);
            Q_EMIT gpuRemoved(keepAlive.get());
            changed = true;
        } else {
            ++it;
        }
    }

    if (changed) {
        updatePublicOutputs();
    }
}

std::unique_ptr<InputBackend> DrmBackend::createInputBackend()
{
    return std::make_unique<LibinputBackend>(m_session);
}

std::unique_ptr<QPainterBackend> DrmBackend::createQPainterBackend()
{
    return std::make_unique<DrmQPainterBackend>(this);
}

std::unique_ptr<EglBackend> DrmBackend::createOpenGLBackend()
{
    return std::make_unique<EglGbmBackend>(this);
}

QList<CompositingType> DrmBackend::supportedCompositors() const
{
    return {OpenGLCompositing, QPainterCompositing};
}

QString DrmBackend::supportInformation() const
{
    QString supportInfo;
    QDebug s(&supportInfo);
    s.nospace();
    s << "Name: DRM\n";
    for (size_t g = 0; g < m_gpus.size(); ++g) {
        s << "Atomic Mode Setting on GPU " << g << ": " << m_gpus.at(g)->atomicModeSetting() << '\n';
    }
    return supportInfo;
}

Output *DrmBackend::createVirtualOutput(const QString &name, const QString &description, const QSize &size, double scale)
{
    const auto ret = new DrmVirtualOutput(this, name, description, size, scale);
    m_virtualOutputs.push_back(ret);
    addOutput(ret);
    Q_EMIT outputsQueried();
    return ret;
}

void DrmBackend::removeVirtualOutput(Output *output)
{
    auto virtualOutput = qobject_cast<DrmVirtualOutput *>(output);
    Q_ASSERT(virtualOutput);
    if (!m_virtualOutputs.removeOne(virtualOutput)) {
        return;
    }
    removeOutput(virtualOutput);
    Q_EMIT outputsQueried();
    virtualOutput->unref();
}

DrmGpu *DrmBackend::primaryGpu() const
{
    return m_gpus.empty() ? nullptr : m_gpus.front().get();
}

DrmGpu *DrmBackend::findGpu(dev_t deviceId) const
{
    auto it = std::ranges::find_if(m_gpus, [deviceId](const auto &gpu) {
        return gpu->drmDevice()->deviceId() == deviceId;
    });
    return it == m_gpus.end() ? nullptr : it->get();
}

size_t DrmBackend::gpuCount() const
{
    return m_gpus.size();
}

OutputConfigurationError DrmBackend::applyOutputChanges(const OutputConfiguration &config)
{
    qCDebug(KWIN_DRM) << "Applying new output configuration...";

    std::vector<DrmOutput *> toBeEnabled;
    std::vector<DrmOutput *> toBeDisabled;
    toBeEnabled.reserve(16);
    toBeDisabled.reserve(16);

    for (const auto &gpu : m_gpus) {
        qCDebug(KWIN_DRM) << "Testing configuration for GPU:" << gpu->drmDevice()->path();
        for (DrmOutput *output : gpu->drmOutputs()) {
            if (output->isNonDesktop()) {
                continue;
            }
            if (const auto changeset = config.constChangeSet(output)) {
                output->queueChanges(changeset);
                if (changeset->enabled.value_or(output->isEnabled())) {
                    toBeEnabled.push_back(output);
                } else {
                    toBeDisabled.push_back(output);
                }
            }
        }

        const auto error = gpu->testPendingConfiguration();
        if (error != DrmPipeline::Error::None) {
            qCWarning(KWIN_DRM) << "Configuration test failed for GPU" << gpu->drmDevice()->path()
                                << "with error" << static_cast<int>(error);
            for (const auto &g : m_gpus) {
                for (DrmOutput *o : g->drmOutputs()) {
                    o->revertQueuedChanges();
                }
            }
            if (error == DrmPipeline::Error::NotEnoughCrtcs) {
                return OutputConfigurationError::TooManyEnabledOutputs;
            } else {
                return OutputConfigurationError::Unknown;
            }
        }
    }

    qCDebug(KWIN_DRM) << "All GPU configurations tested successfully. Applying changes...";

    for (DrmOutput *output : toBeEnabled) {
        if (const auto changeset = config.constChangeSet(output)) {
            output->applyQueuedChanges(changeset);
        }
    }
    for (DrmOutput *output : toBeDisabled) {
        if (const auto changeset = config.constChangeSet(output)) {
            output->applyQueuedChanges(changeset);
        }
    }

    for (const auto &gpu : m_gpus) {
        gpu->releaseUnusedBuffers();
    }
    for (DrmVirtualOutput *output : std::as_const(m_virtualOutputs)) {
        output->applyChanges(config);
    }

    qCDebug(KWIN_DRM) << "Output configuration applied.";
    return OutputConfigurationError::None;
}

void DrmBackend::setRenderBackend(DrmRenderBackend *backend)
{
    m_renderBackend = backend;
}

DrmRenderBackend *DrmBackend::renderBackend() const
{
    return m_renderBackend;
}

void DrmBackend::createLayers()
{
    for (const auto &gpu : m_gpus) {
        gpu->createLayers();
    }
    for (DrmVirtualOutput *virt : std::as_const(m_virtualOutputs)) {
        virt->recreateSurface();
    }
}

void DrmBackend::releaseBuffers()
{
    for (const auto &gpu : m_gpus) {
        gpu->releaseBuffers();
    }
    for (const DrmVirtualOutput *virt : std::as_const(m_virtualOutputs)) {
        virt->primaryLayer()->releaseBuffers();
    }
}

const std::vector<std::unique_ptr<DrmGpu>> &DrmBackend::gpus() const
{
    return m_gpus;
}

EglDisplay *DrmBackend::sceneEglDisplayObject() const
{
    Q_ASSERT(!m_gpus.empty());
    return m_gpus.front()->eglDisplay();
}
} // namespace KWin

#include "moc_drm_backend.cpp"
