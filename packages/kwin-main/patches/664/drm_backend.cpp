/*
    KWin - the KDE window manager
    This file is part of the KDE project.

    SPDX-FileCopyrightText: 2015 Martin Gräßlin <mgraesslin@kde.org>

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
// KF5
#include <KCoreAddons>
#include <KLocalizedString>
// Qt
#include <QCoreApplication>
#include <QElapsedTimer>
#include <QFileInfo>
#include <QSocketNotifier>
#include <QStringBuilder>
// system
#include <algorithm>
#include <cerrno>
#include <ranges>
#include <sys/stat.h>
#include <thread>
#include <unistd.h>
// drm
#include <gbm.h>
#include <libdrm/drm_mode.h>
#include <xf86drm.h>

using namespace std::chrono_literals;

namespace KWin
{

static QStringList splitPathList(const QString &input, const QChar delimiter)
{
    QStringList ret;
    if (input.isEmpty()) {
        return ret;
    }

    ret.reserve(input.count(delimiter) + 1);
    QString tmp;
    tmp.reserve(input.size());

    for (qsizetype i = 0; i < input.size(); ++i) {
        const QChar c = input[i];
        if (c == delimiter) {
            if (i > 0 && input[i - 1] == QLatin1Char('\\')) {
                tmp[tmp.size() - 1] = delimiter;
            } else if (!tmp.isEmpty()) {
                ret.append(tmp);
                tmp.clear();
            }
        } else {
            tmp.append(c);
        }
    }

    if (!tmp.isEmpty()) {
        ret.append(tmp);
    }
    return ret;
}

DrmBackend::DrmBackend(Session *session, QObject *parent)
    : OutputBackend(parent)
    , m_udev(std::make_unique<Udev>())
    , m_udevMonitor(m_udev->monitor())
    , m_session(session)
    , m_explicitGpus(splitPathList(qEnvironmentVariable("KWIN_DRM_DEVICES"), ':'))
{
}

DrmBackend::~DrmBackend() = default;

Session *DrmBackend::session() const
{
    return m_session;
}

QList<BackendOutput *> DrmBackend::outputs() const
{
    QList<BackendOutput *> result;
    result.reserve(m_outputs.size());
    for (DrmAbstractOutput *output : m_outputs) {
        result.append(output);
    }
    return result;
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
            // the output list might've changed while the device was inactive
            // note that this might delete the gpu!
            updateOutputs(gpu);
        }
    });
    connect(m_session, &Session::awoke, this, [this]() {
        // some drivers for old GPUs have problems after suspend, which
        // triggering a modeset works around.
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
        for (const QString &fileName : m_explicitGpus) {
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

    // setup udevMonitor
    if (m_udevMonitor) {
        m_udevMonitor->filterSubsystemDevType("drm");
        const int fd = m_udevMonitor->fd();
        if (fd != -1) {
            m_socketNotifier = std::make_unique<QSocketNotifier>(fd, QSocketNotifier::Read);
            connect(m_socketNotifier.get(), &QSocketNotifier::activated, this, &DrmBackend::handleUdevEvent);
            m_udevMonitor->enable();
        }
    }
    updateOutputs();   // default argument nullptr → update all GPUs on startup

    if (m_explicitGpus.empty() && m_gpus.size() > 1) {
        std::ranges::sort(m_gpus, [](const std::unique_ptr<DrmGpu> &gpu1, const std::unique_ptr<DrmGpu> &gpu2) {
            const auto outputs1 = gpu1->drmOutputs();
            const auto outputs2 = gpu2->drmOutputs();

            const size_t internalOutputs1 = std::ranges::count_if(outputs1, &BackendOutput::isInternal);
            const size_t internalOutputs2 = std::ranges::count_if(outputs2, &BackendOutput::isInternal);
            if (internalOutputs1 != internalOutputs2) {
                return internalOutputs1 > internalOutputs2;
            }

            const size_t desktopOutputs1 = std::ranges::count_if(outputs1, std::not_fn(&BackendOutput::isNonDesktop));
            const size_t desktopOutputs2 = std::ranges::count_if(outputs2, std::not_fn(&BackendOutput::isNonDesktop));
            if (desktopOutputs1 != desktopOutputs2) {
                return desktopOutputs1 > desktopOutputs2;
            }

            return outputs1.size() > outputs2.size();
        });
        qCDebug(KWIN_DRM) << "chose" << m_gpus.front()->drmDevice()->path() << "as the primary GPU";
    }
    return true;
}

void DrmBackend::handleUdevEvent()
{
    static constexpr QLatin1StringView kActionAdd("add");
    static constexpr QLatin1StringView kActionRemove("remove");
    static constexpr QLatin1StringView kActionChange("change");

    while (auto device = m_udevMonitor->getDevice()) {
        const QString devNode = device->devNode();
        const dev_t devNum = device->devNum();

        // Ignore the device seat if KWIN_DRM_DEVICES is set, and filter by explicit list instead.
        if (!m_explicitGpus.isEmpty()) {
            const QString canonicalPath = QFileInfo(devNode).canonicalFilePath();

            const bool foundMatch = std::ranges::any_of(m_explicitGpus, [&](const QString &explicitPath) {
                if (explicitPath == devNode) {
                    return true;
                }
                const QString explicitCanonical = QFileInfo(explicitPath).canonicalFilePath();
                return !canonicalPath.isEmpty() &&
                    !explicitCanonical.isEmpty() &&
                    explicitCanonical == canonicalPath;
            });

            if (!foundMatch) {
                continue;
            }
        } else if (device->seat() != m_session->seat()) {
            continue;
        }

        const QString action = device->action();

        if (action == kActionAdd) {
            if (DrmGpu *const gpu = findGpu(devNum)) {
                qCWarning(KWIN_DRM) << "Received unexpected add udev event for:" << devNode;
                continue;
            }
            if (DrmGpu *gpu = addGpu(devNode)) {
                updateOutputs(gpu);
            }
        } else if (action == kActionRemove) {
            if (DrmGpu *const gpu = findGpu(devNum)) {
                if (primaryGpu() == gpu) {
                    qCCritical(KWIN_DRM) << "Primary gpu has been removed! Quitting...";
                    QCoreApplication::exit(1);
                    return;
                }
                gpu->setRemoved();
                updateOutputs(gpu);
            }
        } else if (action == kActionChange) {
            DrmGpu *gpu = findGpu(devNum);
            if (!gpu) {
                gpu = addGpu(devNode);
            }
            if (gpu && gpu->isActive()) {
                qCDebug(KWIN_DRM) << "Received change event for monitored drm device" << gpu->drmDevice()->path();
                updateOutputs(gpu);
            }
        }
    }
}

DrmGpu *DrmBackend::addGpu(const QString &fileName)
{
    std::expected<int, Session::Error> fd = m_session->openRestricted(fileName);
    QElapsedTimer timer;
    timer.start();
    // Switching between sessions / drm masters seems to be racy in some situations.
    // Lacking a proper solution for that, retry opening the node for up to 5s.
    while (!fd.has_value() && fd.error() == Session::Error::EBusy && timer.durationElapsed() < 5s) {
        qCDebug(KWIN_DRM, "Retrying openRestricted(%s)", qPrintable(fileName));
        std::this_thread::sleep_for(100ms);
        fd = m_session->openRestricted(fileName);
    }
    if (!fd.has_value()) {
        qCWarning(KWIN_DRM, "Failed to open drm device %s", qPrintable(fileName));
        return nullptr;
    }

    if (!drmIsKMS(*fd)) {
        qCDebug(KWIN_DRM) << "Skipping KMS incapable drm device node at" << fileName;
        m_session->closeRestricted(*fd);
        return nullptr;
    }

    auto drmDevice = DrmDevice::openWithAuthentication(fileName, *fd);
    if (!drmDevice) {
        m_session->closeRestricted(*fd);
        return nullptr;
    }

    m_gpus.push_back(std::make_unique<DrmGpu>(this, *fd, std::move(drmDevice)));
    auto gpu = m_gpus.back().get();
    qCDebug(KWIN_DRM, "adding GPU %s", qPrintable(fileName));
    connect(gpu, &DrmGpu::outputAdded, this, &DrmBackend::addOutput);
    connect(gpu, &DrmGpu::outputRemoved, this, &DrmBackend::removeOutput);
    if (m_renderBackend) {
        gpu->createLayers();
    }
    Q_EMIT gpuAdded(gpu);
    return gpu;
}

void DrmBackend::addOutput(DrmAbstractOutput *o)
{
    m_outputs.append(o);
    Q_EMIT outputAdded(o);
}

void DrmBackend::removeOutput(DrmAbstractOutput *o)
{
    m_outputs.removeOne(o);
    Q_EMIT outputRemoved(o);
}

void DrmBackend::updateOutputs(DrmGpu *onlyUpdate)
{
    for (auto it = m_gpus.begin(); it != m_gpus.end(); ++it) {
        if ((*it)->isRemoved()) {
            (*it)->removeOutputs();
        } else if (!onlyUpdate || onlyUpdate == it->get()) {
            (*it)->updateOutputs();
        }
    }

    Q_EMIT outputsQueried();

    for (auto it = m_gpus.begin(); it != m_gpus.end();) {
        DrmGpu *gpu = it->get();
        if (gpu->isRemoved() || (gpu != primaryGpu() && gpu->drmOutputs().isEmpty())) {
            qCDebug(KWIN_DRM) << "Removing GPU" << it->get();
            const std::unique_ptr<DrmGpu> keepAlive = std::move(*it);
            it = m_gpus.erase(it);
            Q_EMIT gpuRemoved(keepAlive.get());
        } else {
            it++;
        }
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
    return QList<CompositingType>{OpenGLCompositing, QPainterCompositing};
}

QString DrmBackend::supportInformation() const
{
    QString supportInfo;
    QDebug s(&supportInfo);
    s.nospace();
    s << "Name: "
      << "DRM" << Qt::endl;
    for (size_t g = 0; g < m_gpus.size(); g++) {
        s << "Atomic Mode Setting on GPU " << g << ": " << m_gpus.at(g)->atomicModeSetting() << Qt::endl;
    }
    return supportInfo;
}

BackendOutput *DrmBackend::createVirtualOutput(const QString &name, const QString &description, const QSize &size, double scale)
{
    const auto ret = new DrmVirtualOutput(this, name, description, size, scale);
    m_virtualOutputs.push_back(ret);
    addOutput(ret);
    Q_EMIT outputsQueried();
    return ret;
}

void DrmBackend::removeVirtualOutput(BackendOutput *output)
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
    using ChangeSet = decltype(config.constChangeSet(static_cast<DrmOutput *>(nullptr)));
    struct PendingChange {
        DrmOutput *output;
        ChangeSet changeset;
    };

    QList<PendingChange> toBeEnabled;
    QList<PendingChange> toBeDisabled;

    for (const auto &gpu : m_gpus) {
        const auto outputs = gpu->drmOutputs();
        for (DrmOutput *output : outputs) {
            if (output->isNonDesktop()) {
                continue;
            }

            ChangeSet changeset = config.constChangeSet(output);
            if (!changeset) {
                continue;
            }

            output->queueChanges(changeset);
            if (changeset->enabled.value_or(output->isEnabled())) {
                toBeEnabled.push_back(PendingChange{output, std::move(changeset)});
            } else {
                toBeDisabled.push_back(PendingChange{output, std::move(changeset)});
            }
        }

        const DrmPipeline::Error error = gpu->testPendingConfiguration();
        if (error != DrmPipeline::Error::None) {
            for (const PendingChange &entry : std::as_const(toBeEnabled)) {
                entry.output->revertQueuedChanges();
            }
            for (const PendingChange &entry : std::as_const(toBeDisabled)) {
                entry.output->revertQueuedChanges();
            }

            if (error == DrmPipeline::Error::NotEnoughCrtcs) {
                return OutputConfigurationError::TooManyEnabledOutputs;
            }
            if (error == DrmPipeline::Error::Timeout) {
                return OutputConfigurationError::Timeout;
            }
            return OutputConfigurationError::Unknown;
        }
    }

    for (const PendingChange &entry : std::as_const(toBeEnabled)) {
        entry.output->applyQueuedChanges(entry.changeset);
    }
    for (const PendingChange &entry : std::as_const(toBeDisabled)) {
        entry.output->applyQueuedChanges(entry.changeset);
    }

    for (const auto &gpu : m_gpus) {
        gpu->releaseUnusedBuffers();
    }

    for (DrmVirtualOutput *output : std::as_const(m_virtualOutputs)) {
        output->applyChanges(config);
    }

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
    if (m_gpus.empty()) {
        return nullptr;
    }
    return m_gpus.front()->eglDisplay();
}
}

#include "moc_drm_backend.cpp"
