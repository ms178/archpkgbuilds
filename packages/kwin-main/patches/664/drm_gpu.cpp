/*
    KWin - the KDE window manager
    This file is part of the KDE project.

    SPDX-FileCopyrightText: 2020 Xaver Hugl <xaver.hugl@gmail.com>

    SPDX-License-Identifier: GPL-2.0-or-later
*/
#include "drm_gpu.h"

#include "config-kwin.h"

#include "core/gbmgraphicsbufferallocator.h"
#include "core/session.h"
#include "drm_backend.h"
#include "drm_buffer.h"
#include "drm_commit.h"
#include "drm_commit_thread.h"
#include "drm_connector.h"
#include "drm_crtc.h"
#include "drm_egl_backend.h"
#include "drm_layer.h"
#include "drm_logging.h"
#include "drm_output.h"
#include "drm_pipeline.h"
#include "drm_plane.h"
#include "drm_virtual_output.h"
#include "utils/envvar.h"

#include <QFile>
#include <QScopeGuard>
#include <QSet>
#include <algorithm>
#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <drm_fourcc.h>
#include <errno.h>
#include <fcntl.h>
#include <gbm.h>
#include <libdrm/drm_mode.h>
#include <poll.h>
#include <ranges>
#include <span>
#include <unistd.h>
#include <xf86drm.h>
#include <xf86drmMode.h>

#ifndef DRM_CLIENT_CAP_CURSOR_PLANE_HOTSPOT
#define DRM_CLIENT_CAP_CURSOR_PLANE_HOTSPOT 6
#endif
#ifndef DRM_CAP_ATOMIC_ASYNC_PAGE_FLIP
#define DRM_CAP_ATOMIC_ASYNC_PAGE_FLIP 0x15
#endif
#ifndef DRM_CLIENT_CAP_PLANE_COLOR_PIPELINE
#define DRM_CLIENT_CAP_PLANE_COLOR_PIPELINE 7
#endif

using namespace std::chrono_literals;

namespace KWin
{

static const std::optional<bool> s_modifiersEnv = environmentVariableBoolValue("KWIN_DRM_USE_MODIFIERS");
static const std::optional<bool> s_colorPipelineEnv = environmentVariableBoolValue("KWIN_DRM_USE_COLOR_PIPELINE");

DrmGpu::DrmGpu(DrmBackend *backend, int fd, std::unique_ptr<DrmDevice> &&device)
    : m_fd(fd)
    , m_drmDevice(std::move(device))
    , m_atomicModeSetting(false)
    , m_platform(backend)
{
    uint64_t capability = 0;

    if (drmGetCap(fd, DRM_CAP_CURSOR_WIDTH, &capability) == 0) {
        m_cursorSize.setWidth(static_cast<int>(capability));
    } else {
        m_cursorSize.setWidth(64);
    }

    if (drmGetCap(fd, DRM_CAP_CURSOR_HEIGHT, &capability) == 0) {
        m_cursorSize.setHeight(static_cast<int>(capability));
    } else {
        m_cursorSize.setHeight(64);
    }

    const int ret = drmGetCap(fd, DRM_CAP_TIMESTAMP_MONOTONIC, &capability);
    if (ret == 0 && capability == 1) {
        m_presentationClock = CLOCK_MONOTONIC;
    } else {
        m_presentationClock = CLOCK_REALTIME;
    }

    if (s_modifiersEnv.has_value() && *s_modifiersEnv == false) {
        qCDebug(KWIN_DRM, "modifier support disabled by environment variable");
    } else {
        m_addFB2ModifiersSupported = drmGetCap(fd, DRM_CAP_ADDFB2_MODIFIERS, &capability) == 0 && capability == 1;
        qCDebug(KWIN_DRM) << "drmModeAddFB2WithModifiers is" << (m_addFB2ModifiersSupported ? "supported" : "not supported") << "on GPU" << this;
    }

    DrmUniquePtr<drmVersion> version(drmGetVersion(fd));
    const char *driverName = (version && version->name) ? version->name : "";
    m_isI915 = std::strstr(driverName, "i915") != nullptr;
    m_isNVidia = std::strstr(driverName, "nvidia-drm") != nullptr;
    m_isAmdgpu = std::strstr(driverName, "amdgpu") != nullptr;
    m_isVmwgfx = std::strstr(driverName, "vmwgfx") != nullptr;
    m_isVirtualMachine = std::strstr(driverName, "virtio") != nullptr
        || std::strstr(driverName, "qxl") != nullptr
        || std::strstr(driverName, "vmwgfx") != nullptr
        || std::strstr(driverName, "vboxvideo") != nullptr;
    if (m_isNVidia) {
        QFile moduleVersion(QStringLiteral("/sys/module/nvidia_drm/version"));
        if (moduleVersion.open(QIODeviceBase::OpenModeFlag::ReadOnly)) {
            m_nvidiaDriverVersion = Version::parseString(moduleVersion.readLine(100));
        }
    }
    m_driverName = QString::fromLatin1(driverName);

    m_socketNotifier = std::make_unique<QSocketNotifier>(fd, QSocketNotifier::Read);
    connect(m_socketNotifier.get(), &QSocketNotifier::activated, this, &DrmGpu::dispatchEvents);

    initDrmResources();

    if (!m_atomicModeSetting) {
        m_asyncPageflipSupported = drmGetCap(fd, DRM_CAP_ASYNC_PAGE_FLIP, &capability) == 0 && capability == 1;
    } else {
        m_asyncPageflipSupported = drmGetCap(fd, DRM_CAP_ATOMIC_ASYNC_PAGE_FLIP, &capability) == 0 && capability == 1;
    }

    m_colorPipelineSupported = s_colorPipelineEnv.value_or(!m_isAmdgpu) && drmSetClientCap(fd, DRM_CLIENT_CAP_PLANE_COLOR_PIPELINE, 1) == 0;

    m_delayedModesetTimer.setInterval(0);
    m_delayedModesetTimer.setSingleShot(true);
    connect(&m_delayedModesetTimer, &QTimer::timeout, this, &DrmGpu::doModeset);
    m_sharpnessSupported = std::ranges::all_of(m_crtcs, [](const std::unique_ptr<DrmCrtc> &crtc) {
        return crtc->sharpnessStrength.isValid();
    });
}

DrmGpu::~DrmGpu()
{
    removeOutputs();
    m_planeLayerMap.clear();
    m_legacyLayerMap.clear();
    m_legacyCursorLayerMap.clear();
    m_pipelineMap.clear();
    m_crtcs.clear();
    m_connectors.clear();
    m_planes.clear();
    m_socketNotifier.reset();
    m_eglDisplay.reset();
    m_platform->session()->closeRestricted(m_fd);
}

FileDescriptor DrmGpu::createNonMasterFd() const
{
    char *path = drmGetDeviceNameFromFd2(m_fd);
    if (!path) {
        qCWarning(KWIN_DRM) << "Could not resolve DRM path for leasing!" << strerror(errno);
        return FileDescriptor{};
    }

    FileDescriptor fd{open(path, O_RDWR | O_CLOEXEC)};
    free(path);
    if (!fd.isValid()) {
        qCWarning(KWIN_DRM) << "Could not open DRM fd for leasing!" << strerror(errno);
    } else if (drmIsMaster(fd.get()) && drmDropMaster(fd.get()) != 0) {
        qCWarning(KWIN_DRM) << "Could not create a non-master DRM fd for leasing!" << strerror(errno);
        return FileDescriptor{};
    }
    return fd;
}

clockid_t DrmGpu::presentationClock() const
{
    return m_presentationClock;
}

void DrmGpu::initDrmResources()
{
    bool isEnvVarSet = false;
    bool noAMS = qEnvironmentVariableIntValue("KWIN_DRM_NO_AMS", &isEnvVarSet) != 0 && isEnvVarSet;
    const bool atomicSuccessful = drmSetClientCap(m_fd, DRM_CLIENT_CAP_ATOMIC, 1) == 0;
    if (noAMS) {
        qCWarning(KWIN_DRM) << "Atomic Mode Setting requested off via environment variable. Using legacy mode on GPU" << this;
    } else if (atomicSuccessful) {
        if (m_isVirtualMachine) {
            if (drmSetClientCap(m_fd, DRM_CLIENT_CAP_CURSOR_PLANE_HOTSPOT, 1) != 0) {
                qCWarning(KWIN_DRM, "Atomic Mode Setting disabled on GPU %s because of cursor offset issues in virtual machines", qPrintable(m_drmDevice->path()));
                drmSetClientCap(m_fd, DRM_CLIENT_CAP_ATOMIC, 0);
                noAMS = true;
            }
        }
        DrmUniquePtr<drmModePlaneRes> planeResources(drmModeGetPlaneResources(m_fd));
        if (planeResources && !noAMS) {
            qCDebug(KWIN_DRM) << "Using Atomic Mode Setting on gpu" << this;
            qCDebug(KWIN_DRM) << "Number of planes on GPU" << this << ":" << planeResources->count_planes;
            m_planes.reserve(planeResources->count_planes);
            for (uint32_t i = 0; i < planeResources->count_planes; ++i) {
                auto plane = std::make_unique<DrmPlane>(this, planeResources->planes[i]);
                if (plane->init()) {
                    m_allObjects << plane.get();
                    m_planes.push_back(std::move(plane));
                }
            }
            if (m_planes.empty()) {
                qCWarning(KWIN_DRM) << "Failed to create any plane. Falling back to legacy mode on GPU " << this;
            }
        } else {
            qCWarning(KWIN_DRM) << "Failed to get plane resources. Falling back to legacy mode on GPU " << this;
        }
    } else {
        qCWarning(KWIN_DRM) << "drmSetClientCap for Atomic Mode Setting failed. Using legacy mode on GPU" << this;
    }

    m_atomicModeSetting = !m_planes.empty();

    DrmUniquePtr<drmModeRes> resources(drmModeGetResources(m_fd));
    if (!resources) {
        qCCritical(KWIN_DRM) << "drmModeGetResources for getting CRTCs failed on GPU" << this;
        return;
    }
    m_crtcs.reserve(resources->count_crtcs);
    for (int i = 0; i < resources->count_crtcs; ++i) {
        auto freePrimaryPlanes = m_planes | std::views::filter([this, i](const auto &plane) {
            return plane->isCrtcSupported(i)
                && plane->type.enumValue() == DrmPlane::TypeIndex::Primary
                && std::ranges::none_of(m_crtcs, [&plane](const auto &crtc) {
                    return crtc->primaryPlane() == plane.get();
                });
        });
        const uint32_t crtcId = resources->crtcs[i];
        auto it = std::ranges::find_if(freePrimaryPlanes, [crtcId](const auto &plane) {
            return plane->crtcId.value() == crtcId;
        });
        if (it == freePrimaryPlanes.end()) {
            it = freePrimaryPlanes.begin();
        }
        DrmPlane *primary = it == freePrimaryPlanes.end() ? nullptr : it->get();
        if (m_atomicModeSetting && !primary) {
            qCWarning(KWIN_DRM) << "Could not find a suitable primary plane for crtc" << resources->crtcs[i];
            continue;
        }
        auto crtc = std::make_unique<DrmCrtc>(this, crtcId, i, primary);
        if (!crtc->init()) {
            continue;
        }
        m_allObjects << crtc.get();
        m_crtcs.push_back(std::move(crtc));
    }
}

bool DrmGpu::updateOutputs()
{
    if (!m_isActive) {
        return false;
    }
    DrmUniquePtr<drmModeRes> resources(drmModeGetResources(m_fd));
    if (!resources) {
        qCWarning(KWIN_DRM) << "drmModeGetResources failed:" << strerror(errno);
        return false;
    }

    if (DrmUniquePtr<drmModeLesseeListRes> lessees{drmModeListLessees(m_fd)}) {
        for (const DrmOutput *output : std::as_const(m_drmOutputs)) {
            if (output->lease()) {
                const bool leaseActive = std::ranges::any_of(std::span(lessees->lessees, lessees->count), [output](uint32_t id) {
                    return output->lease()->lesseeId() == id;
                });
                if (!leaseActive) {
                    Q_EMIT output->lease()->revokeRequested();
                }
            }
        }
    } else {
        qCWarning(KWIN_DRM) << "drmModeListLessees() failed:" << strerror(errno);
    }

    for (const auto &crtc : std::as_const(m_crtcs)) {
        crtc->updateProperties();
    }
    for (const auto &plane : std::as_const(m_planes)) {
        plane->updateProperties();
    }

    QSet<uint32_t> existingConnectorIds;
    existingConnectorIds.reserve(resources->count_connectors);
    for (int i = 0; i < resources->count_connectors; ++i) {
        const uint32_t currentConnector = resources->connectors[i];
        existingConnectorIds.insert(currentConnector);
        const auto it = std::ranges::find_if(m_connectors, [currentConnector](const auto &connector) {
            return connector->id() == currentConnector;
        });
        if (it == m_connectors.end()) {
            auto conn = std::make_shared<DrmConnector>(this, currentConnector);
            if (!conn->init()) {
                continue;
            }
            m_allObjects.push_back(conn.get());
            m_connectors.push_back(std::move(conn));
        } else {
            (*it)->updateProperties();
        }
    }

    for (auto it = m_connectors.begin(); it != m_connectors.end();) {
        DrmConnector *conn = it->get();
        const auto output = findOutput(conn->id());
        const bool stillExists = existingConnectorIds.contains(conn->id());
        if (!stillExists || !conn->isConnected()) {
            if (output) {
                removeOutput(output);
            }
        } else if (!output) {
            if (conn->modes().isEmpty()) {
                qCWarning(KWIN_DRM) << "Ignoring connector without modes:" << conn->connectorName();
                ++it;
                continue;
            }
            qCDebug(KWIN_DRM, "New %soutput on GPU %s: %s", conn->isNonDesktop() ? "non-desktop " : "", qPrintable(m_drmDevice->path()), qPrintable(conn->modelName()));
            auto &pipeline = m_pipelineMap[conn];
            pipeline = std::make_unique<DrmPipeline>(conn);
            m_pipelines.push_back(pipeline.get());
            auto newOutput = new DrmOutput(*it, pipeline.get());
            m_drmOutputs << newOutput;
            Q_EMIT outputAdded(newOutput);
            pipeline->setActive(true);
            pipeline->setEnable(false);
            pipeline->setMode(conn->modes().front());
            pipeline->applyPendingChanges();
        } else {
            output->updateConnectorProperties();
        }

        if (stillExists) {
            if (conn->isConnected() && conn->linkStatus.isValid() && conn->linkStatus.enumValue() == DrmConnector::LinkStatus::Bad) {
                qCWarning(KWIN_DRM, "Bad link status detected on connector %s", qPrintable(conn->connectorName()));
                m_forceModeset = true;
            }
            ++it;
        } else {
            m_allObjects.removeOne(it->get());
            it = m_connectors.erase(it);
        }
    }
    return true;
}

void DrmGpu::removeOutputs()
{
    const auto outputs = m_drmOutputs;
    for (DrmOutput *output : outputs) {
        removeOutput(output);
    }
}

DrmPipeline::Error DrmGpu::checkCrtcAssignment(QList<DrmConnector *> connectors, const QList<DrmCrtc *> &crtcs, std::chrono::steady_clock::time_point deadline)
{
    if (std::chrono::steady_clock::now() > deadline) {
        return DrmPipeline::Error::Timeout;
    }

    struct ActiveConnector {
        DrmConnector *connector;
        DrmPipeline *pipeline;
    };
    QList<ActiveConnector> activeConnectors;
    activeConnectors.reserve(connectors.size());
    for (DrmConnector *connector : std::as_const(connectors)) {
        const auto pipelineIt = m_pipelineMap.find(connector);
        if (pipelineIt == m_pipelineMap.end()) {
            continue;
        }
        DrmPipeline *pipeline = pipelineIt->second.get();
        if (!pipeline->enabled() || !connector->isConnected()) {
            pipeline->setCrtc(nullptr);
            continue;
        }
        activeConnectors.push_back({connector, pipeline});
    }

    if (activeConnectors.isEmpty()) {
        return testPipelines();
    }
    if (crtcs.isEmpty()) {
        return DrmPipeline::Error::NotEnoughCrtcs;
    }

    std::vector<bool> used(static_cast<size_t>(crtcs.size()), false);
    const auto isTerminalError = [](DrmPipeline::Error err) {
        return err == DrmPipeline::Error::None
            || err == DrmPipeline::Error::NoPermission
            || err == DrmPipeline::Error::FramePending
            || err == DrmPipeline::Error::Timeout;
    };

    const auto recurse = [&](auto &&self, qsizetype connectorIndex) -> DrmPipeline::Error {
        if (std::chrono::steady_clock::now() > deadline) {
            return DrmPipeline::Error::Timeout;
        }
        if (connectorIndex >= activeConnectors.size()) {
            return testPipelines();
        }

        const ActiveConnector active = activeConnectors[connectorIndex];
        DrmConnector *connector = active.connector;
        DrmPipeline *pipeline = active.pipeline;
        qsizetype preferredIndex = -1;

        if (m_atomicModeSetting) {
            const uint32_t currentId = connector->crtcId.value();
            for (qsizetype i = 0; i < crtcs.size(); ++i) {
                if (!used[static_cast<size_t>(i)] && crtcs[i]->id() == currentId) {
                    preferredIndex = i;
                    break;
                }
            }
        }

        if (preferredIndex >= 0) {
            DrmCrtc *crtc = crtcs[preferredIndex];
            if (connector->isCrtcSupported(crtc)) {
                used[static_cast<size_t>(preferredIndex)] = true;
                pipeline->setCrtc(crtc);
                const DrmPipeline::Error err = self(self, connectorIndex + 1);
                used[static_cast<size_t>(preferredIndex)] = false;
                if (isTerminalError(err)) {
                    return err;
                }
            }
        }

        for (qsizetype i = 0; i < crtcs.size(); ++i) {
            if (i == preferredIndex || used[static_cast<size_t>(i)]) {
                continue;
            }
            DrmCrtc *crtc = crtcs[i];
            if (!connector->isCrtcSupported(crtc)) {
                continue;
            }
            used[static_cast<size_t>(i)] = true;
            pipeline->setCrtc(crtc);
            const DrmPipeline::Error err = self(self, connectorIndex + 1);
            used[static_cast<size_t>(i)] = false;
            if (isTerminalError(err)) {
                return err;
            }
        }
        return DrmPipeline::Error::InvalidArguments;
    };

    return recurse(recurse, 0);
}

static const std::chrono::milliseconds s_checkCrtcTimeout = environmentVariableIntValue("KWIN_DRM_PENDING_CONFIG_TIMEOUT").transform([](int value) {
    return std::chrono::milliseconds(value);
}).value_or(3s);

DrmPipeline::Error DrmGpu::testPendingConfiguration()
{
    QList<DrmConnector *> connectors;
    connectors.reserve(static_cast<qsizetype>(m_connectors.size()));
    QList<DrmCrtc *> crtcs;
    crtcs.reserve(static_cast<qsizetype>(m_crtcs.size()));

    QSet<DrmConnector *> leasedConnectors;
    leasedConnectors.reserve(m_drmOutputs.size());
    QSet<DrmCrtc *> leasedCrtcs;
    leasedCrtcs.reserve(m_drmOutputs.size());
    bool hasPreferAccuracy = false;
    for (const DrmOutput *output : std::as_const(m_drmOutputs)) {
        hasPreferAccuracy |= output->colorPowerTradeoff() == BackendOutput::ColorPowerTradeoff::PreferAccuracy;
        if (!output->lease()) {
            continue;
        }
        leasedConnectors.insert(output->pipeline()->connector());
        if (DrmCrtc *leasedCrtc = output->pipeline()->crtc()) {
            leasedCrtcs.insert(leasedCrtc);
        }
    }

    for (const auto &conn : m_connectors) {
        if (!leasedConnectors.contains(conn.get())) {
            connectors.push_back(conn.get());
        }
    }
    for (const auto &crtc : m_crtcs) {
        if (!leasedCrtcs.contains(crtc.get())) {
            crtcs.push_back(crtc.get());
        }
    }
    if (m_atomicModeSetting) {
        std::sort(connectors.begin(), connectors.end(), [](auto c1, auto c2) {
            return c1->crtcId.value() > c2->crtcId.value();
        });
    }
    m_forceLowBandwidthMode = false;
    auto err = checkCrtcAssignment(connectors, crtcs, std::chrono::steady_clock::now() + s_checkCrtcTimeout);
    if (err == DrmPipeline::Error::None || err == DrmPipeline::Error::NoPermission || err == DrmPipeline::Error::FramePending) {
        return err;
    }
    if (m_addFB2ModifiersSupported || hasPreferAccuracy) {
        m_forceLowBandwidthMode = true;
        err = checkCrtcAssignment(connectors, crtcs, std::chrono::steady_clock::now() + s_checkCrtcTimeout);
    }
    return err;
}

void DrmGpu::releaseUnusedBuffers()
{
    QSet<DrmPipelineLayer *> usedLayers;
    usedLayers.reserve(static_cast<qsizetype>(m_planeLayerMap.size() + m_legacyLayerMap.size() + m_legacyCursorLayerMap.size()));
    for (DrmPipeline *pipeline : std::as_const(m_pipelines)) {
        const auto layers = pipeline->layers();
        for (DrmPipelineLayer *layer : layers) {
            usedLayers.insert(layer);
        }
    }

    for (const auto &[plane, layer] : m_planeLayerMap) {
        Q_UNUSED(plane);
        if (!usedLayers.contains(layer.get())) {
            layer->releaseBuffers();
        }
    }
    for (const auto &[crtc, layer] : m_legacyLayerMap) {
        Q_UNUSED(crtc);
        if (!usedLayers.contains(layer.get())) {
            layer->releaseBuffers();
        }
    }
    for (const auto &[crtc, layer] : m_legacyCursorLayerMap) {
        Q_UNUSED(crtc);
        if (!usedLayers.contains(layer.get())) {
            layer->releaseBuffers();
        }
    }
}

DrmPipeline::Error DrmGpu::testPipelines()
{
    if (m_pipelines.empty()) {
        return DrmPipeline::Error::None;
    }
    assignOutputLayers();
    for (DrmPipeline *pipeline : m_pipelines) {
        if (pipeline->output()->lease() || !pipeline->enabled()) {
            continue;
        }
        const auto layers = pipeline->layers();
        for (auto layer : layers) {
            if (layer->type() == OutputLayerType::Primary) {
                layer->setTargetRect(Rect(QPoint(0, 0), pipeline->mode()->size()));
                layer->setSourceRect(Rect(QPoint(0, 0), pipeline->mode()->size()));
                layer->setEnabled(true);
                if (!layer->preparePresentationTest()) {
                    return DrmPipeline::Error::InvalidArguments;
                }
            } else {
                layer->setEnabled(false);
            }
        }
    }
    return DrmPipeline::commitPipelines(m_pipelines, DrmPipeline::CommitMode::TestAllowModeset, unusedModesetObjects());
}

DrmOutput *DrmGpu::findOutput(quint32 connector)
{
    auto it = std::ranges::find_if(m_drmOutputs, [connector](DrmOutput *o) {
        return o->connector()->id() == connector;
    });
    if (it != m_drmOutputs.constEnd()) {
        return *it;
    }
    return nullptr;
}

bool DrmGpu::isIdle() const
{
    return std::ranges::none_of(m_pipelines, [](DrmPipeline *pipeline) {
        return pipeline->commitThread()->pageflipsPending();
    });
}

static std::chrono::nanoseconds convertTimestamp(const timespec &timestamp)
{
    return std::chrono::seconds(timestamp.tv_sec) + std::chrono::nanoseconds(timestamp.tv_nsec);
}

static std::chrono::nanoseconds convertTimestamp(clockid_t sourceClock, clockid_t targetClock, const timespec &timestamp)
{
    if (sourceClock == targetClock) {
        return convertTimestamp(timestamp);
    }

    timespec sourceCurrentTime = {};
    timespec targetCurrentTime = {};

    clock_gettime(sourceClock, &sourceCurrentTime);
    clock_gettime(targetClock, &targetCurrentTime);

    const auto delta = convertTimestamp(sourceCurrentTime) - convertTimestamp(timestamp);
    return convertTimestamp(targetCurrentTime) - delta;
}

void DrmGpu::pageFlipHandler(int fd, unsigned int sequence, unsigned int sec, unsigned int usec, unsigned int crtc_id, void *user_data)
{
    (void)fd;
    (void)sequence;
    (void)crtc_id;

    const auto commit = static_cast<DrmCommit *>(user_data);
    const auto gpu = commit->gpu();
    const bool defunct = std::erase_if(gpu->m_defunctCommits, [commit](const auto &defunctCommit) {
        return defunctCommit.get() == commit;
    }) != 0;
    if (defunct) {
        return;
    }

    std::chrono::nanoseconds timestamp = convertTimestamp(gpu->presentationClock(), CLOCK_MONOTONIC,
                                                          {static_cast<time_t>(sec), static_cast<long>(usec) * 1000L});
    if (timestamp == std::chrono::nanoseconds::zero()) {
        static uint64_t s_warningCounter = 0;
        ++s_warningCounter;
        if (s_warningCounter == 10) {
            qCDebug(KWIN_DRM, "Too many invalid timestamps received, suppressing future warnings");
        } else if (s_warningCounter < 10) {
            qCDebug(KWIN_DRM, "Got invalid timestamp (sec: %u, usec: %u) on gpu %s", sec, usec, qPrintable(gpu->drmDevice()->path()));
        }
        timestamp = std::chrono::steady_clock::now().time_since_epoch();
    }
    commit->pageFlipped(timestamp);
}

void DrmGpu::dispatchEvents()
{
    drmEventContext context = {};
    context.version = 3;
    context.page_flip_handler2 = pageFlipHandler;
    drmHandleEvent(m_fd, &context);
}

void DrmGpu::addDefunctCommit(std::unique_ptr<DrmCommit> &&commit)
{
    m_defunctCommits.push_back(std::move(commit));
}

void DrmGpu::removeOutput(DrmOutput *output)
{
    qCDebug(KWIN_DRM) << "Removing output" << output;
    m_drmOutputs.removeOne(output);
    Q_EMIT outputRemoved(output);
    m_pipelines.removeOne(output->pipeline());
    m_pipelineMap.erase(output->connector());
    output->removePipeline();
    output->unref();
    m_forceModeset = true;
}

DrmBackend *DrmGpu::platform() const
{
    return m_platform;
}

const QList<DrmPipeline *> DrmGpu::pipelines() const
{
    return m_pipelines;
}

std::unique_ptr<DrmLease> DrmGpu::leaseOutputs(const QList<DrmOutput *> &outputs)
{
    const bool alreadyLeased = std::ranges::any_of(outputs, [](DrmOutput *output) {
        return output->lease();
    });
    if (alreadyLeased) {
        return nullptr;
    }

    for (DrmOutput *output : outputs) {
        output->pipeline()->setEnable(true);
        output->pipeline()->setActive(false);
    }
    if (testPendingConfiguration() != DrmPipeline::Error::None) {
        return nullptr;
    }

    QList<uint32_t> objects;
    for (DrmOutput *output : outputs) {
        if (!output->addLeaseObjects(objects)) {
            return nullptr;
        }
    }

    uint32_t lesseeId = 0;
    FileDescriptor fd{drmModeCreateLease(m_fd, objects.constData(), objects.count(), 0, &lesseeId)};
    if (!fd.isValid()) {
        qCWarning(KWIN_DRM) << "Could not create DRM lease!" << strerror(errno);
        qCWarning(KWIN_DRM) << "Tried to lease the following" << objects.count() << "resources:";
        for (const uint32_t res : std::as_const(objects)) {
            qCWarning(KWIN_DRM) << res;
        }
        return nullptr;
    }

    qCDebug(KWIN_DRM) << "Created lease for" << objects.count() << "resources:";
    for (const uint32_t res : std::as_const(objects)) {
        qCDebug(KWIN_DRM) << res;
    }
    return std::make_unique<DrmLease>(this, std::move(fd), lesseeId, outputs);
}

QList<DrmOutput *> DrmGpu::drmOutputs() const
{
    return m_drmOutputs;
}

int DrmGpu::fd() const
{
    return m_fd;
}

DrmDevice *DrmGpu::drmDevice() const
{
    return m_drmDevice.get();
}

bool DrmGpu::atomicModeSetting() const
{
    return m_atomicModeSetting;
}

EglDisplay *DrmGpu::eglDisplay() const
{
    return m_eglDisplay.get();
}

void DrmGpu::setEglDisplay(std::unique_ptr<EglDisplay> &&display)
{
    m_eglDisplay = std::move(display);
}

bool DrmGpu::addFB2ModifiersSupported() const
{
    return m_addFB2ModifiersSupported;
}

bool DrmGpu::forceLowBandwidthMode() const
{
    return m_forceLowBandwidthMode;
}

bool DrmGpu::asyncPageflipSupported() const
{
    return m_asyncPageflipSupported;
}

bool DrmGpu::sharpnessSupported() const
{
    return m_sharpnessSupported;
}

bool DrmGpu::colorPipelineSupported() const
{
    return m_colorPipelineSupported;
}

bool DrmGpu::isI915() const
{
    return m_isI915;
}

bool DrmGpu::isNVidia() const
{
    return m_isNVidia;
}

bool DrmGpu::isAmdgpu() const
{
    return m_isAmdgpu;
}

bool DrmGpu::isVmwgfx() const
{
    return m_isVmwgfx;
}

bool DrmGpu::isVirtualMachine() const
{
    return m_isVirtualMachine;
}

std::optional<Version> DrmGpu::nvidiaDriverVersion() const
{
    return m_nvidiaDriverVersion;
}

bool DrmGpu::isRemoved() const
{
    return m_isRemoved;
}

void DrmGpu::setRemoved()
{
    m_isRemoved = true;
}

void DrmGpu::setActive(bool active)
{
    if (m_isActive != active) {
        m_isActive = active;
        if (active) {
            for (const DrmOutput *output : std::as_const(m_drmOutputs)) {
                output->renderLoop()->uninhibit();
            }
            for (const DrmOutput *output : std::as_const(m_drmOutputs)) {
                if (!atomicModeSetting()) {
                    output->pipeline()->forceLegacyModeset();
                }
            }
        } else {
            for (const DrmOutput *output : std::as_const(m_drmOutputs)) {
                output->renderLoop()->inhibit();
            }
        }
        Q_EMIT activeChanged(active);
    }
}

bool DrmGpu::isActive() const
{
    return m_isActive;
}

bool DrmGpu::needsModeset() const
{
    return m_forceModeset
        || !m_pendingModesetFrames.empty()
        || std::ranges::any_of(m_pipelines, [](DrmPipeline *pipeline) {
               return !pipeline->output()->lease() && pipeline->needsModeset();
           });
}

void DrmGpu::maybeModeset(DrmPipeline *pipeline, const std::shared_ptr<OutputFrame> &frame)
{
    if (pipeline && frame) {
        m_pendingModesetFrames.emplace(pipeline, frame);
    }

    QList<DrmPipeline *> pipelines;
    pipelines.reserve(m_pipelines.size());
    for (DrmPipeline *candidate : std::as_const(m_pipelines)) {
        if (!candidate->output()->lease()) {
            pipelines.push_back(candidate);
        }
    }

    const bool presentPendingForAll = std::ranges::all_of(pipelines, [](const DrmPipeline *candidate) {
        return candidate->modesetPresentPending() || !candidate->activePending();
    });
    if (!presentPendingForAll || !isIdle() || m_inModeset) {
        return;
    }
    m_delayedModesetTimer.start();
}

void DrmGpu::doModeset()
{
    QList<DrmPipeline *> pipelines;
    pipelines.reserve(m_pipelines.size());
    for (DrmPipeline *candidate : std::as_const(m_pipelines)) {
        if (!candidate->output()->lease()) {
            pipelines.push_back(candidate);
        }
    }

    if (pipelines.empty()) {
        m_pendingModesetFrames.clear();
        m_forceModeset = false;
        return;
    }

    m_inModeset = true;
    const DrmPipeline::Error err = DrmPipeline::commitPipelines(pipelines, DrmPipeline::CommitMode::CommitModeset, unusedModesetObjects());
    for (DrmPipeline *activePipeline : std::as_const(pipelines)) {
        if (activePipeline->modesetPresentPending()) {
            activePipeline->resetModesetPresentPending();
        }
    }
    m_forceModeset = false;
    if (err == DrmPipeline::Error::None) {
        for (const auto &[pendingPipeline, pendingFrame] : m_pendingModesetFrames) {
            (void)pendingPipeline;
            pendingFrame->presented(std::chrono::steady_clock::now().time_since_epoch(), PresentationMode::VSync);
        }
    } else if (err != DrmPipeline::Error::FramePending) {
        QTimer::singleShot(0, m_platform, &DrmBackend::updateOutputs);
    }
    m_pendingModesetFrames.clear();
    m_inModeset = false;
}

QList<DrmObject *> DrmGpu::unusedModesetObjects() const
{
    QList<DrmObject *> ret = m_allObjects;
    for (const DrmPipeline *pipeline : m_pipelines) {
        ret.removeOne(pipeline->connector());
        if (pipeline->crtc()) {
            ret.removeOne(pipeline->crtc());
            ret.removeOne(pipeline->crtc()->primaryPlane());
        }
    }
    return ret;
}

QSize DrmGpu::cursorSize() const
{
    return m_cursorSize;
}

void DrmGpu::releaseBuffers()
{
    for (DrmPipeline *pipeline : std::as_const(m_pipelines)) {
        pipeline->setLayers({});
        pipeline->applyPendingChanges();
    }
    for (const auto &plane : std::as_const(m_planes)) {
        plane->releaseCurrentBuffer();
        m_planeLayerMap.erase(plane.get());
    }
    for (const auto &crtc : std::as_const(m_crtcs)) {
        crtc->releaseCurrentBuffer();
        m_legacyLayerMap.erase(crtc.get());
        m_legacyCursorLayerMap.erase(crtc.get());
    }
}

void DrmGpu::createLayers()
{
    if (m_atomicModeSetting) {
        m_planeLayerMap.reserve(m_planes.size());
        for (const auto &plane : m_planes) {
            m_planeLayerMap[plane.get()] = m_platform->renderBackend()->createDrmPlaneLayer(plane.get());
        }
    } else {
        m_legacyLayerMap.reserve(m_crtcs.size());
        m_legacyCursorLayerMap.reserve(m_crtcs.size());
        for (const auto &crtc : m_crtcs) {
            m_legacyLayerMap[crtc.get()] = m_platform->renderBackend()->createDrmPlaneLayer(this, DrmPlane::TypeIndex::Primary);
            m_legacyCursorLayerMap[crtc.get()] = m_platform->renderBackend()->createDrmPlaneLayer(this, DrmPlane::TypeIndex::Cursor);
        }
    }
    assignOutputLayers();
    for (DrmPipeline *pipeline : std::as_const(m_pipelines)) {
        pipeline->applyPendingChanges();
    }
}

void DrmGpu::assignOutputLayers()
{
    if (m_atomicModeSetting) {
        QList<DrmPlane *> freePlanes;
        freePlanes.reserve(static_cast<qsizetype>(m_planes.size()));
        for (const auto &plane : m_planes) {
            freePlanes.push_back(plane.get());
        }
        const size_t enabledPipelinesCount = std::ranges::count_if(m_pipelines, &DrmPipeline::enabled);

        for (DrmPipeline *pipeline : std::as_const(m_pipelines)) {
            if (!pipeline->enabled()) {
                pipeline->setLayers({});
                continue;
            }

            DrmCrtc *const crtc = pipeline->crtc();
            if (!crtc) {
                pipeline->setLayers({});
                continue;
            }

            QList<DrmPipelineLayer *> layers;
            layers.reserve(static_cast<qsizetype>(2 + freePlanes.size()));
            const auto primaryIt = m_planeLayerMap.find(crtc->primaryPlane());
            if (primaryIt == m_planeLayerMap.end()) {
                pipeline->setLayers({});
                continue;
            }
            layers.push_back(primaryIt->second.get());
            const auto pipeIndex = crtc->pipeIndex();

            for (qsizetype i = 0; i < freePlanes.size(); ++i) {
                DrmPlane *plane = freePlanes[i];
                if (plane->isCrtcSupported(pipeIndex)
                    && plane->type.enumValue() == DrmPlane::TypeIndex::Cursor) {
                    const auto cursorIt = m_planeLayerMap.find(plane);
                    if (cursorIt != m_planeLayerMap.end()) {
                        layers.push_back(cursorIt->second.get());
                        freePlanes.removeAt(i);
                    }
                    break;
                }
            }

            if (enabledPipelinesCount == 1) {
                for (DrmPlane *plane : std::as_const(freePlanes)) {
                    if (plane->isCrtcSupported(pipeIndex)
                        && plane->type.enumValue() == DrmPlane::TypeIndex::Overlay) {
                        const auto overlayIt = m_planeLayerMap.find(plane);
                        if (overlayIt != m_planeLayerMap.end()) {
                            layers.push_back(overlayIt->second.get());
                        }
                    }
                }
            }
            pipeline->setLayers(layers);
        }
    } else {
        for (DrmPipeline *pipeline : std::as_const(m_pipelines)) {
            if (!pipeline->enabled()) {
                pipeline->setLayers({});
                continue;
            }
            DrmCrtc *const crtc = pipeline->crtc();
            if (!crtc) {
                pipeline->setLayers({});
                continue;
            }
            const auto primaryIt = m_legacyLayerMap.find(crtc);
            const auto cursorIt = m_legacyCursorLayerMap.find(crtc);
            if (primaryIt == m_legacyLayerMap.end() || cursorIt == m_legacyCursorLayerMap.end()) {
                pipeline->setLayers({});
                continue;
            }
            pipeline->setLayers({primaryIt->second.get(), cursorIt->second.get()});
        }
    }
}

std::shared_ptr<DrmFramebuffer> DrmGpu::importBuffer(GraphicsBuffer *buffer, FileDescriptor &&readFence)
{
    const DmaBufAttributes *attributes = buffer->dmabufAttributes();
    if (Q_UNLIKELY(!attributes)) {
        return nullptr;
    }
    if (attributes->planeCount <= 0 || attributes->planeCount > 4) {
        qCWarning(KWIN_DRM) << "Invalid plane count" << attributes->planeCount;
        return nullptr;
    }

    const auto it = m_fbCache.constFind(buffer);
    if (it != m_fbCache.constEnd()) {
        return std::make_shared<DrmFramebuffer>(it->lock(), buffer, std::move(readFence));
    }

    uint32_t handles[] = {0, 0, 0, 0};
    [[maybe_unused]] const auto cleanup = qScopeGuard([this, &handles]() {
        for (int i = 0; i < 4; ++i) {
            if (handles[i] == 0) {
                continue;
            }
            bool closed = false;
            for (int j = 0; j < i; ++j) {
                if (handles[i] == handles[j]) {
                    closed = true;
                    break;
                }
            }
            if (!closed) {
                drmCloseBufferHandle(m_fd, handles[i]);
            }
        }
    });

    for (int i = 0; i < attributes->planeCount; ++i) {
        if (drmPrimeFDToHandle(m_fd, attributes->fd[i].get(), &handles[i]) != 0) {
            qCWarning(KWIN_DRM) << "drmPrimeFDToHandle() failed";
            return nullptr;
        }
    }

    uint32_t framebufferId = 0;
    int ret = 0;
    if (addFB2ModifiersSupported() && attributes->modifier != DRM_FORMAT_MOD_INVALID) {
        uint64_t modifier[4] = {0, 0, 0, 0};
        for (int i = 0; i < attributes->planeCount; ++i) {
            modifier[i] = attributes->modifier;
        }
        ret = drmModeAddFB2WithModifiers(m_fd,
                                         attributes->width,
                                         attributes->height,
                                         attributes->format,
                                         handles,
                                         attributes->pitch.data(),
                                         attributes->offset.data(),
                                         modifier,
                                         &framebufferId,
                                         DRM_MODE_FB_MODIFIERS);
    } else {
        ret = drmModeAddFB2(m_fd,
                            attributes->width,
                            attributes->height,
                            attributes->format,
                            handles,
                            attributes->pitch.data(),
                            attributes->offset.data(),
                            &framebufferId,
                            0);
        if (ret == -EOPNOTSUPP && attributes->planeCount == 1) {
            ret = drmModeAddFB(m_fd,
                               attributes->width,
                               attributes->height,
                               24,
                               32,
                               attributes->pitch[0],
                               handles[0],
                               &framebufferId);
        }
    }

    if (ret != 0) {
        return nullptr;
    }

    auto fbData = std::make_shared<DrmFramebufferData>(this, framebufferId, buffer);
    m_fbCache[buffer] = fbData;
    connect(buffer, &GraphicsBuffer::destroyed, this, &DrmGpu::forgetBufferObject);
    return std::make_shared<DrmFramebuffer>(fbData, buffer, std::move(readFence));
}

void DrmGpu::forgetBuffer(GraphicsBuffer *buf)
{
    disconnect(buf, &GraphicsBuffer::destroyed, this, &DrmGpu::forgetBufferObject);
    m_fbCache.remove(buf);
}

void DrmGpu::forgetBufferObject(QObject *buf)
{
    m_fbCache.remove(static_cast<GraphicsBuffer *>(buf));
}

QString DrmGpu::driverName() const
{
    return m_driverName;
}

QList<OutputLayer *> DrmGpu::compatibleOutputLayers(BackendOutput *output) const
{
    if (auto virt = qobject_cast<DrmVirtualOutput *>(output)) {
        return {virt->primaryLayer()};
    }
    return static_cast<DrmOutput *>(output)->pipeline()->layers() | std::ranges::to<QList<OutputLayer *>>();
}

DrmLease::DrmLease(DrmGpu *gpu, FileDescriptor &&fd, uint32_t lesseeId, const QList<DrmOutput *> &outputs)
    : m_gpu(gpu)
    , m_fd(std::move(fd))
    , m_lesseeId(lesseeId)
    , m_outputs(outputs)
{
    for (DrmOutput *output : m_outputs) {
        output->leased(this);
    }
}

DrmLease::~DrmLease()
{
    qCDebug(KWIN_DRM, "Revoking lease with leaseID %u", m_lesseeId);
    drmModeRevokeLease(m_gpu->fd(), m_lesseeId);
    for (DrmOutput *output : m_outputs) {
        output->leaseEnded();
        output->pipeline()->setEnable(false);
    }
}

FileDescriptor &DrmLease::fd()
{
    return m_fd;
}

uint32_t DrmLease::lesseeId() const
{
    return m_lesseeId;
}
}

QDebug &operator<<(QDebug &s, const KWin::DrmGpu *gpu)
{
    s << gpu->drmDevice()->path();
    return s;
}

#include "moc_drm_gpu.cpp"
