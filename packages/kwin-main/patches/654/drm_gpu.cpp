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

#include <QFile>
#include <algorithm>
#include <array>
#include <atomic>
#include <cerrno>
#include <cstdint>
#include <cstring>
#include <fcntl.h>
#include <gbm.h>
#include <poll.h>
#include <ranges>
#include <unistd.h>
#include <vector>

#include <drm_fourcc.h>
#include <libdrm/drm_mode.h>
#include <xf86drm.h>
#include <xf86drmMode.h>

#ifndef DRM_CLIENT_CAP_CURSOR_PLANE_HOTSPOT
#define DRM_CLIENT_CAP_CURSOR_PLANE_HOTSPOT 6
#endif
#ifndef DRM_CAP_ATOMIC_ASYNC_PAGE_FLIP
#define DRM_CAP_ATOMIC_ASYNC_PAGE_FLIP 0x15
#endif

namespace KWin
{

DrmGpu::DrmGpu(DrmBackend *backend, int fd, std::unique_ptr<DrmDevice> &&device)
    : m_fd(fd)
    , m_drmDevice(std::move(device))
    , m_atomicModeSetting(false)
    , m_platform(backend)
{
    uint64_t capability = 0;

    if (drmGetCap(fd, DRM_CAP_CURSOR_WIDTH, &capability) == 0) {
        m_cursorSize.setWidth(capability);
    } else {
        m_cursorSize.setWidth(64);
    }

    if (drmGetCap(fd, DRM_CAP_CURSOR_HEIGHT, &capability) == 0) {
        m_cursorSize.setHeight(capability);
    } else {
        m_cursorSize.setHeight(64);
    }

    if (drmGetCap(fd, DRM_CAP_TIMESTAMP_MONOTONIC, &capability) == 0 && capability == 1) {
        m_presentationClock = CLOCK_MONOTONIC;
    } else {
        m_presentationClock = CLOCK_REALTIME;
    }

    m_addFB2ModifiersSupported = drmGetCap(fd, DRM_CAP_ADDFB2_MODIFIERS, &capability) == 0 && capability == 1;
    qCDebug(KWIN_DRM) << "drmModeAddFB2WithModifiers is" << (m_addFB2ModifiersSupported ? "supported" : "not supported") << "on GPU" << this;

    DrmUniquePtr<drmVersion> version(drmGetVersion(fd));
    if (version) {
        m_driverName = QString::fromUtf8(version->name);
        m_isI915 = strstr(version->name, "i915");
        m_isNVidia = strstr(version->name, "nvidia-drm");
        m_isAmdgpu = strstr(version->name, "amdgpu");
        m_isVmwgfx = strstr(version->name, "vmwgfx");
        m_isVirtualMachine = strstr(version->name, "virtio") || strstr(version->name, "qxl")
            || strstr(version->name, "vmwgfx") || strstr(version->name, "vboxvideo");
    }

    if (m_isNVidia) {
        QFile moduleVersion("/sys/module/nvidia_drm/version");
        if (moduleVersion.open(QIODeviceBase::OpenModeFlag::ReadOnly)) {
            m_nvidiaDriverVersion = Version::parseString(moduleVersion.readLine(100));
        }
    }

    m_socketNotifier = std::make_unique<QSocketNotifier>(fd, QSocketNotifier::Read);
    connect(m_socketNotifier.get(), &QSocketNotifier::activated, this, &DrmGpu::dispatchEvents);

    initDrmResources();

    if (!m_atomicModeSetting) {
        m_asyncPageflipSupported = drmGetCap(fd, DRM_CAP_ASYNC_PAGE_FLIP, &capability) == 0 && capability == 1;
    } else {
        m_asyncPageflipSupported = drmGetCap(fd, DRM_CAP_ATOMIC_ASYNC_PAGE_FLIP, &capability) == 0 && capability == 1;
    }

    m_delayedModesetTimer.setInterval(0);
    m_delayedModesetTimer.setSingleShot(true);
    connect(&m_delayedModesetTimer, &QTimer::timeout, this, &DrmGpu::doModeset);
}

DrmGpu::~DrmGpu()
{
    removeOutputs();
    m_eglDisplay.reset();
    m_crtcs.clear();
    m_connectors.clear();
    m_planes.clear();
    m_socketNotifier.reset();
    m_platform->session()->closeRestricted(m_fd);
}

FileDescriptor DrmGpu::createNonMasterFd() const
{
    char *path = drmGetDeviceNameFromFd2(m_fd);
    FileDescriptor fd{open(path, O_RDWR | O_CLOEXEC)};
    free(path);
    if (!fd.isValid()) {
        qCWarning(KWIN_DRM) << "Could not open DRM fd for leasing!" << strerror(errno);
    } else {
        if (drmIsMaster(fd.get())) {
            if (drmDropMaster(fd.get()) != 0) {
                qCWarning(KWIN_DRM) << "Could not create a non-master DRM fd for leasing!" << strerror(errno);
                return FileDescriptor{};
            }
        }
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
    if (noAMS) {
        qCWarning(KWIN_DRM) << "Atomic Mode Setting requested off via environment variable. Using legacy mode on GPU" << this;
    } else if (drmSetClientCap(m_fd, DRM_CLIENT_CAP_ATOMIC, 1) == 0) {
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
            for (unsigned int i = 0; i < planeResources->count_planes; ++i) {
                auto plane = std::make_unique<DrmPlane>(this, planeResources->planes[i]);
                if (plane->init()) {
                    m_allObjects.push_back(plane.get());
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
        m_allObjects.push_back(crtc.get());
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
        for (const DrmOutput *output : m_drmOutputs) {
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

    for (const auto &crtc : m_crtcs) {
        crtc->updateProperties();
    }
    for (const auto &plane : m_planes) {
        plane->updateProperties();
    }

    std::vector<DrmConnector *> existing;
    existing.reserve(m_connectors.size());

    for (int i = 0; i < resources->count_connectors; ++i) {
        const uint32_t currentConnector = resources->connectors[i];
        const auto it = std::ranges::find_if(m_connectors, [currentConnector](const auto &connector) {
            return connector->id() == currentConnector;
        });
        if (it == m_connectors.end()) {
            auto conn = std::make_shared<DrmConnector>(this, currentConnector);
            if (!conn->init()) {
                continue;
            }
            existing.push_back(conn.get());
            m_allObjects.push_back(conn.get());
            m_connectors.push_back(std::move(conn));
        } else {
            (*it)->updateProperties();
            existing.push_back(it->get());
        }
    }

    for (auto it = m_connectors.begin(); it != m_connectors.end();) {
        DrmConnector *conn = it->get();
        const auto output = findOutput(conn->id());
        const bool stillExists = std::ranges::find(existing, conn) != existing.end();

        if (!stillExists || !conn->isConnected()) {
            if (output) {
                removeOutput(output);
            }
        } else if (!output) {
            qCDebug(KWIN_DRM, "New %soutput on GPU %s: %s", conn->isNonDesktop() ? "non-desktop " : "", qPrintable(m_drmDevice->path()), qPrintable(conn->modelName()));
            auto &pipeline = m_pipelineMap[conn];
            pipeline = std::make_unique<DrmPipeline>(conn);
            m_pipelines.push_back(pipeline.get());
            auto output = new DrmOutput(*it, pipeline.get());
            m_drmOutputs.push_back(output);
            Q_EMIT outputAdded(output);
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
            it++;
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

// Optimized iterative checkCrtcAssignment (Issue #3)
// Uses bitmasks for zero-allocation state tracking
DrmPipeline::Error DrmGpu::checkCrtcAssignment(const std::vector<DrmConnector *> &connectors, const std::vector<DrmCrtc *> &crtcs)
{
    if (connectors.size() > 32 || crtcs.size() > 32) {
        return DrmPipeline::Error::InvalidArguments;
    }

    struct State {
        uint32_t crtcMask;
        int connectorIdx;
        int loopIdx;
    };

    static thread_local std::vector<State> stack;
    if (stack.capacity() < 64) stack.reserve(64);
    stack.clear();

    // Separate enabled connectors and their pipelines
    std::vector<std::pair<DrmConnector *, DrmPipeline *>> enabledPairs;
    enabledPairs.reserve(connectors.size());

    for (DrmConnector *conn : connectors) {
        auto it = m_pipelineMap.find(conn);
        if (it != m_pipelineMap.end() && it->second->enabled() && conn->isConnected()) {
            enabledPairs.emplace_back(conn, it->second.get());
        } else if (it != m_pipelineMap.end()) {
            it->second->setCrtc(nullptr);
        }
    }

    if (enabledPairs.empty()) {
        return testPipelines();
    }

    // Determine preferred CRTC indices
    std::vector<int> preferredCrtcIdx(enabledPairs.size(), -1);
    if (m_atomicModeSetting) {
        for (size_t i = 0; i < enabledPairs.size(); ++i) {
            const uint32_t currentCrtcId = enabledPairs[i].first->crtcId.isValid() ? enabledPairs[i].first->crtcId.value() : 0;
            if (currentCrtcId != 0) {
                for (int j = 0; j < static_cast<int>(crtcs.size()); ++j) {
                    if (crtcs[j]->id() == currentCrtcId) {
                        preferredCrtcIdx[i] = j;
                        break;
                    }
                }
            }
        }
    }

    stack.push_back({0, 0, 0});

    while (!stack.empty()) {
        auto [crtcMask, connIdx, loopIdx] = stack.back();
        stack.pop_back();

        if (connIdx == static_cast<int>(enabledPairs.size())) {
            auto result = testPipelines();
            if (result == DrmPipeline::Error::None
                || result == DrmPipeline::Error::NoPermission
                || result == DrmPipeline::Error::FramePending) {
                return result;
            }
            continue;
        }

        auto *connector = enabledPairs[connIdx].first;
        auto *pipeline = enabledPairs[connIdx].second;

        // Optimization: Check if enough CRTCs remain
        if (static_cast<int>(crtcs.size()) - __builtin_popcount(crtcMask) < (static_cast<int>(enabledPairs.size()) - connIdx)) {
            continue;
        }

        bool pushedNext = false;
        const int prefIdx = preferredCrtcIdx[connIdx];

        for (int k = loopIdx; k < static_cast<int>(crtcs.size()); ++k) {
            int realIdx = k;
            if (prefIdx >= 0) {
                if (k == 0) realIdx = prefIdx;
                else if (k == prefIdx) realIdx = 0;
            }

            if (crtcMask & (1u << realIdx)) continue;
            if (!connector->isCrtcSupported(crtcs[realIdx])) continue;

            pipeline->setCrtc(crtcs[realIdx]);

            if (k + 1 < static_cast<int>(crtcs.size())) {
                stack.push_back({crtcMask, connIdx, k + 1});
            }
            stack.push_back({crtcMask | (1u << realIdx), connIdx + 1, 0});
            pushedNext = true;
            break;
        }

        if (!pushedNext) {
            pipeline->setCrtc(nullptr);
        }
    }

    return DrmPipeline::Error::InvalidArguments;
}

DrmPipeline::Error DrmGpu::testPendingConfiguration()
{
    std::vector<DrmConnector *> connectors;
    std::vector<DrmCrtc *> crtcs;
    connectors.reserve(m_connectors.size());
    crtcs.reserve(m_crtcs.size());

    for (const auto &conn : m_connectors) {
        const bool isLeased = std::ranges::any_of(m_drmOutputs, [&conn](const auto output) {
            return output->lease() && output->pipeline()->connector() == conn.get();
        });
        if (!isLeased) {
            connectors.push_back(conn.get());
        }
    }
    for (const auto &crtc : m_crtcs) {
        const bool isLeased = std::ranges::any_of(m_drmOutputs, [&crtc](const auto output) {
            return output->lease() && output->pipeline()->crtc() == crtc.get();
        });
        if (!isLeased) {
            crtcs.push_back(crtc.get());
        }
    }
    if (m_atomicModeSetting) {
        std::sort(connectors.begin(), connectors.end(), [](auto c1, auto c2) {
            uint64_t id1 = c1->crtcId.isValid() ? c1->crtcId.value() : 0;
            uint64_t id2 = c2->crtcId.isValid() ? c2->crtcId.value() : 0;
            return id1 > id2;
        });
    }
    m_forceLowBandwidthMode = false;
    auto err = checkCrtcAssignment(connectors, crtcs);
    if (err == DrmPipeline::Error::None || err == DrmPipeline::Error::NoPermission || err == DrmPipeline::Error::FramePending) {
        return err;
    }
    const bool hasPreferAccuracy = std::ranges::any_of(m_drmOutputs, [](const auto &output) {
        return output->colorPowerTradeoff() == Output::ColorPowerTradeoff::PreferAccuracy;
    });
    if (m_addFB2ModifiersSupported || hasPreferAccuracy) {
        m_forceLowBandwidthMode = true;
        err = checkCrtcAssignment(connectors, crtcs);
    }
    return err;
}

void DrmGpu::releaseUnusedBuffers()
{
    const auto isLayerUsed = [this](DrmPipelineLayer *layer) {
        return std::ranges::any_of(m_pipelines, [layer](const auto &pipeline) {
            return pipeline->layers().contains(layer);
        });
    };
    for (const auto &[plane, layer] : m_planeLayerMap) {
        if (!isLayerUsed(layer.get())) {
            layer->releaseBuffers();
        }
    }
    for (const auto &[crtc, layer] : m_legacyLayerMap) {
        if (!isLayerUsed(layer.get())) {
            layer->releaseBuffers();
        }
    }
    for (const auto &[crtc, layer] : m_legacyCursorLayerMap) {
        if (!isLayerUsed(layer.get())) {
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
                layer->setTargetRect(QRect(QPoint(0, 0), pipeline->mode()->size()));
                layer->setSourceRect(QRect(QPoint(0, 0), pipeline->mode()->size()));
                layer->setEnabled(true);
                if (!layer->preparePresentationTest()) {
                    return DrmPipeline::Error::InvalidArguments;
                }
            } else {
                layer->setEnabled(false);
            }
        }
    }
    std::vector<DrmPipeline *> inactivePipelines;
    inactivePipelines.reserve(m_pipelines.size());
    std::ranges::copy_if(m_pipelines, std::back_inserter(inactivePipelines), [](const auto pipeline) {
        return pipeline->enabled() && !pipeline->active();
    });
    DrmPipeline::Error test = DrmPipeline::commitPipelines(m_pipelines, DrmPipeline::CommitMode::TestAllowModeset, unusedModesetObjects());
    if (!inactivePipelines.empty() && test == DrmPipeline::Error::None) {
        for (const auto pipeline : std::as_const(inactivePipelines)) {
            pipeline->setActive(true);
        }
        test = DrmPipeline::commitPipelines(m_pipelines, DrmPipeline::CommitMode::TestAllowModeset, unusedModesetObjects());
        for (const auto pipeline : std::as_const(inactivePipelines)) {
            pipeline->setActive(false);
        }
    }
    return test;
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

static std::chrono::nanoseconds convertTimestamp(clockid_t sourceClock, clockid_t targetClock,
                                                 const timespec &timestamp)
{
    using namespace std::chrono;

    auto toNs = [](const timespec &ts) -> nanoseconds {
        return seconds(ts.tv_sec) + nanoseconds(ts.tv_nsec);
    };

    auto targetNow = [&]() -> nanoseconds {
        timespec now = {};
        if (clock_gettime(targetClock, &now) == 0) {
            return toNs(now);
        }
        return duration_cast<nanoseconds>(steady_clock::now().time_since_epoch());
    };

    if (sourceClock == targetClock) {
        return toNs(timestamp);
    }

    timespec sourceCurrentTime = {};
    timespec targetCurrentTime = {};
    if (clock_gettime(sourceClock, &sourceCurrentTime) != 0 || clock_gettime(targetClock, &targetCurrentTime) != 0) {
        return targetNow();
    }

    const nanoseconds srcNow = toNs(sourceCurrentTime);
    const nanoseconds dstNow = toNs(targetCurrentTime);
    const nanoseconds srcEvent = toNs(timestamp);

    // dstEvent = dstNow - (srcNow - srcEvent)
    return dstNow - (srcNow - srcEvent);
}

void DrmGpu::pageFlipHandler(int fd, unsigned int sequence, unsigned int sec, unsigned int usec,
                            unsigned int crtc_id, void *user_data)
{
    Q_UNUSED(fd)
    Q_UNUSED(sequence)
    Q_UNUSED(crtc_id)

    auto *commit = static_cast<DrmCommit *>(user_data);
    DrmGpu *gpu = commit->gpu();

    if (!gpu->m_defunctCommits.empty()) [[unlikely]] {
        auto &defunct = gpu->m_defunctCommits;
        for (auto it = defunct.begin(), end = defunct.end(); it != end; ++it) {
            if (it->get() == commit) {
                defunct.erase(it);
                return;
            }
        }
    }

    static const bool s_sanitizeTimestamps =
        qEnvironmentVariableIntValue("KWIN_DRM_SANITIZE_PAGEFLIP_TIMESTAMPS") != 0;

    uint64_t s = sec;
    uint64_t us = usec;
    if (us >= 1'000'000ULL) [[unlikely]] {
        s += us / 1'000'000ULL;
        us %= 1'000'000ULL;
    }

    std::chrono::nanoseconds timestamp;
    if (Q_LIKELY(gpu->presentationClock() == CLOCK_MONOTONIC)) {
        timestamp = std::chrono::seconds(s) + std::chrono::microseconds(us);
    } else {
        timestamp = convertTimestamp(gpu->presentationClock(), CLOCK_MONOTONIC,
                                     {static_cast<time_t>(s), static_cast<long>(us * 1000ULL)});
    }

    if (Q_UNLIKELY((s == 0 && us == 0) || timestamp <= std::chrono::nanoseconds::zero())) {
        timespec nowTs = {};
        if (clock_gettime(CLOCK_MONOTONIC, &nowTs) == 0) {
            timestamp = std::chrono::seconds(nowTs.tv_sec) + std::chrono::nanoseconds(nowTs.tv_nsec);
        } else {
            timestamp = std::chrono::duration_cast<std::chrono::nanoseconds>(
                std::chrono::steady_clock::now().time_since_epoch());
        }
    } else if (Q_UNLIKELY(s_sanitizeTimestamps)) {
        timespec nowTs = {};
        if (clock_gettime(CLOCK_MONOTONIC, &nowTs) == 0) {
            const std::chrono::nanoseconds now =
                std::chrono::seconds(nowTs.tv_sec) + std::chrono::nanoseconds(nowTs.tv_nsec);

            constexpr std::chrono::nanoseconds kFutureEpsilon{std::chrono::milliseconds{5}};
            if (timestamp > now + kFutureEpsilon) [[unlikely]] {
                timestamp = now;
            } else if (timestamp > now) [[unlikely]] {
                timestamp = now;
            }
        }
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

    uint32_t lesseeId;
    FileDescriptor fd{drmModeCreateLease(m_fd, objects.constData(), objects.count(), 0, &lesseeId)};
    if (!fd.isValid()) {
        qCWarning(KWIN_DRM) << "Could not create DRM lease!" << strerror(errno);
        return nullptr;
    } else {
        qCDebug(KWIN_DRM) << "Created lease for" << objects.count() << "resources:";
        return std::make_unique<DrmLease>(this, std::move(fd), lesseeId, outputs);
    }
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
            for (const DrmOutput *output : m_drmOutputs) {
                output->renderLoop()->uninhibit();
            }
            for (const DrmOutput *output : m_drmOutputs) {
                if (!atomicModeSetting()) {
                    output->pipeline()->forceLegacyModeset();
                }
            }
        } else {
            for (const DrmOutput *output : m_drmOutputs) {
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
    auto pipelines = m_pipelines;
    for (const DrmOutput *output : m_drmOutputs) {
        if (output->lease()) {
            pipelines.removeOne(output->pipeline());
        }
    }
    const bool presentPendingForAll = std::ranges::all_of(pipelines, [](const DrmPipeline *pipeline) {
        return pipeline->modesetPresentPending() || !pipeline->activePending();
    });
    if (!presentPendingForAll) {
        return;
    }
    if (!isIdle()) {
        return;
    }
    if (m_inModeset) {
        return;
    }
    m_delayedModesetTimer.start();
}

void DrmGpu::doModeset()
{
    auto pipelines = m_pipelines;
    for (const DrmOutput *output : m_drmOutputs) {
        if (output->lease()) {
            pipelines.removeOne(output->pipeline());
        }
    }

    if (pipelines.empty()) {
        m_pendingModesetFrames.clear();
        m_forceModeset = false;
        return;
    }
    m_inModeset = true;

    const DrmPipeline::Error err = DrmPipeline::commitPipelines(pipelines, DrmPipeline::CommitMode::CommitModeset, unusedModesetObjects());
    for (DrmPipeline *pipeline : std::as_const(pipelines)) {
        if (pipeline->modesetPresentPending()) {
            pipeline->resetModesetPresentPending();
        }
    }
    m_forceModeset = false;
    if (err == DrmPipeline::Error::None) {
        for (const auto &[pipeline, frame] : m_pendingModesetFrames) {
            frame->presented(std::chrono::steady_clock::now().time_since_epoch(), PresentationMode::VSync);
        }
    } else {
        if (err != DrmPipeline::Error::FramePending) {
            QTimer::singleShot(0, m_platform, &DrmBackend::updateOutputs);
        }
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
        for (const auto &plane : m_planes) {
            m_planeLayerMap[plane.get()] = m_platform->renderBackend()->createDrmPlaneLayer(plane.get());
        }
    } else {
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

// Optimized assignOutputLayers
void DrmGpu::assignOutputLayers()
{
    if (m_atomicModeSetting) {
        static thread_local std::vector<DrmPipeline *> enabledPipelines;
        static thread_local std::vector<DrmPlane *> freePlanes;

        enabledPipelines.clear();
        freePlanes.clear();

        if (enabledPipelines.capacity() < 16) enabledPipelines.reserve(16);
        if (freePlanes.capacity() < 32) freePlanes.reserve(32);

        for (DrmPipeline *pipeline : m_pipelines) {
            if (pipeline->enabled()) {
                enabledPipelines.push_back(pipeline);
            }
        }

        for (const auto &plane : m_planes) {
            freePlanes.push_back(plane.get());
        }

        const size_t enabledCount = enabledPipelines.size();

        for (DrmPipeline *pipeline : enabledPipelines) {
            QList<DrmPipelineLayer *> layers = {m_planeLayerMap[pipeline->crtc()->primaryPlane()].get()};

            auto cursorIt = std::ranges::find_if(freePlanes, [pipeline](DrmPlane *plane) {
                return plane->isCrtcSupported(pipeline->crtc()->pipeIndex())
                    && plane->type.enumValue() == DrmPlane::TypeIndex::Cursor;
            });

            if (cursorIt != freePlanes.end()) {
                layers.push_back(m_planeLayerMap[*cursorIt].get());
                *cursorIt = freePlanes.back();
                freePlanes.pop_back();
            }

            if (enabledCount == 1) {
                for (DrmPlane *plane : freePlanes) {
                    if (plane->isCrtcSupported(pipeline->crtc()->pipeIndex())
                        && plane->type.enumValue() == DrmPlane::TypeIndex::Overlay) {
                        layers.push_back(m_planeLayerMap[plane].get());
                    }
                }
            }
            pipeline->setLayers(layers);
        }
    } else {
        for (DrmPipeline *pipeline : m_pipelines) {
            if (pipeline->crtc()) {
                pipeline->setLayers({m_legacyLayerMap[pipeline->crtc()].get(), m_legacyCursorLayerMap[pipeline->crtc()].get()});
            }
        }
    }
}

// Optimized importBuffer
std::shared_ptr<DrmFramebuffer> DrmGpu::importBuffer(GraphicsBuffer *buffer, FileDescriptor &&readFence)
{
    const DmaBufAttributes *attributes = buffer->dmabufAttributes();
    if (Q_UNLIKELY(!attributes)) {
        return nullptr;
    }

    const auto it = m_fbCache.find(buffer);
    if (it != m_fbCache.end()) {
        if (auto locked = it->second.lock()) {
            return std::make_shared<DrmFramebuffer>(locked, buffer, std::move(readFence));
        }
    }

    struct HandleCleanup {
        int fd;
        std::array<uint32_t, 4> handles;

        HandleCleanup(int f) : fd(f), handles{0, 0, 0, 0} {}

        ~HandleCleanup() {
            std::array<uint32_t, 4> uniqueHandles = {0, 0, 0, 0};
            int count = 0;

            for (int i = 0; i < 4; ++i) {
                if (handles[i] == 0) continue;

                bool isDuplicate = false;
                for (int j = 0; j < count; ++j) {
                    if (handles[i] == uniqueHandles[j]) {
                        isDuplicate = true;
                        break;
                    }
                }

                if (!isDuplicate) {
                    drmCloseBufferHandle(fd, handles[i]);
                    uniqueHandles[count++] = handles[i];
                }
            }
        }
    };

    HandleCleanup cleanup(m_fd);

    for (int i = 0; i < attributes->planeCount; ++i) {
        if (drmPrimeFDToHandle(m_fd, attributes->fd[i].get(), &cleanup.handles[i]) != 0) {
            qCWarning(KWIN_DRM) << "drmPrimeFDToHandle() failed";
            return nullptr;
        }
    }

    uint32_t framebufferId = 0;
    int ret;

    if (addFB2ModifiersSupported() && attributes->modifier != DRM_FORMAT_MOD_INVALID) {
        std::array<uint64_t, 4> modifier = {0, 0, 0, 0};
        for (int i = 0; i < attributes->planeCount; ++i) {
            modifier[i] = attributes->modifier;
        }

        ret = drmModeAddFB2WithModifiers(m_fd,
                                         attributes->width,
                                         attributes->height,
                                         attributes->format,
                                         cleanup.handles.data(),
                                         attributes->pitch.data(),
                                         attributes->offset.data(),
                                         modifier.data(),
                                         &framebufferId,
                                         DRM_MODE_FB_MODIFIERS);
    } else {
        ret = drmModeAddFB2(m_fd,
                            attributes->width,
                            attributes->height,
                            attributes->format,
                            cleanup.handles.data(),
                            attributes->pitch.data(),
                            attributes->offset.data(),
                            &framebufferId,
                            0);
        if (ret == EOPNOTSUPP && attributes->planeCount == 1) {
            ret = drmModeAddFB(m_fd,
                               attributes->width,
                               attributes->height,
                               24, 32,
                               attributes->pitch[0],
                               cleanup.handles[0],
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
    m_fbCache.erase(buf);
}

void DrmGpu::forgetBufferObject(QObject *buf)
{
    m_fbCache.erase(static_cast<GraphicsBuffer *>(buf));
}

QString DrmGpu::driverName() const
{
    return m_driverName;
}

QList<OutputLayer *> DrmGpu::compatibleOutputLayers(Output *output) const
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
    for (const auto output : m_outputs) {
        output->leased(this);
    }
}

DrmLease::~DrmLease()
{
    qCDebug(KWIN_DRM, "Revoking lease with leaseID %d", m_lesseeId);
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
