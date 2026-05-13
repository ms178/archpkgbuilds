/*
    KWin - the KDE window manager
    This file is part of the KDE project.

    SPDX-FileCopyrightText: 2018 Roman Gilg <subdiff@gmail.com>
    SPDX-FileCopyrightText: 2021 Xaver Hugl <xaver.hugl@gmail.com>

    SPDX-License-Identifier: GPL-2.0-or-later
*/
#include "drm_virtual_output.h"

#include "core/outputconfiguration.h"
#include "core/renderbackend.h"
#include "drm_backend.h"
#include "drm_gpu.h"
#include "drm_layer.h"
#include "drm_render_backend.h"
#include "utils/softwarevsyncmonitor.h"

#include <utility>

namespace KWin
{

DrmVirtualOutput::DrmVirtualOutput(DrmBackend *backend, const QString &name, const QString &description, const QSize &size, qreal scale)
    : m_backend(backend)
    , m_vsyncMonitor(SoftwareVsyncMonitor::create())
{
    connect(m_vsyncMonitor.get(), &VsyncMonitor::vblankOccurred, this, &DrmVirtualOutput::vblank);

    auto mode = std::make_shared<OutputMode>(size, 60000, OutputMode::Flag::Preferred);
    m_renderLoop->setRefreshRate(mode->refreshRate());

    setInformation(Information{
        .name = QStringLiteral("Virtual-") + name,
        .model = description,
        .physicalSize = size,
        .capabilities = Capability::CustomModes,
    });

    setState(State{
        .scale = scale,
        .modes = {mode},
        .currentMode = mode,
    });

    recreateSurface();
}

DrmVirtualOutput::~DrmVirtualOutput() = default;

bool DrmVirtualOutput::testPresentation(const std::shared_ptr<OutputFrame> &frame)
{
    Q_UNUSED(frame)
    return true;
}

bool DrmVirtualOutput::present(const QList<OutputLayer *> &layersToUpdate, const std::shared_ptr<OutputFrame> &frame)
{
    Q_UNUSED(layersToUpdate)

    m_frame = frame;
    m_vsyncMonitor->arm();
    return true;
}

void DrmVirtualOutput::vblank(std::chrono::nanoseconds timestamp)
{
    if (auto frame = std::exchange(m_frame, std::shared_ptr<OutputFrame>{}); frame) {
        frame->presented(timestamp, PresentationMode::VSync);
    }
}

DrmOutputLayer *DrmVirtualOutput::primaryLayer() const
{
    return m_layer.get();
}

void DrmVirtualOutput::recreateSurface()
{
    m_layer = m_backend->renderBackend()->createLayer(this);
}

void DrmVirtualOutput::applyChanges(const OutputConfiguration &config)
{
    auto props = config.constChangeSet(this);
    if (!props) {
        return;
    }

    const bool hasAnyChange = props->enabled
        || props->transform
        || props->pos
        || props->scale
        || props->scaleSetting
        || props->desiredModeSize
        || props->desiredModeRefreshRate
        || props->mode
        || props->uuid
        || props->replicationSource
        || props->priority
        || props->deviceOffset
        || props->customModes;
    if (!hasAnyChange) {
        return;
    }

    Q_EMIT aboutToChange(props.get());

    State next = m_state;
    next.enabled = props->enabled.value_or(m_state.enabled);
    next.transform = props->transform.value_or(m_state.transform);
    next.position = props->pos.value_or(m_state.position);
    next.scale = props->scale.value_or(m_state.scale);
    next.scaleSetting = props->scaleSetting.value_or(m_state.scaleSetting);
    next.desiredModeSize = props->desiredModeSize.value_or(m_state.desiredModeSize);
    next.desiredModeRefreshRate = props->desiredModeRefreshRate.value_or(m_state.desiredModeRefreshRate);
    next.desiredModeFlags = props->desiredModeFlags.value_or(m_state.desiredModeFlags);
    next.currentMode = props->mode.value_or(m_state.currentMode).lock();
    if (!next.currentMode) {
        if (next.modes.isEmpty()) {
            return;
        }
        next.currentMode = next.modes.front();
    }
    next.uuid = props->uuid.value_or(m_state.uuid);
    next.replicationSource = props->replicationSource.value_or(m_state.replicationSource);
    next.priority = props->priority.value_or(m_state.priority);
    next.deviceOffset = props->deviceOffset.value_or(m_state.deviceOffset);

    if (props->customModes.has_value()) {
        next.customModes = *props->customModes;

        QList<std::shared_ptr<OutputMode>> newModes;
        newModes.reserve(next.modes.size() + next.customModes.size());

        for (const auto &mode : std::as_const(next.modes)) {
            if (mode->flags() & OutputMode::Flag::Custom) {
                continue;
            }
            newModes.push_back(mode);
        }

        for (const auto &custom : std::as_const(next.customModes)) {
            newModes.push_back(std::make_shared<OutputMode>(custom.size, custom.refreshRate, custom.flags | OutputMode::Flag::Custom));
        }

        next.modes = std::move(newModes);

        if (!next.currentMode) {
            if (next.modes.isEmpty()) {
                return;
            }
            next.currentMode = next.modes.front();
        } else if (!next.modes.contains(next.currentMode)) {
            const auto currentSize = next.currentMode->size();
            const auto currentRefreshRate = next.currentMode->refreshRate();
            const auto currentFlags = next.currentMode->flags();
            std::shared_ptr<OutputMode> matchedMode;

            for (const auto &mode : std::as_const(next.modes)) {
                if (mode->size() == currentSize
                    && mode->refreshRate() == currentRefreshRate
                    && mode->flags() == currentFlags) {
                    matchedMode = mode;
                    break;
                }
            }

            if (matchedMode) {
                next.currentMode = std::move(matchedMode);
            } else {
                next.modes.push_front(next.currentMode);
                next.currentMode->setRemoved();
            }
        }
    }

    const auto oldRefreshRate = m_state.currentMode ? m_state.currentMode->refreshRate() : 0;
    const auto newRefreshRate = next.currentMode ? next.currentMode->refreshRate() : 0;

    setState(next);

    if (oldRefreshRate != newRefreshRate) {
        m_renderLoop->setRefreshRate(newRefreshRate);
        m_vsyncMonitor->setRefreshRate(newRefreshRate);
    }

    Q_EMIT changed();
}
}

#include "moc_drm_virtual_output.cpp"
