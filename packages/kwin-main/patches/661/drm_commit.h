/*
    KWin - the KDE window manager
    This file is part of the KDE project.

    SPDX-FileCopyrightText: 2023 Xaver Hugl <xaver.hugl@gmail.com>

    SPDX-License-Identifier: GPL-2.0-or-later
*/
#pragma once

#include <memory>
#include <xf86drmMode.h>

#include <QHash>
#include <QList>
#include <chrono>
#include <optional>
#include <unordered_map>
#include <unordered_set>

#include "core/renderloop.h"
#include "drm_pointer.h"
#include "drm_property.h"

namespace KWin
{

class DrmBlob;
class DrmConnector;
class DrmConnectorMode;
class DrmCrtc;
class DrmFramebuffer;
class DrmGpu;
class DrmPlane;
class DrmPipeline;
class OutputFrame;

class DrmCommit
{
public:
    virtual ~DrmCommit();

    [[nodiscard]] DrmGpu *gpu() const noexcept;
    virtual void pageFlipped(std::chrono::nanoseconds timestamp) = 0;
    void setDefunct() noexcept;

protected:
    explicit DrmCommit(DrmGpu *gpu);

    DrmGpu *const m_gpu;
    bool m_defunct = false;
};

class DrmAtomicCommit : public DrmCommit
{
public:
    explicit DrmAtomicCommit(DrmGpu *gpu);
    explicit DrmAtomicCommit(const QList<DrmPipeline *> &pipelines);
    DrmAtomicCommit(const DrmAtomicCommit &copy) = default;

    void addProperty(const DrmProperty &prop, uint64_t value);

    template<typename T>
    void addEnum(const DrmEnumProperty<T> &prop, T enumValue)
    {
        addProperty(prop, prop.valueForEnum(enumValue));
    }

    void addBlob(const DrmProperty &prop, const std::shared_ptr<DrmBlob> &blob);
    void addBuffer(DrmPlane *plane, const std::shared_ptr<DrmFramebuffer> &buffer, const std::shared_ptr<OutputFrame> &frame);
    void setVrr(DrmCrtc *crtc, bool vrr);
    void setPresentationMode(PresentationMode mode) noexcept;

    [[nodiscard]] bool test();
    [[nodiscard]] bool testAllowModeset();
    [[nodiscard]] bool commit();
    [[nodiscard]] bool commitModeset();

    void pageFlipped(std::chrono::nanoseconds timestamp) override;

    [[nodiscard]] bool areBuffersReadable() const;
    void setDeadline(std::chrono::steady_clock::time_point deadline);
    [[nodiscard]] std::optional<bool> isVrr() const noexcept;
    [[nodiscard]] const std::unordered_set<DrmPlane *> &modifiedPlanes() const noexcept;

    void merge(DrmAtomicCommit *onTop);

    void setAllowedVrrDelay(std::optional<std::chrono::nanoseconds> allowedDelay) noexcept;
    [[nodiscard]] std::optional<std::chrono::nanoseconds> allowedVrrDelay() const noexcept;

    [[nodiscard]] std::optional<std::chrono::steady_clock::time_point> targetPageflipTime() const noexcept;
    [[nodiscard]] bool isReadyFor(std::chrono::steady_clock::time_point pageflipTarget) const;
    [[nodiscard]] bool isTearing() const noexcept;

private:
    [[nodiscard]] bool doCommit(uint32_t flags);

    QList<DrmPipeline *> m_pipelines;
    std::optional<std::chrono::steady_clock::time_point> m_targetPageflipTime;
    std::optional<std::chrono::nanoseconds> m_allowedVrrDelay;
    std::unordered_map<const DrmProperty *, std::shared_ptr<DrmBlob>> m_blobs;
    std::unordered_map<DrmPlane *, std::shared_ptr<DrmFramebuffer>> m_buffers;
    std::unordered_map<DrmPlane *, std::shared_ptr<OutputFrame>> m_frames;
    std::unordered_set<DrmPlane *> m_planes;
    std::optional<bool> m_vrr;
    std::unordered_map<uint32_t /* object */, std::unordered_map<uint32_t /* property */, uint64_t /* value */>> m_properties;
    bool m_modeset = false;
    PresentationMode m_mode = PresentationMode::VSync;
};

class DrmLegacyCommit : public DrmCommit
{
public:
    DrmLegacyCommit(DrmPipeline *pipeline, const std::shared_ptr<DrmFramebuffer> &buffer, const std::shared_ptr<OutputFrame> &frame);

    [[nodiscard]] bool doModeset(DrmConnector *connector, DrmConnectorMode *mode);
    [[nodiscard]] bool doPageflip(PresentationMode mode);
    void pageFlipped(std::chrono::nanoseconds timestamp) override;

private:
    DrmPipeline *const m_pipeline;
    DrmCrtc *const m_crtc;
    const std::shared_ptr<DrmFramebuffer> m_buffer;
    std::shared_ptr<OutputFrame> m_frame;
    PresentationMode m_mode = PresentationMode::VSync;
};

}
