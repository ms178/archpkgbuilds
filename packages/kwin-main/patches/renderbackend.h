/*
    SPDX-FileCopyrightText: 2021 Vlad Zahorodnii <vlad.zahorodnii@kde.org>

    SPDX-License-Identifier: GPL-2.0-or-later
*/

#pragma once

#include "core/rendertarget.h"
#include "effect/globals.h"
#include "utils/filedescriptor.h"

#include <QObject>
#include <QPointer>
#include <QRegion>
#include <QHash>
#include <QList>

#include <chrono>
#include <memory>
#include <optional>
#include <vector>

namespace KWin
{

class GraphicsBuffer;
class Output;
class OutputLayer;
class PresentationFeedback;
class RenderLoop;
class DrmDevice;
class SyncTimeline;

/**
 * PresentationFeedback interface for receiving presentation events.
 *
 * presented() is invoked when a frame is actually presented to the screen, with the
 * refresh cycle duration, a monotonic timestamp, and the presentation mode used.
 */
class PresentationFeedback
{
public:
    explicit PresentationFeedback() = default;
    PresentationFeedback(const PresentationFeedback &copy) = delete;
    PresentationFeedback(PresentationFeedback &&move) = default;
    virtual ~PresentationFeedback() = default;

    virtual void presented(std::chrono::nanoseconds refreshCycleDuration,
                           std::chrono::nanoseconds timestamp,
                           PresentationMode mode) = 0;
};

/**
 * Span of time for a rendering operation, represented with steady_clock time points.
 * start and end default to epoch (0ns) so a default-constructed span is valid and
 * identifiable as "unset" unless overwritten.
 */
struct RenderTimeSpan
{
    std::chrono::steady_clock::time_point start = std::chrono::steady_clock::time_point{std::chrono::nanoseconds::zero()};
    std::chrono::steady_clock::time_point end = std::chrono::steady_clock::time_point{std::chrono::nanoseconds::zero()};

    RenderTimeSpan operator|(const RenderTimeSpan &other) const;
};

/**
 * Abstract interface to query a render time span from a GPU/CPU query object.
 */
class KWIN_EXPORT RenderTimeQuery
{
public:
    virtual ~RenderTimeQuery() = default;
    [[nodiscard]] virtual std::optional<RenderTimeSpan> query() = 0;
};

/**
 * CPU-side render time query using steady_clock timestamps.
 */
class KWIN_EXPORT CpuRenderTimeQuery : public RenderTimeQuery
{
public:
    /**
     * Marks the start of the query.
     */
    explicit CpuRenderTimeQuery();

    /**
     * Marks the end of the query.
     */
    void end();

    [[nodiscard]] std::optional<RenderTimeSpan> query() override;

private:
    const std::chrono::steady_clock::time_point m_start;
    std::optional<std::chrono::steady_clock::time_point> m_end;
};

/**
 * Represents a single frame targeted for presentation on an Output.
 *
 * It aggregates content metadata, presentation mode, damage, and optional timing
 * queries used to estimate render duration and to report presentation feedback.
 */
class KWIN_EXPORT OutputFrame
{
public:
    explicit OutputFrame(RenderLoop *loop, std::chrono::nanoseconds refreshDuration);
    ~OutputFrame();

    void presented(std::chrono::nanoseconds timestamp, PresentationMode mode);

    void addFeedback(std::unique_ptr<PresentationFeedback> &&feedback);

    void setContentType(ContentType type);
    [[nodiscard]] std::optional<ContentType> contentType() const;

    void setPresentationMode(PresentationMode mode);
    [[nodiscard]] PresentationMode presentationMode() const;

    void setDamage(const QRegion &region);
    [[nodiscard]] QRegion damage() const;

    void addRenderTimeQuery(std::unique_ptr<RenderTimeQuery> &&query);

    [[nodiscard]] std::chrono::steady_clock::time_point targetPageflipTime() const;
    [[nodiscard]] std::chrono::nanoseconds refreshDuration() const;
    [[nodiscard]] std::chrono::nanoseconds predictedRenderTime() const;

    [[nodiscard]] std::optional<double> brightness() const;
    void setBrightness(double brightness);

    [[nodiscard]] std::optional<double> artificialHdrHeadroom() const;
    void setArtificialHdrHeadroom(double edr);

private:
    [[nodiscard]] std::optional<RenderTimeSpan> queryRenderTime() const;

    const QPointer<RenderLoop> m_loop;
    const std::chrono::nanoseconds m_refreshDuration;
    const std::chrono::steady_clock::time_point m_targetPageflipTime;
    const std::chrono::nanoseconds m_predictedRenderTime;
    std::vector<std::unique_ptr<PresentationFeedback>> m_feedbacks;
    std::optional<ContentType> m_contentType;
    PresentationMode m_presentationMode = PresentationMode::VSync;
    QRegion m_damage;
    std::vector<std::unique_ptr<RenderTimeQuery>> m_renderTimeQueries;
    bool m_presented = false;
    std::optional<double> m_brightness;
    std::optional<double> m_artificialHdrHeadroom;
};

/**
 * The RenderBackend class is the base class for all rendering backends.
 */
class KWIN_EXPORT RenderBackend : public QObject
{
    Q_OBJECT

public:
    [[nodiscard]] virtual CompositingType compositingType() const = 0;

    [[nodiscard]] virtual bool checkGraphicsReset();

    [[nodiscard]] virtual OutputLayer *primaryLayer(Output *output) = 0;
    [[nodiscard]] virtual OutputLayer *cursorLayer(Output *output);
    [[nodiscard]] virtual bool present(Output *output, const std::shared_ptr<OutputFrame> &frame) = 0;
    virtual void repairPresentation(Output *output);

    [[nodiscard]] virtual DrmDevice *drmDevice() const;

    [[nodiscard]] virtual bool testImportBuffer(GraphicsBuffer *buffer);
    [[nodiscard]] virtual QHash<uint32_t, QList<uint64_t>> supportedFormats() const;
};

} // namespace KWin
