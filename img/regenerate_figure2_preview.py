from pathlib import Path
import math

from PIL import Image, ImageDraw, ImageFont


ROOT = Path(__file__).resolve().parent
OUTPUT = ROOT / "figure2-regenerated-preview.png"

FONT_REG_CANDIDATES = [
    Path(r"C:\Windows\Fonts\times.ttf"),
    Path(r"C:\Windows\Fonts\georgia.ttf"),
]
FONT_BOLD_CANDIDATES = [
    Path(r"C:\Windows\Fonts\timesbd.ttf"),
    Path(r"C:\Windows\Fonts\georgiab.ttf"),
]


def get_font(size: int, bold: bool = False) -> ImageFont.FreeTypeFont:
    candidates = FONT_BOLD_CANDIDATES if bold else FONT_REG_CANDIDATES
    for path in candidates:
        if path.exists():
            return ImageFont.truetype(str(path), size=size)
    return ImageFont.load_default()


def draw_centered_text(
    draw: ImageDraw.ImageDraw,
    box: tuple[float, float, float, float],
    text: str,
    font: ImageFont.FreeTypeFont,
    fill: str,
    spacing: int = 0,
) -> None:
    bbox = draw.multiline_textbbox((0, 0), text, font=font, spacing=spacing, align="center")
    width = bbox[2] - bbox[0]
    height = bbox[3] - bbox[1]
    x = (box[0] + box[2] - width) / 2
    y = (box[1] + box[3] - height) / 2
    draw.multiline_text((x, y), text, font=font, fill=fill, spacing=spacing, align="center")


def draw_arrow(
    draw: ImageDraw.ImageDraw,
    start: tuple[float, float],
    end: tuple[float, float],
    color: str,
    width: int,
    head_len: int,
    head_width: int,
) -> None:
    draw.line([start, end], fill=color, width=width)
    angle = math.atan2(end[1] - start[1], end[0] - start[0])
    left = (
        end[0] - head_len * math.cos(angle) + head_width * math.sin(angle),
        end[1] - head_len * math.sin(angle) - head_width * math.cos(angle),
    )
    right = (
        end[0] - head_len * math.cos(angle) - head_width * math.sin(angle),
        end[1] - head_len * math.sin(angle) + head_width * math.cos(angle),
    )
    draw.polygon([end, left, right], fill=color)


def draw_dashed_arrow(
    draw: ImageDraw.ImageDraw,
    start: tuple[float, float],
    end: tuple[float, float],
    color: str,
    width: int,
    dash: int,
    gap: int,
    head_len: int,
    head_width: int,
) -> None:
    total = math.dist(start, end)
    vx = (end[0] - start[0]) / total
    vy = (end[1] - start[1]) / total
    pos = 0.0
    while pos < total - head_len:
        seg_end = min(pos + dash, total - head_len)
        p1 = (start[0] + vx * pos, start[1] + vy * pos)
        p2 = (start[0] + vx * seg_end, start[1] + vy * seg_end)
        draw.line([p1, p2], fill=color, width=width)
        pos += dash + gap

    arrow_base = (end[0] - vx * head_len, end[1] - vy * head_len)
    draw.line([arrow_base, end], fill=color, width=width)
    draw_arrow(draw, arrow_base, end, color, width, head_len, head_width)


def unit_vector(start: tuple[float, float], end: tuple[float, float]) -> tuple[float, float]:
    dx = end[0] - start[0]
    dy = end[1] - start[1]
    length = math.hypot(dx, dy)
    return dx / length, dy / length


def unit_normal(start: tuple[float, float], end: tuple[float, float]) -> tuple[float, float]:
    tx, ty = unit_vector(start, end)
    return -ty, tx


def point_segment_distance(
    point: tuple[float, float],
    start: tuple[float, float],
    end: tuple[float, float],
) -> float:
    ax, ay = start
    bx, by = end
    px, py = point
    abx = bx - ax
    aby = by - ay
    ab2 = abx * abx + aby * aby
    if ab2 == 0:
        return math.hypot(px - ax, py - ay)
    t = ((px - ax) * abx + (py - ay) * aby) / ab2
    t = max(0.0, min(1.0, t))
    qx = ax + t * abx
    qy = ay + t * aby
    return math.hypot(px - qx, py - qy)


def bbox_segment_distance(
    bbox: tuple[float, float, float, float],
    start: tuple[float, float],
    end: tuple[float, float],
    samples: int = 200,
) -> float:
    x1, y1, x2, y2 = bbox
    points = []
    for i in range(samples + 1):
        t = i / samples
        points.extend(
            [
                (x1 + (x2 - x1) * t, y1),
                (x1 + (x2 - x1) * t, y2),
                (x1, y1 + (y2 - y1) * t),
                (x2, y1 + (y2 - y1) * t),
            ]
        )
    return min(point_segment_distance(point, start, end) for point in points)


def path_label_bbox(
    draw: ImageDraw.ImageDraw,
    text: str,
    font: ImageFont.FreeTypeFont,
    start: tuple[float, float],
    end: tuple[float, float],
    normal_sign: int,
    normal_offset: float,
    tangent_shift: float,
) -> tuple[float, float, float, float]:
    tx, ty = unit_vector(start, end)
    nx, ny = unit_normal(start, end)
    mx = (start[0] + end[0]) / 2
    my = (start[1] + end[1]) / 2
    cx = mx + normal_sign * nx * normal_offset + tx * tangent_shift
    cy = my + normal_sign * ny * normal_offset + ty * tangent_shift
    raw_bbox = draw.textbbox((0, 0), text, font=font)
    width = raw_bbox[2] - raw_bbox[0]
    height = raw_bbox[3] - raw_bbox[1]
    return (cx - width / 2, cy - height / 2, cx + width / 2, cy + height / 2)


def draw_path_label(
    draw: ImageDraw.ImageDraw,
    text: str,
    font: ImageFont.FreeTypeFont,
    start: tuple[float, float],
    end: tuple[float, float],
    normal_sign: int,
    min_clearance: float,
    fill: str,
) -> None:
    for offset in range(120, 260, 4):
        bbox = path_label_bbox(draw, text, font, start, end, normal_sign, offset, 0)
        if bbox_segment_distance(bbox, start, end) >= min_clearance:
            draw.text((bbox[0], bbox[1]), text, font=font, fill=fill)
            return
    bbox = path_label_bbox(draw, text, font, start, end, normal_sign, 260, 0)
    draw.text((bbox[0], bbox[1]), text, font=font, fill=fill)


def draw_centered_line_label(
    draw: ImageDraw.ImageDraw,
    text: str,
    font: ImageFont.FreeTypeFont,
    center_x: float,
    line_y: float,
    margin_from_line: float,
    place_above: bool,
    fill: str,
) -> None:
    bbox = draw.textbbox((0, 0), text, font=font)
    width = bbox[2] - bbox[0]
    height = bbox[3] - bbox[1]
    if place_above:
        top = line_y - margin_from_line - height
    else:
        top = line_y + margin_from_line
    draw.text((center_x - width / 2, top), text, font=font, fill=fill)


def main() -> None:
    scale = 2
    width = 2200
    height = 560

    canvas = Image.new("RGB", (width * scale, height * scale), "white")
    draw = ImageDraw.Draw(canvas)

    color = "#222222"
    title_font = get_font(36 * scale, bold=True)
    path_font = get_font(22 * scale, bold=True)
    center_font = get_font(24 * scale, bold=True)
    indirect_font = get_font(24 * scale, bold=True)

    left_box = (90 * scale, 250 * scale, 640 * scale, 430 * scale)
    top_box = (830 * scale, 10 * scale, 1390 * scale, 190 * scale)
    right_box = (1570 * scale, 250 * scale, 2140 * scale, 430 * scale)

    draw.rounded_rectangle(left_box, radius=34 * scale, outline=color, width=4 * scale, fill="white")
    draw.rounded_rectangle(top_box, radius=34 * scale, outline=color, width=4 * scale, fill="white")
    draw.rounded_rectangle(right_box, radius=34 * scale, outline=color, width=4 * scale, fill="white")

    draw_centered_text(draw, left_box, "Sweetener Type", title_font, color)
    draw_centered_text(draw, top_box, "Perceived\nNaturalness", title_font, color, spacing=6 * scale)
    draw_centered_text(draw, right_box, "Purchase Intention", title_font, color)

    left_mid_y = (left_box[1] + left_box[3]) / 2
    right_mid_y = (right_box[1] + right_box[3]) / 2
    top_attach_y = top_box[1] + 0.60 * (top_box[3] - top_box[1])

    left_start = (left_box[2], left_mid_y)
    left_end = (top_box[0], top_attach_y)
    right_start = (top_box[2], top_attach_y)
    right_end = (right_box[0], right_mid_y)
    direct_start = (left_box[2], left_mid_y)
    direct_end = (right_box[0], right_mid_y)

    draw_arrow(draw, left_start, left_end, color, 4 * scale, 18 * scale, 12 * scale)
    draw_arrow(draw, right_start, right_end, color, 4 * scale, 18 * scale, 12 * scale)
    draw_dashed_arrow(draw, direct_start, direct_end, color, 3 * scale, 20 * scale, 14 * scale, 18 * scale, 12 * scale)

    draw_path_label(
        draw,
        "b = 3.23***",
        path_font,
        left_start,
        left_end,
        normal_sign=-1,
        min_clearance=52,
        fill=color,
    )
    draw_path_label(
        draw,
        "b = 0.42***",
        path_font,
        right_start,
        right_end,
        normal_sign=-1,
        min_clearance=52,
        fill=color,
    )

    center_x = (direct_start[0] + direct_end[0]) / 2
    line_y = direct_start[1]
    draw_centered_line_label(
        draw,
        "b = -0.51 (ns)",
        center_font,
        center_x,
        line_y,
        margin_from_line=10 * scale,
        place_above=True,
        fill=color,
    )
    draw_centered_line_label(
        draw,
        "indirect effect: b = 1.36, 95% CI [0.65, 2.09]",
        indirect_font,
        center_x,
        line_y,
        margin_from_line=14 * scale,
        place_above=False,
        fill=color,
    )

    canvas = canvas.resize((width, height), resample=Image.Resampling.LANCZOS)
    canvas.save(OUTPUT, format="PNG", optimize=True)


if __name__ == "__main__":
    main()
