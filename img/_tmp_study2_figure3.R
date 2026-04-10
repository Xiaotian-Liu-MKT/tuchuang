suppressPackageStartupMessages({
  library(ggplot2)
})

spec <- list(
  output = "study2-figure3.svg",
  width = 6.2,
  height = 4.8,
  dpi = 320,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  x_label = "Product anthropomorphism",
  y_label = "Purchase intention",
  legend_title = "Sweetener type",
  y_limits = c(0, 7),
  y_breaks = 0:7,
  show_error_bars = FALSE,
  factor_a_levels = c("Absent", "Present"),
  factor_b_levels = c("Artificial", "Natural"),
  fills = c("white", "grey70"),
  cells = data.frame(
    factor_a = c("Absent", "Absent", "Present", "Present"),
    factor_b = c("Artificial", "Natural", "Artificial", "Natural"),
    mean = c(2.88, 4.30, 2.83, 3.49),
    error = c(NA_real_, NA_real_, NA_real_, NA_real_),
    stringsAsFactors = FALSE
  ),
  simple_effect_labels = c("p < .001", "p = .011"),
  interaction_label = "Interaction p = .040",
  mean_digits = 2
)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 1 && nzchar(args[1])) {
  spec$output <- args[1]
}

theme_jcr <- function(base_size = 12, base_family = "Times New Roman") {
  theme_classic(base_size = base_size, base_family = base_family) +
    theme(
      legend.position = "bottom",
      legend.title = element_text(face = "plain"),
      legend.key.width = grid::unit(1.4, "cm"),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.title = element_text(face = "bold"),
      plot.caption = element_text(size = base_size - 2)
    )
}

save_jcr_plot <- function(plot, output, width, height, dpi = 320) {
  ext <- tolower(tools::file_ext(output))
  if (ext == "svg") {
    ggplot2::ggsave(
      filename = output,
      plot = plot,
      width = width,
      height = height,
      units = "in",
      device = grDevices::svg
    )
  } else {
    ggplot2::ggsave(
      filename = output,
      plot = plot,
      width = width,
      height = height,
      units = "in",
      dpi = dpi
    )
  }
}

make_brackets_2x2 <- function(df, y_limits, simple_labels = NULL, interaction_label = NULL) {
  offset <- 0.18
  y_range <- diff(y_limits)
  if (!is.finite(y_range) || y_range <= 0) {
    y_range <- max(df$label_y) - min(0, df$mean)
  }
  if (!is.finite(y_range) || y_range <= 0) {
    y_range <- 1
  }

  tops <- tapply(df$label_y, df$factor_a, max)
  segments <- list()
  labels <- list()
  upper <- y_limits[2]

  if (!is.null(simple_labels)) {
    for (i in seq_along(simple_labels)) {
      label <- simple_labels[i]
      if (is.na(label) || !nzchar(label)) {
        next
      }
      base_x <- i
      y <- tops[i] + 0.06 * y_range
      y_top <- y + 0.03 * y_range
      label_y <- y_top + 0.025 * y_range

      segments[[length(segments) + 1]] <- data.frame(
        x = c(base_x - offset, base_x + offset, base_x - offset),
        xend = c(base_x - offset, base_x + offset, base_x + offset),
        y = c(y, y, y_top),
        yend = c(y_top, y_top, y_top)
      )
      labels[[length(labels) + 1]] <- data.frame(
        x = base_x,
        y = label_y,
        label = label,
        stringsAsFactors = FALSE
      )
      upper <- max(upper, label_y + 0.04 * y_range)
    }
  }

  if (!is.null(interaction_label) && nzchar(interaction_label)) {
    y <- max(tops) + 0.18 * y_range
    y_top <- y + 0.03 * y_range
    label_y <- y_top + 0.025 * y_range

    segments[[length(segments) + 1]] <- data.frame(
      x = c(1, 2, 1),
      xend = c(1, 2, 2),
      y = c(y, y, y_top),
      yend = c(y_top, y_top, y_top)
    )
    labels[[length(labels) + 1]] <- data.frame(
      x = 1.5,
      y = label_y,
      label = interaction_label,
      stringsAsFactors = FALSE
    )
    upper <- max(upper, label_y + 0.04 * y_range)
  }

  list(
    segments = do.call(rbind, segments),
    labels = do.call(rbind, labels),
    y_upper = upper
  )
}

df <- spec$cells
df$factor_a <- factor(df$factor_a, levels = spec$factor_a_levels)
df$factor_b <- factor(df$factor_b, levels = spec$factor_b_levels)
df$top_value <- if (isTRUE(spec$show_error_bars)) df$mean + df$error else df$mean
df$label_y <- df$top_value + 0.03 * diff(spec$y_limits)

brackets <- make_brackets_2x2(df, spec$y_limits, spec$simple_effect_labels, spec$interaction_label)
spec$y_limits[2] <- brackets$y_upper

p <- ggplot(df, aes(x = factor_a, y = mean, fill = factor_b)) +
  geom_col(position = position_dodge(width = 0.72), width = 0.62, color = "black", linewidth = 0.35) +
  geom_text(
    aes(y = label_y, label = format(round(mean, spec$mean_digits), nsmall = spec$mean_digits)),
    position = position_dodge(width = 0.72),
    vjust = 0,
    size = 3.4,
    family = "Times New Roman"
  ) +
  scale_fill_manual(values = stats::setNames(spec$fills, spec$factor_b_levels)) +
  scale_y_continuous(breaks = spec$y_breaks, expand = expansion(mult = c(0, 0.02))) +
  coord_cartesian(ylim = spec$y_limits, clip = "off") +
  labs(
    title = spec$title,
    subtitle = spec$subtitle,
    caption = spec$caption,
    x = spec$x_label,
    y = spec$y_label,
    fill = spec$legend_title
  ) +
  theme_jcr()

if (isTRUE(spec$show_error_bars)) {
  p <- p +
    geom_errorbar(
      aes(ymin = mean - error, ymax = mean + error),
      position = position_dodge(width = 0.72),
      width = 0.12,
      linewidth = 0.45
    )
}

if (!is.null(brackets$segments)) {
  p <- p +
    geom_segment(
      data = brackets$segments,
      aes(x = x, xend = xend, y = y, yend = yend),
      inherit.aes = FALSE,
      linewidth = 0.45
    )
}

if (!is.null(brackets$labels)) {
  p <- p +
    geom_text(
      data = brackets$labels,
      aes(x = x, y = y, label = label),
      inherit.aes = FALSE,
      family = "Times New Roman",
      size = 3.6
    )
}

save_jcr_plot(p, spec$output, spec$width, spec$height, spec$dpi)
message("Saved figure: ", normalizePath(spec$output, winslash = "/", mustWork = FALSE))
