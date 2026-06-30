# ============================================================
# Mock coefficient plot template: online follower behaviors
# ============================================================
#
# Purpose:
# This script generates a publication-style mock coefficient plot.
# The data below are fake and are intended only for design prototyping.
# Replace the mock df with real estimates before using the figure in the paper.
#
# Required real-data columns:
# panel, family, outcome, estimate, ci_low, ci_high,
# control_mean, control_sd, outcome_range
#
# Outputs:
# - mock_average_treatment_effect_online_behaviors.png
# - mock_average_treatment_effect_online_behaviors.pdf
# ============================================================

# ----------------------------
# 0. Packages
# ----------------------------

library(tidyverse)
library(patchwork)
library(scales)
library(grid)

# ----------------------------
# 1. Mock data
# ----------------------------

# IMPORTANT:
# These values are fake and are only used to reproduce the visual layout.
# Replace this entire tribble with the real estimates and summary statistics.

df <- tribble(
  ~panel, ~family, ~outcome, ~estimate, ~ci_low, ~ci_high, ~control_mean, ~control_sd, ~outcome_range,
  
  "Panel A.", "Overall engagement", "log(Total reactions)", -0.14, -0.25, -0.04, 3.45, 1.21, "(0.00, 9.73)",
  "Panel A.", "Overall engagement", "log(Total comments)",  -0.11, -0.20,  0.04, 2.31, 1.08, "(0.00, 8.64)",
  "Panel A.", "Overall engagement", "log(Total shares)",    -0.08, -0.18,  0.05, 2.87, 1.23, "(0.00, 8.92)",
  
  "Panel B.", "Posting and veracity behavior", "log(Number of posts + shares)",          -0.08,  -0.16,  0.04, 1.98, 0.92, "(0.00, 7.18)",
  "Panel B.", "Posting and veracity behavior", "log(Number of posts + shares, English)", -0.07,  -0.17,  0.04, 1.41, 0.81, "(0.00, 6.32)",
  "Panel B.", "Posting and veracity behavior", "log(Verifiable posts + shares)",         -0.08,  -0.19,  0.05, 1.67, 0.89, "(0.00, 6.75)",
  "Panel B.", "Posting and veracity behavior", "log(Non-verifiable posts + shares)",     -0.10,  -0.20,  0.02, 0.87, 0.73, "(0.00, 5.61)",
  "Panel B.", "Posting and veracity behavior", "log(True posts + shares)",               -0.11,  -0.22,  0.04, 1.52, 0.86, "(0.00, 6.23)",
  "Panel B.", "Posting and veracity behavior", "log(Fake posts + shares)",               -0.085, -0.20,  0.00, 0.38, 0.60, "(0.00, 3.84)",
  
  "Panel C.", "COVID-19 content", "log(COVID posts + shares)",                 0.000, -0.008, 0.015, 0.74, 0.62, "(0.00, 3.49)",
  "Panel C.", "COVID-19 content", "log(Positive COVID posts + shares)",        0.001, -0.007, 0.011, 0.29, 0.50, "(0.00, 2.60)",
  "Panel C.", "COVID-19 content", "log(Neutral COVID posts + shares)",         0.002, -0.006, 0.012, 0.32, 0.51, "(0.00, 2.46)",
  "Panel C.", "COVID-19 content", "log(Negative COVID posts + shares)",        0.000, -0.008, 0.010, 0.25, 0.46, "(0.00, 2.22)",
  "Panel C.", "COVID-19 content", "log(Vaccine posts + shares)",               0.002, -0.009, 0.015, 0.68, 0.61, "(0.00, 3.22)",
  "Panel C.", "COVID-19 content", "log(Positive vaccine posts + shares)",      0.004, -0.008, 0.012, 0.26, 0.48, "(0.00, 2.33)",
  "Panel C.", "COVID-19 content", "log(Neutral vaccine posts + shares)",       0.004, -0.008, 0.013, 0.30, 0.49, "(0.00, 2.34)",
  "Panel C.", "COVID-19 content", "log(Negative vaccine posts + shares)",      0.000, -0.010, 0.010, 0.25, 0.46, "(0.00, 2.05)"
) %>%
  mutate(
    panel = factor(panel, levels = c("Panel A.", "Panel B.", "Panel C.")),
    family = factor(
      family,
      levels = c(
        "Overall engagement",
        "Posting and veracity behavior",
        "COVID-19 content"
      )
    )
  ) %>%
  group_by(panel) %>%
  mutate(
    row_id = row_number(),
    y = rev(row_id)
  ) %>%
  ungroup()

panel_colors <- c(
  "Overall engagement" = "#0B8F8A",
  "Posting and veracity behavior" = "#006DFF",
  "COVID-19 content" = "#F15A24"
)

# ----------------------------
# 2. Helper function
# ----------------------------

make_panel <- function(panel_name, x_limits, x_breaks, show_x_title = FALSE) {
  
  d <- df %>% filter(panel == panel_name)
  
  family_label <- as.character(unique(d$family))
  family_color <- panel_colors[[family_label]]
  
  n <- nrow(d)
  y_header <- n + 1.35
  y_rule   <- n + 0.85
  
  ggplot(d) +
    
    # Header rule below panel title
    geom_segment(
      aes(x = x_limits[1], xend = x_limits[2], y = y_rule, yend = y_rule),
      inherit.aes = FALSE,
      linewidth = 0.35,
      color = "grey45"
    ) +
    
    # Solid zero line
    geom_vline(
      xintercept = 0,
      linewidth = 0.45,
      color = "black"
    ) +
    
    # Confidence intervals
    geom_errorbarh(
      aes(y = y, xmin = ci_low, xmax = ci_high),
      height = 0,
      linewidth = 0.45,
      color = "black"
    ) +
    
    # Point estimates
    geom_point(
      aes(x = estimate, y = y),
      size = 2.5,
      color = family_color
    ) +
    
    # Outcome labels on the left
    geom_text(
      aes(x = x_limits[1], y = y, label = outcome),
      hjust = 0,
      size = 4.2,
      color = "black"
    ) +
    
    # Panel title and family label
    annotate(
      "text",
      x = x_limits[1],
      y = y_header,
      label = panel_name,
      hjust = 0,
      fontface = "bold",
      size = 5.1
    ) +
    annotate(
      "text",
      x = x_limits[1] + 0.07 * diff(x_limits),
      y = y_header,
      label = family_label,
      hjust = 0,
      fontface = "bold",
      size = 5.1,
      color = family_color
    ) +
    
    # Right-side column headers
    annotate(
      "text",
      x = x_limits[2] + 0.14 * diff(x_limits),
      y = y_header,
      label = "Control mean",
      hjust = 0.5,
      fontface = "bold",
      size = 4.1
    ) +
    annotate(
      "text",
      x = x_limits[2] + 0.28 * diff(x_limits),
      y = y_header,
      label = "Control SD",
      hjust = 0.5,
      fontface = "bold",
      size = 4.1
    ) +
    annotate(
      "text",
      x = x_limits[2] + 0.44 * diff(x_limits),
      y = y_header,
      label = "Outcome range",
      hjust = 0.5,
      fontface = "bold",
      size = 4.1
    ) +
    
    # Right-side values
    geom_text(
      aes(
        x = x_limits[2] + 0.14 * diff(x_limits),
        y = y,
        label = sprintf("%.2f", control_mean)
      ),
      hjust = 0.5,
      size = 4.0
    ) +
    geom_text(
      aes(
        x = x_limits[2] + 0.28 * diff(x_limits),
        y = y,
        label = sprintf("%.2f", control_sd)
      ),
      hjust = 0.5,
      size = 4.0
    ) +
    geom_text(
      aes(
        x = x_limits[2] + 0.44 * diff(x_limits),
        y = y,
        label = outcome_range
      ),
      hjust = 0.5,
      size = 4.0
    ) +
    
    scale_x_continuous(
      limits = c(x_limits[1], x_limits[2] + 0.50 * diff(x_limits)),
      breaks = x_breaks,
      labels = scales::number_format(accuracy = 0.01)
    ) +
    scale_y_continuous(
      limits = c(0.5, n + 1.7),
      breaks = NULL
    ) +
    coord_cartesian(clip = "off") +
    labs(
      x = if (show_x_title) {
        "Average treatment effect, with 95% confidence interval"
      } else {
        NULL
      },
      y = NULL
    ) +
    theme_minimal() +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_text(size = 11, color = "black"),
      axis.title.x = element_text(size = 12, color = "black", margin = margin(t = 6)),
      axis.ticks.x = element_line(color = "black", linewidth = 0.35),
      axis.line.x = element_line(color = "black", linewidth = 0.45),
      plot.margin = margin(t = 4, r = 18, b = 4, l = 10)
    )
}

# ----------------------------
# 3. Build panels
# ----------------------------

p_a <- make_panel(
  panel_name = "Panel A.",
  x_limits = c(-0.34, 0.11),
  x_breaks = c(-0.30, -0.20, -0.10, 0.00, 0.10),
  show_x_title = FALSE
)

p_b <- make_panel(
  panel_name = "Panel B.",
  x_limits = c(-0.34, 0.11),
  x_breaks = c(-0.30, -0.20, -0.10, 0.00, 0.10),
  show_x_title = FALSE
)

p_c <- make_panel(
  panel_name = "Panel C.",
  x_limits = c(-0.024, 0.022),
  x_breaks = c(-0.02, -0.01, 0.00, 0.01, 0.02),
  show_x_title = TRUE
)

# ----------------------------
# 4. Combine figure
# ----------------------------

final_plot <- p_a / p_b / p_c +
  plot_layout(heights = c(1.1, 1.55, 1.8)) +
  plot_annotation(
    title = "Average effect of one initially-followed SMI being assigned to treatment on online follower behaviors",
    subtitle = "Aggregated effects across weeks 1–12, batch 1",
    theme = theme(
      plot.title = element_text(size = 18, face = "bold", hjust = 0),
      plot.subtitle = element_text(size = 13, color = "grey35", hjust = 0),
      plot.margin = margin(t = 8, r = 8, b = 8, l = 8)
    )
  )

final_plot

ggsave(
  filename = "mock_average_treatment_effect_online_behaviors.png",
  plot = final_plot,
  width = 14,
  height = 10,
  dpi = 300
)

ggsave(
  filename = "mock_average_treatment_effect_online_behaviors.pdf",
  plot = final_plot,
  width = 14,
  height = 10
)
