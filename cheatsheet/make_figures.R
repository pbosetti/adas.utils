# Generates the thumbnails used by cheatsheet.html.
# Run from the package root, outside renv:  Rscript cheatsheet/make_figures.R
suppressMessages({
  library(adas.utils)
  library(dplyr)
  library(ggplot2)
})

out <- "cheatsheet/figures"
dir.create(out, showWarnings = FALSE)
blue <- "#1d5c8a"
orange <- "#e0731f"
green <- "#3f9b4f"

theme_cs <- function(...) {
  theme_minimal(base_size = 6) +
    theme(
      plot.title = element_blank(), plot.subtitle = element_blank(),
      plot.margin = margin(3, 3, 3, 3),
      legend.key.size = unit(7, "pt"),
      panel.grid.minor = element_blank(),
      ...
    )
}
# Package plots use default text sizes, too big for thumbnails
shrink_text <- function(p, size = 1.7) {
  for (i in seq_along(p$layers)) {
    if (inherits(p$layers[[i]]$geom, c("GeomText", "GeomLabel"))) p$layers[[i]]$aes_params$size <- size
  }
  p
}
save <- function(p, name, w = 1.7, h = 1.25) {
  ggsave(file.path(out, paste0(name, ".png")), p,
         width = w, height = h, dpi = 300, device = ragg::agg_png, bg = "white")
}

# --- DoE concept figures ------------------------------------------------------
proj <- function(d) mutate(d, px = x + 0.55 * z, py = y + 0.4 * z)
v <- expand.grid(x = c(-1, 1), y = c(-1, 1), z = c(-1, 1)) %>%
  mutate(keep = x * y * z == 1) %>% proj()
e <- bind_rows(
  expand.grid(y = c(-1, 1), z = c(-1, 1)) %>% mutate(x = -1, xe = 1, ye = y, ze = z),
  expand.grid(x = c(-1, 1), z = c(-1, 1)) %>% mutate(y = -1, xe = x, ye = 1, ze = z),
  expand.grid(x = c(-1, 1), y = c(-1, 1)) %>% mutate(z = -1, xe = x, ye = y, ze = 1)
) %>% proj() %>% mutate(pxe = xe + 0.55 * ze, pye = ye + 0.4 * ze)
p <- ggplot() +
  geom_segment(aes(px, py, xend = pxe, yend = pye), e, colour = "grey60", linewidth = 0.3) +
  geom_point(aes(px, py, fill = keep), v, shape = 21, size = 3.2, stroke = 0.4) +
  scale_fill_manual(values = c(`TRUE` = blue, `FALSE` = "white"), guide = "none") +
  annotate("text", x = 0, y = -1.3, label = "A", size = 2.5, fontface = "bold") +
  annotate("text", x = -1.3, y = 0, label = "B", size = 2.5, fontface = "bold") +
  annotate("text", x = 1.48, y = -0.95, label = "C", size = 2.5, fontface = "bold") +
  coord_equal() + theme_void()
save(p, "cube_fraction", 1.3, 1.1)

ccd <- bind_rows(
  expand.grid(A = c(-1, 1), B = c(-1, 1)) %>% mutate(type = "factorial"),
  tibble(A = 0, B = 0, type = "center"),
  tibble(A = c(-1, 1, 0, 0) * sqrt(2), B = c(0, 0, -1, 1) * sqrt(2), type = "axial")
)
p <- ggplot(ccd, aes(A, B)) +
  annotate("rect", xmin = -1, xmax = 1, ymin = -1, ymax = 1, fill = NA, colour = "grey60", linewidth = 0.3) +
  annotate("path", x = sqrt(2) * cos(seq(0, 2 * pi, length.out = 100)),
           y = sqrt(2) * sin(seq(0, 2 * pi, length.out = 100)), linetype = 2, colour = "grey70", linewidth = 0.3) +
  geom_segment(aes(x = 0, y = 0, xend = A, yend = B), data = filter(ccd, type == "axial"), colour = green, linewidth = 0.3) +
  geom_point(aes(colour = type), size = 2.4) +
  scale_colour_manual(values = c(factorial = blue, center = orange, axial = green), guide = "none") +
  coord_equal() + theme_void()
save(p, "ccd", 1.2, 1.2)

# --- Names, scales and predictions -------------------------------------------
fp <- fp_design_matrix(2, rep = 3) %>%
  fp_add_names(A = "Temperature (°C)", B = "Pressure (bar)") %>%
  fp_add_scale(A = c(20, 30), B = c(2, 3))
fp$Y <- ccd_experiment_yield$base
fp.lm <- lm(Y ~ A * B, data = fp)

p <- fp %>%
  add_predictions(fp.lm, interval = "confidence") %>%
  mutate(B = factor(B)) %>%
  ggplot(aes(x = A, y = fit, colour = B, fill = B)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2, colour = NA) +
  geom_line() +
  scale_x_continuous(sec.axis = fp_scaled_axis(fp, "A")) +
  scale_colour_manual(values = c(blue, orange), labels = fp_scale(fp, "B"), aesthetics = c("colour", "fill")) +
  labs(colour = fp_name(fp, "B"), fill = fp_name(fp, "B"), y = "Yield") +
  theme_cs(legend.position = "inside", legend.position.inside = c(0.25, 0.78),
           legend.title = element_text(size = 5), legend.text = element_text(size = 5))
save(p, "interaction", 1.35, 1.2)

p <- expand.grid(A = seq(-1, 1, length.out = 60), B = seq(-1, 1, length.out = 60)) %>%
  add_predictions(fp.lm, var = "Y") %>%
  ggplot(aes(A, B, z = Y)) +
  geom_contour_filled(bins = 7) +
  scale_fill_viridis_d(option = "G", guide = "none") +
  scale_x_continuous(sec.axis = fp_scaled_axis(fp, "A"), expand = c(0, 0)) +
  scale_y_continuous(sec.axis = fp_scaled_axis(fp, "B"), expand = c(0, 0)) +
  theme_cs()
save(p, "surface", 1.35, 1.2)

# --- Alias matrix -------------------------------------------------------------
p <- fp_alias_matrix(~A*B*C, ~B*C*D) %>%
  plot() + theme_cs(axis.text.x = element_text(angle = 90, size = 4, vjust = 0.5, hjust = 1),
                    axis.text.y = element_text(size = 4), legend.position = "none",
                    axis.title = element_blank())
save(p, "alias_matrix", 1.6, 1.6)

# --- Effect screening ---------------------------------------------------------
f.lm <- lm(Y ~ A*B*C*D, data = filtration)
save(shrink_text(daniel_plot_qq(f.lm), 1.5) + labs(x = "theoretical quantiles", y = "effects") + theme_cs(), "daniel_qq")
save(shrink_text(daniel_plot_hn(f.lm, label_n = 5)) + labs(x = "half-normal quantiles", y = "|effects|") + theme_cs(), "daniel_hn")
save(pareto_chart(f.lm) + labs(x = NULL) +
       theme_cs(axis.text.x = element_text(angle = 90, size = 3.5, vjust = 0.5, hjust = 1),
                legend.position = "none"), "pareto_lm")

set.seed(1)
p <- tibble(val = rnorm(10, sd = 5), cat = LETTERS[1:10]) %>%
  ggplot(aes(x = cat, y = val)) +
  geom_pareto_bars(fill = blue) +
  geom_pareto_line(colour = orange) +
  geom_pareto_point(colour = orange, size = 0.8) +
  scale_y_pareto() + labs(x = NULL, y = NULL) + theme_cs()
save(p, "pareto_geom")

set.seed(1)
df <- data.frame(
  group = rep(c("ctrl", "trt"), each = 60),
  value = c(rnorm(60), rnorm(60, 0.4, 1.4))
)
df$id <- paste0("obs_", seq_len(nrow(df)))
p <- ggplot(df, aes(sample = value, labels = id, colour = group, fill = group, group = group)) +
  geom_qqhn_band(alpha = 0.15, colour = NA) +
  geom_qqhn(size = 0.6) +
  geom_qqhn_line(linewidth = 0.3) +
  scale_colour_manual(values = c(blue, orange), aesthetics = c("colour", "fill")) +
  labs(x = "half-normal quantiles", y = "|sample|") +
  theme_cs(legend.position = "inside", legend.position.inside = c(0.2, 0.8),
           legend.title = element_blank(), legend.text = element_text(size = 5))
save(p, "qqhn")

# --- Tests & diagnostics ------------------------------------------------------
p <- battery %>%
  mutate(Material = LETTERS[Material]) %>%
  ggTukey(Response ~ Temperature, splt = ~Material) +
  theme_cs(legend.position = "top", legend.title = element_blank(), legend.margin = margin(),
           legend.box.spacing = unit(0, "pt"))
save(p, "tukey", 2.3, 1.25)

set.seed(2)
p <- tibble(x = rnorm(60, 20, 5)) %>% normplot(x, breaks = c(0.05, 0.25, 0.5, 0.75, 0.95), linecolor = orange) + theme_cs()
save(p, "normplot")

set.seed(3)
x <- c(rnorm(25), 3.6)
p <- tibble(i = seq_along(x), x = x, out = i == which.max(abs(x - mean(x)))) %>%
  ggplot(aes(i, x, colour = out)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = mean(x) - 2 * sd(x), ymax = mean(x) + 2 * sd(x),
           fill = blue, alpha = 0.08) +
  geom_hline(yintercept = mean(x), colour = "grey60", linewidth = 0.3) +
  geom_point(size = 1) +
  scale_colour_manual(values = c(`FALSE` = blue, `TRUE` = orange), guide = "none") +
  labs(x = "index", y = "x") + theme_cs()
save(p, "chauvenet", 1.5, 1.1)
