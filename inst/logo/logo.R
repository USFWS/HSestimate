library(hexSticker)
library(usmap)
library(ggplot2)
library(dplyr)
library(png)
library(cowplot)

sysfonts::font_add_google("Lato", "pf", regular.wt = 700)

mall <- readPNG("man/figures/mall_silhouette_sm.png", native = TRUE)

usa49 <-
  plot_usmap(
    regions = "state",
    data = statepop,
    values = "pop_2022",
    fill = "#FFFFFF", color = "#0270bb", exclude = "Hawaii") +
  theme(
    plot.background = element_rect(fill = NA, color = NA),
    plot.margin = unit(c(0, .2, 0, 0), "cm"))

map_mall <-
  ggdraw(usa49) +
  draw_image(mall, x = .03, y = .15, width = .95, height = .9, scale = 0.9)

# PNG sticker
hexSticker::sticker(
  map_mall,
  package = "HSestimate", p_color = "#FFFFFF",
  p_size = 19, s_x = 1, s_y = .75, s_width = 1.4, s_height = .9,
  h_fill = "#faa225", h_color = "#ffd151",
  dpi = 320,
  filename = "man/figures/logo.png")

# SVG sticker
hexSticker::sticker(
  map_mall,
  package = "HSestimate", p_color = "#FFFFFF",
  p_size = 6, s_x = 1, s_y = .75, s_width = 1.4, s_height = .9,
  h_fill = "#faa225", h_color = "#ffd151",
  dpi = 320,
  filename = "man/figures/logo.svg")
