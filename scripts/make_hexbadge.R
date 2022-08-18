library(hexSticker)

library(showtext)
## Loading Google fonts (http://www.google.com/fonts)
font_add_google("lobster")
## Automatically use showtext to render text for future devices
showtext_auto()

sticker(
  "man/figures/NMA_logo.png",
  package = "",
  p_size = 0,
  s_x = 1,
  s_y = 1,
  s_width = 0.7,
  filename = "man/figures/hexbadge.svg",
  h_fill = "white",
  h_color = "lightgreen",
  p_y = 1.5,
  p_color = "brown",
  p_family = "lobster",
  spotlight = FALSE,
  url = "https://github.com/ICON-in-R/NMA",
  u_size = 1.5,
  u_y = 0.05,
  l_alpha = 1,
  l_y = 0.85)
