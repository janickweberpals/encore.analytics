# create sticker for package
library(hexSticker)
library(here)

# sticker for package
img_path <- here("sticker", "artwork.png")

sticker(
  subplot = img_path,
  s_width = 1,
  s_height = 1,
  s_x = 1.0,
  s_y= 1.15,
  package = "encore.analytics",
  h_fill = "white",
  h_color = "darkred",
  #h_size = 5,
  p_size = 50,
  p_color = "black",
  # p_x = ,
  p_y = 0.5,
  dpi = 1200,
  filename="man/figures/encore.analytics_hexagon.png"
  )
