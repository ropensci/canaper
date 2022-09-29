# Script to generate logo
library(tidyverse)
library(canaper)
library(hexSticker)
library(magick)

# So we get the same background plot each time
set.seed(142)

# Make 20x20 grid simulating endemism
# this will be the sticker background
df <- expand.grid(x = 1:20, y = 1:20) %>%
  as_tibble() %>%
  mutate(
    endem = sample(
      x = names(cpr_endem_cols),
      size = 20 * 20,
      replace = TRUE,
      # 30x more likely to get non-significant
      prob = c(1, 1, 30, 1, 1)
    )
  ) %>%
  mutate(
    endem = case_when(
      # Make an empty space -2 and +2 around middle for printing package name
      y > 8 & y < 12 ~ "not significant",
      TRUE ~ endem
    )
  )

# Save this as background
ggplot(df, aes(x, y)) +
  geom_tile(aes(fill = endem)) +
  scale_fill_manual(values = mishler_endem_cols) +
  theme_void() +
  theme(
    legend.position = "none"
  )

ggsave("data-raw/logo_back.png")

# Generate sticker: background extends beyond hex
canaper_s <- sticker(
  "data-raw/logo_back.png",
  package = "canaper",
  p_x = 1,
  p_y = 1,
  p_family = "mono",
  p_size = 8,
  p_color = "grey10",
  s_x = 1,
  s_y = 1,
  s_width = 1.2,
  s_height = 40,
  dpi = 600,
  asp = 1,
  h_fill = mishler_endem_cols["not significant"],
  h_color = "grey10",
  h_size = 1.2,
  url = "https://github.com/ropensci/canaper",
  u_family = "sans",
  u_size = 1.45,
  u_y = 0.08,
  u_x = 1,
  # Make area around hex white
  white_around_sticker = TRUE,
  filename = "man/figures/logo.png"
)

# Convert area around hex to transparent
# https://github.com/GuangchuangYu/hexSticker/issues/39
image_read("man/figures/logo.png") %>%
  image_fill(color = "transparent", refcolor = "white", fuzz = 20, point = "+1+1") %>% # nolint
  image_fill(color = "transparent", refcolor = "white", fuzz = 20, point = "+1034+1") %>% # nolint
  image_fill(color = "transparent", refcolor = "white", fuzz = 20, point = "+1+1198") %>% # nolint
  image_fill(color = "transparent", refcolor = "white", fuzz = 20, point = "+1034+1198") %>% # nolint
  image_write("man/figures/logo.png")
