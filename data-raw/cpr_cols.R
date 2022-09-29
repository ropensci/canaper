library(RColorBrewer)
library(tidyverse)

# - Randomization test significance (colorbrewer CVD safe)
colorb_paired <- brewer.pal(n = 8, name = "Paired")
colorb_ylgn <- brewer.pal(n = 5, name = "YlGn")
cpr_signif_cols <-
  c(
    "< 0.01" = colorb_paired[[6]], #  Dark red
    "< 0.025" = colorb_paired[[5]], # Light red
    "not significant" = colorb_ylgn[[1]], # Beige
    "> 0.975" = colorb_paired[[1]], # Light blue
    "> 0.99" = colorb_paired[[2]] # Dark blue
  )

usethis::use_data(cpr_signif_cols, overwrite = TRUE)

# - Randomization, grey version
cpr_signif_cols_2 <-
  c(
    "< 0.01" = colorb_paired[[6]], #  Dark red
    "< 0.025" = colorb_paired[[5]], # Light red
    "not significant" = "grey90", # Light Grey
    "> 0.975" = colorb_paired[[1]], # Light blue
    "> 0.99" = colorb_paired[[2]] # Dark blue
  )

usethis::use_data(cpr_signif_cols_2, overwrite = TRUE)

# - Randomization, Mishler 2014
mishler_signif_cols <-
  c(
    "< 0.01" = "#7D170E", #  Dark red
    "< 0.025" = "#E73323", # Light red
    "not significant" = "#FAFAD6", # Beige
    "> 0.975" = "#5577F7", # Light blue
    "> 0.99" = "#2E4086" # Dark blue
  )

usethis::use_data(mishler_signif_cols, overwrite = TRUE)

# - Mishler 2014
# original biodiverse colors
mishler_endem_cols <-
  c(
    "neo" = "#E73323",
    "paleo" = "#5577F7",
    "not significant" = "#FAFAD6",
    "mixed" = "#BF84F8",
    "super" = "#8E25F6"
  )

usethis::use_data(mishler_endem_cols, overwrite = TRUE)

# - CANAPE (Okabe-Ito CVD safe)
cpr_endem_cols <-
  c(
    "paleo" = "#0072B2", # dark blue
    "neo" = "#D55E00", # red
    "not significant" = colorb_ylgn[[1]], # Beige
    "mixed" = "#009E73", # green
    "super" = "#F0E442" # yellow
  )

usethis::use_data(cpr_endem_cols, overwrite = TRUE)

# - CANAPE, grey version (2)
cpr_endem_cols_2 <-
  c(
    "paleo" = "#0072B2", # dark blue
    "neo" = "#D55E00", # red
    "not significant" = "grey90", # Light Grey
    "mixed" = "#009E73", # green
    "super" = "#F0E442" # yellow
  )

usethis::use_data(cpr_endem_cols_2, overwrite = TRUE)

# - CANAPE, grey version
cpr_endem_cols_3 <-
  c(
    "paleo" = "#0072B2", # dark blue
    "neo" = "#D55E00", # red
    "not significant" = "grey90", # Light Grey
    "mixed" = "#009E73", # green
    "super" = "#E69F00" # orange
  )

usethis::use_data(cpr_endem_cols_3, overwrite = TRUE)

# - CANAPE, grey version
cpr_endem_cols_4 <-
  c(
    "paleo" = "#56B4E9", # light blue
    "neo" = "#D55E00", # red
    "not significant" = "grey90", # Light Grey
    "mixed" = "#009E73", # green
    "super" = "#E69F00" # orange
  )

usethis::use_data(cpr_endem_cols_4, overwrite = TRUE)

# Test the colors, check they are CVD-safe
# - cpr_signif_cols
tibble(
  var_a = names(cpr_signif_cols),
  var_b = seq_along(cpr_signif_cols),
  var_c = seq_along(cpr_signif_cols),
) |>
  ggplot(aes(x = var_b, y = var_c, color = var_a)) +
  geom_point(size = 3) +
  scale_color_manual(values = cpr_signif_cols) +
  theme_bw()

colorBlindness::cvdPlot()

# - cpr_signif_cols_2
tibble(
  var_a = names(cpr_signif_cols_2),
  var_b = seq_along(cpr_signif_cols_2),
  var_c = seq_along(cpr_signif_cols_2),
) |>
  ggplot(aes(x = var_b, y = var_c, color = var_a)) +
  geom_point(size = 3) +
  scale_color_manual(values = cpr_signif_cols_2) +
  theme_bw()

colorBlindness::cvdPlot()

# - cpr_endem_cols
tibble(
  var_a = names(cpr_endem_cols),
  var_b = seq_along(cpr_endem_cols),
  var_c = seq_along(cpr_endem_cols),
) |>
  ggplot(aes(x = var_b, y = var_c, color = var_a)) +
  geom_point(size = 3) +
  scale_color_manual(values = cpr_endem_cols) +
  theme_bw()

colorBlindness::cvdPlot()

# - cpr_endem_cols_2
tibble(
  var_a = names(cpr_endem_cols_2),
  var_b = seq_along(cpr_endem_cols_2),
  var_c = seq_along(cpr_endem_cols_2),
) |>
  ggplot(aes(x = var_b, y = var_c, color = var_a)) +
  geom_point(size = 3) +
  scale_color_manual(values = cpr_endem_cols_2) +
  theme_bw()

colorBlindness::cvdPlot()
