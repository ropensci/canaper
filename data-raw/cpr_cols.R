library(RColorBrewer)

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

# Test the colors, check they are CVD-safe
# - cpr_signif_cols
tibble(
  var_a = names(cpr_signif_cols),
  var_b = 1:length(cpr_signif_cols),
  var_c = 1:length(cpr_signif_cols),
) |>
  ggplot(aes(x = var_b, y = var_c, color = var_a)) +
  geom_point(size = 3) +
  scale_color_manual(values = cpr_signif_cols)

colorBlindness::cvdPlot()

# - cpr_endem_cols
tibble(
  var_a = names(cpr_endem_cols),
  var_b = 1:length(cpr_endem_cols),
  var_c = 1:length(cpr_endem_cols),
) |>
  ggplot(aes(x = var_b, y = var_c, color = var_a)) +
  geom_point(size = 3) +
  scale_color_manual(values = cpr_endem_cols)

colorBlindness::cvdPlot()
