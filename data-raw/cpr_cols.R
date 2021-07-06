# all significance colors are from colorbrewer https://colorbrewer2.org
cpr_signif_cols <- c(
	"< 0.01" = "#a50f15", # dark red
	"< 0.025" = "#de2d26", # light red
	"not significant" = "#ffffcc", # off-white
	"> 0.975" = "#2c7fb8", # light blue
	"> 0.99" = "#253494" # dark blue
)

usethis::use_data(cpr_signif_cols, overwrite = TRUE)

# endemism colors are from microshades or colorbrewer (https://karstenslab.github.io/microshades/)
cpr_endem_cols <- c(
	"paleo" = "#7DCCFF", # blue (microshades)
	"neo" = "#F09163", # orange (microshades)
	"not significant" = "#ffffcc", # off-white (colorbrewer)
	"mixed" = "#E794C1", # light purple (microshades)
	"super" = "#CC79A7"  # dark purple (microshades)
)

usethis::use_data(cpr_endem_cols, overwrite = TRUE)
