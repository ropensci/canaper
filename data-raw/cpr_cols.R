cpr_signif_cols <- c(
	"< 0.01" = "#a50f15", # dark red
	"< 0.025" = "#de2d26", # light red
	"not significant" = "#ffffcc", # off-white
	"> 0.975" = "#2c7fb8", # light blue
	"> 0.99" = "#253494" # dark blue
)

usethis::use_data(cpr_signif_cols, overwrite = TRUE)

cpr_endem_cols <- c(
	"paleo" = "#4876fd", # blue
	"neo" = "#fe0000", # red
	"not significant" = "#ffffcc", # off-white
	"mixed" = "#cc7efd", # light purple
	"super" = "#9d00ff" # dark purple
)

usethis::use_data(cpr_endem_cols, overwrite = TRUE)
