# Load package in working state
# not with library()
library(devtools)
library(knitr)
load_all()

# Precompile vignettes to shorten check time
vgnts <- c("canape.Rmd", "how-many-rand.Rmd", "parallel.Rmd")
for (vgnt in vgnts) {
  knit(paste0("vignettes/", vgnt, ".orig"), paste0("vignettes/", vgnt))
}

# Move figures into vignettes/ folder
figs <- list.files(pattern = "vigfig-")
fs::file_move(figs, fs::path("vignettes/", figs))
