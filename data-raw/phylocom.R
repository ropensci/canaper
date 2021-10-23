library(picante)
data(phylocom)

names(phylocom) <- c("phy", "comm", "traits")

usethis::use_data(phylocom, overwrite = TRUE)
