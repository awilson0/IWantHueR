full_rgb <- as.matrix(expand.grid(rep(list(0:255), 3)))/255
full_Lab <- convertColor(full_rgb, "sRGB", "Lab")


usethis::use_data(full_rgb, full_Lab, internal = TRUE, overwrite = TRUE)
