# xyz_to_rgb
test_that("IWantHueR:::xyz_to_rgb() uses the correct functions for gamma correction",{
  r <- 0.2000
  g <- 0.0002
  b <- 0

  rgb <- cbind(r,g,b)
  testthat::expect_equal(sapply(rgb, function(x){xyz_to_rgb(x)}), c(124, 1, 0))
})

