test_that("IWantHueR::euclidean_dist() produces the same results as stats::dist()", {
  a <- c(0,1,0)
  b <- c(2,0,0)
  m <- rbind(a, b)
  expect_equal(euclidean_dist(a,b), as.numeric(dist(m)))
})

