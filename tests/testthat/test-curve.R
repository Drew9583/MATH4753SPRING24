test_that("curve works", {
  result = MATH4753SPRING24::myncurve(mu = 10, sigma = 5, a = 6)
  expect_equal(result$mu, 10)
  expect_equal(result$sigma, 5)
  expect_type(result$probability, "double")
})
