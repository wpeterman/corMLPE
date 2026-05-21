test_that("corMLPE constructor validates parameter bounds", {
  expect_error(corMLPE(value = 0), "between 0 and 0.5")
  expect_error(corMLPE(value = 0.5), "between 0 and 0.5")
  expect_s3_class(corMLPE(value = 0.25), "corMLPE")
})

test_that("simulate_IBD_corMLPE returns expected structure", {
  dat <- simulate_IBD_corMLPE(
    sets = 2,
    elements = c(6, 5),
    intercept = 0.1,
    slope = 0.2,
    correlation = 0.2,
    residual_sd = 1.1,
    seed = 101
  )

  expect_true(is.data.frame(dat))
  expect_true(all(c("y", "pop1", "pop2", "set", "x") %in% names(dat)))
  expect_equal(attr(dat, "seed"), 101)
  expect_equal(length(unique(dat$set)), 2)
  expect_true(nrow(dat) > 0)
})

test_that("simulate_IBD_corMLPE validates correlation and distances input", {
  expect_error(
    simulate_IBD_corMLPE(correlation = 0.5),
    "0 < rho < 0.5"
  )
  expect_error(
    simulate_IBD_corMLPE(correlation = 0),
    "0 < rho < 0.5"
  )

  bad_dist <- matrix(1, 3, 3)
  expect_error(
    simulate_IBD_corMLPE(sets = 2, elements = c(3, 3), distances = bad_dist),
    "Distance matrices must be supplied as a list"
  )
})

test_that("simulate_corMLPE_residuals works with fitted gls object", {
  skip_if_not_installed("nlme")
  dat <- simulate_IBD_corMLPE(sets = 1, elements = 8, seed = 202)
  fit <- nlme::gls(
    y ~ x,
    data = dat,
    correlation = corMLPE(form = ~pop1 + pop2)
  )

  sims <- simulate_corMLPE_residuals(fit, n = 3, seed = 999)
  expect_true(is.matrix(sims))
  expect_equal(ncol(sims), 3)
  expect_equal(nrow(sims), nrow(dat))

  expect_error(simulate_corMLPE_residuals(list(), n = 2))
  expect_error(simulate_corMLPE_residuals(fit, n = 0))
})
