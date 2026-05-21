test_that("corMaternMLPE constructor and initialization checks", {
  obj <- corMaternMLPE(value = c(0.2, 0.4), nu = 2, form = ~pop1 + pop2 | set)
  expect_s3_class(obj, "corMaternMLPE")

  dat <- simulate_IBD_corMLPE(sets = 1, elements = 6, seed = 123)
  labs <- unique(c(dat$pop1, dat$pop2))
  dmat <- as.matrix(stats::dist(matrix(seq_along(labs), ncol = 1)))
  rownames(dmat) <- labs
  colnames(dmat) <- labs

  objd <- corMaternMLPE(value = c(0.2, 0.4), nu = 2, form = ~pop1 + pop2, distances = dmat)
  expect_s3_class(objd, "corMaternMLPE")
  expect_error(Initialize(objd, dat), NA)
})

test_that("corNMLPE constructor and basic validation paths", {
  obj <- corNMLPE(form = ~pop1 + pop2 | set)
  expect_s3_class(obj, "corNMLPE")

  dat <- simulate_IBD_corMLPE(sets = 1, elements = 6, seed = 321)
  labs <- unique(c(dat$pop1, dat$pop2))
  clusters <- setNames(rep(c("a", "b"), length.out = length(labs)), labs)

  # cluster-level distances for clusters a/b
  clust_names <- unique(clusters)
  dmat <- matrix(c(0, 1, 1, 0), 2, 2, dimnames = list(clust_names, clust_names))

  objd <- corNMLPE(
    form = ~pop1 + pop2,
    clusters = clusters,
    distances = dmat
  )
  expect_s3_class(objd, "corNMLPE")
  expect_error(Initialize(objd, dat), NA)
})

test_that("corNMLPE2 constructor and initialization", {
  obj <- corNMLPE2(form = ~pop1 + pop2 | set)
  expect_s3_class(obj, "corNMLPE2")

  dat <- simulate_IBD_corMLPE(sets = 1, elements = 6, seed = 222)
  labs <- unique(c(dat$pop1, dat$pop2))
  clusters <- setNames(rep(c("a", "b"), length.out = length(labs)), labs)
  objd <- corNMLPE2(form = ~pop1 + pop2, clusters = clusters)
  expect_error(Initialize(objd, dat), NA)
})
