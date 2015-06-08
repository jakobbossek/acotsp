context("ploting")

test_that("plotIteration works", {
  x = netgen::generateRandomNetwork(n.points = 15L)
  ctrl = makeAntsControl(max.iter = 5L, n.ants = 10L, rho = 0.1, trace.all = TRUE)
  res = aco(x, ctrl)
  storage = res$storage

  expect_true(!is.null(storage))

  for (iter in 1:length(storage)) {
    pl = ants:::plotIteration(x, storage = storage[[iter]])
    expect_is(pl, "ggplot", info = sprintf("Plot is o ggplot object in iteration %i", iter))
  }
})
