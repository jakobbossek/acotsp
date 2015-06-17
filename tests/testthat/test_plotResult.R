context("test autoplot function")

test_that("plot list is generated", {
  lower = 1L
  upper = 3L
  instance = buildRectangleInstance(lower, upper)

  max.iter = 3L
  res = runAS(instance, trace.all = TRUE, max.iter = max.iter)

  pls = plotResult(res, step.size = 1L)
  expect_equal(length(pls), max.iter)
  for (i in seq_along(pls)) {
    expect_is(pls[[i]], "ggplot", info = sprintf("Did not generate plot for iteration %i", i))
  }
})
