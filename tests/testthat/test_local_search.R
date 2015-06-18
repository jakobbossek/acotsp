context("local search")

test_that("Local Search is applied.", {
  instance = buildRectangleInstance(1, 3)

  # Here we apply local search in each iteration. The LS function always returns
  # the string 1,...,n. Thus we check in the end, if the best tour corresponds to
  # that sequence.
  myLocalSearchFun = function(x, initial.tour) {
    n = length(initial.tour)
    # always return 1, 2, ... , n
    return(seq(n))
  }
  max.iter = 2L
  ctrl = makeACOTSPControl(
    max.iter = max.iter, n.ants = 2L,
    local.search.fun = myLocalSearchFun, local.search.step = seq(max.iter)
  )
  res = runACOTSP(instance, ctrl)
  expect_true(all.equal(res$best.tour, seq(getNumberOfNodes(instance))))
})
