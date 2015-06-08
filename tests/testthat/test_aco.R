context("ants at work")

test_that("Ant System finds optimum of simple rectangluar instance", {
  # FIXME: move this to netgen (as well as makeGridNetwork)
  buildRectangleInstance = function(lower, upper) {
    x = as.matrix(expand.grid(lower:upper, lower:upper))
    coordinates = lapply(1:nrow(x), function(i) {
      if ((x[i, 1] %in% c(lower, upper)) || (x[i, 2] %in% c(lower, upper)))
        return(x[i, ])
      return(c())
    })
    coordinates = do.call(rbind, coordinates)
    netgen::makeNetwork(coordinates, lower = lower, upper = upper)
  }

  # we have 8 cities/nodes with this setting, i.e., possibly (8-1)!/2 = 2520 tours
  # and there is only one optimal tour with length 8
  lower = 1L
  upper = 3L
  opt.tour.length = 8
  instance = buildRectangleInstance(lower, upper)
  n = getNumberOfNodes(instance)


  # setup(s) for the ant algorithm
  max.iter = 25L
  n.ants = c(5L, 10L)
  alphas = c(1, 1.2, 1.5)
  betas = c(1, 1.5, 2.0)

  set.seed(2)

  for (n.ant in n.ants) {
    for (alpha in alphas) {
      for (beta in betas) {
        ctrl = makeAntsControl(max.iter = max.iter, alpha = alpha, beta = beta, n.ants = n.ant)
        res = aco(instance, ctrl, show.info = FALSE)
        err.msg = sprintf("Failed for alpha = %f, beta = %f, n.ants = %f", alpha, beta, n)
        expect_equal(length(res$best.tour), n, info = err.msg)
        expect_equal(res$best.tour.length, opt.tour.length, info = err.msg)
        expect_equal(res$termination.code, 0, info = err.msg)
      }
    }
  }
})
