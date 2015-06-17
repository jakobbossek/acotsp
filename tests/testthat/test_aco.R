context("ants at work")

test_that("Ant System finds optimum of simple rectangluar instance", {
  # we have 8 cities/nodes with this setting, i.e., possibly (8-1)!/2 = 2520 tours
  # and there is only one optimal tour with length 8
  lower = 1L
  upper = 3L
  opt.tour.length = 8
  instance = buildRectangleInstance(lower, upper)
  n = getNumberOfNodes(instance)

  expect_found_optimum = function(result, err.msg) {
    expect_equal(length(res$best.tour), n, info = err.msg)
    expect_equal(res$best.tour.length, opt.tour.length, info = err.msg)
    expect_equal(res$termination.code, 0, info = err.msg)
    expect_output(print(res), "Ants found solution")
    expect_true(is.numeric(getTour(res)))
  }

  # setup(s) for the ant algorithm
  max.iter = 25L
  n.ants = c(5L, 10L)
  alphas = c(1, 1.2, 1.5)
  betas = c(1, 1.5, 2.0)

  set.seed(2)

  # test different values of the default ACO parameters
  for (n.ant in n.ants) {
    for (alpha in alphas) {
      for (beta in betas) {
        ctrl = makeAntsControl(max.iter = max.iter, alpha = alpha, beta = beta, n.ants = n.ant)
        expect_output(ctrl, "Ants Control Object")
        res = aco(instance, ctrl)
        err.msg = sprintf("Failed for alpha = %f, beta = %f, n.ants = %f", alpha, beta, n)
        expect_found_optimum(res, err.msg)
      }
    }
  }

  # test different n.elite values
  ns.elite = c(3L, 7L)
  for (n.elite in ns.elite) {
    ctrl = makeAntsControl(max.iter = 10L, n.ants = 10L, n.elite = n.elite)
    res = aco(instance, ctrl)
    err.msg = sprintf("Failed to find optimum for n.elite = %i.", n.elite)
    expect_found_optimum(res, err.msg)
  }

  # test that using only the global best tour works
  ctrl = makeAntsControl(max.iter = 10L, n.ants = 10L, use.global.best = TRUE, best.deposit.only = TRUE)
  res = aco(instance, ctrl)
  expect_found_optimum(res, err.msg = sprintf("Failed to find optimum with only global best deposit (ACS)."))

  # CHECK IF CONTROL OBJECT COMPUTATION FAILS

  # at least one ant must participate in pheromone update
  expect_error(makeAntsControl(n.elite = 0L, use.global.best = FALSE))

  # n.elite needs to be at most n.ants
  expect_error(makeAntsControl(n.ants = 10L, n.elite = 11L))

  # local search function given, but not called anytime
  expect_warning(makeAntsControl(local.search.fun = function(x, initial.tour) initial.tour))
})
