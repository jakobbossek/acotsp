context("test shortcut functions, e.g., runAS, runMMAS, ...")

test_that("shortcuts work", {
  # not much to do here, since we check for a vast number of parameter combinations
  # in test_aco.R. Here we simply check the wrapper code to call aco appropriately
  lower = 1L
  upper = 3L
  instance = buildRectangleInstance(lower, upper)
  n = getNumberOfNodes(instance)

  expect_aco_result = function(result, err.msg) {
    expect_equal(length(res$best.tour), n, info = err.msg)
    expect_equal(res$termination.code, 0, info = err.msg)
    expect_output(print(res), "Ants found solution")
    expect_true(is.numeric(getTour(res)))
  }

  # We thus perform only a single iteration and do force the solver to find the
  # global optimum
  max.iter = 1L

  # call Ant-System with defaults
  res = runAS(instance, max.iter = max.iter)
  expect_aco_result(res, err.msg = "runAS fails.")

  # call Ant-Colony-System
  res = runACS(instance, max.iter = max.iter)
  expect_aco_result(res, err.msg = "runACS fails.")

  # call MAXMIN-Ant-System
  res = runMMAS(instance, max.iter = max.iter)
  expect_aco_result(res, err.msg = "runMMAS fails.")
})
