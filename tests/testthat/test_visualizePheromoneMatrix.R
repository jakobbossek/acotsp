context("visualiuePheromoneMatrix")

test_that("pheromone visualization works as expected.", {
  # first create an illegal pheromone matrix, i.e., one with negative values
  pher.mat = matrix(c(-1, -0.001, 3, 4), ncol = 2L)
  expect_error(visualizePheromoneMatrix(pher.mat))

  # now create a non-square matrix
  pher.mat = matrix(1:4, ncol = 1L)
  expect_error(visualizePheromoneMatrix(pher.mat))

  # now check if a pheromone matrix is visualized correctly
  pher.mat = matrix(1:25, ncol = 5L)
  pl = visualizePheromoneMatrix(pher.mat)
  expect_is(pl, c("gg", "ggplot"))
})
