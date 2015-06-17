#' @title Simple monitoring function.
#'
#' @description
#' This is the default monitoring function used by \pkg{ants}. It simply outputs
#' the iteration as well as minimal, mean and maximal target values from the current
#' population.
#'
#' @param show.info.stepsize [\code{integer(1)}]\cr
#'   Adjust the stepsize of iterations with informative messages.
#' @return [\code{AntsMonitor}]
#'
#' @export
makeConsoleMonitor = function(show.info.stepsize = 5L) {
  force(show.info.stepsize)
  assertInteger(show.info.stepsize, len = 1L, lower = 1L, upper = 100L, any.missing = FALSE)

  makeMonitor(
    before = function(envir = parent.frame()) {
      cat("Initialization finished! Starting optimization process ...\n")
    },
    step = function(envir = parent.frame()) {
      max.iter = envir$control$max.iter
      opt.path = envir$opt.path
      iter = envir$iter
      if ((iter %% show.info.stepsize) == 0L) {
        tour.lengths = getOptPathCol(op = opt.path, name = "tour.length")
        best.idx = which.min(tour.lengths)
        catf("Iter %i:", iter)
        catf("Global best tour length: %f", tour.lengths[best.idx])
        catf("Iteration best tour length: %f", tour.lengths[iter])
      }
    },
    after = function(envir = parent.frame()) {
      cat("Finished!\n")
    }
  )
}
