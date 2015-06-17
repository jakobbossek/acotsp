#' @title Plots optimization progress.
#'
#' @description
#' This function generates \code{\link[ggplot2]{ggplot}} objects for each iteration
#' specified by \code{step.size} and returns these plots in a list. Moreover the
#' plots can be rendered directly via the option \code{pause}.
#'
#' @param object [\code{AntsResult}]\cr
#'   Return value of \code{\link{aco}} or one of the shortcut functions, e.g.,
#'   \code{\link{runAS}}.
#' @param step.size [\code{integer(1)}]\cr
#'   Which iterations shall be plotted? Default is \code{1L}.
#' @param pause [\code{logical(1)}]\cr
#'   Should each plot be displayed after generation? Default is \code{FALSE}.
#' @param ... [any]\cr
#'   Currently not used.
#' @note This function requires the storage of the entire optimization trace,
#'   i. e., set the parameter \code{trace.all} of \code{makeAntsControl} to
#'   \code{TRUE}.
#'
#' @return [\code{list}] A named list of \code{\link[ggplot2]{ggplot}} objects.
plotResult = function(object, step.size = 1L, pause = FALSE, ...) {
  assertInteger(step.size, len = 1L, lower = 1L, upper = object$iters.done)
  assertFlag(pause)
  storage = object$storage
  if (is.null(storage)) {
    stopf("Result can not be plotted! Storage is empty. You need to set the trace.all
      parameter in the AntsControl object.")
  }
  network = object$network
  reached.iter = object$iters.done
  iter = 1L
  pls = list()
  while (iter <= reached.iter && !is.null(storage[[iter]])) {
    pl = plotIteration(network, storage = storage[[iter]])
    # store the plot
    pls[[paste0("iter_", iter)]] = pl
    if (pause) {
      print(pl)
      pause()
    }
    iter = iter + step.size
  }
  return(pls)
}
