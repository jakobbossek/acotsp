#' Plots optimization progress.
#'
#' @param object [\code{AntsResult}]\cr
#'   Return value of \code{\link{aco}}.
#' @param step.size [\code{integer(1)}]\cr
#'   Which iterations shall be plotted? Default is \code{1L}.
#' @param ... [any]\cr
#'   Currently not used.
#' @note This function requires the storage of the entire optimization trace,
#'   i. e., set the parameter \code{trace.all} of \code{aco} to \code{TRUE}.
#' @return Nothing.
autoplot.AntsResult = function(object, step.size = 1L, ...) {
  assertInteger(step.size, len = 1L, lower = 1L, upper = object$iters.done)
  storage = object$storage
  network = object$network
  reached.iter = object$iters.done
  iter = 1L
  while (iter <= reached.iter && !is.null(storage[[iter]])) {
    pl = plotIteration(network, storage = storage[[iter]])
    print(pl)
    iter = iter + step.size
    pause()
  }
}
