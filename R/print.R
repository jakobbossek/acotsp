#' @title Prints \code{AntsResult} object to standard output.
#'
#' @param x [\code{AntsResult}]\cr
#'   Result object.
#' @param ... [any]\cr
#'   Currently not used.
#' @return Nothing
#'
#' @export
print.AntsResult = function(x, ...) {
  n = getNumberOfNodes(x$network)
  catf("Ants found solution after %i iterations (%.2f seconds passed)", x$iters.done, x$time.passed)
  catf("Tour length: %.5f", x$best.tour.length)
  catf("Tour: %s%s", collapse(x$best.tour[1:10], sep = ", "), if (n > 10) ", ..." else "")
}
