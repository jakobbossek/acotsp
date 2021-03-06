#' @title Wrapper function for the the MAX-MIN Ant System.
#'
#' @description
#' Offers a more R-like interface to the MAX-MIN Ant System (MMAS) for the TSP.
#'
#' @family ants_shortcuts
#'
#' @references
#' Stuetzle, T. and Hoos, H.H. (1998)
#' \emph{Improvements on the Ant System: Introducing the MAX-MIN Ant System.}
#' In Artificial Neural Networks and Genetic Algorithms, pp. 245-249.
#' Springer Verlag, Wien New York.
#'
#' @inheritParams makeACOTSPControl
#' @template arg_network
#' @template arg_monitor
#' @param ... [\code{any}]\cr
#'   Further parameters passed to control object, e.g., \code{max.iter}. See
#'   \code{makeACOTSPControl}.
#' @return [\code{AntsResult}]
#'   S3 result object.
#'
#' @export
#FIXME: max.pher.conc is updated to 1/(rho * C[best]) in MMAS each time a new best-so-far
# tour is found, min.pher.conc is set to max.pher.conc / a with a being a parameter (
# Stuetzle, 1999, Stuetzle & Hoos, 2000)
#FIXME: reinitialization of pheromone trails on stagnation (statistically determined or
# if no improvement in the last x iterations)
runMMAS = function(
  x, n.ants = 10L, alpha = 1, beta = 2, rho = 0.1, att.factor = 1,
  min.pher.conc, max.pher.conc, init.pher.conc = max.pher.conc,
  monitor = makeNullMonitor(), ...) {
  control = makeACOTSPControl(n.ants = n.ants, alpha = alpha, beta = beta,
    rho = rho, att.factor = att.factor, ...)
  runACOTSP(x, control, monitor)
}
