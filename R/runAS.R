#' @title Wrapper function for the classical Ant System.
#'
#' @description
#' Offers a more R-like interface to the basic Ant System (AS) for the TSP.
#'
#' @family ants_shortcuts
#'
#' @references
#' Dorigo, M. and Maniezzo, V. and Colorni, A. (1996)
#' \emph{Ant System: Optimization by a Colony of Cooperating Agents.}
#' In IEEE Transactions on Systems, Man, and Cybernetics - Part B.
#' IEEE Press, Piscataway, NJ, USA.
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
runAS = function(
  x, n.ants = 10L, alpha = 1, beta = 2, rho = 0.1, att.factor = 1,
  monitor = makeNullMonitor(), ...) {
  control = makeACOTSPControl(n.ants = n.ants, alpha = alpha, beta = beta,
    rho = rho, att.factor = att.factor, ...)
  runACOTSP(x, control, monitor)
}
