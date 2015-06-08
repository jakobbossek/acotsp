#' Wrapper function for the classical Ant System.
#'
#' Offers a more R-like interface to the basic Ant System (AS) for the TSP.
#'
#' @template arg_network
#' @template arg_nants
#' @template arg_alpha
#' @template arg_beta
#' @template arg_rho
#' @template arg_attfactor
#' @template arg_monitor
#' @param ... [\code{any}]\cr
#'   Further parameters passed to control object, e.g., \code{max.iter}. See
#'   \code{makeAntsControl}
#' @return [\code{AntsResult}]
#'   S3 result object.
#' @export
#FIXME: add @rdname or family or something like that for all these shortcut funs.
runAS = function(
  x, n.ants = 10L, alpha = 1, beta = 2, rho = 0.1, att.factor = 1,
  monitor = makeNullMonitor(), ...) {
  control = makeAntsControl(n.ants = n.ants, alpha = alpha, beta = beta,
    rho = rho, att.factor = att.factor, ...)
  aco(x, control, monitor)
}
