#' @title Wrapper function for the Ant Colony System.
#'
#' @description
#' Offers a more R-like interface to the Ant Colony System (ACS) for the TSP.
#'
#' @family ants_shortcuts
#'
#' @references
#' Dorigo, M. and Gambardella, L. M. (1997)
#' \emph{Ant Colony System: A Cooperative Learning Approach to the Traveling Salesman Problem.}
#' In IEEE Transactions on Evolutionary Computation, pp. 53-66.
#'
#' @inheritParams makeAntsControl
#' @template arg_network
#' @template arg_monitor
#' @param ... [\code{any}]\cr
#'   Further parameters passed to control object, e.g., \code{max.iter}. See
#'   \code{makeAntsControl}.
#' @return [\code{AntsResult}]
#'   S3 result object.
#'
#' @export
#FIXME: add @rdname or family or something like that for all these shortcut funs.
runACS = function(
  x, n.ants = 10L, alpha = 1, beta = 2, rho = 0.1, att.factor = 1,
  local.pher.update.fun = NULL,
  monitor = makeNullMonitor(), ...) {
  if (is.null(local.pher.update.fun)) {
    # in ACS we need the local update function and thus provide a default if
    # none is given
    local.pher.update.fun = function(pher) {
      #FIXME: recommended values? See the ACO book by Dorigo et al.
      xhi = 1 / getNumberOfNodes(x)
      tau0 = 0.1
      return((1 - xhi) * pher + xhi * tau0)
    }
  }
  assertFunction(local.pher.update.fun, args = "pher")
  control = makeAntsControl(
    n.ants = n.ants,
    n.elite = 0L, # only the global best tour/ant is allowed to place pheromones
    use.global.best = TRUE,
    best.deposit.only = TRUE,
    local.pher.update.fun = local.pher.update.fun,
    alpha = alpha, beta = beta,
    rho = rho, att.factor = att.factor, ...)
  aco(x, control, monitor)
}
