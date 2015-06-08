#' Generates a control object for the \code{\link{aco}} function.
#'
#' This function generates a control object, i.e., internally a simple list, of
#' parameters and does sanity checks.
#'
#' @template arg_nants
#' @template arg_nelite
#' @template arg_useglobalbest
#' @template arg_alpha
#' @template arg_beta
#' @template arg_rho
#' @template arg_attfactor
#' @template arg_initpherconc
#' @template arg_minpherconc
#' @template arg_maxpherconc
#' @template arg_prpprob
#' @template arg_localpherupdatefun
#' @param max.iter [\code{integer(1)}]\cr
#'   Maximal number of iterations. Default is \code{10}.
#' @param max.time [\code{integer(1)}]\cr
#'   Maximum running time in seconds. The algorithm tries hard to hold this
#'   restriction by logging the times of a number of prior iterations and
#'   determining statistically whether the time limit can be hold when another
#'   iteration is done. Default ist \code{Inf}, which means no time limit at all.
#' @param global.opt.value [\code{numeric(1)}]\cr
#'   Known global best tour length. This can be used as another termination
#'   criterion. Default is \code{NULL}, which means, that the length of the
#'   globally best tour is unknown.
#' @param termination.eps [\code{numeric(1)}]\cr
#'   If \code{global.opt.value} is set, the algorithm stops if the quadratic
#'   distance between the global optimum value and the value of the best tour
#'   found so far is lower than these gap value. Ignored if \code{global.opt.value}
#'   is \code{NULL}.
#' @param trace.all [\code{logical(1)}]\cr
#'   Should we save additional information in each iteration, i. e., pheromone
#'   matrix, all ant trails, best ant trail of the current iteration and so on?
#'   Default is \code{FALSE}. You need to set this to \code{TRUE} if you want
#'   to plot the optimization progress via \code{autoplot.AntsResult}.
#' @return [\code{AntsControl}]
#'   S3 control object containing all the checked parameters and reasonable defaults.
#' @export
makeAntsControl = function(
  n.ants = 2L,
  n.elite = n.ants,
  use.global.best = FALSE,
  alpha = 1, beta = 2, rho = 0.1, att.factor = 1,
  init.pher.conc = 0.0001, min.pher.conc = 0, max.pher.conc = 10e5,
  prp.prob = 0,
  local.pher.update.fun = NULL,
  max.iter = 10L, max.time = Inf, global.opt.value = NULL, termination.eps = 0.1,
  trace.all = FALSE) {

  # do sanity checks
  assertInteger(n.ants, lower = 1L)
  assertInteger(n.elite, lower = 0L)
  if (n.elite > n.ants) {
    stopf("n.elite must be lower or equal to n.ants, but %i = n.elite > n.ants = %i", n.elite, n.ants)
  }
  assertFlag(use.global.best)
  if (n.elite == 0 && !use.global.best) {
    stopf("Zero elite ants and no global update not allowed! Somehow the pheromones need
      to be updated.")
  }
  assertNumber(alpha, lower = 0, finite = TRUE, na.ok = FALSE)
  assertNumber(beta, lower = 1, finite = TRUE, na.ok = FALSE)
  assertNumber(rho, lower = 0, upper = 1, na.ok = FALSE)
  assertNumber(att.factor, lower = 1, finite = TRUE, na.ok = FALSE)
  assertNumber(init.pher.conc, lower = 0.0001, finite = TRUE, na.ok = FALSE)
  assertNumber(min.pher.conc, lower = 0, finite = TRUE, na.ok = FALSE)
  assertNumber(max.pher.conc, lower = 1, finite = TRUE, na.ok = FALSE)
  assertNumber(prp.prob, lower = 0, upper = 1, na.ok = FALSE)

  if (!is.null(local.pher.update.fun)) {
    assertFunction(local.pher.update.fun, args = "pher")
  }

  if (is.finite(max.time)) {
    max.time = convertInteger(max.time)
  }
  assertNumber(max.time, lower = 100L, na.ok = FALSE)

  if (is.finite(max.iter)) {
    max.iter = convertInteger(max.iter)
  }
  assertInt(max.iter, lower = 1L, na.ok = FALSE)
  if (!is.null(global.opt.value)) {
    assertNumber(global.opt.value, na.ok = FALSE, finite = TRUE)
  }
  assertNumber(termination.eps, lower = 0.000001, finite = TRUE, na.ok = FALSE)

  makeS3Obj(
    n.ants = n.ants,
    n.elite = n.elite,
    use.global.best = use.global.best,
    alpha = alpha,
    beta = beta,
    rho = rho,
    att.factor = att.factor,
    init.pher.conc = init.pher.conc,
    min.pher.conc = min.pher.conc,
    max.pher.conc = max.pher.conc,
    prp.prob = prp.prob,
    local.pher.update.fun = local.pher.update.fun,
    max.iter = max.iter,
    max.time = max.time,
    global.opt.value = global.opt.value,
    termination.eps = termination.eps,
    trace.all = trace.all,
    classes = "AntsControl"
  )
}
