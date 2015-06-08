#' Generates a control object for the \code{\link{aco}} function.
#'
#' This function generates a control object, i.e., internally a simple list, of
#' parameters and does sanity checks.
#'
#' @param n.ants [\code{integer(1)}]\cr
#'   Number of ants. Positive integer.
#' @param alpha [\code{numeric(1)}]\cr
#'   This parameter decides how much influence the pheromones on an edge have
#'   on the selection of edges. Default is \code{1}.
#' @param beta [\code{numeric(1)}]\cr
#'   This parameter decided how much influence the edge distances have
#'   on the selection of edges. Default is \code{2}.
#' @param rho [\code{numeric(1)}]\cr
#'   \dQuote{Pheromone evaporation coefficient} respectively \dQuote{evaporation rate}.
#'   In each iteration the pheromones on edge (i,j) are first decreased by (1 - rho)
#'   before ants deposit their pheromones on it. Must be in (0, 1). Default is \code{0.1}.
#' @param att.factor [\code{numeric(1)}]\cr
#'   This is the socalled \dQuote{constant attractiveness factor}. Default is \code{1}.
#' @param init.pher.conc [\code{numeric(1)}]\cr
#'   Initial pheromone concentration for every single edge. Default is \code{0.0001}.
#' @param min.pher.conc [\code{numeric(1)}]\cr
#'   Minimal pheromone concentration for every single edge. Default is \code{0}.
#' @param max.pher.conc [\code{numeric(1)}]\cr
#'   Maximal pheromone concentration for every single edge. Default is \code{10e5}.
#' @param pher.conc.in.bounds [\code{logical(1)}]\cr
#'   Should the pheromone concentration be bounded by \code{min.pher.conc} and \code{max.pher.conc}?
#'   Default is \code{TRUE}.
#' @param prp.prob [\code{numeric(1)}]\cr
#'   Probability used in the pseudo-random-proportional action choice rule used, e.g., by
#'   the Ant Colony System. Default is 0, which means that the rule is not applied
#'   at all and the tour construction is done in the classical way.
#' @param local.pher.update.fun [\code{function}]\cr
#'   Local (pheromone) update rule applied right after an ant crossed an edge. Default
#'   is \code{NULL}, which means no local update at all. This must be a function which
#'   expects a single parameter \code{pher}.
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
  alpha = 1, beta = 2, rho = 0.1, att.factor = 1,
  init.pher.conc = 0.0001, min.pher.conc = 0, max.pher.conc = 10e5,
  pher.conc.in.bounds = TRUE,
  prp.prob = 0,
  local.pher.update.fun = NULL,
  max.iter = 10L, max.time = Inf, global.opt.value = NULL, termination.eps = 0.1,
  trace.all = FALSE) {

  # do sanity checks
  assertInteger(n.ants, lower = 1)
  assertNumber(alpha, lower = 0, finite = TRUE, na.ok = FALSE)
  assertNumber(beta, lower = 1, finite = TRUE, na.ok = FALSE)
  assertNumber(rho, lower = 0, upper = 1, na.ok = FALSE)
  assertNumber(att.factor, lower = 1, finite = TRUE, na.ok = FALSE)
  assertNumber(init.pher.conc, lower = 0.0001, finite = TRUE, na.ok = FALSE)
  assertNumber(min.pher.conc, lower = 0, finite = TRUE, na.ok = FALSE)
  assertNumber(max.pher.conc, lower = 1, finite = TRUE, na.ok = FALSE)
  assertFlag(pher.conc.in.bounds)
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
    alpha = alpha,
    beta = beta,
    rho = rho,
    att.factor = att.factor,
    init.pher.conc = init.pher.conc,
    min.pher.conc = min.pher.conc,
    max.pher.conc = max.pher.conc,
    pher.conc.in.bounds = pher.conc.in.bounds,
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
