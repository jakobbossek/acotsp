#' @param local.search.fun [\code{function}]\cr
#'   Local search procedure which should be applied to all ant tours every
#'   \code{local.search.step} iterations. The function must expect the TSP
#'   instance \code{x} as the first, the initial tour \code{initial.tour} as
#'   the second argument. Default is \code{NULL}, i.e., no local search at all.
