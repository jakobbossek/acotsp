#' @param local.search.step [\code{integer}]\cr
#'   This parameter determines when to apply the local search procedure given by
#'   the \code{local.search.fun} function. If this is a single scalar value n, the
#'   local search is applied every n iterations. If a integer vector is passed,
#'   the local search procedure is called on exactly the given iterations. This
#'   parameter is considered only if \code{local.search.step} is not \code{NULL}.
#'   Default is the empty vector, which means no local search at all.
