#' @param use.global.best [\code{logical(1)}]\cr
#'   Should the global best tour be used to update the pheromones? Default is \code{FALSE}.
#'   Keep in mind: if \code{use.global.best} is \code{TRUE} and \code{n.elite} is
#'   0, only the global best tour (and consequently only the arcs of the global best
#'   tour) gain pheromones in each iteration.
