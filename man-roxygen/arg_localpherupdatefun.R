#' @param local.pher.update.fun [\code{function}]\cr
#'   Local (pheromone) update rule applied right after an ant crossed an edge. Default
#'   is \code{NULL}, which means no local update at all. This must be a function which
#'   expects a single parameter \code{pher}.
