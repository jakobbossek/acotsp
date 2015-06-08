#' @param rho [\code{numeric(1)}]\cr
#'   \dQuote{Pheromone evaporation coefficient} respectively \dQuote{evaporation rate}.
#'   In each iteration the pheromones on edge (i,j) are first decreased by (1 - rho)
#'   before ants deposit their pheromones on it. Must be in (0, 1). Default is \code{0.1}.
