#' @title ants: Ant Colony Optimization for the Travelling Salesperson Problem
#'
#' @description
#' The ants package makes it possible to tackle problem instances for the
#' symmetric Traveling-Salesperson-Problem (TSP) with an Ant-Colony-Optiomization
#' (ACO) approach. ACO is based on observations of real ants finding somewhat
#' optimal trials between a food storage and the den: Each ant leaves the den
#' aiming to find some foot and on its way drops pheromones on the trial used.
#' While the pheromone concentration slowly evaporates it accumulates on promising,
#' i.e., short trials which are frequently used by the ants and more and more ants
#' start to follow this \emph{ant trial}.
#'
#' To solve a given problem instance with the \pkg{ants} package, one has to
#' wrap it in a \code{Network} (see package \pkg{netgen}). The next step is
#' setting up an \code{AntsControl} control object via \code{\link{makeAntsControl}}.
#' Here we specify all the parameters, e.g. the evaporation rate, the minimal
#' pheromone concentration or an additional local search procedure. There is a
#' vast number of parameters available with reasonable defaults which makes
#' it possible to highly customize the used solver. Thus, the final solver can
#' be build up of different building blocks.
#'
#' @section Shortcuts:
#' Moreover, for some classical ACO-approaches there exist different shortcut
#' functions which do not require to build an initial control object by hand.
#' Instead they offer a more R-like interface to these famous methods.
#'
#' @section Visualization:
#' The optimization process of ACO-based algorithms for the TSP can be nicely
#' visualized. Provided that the \code{trace.all} parameter is set in the control
#' object, the pheromone matrix and the best tour of each iteration is saved in
#' the result object. This information can be displayed afterwards. Two methods
#' exist:
#' \describe{
#'   \item{\code{\link{plotResult}}}{Generate plots of the problem instance for
#'   selected iterations. Displayed are the arcs with the transparency set
#'   according to the pheromone concentration and the best tour so far.}
#'   \item{\code{\link{visualizePheromoneMatrix}}}{Draw a heatmap of the
#'   pheromone matrix with darker colors representing a higher and lighter colors
#'   representing a lower pheromone concentration on the corresponding arc.}
#' }
#'
#' @docType package
#' @name ants
# EXCLUDE COVERAGE START
NULL
# EXCLUDE COVERAGE END
