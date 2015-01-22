#' Ant System optimization procedure for the symmetric or asymetric Travelling
#' Salesperson Problem (TSP).
#'
#' Implementation of the classical Ant System (AS) introduced by Dorigo.
#'
#' @param x [\code{Network}]\cr
#'   A network, i. e., a graph generated with the netgen package.
#'   See \code{\link[netgen]{makeNetwork}} for details.
#' @param n.ants [\code{integer(1)}]\cr
#'   Number of ants. Positive integer.
#' @param alpha [\code{numeric(1)}]\cr
#'   This parameter decided how much influence the pheromones on an edge have
#'   on the selection of edges. Default is \code{1}.
#' @param beta [\code{numeric(1)}]\cr
#'   This parameter decided how much influence the edges distances have
#'   on the selection of edges.
#' @param rho [\code{numeric(1)}]\cr
#'   \dQuote{Pheromone evaporation coefficient} respectively \dQuote{evaporation rate}.
#'   In each iteration the pheromone on edge (i,j) are first decreased by (1 - rho)
#'   before ants deposit their pheromones on it. Must be in (0, 1). Default is \code{0.1}.
#' @param att.factor [\code{numeric(1)}]\cr
#'   Constant attractiveness factor. Default is \code{1}.
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
#' @param show.info [\code{logical(1)}]\cr
#'   If \code{TRUE}, which is the default value, some information is printed to
#'   the standard output during execution.
#' @return [\code{AntsResult}]
#'   S3 object containing the result.
#FIXME: what are reasonable defaults for these parameters?
#FIXME: add weight parameter for the pheromene increasing (Q)
#FIXME: add another stopping criterion: max.time (in seconds), implement an heuristic,
# which saves the execution times of the last 5 or so iterations, and takes a statistic
# of these (i.e., median, max, 0.75-quartile) to decide whether another iteration is
# possible within time.
#FIXME: add monitoring option (see ecr for this)
#FIXME: this is just a "case study". Need to make a framework out of it
# with lots of different strategies: classical AS, ACO, Max-Min AS, ...
aco = function(x,
    n.ants = 2L,
    alpha = 1, beta = 2, rho = 0.1, att.factor = 1,
    max.iter = 10L, max.time = Inf, global.opt.value = NULL, termination.eps = 0.1,
    show.info = TRUE) {

    #FIXME: do we need special class here? We need the distance matrix.
    assertClass(x, "Network")

    # generate distance matrix d(i, j)
    dist.mat = as.matrix(dist(x$coordinates, method = "euclidean"))
    n = getNumberOfNodes(x)

    # init termination criteria values
    start.time = Sys.time()
    iter = 1L
    if (is.finite(max.time)) {
        max.time = convertInteger(max.time)
    }
    assertNumber(max.time, lower = 100L, na.ok = FALSE)

    if (is.finite(max.iter)) {
        max.iter = convertInteger(max.iter)
    }
    assertInt(max.iter, lower = 1L, na.ok = FALSE)
    #FIXME: allow value to be NULL explicitely via null.ok = TRUE in checkmate would be great
    if (!is.null(global.opt.value)) {
        assertNumber(global.opt.value, na.ok = FALSE, finite = TRUE)
    }
    assertNumber(termination.eps, lower = 0.000001, finite = TRUE, na.ok = FALSE)

    # do sanity checks
    #FIXME: upper should be somewhat n/2 or something of this type.
    assertInteger(n.ants, lower = 1)
    assertNumber(alpha, lower = 0, finite = TRUE, na.ok = FALSE)
    #FIXME: beta should be >= 1 according to wikipedia :-) Check this in literature.
    assertNumber(beta, lower = 1, finite = TRUE, na.ok = FALSE)
    assertNumber(rho, lower = 0, upper = 1, na.ok = FALSE)
    assertNumber(att.factor, lower = 1, finite = TRUE, na.ok = FALSE)


    best.tour.length = Inf
    best.tour = rep(NA, n)

    # initialize ants! We keep them implicitely, i. e., we keep an (m x n)
    # matrix, where m is the number of ants and n is the number of nodes
    # of the problem instance at hand.
    # Each row of the matrix contains a permutation of {1,...,n}, i. e., a
    # valid tour.

    #FIXME: get rid of magic number. Make parameter out of it.
    pher.mat = matrix(0.0001, ncol = n, nrow = n)
    par.set = makeNumericParamSet("n", len = n, lower = 1, upper = n)
    opt.path = makeOptPathDF(par.set, "tour.length", minimize = TRUE, )

    # initialize first ant tours, i. e., select a start node randomly and
    # construct a valid tour
    ants.tours = matrix(NA, ncol = n, nrow = n.ants)


    repeat {
        if (show.info) {
            print(round(pher.mat, digits = 2))
            catf("----------------")
        }
        for (ant in seq(n.ants)) {
            #FIXME: this is O(n^2). Research method to make this more effective!
            start = sample(1:n, size = 1)
            ants.tours[ant, 1] = start
            used = logical(n)
            used[start] = TRUE
            i = 2
            while (any(!used)) {
                probs = getTransitionProbabilities(start, used, alpha, beta, dist.mat, pher.mat)
                unused.idx = which(!used)
                #catf("%i , %i", length(unused.idx), length(probs))
                #print(unused.idx)
                #print(probs)
                #FIXME: what is going on here? sample(1, 1, prob = 1) works! Why not here?
                if (length(probs) > 1L)
                    dest = sample(unused.idx, size = 1, prob = probs)
                else
                    dest = unused.idx
                used[dest] = TRUE
                ants.tours[ant, i] = dest
                start = dest
                i = i + 1L
            }
        }
        #print(ants.tours)

        # get tour length
        ants.tour.lengths = apply(ants.tours, 1, function(x) {
            getTourLength(x, dist.mat)
        })
        #print(ants.tour.lengths)

        best.current.ant = which.min(ants.tour.lengths)
        best.current.tour.length = ants.tour.lengths[best.current.ant]
        best.current.tour = ants.tours[best.current.ant, ]
        addOptPathEl(opt.path, x = list(best.current.tour), y = best.current.tour.length)
        if (best.current.tour.length < best.tour.length) {
            best.tour.length = best.current.tour.length
            best.tour = best.current.tour
        }
        if (!show.info) {
            catf("======")
            catf("Current iteration: %i", i)
            catf("Best ant: %i", best.current.ant)
            catf("Best tour length: %f", best.current.tour.length)
            catf("Best tour: %s", collapse(best.current.tour, sep = ", "))
            catf("======")
            catf("Overall best:")
            catf("Best tour length: %f", best.tour.length)
            catf("Best tour: %s", collapse(best.tour, sep = ", "))
        }
        termination.code = getTerminationCode(
            current.iter = iter,
            max.iter = max.iter,
            global.opt.value = global.opt.value,
            current.best.value = best.current.tour.length,
            termination.eps = termination.eps,
            start.time = start.time,
            max.time = max.time
        )

        if (termination.code > -1L) {
            break
        }

        pher.mat = updatePheromones(pher.mat, dist.mat, ants.tours, ants.tour.lengths, rho, att.factor)
        iter = iter + 1L
    }

    makeS3Obj(
        best.tour = best.tour,
        best.tour.length = best.tour.length,
        opt.path = opt.path,
        termination.code = termination.code,
        termination.message = getTerminationMessage(termination.code),
        classes = "AntsResult"
    )
    #FIXME: save state in trace. Maybe in ParamHelpers::optPath?
    #FIXME: keep different update strategies in mind! Write a nice interface
    #FIXME: write plot monitoring function. Highlight edges according to pheromones
}


# Get the transition probabilities which ants use to determine randomly which
# edges to choose next.
#
# @param start [integer(1)]
#   Id of the start city.
# @param used [logical]
#   Logical vector. If used[i] is TRUE, the i-th city has already been visited.
# @param alpha [integer(1)]
#   See aco documentation.
# @param beta [integer(1)]
#   See aco documentation.
# @param dist.mat [matrix]
#   Distance matrix.
# @param pher.mat [matrix]
#   Pheromone matrix.
# @return [numeric]
#   Vector of probabilities
getTransitionProbabilities = function(start, used, alpha, beta, dist.mat, pher.mat) {
    unused.idx = which(!used)
    probs = sapply(unused.idx, function(dest) {
        (pher.mat[start, dest])^alpha * (1 / dist.mat[start, dest])^beta
    })
    (probs / sum(probs))
}

# Checks whether an ant has used an edge on its trail.
#
# @param tour [integer]
#   Ants tour, i.e., a permutation of 1:n, where n is the number of nodes.
# @param start [integer(1)]
#   Id of the start node of the edge.
# @param end [integer(1)]
#   Id of the end node of the edge.
# @return [logical(1)]
#   TRUE, if ant used the (start, end) edge on its trail.
#FIXME: this implementation is ugly and not R-like!
hasAntUsedEdge = function(tour, start, end) {
    tour = c(tour, tour[1])
    for (i in 1:(length(tour) - 1)) {
        if ((tour[i] == start && tour[i + 1] == end) || (tour[i] == end && tour[i + 1] == start))
            return(TRUE)
    }
    return(FALSE)
}

# AS-like update of pheromone concentration of the edges, i. e., gentle evaporation
# and increase according to number of visits and inverse distance of each edge.
#
# @param pher.mat [matrix]
#   Matrix of pheromone concentration.
# @param dist.mat [matrix]
#   Distance matrix.
# @param ants.tours [matrix]
#   Matrix containing the trails/tours of each ant row-wise.
# @param tour.lengths [numeric]
#   Vector of the ant tour length.
# @param rho [numeric(1)]
#   Evaporation rate.
# @param att.factor [numeric(1)]
#   Constant attractiveness factor.
# @return [matrix]
#   Updated pheromone matrix.
#FIXME: ugly as sin!!!
#FIXME: Save used edges during tour constuction!
updatePheromones = function(pher.mat, dist.mat, ants.tours, tour.lengths, rho, att.factor) {
    n = nrow(pher.mat)
    # first evaporate all edges
    pher.mat = (1 - rho) * pher.mat
    n.ants = length(tour.lengths)
    for (i in seq(n)) {
        for (j in seq(n)) {
            ants.pher.evap = sapply(seq(n.ants), function(k) {
                if (hasAntUsedEdge(ants.tours[k, ], i, j)) att.factor / tour.lengths[k] else 0
            })
            pher.mat[i, j] = pher.mat[i, j] + sum(ants.pher.evap)
        }
    }
    return(pher.mat)
}

# Helper function which determines the tour length of a given tour based on
# the distance matrix.
#
# @param tour [numeric]
#   Ants tour, i.e., a permutation of 1:n, where n is the number of nodes.
# @param dist.mat [matrix]
#   Distance matrix.
# @return [numeric(1)]
#   Tour length.
getTourLength = function(tour, dist.mat) {
    tour.length = 0
    n = length(tour)
    for (i in 1:(n - 1)) {
        tour.length = tour.length + dist.mat[tour[i], tour[i + 1]]
    }
    tour.length = tour.length + dist.mat[tour[n], tour[1]]
    return(tour.length)
}