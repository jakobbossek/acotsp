#' Ant System Optimization (ACO) algorithm for the (a)symmetric
#' \href{http://en.wikipedia.org/wiki/Travelling_salesman_problem}{Travelling
#' Salesperson Problem} (TSP).
#'
#' Implementation of the classical Ant System (AS) introduced by Dorigo.
#'
#' @param x [\code{Network}]\cr
#'   A network, i. e., a graph generated with the \pkg{netgen} package.
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
#' @param init.pher.conc [\code{numeric(1)}]\cr
#'   Initial pheromone concentration for every single edge. Default is \code{0.0001}.
#' @param min.pher.conc [\code{numeric(1)}]\cr
#'   Minimal pheromone concentration for every single edge. Default is \code{0}.
#' @param max.pher.conc [\code{numeric(1)}]\cr
#'   Maximal pheromone concentration for every single edge. Default is \code{10e5}.
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
#'   the standard output during execution. Default is \code{FALSE}, which means
#'   that the algorithm behaves like a typical Unix terminal program. It stops
#'   on failure with a cryptic message and stops on success with no message.
#' @param trace.all [\code{logical(1)}]\cr
#'   Should we save additional information in each iteration, i. e., pheromone
#'   matrix, all ant trails, best ant trail of the current iteration and so on?
#'   Default is \code{FALSE}. You need to set this to \code{TRUE} if you want
#'   to plot the optimization progress via \code{autoplot.AntsResult}.
#' @return [\code{AntsResult}]
#'   S3 object containing the result.
#' @keywords Optimization
#' @examples
#'   library(netgen)
#'   x = generateRandomInstance(n.points = 6L)
#'   res = aco(x, alpha = 1.2, beta = 1.8, n.ants = 15L, init.pher.conc = 0.01, max.iter = 10L)
#'   print(res)
#'
#'   x = matrix(c(1, 2, 1, 3, 1, 4, 3, 1, 3, 2, 3, 4), ncol = 2, byrow = TRUE)
#'   x = makeNetwork(x, lower = 1, upper = 4)
#'   res = aco(x, max.iter = 30L, n.ants = 20L, show.info = TRUE)
#' @export
aco = function(x,
    n.ants = 2L,
    alpha = 1, beta = 2, rho = 0.1, att.factor = 1,
    init.pher.conc = 0.0001, min.pher.conc = 0, max.pher.conc = 10e5,
    max.iter = 10L, max.time = Inf, global.opt.value = NULL, termination.eps = 0.1,
    show.info = FALSE, trace.all = FALSE) {

    used.arguments = list(
        n.ants = n.ants,
        alpha = alpha, beta = beta, rho = rho, att.factor = att.factor,
        init.pher.conc = init.pher.conc,
        min.pher.conc = min.pher.conc, max.pher.conc = max.pher.conc
    )

    #FIXME: do we need special class here? We need the distance matrix.
    assertClass(x, "Network")

    # generate distance matrix d(i, j)
    dist.mat = as.matrix(dist(x$coordinates, method = "euclidean"))
    n = getNumberOfNodes(x)

    # init termination criteria values
    start.time = Sys.time()
    iter.times = numeric(5L)
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
    assertInteger(n.ants, lower = 1)
    assertNumber(alpha, lower = 0, finite = TRUE, na.ok = FALSE)
    assertNumber(beta, lower = 1, finite = TRUE, na.ok = FALSE)
    assertNumber(rho, lower = 0, upper = 1, na.ok = FALSE)
    assertNumber(att.factor, lower = 1, finite = TRUE, na.ok = FALSE)
    assertNumber(init.pher.conc, lower = 0.0001, finite = TRUE, na.ok = FALSE)
    assertNumber(min.pher.conc, lower = 0, finite = TRUE, na.ok = FALSE)
    assertNumber(max.pher.conc, lower = 1, finite = TRUE, na.ok = FALSE)

    best.tour.length = Inf
    best.tour = rep(NA, n)

    pher.mat = matrix(init.pher.conc, ncol = n, nrow = n)
    par.set = makeNumericParamSet("n", len = n, lower = 1, upper = n)
    opt.path = makeOptPathDF(par.set, "tour.length", minimize = TRUE, )

    # initialize ants! We keep them implicitely, i. e., we keep an (m x n)
    # matrix, where m is the number of ants and n is the number of nodes
    # of the problem instance at hand.
    # Each row of the matrix contains a permutation of {1,...,n}, i. e., a
    # valid tour.
    ants.tours = matrix(NA, ncol = n, nrow = n.ants)

    storage = NULL
    if (trace.all) {
        storage = list()
    }

    repeat {
        iter.start.time = Sys.time()

        if (show.info) {
            print(round(pher.mat, digits = 2))
            catf("----------------")
        }
        # initialize first ant tours, i. e., select a start node randomly and
        # construct a valid tour
        #FIXME: move this to dedicated function 'searchForFood' or findAntTrails
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

        # get tour length
        ants.tour.lengths = apply(ants.tours, 1, function(x) {
            getTourLength(x, dist.mat)
        })

        best.current.ant = which.min(ants.tour.lengths)
        best.current.tour.length = ants.tour.lengths[best.current.ant]
        best.current.tour = ants.tours[best.current.ant, ]
        addOptPathEl(opt.path, x = list(best.current.tour), y = best.current.tour.length)
        if (best.current.tour.length < best.tour.length) {
            best.tour.length = best.current.tour.length
            best.tour = best.current.tour
        }
        if (show.info) {
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
            max.time = max.time,
            iter.times = iter.times
        )

        if (termination.code > -1L) {
            break
        }

        pher.mat = updatePheromones(pher.mat, dist.mat, ants.tours, ants.tour.lengths, rho, att.factor, min.pher.conc, max.pher.conc)
        if (trace.all) {
            storage[[iter]] = list(
                pher.mat = pher.mat,
                ants.tours = ants.tours,
                best.tour = best.tour
            )
        }
        iter.times[iter %% 5] = difftime(Sys.time(), iter.start.time, units = "secs")
        iter = iter + 1L
    }

    makeS3Obj(
        call = match.call(),
        used.arguments = used.arguments,
        network = x,
        best.tour = best.tour,
        best.tour.length = best.tour.length,
        opt.path = opt.path,
        time.passed = difftime(Sys.time(), start.time, units = "secs"),
        iters.done = iter,
        storage = storage,
        termination.code = termination.code,
        termination.message = getTerminationMessage(termination.code),
        classes = "AntsResult"
    )
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
updatePheromones = function(pher.mat, dist.mat, ants.tours, tour.lengths, rho, att.factor, min.pher.conc, max.pher.conc) {
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
            if (pher.mat[i, j] < min.pher.conc) {
                pher.mat[i, j] = min.pher.conc
            } else if (pher.mat[i, j] > max.pher.conc) {
                pher.mat[i, j] = max.pher.conc
            }
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