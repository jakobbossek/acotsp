#' Ant System Optimization (ACO) algorithm for the (a)symmetric
#' \href{http://en.wikipedia.org/wiki/Travelling_salesman_problem}{Travelling
#' Salesperson Problem} (TSP).
#'
#' Implementation of the classical Ant System (AS) introduced by Dorigo.
#'
#' @template arg_network
#' @param control [\code{AntsControl}]\cr
#'   Control object. See \code{\link{makeAntsControl}}.
#' @template arg_monitor
#' @return [\code{AntsResult}]
#'   S3 result object.
#' @keywords Optimization
#' @examples
#'   library(netgen)
#'   x = generateRandomNetwork(n.points = 6L)
#'   ctrl = makeAntsControl(
#'     alpha = 1.2, beta = 1.8, n.ants = 15L,
#'     init.pher.conc = 0.01, max.iter = 10L
#'   )
#'   res = aco(x, ctrl)
#'   print(res)
#'
#'   x = matrix(c(1, 2, 1, 3, 1, 4, 3, 1, 3, 2, 3, 4), ncol = 2, byrow = TRUE)
#'   x = makeNetwork(x, lower = 1, upper = 4)
#'   ctrl = makeAntsControl(alpha = 1.2, beta = 1.8, n.ants = 20L, max.iter = 30L)
#'   res = aco(x, ctrl, monitor = makeConsoleMonitor())
#' @export
aco = function(x, control, monitor = makeNullMonitor()) {
  assertClass(x, "Network")
  assertClass(control, "AntsControl")
  assertClass(monitor, "AntsMonitor")

  # extract some instance information
  dist.mat = x$distance.matrix
  n = getNumberOfNodes(x)

  # init storage for best tour
  best.tour.length = Inf
  best.tour = rep(NA, n)

  n.ants = control$n.ants
  alpha = control$alpha
  beta = control$beta
  doLocalPheromoneUpdate = control$local.pher.update.fun

  # init opt path
  par.set = makeNumericParamSet("n", len = n, lower = 1, upper = n)
  opt.path = makeOptPathDF(par.set, "tour.length", minimize = TRUE, )

  # init ants and pheromones
  # initialize ants! We keep them implicitely, i. e., we keep an (m x n)
  # matrix, where m is the number of ants and n is the number of nodes
  # of the problem instance at hand.
  # Each row of the matrix contains a permutation of {1,...,n}, i. e., a
  # valid tour.
  pher.mat = matrix(control$init.pher.conc, ncol = n, nrow = n)
  diag(pher.mat) = 0
  ants.tours = matrix(NA, ncol = n, nrow = control$n.ants)

  # init termination criteria values
  start.time = Sys.time()
  iter = 1L

  storage = NULL
  if (control$trace.all) {
    storage = list()
  }

  monitor$before()

  repeat {
    iter.start.time = Sys.time()

    # initialize first ant tours, i. e., select a start node randomly and
    # construct a valid tour
    for (ant in seq(n.ants)) {
      #FIXME: this is O(n^2). Research method to make this more effective!
      start = sample(seq(n), size = 1L)
      ants.tours[ant, 1L] = start
      used = logical(n)
      used[start] = TRUE
      i = 2L
      while (any(!used)) {
        dest = getNextEdgeOnTrail(start, used, alpha, beta, dist.mat, pher.mat, control$prp.prob)
        used[dest] = TRUE

        # do local pheromone update
        if (!is.null(doLocalPheromoneUpdate)) {
          pher.mat[start, dest] = doLocalPheromoneUpdate(pher.mat[start, dest])
        }
        ants.tours[ant, i] = dest
        start = dest
        i = i + 1L
      }
    }

    # get tour length (row-wise)
    ants.tour.lengths = apply(ants.tours, 1L, getTourLength, dist.mat = dist.mat)

    # get best tour length of this iteration
    best.current.ant = which.min(ants.tour.lengths)
    best.current.tour.length = ants.tour.lengths[best.current.ant]
    best.current.tour = ants.tours[best.current.ant, ]

    # update optimization path
    addOptPathEl(opt.path, x = list(best.current.tour), y = best.current.tour.length)
    if (best.current.tour.length < best.tour.length) {
      best.tour.length = best.current.tour.length
      best.tour = best.current.tour
    }

    monitor$step()

    #FIXME: this is ugly
    termination.code = getTerminationCode(
      current.iter = iter,
      max.iter = control$max.iter,
      global.opt.value = control$global.opt.value,
      current.best.value = best.current.tour.length,
      termination.eps = control$termination.eps,
      start.time = start.time,
      max.time = control$max.time
    )

    if (termination.code > -1L) {
      break
    }

    # update pheromone trails (this is where the different ACO systems differ most!)
    pher.mat = updatePheromoneMatrix(
      pher.mat, dist.mat,
      ants.tours, ants.tour.lengths,
      best.tour, best.tour.length,
      control
    )

    # store all the stuff in neccessary
    if (control$trace.all) {
      storage[[iter]] = list(
        pher.mat = pher.mat,
        ants.tours = ants.tours,
        best.tour = best.tour
      )
    }

    iter = iter + 1L
  }

  monitor$after()

  makeS3Obj(
    call = match.call(),
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

# Get the next edge during trail construction.
#
# @param start [\code{integer(1)}]
#   Node id of the source node.
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
# @return [integer(1)] Node id of the next node to visit.
getNextEdgeOnTrail = function(start, used, alpha, beta, dist.mat, pher.mat, prp.prob) {
  # get the IDs of the nodes, which are not yet included in the tour
  unused.idx = which(!used)
  # Here we apply the socalled "pseudo-random-proportional action choice rule"
  # (See Stützle and Dorigo - ACO Algorithms for the Travelling Salesman Problem)
  if (runif(1) <= prp.prob) {
    # get the unvisited city which is most promising according to pheromone
    # concentration and distance
    vals = sapply(unused.idx, function(dest) {
      pher.mat[start, dest] * (1 / dist.mat[start, dest])^beta
    })
    dest = unused.idx[which.max(vals)]
  } else {
    # use the familiar formula to compute probabilities
    probs = getTransitionProbabilities(start, used, alpha, beta, dist.mat, pher.mat)
    #FIXME: what is going on here? sample(1, 1, prob = 1) works! Why not here?
    if (length(probs) > 1L) {
      dest = sample(unused.idx, size = 1, prob = probs)
    } else {
      dest = unused.idx
    }
  }
  return(dest)
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
  return(probs / sum(probs))
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

# Helper to get the IDs of the best ants, i.e., the ants which walked the shortest
# tours.
#
# @param tour.length [numeric]\cr
#   Vector of tour length.
# @param n.elite [integer]
#   Number of elite ants.
# @return integer(n.elite)
getEliteAnts = function(tour.length, n.elite) {
  # order ants accoridng to the tour length in increasing order and select the # n.elite best
  idx = order(tour.length, decreasing = FALSE)[seq(n.elite)]
  return(idx)
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
# @param global.best.tour [numeric]
#   Global best tour, i.e., best tour so far.
# @param global.best.tour.length [numeric(1)]
#   Length of the global best tour.
# @param control [AntsControl]
#   Control object.
# @return [matrix]
#   Updated pheromone matrix.
updatePheromoneMatrix = function(
  pher.mat, dist.mat,
  ants.tours, tour.lengths,
  global.best.tour, global.best.tour.length,
  control) {
  n = nrow(pher.mat)

  ss = seq.int(n)

  if (control$use.global.best && control$best.deposit.only) {
    # ACS-LIKE UPDATE OF PHEROMONES - O(n)
    # i.e., only the arcs of the global best tour are affected by deposit and
    # evaporation of pheromone. Thus we have O(n) complexity here instead of O(n^2).
    tt = c(global.best.tour, global.best.tour[1])
    for (i in 1:(length(tt) - 1L)) {
      start = tt[i]
      end = tt[i + 1L]
      pher.mat[start, end] = (1 - control$rho) * pher.mat[start, end] + control$rho / global.best.tour.length
    }
  } else {
    # CLASSICAL UPDATE OF PHEROMONES - O(n^2)
    # i.e., many of the ants drop pheromones

    # determine the IDs of the "best" ants
    elite.ants = getEliteAnts(tour.lengths, control$n.elite)

    # first evaporate all edges ...
    pher.mat = (1 - control$rho) * pher.mat

    for (i in ss) {
      for (j in ss) {
        if (i == j) {
          next # diagonal elements are always zero
        }
        ants.pher.evap = 0
        # get the "delta-tau" values for all the elite ants
        if (control$n.elite > 0L) {
          ants.pher.evap = sapply(elite.ants, function(k) {
            if (hasAntUsedEdge(ants.tours[k, ], i, j)) control$att.factor / tour.lengths[k] else 0
          })
        }
        # update pheromones
        pher.mat[i, j] = pher.mat[i, j] + sum(ants.pher.evap)
        # update the pheromone concentration of the arcs on the global best tour
        if (control$use.global.best) {
          global.best.pher.evap = 0
          if (hasAntUsedEdge(global.best.tour, i, j)) {
            global.best.pher.evap = control$att.factor / global.best.tour.length
          }
          pher.mat[i, j] = pher.mat[i, j] + global.best.pher.evap
        }

        # MAX-MIN Ant System, i.e., bound the pheromone concentration
        if (pher.mat[i, j] < control$min.pher.conc) {
          pher.mat[i, j] = control$min.pher.conc
        } else if (pher.mat[i, j] > control$max.pher.conc) {
          pher.mat[i, j] = control$max.pher.conc
        }
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
    tour.length = tour.length + dist.mat[tour[i], tour[i + 1L]]
  }
  tour.length = tour.length + dist.mat[tour[n], tour[1L]]
  return(tour.length)
}
