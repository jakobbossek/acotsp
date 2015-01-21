getTransitionProbabilities = function(start, used, alpha, beta, dist.mat, pher.mat) {
    unused.idx = which(!used)
    probs = sapply(unused.idx, function(dest) {
        (pher.mat[start, dest])^alpha * (1 / dist.mat[start, dest])^beta
    })
    (probs / sum(probs))
}

#FIXME: write documentation for this function.
#FIXME: what are reasonable defaults for these parameters?
#FIXME: this is just a "case study". Need to make a framework out of it
# with lots of different strategies: classical AS, ACO, Max-Min AS, ...
aco = function(x, n.ants = 2L, alpha = 1, beta = 2, rho = 0.2, max.iter = 10L, show.info = TRUE) {
    #FIXME: upper should be somewhat n/2 or something of this type.
    assertInteger(n.ants, lower = 1)
    assertNumber(alpha, lower = 0, finite = TRUE, na.ok = FALSE)
    assertNumber(beta, lower = 0, finite = TRUE, na.ok = FALSE)
    assertNumber(rho, lower = 0, upper = 1, na.ok = FALSE)

    #FIXME: do we need special class here? We need the distance matrix.
    assertClass(x, "Network")
    # generate distance matrix d(i, j)
    dist.mat = as.matrix(dist(x$coordinates, method = "euclidean"))
    n = getNumberOfNodes(x)

    best.tour.length = Inf
    best.tour = rep(NA, n)

    # initialize ants! We keep them implicitely, i. e., we keep an (m x n)
    # matrix, where m is the number of ants and n is the number of nodes
    # of the problem instance at hand.
    # Each row of the matrix contains a permutation of {1,...,n}, i. e., a
    # valid tour.

    #FIXME: get rid of magic number
    pher.mat = matrix(0.0001, ncol = n, nrow = n)
    par.set = makeNumericParamSet("n", len = n, lower = 1, upper = n)
    opt.path = makeOptPathDF(par.set, "tour.length", minimize = TRUE, )

    # initialize first ant tours, i. e., select a start node randomly and
    # construct a valid tour
    ants.tours = matrix(NA, ncol = n, nrow = n.ants)

    iter = 1L
    while (iter <= max.iter) {
        if (show.info) {
            #FIXME: pher.mat is not symmetric
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

        pher.mat = updatePheromones(pher.mat, dist.mat, ants.tours, ants.tour.lengths, rho)
        iter = iter + 1L
    }

    makeS3Obj(
        best.tour = best.tour,
        best.tour.length = best.tour.length,
        opt.path = opt.path,
        classes = "AntsResult"
    )
    #FIXME: add result object
    #FIXME: save state in trace. Maybe in ParamHelpers::optPath?
    #FIXME: add update of pheromones
    #FIXME: keep different update strategies in mind! Write a nice interface
    #FIXME: write plot monitoring function. Highlight edges according to pheromones
}

#FIXME: ugly as sin!!!
hasAntUsedEdge = function(tour, start, end) {
    tour = c(tour, tour[1])
    for (i in 1:(length(tour) - 1)) {
        if ((tour[i] == start && tour[i + 1] == end) || (tour[i] == end && tour[i + 1] == start))
            return(TRUE)
    }
    return(FALSE)
}

#FIXME: ugly as sin!!!
# Save used edges during tour constuction!
updatePheromones = function(pher.mat, dist.mat, ants.tours, tour.lengths, rho) {
    n = nrow(pher.mat)
    for (i in seq(n)) {
        for (j in seq(n)) {
            ants.pher.evap = sapply(1:length(tour.lengths), function(k) {
                if (hasAntUsedEdge(ants.tours[k, ], i, j)) 1 / tour.lengths[k] else 0
            })
            pher.mat[i, j] = (1 - rho) * pher.mat[i, j] + sum(ants.pher.evap)
        }
    }
    return(pher.mat)
}

getTourLength = function(tour, dist.mat) {
    tl = 0
    n = length(tour)
    for (i in 1:(n - 1)) {
        tl = tl + dist.mat[i, i + 1]
    }
    tl = dist.mat[tour[n], tour[1]]
    return(tl)
}