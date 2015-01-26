#FIXME: add sanity checks (needs full storage)
#FIXME: test
#FIXME: write documentation
#FIXME: export
#FIXME: write this in a modular way! This could be used also as a monitor. In
# this case it does not need the full trace, but only the current stuff.
autoplot.AntsResult = function(object, ...) {
    storage = object$storage
    pher.mats = storage$pher.mat.storage
    ants.tours = storage$ants.tours.storage
    best.tours = storage$best.tour.storage

    network = object$network
    #FIXME: assert that this does not fail if max.iter was not reached
    n.iter = length(pher.mats)
    iter = 1L
    while (iter < n.iter && !is.null(pher.mats[iter])) {
        df.lines = data.frame()
        coords = network$coordinates
        pher.mat = pher.mats[[iter]]

        #FIXME: think of a better way.
        for (i in 1:nrow(coords)) {
            for (j in i:nrow(coords)) {
                if (i == j) {
                    next
                }
                df.lines = rbind(df.lines, data.frame("x1" = coords[i, 1], "x2" = coords[i, 2], "xend" = coords[j, 1], "yend" = coords[j, 2], "pheromones" = pher.mat[i, j]))
            }
        }
        best.tour = best.tours[[iter]]
        best.tour = c(best.tour, best.tour[1])
        df.tour = as.data.frame(coords[best.tour, ])
        colnames(df.tour) = c("x1", "x2")

        print(head(df.lines))
        # test
        pl = autoplot(network)
        pl = pl + geom_segment(data = df.lines, aes(x = x1, y = x2, xend = xend, yend = yend, alpha = pheromones))
        pl = pl + geom_path(data = df.tour, colour = "tomato", linetype = "dashed")

        print(pl)
        #FIXME: step.size as parameter
        iter = iter + 5L
        pause()
    }
}


pasteNamedList = function(l) {
    paste(mixupStrings(names(l), as.character(unlist(l))), sep = ",")
}

listToString = function(l) {
    ns = names(l)
    res = lapply(1:length(l), function(i) {
        paste(ns[i], i, sep = "=")
    })
    collapse(unlist(res), ", ")
}

#FIMXE: name it "Reißverschluss" in englisch
mixupStrings = function(s, t) {
    assertCharacter(s, any.missing = FALSE, min.len = 1L)
    assertCharacter(t, any.missing = FALSE, min.len = 1L)

    n = length(s)
    if (n != length(t)) {
        stopf("Only strings of equal size can be combined!")
    }

    res = character(2 * n)
    res[seq(1, 2 * n, by = 2)] = s
    res[seq(2, 2 * n, by = 2)] = t
    return(res)
}
