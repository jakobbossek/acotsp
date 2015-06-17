#' @title Generates a ggplot object of a single iteration.
#'
#' @param network [\code{Network}]\cr
#'   Network.
#' @param storage [\code{list}]\cr
#'   List of internal iteration state containing pher.mat, best.tour and ants.tours
#'   for a single iteration.
#' @return [\code{ggplot}]
#'
#' @export
plotIteration = function(network, storage) {
  assertClass(network, "Network")
  assertList(storage, any.missing = FALSE)

  BBmisc::requirePackages("ggplot2", why = "ants")
  df.lines = data.frame()
  coords = network$coordinates
  pher.mat = storage$pher.mat
  best.tour = storage$best.tour

  #FIXME: think of a better way.
  for (i in 1:nrow(coords)) {
    for (j in i:nrow(coords)) {
      if (i == j) {
        next
      }
      df.lines = rbind(df.lines, data.frame(
        "x1" = coords[i, 1], "x2" = coords[i, 2],
        "xend" = coords[j, 1], "yend" = coords[j, 2],
        "pheromones" = pher.mat[i, j])
      )
    }
  }
  best.tour = c(best.tour, best.tour[1])
  df.tour = as.data.frame(coords[best.tour, ])
  colnames(df.tour) = c("x1", "x2")

  pl = autoplot(network)
  pl = pl + geom_segment(data = df.lines, aes_string(x = "x1", y = "x2", xend = "xend", yend = "yend", alpha = "pheromones"))
  pl = pl + geom_path(data = df.tour, colour = "tomato", linetype = "dashed")
  return(pl)
}
