#' Helper function to visualize a pheromone matrix.
#'
#' This function draws a heatmap with darker color indicating higher pheromone
#' concentration on the corresponding arc.
#'
#' @param pher.mat [\code{matrix}]\cr
#'   Square numeric matrix.
#' @param ... [any]\cr
#'   Currently not used.
#' @return \code{\link[ggplot2]{ggplot}} object.
#' @export
visualizePheromoneMatrix = function(pher.mat, ...) {
  assertMatrix(pher.mat)
  n = ncol(pher.mat)
  if (nrow(pher.mat) != n) {
    stopf("A pheromone matrix needs to be a square n x n matrix,
      dim(pher.mat) = (%i, %i)", nrow(pher.mat), n
    )
  }

  if (!all(pher.mat >= 0)) {
    stopf("Pheromone concentration must not be negative, but %i component(s)
      of the passed matrix are.", sum(pher.mat < 0))
  }

  # convert to ggplot-friendly format
  df = pheromoneMatrixToGGDataframe(pher.mat)
  requirePackages("ggplot2", why = "ants::visualizePheromoneMatrix")

  pl = ggplot(data = df, mapping = aes_string(x = "x1", y = "x2", fill = "Pheromones"))
  pl = pl + geom_tile(colour = "white")
  pl = pl + scale_fill_gradient(low = "white", high = "steelblue")
  pl = pl + scale_x_discrete(expand = c(0, 0))
  pl = pl + scale_y_discrete(expand = c(0, 0))
  pl = pl + theme_grey(base_size = 9L)
  pl = pl + xlab("") + ylab("")
  pl = pl + theme(axis.ticks = element_blank())
  return(pl)
}

# Helper function which converts square pheromone matrix into a data frame
# that can be handled by ggplot.
#
# @param x [matrix]
#   Pheromone matrix.
# @return [data.frame]
pheromoneMatrixToGGDataframe = function(x) {
  n = ncol(x)
  colnames(x) = rownames(x) = paste0("N", seq(n))
  requirePackages("reshape2", why = "ants:::pheromoneMatrixToGGDataframe")
  df = reshape2::melt(x)
  colnames(df) = c("x1", "x2", "Pheromones")
  return(df)
}
