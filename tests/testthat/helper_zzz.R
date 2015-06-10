set.seed(123)

# FIXME: move this to netgen (as well as makeGridNetwork)
buildRectangleInstance = function(lower, upper) {
  x = as.matrix(expand.grid(lower:upper, lower:upper))
  coordinates = lapply(1:nrow(x), function(i) {
    if ((x[i, 1] %in% c(lower, upper)) || (x[i, 2] %in% c(lower, upper)))
      return(x[i, ])
    return(c())
  })
  coordinates = do.call(rbind, coordinates)
  netgen::makeNetwork(coordinates, lower = lower, upper = upper)
}
