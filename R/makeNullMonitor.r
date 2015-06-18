#' @title No monitoring at all.
#'
#' @return [\code{AntsMonitor}]
#'
#' @export
# nocov start
makeNullMonitor = function() {
  makeMonitor(
    before = function(envir = parent.frame()) {},
    step = function(envir = parent.frame()) {},
    after = function(envir = parent.frame()) {}
  )
}
# nocov end
