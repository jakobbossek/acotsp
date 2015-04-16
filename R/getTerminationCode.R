# Checks whether the EA run should terminate.
#
# @param current.iter [integer(1)]
#   Current iteration of the algorithm.
# @param max.iter [integer(1)]
#   Maximum number of iterations.
# @param global.opt.value [numeric]
#   Known shortest tour length.
# @param current.best.value [numeric(1)]
#   Length of the current best tour.
# @param termination.eps [numeric(1)]
#   Maximal gap between best individual so far and the global optimum.
# @param start.time [POSIXct]
#   Start time as returned by Sys.time().
# @param max.time [integer]
#   Maximal time budget in seconds.
# @param iter.times [numeric]
#   Runtimes of the last iterations.
# @return [integer(1)]
#   Return code. See getTerminationMessage for encoding.
getTerminationCode = function(
  current.iter, max.iter,
  global.opt.value, current.best.value, termination.eps,
  start.time, max.time, iter.times) {
  if (didReachMaximumIterations(current.iter, max.iter)) {
    return(0L)
  }
  if (didReachToleranceLevel(global.opt.value, current.best.value, termination.eps)) {
    return(1L)
  }
  if (didReachMaximumTimeBudget(start.time, max.time)) {
    return(2L)
  }
  return(-1L)
}

# Returns a meaningful message (reason for termination).
#
# A simple mapping from code to message.
#
# @param termination.code [integer(1)]
#   Return value of getTerminationCode.
# @return [character(1)]
getTerminationMessage = function(termination.code) {
  termination.messages = c(
    "Reached maximal number of iterations.", # 0
    "Reached tolerence level.", # 1
    "Time budget exceeded." # 2
    )
  if (termination.code %nin% 0:2) {
    stopf("Unknown terminaton code %i.", termination.code)
  }
  return(termination.messages[termination.code + 1])
}

# Reached the maximum number of iterations?
#
# @param current.iter [integer(1)]
#   Current iteration.
# @param max.iter [integer(1)]
#   Maximum number of iterations.
# @return [logical(1)]
didReachMaximumIterations = function(current.iter, max.iter) {
  return(current.iter > max.iter)
}

# Reached tolerance level?
#
# @param global.opt.value [numeric]
#   Known shortest tour length.
# @param current.best.value [numeric(1)]
#   Length of the current best tour.
# @param termination.eps [numeric(1)]
#   Maximal gap between best tour length so far and the global optimum.
# @return [logical(1)]
didReachToleranceLevel = function(global.opt.value, current.best.value, termination.eps) {
  if (is.null(global.opt.value)) {
    return(FALSE)
  }
  gap = sqrt(sum((current.best.value - as.numeric(global.opt.value))^2))
  return(gap < termination.eps)
}

# Reached time limit?
#
# @param start.time [POSIXct]
#   Start time as returned by Sys.time().
# @param max.time [integer]
#   Maximal time budget in seconds.
# @return [logical(1)]
didReachMaximumTimeBudget = function(start.time, max.time) {
  current.time = Sys.time()
  time.difference.in.seconds = difftime(current.time, start.time, units = "secs")
  return(time.difference.in.seconds >= max.time)
}
