#' Prints AntsResult object to standard output.
#'
#' @param x [\code{AntsResult}]\cr
#'   Result object.
#' @param ... [any]\cr
#'   Currently not used.
#' @return Nothing
#' @export
print.AntsResult = function(x, ...) {
    n = getNumberOfNodes(x$network)
    catf("ALGORITHM AND PARAMETERS")
    catf("Result object of call:")
    print(x$call)
    catf("Parameters: %s", collapseList(x$used.arguments, sep = ", "))

    catf("\nRESULT")
    catf("Tour length: %.5f", x$best.tour.length)
    catf("Tour: %s%s", collapse(x$best.tour[1:10], sep = ", "), if (n > 10) ", ..." else "")

    catf("\nOPTIMIZATION PATH")
    print(head(as.data.frame(x$opt.path)))
    catf("...")
}

#FIXME: move this to BBmisc/HELL?
collapseList = function(l, sep = ";") {
    ns = names(l)
    res = lapply(1:length(l), function(i) {
        paste(ns[i], i, sep = "=")
    })
    collapse(unlist(res), sep)
}