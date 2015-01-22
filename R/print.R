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
    catf("ALGORITHM AND PARAMETERS:")
    catf("Result object of call:")
    print(x$call)
    catf("Operated on complete graph of size %i", n)
    catf("Parametrization of solver: %s", collapseList(x$used.arguments, sep = ", "))

    catf("\nRESULT:")
    catf("Length of shortest (best) tour found: ", x$best.tour.length)
    catf("Tour beginns with: %s%s", collapse(x$best.tour[1:10], sep = ", "), if (n > 10) " ..." else "")

    catf("\nOPTIMIZATION PATH:")
    print(head(as.data.frame(x$opt.path)))
}

#FIXME: move this to BBmisc/HELL?
collapseList = function(l, sep = ";") {
    ns = names(l)
    res = lapply(1:length(l), function(i) {
        paste(ns[i], i, sep = "=")
    })
    collapse(unlist(res), sep)
}