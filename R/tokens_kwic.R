#' Segment documents into windows
#' @export
#' @examples
#' require(quanteda)
#' toks <- tokens(data_corpus_irishbudget2010)
#' toks_kwic <- tokens_kwic(toks, "tax")
tokens_kwic <- function(x, pattern, window = 5,
                 valuetype = c("glob", "regex", "fixed"),
                 case_insensitive = TRUE,
                 use_docvars = TRUE,
                 ...) {
    UseMethod("tokens_kwic")
}

#' @export
#' @import quanteda Rcpp
#' @importFrom RcppParallel RcppParallelLibs
#' @useDynLib quanteda.features
tokens_kwic.tokens <- function(x, pattern, window = 5,
                           valuetype = c("glob", "regex", "fixed"),
                           case_insensitive = TRUE,
                           use_docvars = TRUE, ...) {

    valuetype <- match.arg(valuetype)
    type <- types(x)

    attrs <- attributes(x)
    types <- types(x)
    vars <- docvars(x)

    ids <- quanteda:::pattern2list(pattern, type,
                                   valuetype, case_insensitive,
                                   attr(x, "concatenator"))

    # this code is based on quanteda::tokens_segment()
    x <- qatd_cpp_tokens_kwic(x, type, ids, window)
    docname <- paste(attr(x, "document"), as.character(attr(x, "segid")), sep = ".")

    # add repeated versions of remaining docvars
    if (use_docvars && !is.null(vars)) {
        vars <- vars[attr(x, "docid"), , drop = FALSE] # repeat rows
        rownames(vars) <- docname
    } else {
        attrs$docvars <- NULL
        vars <- NULL
    }
    result <- quanteda:::create(
        x, what = "tokens",
        attrs = attrs,
        docvars = vars,
        names = docname,
        document = NULL,
        docid = NULL,
        segid = NULL
        )

    docvars(result, "_document") <- attr(x, "document")
    docvars(result, "_docid") <- attr(x, "docid")
    docvars(result, "_segid") <- attr(x, "segid")
    return(result)
}
