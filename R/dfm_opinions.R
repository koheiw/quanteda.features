#' Create a dfm with opinion features
#' @param x a tokens object
#' @param dictionary a dictionary obejct to specify topics or entities the
#'   opinions are about.
#' @param dictionary_sentiment a dictionary object for sentiment analysis where
#'   first and second keys are negative and positive sentiment, respectively.
#' @param negation tokens indicating negations in sentences
#' @param ... additional argumets passed to \code{tokens_kwic}
#' @export
dfm_opinions <- function(x,
                        dictionary,
                        dictionary_sentiment,
                        negation = negations(),
                        ...) {

    result <- matrix(nrow = ndoc(x), ncol = 0)
    for (key in names(dictionary)) {
        temp <- tokens_kwic(x, dictionary[key], ...)
        if (!ndoc(temp)) {
            result <- cbind(result, rep(0, nrow(result)))
        } else {
            temp <- tokens_remove(temp, dictionary[key])
            temp <- tokens_lookup(temp, dictionary_sentiment)
            dfmt <- dfm(temp)
            neg <- rowSums(dfm_select(dfmt, negation))
            dfmt <- dfmt * ifelse(neg > 0, -1, 1)
            dfmt <- dfm_group(dfmt,
                              factor(docvars(dfmt, "_document"),
                                     levels = docnames(x)), fill = TRUE)
            result <- cbind(result, dfmt %*% c(-1, 1))
        }
    }
    colnames(result) <- names(dict)
    as.dfm(result)
}

#' List of negation expressions
#' @export
negations <- function() {
    c("no", "not", "cannot", "*n't", "never", "nor", "hardly", "barely", "scarcely")
}
