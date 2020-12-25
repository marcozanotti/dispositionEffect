#' @name %>%
#'
#' @title Pipe operator
#'
#' @description See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @rdname pipe
#' @keywords internal
#' @importFrom magrittr %>%
NULL


#' @name negative_match
#'
#' @title Negative match operator
#'
#' @description See \code{base::\link[base:match]{\%in\%}} for details on the
#'   match operator.
#'   The negative match operator is simply the negation of the match operator
#'   obtained via the \code{base::\link[base:Negate]{Negate}} function.
#'
#' @usage x \%!in\% table
#'
#' @param x vector or NULL: the values to be matched. Long vectors are supported.
#' @param table vector or NULL: the values to be matched against. Long vectors are
#'   not supported.
#'
#' @rdname negative_match
#' @keywords internal
`%!in%` <- function(x, table) {!(match(x, table, nomatch = 0) > 0)}
