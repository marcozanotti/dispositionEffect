#' @name %>%
#'
#' @title Pipe operator
#'
#' @description See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @usage lhs \%>\% rhs
#'
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
NULL


#' @name %!in%
#'
#' @title Negative match operator
#'
#' @description See \code{base::\link[base:match]{\%in\%}} for details on the
#'   match operator.
#'   The negative match operator is simply the negation of the match operator
#'   obtained via the \code{base::\link[base:Negate]{Negate}} function.
#'
#' @usage lhs \%!in\% rhs
#'
#' @rdname negative_match
#' @keywords internal
#' @export
`%!in%` <- Negate(`%in%`)

