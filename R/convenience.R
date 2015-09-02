# probably only need invwich and is.empty. mayyyybe %notin% but not worth the effort of portability.

#' Shorthand for \code{x[x \%in\% y]}.
#'
#' @param x vector to test
#' @param y vector to check membership of
#' @param inv whether we are checking membership (\code{\%in\%}) or
#'            exclusion (\code{\%notin\%})
#' @param value whether to return the values of `x` that are in (or not) `y`,
#'        or the indices. If `value=F` this is just like \code{\%in\%} or \code{\%notin\%}.
#' @return If `value=T`, the subset of `x` that is in `y` (`inv=F`), or that is
#'         not in y (`inv=T`).
#'         If `value=F`, a logical vector that `x \%in\% y` if `inv=F` or
#'         `!(x \%in\% y)` otherwise.
#' @seealso \code{\link[base:match]{\%in\%}}
#' @seealso \code{\link[utilitiesR:notin]{\%notin\%}}
#' @seealso \code{\link{invwhich}}
#' @examples
#' x <- 1:5
#' y <- sample(x, size=3)
#' find.in(x, y) # all elements of x that are in y
#' find.in(x, y, inv=TRUE) # all elements of x that are not in y
#'
#' find.in(x, y, value=FALSE) # equivalent to x %in% y
#' find.in(x, y, value=FALSE, inv=TRUE) # equivalent to !(x %in% y)
#' @export
find.in <- function (x, y, inv=FALSE, value=TRUE) {
    idx <- x %in% y
    if (inv)
        idx <- !idx
    if (value) {
        x[idx]
    } else {
        idx
    }
}

#' Opposite to \code{\%in\%}.
#'
#' Returns a logical vector for elements of `x` not in `table`.
#' It is simply equivalent to \code{!(x \%in\% table)}.
#' @usage x \%notin\% table
#' @param x     vector to test
#' @param table vector to check membership of
#' @return a logical vector, same length as `x`, that is TRUE in the `i`th position
#' if `x[i]` is NOT in `table`.
#' @examples
#' x <- 1:5
#' x[x %notin% c(1, 3, 5)] # 2, 4
#' @export
#' @seealso \code{\link[base:match]{\%in\%}}
#' @seealso \code{\link{find.in}}
#' @seealso \code{\link{invwhich}}
#' @aliases %notin%
#' @rdname notin
#' @export
"%notin%" <- function (x, table) {
    match(x, table, nomatch=0L) == 0L
}

#' Opposite to \code{which}.
#'
#' @param indices the indices that should be TRUE
#' @param length.out the length of the output vector. if NULL, `max(indices)` is used.
#' @param use.names whether to use the names of `indices` if there are any.
#' @author user Nick Sabbe on StackOverflow: http://stackoverflow.com/a/7661128/913184
#' @return a logical vector of length `length.out` that is TRUE at the indices
#'         in `indices`.
#' @examples
#' idx <- c(1, 3)
#' invwhich(idx)     # c(TRUE, FALSE, TRUE)
#' invwhich(idx, 5)  # c(TRUE, FALSE, TRUE, FALSE, FALSE)
#'
#' ## example with use.names
#' names(idx) <- c('one', 'three')
#' invwhich(idx, 4)
#' @seealso \code{\link[base:match]{\%in\%}}
#' @seealso \code{\link{find.in}}
#' @seealso \code{\link[utilitiesR:notin]{\%notin\%}}
#' @export
invwhich <- function(indices, length.out=NULL, use.names=TRUE) {
    if (missing(length.out) || is.null(length.out)) {
        length.out <- max(indices)
    }
    rv <- logical(length.out)
    if(length(indices) > 0) {
        rv[indices] <- TRUE
        if (use.names)
            names(rv)[indices] <- names(indices)
    }
    return(rv)
}

#' See if an argument is na, null, or length 0 (syntactic sugar).
#' @param x object to check
#' @return boolean, TRUE if it is na, null OR length 0.
#' @note syntactic sugar for \code{is.null(x) || is.na(x) || length(x)==0}.
#' Note that the empty string ("") is NOT "empty" in this sense.
#' @examples
#' is.empty(NA)          # true
#' is.empty(NULL)        # true
#' is.empty(character()) # true, length 0
#' is.empty("")          # false, this has length 1.
#' @export
is.empty <- function(x)
{
    return(is.null(x) || length(x)==0 || all(is.na(x)))
}
