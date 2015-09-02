#' Convenience `paste` wrapper that colllapses a vector into a string.
#'
#' Just a wrapper around `base::paste` with `collapse=' '` (`collapse`) or `collapse=''` (`collapse0`).
#' The `sep` parameter is applied before the `collapse` one as with base `paste`.
#' In fact, if you find yourself specifying the 'collapse' argument at all,
#' you may as well use `base::paste`!
#'
#' @param ... one or more `R` objects, to be converted to character vectors.
#' @param collapse string to separate each element of the result vector. Default ''.
#' @param sep string to separate terms. This is used before collapsing. Default `' '` for `collapse` and `''` for `collapse0`.
#' @return the input strings collapsed and pasted.
#' @examples
#' collapse(letters) # "a b c d e f g h i j k l m n o p q r s t u v w x y z"
#' collapse0(letters) # "abcdefghijklmnopqrstuvwxyz"
#'
#' collapse(letters[1:10], 1:10) # "a 1 b 2 c 3 d 4 e 5 f 6 g 7 h 8 i 9 j 10"
#' collapse0(letters[1:10], 1:10) # "a1b2c3d4e5f6g7h8i9j10"
#'
#' collapse(letters[1:10], 1:10, sep='.') # "a.1 b.2 c.3 d.4 e.5 f.6 g.7 h.8 i.9 j.10"
#' collapse0(letters[1:10], 1:10, sep='.') # "a.1b.2c.3d.4e.5f.6g.7h.8i.9j.10"
#'
#' # Note - if you supply collapse at all, then you may as well use paste.
#' collapse(1:4, collapse='|') # same as
#' paste(1:4, collapse='|')
#'
#' @export
#' @seealso `base::paste`
#' @family string
collapse <- function (..., sep=' ', collapse=' ') {
    return(paste(..., sep=sep, collapse=collapse))
}
#' @describeIn collapse
collapse0 <- function (..., sep='', collapse='') {
    return(paste(..., sep=sep, collapse=collapse))
}
