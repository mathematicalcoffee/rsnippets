#' Extract captured groups in a (Perl) regex.
#'
#' @param pattern the regex (in Perl), with capturing brackets, possibly named.
#' @param text    the string or character string to test the regex against
#' @param ...     passed into \code{\link[base:grep]{regexpr}} or
#'                \code{\link[base:grep]{gregexpr}} (e.g. ignore.case)
#' @return A matrix with the same number of rows as `text` and one column per
#'         captured group. If named captured groups are used
#'         (e.g. '(?<num>[0-9]+)'), the names will be the column names.
#' @details
#' If there are no capturing groups in the pattern, an error is thrown.
#' If there are capturing groups but a group simply does not have a match
#' in the string, that entry is the empty string "".
#' @note This is essentially stringr's str_matches but avoids me loading stringr in.
#' @examples
#' regexp('^(?<greeting>.+?)[, ]+(?<target>.+)$', c('Hello, world', 'Hi bar'))
#' #      greeting target
#' # [1,] "Hello"  "world"
#' # [2,] "Hi"     "bar"
#' @export
#' @seealso \code{\link{gregexp}} for the same thing, but using the 'g' flag
#' (yielding much more unwieldy output)
regexp <- function(pattern, text, ...) {
    if (length(text) == 0) {
        return(character(0))
    }
    m <- regexpr(pattern, text, perl=T, ...)
    if (is.null(attr(m, 'capture.start'))) {
        # no capturing group
        stop("gregexp: No capturing groups detected in regexp ", pattern)
    }
    st <- attr(m, 'capture.start')
    en <- st + attr(m, 'capture.length') - 1
    nms <- attr(m, 'capture.names')
    out <- sapply(seq_along(nms), function (n) { return(substr(text, st[,n], en[,n])) })
    out <- matrix(out, ncol=length(nms))
    colnames(out) <- nms
    return(out)
}

#' Extracts all matches from a string(s) against a Perl regex.
#'
#' By "all matches" we mean adding the 'g' switch to a regex.
#' For example, the regex '([oe])' matches 'hello' two times.
#'
#' @inheritParams regexp
#' @param use.names whether to set the names of the output list to `text`.
#' @return A list of the same length as `text`, with names equal to `text`
#'         (if use.names is TRUE).
#'         Each element is *itself* a list, one element per captured group,
#'         being a vector of the matches.
#'         If a captured group does not match, \code{out[[word]][[groupname]]}
#'         will be of length 0. If there are no capturing groups to begin with,
#'         an error is thrown.
#' @examples
#' gregexp('([eo])', 'hello')
#' # list(hello=list(c('e', 'o')))
#'
#' gregexp('Name=(?<name>[a-z]+), Surname=(?<surname>[a-z]+)',
#'         'Name=Jane, Surname=Doe\nName=John, Surname=Smith',
#'         ignore.case=TRUE,
#'         use.names=FALSE)
#' # list(list(name=c("Jane", "John"), surname=c("Doe", "Smith")))
#'
#' text <- c('apples and oranges or pears and bananas', 'dogs and cats')
#' gregexp('(?<first>\\w+) and (?<second>\\w+)', text, use.names=FALSE)
#' # list(list(first=c('apples', 'pears'), second=c('oranges', 'bananas')),
#' #      list(first='dogs', second='cats'))
#'
#' # same as above but names(out) == text
#' gregexp('(?<first>[a-z]+) and (?<second>[a-z]+)', text)
#'
#' @export
#' @seealso \code{\link{regexp}} for extracting matches in a more managable manner
#' (a table), but only extracts the first match for each captured group (i.e. no 'g' flag).
gregexp <- function(pattern, text, use.names=T, ...) {
    M <- gregexpr(pattern, text, perl=T, ...)
    if (!length(M) || is.null(attr(M[[1]], 'capture.start'))) {
        # no capturing group
        stop("gregexp: No capturing groups detected in regexp ", pattern)
    }
    out <- lapply(seq_along(M), function (i) {
        m <- M[[i]]
        st <- attr(m, 'capture.start')
        en <- st + attr(m, 'capture.length') - 1
        nms <- attr(m, 'capture.names')
        text <- rep(text[i], nrow(st))
        out <- lapply(seq_along(nms), function (n) {
            ms <- substr(text, st[,n], en[,n])
            return(ms[st[, n] > 0])
        })
        names(out) <- nms
        out
    })
    if (use.names) {
        names(out) <- text
    }
    out
}

# NOTE: have added many extra '\' in the in-line documentation because this
#  translates badly in .Rd form, particularly curly braces...
# (that's also the reason for the usage string with extra backslashes)
# Read this documentation via ?regescape, *NOT* from the comments, or you'll be
#  confused.
#' Escapes a string to be regex-safe.
#'
#' @usage regescape(x, escape='.+*?^$[]\\\\()\{\}|-')
#' @param x the string (or character vector of strings) to escape
#' @param escape characters to escape, default '.*?+^$[]\\\\\{\}()|-' (the double-backslash
#'               is interpreted as a single blackslash by R).
#' @return the escaped string. By default the characters '.*?+^$[]\\\{\}()|-' are all escaped.
#' @note Remember that R strings require escaping of backslashes too (as well as
#' regex), so for example to escape the actual string '\' which is R string '\\\\',
#' you would get '\\\\\\\\'.
#' @examples
#' regescape(c('Hi.', '**hello?**', 'C:\\')) # C:\
#' # c('Hi\\.',    '\\*\\*hello\\?\\*\\*',    'C:\\\\')
#'
#' regescape('Hi...?', escape='?') # only escape the '?'
#' # 'Hi...\\?'
#' @export
regescape <- function(x, escape='.+*?^$[]\\(){}|-') {
    if (is.empty(escape) || nchar(escape) == 0) {
        return(x)
    }
    # turn '\' into '\\' (for regexp)
    escape <- gsub('\\', '\\\\', escape, fixed=T)
    # escape ^[]- inside `escape` as they have special meaning in [].
    escape <- gsub('([\\^\\[\\]\\-])', '\\\\\\1', escape, perl=T)
    return(gsub(paste0('([', escape, '])'), '\\\\\\1', x, perl=T))
}
