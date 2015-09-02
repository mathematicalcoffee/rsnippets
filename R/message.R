# utility

#' Generate a diagnostic message with formatted message.
#'
#' Just pipes the input arguments through to `base::message`.
#' @param ... passed to `base::sprintf`
#' @param domain Passed to `base::message` See `base::gettext`. If `NA`, messages will not be translated, see also the note in `base::stop`.
#' @param appendLF logical: should messages given as a character string have a newline appended?
#' @seealso `base::sprintf`, `base::message`
#' @family message, warnings and errors.
#' @examples
#' messagef("Your %s has only %i %s!", "flooglebar", 2, "snarfles")
#' @export
messagef <- function (..., domain=NULL, appendLF=TRUE) {
    message(sprintf(...), domain=domain, appendLF=appendLF)
}

#' Generate a warning with a formatted message
#'
#' Essentially calls `stop(warningf(...))`, with some care to give the correct calling function.
#' @param ... passed to `base::sprintf`
#' @param call. passed to `base::warning`; logical, indicating if the call should become part of the warning message.
#' @param immediate. passed to `base::warning`; logical, indicating if the call should be output immediately, even if `getOption("warn") <= 0`
#' @param noBreaks. passed to `base::warning`; logical, indicating as far as possible the message should be output as a single line when `options(warn=1)`.
#' @param domain passed to `base::warning` See `base::gettext`. If `NA`, messages will not be translated, see also the note in `base::stop`.
#' @details
#'
#' @seealso `base::sprintf`, `base::warning`
#' @family message, warnings and errors.
#' @examples
#' # toplevel warnings. should be the same:
#' warning('foo! bar 1')
#' warningf('%s bar %i', 'foo!', 1)
#'
#' # warnings from within a function
#' f <- function (x) warning("I don't know what to do with ", x)
#' f('hi')
#' f <- function (x) warningf("I don't know what to do with %s", x)
#' f('hi')
#' @export
warningf <- function(..., call.=TRUE, immediate.=FALSE, domain=NULL) {
    # raise the warning up to the parent function, not the warningf function
    if (call.) {
        wh <- sys.call(-1)
        if (is.null(wh))
            warning(sprintf(...), call.=FALSE, immediate.=immediate., domain=domain)
        else
            warning(paste('In', deparse(wh), ':', sprintf(...)), call.=FALSE,
                    immediate.=immediate., domain=domain)
    } else {
        warning(sprintf(...), call.=call., immediate.=immediate., domain=domain)
    }
}

#' Stop function execution, with a formatted message
#' Essentially calls `stop(sprintf(...))`, with some care to give the correct calling function.
#' @param ... passed to `base::sprintf`
#' @param call. passed to `base::stop`; logical, indicating if the call should become part of the error message.
#' @param domain. passed to `base::stop` See `base::gettext`. If `NA`, messages will not be translated, see also the note in `base::stop`.
#' @details
#' There's some farting around in order to try and report the appropriate function that
#' threw the error; by default, it'll report `stopf` as the function throwing the
#' error (as `stop` is called from within `stopf`).
#'
#' Also, the traceback will have an extra level with the `stopf` call in it.
#'
#' Also, note that if you `stopf` a condition object (as opposed to something that
#'  sprintf can handle), you will be sorry (we do not handle it gracefully).
#' @family message, warnings and errors
#' @seealso `base::stop`, `base::sprintf`
#' @examples
#' \dontrun{
#' # should be the same
#' stopf("an error!")
#' stop("an error!")
#'
#' f <- function (x) stopf("An error: %i", x)
#' f(1)
#' # (almost) same as (we have an extra colon I can't do anything about)
#' f <- function (x) stop("An error: ", x)
#' f(1)
#' }
#' @export
stopf <- function(..., call.=TRUE, domain=NULL) {
    # raise the error up to the parent function, not the stopf function.
    if (call.) {
        # try to work out the calling function. if NULL, top-level "Error: {message}"
        # Otherwise, "Error in {parent call} : {message}"
        wh <- sys.call(-1)
        if (is.null(wh))
            stop(sprintf(...), call.=FALSE, domain=domain)
        else
            stop(paste('In', deparse(wh), ':', sprintf(...)), call.=FALSE,
                 domain=domain)
    } else {
        stop(sprintf(...), call.=call., domain=domain)
    }
}

#' Suppress messages, cat and printed output.
#'
#' This function suppresses any output caused by `base::message`,
#' `base::packageStartupMessage`, `base::cat` and `base::print`, returning
#' (invisibly) the result of the expression.
#'
#' Warnings and errors are passed through.
#' @param expr Expression to evaluate and muffle the output off.
#' @return the result of evaluating `expr`, invisibly.
#' @details
#' Uses `base::invisible`, `utils::capture.output`, `base::suppressPackageStartupMessages`,
#' and `base::suppressMessages` to suppress output.
#'
#' Errors and warnings are not suppressed.
#' @examples
#' muffle({cat('hi\n'); message('boo'); 1})   # returns 1, suppresses all messages.
#' # returns TRUE or FALSE depending on whether you have 'tcltk' package,
#' #  and suppresses "Loading Tcl/Tk interface ... done" message, as well as
#' #  the warning (if you don't have that package).
#' muffle(require(tcltk))
#' muffle({message('one'); warning('two'); 3}) # warnings are not suppressed.
#' @family message, warnings and errors
#' @seealso `::muffle.all`, which muffles warnings too.
#' @export
muffle <- function(expr) {
    invisible(capture.output(x <- suppressPackageStartupMessages(suppressMessages(expr))))
    invisible(x)
}
#' Suppresses as much output as possible (except errors)
#'
#' Suppress output from `base::message`, `base::warning`, `base::cat` and `base::print`.
#' @inheritParams muffle
#' @return the result of evaluating `expr`, invisibly.
#' @details
#' Uses `base::invisible`, `utils::capture.output`, `base::suppressPackageStartupMessages`,
#' `base::suppressMessages`, and `base::suppressWarnings` to suppress output as much
#' as possible.
#'
#' Note that errors are not suppressed (of course).
#' @examples
#' muffle.all(cat('hi'))
#' muffle.all(message('hi'))
#' muffle.all(warning('warning!'))
#' @export
#' @seealso `::muffle`, which muffles messages, `print` and `cat` only.
#' @family message, warnings and errors
muffle.all <- function(expr) {
    invisible(capture.output(x <- suppressPackageStartupMessages(suppressMessages(suppressWarnings(expr)))))
    invisible(x)
}
