test.template <- '
describe("FUNCTION", {
    it("works", {
       @TODO
    })
})
'
#' Generates skeleton testthat files for your R package
#'
#' This scans through your package R files for any objects (functions, data, ...) defined.
#' It creates one file in `tests/testthat/` for each file in `R/`.
#' In each file there is one `context()` per function/object defined in the R file.
#' It is up to you to fill out the tests.
#'
#' Existing files will not be clobbered.
#' @param pkg path to package
#' @param quiet if `TRUE` suppresses output from this function.
#' @return a list with one element per file created, the value being a vector of function names we added test skeletons for (invisibly)
#' @export
extract_tests <- function (pkg='.', quiet=FALSE) {
    o <- list()
    if (!devtools:::uses_testthat(pkg))
        devtools::use_testthat(pkg=pkg)
    code_files <- tools::list_files_with_type(file.path(pkg, 'R'), "code", full.names=T)
    # get functions in the r code
    for (cf in code_files) {
        env <- new.env(hash=T)
        sys.source(cf, envir=env)
        tests.toadd <- ls(env, all.names=T)
        if (!length(tests.toadd)) {
            if (!quiet)
                message(sprintf("Nothing in file %s, skipping", basename(cf)))
            next
        }

        name <- sub('\\.[rRq]$', '', basename(cf))
        test.path <- file.path(pkg, sprintf('tests/testthat/test-%s.r', name))

        # append to file if exists
        # try to be case insensitiive...
        existing.files <- list.files(file.path(pkg, 'tests/testthat'))
        if (!is.na(i <- match(tolower(basename(test.path)), tolower(existing.files)))) {
            test.path <- file.path(pkg, 'tests/testthat', existing.files[i])
            cd <- paste(readLines(test.path), collapse='\n')
            # crude detection
            existing.tests <- tests.toadd[vapply(sprintf('(context|test_that)\\(["\'].*?\\b%s\\b', tests.toadd), grepl, FALSE, x=cd)]
            tests.toadd <- setdiff(tests.toadd, existing.tests)
            if (!length(tests.toadd)) {
                if (!quiet)
                    message(sprintf("All objects in %s have test skeletons already, skipping", basename(cf)))
                next
            }
        }
        o[[basename(test.path)]] <- tests.toadd
        cat(paste(vapply(tests.toadd, gsub, 'template', pattern='FUNCTION', x=test.template, fixed=T), collapse=''),
            file=test.path,
            append=T)
        if (!quiet)
            message(sprintf("Added test skeletons to %s", test.path))
    }
    invisible(o)
}
