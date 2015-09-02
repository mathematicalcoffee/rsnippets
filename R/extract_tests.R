test.template <- '
context("FUNCTION")
test_that("FUNCTION works", {
    # @TODO
})
'
# scans through R/{filename} for functions, generates testthat
#  skeletons in tests/{filename} with a context per function.
library(devtools)
extract_tests <- function (pkg='.') {
    if (!devtools:::uses_testthat(pkg))
        use_testthat(pkg=pkg)
    code_files <- tools::list_files_with_type(file.path(pkg, 'R'), "code", full.names=T)
    # get functions in the r code
    for (cf in code_files) {
        env <- new.env(hash=T)
        sys.source(cf, envir=env)
        tests.toadd <- ls(env, all.names=T)
        if (!length(tests.toadd)) {
            message(sprintf("Nothing in file %s, skipping", basename(cf)))
            next
        }

        name <- sub('\\.[rRq]$', '', basename(cf))
        test.path <- file.path(pkg, sprintf('tests/testthat/test-%s.R', name))

        # append to file if exists
        if (file.exists(test.path)) {
            cd <- paste(readLines(test.path), collapse='\n')
            # crude detection
            existing.tests <- fns[vapply(sprintf('context\\(["\']%s["\']\\)', fns), grepl, FALSE, x=cd)]
            tests.toadd <- setdiff(tests.toadd, existing.tests)
            if (!length(tests.toadd)) {
                message(sprintf("All objects in %s have test skeletons already, skipping", basename(cf)))
                next
            }
        }
        cat(paste(vapply(tests.toadd, gsub, 'template', pattern='FUNCTION', x=test.template, fixed=T), collapse=''),
            file=test.path,
            append=T)
        message(sprintf("Added test skeletons to %s", test.path))
    }
}
