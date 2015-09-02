#' rsnippets: R snippets I use often.
#'
#' Messages/diagnostics:
#' * `::messagef`, `::warningf` and `::stopf`, which are just the base versions
#'   wrapped in `sprintf`
#' * `::muffle` and `::muffle.all`, to suppress messages/cat/print output (the former),
#'  as well as warnings (the latter)
#'
#'  Regex:
#'  *
#'
#'  String (convenience):
#'  * `::collapse`, and `::collapse0`, just a convenience for `paste(..., collapse=' ')` (or `''` for the latter).
#'
#' Package:
#' * extract_tests (needs devtools): run in a package dir, and it creates one test
#'   file per file in `R/` with a skeleton of the tests.
#'
#' Planned:
#'
#' * function to convert a file that uses rsnippets functions to an rsnippet-free
#'   one. Note: some functions are oneliners and can really just be aliases, like `muffle`.
#'   and `collapse`
#' * grab stuff from my old utilitiesR package
#' @name rsnippets
#' @docType package
#' @aliases rsnippets-package
