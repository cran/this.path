this.path::env.path(verbose = FALSE)


#' @export
doc_cntxt <- get(
    ".this.path::document.context",
    envir = sys.frame(this.path:::.getframenumber()),
    inherits = FALSE
)


#' @export
fun_that_calls_src_path <- function (...)
{
    this.path::src.path(...)
}


#' @export
fun_that_calls_env_path <- function (...)
{
    this.path::env.path(...)
}
