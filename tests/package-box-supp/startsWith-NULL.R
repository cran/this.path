##
## this.path : Get Executing Script's Path
## Copyright (C) 2024   Iris Simmons
##


NULL


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
