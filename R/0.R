# cat("\n> sys.calls()\n"); print(sys.calls())
# cat("\n> sys.parents()\n"); print(sys.parents())
# cat("\n> sys.frames()\n"); print(sys.frames())


.IS_SCALAR_STR <- function (x)
is.character(x) && length(unclass(x)) == 1L


.removeSource <- function (fn)
{
    recurse <- function(part) {
        if (is.name(part)) return(part)
        if (inherits(part, "srcref")) return(NULL)
        attr(part, "srcref") <- NULL
        attr(part, "wholeSrcref") <- NULL
        attr(part, "srcfile") <- NULL
        if (is.pairlist(part)) {
            for (i in seq_along(part))
                part[i] <- list(recurse(part[[i]]))
            as.pairlist(part)
        }
        else if (is.language(part) && is.recursive(part)) {
            for (i in seq_along(part))
                part[i] <- list(recurse(part[[i]]))
            part
        }
        else part
    }
    if (is.function(fn)) {
        if (!is.primitive(fn)) {
            attr(fn, "srcref") <- NULL
            at <- attributes(fn)
            formals(fn) <- recurse(formals(fn))
            attr(body(fn), "wholeSrcref") <- NULL
            attr(body(fn), "srcfile") <- NULL
            body(fn) <- recurse(body(fn))
            if (!is.null(at))
                attributes(fn) <- at
        }
        fn
    }
    else if (is.language(fn)) {
        recurse(fn)
    }
    else stop("argument is not a function or language object:", typeof(fn))
}


.R_FunctionSymbol <- as.symbol("function")


.removeSource_from_inner_functions <- function (fn)
{
    ## allow a function/language object to keep its source references
    ## while removing source references from any sub functions
    recurse <- function(part) {
        if (is.name(part))
            part
        else if (is.call(part) && identical(part[[1L]], .R_FunctionSymbol))
            .removeSource(part)
        else if (is.language(part) && is.recursive(part)) {
            for (i in seq_along(part))
                part[i] <- list(recurse(part[[i]]))
            part
        }
        else part
    }
    if (is.function(fn)) {
        if (!is.primitive(fn)) {
            at <- attributes(fn)
            formals(fn) <- lapply(formals(fn), recurse)
            body(fn) <- recurse(body(fn))
            if (!is.null(at))
                attributes(fn) <- at
        }
        fn
    }
    else if (is.language(fn)) {
        recurse(fn)
    }
    else stop("argument is not a function or language object:", typeof(fn))
}
