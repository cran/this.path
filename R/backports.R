

if (getRversion() < "3.0.0") {


delayedAssign(".C_mapply", { getNativeSymbolInfo("do_mapply", PACKAGE = "base") })


.mapply <- function (FUN, dots, MoreArgs)
.Call(.C_mapply, match.fun(FUN), dots, MoreArgs, environment())


list.files <- function (path = ".", pattern = NULL, all.files = FALSE, full.names = FALSE,
    recursive = FALSE, ignore.case = FALSE, include.dirs = FALSE,
    no.. = FALSE)
{
    no.. <- .asLogical(no..)
    if (is.na(no..))
        stop(gettextf("invalid '%s' argument", "no..", domain = "R"), domain = NA)
    value <- .BaseNamespaceEnv$list.files(path = path, pattern = pattern,
        all.files = all.files, full.names = full.names, recursive = recursive,
        ignore.case = ignore.case, include.dirs = include.dirs)
    if (all.files && no..)
        value[!(basename2(value) %in% c(".", ".."))]
    else value
}


## parse(keep.source = getOption("keep.source")) was added in R 3.0.0
parse <- .removeSource(evalq(envir = .BaseNamespaceEnv,
         function (file = "", n = NULL, text = NULL, prompt = "?", keep.source = getOption("keep.source"),
    srcfile = NULL, encoding = "unknown")
{
    if (missing(srcfile)) {
        if (!missing(keep.source)) {
            opt.keep.source <- getOption("keep.source")
            if (isTRUE(keep.source) != isTRUE(opt.keep.source)) {
                on.exit(options(keep.source = opt.keep.source))
                options(keep.source = keep.source)
            }
        }
        parse(file = file, n = n, text = text, prompt = prompt, srcfile = , encoding = encoding)
    }
    else parse(file = file, n = n, text = text, prompt = prompt, srcfile = srcfile, encoding = encoding)
}
))


}


if (getRversion() < "3.1.0") {


anyNA <- function (x, recursive = FALSE)
.External2(.C_anyNA, x, recursive)


.anyNA_dispatch <- function (x, recursive = FALSE)
UseMethod("anyNA")


anyNA.data.frame <- function (x, recursive = FALSE)
.External2(.C_anyNA.data.frame, x, recursive)


anyNA.numeric_version <- function (x, recursive = FALSE)
.External2(.C_anyNA.numeric_version, x)


anyNA.POSIXlt <- function (x, recursive = FALSE)
anyNA(as.POSIXct.POSIXlt(x))


.anyNA.default <- function (x, recursive = FALSE)
.External2(.C_anyNA.default, x, recursive)


}


if (getRversion() < "3.2.0") {


isNamespaceLoaded <- function (name)
.External2(.C_isRegisteredNamespace, name)


dir.exists <- function (paths)
.External2(.C_dir.exists, paths)


file.copy <- function (from, to, overwrite = recursive, recursive = FALSE,
    copy.mode = TRUE, copy.date = FALSE)
{
    okay <- .BaseNamespaceEnv$file.copy(from, to, overwrite = overwrite,
        recursive = recursive, copy.mode = copy.mode)
    if (copy.date) {
        if (any(okay)) {
            if (length(to) == 1L && dir.exists(to)) {
                from <- from[okay]
                to <- path.join(to, basename2(from))
                if (recursive) {
                    n <- lapply(from, list.files, all.files = TRUE,
                        recursive = TRUE, include.dirs = TRUE)
                    times <- lengths(n)
                    n <- unlist(n)
                    from <- c(path.join(rep(from, times), n), from)
                    to <- c(path.join(rep(to, times), n), to)
                }
                Sys.setFileTime(to, file.mtime(from))
            }
            else Sys.setFileTime(to[okay], file.mtime(from[okay]))
        }
    }
    okay
}


lengths <- function (x, use.names = TRUE)
.External2(.C_lengths, x, use.names)


.lengths.default <- function (x, use.names = TRUE)
.External2(.C_lengths.default, x, use.names)


## file.info(extra_cols = TRUE) was added in R 3.2.0
file.info <- .removeSource(evalq(envir = .BaseNamespaceEnv,
             function (..., extra_cols = TRUE)
{
    if (extra_cols)
        file.info(...)
    else file.info(...)[1:6]
}
))


file.mtime <- .removeSource(evalq(envir = .BaseNamespaceEnv,
              function (...)
file.info(...)$mtime
))


file.size <- .removeSource(evalq(envir = .BaseNamespaceEnv,
             function (...)
file.info(...)$size
))


.isdir <- .removeSource(evalq(envir = .BaseNamespaceEnv,
          function (...)
file.info(...)$isdir
))


} else {  ## (getRversion() >= "3.2.0")


.isdir <- .removeSource(evalq(envir = .BaseNamespaceEnv,
          function (...)
file.info(..., extra_cols = FALSE)$isdir
))


}


if (getRversion() < "3.3.0") {


strrep <- function (x, times)
{
    if (!is.character(x))
        x <- as.character(x)
    .External2(.C_strrep, x, as.integer(times))
}


startsWith <- function (x, prefix)
.External2(.C_startsWith, x, prefix)


endsWith <- function (x, suffix)
.External2(.C_endsWith, x, suffix)


}


if (getRversion() < "3.4.0") {


print.connection <- .removeSource(evalq(envir = .BaseNamespaceEnv,
                    function (x, ...)
{
    usumm <- tryCatch(unlist(summary(x)), error = function(e) {
    })
    if (is.null(usumm)) {
        cl <- oldClass(x)
        cl <- cl[cl != "connection"]
        cat("A connection, ", if (length(cl))
            paste0("specifically, ", paste(sQuote(cl), collapse = ", "),
                ", "), "but invalid.\n", sep = "")
    }
    else {
        cat("A connection with")
        print(cbind(` ` = usumm), ...)
    }
    invisible(x)
}
))


.withAutoprint <- function (exprs, evaluated = FALSE, local = parent.frame(), print. = TRUE,
    echo = TRUE, max.deparse.length = Inf, width.cutoff = max(20, getOption("width")),
    deparseCtrl = c("keepInteger", "showAttributes", "keepNA"),
    skip.echo = 0, spaced = FALSE, ...)
{
    if (!evaluated) {
        exprs <- substitute(exprs)
        if (is.call(exprs)) {
            if (typeof(exprs[[1L]]) == "symbol" && exprs[[1L]] == "{") {
                exprs <- as.list(exprs)[-1L]
                if (missing(skip.echo) &&
                    length(exprs) &&
                    typeof(srcref <- attr(exprs, "srcref", exact = TRUE)) == "list")
                {
                    skip.echo <- srcref[[1L]][7L] - 1L
                }
            }
        }
    }
    if (!is.expression(exprs))
        exprs <- as.expression(exprs)
    conn <- textConnection(.code2character(exprs, width.cutoff = width.cutoff, deparseCtrl = deparseCtrl))
    on.exit(close(conn))
    source(file = conn, local = local, print.eval = print., echo = echo,
        max.deparse.length = max.deparse.length, skip.echo = skip.echo, ...)
}


withAutoprint <- .withAutoprint


} else {  ## (getRversion() >= "3.4.0")


.withAutoprint <- .removeSource(evalq(envir = .BaseNamespaceEnv,
                  function (exprs, evaluated = FALSE, local = parent.frame(), print. = TRUE,
    echo = TRUE, max.deparse.length = Inf, width.cutoff = max(20, getOption("width")),
    deparseCtrl = c("keepInteger", "showAttributes", "keepNA"),
    skip.echo = 0, ...)
{
    if (!evaluated) {
        exprs <- substitute(exprs)
        if (is.call(exprs)) {
            if (typeof(exprs[[1L]]) == "symbol" && exprs[[1L]] == "{") {
                exprs <- as.list(exprs)[-1L]
                if (missing(skip.echo) &&
                    length(exprs) &&
                    typeof(srcref <- attr(exprs, "srcref", exact = TRUE)) == "list")
                {
                    skip.echo <- srcref[[1L]][7L] - 1L
                }
            }
        }
    }
    source(exprs = exprs, local = local, print.eval = print.,
        echo = echo, max.deparse.length = max.deparse.length,
        width.cutoff = width.cutoff, deparseCtrl = deparseCtrl,
        skip.echo = skip.echo, ...)
}
))


}


if (getRversion() < "3.5.0") {


...length <- function ()
.External2(.C_...length)


## isTRUE(x) was previously defined as:
## function (x)
## identical(TRUE, x)
isTRUE <- .removeSource(evalq(envir = .BaseNamespaceEnv,
          function (x)
is.logical(x) && length(x) == 1L && !is.na(x) && x
))


isFALSE <- .removeSource(evalq(envir = .BaseNamespaceEnv,
           function (x)
is.logical(x) && length(x) == 1L && !is.na(x) && !x
))


}


if (getRversion() < "3.6.0") {


errorCondition <- .removeSource(evalq(envir = .BaseNamespaceEnv,
                  function (message, ..., class = NULL, call = NULL)
structure(list(message = as.character(message), call = call, ...),
    class = c(class, "error", "condition"))
))


str2expression <- function (text)
{
    if (!is.character(text))
        stop("argument must be character", domain = "R")
    parse(text = text, n = -1, keep.source = FALSE, srcfile = NULL)
}


str2lang <- function (s)
{
    if (!.IS_SCALAR_STR(s))
        stop("argument must be a character string", domain = "R")
    ans <- parse(text = s, n = -1, keep.source = FALSE, srcfile = NULL)
    if (length(ans) != 1L)
        stop(gettextf("parsing result not of length one, but %d", length(ans), domain = "R"), domain = NA)
    ans[[1L]]
}


Sys.setFileTime <- function (path, time)
{
    if (!is.character(path))
        stop("invalid 'path' argument", domain = "R-base")
    time <- as.POSIXct(time)
    if (any(is.na(time)))
        stop("invalid 'time' argument", domain = "R-base")
    n <- length(path)
    if (!n)
        return(logical())
    m <- length(time)
    if (!m)
        stop(gettextf("'%s' must be of length at least one",
            "time", domain = "R"), domain = NA)
    if (m > n)
        time <- time[seq_len(n)]
    as.logical(.mapply(.BaseNamespaceEnv$Sys.setFileTime, list(path, time), NULL))
}


}


if (getRversion() < "4.0.0") {


deparse1 <- .removeSource(evalq(envir = .BaseNamespaceEnv,
            function (expr, collapse = " ", width.cutoff = 500L, ...)
paste(deparse(expr, width.cutoff, ...), collapse = collapse)
))


unlink <- .removeSource(evalq(envir = .BaseNamespaceEnv,
          function (x, recursive = FALSE, force = FALSE, expand = TRUE)
unlink(x, recursive = recursive, force = force)
))


}


if (getRversion() < "4.1.0") {


...elt <- function (n)
.External2(.C_...elt, n)


## bquote(splice = TRUE) was added in R 4.1.0
bquote <- .removeSource(evalq(envir = .BaseNamespaceEnv,
          function (expr, where = parent.frame(), splice = FALSE)
{
    if (!is.environment(where))
        where <- as.environment(where)
    unquote <- function(e) {
        if (is.pairlist(e))
            as.pairlist(lapply(e, unquote))
        else if (is.call(e)) {
            if (typeof(e[[1L]]) == "symbol" && e[[1L]] == ".")
                eval(e[[2L]], where)
            else if (splice) {
                if (typeof(e[[1L]]) == "symbol" && e[[1L]] == "..")
                    stop("can only splice inside a call", call. = FALSE)
                else as.call(unquote.list(e))
            }
            else as.call(lapply(e, unquote))
        }
        else e
    }
    is.splice.macro <- function(e) is.call(e) && typeof(e[[1L]]) == "symbol" && e[[1L]] == ".."
    unquote.list <- function(e) {
        p <- Position(is.splice.macro, e, nomatch = NULL)
        if (is.null(p))
            lapply(e, unquote)
        else {
            n <- length(e)
            head <- if (p == 1)
                NULL
            else e[1:(p - 1)]
            tail <- if (p == n)
                NULL
            else e[(p + 1):n]
            macro <- e[[p]]
            mexp <- eval(macro[[2L]], where)
            if (!is.vector(mexp) && !is.expression(mexp))
                stop("can only splice vectors")
            c(lapply(head, unquote), mexp, as.list(unquote.list(tail)))
        }
    }
    unquote(substitute(expr))
}
))


}


if (getRversion() < "4.2.0") {


## gettext(trim = TRUE) was added in R 4.2.0
gettext <- .removeSource(evalq(envir = .BaseNamespaceEnv,
           function (..., domain = NULL, trim = TRUE)
{
    gettext(..., domain = domain)
}
))


## gettextf(trim = TRUE) was added in R 4.2.0
gettextf <- .removeSource(evalq(envir = .BaseNamespaceEnv,
            function (fmt, ..., domain = NULL, trim = TRUE)
sprintf(gettext(fmt, domain = domain), ...)
))


}
