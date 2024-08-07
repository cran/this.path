local({
    owd <- getwd()
    if (is.null(owd)) {
        message("cannot 'chdir' as current directory is unknown")
        return(invisible())
    } else on.exit(setwd(owd), add = TRUE)


    ## test for 3 specific cases of sourcing
    ## * sourcing a file by specifying its basename
    ## * sourcing a file by specifying its absolute path
    ## * sourcing a file by specifying one of its relative paths
    abs_path_R <- tempfile("test", fileext = ".R")
    on.exit(unlink(abs_path_R), add = TRUE)
    this.path:::.writeCode({
        n <- this.path:::.getframenumber()
        if (is.na(n) || n < 1L) stop("invalid traceback")
        sym <- ".this.path::document.context"
        frame <- sys.frame(n)
        if (!exists(sym, envir = frame, inherits = FALSE))
            sym <- ".this.path::document.contexts"
        stopifnot(bindingIsLocked(sym, frame))
        cat("\n> getwd()\n")
        print(getwd())
        cat("\n> ", paste(deparse(call("dynGet", sym)), collapse = "\n+ "), "\n", sep = "")
        print(frame[[sym]])
        cat("\n> this.path::sys.path(original = TRUE)\n")
        print(this.path::sys.path(original = TRUE))
        cat("\n> this.path::sys.path(for.msg = TRUE)\n")
        print(this.path::sys.path(for.msg = TRUE))
        cat("\n> sys.path(verbose = TRUE)\n")
        stopifnot(identical(
            print(this.path::sys.path(verbose = TRUE)),
            getOption("this.path::sys.path() expectation")
        ))
        cat("\n> this.path::sys.path(original = TRUE)\n")
        print(this.path::sys.path(original = TRUE))
        cat("\n> this.path::sys.path(for.msg = TRUE)\n")
        print(this.path::sys.path(for.msg = TRUE))
    }, file = abs_path_R)
    abs_path_R <- normalizePath(abs_path_R, "/", TRUE)
    abs_path_dir <- normalizePath(R.home(), "/", TRUE)
    basename_R <- this.path::basename2(abs_path_R)
    basename_dir <- this.path::dirname2(abs_path_R)


    rel_path_and_dir <- function(file) {
        x <- this.path::path.split.1(file)
        n <- length(x)
        if (n < 3L) {
            c(this.path::dirname2(file), this.path::basename2(file))
        } else {
            i <- n < seq_len(n) + max(2L, n%/%2L)
            c(this.path::path.unsplit(x[!i]), this.path::path.unsplit(x[i]))
        }
    }
    tmp <- rel_path_and_dir(abs_path_R)
    rel_path_dir <- tmp[[1L]]
    rel_path_R <- tmp[[2L]]
    rm(tmp)


    ## for 'source' and 'debugSource' specifically,
    ## try sourcing a 'file://' URL
    basename_R_URL <- this.path:::.as_file_URL(basename_R)
    rel_path_R_URL <- this.path:::.as_file_URL(rel_path_R)
    abs_path_R_URL <- this.path:::.as_file_URL(abs_path_R)


    fun <- function(expr, envir = parent.frame(),
        bquote.envir = envir, eval.envir = envir)
    {
        if (!is.environment(envir))
            stop("not an environment", domain = "R")
        expr <- call("bquote", substitute(expr), as.symbol("bquote.envir"))
        expr <- eval(expr)
        dep <- deparse(expr)
        cat("\n\n\n\n\n\n\n\n\n\n")
        cat("\n> getwd()\n")
        print(getwd())
        cat("\n> ")
        cat(dep, sep = "\n+ ")
        eval(expr, eval.envir)
    }


    oopt <- options(`this.path::sys.path() expectation` = normalizePath(abs_path_R, "/", TRUE))
    on.exit(options(oopt), add = TRUE)


    ## try using source in all possible manners
    setwd(basename_dir)
    fun(source(.(basename_R)                                 , local = TRUE, chdir = FALSE))               ## from a  basename                    without changing directory
    fun(source(.(basename_R)                                 , local = TRUE, chdir = TRUE ))               ## from a  basename                    with    changing directory (shouldn't do anything)
    fun(source(.(basename_R_URL)                             , local = TRUE))                              ## from a  basename      'file://' URL
    fun(source(print(conn <- file(open = "r", .(basename_R))), local = TRUE))               ; close(conn)  ## from a  basename      connection
    setwd(rel_path_dir)
    fun(source(.(rel_path_R)                                 , local = TRUE, chdir = FALSE))               ## from a  relative path               without changing directory
    fun(source(.(rel_path_R)                                 , local = TRUE, chdir = TRUE ))               ## from a  relative path               with    changing directory
    fun(source(.(rel_path_R_URL)                             , local = TRUE))                              ## from a  relative path 'file://' URL
    fun(source(print(conn <- file(open = "r", .(rel_path_R))), local = TRUE))               ; close(conn)  ## from a  relative path connection
    setwd(abs_path_dir)
    fun(source(.(abs_path_R)                                 , local = TRUE, chdir = FALSE))               ## from an absolute path               without changing directory
    fun(source(.(abs_path_R)                                 , local = TRUE, chdir = TRUE ))               ## from an absolute path               with    changing directory
    fun(source(.(abs_path_R_URL)                             , local = TRUE))                              ## from an absolute path 'file://' URL
    fun(source(print(conn <- file(open = "r", .(abs_path_R))), local = TRUE))               ; close(conn)  ## from an absolute path connection


    ## 'sys.source' cannot handle 'file://' URLs nor connections
    setwd(basename_dir)
    fun(sys.source(.(basename_R), envir = environment(), chdir = FALSE))
    fun(sys.source(.(basename_R), envir = environment(), chdir = TRUE ))
    setwd(rel_path_dir)
    fun(sys.source(.(rel_path_R), envir = environment(), chdir = FALSE))
    fun(sys.source(.(rel_path_R), envir = environment(), chdir = TRUE ))
    setwd(abs_path_dir)
    fun(sys.source(.(abs_path_R), envir = environment(), chdir = FALSE))
    fun(sys.source(.(abs_path_R), envir = environment(), chdir = TRUE ))


    ## 'debugSource' cannot handle connections
    if (.Platform$GUI == "RStudio") {
        debugSource <- get("debugSource", "tools:rstudio", inherits = FALSE)
        setwd(basename_dir)
        fun(debugSource(.(basename_R)    ))
        fun(debugSource(.(basename_R_URL)))
        setwd(rel_path_dir)
        fun(debugSource(.(rel_path_R)    ))
        fun(debugSource(.(rel_path_R_URL)))
        setwd(abs_path_dir)
        fun(debugSource(.(abs_path_R)    ))
        fun(debugSource(.(abs_path_R_URL)))
    }


    ## 'compiler::loadcmp' cannot handle 'file://' URLs nor connections
    if (requireNamespace("compiler", quietly = TRUE)) {
        basename_Rc <- basename_R; this.path::ext(basename_Rc) <- ".Rc"
        rel_path_Rc <- rel_path_R; this.path::ext(rel_path_Rc) <- ".Rc"
        abs_path_Rc <- abs_path_R; this.path::ext(abs_path_Rc) <- ".Rc"


        on.exit(unlink(abs_path_Rc), add = TRUE)
        compiler::cmpfile(abs_path_R, abs_path_Rc)


        opt2 <- options(`this.path::sys.path() expectation` = normalizePath(abs_path_Rc, "/", TRUE))
        setwd(basename_dir)
        fun(compiler::loadcmp(.(basename_Rc), envir = environment(), chdir = FALSE))
        fun(compiler::loadcmp(.(basename_Rc), envir = environment(), chdir = TRUE ))
        setwd(rel_path_dir)
        fun(compiler::loadcmp(.(rel_path_Rc), envir = environment(), chdir = FALSE))
        fun(compiler::loadcmp(.(rel_path_Rc), envir = environment(), chdir = TRUE ))
        setwd(abs_path_dir)
        fun(compiler::loadcmp(.(abs_path_Rc), envir = environment(), chdir = FALSE))
        fun(compiler::loadcmp(.(abs_path_Rc), envir = environment(), chdir = TRUE ))
        options(opt2)
    }


    ## 'utils::Sweave' cannot handle 'file://' URLs nor connections
    if (requireNamespace("utils", quietly = TRUE)) {
        basename_Rnw <- basename_R; this.path::ext(basename_Rnw) <- ".Rnw"
        abs_path_Rnw <- abs_path_R; this.path::ext(abs_path_Rnw) <- ".Rnw"


        on.exit(unlink(abs_path_Rnw), add = TRUE)
        writeLines(c(
            "\\documentclass{article}",
            "",
            "\\begin{document}",
            "",
            "<<>>=",
            ## remove expressions starting with 'cat'
            {
                exprs <- parse(abs_path_R)
                exprs <- exprs[!vapply(exprs, function(expr) {
                    is.call(expr) && identical(expr[[1L]], as.symbol("cat"))
                }, NA, USE.NAMES = FALSE)]
                this.path:::.writeCode(exprs, NULL)
            },
            "@",
            "",
            "\\end{document}"
        ), abs_path_Rnw)


        opt2 <- options(`this.path::sys.path() expectation` = normalizePath(abs_path_Rnw, "/", TRUE))
        setwd(basename_dir)
        outputname <- fun(utils::Sweave(.(basename_Rnw)))
        writeLines(readLines(outputname))
        unlink(outputname)


        tmpdir <- tempfile("dir")
        on.exit(unlink(tmpdir, recursive = TRUE, force = TRUE), add = TRUE)
        dir.create(tmpdir)
        setwd(tmpdir)
        writeLines(readLines(fun(utils::Sweave(.(this.path::path.join("..", basename_Rnw))))))
        options(opt2)
    }


    ## 'box::use' cannot handle 'file://' URLs nor connections nor absolute paths
    if (requireNamespace("box", quietly = TRUE)) {
        tmp <- c(sub("\\.R$", ".r", abs_path_R), abs_path_R)
        tmp <- tmp[[match(TRUE, file.exists(tmp))]]
        opt2 <- options(`this.path::sys.path() expectation` = normalizePath(tmp, "/", TRUE))
        setwd(basename_dir); box::set_script_path(this.path::path.join(basename_dir, "."))
        fun(box::use(module = ./.(as.symbol(sub("\\.R$", "", basename_R))))); box::unload(module)
        if (!this.path:::.is_abs_path(rel_path_R)) {
            tmp_fun <- function(x) {
                n <- length(x)
                if (n > 1L)
                    call("/", tmp_fun(x[-n]), as.symbol(x[[n]]))
                else as.symbol(x[[1L]])
            }
            tmp <- tmp_fun(c(".", this.path::path.split.1(sub("\\.R$", "", rel_path_R))))
            setwd(rel_path_dir); box::set_script_path(this.path::path.join(rel_path_dir, "."))
            fun(box::use(module = .(tmp))); box::unload(module)
            rm(tmp, tmp_fun)
        }
        options(opt2)
    }


    ## 'knitr::knit' cannot handle 'file://' URLs
    if (requireNamespace("knitr", quietly = TRUE)) {
        basename_Rmd <- basename_R; this.path::ext(basename_Rmd) <- ".Rmd"
        rel_path_Rmd <- rel_path_R; this.path::ext(rel_path_Rmd) <- ".Rmd"
        abs_path_Rmd <- abs_path_R; this.path::ext(abs_path_Rmd) <- ".Rmd"


        on.exit(unlink(abs_path_Rmd), add = TRUE)
        writeLines(c(
            "```{r}",
            ## remove expressions starting with 'cat'
            {
                exprs <- parse(abs_path_R)
                exprs <- exprs[!vapply(exprs, function(expr) {
                    is.call(expr) && identical(expr[[1L]], as.symbol("cat"))
                }, NA, USE.NAMES = FALSE)]
                this.path:::.writeCode(exprs, NULL)
            },
            "```"
        ), abs_path_Rmd)


        opt2 <- options(`this.path::sys.path() expectation` = normalizePath(abs_path_Rmd, "/", TRUE))
        setwd(basename_dir)
        fun(knitr::knit(.(basename_Rmd)                     , output = stdout(), quiet = TRUE))
        fun(knitr::knit(print(conn <- file(.(basename_Rmd))), output = stdout(), quiet = TRUE)); close(conn)
        setwd(rel_path_dir)
        fun(knitr::knit(.(rel_path_Rmd)                     , output = stdout(), quiet = TRUE))
        fun(knitr::knit(print(conn <- file(.(rel_path_Rmd))), output = stdout(), quiet = TRUE)); close(conn)
        setwd(abs_path_dir)
        fun(knitr::knit(.(abs_path_Rmd)                     , output = stdout(), quiet = TRUE))
        fun(knitr::knit(print(conn <- file(.(abs_path_Rmd))), output = stdout(), quiet = TRUE)); close(conn)
        options(opt2)
    }


    ## 'plumber::plumb' cannot handle 'file://' URLs nor connections
    if (requireNamespace("plumber", quietly = TRUE)) {
        opt2 <- options(`this.path::sys.path() expectation` = normalizePath(abs_path_R, "/", TRUE))
        setwd(basename_dir)
        fun(plumber::plumb(.(basename_R)))
        setwd(rel_path_dir)
        fun(plumber::plumb(.(rel_path_R)))
        setwd(abs_path_dir)
        fun(plumber::plumb(.(abs_path_R)))
        options(opt2)


        entrypoint_R <- this.path::path.join(basename_dir, "entrypoint.R")
        on.exit(unlink(entrypoint_R), add = TRUE)
        writeLines(c(
            readLines(abs_path_R),
            "plumber::Plumber$new()"
        ), entrypoint_R)
        opt2 <- options(`this.path::sys.path() expectation` = normalizePath(entrypoint_R, "/", TRUE))
        setwd(basename_dir)
        fun(plumber::plumb())
        setwd(rel_path_dir)
        fun(plumber::plumb(dir = .(dirname(rel_path_R))))
        setwd(abs_path_dir)
        fun(plumber::plumb(dir = .(dirname(abs_path_R))))
        options(opt2)
    }


    ## 'shiny::runApp'
    if (requireNamespace("shiny", quietly = TRUE)) {
        shinytmp <- tempfile("shinytmp", tmpdir = basename_dir)
        on.exit(unlink(shinytmp, recursive = TRUE, force = TRUE), add = TRUE)
        dir.create(shinytmp)
        file <- this.path::path.join(shinytmp, "app.R")
        writeLines(c(
            readLines(abs_path_R),
            "stop(structure(list(message = \"\", call = NULL), class = c(\"this_path_tests_R_catch_this_error\", \"error\", \"condition\")))"
        ), file)
        opt2 <- options(`this.path::sys.path() expectation` = normalizePath(file, "/", TRUE))
        rm(file)
        abs_path_app_R <- normalizePath(shinytmp, "/", TRUE)
        abs_path_app_dir <- abs_path_dir
        basename_app_R <- "."
        basename_app_dir <- abs_path_app_R
        tmp <- rel_path_and_dir(abs_path_app_R)
        rel_path_app_dir <- tmp[[1L]]
        rel_path_app_R <- tmp[[2L]]
        rm(tmp)
        setwd(basename_app_dir)
        this.path::tryCatch3({
            fun(shiny::runApp(.(basename_app_R)))
        }, this_path_tests_R_catch_this_error = )
        setwd(rel_path_app_dir)
        this.path::tryCatch3({
            fun(shiny::runApp(.(rel_path_app_R)))
        }, this_path_tests_R_catch_this_error = )
        setwd(abs_path_app_dir)
        this.path::tryCatch3({
            fun(shiny::runApp(.(abs_path_app_R)))
        }, this_path_tests_R_catch_this_error = )
        options(opt2)
    }


    ## 'testthat::source_file' cannot handle 'file://' URLs nor connections
    if (requireNamespace("testthat", quietly = TRUE)) {
        setwd(basename_dir)
        fun(testthat::source_file(.(basename_R), env = environment(), chdir = FALSE, wrap = FALSE))
        fun(testthat::source_file(.(basename_R), env = environment(), chdir = FALSE, wrap = TRUE ))
        fun(testthat::source_file(.(basename_R), env = environment(), chdir = TRUE , wrap = FALSE))
        fun(testthat::source_file(.(basename_R), env = environment(), chdir = TRUE , wrap = TRUE ))
        setwd(rel_path_dir)
        fun(testthat::source_file(.(rel_path_R), env = environment(), chdir = FALSE, wrap = FALSE))
        fun(testthat::source_file(.(rel_path_R), env = environment(), chdir = FALSE, wrap = TRUE ))
        fun(testthat::source_file(.(rel_path_R), env = environment(), chdir = TRUE , wrap = FALSE))
        fun(testthat::source_file(.(rel_path_R), env = environment(), chdir = TRUE , wrap = TRUE ))
        setwd(abs_path_dir)
        fun(testthat::source_file(.(abs_path_R), env = environment(), chdir = FALSE, wrap = FALSE))
        fun(testthat::source_file(.(abs_path_R), env = environment(), chdir = FALSE, wrap = TRUE ))
        fun(testthat::source_file(.(abs_path_R), env = environment(), chdir = TRUE , wrap = FALSE))
        fun(testthat::source_file(.(abs_path_R), env = environment(), chdir = TRUE , wrap = TRUE ))
    }


    invisible()
})


local({
    FILE.R <- tempfile(fileext = ".R")
    on.exit(unlink(FILE.R))
    this.path:::.writeCode({
        stopifnot(identical(
            this.path::this.path(),
            getOption("this.path::this.path() expectation")
        ))
    }, FILE.R)
    oopt <- options(
        `this.path::this.path() expectation` = normalizePath(FILE.R, "/", TRUE),
        keep.source = TRUE
    )
    on.exit(options(oopt), add = TRUE)
    eval(
        parse(FILE.R),
        structure(list2env(list(.packageName = FILE.R), parent = .BaseNamespaceEnv), path = FILE.R)
    )
    eval(parse(FILE.R))
})


local({
    FILE.R <- tempfile(fileext = ".R")
    on.exit(unlink(FILE.R))
    this.path:::.writeCode({
        list(
            this.path::src.path(original = TRUE),
            this.path::src.path(original = NA),
            this.path::src.path(),
            this.path::src.path(original = TRUE),
            this.path::src.path(original = NA)
        )
    }, FILE.R)
    oopt <- options(keep.source = TRUE)
    on.exit(options(oopt), add = TRUE)
    stopifnot(identical(
        eval(parse(FILE.R)),
        list(FILE.R, normalizePath(FILE.R, "/", TRUE))[c(1L, 1L, 2L, 1L, 2L)]
    ))
})


local({
    FILE1.R <- tempfile(pattern = "file1_", fileext = ".R")
    on.exit(unlink(FILE1.R), add = TRUE)
    this.path:::.writeCode({
        fun <- function(x) x
        fun1 <- function() fun(this.path::src.path())
    }, FILE1.R)
    source(FILE1.R, environment(), keep.source = TRUE)


    FILE2.R <- tempfile(pattern = "file2_", fileext = ".R")
    on.exit(unlink(FILE2.R), add = TRUE)
    this.path:::.writeCode({
        fun2 <- function() fun(this.path::src.path())
    }, FILE2.R)
    source(FILE2.R, environment(), keep.source = TRUE)


    ## it might seem weird to use eval(expression())
    ## it is just to prevent the expressions from having source references
    stopifnot(identical(eval(expression(fun1())), normalizePath(FILE1.R, "/", TRUE)))
    stopifnot(identical(eval(expression(fun2())), normalizePath(FILE2.R, "/", TRUE)))


    FILE3.R <- tempfile("file3_", fileext = ".R")
    on.exit(unlink(FILE3.R), add = TRUE)
    this.path:::.writeCode({
        x <- list(fun1(), fun2(), fun(this.path::src.path()))
    }, FILE3.R)
    source(FILE3.R, environment(), keep.source = TRUE)
    stopifnot(identical(x, as.list(normalizePath(c(FILE1.R, FILE2.R, FILE3.R), "/", TRUE))))
})
