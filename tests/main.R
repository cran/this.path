owd <- getwd()


base.path  <- "test.R"
full.path  <- file.path(this.path::this.dir(verbose = FALSE), base.path)
short.path <- file.path(basename(dirname(full.path))        , base.path)


base.path.dir  <- dirname(full.path)
short.path.dir <- dirname(base.path.dir)
full.path.dir  <- tempdir(check = TRUE)


setwd(base.path.dir)
source(base.path            , chdir = FALSE)
source(base.path            , chdir = TRUE )
source(x <- file(base.path) )               ; close(x)
setwd(short.path.dir)
source(short.path           , chdir = FALSE)
source(short.path           , chdir = TRUE )
source(x <- file(short.path))               ; close(x)
setwd(full.path.dir)
source(full.path            , chdir = FALSE)
source(full.path            , chdir = TRUE )
source(x <- file(full.path) )               ; close(x)


setwd(base.path.dir)
sys.source(base.path , chdir = FALSE)
sys.source(base.path , chdir = TRUE )
setwd(short.path.dir)
sys.source(short.path, chdir = FALSE)
sys.source(short.path, chdir = TRUE )
setwd(full.path.dir)
sys.source(full.path , chdir = FALSE)
sys.source(full.path , chdir = TRUE )


if (.Platform$GUI == "RStudio") {
    debug.source <- get("debugSource", mode = "function",
        "tools:rstudio", inherits = FALSE)
    setwd(base.path.dir)
    debug.source(base.path )
    debug.source(base.path )
    setwd(short.path.dir)
    debug.source(short.path)
    debug.source(short.path)
    setwd(full.path.dir)
    debug.source(full.path )
    debug.source(full.path )
}


unload.testthat.namespace <- !isNamespaceLoaded("testthat")
if (requireNamespace("testthat", quietly = TRUE)) {
    setwd(base.path.dir)
    testthat::source_file(base.path , chdir = FALSE)
    testthat::source_file(base.path , chdir = TRUE )
    setwd(short.path.dir)
    testthat::source_file(short.path, chdir = FALSE)
    testthat::source_file(short.path, chdir = TRUE )
    setwd(full.path.dir)
    testthat::source_file(full.path , chdir = FALSE)
    testthat::source_file(full.path , chdir = TRUE )
}


if (!is.null(owd))
    setwd(owd)
if (unload.testthat.namespace)
    unloadNamespace("testthat")
