\name{this.path}
\alias{this.path}
\alias{this.dir}
\title{Determine Executing Script's Filename
}
\description{
\code{this.path()} returns the full path of the executing script.

\code{this.dir()} is a shortcut for
\code{\link[base:basename]{dirname}(this.path())}, returning the full path of
the directory in which the executing script is located.
}
\usage{
this.path(verbose = getOption("verbose"))
this.dir(verbose = getOption("verbose"))
}
\arguments{
  \item{verbose}{
\code{TRUE} or \code{FALSE}; should the method in which the path of the
executing script was determined be printed?
  }
}
\details{
There are three ways in which \R code is typically run; in \sQuote{RStudio} or
\sQuote{RGui} by running the current line or selection with the \strong{Run}
button (or appropriate keyboard shortcut), through a source call (a call to
function \code{base::source} or \code{base::sys.source} or \code{debugSource}
(\sQuote{RStudio} exclusive) or
\code{\link[testthat:source_file]{testthat::source_file}}), and finally from the
command-line / / terminal.

To retrieve the executing script's filename, first an attempt is made to find a
source call. The calls are searched in reverse order so as to grab the most
recent source call in the case of nested source calls. If a source call was
found, the argument \var{file} (\var{fileName} in the case of
\code{debugSource}, \var{path} in the case of \code{testthat::source_file}) is
returned from the function's evaluation environment (not the function's
environment).

If no source call is found up the calling stack, then an attempt is made to
figure out how \R is currently being used.

If \R is being run from the command-line / / terminal, the command-line
arguments are searched for \option{-f file} or \option{--file=file} (the two
methods of taking input from \sQuote{file}). If \option{-f file} is used, then
\sQuote{file} is returned. If \option{--file=file} is used, then the text
following \option{--file=} is returned. When multiple arguments of either type
are supplied, the last of these arguments is returned (with a warning). It is an
error to use \code{this.path} when no arguments of either type are supplied.

If \R is being run from \sQuote{RStudio}, the source document's filename (the
document open in the current tab) is returned (at the time of evaluation). It is
important to not leave the current tab (either by closing or switching tabs)
while any calls to \code{this.path} have yet to be evaluated in the run
selection. It is an error for no documents to be open or for a document to not
exist (not saved anywhere).

If \R is being run from \sQuote{RGui}, the source document's filename (the
document most recently interacted with besides the \R Console) is returned (at
the time of evaluation). It is important to not leave the current document
(either by closing the document or interacting with another document) while any
calls to \code{this.path} have yet to be evaluated in the run selection. It is
an error for no documents to be open or for a document to not exist (not saved
anywhere).

If \R is being run in another manner, it is an error to use \code{this.path}.
}
\value{
A character vector of length 1; the executing script's filename.
}
\note{
The first time \code{this.path} is called within a script, it will
\code{\link[base:normalizePath]{normalize}} the script's path, check that the
script exists (throwing an error if it does not), and save it in the appropriate
environment. When \code{this.path} is called subsequent times within the same
script, it returns the saved path. This will be faster than the first time, will
not check for file existence, and will not depend on the working directory. This
means that a script can delete itself using
\code{file.remove(this.path::this.path())} but still know its own path for the
remainder of the script.

Within a script that contains calls to both \code{this.path} and
\code{\link[base:getwd]{setwd}}, \code{this.path} \emph{MUST} be used
\emph{AT LEAST} once before the first call to \code{setwd} since the script's
path is normalized against the working directory.
}
\seealso{
\code{\link{this.path-package}}

\code{\link[base]{source}}

\code{\link[base]{sys.source}}

\code{\link[testthat:source_file]{testthat::source_file}}

\code{\link{Running.R.from.the.command-line}}
}
\examples{
\dontrun{
The following will create a temporary R script containing
calls to 'this.path'. You should see that 'this.path' works
through a call to 'source', a call to 'sys.source', a call
to 'debugSource' (if running from 'RStudio'), and when
running R from the command-line / / terminal.

Unfortunately, it is impossible to use 'example(this.path)'
to demonstrate the functionality of 'this.path' in 'RStudio'
and 'RGui'. If you would like to see this functionality, you
could try this:
* make a new R script containing just this one command:
    this.path::this.path(verbose = TRUE)
* open this script in 'RStudio' or 'RGui'
* run that command directly from the script
  (both should print "Source: active document ..." along
      with the script's path)
* copy and paste that command into the R Console and run
  that command again
  (both should print "Source: source document ..." along
      with the script's path)
* try closing all your documents and run that same command
  in the R Console
  (both should raise an error "R is being run ... with no
      documents open")
}

tryCatch((function() {
    .interactive <- interactive()
    if (.interactive) {
        cat("\\n")
        prompt <- "Would you like to run this example interactively? (Yes/No/Cancel): "
        repeat {
            tmp <- tolower(substr(readline(prompt), 1L, 1L))
            if (tmp \%in\% c("y", "n", "c"))
                break
        }
        if (tmp == "c") {
            cat("\\n")
            return(invisible())
        }
        .interactive <- tmp == "y"
    }
    if (.interactive) {
        pressEnter2Continue <- function(x = "\\n") {
            readline("Hit <Return> to continue: ")
            cat(x)
        }
    }
    else pressEnter2Continue <- function(...) NULL


    oopt <- options(useFancyQuotes = TRUE)
    on.exit(options(oopt))


    tryCatch({
        tmp.R.script <- normalizePath(tempfile(
            pattern = "this.path.example.R.script.",
            tmpdir = tempdir(check = TRUE), fileext = ".R"
        ), mustWork = FALSE)
        on.exit(suppressWarnings(file.remove(tmp.R.script)), add = TRUE)
    }, condition = function(c) {
        stop(errorCondition(paste0(conditionMessage(c),
            "\\nunable to create temporary R script"),
            call = conditionCall(c)))
    })


    results.file <- tryCatch({
        .Sys.time <- format(Sys.time(), format = "\%Y-\%m-\%d_\%H.\%M.\%OS.")
        normalizePath(tempfile(
            pattern = paste0("this.path.example.results.", .Sys.time),
            tmpdir = dirname(tmp.R.script), fileext = ".txt"
        ), mustWork = FALSE)
    }, condition = as.null)


    write.results <- function(expr) {
        if (!is.null(results.file)) {
            sink(file = results.file, append = TRUE)
            on.exit(sink())
        }
        expr
    }


    tmp.R.script.code <- substitute({
        options(useFancyQuotes = TRUE)
        results.file <- `results.file sub`
        write.results <- `write.results sub`
        cat("\\n")
        write.results({
            x <- tryCatch({
                this.path::this.path(verbose = TRUE)
                TRUE
            }, condition = force)
            cat("this.path status : ")
        })
        if (!isTRUE(x)) {
            msg <- conditionMessage(x)
            call <- conditionCall(x)
            write.results({
                if (!is.null(call))
                  cat("Error in ", deparse(call), " :\\n  ", msg, "\\n",
                      sep = "")
                else cat("Error: ", msg, "\\n", sep = "")
            })
            if (!is.null(call))
                cat("Error in ", deparse(call), " :\\n  ", msg, "\\n", sep = "")
            else cat("Error: ", msg, "\\n", sep = "")
            cat(sQuote("this.path"), " could not determine the executing ",
                "script's filename\\n", sep = "")
        }
        else {
            cat("Executing script's filename:\\n")
            cat(sQuote(`tmp.R.script sub`), "\\n\\n", sep = "")
            cat("Executing script's filename (as determined by ",
                sQuote("this.path"), "):\\n", sep = "")
            cat(sQuote(this.path::this.path(verbose = TRUE)), "\\n", sep = "")
            if (`tmp.R.script sub` != this.path::this.path(verbose = FALSE)) {
                write.results({
                  cat("Error: ", sQuote("this.path"), " could not correctly ",
                    "determine the executing script's filename\\n", sep = "")
                })
                cat("\\nError: ", sQuote("this.path"), " could not correctly ",
                  "determine the executing script's filename\\n", sep = "")
            }
            else write.results(cat("success\\n"))
        }
    }, list(
        `write.results sub` = write.results,
        `tmp.R.script sub` = tmp.R.script,
        `results.file sub` = results.file
    ))


    writeRcode2file <- function(x, file) {
        tryCatch({
            lines <- vapply(as.list(x[-1]), function(y) {
                paste0(deparse(y), collapse = "\\n")
            }, FUN.VALUE = "")
            writeLines(lines, con = file)
        }, condition = function(c) {
            stop(errorCondition(paste0(conditionMessage(c),
                "\\nunable to write R code to file ", sQuote(file)),
                call = conditionCall(c)))
        })
    }


    writeRcode2file(tmp.R.script.code, tmp.R.script)


    cat2 <- function(msg, ..., appendLF = TRUE) {
        cat(if (appendLF) "\\n", paste0(strwrap(msg, exdent = 2),
            "\\n", collapse = ""), ..., sep = "")
    }
    cat2(paste0("Created an example R script. This script will be run in ",
        "all possible ways that are compatible with ", sQuote("this.path"),
        " that are currently available."))
    if (.interactive) {
        cat2(paste0("Attempting to open the example R script. If the ",
            "script did not open automatically, the script's path is:"),
            sQuote(tmp.R.script), "\\n")
        tryCatch({
            this.path:::file.open(tmp.R.script)
        }, condition = invisible)
        pressEnter2Continue("")
    }


    write.results(cat2(paste0("Attempting to use ", sQuote("this.path"),
        " when using ", sQuote("source")), appendLF = FALSE))


    tryCatch({
        cat("\\n* first, using ", sQuote("source"), "\\n", sep = "")
        source(tmp.R.script, local = TRUE)
        pressEnter2Continue("")
    }, condition = function(c) {
        msg <- paste0(conditionMessage(c), "\\nunable to source file ",
            sQuote(tmp.R.script))
        call <- conditionCall(c)
        if (!is.null(call))
            cat("Error in ", deparse(call), " :\\n  ", msg, "\\n", sep = "")
        else cat("Error: ", msg, "\\n", sep = "")
    })


    write.results(cat2(paste0("Attempting to use ", sQuote("this.path"),
        " when using ", sQuote("sys.source"))))


    tryCatch({
        cat("\\n* second, using ", sQuote("sys.source"), "\\n", sep = "")
        sys.source(tmp.R.script, envir = environment())
        pressEnter2Continue("")
    }, condition = function(c) {
        msg <- paste0(conditionMessage(c), "\\nunable to source file ",
            sQuote(tmp.R.script))
        call <- conditionCall(c)
        if (!is.null(call))
            cat("Error in ", deparse(call), " :\\n  ", msg, "\\n", sep = "")
        else cat("Error: ", msg, "\\n", sep = "")
    })


    if (.Platform$GUI == "RStudio") {
        write.results(cat2(paste0("Attempting to use ", sQuote("this.path"),
            " when using ", sQuote("debugSource"))))


        tryCatch({
            dbs <- get("debugSource", mode = "function", "tools:rstudio",
                inherits = FALSE)
            cat("\\n* third, using ", sQuote("debugSource"),
                " from ", sQuote("RStudio"), "\\n", sep = "")
            dbs(tmp.R.script, local = TRUE)
            pressEnter2Continue("")
        }, condition = function(c) {
            msg <- paste0(conditionMessage(c), "\\nunable to source file ",
                sQuote(tmp.R.script))
            call <- conditionCall(c)
            if (!is.null(call))
                cat("Error in ", deparse(call), " :\\n  ", msg, "\\n", sep = "")
            else cat("Error: ", msg, "\\n", sep = "")
        })
    }
    else write.results({
        cat2(paste0("Unfortunately, it is impossible to demonstrate the ",
            "functionality of ", sQuote("this.path"), " when using ",
            sQuote("debugSource"), " because ", sQuote("RStudio"), " is not ",
            "presently running."))
    })


    if (!isNamespaceLoaded("testthat") && .interactive) {
        cat2(paste0(sQuote("this.path"), " also works with function ",
            sQuote("source_file"), " from package ", sQuote("testthat"),
            ", but this package is not presently loaded."))
        prompt <- "Would you like to load this package? (Yes/No/Cancel): "
        repeat {
            tmp <- tolower(substr(readline(prompt), 1, 1))
            if (tmp \%in\% c("y", "n", "c"))
                break
        }
        if (tmp == "y") {
            requireNamespace("testthat", quietly = TRUE)
            on.exit(unloadNamespace("testthat"), add = TRUE)
            cat2(paste0("Package ", sQuote("testthat"), " will be unloaded ",
                " once the example concludes."))
        }
    }
    if (isNamespaceLoaded("testthat")) {
        write.results(cat2(paste0("Attempting to use ", sQuote("this.path"),
            " when using ", sQuote("testthat::source_file"))))


        tryCatch({
            cat("\\n* fourth, using ", sQuote("testthat::source_file"),
                "\\n", sep = "")
            testthat::source_file(tmp.R.script)
            pressEnter2Continue("")
        }, condition = function(c) {
            msg <- paste0(conditionMessage(c), "\\nunable to source file ",
                sQuote(tmp.R.script))
            call <- conditionCall(c)
            if (!is.null(call))
                cat("Error in ", deparse(call), " :\\n  ", msg, "\\n", sep = "")
            else cat("Error: ", msg, "\\n", sep = "")
        })
    }
    else write.results({
        cat2(paste0("Unfortunately, it is impossible to demonstrate the ",
            "functionality of ", sQuote("this.path"), " when using ",
            sQuote("source_file"), " because package ", sQuote("testthat"),
            " is not presently loaded."))
    })


    cmt <- if (.Platform$OS.type == "windows")
        "Windows command-line"
    else "Unix terminal"
    write.results(cat2(paste0("Attempting to use ", sQuote("this.path"),
        " when running from the ", cmt)))


    command <- sprintf("Rterm --no-echo --no-restore --file=\%s",
        this.path:::file.encode(tmp.R.script))
    tryCatch({
        cat("\\n* last, running from the ", cmt, "\\n", sep = "")
        cat("\\nProcess finished with exit code ",
            system(command), "\\n", sep = "")
        pressEnter2Continue()
    }, condition = function(c) {
        msg <- paste0(conditionMessage(c), "\\nunable to run file ",
            sQuote(tmp.R.script), "\\n from the ", cmt)
        call <- conditionCall(c)
        if (!is.null(call))
            cat("Error in ", deparse(call), " :\\n  ", msg, "\\n", sep = "")
        else cat("Error: ", msg, "\\n", sep = "")
    })


    write.results({
        cat2(paste0("Unfortunately, it is impossible to use ",
            sQuote("example(this.path)"), " to demonstrate the functionality ",
            "of ", sQuote("this.path"), " in ", sQuote("RStudio"), " and ",
            sQuote("RGui"), ". If you would like to see this functionality, ",
            "you could try this:"),
            "* make a new R script containing just this one command:\\n",
            "    this.path::this.path(verbose = TRUE)\\n",
            "* open this script in ", sQuote("RStudio"), " or ", sQuote("RGui"),
                "\\n",
            "* run that command directly from the script\\n",
            "    (both should print \\"Source: active document ...\\" along ",
                "with the script's path)\\n",
            "* copy and paste that command into the R Console and run that ",
                "command again\\n",
            "    (both should print \\"Source: source document ...\\" along ",
                "with the script's path)\\n",
            "* try closing all your documents and run that same command in ",
                "the R Console\\n",
            "    (both should raise an error \\"R is being run ... with no ",
                "documents open\\")\\n")
    })


    write.results({
        cat2(paste0("If ", sQuote("this.path"), " did not correctly determine ",
            "the executing script's filename, please send a bug report to the ",
            "package maintainer, ",
            utils::packageDescription("this.path")$Maintainer, ". Please ",
            "include your session information in your bug report, which can ",
            "be found with the following command:"), "utils::sessionInfo()\\n")
    })


    if (.interactive) {
        tryCatch({
            this.path:::file.open(results.file)
        }, condition = function(c) {
            cat("\\n")
            cat("* results\\n", readLines(results.file), sep = "\\n")
        })
    }
    else if (!is.null(results.file)) {
        cat("\\n")
        cat("* results\\n", readLines(results.file), sep = "\\n")
    }
    invisible()
})(), condition = function(c) {
    msg <- conditionMessage(c)
    call <- conditionCall(c)
    if (!is.null(call))
        cat("Error in ", deparse(call), " :\\n  ", msg, "\\n", sep = "")
    else cat("Error: ", msg, "\\n", sep = "")
})
}
