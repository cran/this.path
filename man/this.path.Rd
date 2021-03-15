\name{this.path}
\alias{this.path}
\alias{this.dir}
\title{Determine Executing Script's Filename
}
\description{
\code{this.path()} returns the full path of the executing script.

\code{this.dir()} is a shortcut for \code{dirname(this.path())}, returning the
full path of the directory where the executing script is located.
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
The first time \code{this.path} is called within a script, it will normalize the
script's path (express as an absolute path in its canonical form), check that
the script exists (and throw an error if it does not), and save it in the
appropriate environment. When \code{this.path} is called subsequent times within
that same script, it returns the saved path. This will be faster than the first
time, and it will not check for file existence. This means that a script can
delete itself using \code{file.remove(this.path::this.path())} but still know
its own path for the remainder of the script.
}
\seealso{
\code{\link[base]{source}}

\code{\link[base]{sys.source}}

\code{\link[utils]{Rscript}}

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
        cat("\n")
        prompt <- "Would you like to run this example interactively? (Yes/No/Cancel): "
        repeat {
            tmp <- tolower(substr(readline(prompt), 1L, 1L))
            if (tmp \%in\% c("y", "n", "c"))
                break
        }
        if (tmp == "c") {
            cat("\n")
            return(invisible())
        }
        .interactive <- tmp == "y"
    }
    if (.interactive) {
        pressEnter2Continue <- function(x = "\n") {
            readline("Hit <Return> to continue: ")
            cat(x)
        }
    }
    else pressEnter2Continue <- function(...) NULL


    oopt <- options(useFancyQuotes = TRUE)
    on.exit(options(oopt), add = TRUE)


    tryCatch({
        tmp.R.script <- normalizePath(tempfile(
            pattern = "this.path.example.R.script.",
            tmpdir = tempdir(check = TRUE), fileext = ".R"
        ), mustWork = FALSE)
        on.exit(file.remove(tmp.R.script), add = TRUE)
    }, warning = function(c) {
        stop("\nunable to create temporary R script\n* ",
            conditionMessage(c), "\n", sep = "")
    }, error = function(c) {
        stop("\nunable to create temporary R script\n* ",
            conditionMessage(c), "\n", sep = "")
    })


    results.file <- tryCatch({
        .Sys.time <- format(Sys.time(), format = "\%Y-\%m-\%d_\%H.\%M.\%OS.")
        normalizePath(tempfile(
            pattern = paste0("this.path.example.results.", .Sys.time),
            tmpdir = dirname(tmp.R.script), fileext = ".txt"
        ), mustWork = FALSE)
    }, warning = function(c) {
        NULL
    }, error = function(c) {
        NULL
    })


    write.results <- function(expr) {
        if (!is.null(results.file)) {
            sink(file = results.file, append = TRUE)
            on.exit(sink())
        }
        expr
    }


    tmp.R.script.code <- substitute({
        results.file <- `results.file sub`
        write.results <- `write.results sub`
        write.results({
            cat("this.path status : ", x <- tryCatch({
                if (`tmp.R.script sub` == this.path::this.path(verbose = TRUE))
                    "success"
                else stop("'this.path' did not correctly determine the current file")
            }, warning = function(c) {
                paste0("failure\n* ", conditionMessage(c))
            }, error = function(c) {
                paste0("failure\n* ", conditionMessage(c))
            }), "\n", sep = "")
        })
        if (x == "success") {
            cat("\nExecuting script's filename:\n")
            cat(sQuote(`tmp.R.script sub`), "\n\n", sep = "")
            cat("Executing script's filename (as determined by 'this.path'):\n")
            cat(sQuote(this.path::this.path(verbose = TRUE)), "\n", sep = "")
        }
    }, list(
        `write.results sub` = write.results,
        `tmp.R.script sub` = tmp.R.script,
        `results.file sub` = results.file
    ))


    writeRcode2file <- function(x, file) {
        tryCatch({
            lines <- vapply(as.list(x[-1]), function(y) {
                paste0(deparse(y), collapse = "\n")
            }, FUN.VALUE = "")
            writeLines(lines, con = file)
        }, warning = function(c) {
            stop("\nunable to write R code to file: ",
                sQuote(file), "\n* ", conditionMessage(c), "\n",
                sep = "")
        }, error = function(c) {
            stop("\nunable to write R code to file: ",
                sQuote(file), "\n* ", conditionMessage(c), "\n",
                sep = "")
        })
    }


    writeRcode2file(tmp.R.script.code, tmp.R.script)


    msg <- paste0("Created an example R script. This script will be run in ",
        "all possible ways that are compatible with ", sQuote("this.path"),
        " that are currently available."
    )
    cat("\n", paste0(strwrap(msg, exdent = 2), "\n", collapse = ""), sep = "")
    if (.interactive) {
        msg <- paste0("Attempting to open the example R script. If the ",
            "script did not open automatically, the script's path is:")
        cat("\n", paste0(strwrap(msg, exdent = 2), "\n", collapse = ""),
            sQuote(tmp.R.script), "\n", sep = "")
        tryCatch({
            this.path:::file.open(tmp.R.script)
        }, warning = function(c) {
        }, error = function(c) {
        })
        pressEnter2Continue("")
    }


    write.results({
        cat("** session info\n\n")
        print(utils::sessionInfo())
        cat("\n** the above is information the package maintainer will likely ",
            "need if you report a bug\n", sep = "")
    })


    write.results({
        msg <- "Attempting to use 'this.path' when using 'source'"
        cat("\n", paste0(strwrap(msg, exdent = 2), "\n", collapse = ""),
            sep = "")
    })


    tryCatch({
        cat("\n* first, using ", sQuote("source"), "\n", sep = "")
        source(tmp.R.script, local = TRUE)
        msg <- paste0("You should see that 'this.path' correctly determined ",
            "the executing script's filename when using 'source'. If you ",
            "believe 'this.path' did not correctly determine the executing ",
            "script's filename, please send a bug report to the package ",
            "maintainer.")
        cat("\n", paste0(strwrap(msg, exdent = 2), "\n", collapse = ""),
            sep = "")
        pressEnter2Continue("")
    }, warning = function(c) {
        cat("\nunexpected error when attempting to source file: ",
            tmp.R.script, "\n* ", conditionMessage(c),
            "\n", sep = "")
    }, error = function(c) {
        cat("\nunexpected error when attempting to source file: ",
            tmp.R.script, "\n* ", conditionMessage(c),
            "\n", sep = "")
    })


    write.results({
        msg <- "Attempting to use 'this.path' when using 'sys.source'"
        cat("\n", paste0(strwrap(msg, exdent = 2), "\n", collapse = ""),
            sep = "")
    })


    tryCatch({
        cat("\n* second, using ", sQuote("sys.source"), "\n", sep = "")
        sys.source(tmp.R.script, envir = environment())
        msg <- paste0("You should see that 'this.path' correctly determined ",
            "the executing script's filename when using 'sys.source'. If you ",
            "believe 'this.path' did not correctly determine the executing ",
            "script's filename, please send a bug report to the package",
            "maintainer.")
        cat("\n", paste0(strwrap(msg, exdent = 2), "\n", collapse = ""),
            sep = "")
        pressEnter2Continue("")
    }, warning = function(c) {
        cat("\nunexpected error when attempting to source file: ",
            tmp.R.script, "\n* ", conditionMessage(c),
            "\n", sep = "")
    }, error = function(c) {
        cat("\nunexpected error when attempting to source file: ",
            tmp.R.script, "\n* ", conditionMessage(c),
            "\n", sep = "")
    })


    if (.Platform$GUI == "RStudio") {
        write.results({
            cat("\nAttempting to use 'this.path' when using 'debugSource'\n")
        })


        tryCatch({
            dbs <- get("debugSource", mode = "function", "tools:rstudio",
                inherits = FALSE)
            cat("\n* third, using ", sQuote("debugSource"),
                " from ", sQuote("RStudio"), "\n", sep = "")
            dbs(tmp.R.script, local = TRUE)
            msg <- paste0("You should see that 'this.path' correctly ",
                "determined the executing script's filename when using ",
                "'debugSource'. If you believe 'this.path' did not correctly ",
                "determine the executing script's filename, please send a bug ",
                "report to the package maintainer.")
            cat("\n", paste0(strwrap(msg, exdent = 2), "\n", collapse = ""),
                sep = "")
            pressEnter2Continue("")
        }, warning = function(c) {
            cat("\nunexpected error when attempting to source file: ",
                tmp.R.script, "\n* ", conditionMessage(c),
                "\n", sep = "")
        }, error = function(c) {
            cat("\nunexpected error when attempting to source file: ",
                tmp.R.script, "\n* ", conditionMessage(c),
                "\n", sep = "")
        })
    }
    else write.results({
        msg <- paste0("Unfortunately, it is impossible to demonstrate the ",
            "functionality of 'this.path' when using 'debugSource' because ",
            "'RStudio' is not presently running.")
        cat("\n", paste0(strwrap(msg, exdent = 2), "\n", collapse = ""),
            sep = "")
        pressEnter2Continue("")
    })


    cmt <- if (.Platform$OS.type == "windows")
        "command-line"
    else "terminal"
    write.results({
        msg <- paste0("Attempting to use 'this.path' when running from the ",
            cmt)
        cat("\n", paste0(strwrap(msg, exdent = 2), "\n", collapse = ""),
            sep = "")
    })


    owd <- getwd()
    if (!is.null(owd))
        on.exit(setwd(owd), add = TRUE)
    setwd(dirname(tmp.R.script))
    command <- sprintf("Rterm --no-echo --no-restore --file=\%s",
        encodeString(basename(tmp.R.script), quote = "\""))
    tryCatch({
        cat("\n* last, running from the ", cmt, "\n", sep = "")
        cat("\nProcess finished with exit code ",
            system(command), "\n", sep = "")
        msg <- paste0("You should see that 'this.path' correctly determined ",
            "the executing script's filename when running from the ", cmt, ". ",
            "If you believe 'this.path' did not correctly determine the ",
            "executing script's filename, please send a bug report to the ",
            "package maintainer.")
        cat("\n", paste0(strwrap(msg, exdent = 2), "\n", collapse = ""),
            sep = "")
        pressEnter2Continue()
    }, warning = function(c) {
        cat("\nunexpected error when attempting to run file: ",
            tmp.R.script, "\nfrom the command-line\n* ",
            conditionMessage(c), "\n", sep = "")
    }, error = function(c) {
        cat("\nunexpected error when attempting to run file: ",
            tmp.R.script, "\nfrom the command-line\n* ",
            conditionMessage(c), "\n", sep = "")
    })
    if (!is.null(owd))
        setwd(owd)


    write.results({
        msg <- paste0("Unfortunately, it is impossible to use ",
            "'example(this.path)' to demonstrate the functionality of ",
            "'this.path' in 'RStudio' and 'RGui'. If you would like to see ",
            "this functionality, you could try this:")
        cat("\n", paste0(strwrap(msg, exdent = 2), "\n", collapse = ""),
            "* make a new R script containing just this one command:\n",
            "    this.path::this.path(verbose = TRUE)\n",
            "* open this script in 'RStudio' or 'RGui'\n",
            "* run that command directly from the script\n",
            "    (both should print \"Source: active document ...\" along ",
                "with the script's path)\n",
            "* copy and paste that command into the R Console and run that ",
                "command again\n",
            "    (both should print \"Source: source document ...\" along ",
                "with the script's path)\n",
            "* try closing all your documents and run that same command in ",
                "the R Console\n",
            "    (both should raise an error \"R is being run ... with no ",
                "documents open\")\n",
            sep = "")
    })


    if (.interactive) {
        tryCatch({
            this.path:::file.open(results.file)
        }, warning = function(c) {
            cat("\n")
            cat(readLines(results.file), sep = "\n")
        }, error = function(c) {
            cat("\n")
            cat(readLines(results.file), sep = "\n")
        })
    }
    else if (!is.null(results.file)) {
        cat("\n")
        cat(readLines(results.file), sep = "\n")
    }
    invisible()
})(), warning = function(c) {
    cat(conditionMessage(c))
}, error = function(c) {
    cat(conditionMessage(c))
})
}
