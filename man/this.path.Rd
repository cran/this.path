\name{this.path}
\alias{this.path}
\alias{this.dir}
\title{Determine Executing Scripts Filename
}
\description{
this.path() returns the full path of the executing script. this.dir() is a shortcut
for dirname(this.path()), returning the full path of the directory where the executing
script is located.
}
\usage{
this.path(verbose = getOption("verbose"))
this.dir(verbose = getOption("verbose"))
}
\arguments{
  \item{verbose}{
TRUE or FALSE; controls whether the method of determining the path is printed
}
}
\details{
There are three ways in which R code is typically run; in 'RStudio' or 'RGui' by
running the current line or selection with the 'Run' button (or appropriate keyboard
shortcut), through a source call (a call to function base::source or base::sys.source
or debugSource ('RStudio' exclusive)), and finally from the command-line / / terminal.

To retrieve the executing script's filename, first an attempt is made to find a
source call. The calls are searched in reverse order so as to grab the most recent
source call in the case of nested source calls. If a source call was found, the
argument 'file' ('fileName' in the case of debugSource) is returned from the function's
evaluation environment (not the function's environment).

If no source call is found up the calling stack, then an attempt is made to figure
out how R is currently being used.

If R is being run from the command-line / / terminal, the command-line / / terminal
arguments are searched for one starting with "\verb{--}file=". It is an error for
no arguments (or multiple arguments) to match "\verb{--}file=" when using 'this.path'.
If one and only one argument is found that starts with "\verb{--}file=", the text
following "\verb{--}file=" is returned.

If R is being run from RStudio, the source document's filename (the document open
in the current tab) is returned (at the time of evaluation). It is important to not
leave the current tab (either by closing or switching tabs) while any calls to 'this.path'
have yet to be evaluated in the run selection. It is an error for no documents to
be open or for a document to not exist (not saved anywhere).

If R is being run from RGui, the source document's filename (the document most recently
interacted with besides the R Console) is returned (at the time of evaluation).
It is important to not leave the current document (either by closing the document
or interacting with another document) while any calls to 'this.path' have yet to
be evaluated in the run selection. It is an error for no documents to be open or
for a document to not exist (not saved anywhere).

If R is being run in another manner, it is an error to use 'this.path'.
}
\value{
A character vector of length 1; the executing script's filename.
}
\seealso{
\code{\link[base]{source}}

\code{\link[base]{sys.source}}

\code{\link[utils]{Rscript}}
}
\examples{
\dontrun{
To run the following example, try running this command:
example(topic = "this.path", package = "this.path")

The following will create two temporary directories and three temporary R scripts,
each containing calls to 'this.path'. You should see that it works through a source
call, nested source calls, and when running R from the command-line.

Unfortunately, it is impossible to use 'example(topic = "this.path", package = "this.path")'
to demonstrate the functionality of 'this.path' in 'RStudio' and 'RGui'. If you would
like to see this functionality, you could try this:
* make a new R script containing just this one command:
    this.path::this.path(verbose = TRUE)
* open this script in 'RStudio' or 'RGui'
* run that command directly from the script
    (both should print "Source: active document ..." along with the script's path)
* copy and paste that command into the R Console and run that command again
    (both should print "Source: source document ..." along with the script's path)
* try closing all your documents and run that same command in the R Console
    (both should raise an error "R is being run ... with no documents open")
}

tryCatch((function() {

    interactive <- interactive()
    if (interactive) {
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
        interactive <- tmp == "y"
    }
    if (interactive) {
        cat2 <- function(..., sep = " ", timeout = 0.025) {
            x <- list(...)
            x <- vapply(X = x, FUN = "paste0", collapse = sep, FUN.VALUE = "")
            x <- paste0(x, collapse = sep)
            for (char in strsplit(x, split = NULL)[[1L]]) {
                Sys.sleep(timeout)
                base::cat(char)
            }
        }
        pressEnter2Continue <- function(x = "\n") {
            readline("Hit <Return> to continue: ")
            cat(x)
        }
    }
    else {
        cat2 <- function(...) NULL
        pressEnter2Continue <- function(...) NULL
    }


    file.open <- function(file) {
        file <- as.character(file)[1L]
        if (is.na(file) || !nzchar(file))
            return(invisible(file))
        file <- normalizePath(file, mustWork = TRUE)
        if (.Platform$OS.type == "windows")
            shell.exec(file)
        else {
            if (getRversion() >= "4.0.0")
                os <- utils::osVersion
            else os <- utils::sessionInfo()$running
            if (is.null(os))
                system(sprintf("xdg-open \"\%s\"", file))
            else if (startsWith(os, "macOS "))
                system(sprintf("open \"\%s\"", file))
            else system(sprintf("xdg-open \"\%s\"", file))
        }
        invisible(file)
    }


    tmpdir <- tryCatch({
        tempdir(check = TRUE)
    }, warning = function(c) {
        cat("\nunable to create temporary directory using 'tempdir'\n* ",
            conditionMessage(c), "\n", sep = "")
        NULL
    }, error = function(c) {
        cat("\nunable to create temporary directory using 'tempdir'\n* ",
            conditionMessage(c), "\n", sep = "")
        NULL
    })
    if (is.null(tmpdir))
        return(invisible())


    tmp.main.dir <- tryCatch({
        tmp.main.dir <- tempfile(pattern = "project folder ",
            tmpdir = tmpdir)
        on.exit(unlink(tmp.main.dir, recursive = TRUE, force = TRUE))
        dir.create(tmp.main.dir, recursive = TRUE)
        cat2("\nCreating an example project folder named:\n")
        cat2("  ", encodeString(tmp.main.dir, quote = "\""), "\n",
            sep = "", timeout = 0)
        pressEnter2Continue()
        tmp.main.dir
    }, warning = function(c) {
        cat("\nunable to create temporary directory\n* ",
            conditionMessage(c), "\n", sep = "")
        NULL
    }, error = function(c) {
        cat("\nunable to create temporary directory\n* ",
            conditionMessage(c), "\n", sep = "")
        NULL
    })
    if (is.null(tmp.main.dir))
        return(invisible())


    tmp.dir.1 <- tryCatch({
        tmp.dir.1 <- normalizePath(file.path(tmp.main.dir, "code 1"),
            mustWork = FALSE)
        tmp.dir.2 <- normalizePath(file.path(tmp.main.dir, "code 2"),
            mustWork = FALSE)
        tmp.dir.3 <- normalizePath(file.path(tmp.main.dir, "input"),
            mustWork = FALSE)
        tmp.dir.4 <- normalizePath(file.path(tmp.main.dir, "output"),
            mustWork = FALSE)
        dir.create(tmp.dir.1, showWarnings = FALSE, recursive = TRUE)
        dir.create(tmp.dir.2, showWarnings = FALSE, recursive = TRUE)
        dir.create(tmp.dir.3, showWarnings = FALSE, recursive = TRUE)
        dir.create(tmp.dir.4, showWarnings = FALSE, recursive = TRUE)
        cat2("Creating four project sub-folders named:\n",
            "[1] ", encodeString(basename(tmp.dir.1), quote = "\""), "\n",
            "[2] ", encodeString(basename(tmp.dir.2), quote = "\""), "\n",
            "[3] ", encodeString(basename(tmp.dir.3), quote = "\""), "\n",
            "[4] ", encodeString(basename(tmp.dir.4), quote = "\""), "\n",
            sep = "")
        pressEnter2Continue()
        tmp.dir.1
    }, warning = function(c) {
        cat("\nunable to create temporary folders\n* ",
            conditionMessage(c), "\n", sep = "")
        NULL
    }, error = function(c) {
        cat("\nunable to create temporary folders\n* ",
            conditionMessage(c), "\n", sep = "")
        NULL
    })
    if (is.null(tmp.dir.1))
        return(invisible())


    tmp.R.script.1 <- tryCatch({
        tmp.R.script.1 <- normalizePath(file.path(tmp.dir.1,
            "R script 1.R"), mustWork = FALSE)
        tmp.R.script.2 <- normalizePath(file.path(tmp.dir.1,
            "R script 2.R"), mustWork = FALSE)
        tmp.R.script.3 <- normalizePath(file.path(tmp.dir.2,
            "R script 3.R"), mustWork = FALSE)
        cat2("Creating three R scripts named:\n",
            "[1] ", encodeString(basename(tmp.R.script.1), quote = "\""),
                " in ", encodeString(basename(dirname(tmp.R.script.1)),
                    quote = "\""), "\n",
            "[2] ", encodeString(basename(tmp.R.script.2), quote = "\""),
                " in ", encodeString(basename(dirname(tmp.R.script.2)),
                    quote = "\""), "\n",
            "[3] ", encodeString(basename(tmp.R.script.3), quote = "\""),
                " in ", encodeString(basename(dirname(tmp.R.script.3)),
                    quote = "\""), "\n",
            sep = "")
        pressEnter2Continue()
        tmp.R.script.1
    }, warning = function(c) {
        cat("\nunable to create temporary R scripts\n* ",
            conditionMessage(c), "\n", sep = "")
        NULL
    }, error = function(c) {
        cat("\nunable to create temporary R scripts\n* ",
            conditionMessage(c), "\n", sep = "")
        NULL
    })
    if (is.null(tmp.R.script.1))
        return(invisible())


    results.file <- tryCatch({
            .Sys.time <- format(Sys.time(), usetz = TRUE,
                format = "\%Y-\%m-\%d \%H-\%M-\%OS")
            tempfile(
                pattern = paste0("this.path example results ", .Sys.time, " "),
                tmpdir = tmpdir, fileext = ".txt")
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


    env <- list(
        `write.results sub` = write.results,
        `tmp.R.script.1 sub` = tmp.R.script.1,
        `tmp.R.script.2 sub` = tmp.R.script.2,
        `tmp.R.script.3 sub` = tmp.R.script.3,
        `results.file sub` = results.file
    )
    tmp.R.script.1.code <- substitute({
        results.file <- `results.file sub`
        write.results <- `write.results sub`


        write.results({
            cat("this.path status : ", x <- tryCatch({
                this.path::this.path(verbose = TRUE)
                "success"
            }, warning = function(c) {
                paste0("failure\n* ", conditionMessage(c))
            }, error = function(c) {
                paste0("failure\n* ", conditionMessage(c))
            }), "\n", sep = "")
        })


        if (x == "success") {
            cat("\nExecuting script's filename:\n")
            cat("  ", encodeString(`tmp.R.script.1 sub`, quote = "\""), "\n",
                sep = "")
            cat("\nExecuting script's filename (as determined by 'this.path'):",
                "\n", sep = "")
            cat("  ", encodeString(this.path::this.path(verbose = TRUE),
                quote = "\""), "\n", sep = "")
            source(file.path(this.path::this.dir(verbose = FALSE),
                "R script 2.R"))
        }
    }, env)


    tmp.R.script.2.code <- substitute({
        cat("\nExecuting script's filename:\n")
        cat("  ", encodeString(`tmp.R.script.2 sub`, quote = "\""), "\n",
            sep = "")
        cat("\nExecuting script's filename (as determined by 'this.path'):\n")
        cat("  ", encodeString(this.path::this.path(verbose = TRUE),
            quote = "\""), "\n", sep = "")
        source(file.path(dirname(this.path::this.dir(verbose = FALSE)),
            "code 2", "R script 3.R"))
    }, env)


    tmp.R.script.3.code <- substitute({
        cat("\nExecuting script's filename:\n")
        cat("  ", encodeString(`tmp.R.script.3 sub`, quote = "\""), "\n",
            sep = "")
        cat("\nExecuting script's filename (as determined by 'this.path'):\n")
        cat("  ", encodeString(this.path::this.path(verbose = TRUE),
            quote = "\""), "\n", sep = "")
    }, env)


    writeRcode2file <- function(x, file) {
        tryCatch({
            lines <- vapply(X = as.list(x[-1]), FUN = function(y) {
                paste0(deparse(y), collapse = "\n")
            }, FUN.VALUE = "")
            writeLines(lines, con = file)
            TRUE
        }, warning = function(c) {
            cat("\nunable to write R code to file: ",
                file, "\n* ", conditionMessage(c), "\n",
                sep = "")
            FALSE
        }, error = function(c) {
            cat("\nunable to write R code to file: ",
                file, "\n* ", conditionMessage(c), "\n",
                sep = "")
            FALSE
        })
    }


    if (!writeRcode2file(tmp.R.script.1.code, tmp.R.script.1))
        return(invisible())
    if (!writeRcode2file(tmp.R.script.2.code, tmp.R.script.2))
        return(invisible())
    if (!writeRcode2file(tmp.R.script.3.code, tmp.R.script.3))
        return(invisible())


    if (interactive) {
        cat2("Attempting to open the example project folder.\n",
            "  If the folder did not open, the folder's path is:\n", sep = "")
        cat("    ", encodeString(tmp.main.dir, quote = "\""), "\n", sep = "")
        tryCatch({
            file.open(tmp.main.dir)
        }, warning = function(c) {
        }, error = function(c) {
        })
        pressEnter2Continue("")
    }


    write.results({
        cat("*** session info ***\n\n")
        print(utils::sessionInfo())
        cat("\n*** end of session info ***\n")
        cat("The above is information the package maintainer will likely need ",
            "if you report a bug\n", sep = "")
    })


    write.results({
        cat("\nAttempting to use 'this.path' when using 'source'\n")
    })


    tryCatch({
        source(tmp.R.script.1, local = TRUE)
        cat2("\nYou should see that 'this.path' correctly determined each of t",
            "he executing script's\n  filenames when using 'source'. If you be",
            "lieve 'this.path' did not correctly determine\n  any of the execu",
            "ting script's filenames, please send a bug report to the package ",
            "maintainer.\n", sep = "", timeout = 0.015)
        pressEnter2Continue("")
    }, warning = function(c) {
        cat("\nunexpected error when attempting to source file: ",
            tmp.R.script.1, "\n* ", conditionMessage(c),
            "\n", sep = "")
    }, error = function(c) {
        cat("\nunexpected error when attempting to source file: ",
            tmp.R.script.1, "\n* ", conditionMessage(c),
            "\n", sep = "")
    })


    write.results({
        cat("\nAttempting to use 'this.path' when using 'sys.source'\n")
    })


    tryCatch({
        sys.source(tmp.R.script.1, envir = environment())
        cat2("\nYou should see that 'this.path' correctly determined each of t",
            "he executing script's\n  filenames when using 'sys.source'. If yo",
            "u believe 'this.path' did not correctly determine\n  any of the e",
            "xecuting script's filenames, please send a bug report to the pack",
            "age maintainer.\n", sep = "", timeout = 0.015)
        pressEnter2Continue("")
    }, warning = function(c) {
        cat("\nunexpected error when attempting to source file: ",
            tmp.R.script.1, "\n* ", conditionMessage(c),
            "\n", sep = "")
    }, error = function(c) {
        cat("\nunexpected error when attempting to source file: ",
            tmp.R.script.1, "\n* ", conditionMessage(c),
            "\n", sep = "")
    })


    if (.Platform$GUI == "RStudio") {
        write.results({
            cat("\nAttempting to use 'this.path' when using 'debugSource'\n")
        })


        tryCatch({
            dbs <- get("debugSource", mode = "function", "tools:rstudio",
                inherits = FALSE)
            dbs(tmp.R.script.1, local = TRUE)
            cat2("\nYou should see that 'this.path' correctly determined each ",
                "of the executing script's\n  filenames when using 'debugSourc",
                "e'. If you believe 'this.path' did not correctly determine\n ",
                " any of the executing script's filenames, please send a bug r",
                "eport to the package maintainer.\n", sep = "", timeout = 0.015)
            pressEnter2Continue("")
        }, warning = function(c) {
            cat("\nunexpected error when attempting to source file: ",
                tmp.R.script.1, "\n* ", conditionMessage(c),
                "\n", sep = "")
        }, error = function(c) {
            cat("\nunexpected error when attempting to source file: ",
                tmp.R.script.1, "\n* ", conditionMessage(c),
                "\n", sep = "")
        })
    }
    else write.results({
        cat("\nUnfortunately, it is impossible to demonstrate the functionalit",
            "y of 'this.path'\n  when using 'debugSource' because 'RStudio' is",
            " not presently running.\n", sep = "")
    })

    write.results({
        cat("\nAttempting to use 'this.path' when running from command-line\n")
    })


    command <- sprintf("Rscript \"\%s\"", tmp.R.script.1)
    tryCatch({
        cat("\nProcess finished with exit code ",
            system(command), "\n", sep = "")
        cat2("\nYou should see that 'this.path' correctly determined each of t",
            "he executing script's\n  filenames when running from the command-",
            "line / / terminal. If you believe 'this.path'\n  did not correctl",
            "y determine any of the executing script's filenames, please send",
            "\n  a bug report to the package maintainer.\n",
            sep = "", timeout = 0.015)
        pressEnter2Continue()
    }, warning = function(c) {
        cat("\nunexpected error when attempting to run file: ",
            tmp.R.script.1, "\nfrom the command-line\n* ",
            conditionMessage(c), "\n", sep = "")
    }, error = function(c) {
        cat("\nunexpected error when attempting to run file: ",
            tmp.R.script.1, "\nfrom the command-line\n* ",
            conditionMessage(c), "\n", sep = "")
    })


    write.results({
        cat("\nUnfortunately, it is impossible to use 'example(topic = \"this.",
            "path\", package = \"this.path\")'\n  to demonstrate the functiona",
            "lity of 'this.path' in 'RStudio' and 'RGui'. If you\n  would like",
            " to see this functionality, you could try this:\n* make a new R s",
            "cript containing just this one command:\n    this.path::this.path",
            "(verbose = TRUE)\n* open this script in 'RStudio' or 'RGui'\n* ru",
            "n that command directly from the script\n    (both should print ",
            "\"Source: active document ...\" along with the script's path)\n* ",
            "copy and paste that command into the R Console and run that comma",
            "nd again\n    (both should print \"Source: source document ...\" ",
            "along with the script's path)\n* try closing all your documents a",
            "nd run that same command in the R Console\n    (both should raise",
            " an error \"R is being run ... with no documents open\")\n",
            sep = "")
    })


    if (interactive) {
        tryCatch({
            file.open(results.file)
            cat("If the results file did not open automatically, you can find ",
                "the file here:\n", results.file, "\n", sep = "")
        }, warning = function(c) {
            cat("Ran into an issue while attempting to automatically open the ",
                "results file.\n  You can find the file here:\n",
                results.file, "\n", sep = "")
        }, error = function(c) {
            cat("Ran into an issue while attempting to automatically open the ",
                "results file.\n  You can find the file here:\n",
                results.file, "\n", sep = "")
        })
    }
    else if (!is.null(results.file)) {
        cat("\n")
        cat(readLines(results.file), sep = "\n")
    }
    invisible()
})(), warning = conditionMessage, error = conditionMessage)
}