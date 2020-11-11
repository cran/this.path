\name{this.path}
\alias{this.path}
\alias{this.dir}
\title{Determine Executing Scripts Filename
}
\description{
this.path returns the full path of the executing script. this.dir() is a shortcut
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
There are three ways in which R scripts are typically run; in RStudio by running
the current line or selection with the Run button (or appropriate keyboard shortcut),
through a source call (a call to function base::source or base::sys.source), and
finally from the command-line.

To retrieve the executing script's filename, first an attempt is made to find a source
call. The calls are searched in reverse order so as to grab the most recent source
call in the case of nested source calls. If a source call was found, the argument
'file' is returned from the function's evaluating environment (not the function's
environment).

If no source call is found up the calling stack, then an attempt is made to figure
out how R is currently being used.

If R is being run from the command-line, the command arguments are searched for
one starting with "\verb{--}file=". It is an error for no arguments (or multiple arguments)
to match "\verb{--}file=" when using 'this.path'. If one and only one argument is found
that starts with "\verb{--}file=", the text following "\verb{--}file=" is returned.

If R is being run from RStudio, the source document's filename (the document open
in the current tab) is returned (at the time of evaluation). It is important to not
leave the current tab (either by closing or switching tabs) while any calls to 'this.path'
have yet to be evaluated in the run selection. It is an error for no documents to
be open.

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
The following will create two temporary directories and three temporary R scripts,
each containing calls to 'this.path'. You should see that it works through a source
call, nested source calls, and when running R from the command-line.
It is impossible to use 'example("this.path", package = "this.path")' to demonstrate
the functionality of 'this.path' in RStudio because 'example' uses a source call
to execute the code in the example section of the documentation.
}

tryCatch((function() {


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
        tmp.main.dir <- tempfile(pattern = "temp folder ",
            tmpdir = tmpdir)
        on.exit(unlink(tmp.main.dir, recursive = TRUE, force = TRUE))
        dir.create(tmp.main.dir, recursive = TRUE)
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
        tmp.dir.1 <- normalizePath(file.path(tmp.main.dir, "folder 1"),
            mustWork = FALSE)
        tmp.dir.2 <- normalizePath(file.path(tmp.main.dir, "folder 2"),
            mustWork = FALSE)
        dir.create(tmp.dir.1, showWarnings = FALSE, recursive = TRUE)
        dir.create(tmp.dir.2, showWarnings = FALSE, recursive = TRUE)
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


    results.file <- if (!interactive())
        NULL
    else tryCatch({
        .Sys.time <- format(Sys.time(), usetz = TRUE,
            format = "\%Y-\%m-\%d \%H-\%M-\%OS")
        tempfile(
            pattern = paste0("this.path example results ", .Sys.time, " "),
            tmpdir = tmpdir, fileext = ".txt")
    }, warning = function(c) {
        cat("\nunable to create result file:\n* ",
            conditionMessage(c), "\n", sep = "")
        NULL
    }, error = function(c) {
        cat("\nunable to create result file:\n* ",
            conditionMessage(c), "\n", sep = "")
        NULL
    })


    tmp.R.script.1.code <- substitute({
        write.results <- function(expr) {
            if (!is.null(results.file)) {
                sink(file = results.file, append = TRUE)
                on.exit(sink())
            }
            expr
        }


        write.results({
            if ("with.GUI" \%in\% commandArgs(trailingOnly = TRUE))
                cat("\nGUI              : ", .Platform$GUI,
                    "\n", sep = "")
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
            print(tmp.R.script.1)
            cat("\nExecuting script's filename (as determined by 'this.path'):",
                "\n", sep = "")
            print(this.path::this.path(verbose = TRUE))
            # cat("\nAttempting to source R script:\n")
            # print(tmp.R.script.2)
            # cat("without knowing its exact path, only the file structure of",
            #     "this temporary folder.\n")
            source(file.path(this.path::this.dir(verbose = FALSE),
                "R script 2.R"))
        }
    })


    tmp.R.script.2.code <- substitute({
        cat("\nExecuting script's filename:\n")
        print(tmp.R.script.2)
        cat("\nExecuting script's filename (as determined by 'this.path'):\n")
        print(this.path::this.path(verbose = TRUE))
        # cat("\nAttempting to source R script:\n")
        # print(tmp.R.script.3)
        # cat("without knowing its exact path, only the file structure of this",
        #     "temporary folder.\n")
        source(file.path(dirname(this.path::this.dir(verbose = FALSE)),
            "folder 2", "R script 3.R"))
    })


    tmp.R.script.3.code <- substitute({
        cat("\nExecuting script's filename:\n")
        print(tmp.R.script.3)
        cat("\nExecuting script's filename (as determined by 'this.path'):\n")
        print(this.path::this.path(verbose = TRUE))
    })


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


    cat("\nAttempting to source R script:\n")
    print(tmp.R.script.1)


    write.results <- function(expr) {
        if (!is.null(results.file)) {
            sink(file = results.file, append = TRUE)
            on.exit(sink())
        }
        expr
    }


    write.results({
        if (isNamespaceLoaded("utils")) {
            cat("*** session info\n\n")
            print(utils::sessionInfo())
            cat("\n*** end of session info\n")
        }
        cat("\nOS type          : ", .Platform$OS.type,
            "\n", sep = "")
        cat("\nAttempting to use 'this.path' through a source call\n")
    })


    tryCatch({
        source(tmp.R.script.1, local = TRUE)
    }, warning = function(c) {
        cat("\nunexpected error when attempting to source file: ",
            tmp.R.script.1, "\n* ", conditionMessage(c),
            "\n", sep = "")
    }, error = function(c) {
        cat("\nunexpected error when attempting to source file: ",
            tmp.R.script.1, "\n* ", conditionMessage(c),
            "\n", sep = "")
    })


    Rscript.executable.path <- tryCatch({
        normalizePath(file.path(normalizePath(R.home("bin"),
            mustWork = TRUE), "Rscript"), mustWork = FALSE)
    }, warning = function(c) {
        cat("\nunable to demonstrate functionality of 'this.path' from the com",
            "mand-line:\n* ", conditionMessage(c), "\n",
            sep = "")
        NULL
    }, error = function(c) {
        cat("\nunable to demonstrate functionality of 'this.path' from the com",
            "mand-line:\n* ", conditionMessage(c), "\n",
            sep = "")
        NULL
    })


    if (!is.null(Rscript.executable.path)) {
        cat("\nAttemping to run R script:\n")
        print(tmp.R.script.1)
        cat("from the command-line.\n")


        command <- sprintf("\"\%s\" \"\%s\" with.GUI",
            Rscript.executable.path, tmp.R.script.1)
        cat("\nEvaluating the following system command:\n")
        cat(command, "\n", sep = "")
        tryCatch({
            cat("\nProcess finished with exit code ", system(command), "\n", sep = "")
        }, warning = function(c) {
            cat("\nunexpected error when attempting to run file: ",
                tmp.R.script.1, "\nfrom the command-line\n* ",
                conditionMessage(c), "\n", sep = "")
        }, error = function(c) {
            cat("\nunexpected error when attempting to run file: ",
                tmp.R.script.1, "\nfrom the command-line\n* ",
                conditionMessage(c), "\n", sep = "")
        })
    }


    cat("\nUnfortunately, it is impossible to use 'example(\"this.path\", pack",
        "age = \"this.path\")'\n  to demonstrate the functionality of 'this.pa",
        "th' in RStudio because 'example' uses\n  a source call to execute the",
        " code in the example section of the documentation.\n",
        sep = "")

    if (!is.null(results.file)) {
        cat("\n'this.path' example results are in the following file:\n")
        cat(results.file, "\n", sep = "")
    }
    invisible()
})(), warning = conditionMessage, error = conditionMessage)
}
