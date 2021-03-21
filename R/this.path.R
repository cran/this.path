file.encode <- function (file)
{
    if (!is.character(file))
        stop("a character vector argument expected")
    if (.Platform$OS.type == "windows")
        paste0("\"", file, "\"")
    else encodeString(file, quote = "\"")
}


file.open <- function (file)
{
    if (!is.character(file))
        stop("a character vector argument expected")
    if (!length(file))
        return(invisible(file))
    value <- file
    file <- file[[1L]]
    file <- tryCatch({
        normalizePath(file, mustWork = TRUE)
    }, error = function(c) {
        warning(c)
        NULL
    })
    if (is.null(file))
        return(invisible(value))
    if (.Platform$OS.type != "windows") {
        if (getRversion() >= "4.0.0")
            os <- utils::osVersion
        else os <- utils::sessionInfo()$running
        file <- file.encode(file)
        if (is.null(os) || !startsWith(os, "macOS "))
            system(sprintf("xdg-open %s", file))
        else system(sprintf("open %s", file))
    }
    else shell.exec(file)
    return(invisible(value))
}


unix.command.line.argument.file.containing.space.fix <- function (path)
{
    # on a computer running a Unix OS, you will experience the following bug
    # when running R scripts from the command-line:
    # all " " in argument 'file' will be replaced with "~+~".
    # To elaborate, the 'Rterm' executable does open the correct file:
    #
    # Rterm --file="/tmp/RtmpqapV6Q/temp R script 15f42409a2.R"
    #
    # will, in fact, open the file "/tmp/RtmpqapV6Q/temp R script 15f42409a2.R"
    # and start parsing and evaluating the code within that file as it should,
    # but the argument 'file' will be recorded incorrectly. If, in the script
    # "/tmp/RtmpqapV6Q/temp R script 15f42409a2.R", you had a piece of code that
    # looked like:
    #
    # print(commandArgs())
    #
    # you would see something like:
    #
    # [1] "Rterm"
    # [2] "--file=/tmp/RtmpqapV6Q/temp~+~R~+~script~+~15f42409a2.R"
    #
    # as you can see, the filename is recorded incorrectly. this function should
    # be able to solve this issue.


    # if the file exists it is assumed to be correct
    if (file.exists(path))
        return(normalizePath(path, mustWork = TRUE))


    # the filename 'path' does not exist
    # one more simple case we can test is replacing all "~+~" with " "
    # if that exists, it is assumed to be correct
    x <- gsub("~+~", " ", path, fixed = TRUE)
    if (file.exists(x))
        return(normalizePath(x, mustWork = TRUE))


    # if we passed to condition above, it means
    # * original filename 'path' does not exist
    # * filename 'path' with all "~+~" replaced with " " does not exist
    # this must mean that some combination of "~+~" need to be replaced, but not all and not none
    # even still, we check that 'path' contains "~+~" regardless of the above conditions


    opath <- path  # record the original 'path' argument
    if (grepl("~+~", path, fixed = TRUE)) {


        # find every instance of "~+~" in 'path' including nested cases ("~+~+~+~")
        first <- seq.int(1, nchar(path) - 2)
        m <- which(substring(path, first, first + 2L) == "~+~")
        attr(m, "match.length") <- rep(3L, length(m))
        attr(m, "index.type") <- "chars"
        attr(m, "useBytes") <- TRUE


        # the issue is that I don't know which of the "~+~" should be replaced with " "
        #     and which should NOT be replaced
        # the idea is that we attempt all possible combinations of replacing "~+~" with " "
        # if we had, for example, five instances of "~+~", then we have several combinations to try:
        # 0 replacements, 1  combination
        # 1 replacement , 5  combinations
        # 2 replacements, 10 combinations
        # 3 replacements, 10 combinations
        # 4 replacements, 5  combinations
        # 5 replacements, 1  combination
        # some of these combinations may be invalid due to overlapping matches,
        #     but those will be handled later


        tmp <- c(TRUE, FALSE)
        tmp <- .mapply(rep, list(each = length(tmp)^(seq_along(m) - 1)), list(x = tmp))
        tmp <- do.call("cbind", tmp)
        tmp <- asplit(tmp, MARGIN = 1L)


        # each element of 'tmp' should be a different combination of replacing "~+~"
        #
        # each element of 'tmp' should be equal in length to the length of 'm' (number of "~+~" found)
        # there should be '2^length(m)' elements in 'tmp'
        # there should be NO DUPLICATE ELEMENTS !!


        tmp <- lapply(tmp, function(i) {
            if (sum(i) == 0L) {  # if we are replacing NONE of "~+~"
                value <- -1L
                attr(value, "match.length") <- -1L
            }
            else {
                value <- m[i]
                attr(value, "match.length") <- attr(m,
                    "match.length")[i]
            }


            # if value contains overlapping patterns, it is invalid and should be removed
            if (any((value + attr(value, "match.length"))[-length(value)] >
                value[-1L]))
                value <- NULL
            else {
                attr(value, "index.type") <- "chars"
                attr(value, "useBytes") <- TRUE
            }
            return(value)
        })


        # remove NULL values
        tmp <- tmp[!vapply(tmp, "is.null", FUN.VALUE = NA)]


        # replace instances of "~+~" with " "
        path <- rep(path, length(tmp))
        regmatches(path, tmp) <- " "


        path <- path[file.exists(path)]  # select the filenames that exist
        if (length(path) == 0L) {
            stop("unable to resolve Unix command-line argument 'file' conflict for file\n",
                "  ", encodeString(opath, quote = "\""), "\n",
                "* when running an R script that contains \" \" from the Unix command-line,\n",
                "  the R script's path is recorded incorrectly\n",
                "* each \" \" in argument 'file' is replaced by \"~+~\"\n",
                "* all possible combinations of replacing instances of \"~+~\" with \" \"\n",
                "  were attempted, but no file was found.")
        }
        else if (length(path) > 1L) {
            stop("unable to resolve Unix command-line argument 'file' conflict for file\n",
                "  ", encodeString(opath, quote = "\""), "\n",
                "* when running an R script that contains \" \" from the Unix command-line,\n",
                "  the R script's path is recorded incorrectly\n",
                "* each \" \" in argument 'file' is replaced by \"~+~\"\n",
                "* all possible combinations of replacing instances of \"~+~\" with \" \"\n",
                "  were attempted, but multiple files were found.")
        }
    }
    return(normalizePath(path, mustWork = TRUE))
}


this.path <- function (verbose = getOption("verbose"))
{
    ## function to print the method in which the
    ## path of the executing script was determined
    where <- function(x) {
        if (verbose)
            cat("Source: ", x, "\n", sep = "")
    }


    ## functions to get or check for an object in the n-th frame
    existsn <- function(x) exists(x, envir = sys.frame(n), inherits = FALSE)
    getn <- function(x) get(x, envir = sys.frame(n), inherits = FALSE)


    ## function to save a path in the n-th frame
    assign.__file__ <- function(value = normalizePath(path, mustWork = TRUE)) {
        assign("__file__", value, envir = sys.frame(n))
    }


    # loop through functions that lead here from most recent to earliest looking
    # for an appropriate source call (a call to function base::source,
    # base::sys.source, debugSource, or testthat::source_file)
    #
    # An appropriate source call is a source call in which argument 'file'
    # ('fileName' in the case of debugSource, 'path' in the case of
    # testthat::source_file) has been evaluated (forced). For example, the
    # following is an inappropriate source call:
    #
    # source(this.path())
    #
    # The argument 'file' is stored as a promise containing the expression
    # this.path(). When the value of 'file' is requested, it assigns the value
    # returned by evaluating this.path() to variable 'file'
    #
    # There are two functions on the calling stack at this point being source
    # and this.path. Clearly, you don't want to request the 'file' argument from
    # that source call because the value of 'file' is under evaluation right
    # now! The trick is to ask if a variable exists in that function's
    # evaluation environment that is only created AFTER 'file' has been forced.
    # For base::source, we ask if variable 'ofile' exists. For base::sys.source
    # and testthat::source_file, we ask if variable 'exprs' exists. For
    # debugSource, we cannot use this trick. Refer to the documentation below.
    #
    # If that variable exists, then argument 'file' has been forced and the
    # source call is deemed appropriate. If that variable does not exist, then
    # argument 'file' has not been forced and the source call is deemed
    # inappropriate. The loop continues to the next iteration (if available)


    ## as of this.path_0.2.0, compatibility with 'debugSource' from the
    ## 'RStudio' environment was added. 'debugSource' presents challenges that
    ## other source functions do not. For example, it is impossible to test if
    ## argument 'fileName' has been forced since all of the work is done
    ## internally in C. This is why we use a 'tryCatch' statement instead of an
    ## 'existsn'
    dbs <- if (.Platform$GUI == "RStudio")
        get("debugSource", mode = "function", "tools:rstudio",
            inherits = FALSE)


    ## as of this.path_0.4.0, compatibility with 'source_file' from package
    ## 'testthat' was added. 'testthat::source_file' is almost identical to
    ## 'base::sys.source' except that, strangely enough, it does not have the
    ## issue when sourcing a file named 'clipboard' ('base::sys.source' does
    ## have this issue where it tries to source the clipboard instead of the
    ## file named 'clipboard'. You might ask "why are you even concerned about
    ## this case, who would name a file 'clipboard'?", and that's a valid
    ## question, and I don't really have an answer. I was just messing around
    ## when I found this, so I accounted for it, despite how little it will
    ## occur, because I wanted to).
    sf <- if (isNamespaceLoaded("testthat"))
        getExportedValue("testthat", "source_file")


    for (n in seq.int(sys.nframe(), 1L)[-1L]) {
        if (identical(sys.function(n), base::source)) {


            ## if the argument 'file' to 'base::source' has not been forced,
            ## continue to the next iteration
            if (!existsn("ofile"))
                next


            ## if the path has yet to be saved
            if (!existsn("__file__")) {


                ## retrieve the unmodified 'file' argument
                path <- getn("ofile")


                ## as of this.path_0.3.0, compatibility was added when using
                ## 'base::source' with argument 'chdir' set to TRUE.
                ## this changes the working directory to the directory of the
                ## 'file' argument. since the 'file' argument was relative to
                ## the working directory prior to being changed, we need to
                ## change it to the previous working directory before we can
                ## open a connection (if necessary) and normalize the path
                if (existsn("owd")) {
                  cwd <- getwd()
                  on.exit(setwd(cwd))
                  setwd(getn("owd"))
                }


                ## there are two options for 'file'
                ## * connection
                ## * character string
                ## start with character string
                if (is.character(path)) {


                  ## use of "" refers to the R-level 'standard input' stdin.
                  ## this means 'base::source' did not open a file, so we assign
                  ## __file__ the value of NULL and continue to the next
                  ## iteration. We use __file__ as NULL to skip this source call
                  ## the next time this.path leads here
                  if (path == "") {
                    assign.__file__(NULL)
                    next
                  }


                  ## as of this.path_0.4.3, the path is determined slightly
                  ## differently. this change is to account for two possible
                  ## scenarios
                  ## * source("file://absolute or relative path")
                  ## * source("file:///absolute path")
                  ## the description of this connection should remove the
                  ## leading characters
                  con <- file(path, "r")
                  on.exit(close(con), add = TRUE)
                  path <- summary.connection(con)$description
                  if (existsn("owd"))
                    on.exit(setwd(cwd))
                  else on.exit()
                  close(con)
                }
                else path <- summary.connection(path)$description


                ## use of "clipboard" and "stdin" refer to the clipboard or to
                ## the C-level 'standard input' of the process, respectively.
                ## this means 'base::source' did not open a file, so we assign
                ## __file__ the value of NULL and continue to the next
                ## iteration. We use __file__ as NULL to skip this source call
                ## the next time this.path leads here
                if (path %in% c("clipboard", "stdin")) {
                  assign.__file__(NULL)
                  next
                }


                ## assign __file__ as the absolute path
                assign.__file__()
            }
            else if (is.null(getn("__file__")))
                next
            where("call to function source")
            return(getn("__file__"))
        }
        else if (identical(sys.function(n), base::sys.source)) {


            ## as with 'base::source', we check that argument 'file' has been
            ## forced, and continue to the next iteration if not
            if (!existsn("exprs"))
                next


            ## much the same as 'base::source' except simpler, we don't have to
            ## account for argument 'file' being a connection or ""
            if (!existsn("__file__")) {
                path <- getn("file")
                if (existsn("owd")) {
                  cwd <- getwd()
                  on.exit(setwd(cwd))
                  setwd(getn("owd"))
                }


                ## unlike 'base::source', 'base::sys.source' is intended to
                ## source a file (not a connection), so we have to throw an
                ## error if the user attempts to source a file named "clipboard"
                ## or "stdin" since both of these DO NOT refer to files
                if (path %in% c("clipboard", "stdin"))
                  stop(errorCondition("invalid 'file' argument, must not be \"clipboard\" nor \"stdin\"",
                    call = sys.call(n)))
                assign.__file__()
            }
            where("call to function sys.source")
            return(getn("__file__"))
        }
        else if (identical(sys.function(n), dbs)) {


            ## unlike 'base::source' and 'base::sys.source', there is no way to
            ## check that argument 'fileName' has been forced, since all of the
            ## work is done internally in C. Instead, we have to use a
            ## 'tryCatch' statement. If argument 'fileName' has been forced, the
            ## statement will proceed without an issue. If it has not, it is
            ## because argument 'fileName' depends on itself recursively with
            ## message "promise already under evaluation: recursive default
            ## argument reference or earlier problems?". If you'd like to see,
            ## try this:
            ##
            ## test <- function() get("fileName", sys.frame(1), inherits = FALSE)
            ## debugSource(test())
            ##
            ## and you should see the error in which I'm referring. So the trick
            ## is to use the 'tryCatch' statement to request argument
            ## 'fileName', return TRUE if the statement proceeded without error
            ## and FALSE if the statement produced an error.
            cond <- tryCatch({
                path <- getn("fileName")
                TRUE
            }, error = function(c) FALSE)
            if (!cond)
                next
            if (!existsn("__file__")) {


                ## we have to use 'enc2utf8' because 'debugSource' does as well
                path <- enc2utf8(path)
                if (path == "") {
                  assign.__file__(NULL)
                  next
                }
                con <- file(path, "r")
                on.exit(close(con))
                path <- summary.connection(con)$description
                on.exit()
                close(con)
                if (path %in% c("clipboard", "stdin")) {
                  assign.__file__(NULL)
                  next
                }
                assign.__file__()
            }
            else if (is.null(getn("__file__")))
                next
            where("call to function debugSource in RStudio")
            return(getn("__file__"))
        }
        else if (identical(sys.function(n), sf)) {


            ## as with 'base::source' and 'base::sys.source', we check that
            ## argument 'path' has been forced, and continue to the next
            ## iteration if not
            if (!existsn("exprs"))
                next
            if (!existsn("__file__")) {
                path <- getn("path")
                if (existsn("old_dir")) {
                  cwd <- getwd()
                  on.exit(setwd(cwd))
                  setwd(getn("old_dir"))
                }


                ## like 'base::sys.source', 'testthat::source_file' is intended
                ## to source a file (not a connection), so we have to throw an
                ## error if the user attempts to source a file named "stdin"
                ## since this DOES NOT refer to a file. However, we do not need
                ## to check for "clipboard" since 'testthat::source_file'
                ## handles that case correctly
                if (path == "stdin")
                  stop(errorCondition("invalid 'path' argument, must not be \"stdin\"",
                    call = sys.call(n)))
                assign.__file__()
            }
            where("call to function source_file in package testthat")
            return(getn("__file__"))
        }
    }


    # if the for loop is passed, no appropriate
    #     source call was found up the calling stack
    # next, check if the user is running R from the command-line
    #     on a Windows OS, the GUI is "RTerm"
    #     on a Unix    OS, the GUI is "X11"


    if (.Platform$OS.type == "windows" && .Platform$GUI == "RTerm" ||  # running from Windows command-line
        .Platform$OS.type == "unix" && .Platform$GUI == "X11") {       # running from Unix command-line


        if (is.null(`__file__`)) {


            # when running R from the command-line, there are a few things to
            # keep in mind when trying to select the correct 'file' to return.
            # First, there are a few command-line arguments where the name and
            # value are separate. This means that the name of the argument is
            # the n-th argument and the value of the argument is the n+1-th
            # argument. Second, the --args command-line flag means that all
            # arguments after are for the R script to use while the arguments
            # before are for the 'Rterm' executable. Finally, to take input from
            # a file, the two accepted methods are -f file and --file=file where
            # the input is taken from 'file'. If multiple of these arguments are
            # supplied, (it seems as though) 'Rterm' takes input from the last
            # 'file' argument.


            ca <- commandArgs()


            # select the command-line arguments intended for the Rterm
            # executable. also remove the first argument, this is the name of
            # the executable by which this R process was invoked (not useful
            # here)
            ca <- ca[seq_len(length(ca) - length(commandArgs(trailingOnly = TRUE)))]
            ca <- ca[-1L]


            # remove the --encoding=enc and --encoding enc command-line flags
            # these flags have highest priority, they are always handled first,
            # regardless of the preceding argument.
            #
            # Example:
            # Rterm -f --encoding=UTF-8 file
            #
            # you might think the value for -f would be --encoding=UTF-8 but
            # it's actually file because --encoding=enc and --encoding enc are
            # processed first
            which.rm <- which(ca == "--encoding")
            which.rm <- c(which.rm, which.rm + 1L)
            ca <- ca[setdiff(seq_along(ca), which.rm)]


            # there are 17 more arguments that are evaluated before -f is
            # grouped with its corresponding value. Some of them are static (the
            # first fifteen) while the others are variable (same beginning,
            # different values after)
            #
            # Example:
            # Rterm -f --silent --max-ppsize=100 file
            #
            # you might think the value of -f would be --silent but it's
            # actually file because --silent and --max-ppsize=N are processed
            # before -f is processed
            which.rm <- which(ca %in% c("--save", "--no-save",
                "--no-environ", "--no-site-file",
                "--no-init-file", "--restore", "--no-restore-data",
                "--no-restore-history", "--no-restore",
                "--vanilla", "-q", "--quiet",
                "--silent", "--no-echo", "--verbose") |
                grepl("^--encoding=|^--max-ppsize=", ca))
            ca <- ca[setdiff(seq_along(ca), which.rm)]


            # next, we group the command-line arguments where the name and value
            # are separate. there are two left being -f file and -e expression.
            # find the locations of these special arguments
            special <- ca %in% c("-f", "-e")


            # next, we figure out which of these special arguments are ACTUALLY
            # special.
            #
            # Example:
            # Rterm -f -f -f -e
            #
            # here, the first and third -f are special
            # while the second -f and first -e are not
            for (n in seq_len(max(length(special) - 1L, 0L))) {
                if (special[[n]])
                  special[[n + 1L]] <- FALSE
            }
            which.special <- which(special)


            # with the locations of the special arguments,
            #     figure out which of those are -f file arguments
            which.f <- which.special[ca[which.special] == "-f"]


            # use the locations of the special arguments to
            #     find the non-special argument locations
            which.non.special <- setdiff(seq_along(ca), c(which.special,
                which.special + 1L))


            # in these non-special command-line arguments,
            #     figure out which arguments start with --file=
            which.file <- which.non.special[grep("^--file=",
                ca[which.non.special])]


            # given the number of -f file and --file=file arguments,
            #     signal a possible error or warning
            n.file <- length(which.f) + length(which.file)
            if (!n.file)
                stop("'this.path' used in an inappropriate fashion\n",
                  "* no appropriate source call was found up the calling stack\n",
                  "* R is being run from the command-line and argument 'file' is missing")
            if (n.file > 1L)
                warning("command-line formal argument 'file' matched by multiple actual arguments")


            # since 'Rterm' uses the last -f file or --file=file argument,
            #     use max to find the index of the last one of these arguments
            n <- max(which.f, which.file)
            if (n %in% which.file)
                path <- sub("^--file=", "", ca[[n]])
            else path <- ca[[n + 1L]]


            # R has a weird bug when running from the Unix command-line,
            #     all " " in argument 'file' are replaced with "~+~"
            # the following is a work around and may not work in all instances
            # if you'd like to take a look at the details, try:
            # this.path:::unix.command.line.argument.file.containing.space.fix


            assignInMyNamespace("__file__", if (.Platform$OS.type == "windows")
                normalizePath(path, mustWork = TRUE)
            else unix.command.line.argument.file.containing.space.fix(path))
        }
        where("command-line argument 'file'")
        return(`__file__`)
    }
    else if (.Platform$GUI == "RStudio") {  # running R from 'RStudio'


        # function ".rs.api.getActiveDocumentContext" from the environment
        # "tools:rstudio" returns a list of information about the document where
        # your cursor is located
        #
        # function ".rs.api.getSourceEditorContext" from the environment
        # "tools:rstudio" returns a list of information about the document open
        # in the current tab
        #
        # element 'id' is a character string, an identification for the document
        # element 'path' is a character string, the path of the document


        adc <- get(".rs.api.getActiveDocumentContext", mode = "function",
            "tools:rstudio", inherits = FALSE)()
        if (adc$id != "#console") {
            path <- adc$path
            if (nzchar(path)) {
                where("active document in RStudio")
                return(normalizePath(path, mustWork = FALSE))
            }
            else stop("'this.path' used in an inappropriate fashion\n",
                "* no appropriate source call was found up the calling stack\n",
                "* active document in RStudio does not exist")
        }


        sec <- get(".rs.api.getSourceEditorContext", mode = "function",
            "tools:rstudio", inherits = FALSE)()
        if (!is.null(sec)) {
            path <- sec$path
            if (nzchar(path)) {
                where("source document in RStudio")
                return(normalizePath(path, mustWork = FALSE))
            }
            else stop("'this.path' used in an inappropriate fashion\n",
                "* no appropriate source call was found up the calling stack\n",
                "* source document in RStudio does not exist")
        }
        else stop("'this.path' used in an inappropriate fashion\n",
            "* no appropriate source call was found up the calling stack\n",
            "* R is being run from RStudio with no documents open")
    }
    else if (.Platform$OS.type == "windows" && .Platform$GUI == "Rgui") {  # running R from 'RGui' on Windows


        # on a Windows OS only, the function "getWindowsHandles" from the base
        # package "utils" returns a list of external pointers containing the windows
        # handles. The thing of interest are the names of this list, these should
        # be the names of the windows belonging to the current R process. Since
        # RGui can have files besides R scripts open (such as images), a regular
        # expression is used to subset only windows handles with names that exactly
        # match the string "R Console" or end with " - R Editor". I highly suggest
        # that you NEVER end a document's filename with " - R Editor". From there,
        # similar checks are done as in the above section for 'RStudio'


        wh <- names(utils::getWindowsHandles(pattern = "^R Console$| - R Editor$",
            minimized = TRUE))


        if (!length(wh))
            stop("no windows in RGui; should never happen, please report!")


        path <- wh[1L]
        if (path != "R Console") {
            path <- sub(" - R Editor$", "", path)
            if (path != "Untitled") {
                where("active document in RGui")
                return(normalizePath(path, mustWork = FALSE))
            }
            else stop("'this.path' used in an inappropriate fashion\n",
                "* no appropriate source call was found up the calling stack\n",
                "* active document in RGui does not exist")
        }


        path <- wh[2L]
        if (!is.na(path)) {
            path <- sub(" - R Editor$", "", path)
            if (path != "Untitled") {
                where("source document in RGui")
                return(normalizePath(path, mustWork = FALSE))
            }
            else stop("'this.path' used in an inappropriate fashion\n",
                "* no appropriate source call was found up the calling stack\n",
                "* source document in RGui does not exist")
        }
        else stop("'this.path' used in an inappropriate fashion\n",
            "* no appropriate source call was found up the calling stack\n",
            "* R is being run from RGui with no documents open")
    }
    else if (.Platform$OS.type == "unix" && .Platform$GUI == "AQUA") {  # running R from 'RGui' on Unix
        stop("'this.path' used in an inappropriate fashion\n",
            "* no appropriate source call was found up the calling stack\n",
            "* R is being run from AQUA which requires a source call on the calling stack")
    }
    else stop("'this.path' used in an inappropriate fashion\n",
        "* no appropriate source call was found up the calling stack\n",
        "* R is being run in an unrecognized manner")
}


this.dir <- function (verbose = getOption("verbose"))
dirname(this.path(verbose = verbose))


`__file__` <- NULL
