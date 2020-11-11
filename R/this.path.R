

unix.command.line.argument.file.containing.space.fix <- function (path)
{
    path <- normalizePath(path, mustWork = FALSE)


    # since the bug only happens on unix, do not fix if not on unix
    # even if you're on unix, if the file exists, it is assumed to be the correct one
    if (.Platform$OS.type != "unix" || file.exists(path))
        return(path)


    # if we passed the condition above
    # * we MUST be running on unix
    # * the filename 'path' does NOT exist
    # one more simple case we can test is replacing all "~+~" with " "
    # if that exists, it is assumed to be correct
    if (file.exists(x <- gsub("~+~", " ", path, fixed = TRUE)))
        return(x)


    # if we passed to condition above, it means
    # * we MUST be running on unix
    # * original filename 'path' does not exist
    # * filename 'path' with all "~+~" replaced with " " does not exist
    # this must mean that some combination of "~+~" need to be replaced, but not all and not none
    # even still, we check that 'path' contains "~+~" regardless of the above conditions
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
        # some of these combitions may be invalid due to overlapping matches,
        #     but those will be handled later


        tmp <- c(TRUE, FALSE)
        tmp <- .mapply(rep, list(each = length(tmp)^(seq_along(m) - 1)), list(x = tmp))
        tmp <- do.call("cbind", tmp)
        tmp <- asplit(tmp, 1)


        # each element of 'tmp' should be a different combination of replacing "~+~"
        #
        # each element of 'tmp' should be equal in length to the length of 'm' (number of "~+~" found)
        # there should be '2^length(m)' elements in 'tmp'
        # there should be NO DUPLICATE ELEMENTS !!
        # verify that:
        if (length(tmp) == 2^length(m) &&
            !anyDuplicated(tmp) &&
            all(lengths(tmp) == length(m)))
            cat("correctly formed list of possible combinations!\n")
        else stop("malformed combinations list")


        tmp <- lapply(tmp, function(i) {
            if (sum(i) == 0L) {  # if we are replacing NONE of "~+~"
                value <- -1L
                attr(value, "match.length") <- -1L
            }
            else {
                value <- m[i]
                attr(value, "match.length") <- attr(m, "match.length")[i]
            }


            # if value contains overlapping patterns, it is invalid and should be removed
            if (any((value + attr(value, "match.length"))[-length(value)] > value[-1L])) {
                value <- NULL
            }
            else {
                attr(value, "index.type") <- "chars"
                attr(value, "useBytes") <- TRUE
            }
            return(value)
        })


        # remove NULL values
        tmp <- tmp[!vapply(X = tmp, FUN = "is.null", FUN.VALUE = NA)]


        # replace instances of "~+~" with " "
        path <- rep(path, length(tmp))
        regmatches(path, tmp) <- " "


        path <- path[file.exists(path)]  # select the filenames that exist
        if (length(path) == 0L) {
            stop("unable to resolve unix command-line \"--file=\" argument conflict\n",
                "* when running an R script that contains \" \" from the unix command-line,\n",
                "    R seems to open the R script and start executing the code (as it should),\n",
                "    but it seems to record the R script's filename incorrectly\n",
                "* each \" \" is replaced by \"~+~\", so when you ask for the \"--file=\" command argument,\n",
                "    it returns something that is not what you want.\n",
                "* I have tried all possible combinations of replacing instances of \"~+~\" with \" \",\n",
                "    but have been unsuccessful.\n",
                "* None of the combinations of replacing \"~+~\" with \" \" were successful")
        }
        else if (length(path) > 1L) {
            stop("unable to resolve unix command-line \"--file=\" argument conflict\n",
                "* when running an R script that contains \" \" from the unix command-line,\n",
                "    R seems to open the R script and start executing the code (as it should),\n",
                "    but it seems to record the R script's filename incorrectly\n",
                "* each \" \" is replaced by \"~+~\", so when you ask for the \"--file=\" command argument,\n",
                "    it returns something that is not what you want.\n",
                "* I have tried all possible combinations of replacing instances of \"~+~\" with \" \",\n",
                "    but have been unsuccessful.\n",
                "* Multiple combinations of replacing \"~+~\" with \" \" were successful")
        }
    }
    return(normalizePath(path, mustWork = TRUE))
}


this.path <- function (verbose = getOption("verbose"))
{
    where <- function(x) if (verbose) cat(x, "\n", sep = "")


    # loop through functions that lead here from most recent to earliest looking
    #     for an appropriate source call (a call to function base::source or base::sys.source)
    # an appropriate source call is a source call in which
    #     argument 'file' has been evaluated (forced)
    # this means, for example, the following is an inappropriate source call:
    #     source(this.path())
    # the argument 'file' is stored as a promise
    #     containing the expression "this.path()"
    # when the value of 'file' is requested, it assigns the value
    #     returned by evaluating "this.path()" to variable 'file'
    # there are two functions on the calling stack at
    #     this point being 'source' and 'this.path'
    # clearly, you don't want to request the 'file' argument from that source
    #     call because the value of 'file' is under evaluation right now!
    # the trick is to ask if variable ('ofile' for base::source, 'exprs' for base::sys.source)
    #     exists in that function's environment. this is because that variable is
    #     created AFTER argument 'file' has been forced
    # if that variable does exist, then argument 'file' has been forced and the
    #     source call is deemed appropriate. For base::source, the filename we want
    #     is the variable 'ofile' from that function's evaluation environment. For
    #     base::sys.source, the filename we want is the variable 'file' from that
    #     function's evaluation environment.
    # if that variable does NOT exist, then argument 'file' hasn't been forced and
    #     the source call is deemed inappropriate. The 'for' loop moves to the next
    #     function up the stack (if available)


    for (n in seq.int(sys.nframe(), 1L)[-1L]) {
        if (identical(sys.function(n), base::source) &&
            exists("ofile", envir = sys.frame(n), inherits = FALSE)) {
            where("Source: call to function source")
            path <- get("ofile", envir = sys.frame(n), inherits = FALSE)
            if (!is.character(path))
                path <- summary.connection(path)[["description"]]
            return(normalizePath(path, mustWork = TRUE))
        }
        else if (identical(sys.function(n), base::sys.source) &&
            exists("exprs", envir = sys.frame(n), inherits = FALSE)) {
            where("Source: call to function sys.source")
            path <- get("file", envir = sys.frame(n), inherits = FALSE)
            return(normalizePath(path, mustWork = TRUE))
        }
    }


    # if the for loop is passed, no appropriate
    #     source call was found up the calling stack
    # next, check if the user is running R from the command-line
    #     on a windows OS, the GUI is RTerm
    #     on a unix    OS, the GUI is X11


    if (.Platform$OS.type == "windows" && .Platform$GUI == "RTerm" ||  # running R from the Windows command-line
        .Platform$OS.type == "unix" && .Platform$GUI == "X11") {  # running R from the Unix command-line


        # get all command-line arguments that start with "--file="
        # check if there are any meeting this criteria using 'length'


        if (length(path <- grep("^--file=", commandArgs(), value = TRUE)) == 1L) {
            where("Source: Command-line argument 'file'")
            path <- sub("^--file=", "", path)


            # R has a weird bug when running from the unix command-line,
            #     all " " in argument 'file' are replaced with "~+~"
            # the following is a work around and may not work in all instances
            # if you'd like to take a look at the details, try running:
            # this.path:::unix.command.line.argument.file.containing.space.fix


            return(unix.command.line.argument.file.containing.space.fix(path))
        }
        else if (length(path))
            stop("'this.path' used in an inappropriate fashion\n",
                "* no appropriate 'source' or 'sys.source' call was found up the calling stack\n",
                "* R is being run from the command-line and formal argument \"--file=\" matched by multiple actual arguments\n")
        else stop("'this.path' used in an inappropriate fashion\n",
            "* no appropriate 'source' or 'sys.source' call was found up the calling stack\n",
            "* R is being run from the command-line and argument \"--file=\" is missing\n")
    }
    else if (.Platform$GUI == "RStudio") {  # running R from RStudio


        # function ".rs.api.getActiveDocumentContext" from the environment "tools:rstudio"
        #     returns a list of information about the document where your cursor is located
        # the element 'path' is a string; the file name we want
        # if your cursor is located outside of an open document (such as in the R Console),
        #     the element 'path' is an empty string (a string with no characters)


        # function ".rs.api.getSourceEditorContext" from the environment "tools:rstudio"
        #     returns a list of information about the document open in the current tab
        # the element 'path' is a string; the file name we want
        # if no documents are open in RStudio, NULL is returned


        if (nzchar(path <- get(".rs.api.getActiveDocumentContext",
            mode = "function", "tools:rstudio", inherits = FALSE)()$path)) {
            where("Source: active file in RStudio")
            return(normalizePath(path, mustWork = TRUE))
        }
        else if (!is.null(path <- get(".rs.api.getSourceEditorContext",
            mode = "function", "tools:rstudio", inherits = FALSE)()$path)) {
            where("Source: source file in RStudio")
            return(normalizePath(path, mustWork = TRUE))
        }
        else stop("'this.path' used in an inappropriate fashion\n",
            "* no appropriate 'source' or 'sys.source' call was found up the calling stack\n",
            "* R is being run from RStudio with no documents open\n")
    }
    else if (.Platform$OS.type == "windows" && .Platform$GUI == "Rgui" ||  # running R from RGui on Windows
        .Platform$OS.type == "unix" && .Platform$GUI == "AQUA")  # running R from RGui on Unix
        stop("'this.path' used in an inappropriate fashion\n",
            "* no appropriate 'source' or 'sys.source' call was found up the calling stack\n",
            "* R is being run from RGui which requires a 'source' and 'sys.source' call on the calling stack\n")
    else stop("'this.path' used in an inappropriate fashion\n",
        "* no appropriate 'source' or 'sys.source' call was found up the calling stack\n",
        "* R is being run in an unrecognized manner\n")
}


this.dir <- function (verbose = getOption("verbose"))
dirname(this.path(verbose = verbose))
