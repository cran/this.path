\name{Running.R.from.the.command-line}
\alias{Running.R.from.the.command-line}
\alias{Running.R.from.the.command.line}
\alias{Running R from the command-line / / terminal}
\title{Running R from the command-line / / terminal
}
\description{
How to run \R from the Windows command-line / / Unix terminal.
}
\details{
When you download \R, you are given four executable files that can be used from
the command-line / / terminal. These are \sQuote{R}, \sQuote{Rcmd},
\sQuote{Rscript}, and \sQuote{Rterm}. For the purposes of running \R scripts,
only \sQuote{Rterm} is needed. Explanations for the other executables can be
found in sections \strong{Difference Between R Executables} and
\strong{Examples}.

Suppose you wanted to run an \R script with a filename \sQuote{input R script}
from the command-line / / terminal. You would write and execute one of the
following commands:

\command{Rterm} \option{-f "input R script"}

\command{Rterm} \option{--file="input R script"}

\pkg{this.path} is capable of recognizing either of these, so use whichever you
prefer.

There are plenty of options that \sQuote{Rterm} accepts, you can find all of
them by executing the following command:

\command{Rterm} \option{--help}

The few I use the most are:

\option{--quiet} do not print the \R startup message

\option{--no-echo} make \R run as quietly as possible (includes
\option{--quiet})

\option{--verbose} print information about progress (includes setting option
\dQuote{verbose} to \code{TRUE})

\option{--args} indicates that arguments following this argument are for the \R
script itself, which can be accessed in your script with the following command:
\code{commandArgs(trailingOnly = TRUE)}

If, when you execute one of the previous commands, you see the following error
message: \dQuote{\sQuote{Rterm} is not recognized as an internal or external
command, operable program or batch file.}, see section \strong{Ease of Use on
Windows}.
}
\section{Difference Between R Executables}{
For running \R scripts, I recommended the \sQuote{Rterm} executable. Here are
how the other executables can also be used to run \R scripts, and what makes
them different.

The \sQuote{R} executable can be used to call the \sQuote{Rcmd} or
\sQuote{Rterm} executable. It does not provide any additional functionality that
the \sQuote{Rcmd} and \sQuote{Rterm} executables do not already have, so it is
unnecessary to call this executable directly.

The \sQuote{Rcmd} executable is mainly used for preparing \R packages. The only
command that that isn't for \R packages specifically is \command{Rcmd}
\command{BATCH} \option{...} which is used for running \R scripts. However, this
command directly calls the \sQuote{Rterm} executable. The difference is that the
\sQuote{Rcmd} executable always produces an output file (containing
\code{\link{stdout}} and \code{\link{stderr}}) and always uses the options
\option{--restore} and \option{--save}. Depending on the setting, you may want
an output file and to use those two options, but since the choice is not given,
the \sQuote{Rterm} executable is preferable. Since the \sQuote{Rcmd} executable
directly calls the \sQuote{Rterm} executable, it means that the \sQuote{Rcmd}
executable does not provide any additional functionality.

The \sQuote{Rscript} executable directly calls the \sQuote{Rterm} executable.
The difference is that the \sQuote{Rscript} executable has different syntax for
providing the filename of the input script and always uses the options
\option{--no-echo} and \option{--no-restore}. Since the choice is not given, the
\sQuote{Rterm} executable is again preferable. The \sQuote{Rscript} executable
does not provide any additional functionality.

The \sQuote{Rterm} executable has a few advantages. The syntax is more
consistent for providing an input file. The syntax is better for providing the
filename in which to place output. No options are forced. And environment
variables can be provided in the command.
}
\section{Ease of Use on Windows}{
On a Unix-alike OS (including macOS), it is easy to invoke an \R session from
the terminal by simply typing the name of the \R executable file you wish to
run. On Windows, you should see that typing the name of the executable file you
wish to run does not run that application, but instead throws an error. Instead,
you will have to type the full path of the directory where your \R executable
files are located (see section \strong{Where are my R executable files
located?}), followed by the name of the \R executable file you wish to run.

This is not very convenient to type everytime something needs to be run from the
command-line, plus it has another issue of being computer dependent. The
solution is to add the path of the directory where your \R executable files are
located to the \env{Path} environment variable. The \env{Path} environment
variable is a list of directories where executable programs are located. When
you type the name of an executable program you wish to run, Windows looks for
that program through each directory in the \env{Path} environment variable. When
you add the full path of the directory where your \R executable files are
located to your \env{Path} environment variable, you should be able to run any
of those executable programs by their basenames (\sQuote{R}, \sQuote{Rcmd},
\sQuote{Rscript}, and \sQuote{Rterm}) instead of their full paths.

To add a new path to your \env{Path} environment variable, first open the
Control Panel. You should be able to do this by pressing \strong{Windows},
\strong{Windows}+\strong{R}, or opening a file explorer window and then typing
\strong{Control Panel} in the prompt. From there, open the category
\strong{System and Security}, then open the category \strong{System}, then open
\strong{Advanced system settings} (should appear near the top left), then open
\strong{Environment Variables...} (should appear near the bottom right). You
should see a new window split in two sections, environment variables for the
current user and environment variables for the system. If all users of this
machine are using the same \R executable files, you can add the path to the
system environment variables (this is what I did), otherwise you can add it to
your user environment variables. Click the variable \env{Path}, then click
\strong{Edit...}, then click \strong{New}, then type (or paste) the full path of
the directory where your \R executable files are located.

To check that this worked correctly, open the command-line and execute the
following commands:

\command{Rterm} \option{--help}

\command{Rterm} \option{--version}

You should see that the first prints the usage message for the \sQuote{Rterm}
executable file while the second prints information about the version of \R
currently being run. Make sure this is the version of \R you wish to run.
}
\section{Where are my R executable files located?}{
In an \R session, you can find the location of your \R executable files with the
following command:

\code{cat(sQuote(normalizePath(R.home(component = "bin"))))}

For me, this is:

\file{C:\\Program Files\\R\\R-4.0.2\\bin\\x64}
}
\examples{
tryCatch({


    ## the following shows that 'Rscript' is just a wrapper for 'Rterm' with a few
    ## extra options. This makes 'Rscript' less desirable in my opinion.


    cat("\n* first, from 'Rterm'\n")
    invisible(system("Rterm   -e \"commandArgs()\"")
    )  # "Rterm" "-e" "commandArgs()"


    cat("\n* next, from 'Rscript'\n\n")
    invisible(system("Rscript -e \"commandArgs()\"")
    )  # "Rterm" "--no-echo" "--no-restore" "-e" "commandArgs()"
}, condition = function(c) cat(conditionMessage(c)))


tryCatch((function() {
    on.exit(suppressWarnings(file.remove(tmp.R.script, outfile)))


    ## the following shows that 'Rcmd BATCH' is just a wrapper for 'Rterm' with a
    ## few extra options. It also show that 'Rcmd BATCH' always creates an output
    ## file, making 'Rcmd BATCH' less desirable in my opinion.


    tmp.R.script <- tempfile(fileext = ".R")
    writeLines("commandArgs()", tmp.R.script)
    invisible(system(sprintf("Rcmd BATCH \%s",
        this.path:::file.encode(tmp.R.script))))
    outfile <- paste0(sub("\\\\.R$", "", tmp.R.script), ".Rout")
    outfile.lines <- readLines(outfile)
    outfile.lines <- outfile.lines[-1:-which(outfile.lines == "> commandArgs()")[[1L]]]
    outfile.lines <- outfile.lines[-which(outfile.lines == "> "):-length(outfile.lines)]


    cat("\n* first, from 'Rterm'\n")
    invisible(system(sprintf("Rterm -f \%s",
        this.path:::file.encode(tmp.R.script))))


    cat("\n* next, from 'Rcmd BATCH'\n\n")
    cat(outfile.lines, sep = "\n")
})(), condition = function(c) cat(conditionMessage(c)))
}
