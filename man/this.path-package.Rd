\name{this.path-package}
\alias{this.path-package}
\docType{package}
\title{
\packageTitle{this.path}
}
\description{
\packageDescription{this.path}
}
\details{
There are only two exported functions from this package being \code{this.path}
and \code{this.dir}.

\code{\link{this.path}()} returns the full path of the executing script.

\code{\link{this.dir}()} is a shorter way of writing \code{dirname(this.path())}.
It returns the full path of the directory where the executing script is located.

There are two unexported functions from this package being \code{file.encode}
and \code{file.open}. Until they have been sufficiently tested, they will remain
unexported. The names and behaviour will not change, so they can be used if
desired (see \code{\link[base:ns-dblcolon]{pkg:::name}}), but they may not work
for strange characters.

\code{file.encode} formats a character vector such that it can be used at the
Windows command-line / / Unix terminal. It is used in section \strong{Examples}
of \code{\link{this.path}} and \code{\link{Running.R.from.the.command-line}}.

\code{file.open} opens the specified file using the default application for that
file's extension. It is used in section \strong{Examples} of
\code{\link{this.path}}.

If you are using either of these unexported functions in a package that is being
submitted to CRAN, a WARNING will be issued:

Unexported objects imported by ':::' calls:
  'this.path:::file.encode' 'this.path:::file.open'

Instead, put these two functions in your package and use them instead. This will
eliminate the WARNING.

\code{file.encode <- function (...)
get("file.encode", envir = asNamespace("this.path"),
    inherits = FALSE)(...)}

\code{file.open <- function (...)
get("file.open", envir = asNamespace("this.path"),
    inherits = FALSE)(...)}
}
\author{
\packageAuthor{this.path}

Maintainer: \packageMaintainer{this.path}
}
\keyword{ package }
\seealso{
\code{\link[this.path]{this.path}}

\code{\link[base]{source}}

\code{\link[base]{sys.source}}

\code{\link[testthat:source_file]{testthat::source_file}}

\code{\link{Running.R.from.the.command-line}}
}
