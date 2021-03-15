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

\code{this.path()} returns the full path of the executing script.

\code{this.dir()} is a shorter way of writing \code{dirname(this.path())}.
It returns the full path of the directory where the executing script is located.

\code{this.path:::file.open} is an unexported function that opens the specified
file using the default application for that file's extension. It is experimental
and may not work, but appears to be reliable. It is used in section
\strong{Examples} of \code{\link{this.path}}.
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

\code{\link[utils]{Rscript}}
}
