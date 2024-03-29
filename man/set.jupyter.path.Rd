\name{set.jupyter.path}
\alias{set.jupyter.path}
\title{Declare Executing 'Jupyter' Notebook's Filename}
\description{
  \code{\link{this.path}()} does some guess work to determine the path of the
  executing notebook in \sQuote{\Jupyter}. This involves listing all the files
  in the initial working directory, filtering those which are \R notebooks,
  then filtering those with contents matching the top-level expression.

  This could possibly select the wrong file if the same top-level expression is
  found in another file. As such, you can use \code{set.jupyter.path()} to
  declare the executing \sQuote{Jupyter} notebook's filename.
}
\usage{
set.jupyter.path(...)
}
\arguments{
  \item{\dots}{further arguments passed to \code{\link{path.join}()}. If no
    arguments are provided or exactly one argument is provided that is
    \codelink2{base}{NA} or \codelink2{base}{NULL}, the \sQuote{Jupyter} path
    is unset.}
}
\details{
  This function may only be called from a top-level context in
  \sQuote{Jupyter}. It is recommended that you do \strong{NOT} provide an
  absolute path. Instead, provide just the basename and the directory will be
  determined by the initial working directory.
}
\value{
  character string, invisibly; the declared path for \sQuote{Jupyter}.
}
\examples{
# ## if you opened the file "~/file50b816a24ec1.ipynb", the initial
# ## working directory should be "~". You can write:
#
# set.jupyter.path("file50b816a24ec1.ipynb")
#
# ## and then this.path() will return "~/file50b816a24ec1.ipynb"
}
