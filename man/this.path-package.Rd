\name{this.path-package}
\alias{this.path-package}
\docType{package}
\title{Get Script's Path}
\description{
  Determine the path of the executing script.

  Compatible with several popular GUIs:

  \itemize{
    \item{\sQuote{Rgui}}

    \item{\sQuote{\RStudio} (including
      \href{https://docs.posit.co/ide/user/ide/guide/tools/jobs.html}{background jobs})}

    \item{\sQuote{\Positron}}

    \item{\sQuote{\VSCode} + \sQuote{\REditorSupport}}

    \item{\sQuote{\Jupyter}}

    \item{\sQuote{\Emacs} + \sQuote{\ESS}}

    \item{\sQuote{\ifelse{html}{\Rscriptlink}{\ifelse{text}{Rscript}{\code{Rscript}}}}
      (shell)}
  }

  Compatible with several functions and packages: \syspathcompatiblelinks
}
\details{
  The most important functions from \pkg{package:this.path} are
  \code{\link{this.path}()}, \code{\link{this.dir}()}, \code{\link{here}()},
  and \code{\link{this.proj}()}:

  \itemize{
    \item{\code{\link{this.path}()} returns the normalized path of the script
      in which it is written.}

    \item{\code{\link{this.dir}()} returns the directory of
      \code{this.path()}.}

    \item{\code{\link{here}()} constructs file paths against
      \code{this.dir()}.}

    \item{\code{\link{this.proj}()} constructs file paths against the project
      root of \code{this.dir()}.}
  }

  New additions include:

  \itemize{
    \item{\code{\link{LINENO}()} returns the line number of the executing
      expression.}

    \item{\code{\link{shFILE}()} looks through the command line arguments,
      extracting \option{FILE} from either of the following: \option{-f}
      \option{FILE} or \option{--file=FILE}}

    \item{\code{\link{set.sys.path}()} implements \code{\link{this.path}()} for
      any \sourcelink-like functions outside of the builtins.}

    \item{\code{\link{with_init.file}()} allows \code{this.path()} and related
      to be used in a user profile.}
  }

  \pkg{package:this.path} also provides functions for constructing and
  manipulating file paths:

  \itemize{
    \item{\code{\link{path.join}()}, \code{\link{basename2}()}, and
      \code{\link{dirname2}()} are drop in replacements for
      \codelink3{base}{file.path}{()}, \codelink3{base}{basename}{()}, and
      \codelink3{base:basename}{dirname}{()} which better handle drives and
      network shares.}

    \item{\code{\link{splitext}()}, \code{\link{removeext}()},
      \code{\link{ext}()}, and \code{\link{ext<-}()} split a path into root and
      extension, remove a file extension, get an extension, or set an extension
      for a file path.}

    \item{\code{\link{path.split}()}, \code{\link{path.split.1}()}, and
      \code{\link{path.unsplit}()} split the path to a file into components.}

    \item{\code{\link{relpath}()}, \code{\link{rel2here}()}, and
      \code{\link{rel2proj}()} turn absolute paths into relative paths.}
  }
}
\note{
  This package started from a stack overflow posting:

  \Sexpr[results=rd,stage=install]{if (getRversion() >= "3.0.0") "\\\\href{https://stackoverflow.com/questions/1815606/determine-path-of-the-executing-script/64129649#64129649}{https://stackoverflow.com/questions/1815606/determine-path-of-the-executing-script/64129649#64129649}" else "\\\\ifelse{latex}{\\\\href{https://stackoverflow.com/questions/1815606/determine-path-of-the-executing-script/64129649}{https://stackoverflow.com/questions/1815606/determine-path-of-the-executing-script/64129649}}{\\\\href{https://stackoverflow.com/questions/1815606/determine-path-of-the-executing-script/64129649#64129649}{https://stackoverflow.com/questions/1815606/determine-path-of-the-executing-script/64129649#64129649}}"}

  If you like this package, please consider upvoting my answer so that more
  people will see it! If you have an issue with this package, please use
  \codelink3{utils:bug.report}{bug.report}{(package = "this.path")} to
  report your issue.
}
\author{
  Iris Simmons [aut, cre]

  Maintainer: Iris Simmons <ikwsimmo@gmail.com>
}
\keyword{package}
