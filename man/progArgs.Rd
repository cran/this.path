\name{progArgs}
\alias{asArgs}
\alias{progArgs}
\alias{withArgs}
\title{Providing Arguments to a Script}
\description{
  \code{withArgs()} allows you to \sourcelink an \R script while providing
  arguments. As opposed to running with \Rscriptlink, the code will be
  evaluated in the same session in an environment of your choosing.

  \code{progArgs()} is a generalized version of
  \codelink3{base}{commandArgs}{(trailingOnly = TRUE)}, allowing you to access
  the program's arguments whether it was \sourcenolink-ed or run from a shell.

  \code{asArgs()} coerces \R objects into a character vector, for use with
  command line applications and \code{withArgs()}.
}
\usage{
asArgs(...)
progArgs(ifnotfound = character())
withArgs(...)
}
\arguments{
  \item{\dots}{\R objects to turn into script arguments; typically
    \codelink2{base}{logical}, \codelink2{base}{numeric},
    \codelink2{base}{character}, \codelink2{base:Dates}{Date}, and
    \codelink2{base:DateTimeClasses}{POSIXt} vectors.

    for \code{withArgs()}, the first argument should be an (unevaluated) call
    to a \sourcenolink-like function.}

  \item{ifnotfound}{any \R object, e.g., \code{character(0)}, \code{NULL}, a
    call to \codelink3{base}{stop}{()}}
}
\details{
  \code{progArgs()} will return the arguments associated with the executing
  script, or \code{commandArgs(trailingOnly = TRUE)} when there is no executing
  script.

  \code{asArgs()} coerces objects into command-line arguments. \code{\dots} is
  first put into a list, and then each non-list element is converted to
  character. They are converted as follows:

  \describe{
    \item{Factors (class \string{factor})}{

      using \codelink3{base:character}{as.character.factor}{()}}

    \item{Date-Times (class \string{POSIXct} and \string{POSIXlt})}{

      using format
      \ifelse{text}{"\%Y-\%m-\%d \%H:\%M:\%OS6"}{\code{"\%Y-\%m-\%d \%H:\%M:\%OS6"}}
      (retains as much precision as possible)}

    \item{Numbers (class \string{numeric} and \string{complex})}{

      with 17 significant digits (retains as much precision as possible) and
      \string{.} as the decimal point character.}

    \item{Raw Bytes (class \string{raw})}{

      using
      \ifelse{html}{\code{\link[base]{sprintf}("0x\%02x", )}}{\code{sprintf("0x\%02x", )}}
      (for easy conversion back to raw with \codelink3{base:raw}{as.raw}{()} or
      \codelink3{base:vector}{as.vector}{(, "raw")})}
  }

  All others will be converted to character using
  \codelink3{base:character}{as.character}{()} and its methods.

  The arguments will then be unlisted, and all attributes will be removed.
  Arguments that are \code{NA_character_} after conversion will be converted to
  \string{NA} (since the command-line arguments also never have missing
  strings).
}
\value{
  for \code{asArgs()} and \code{progArgs()}, a character vector.

  for \code{withArgs()}, the result of evaluating the first argument.
}
\examples{
this.path::asArgs(NULL, c(TRUE, FALSE, NA), 1:5, pi, exp(6i),
    letters[1:5], as.raw(0:4), Sys.Date(), Sys.time(),
    list(list(list("lists are recursed"))))


FILE.R <- tempfile(fileext = ".R")
this.path:::.writeCode({
    this.path:::.withAutoprint({
        this.path::sys.path()
        this.path::progArgs()
    }, spaced = TRUE, verbose = FALSE, width.cutoff = 60L)
}, FILE.R)


## wrap your source call with a call to withArgs()
this.path::withArgs(
    source(FILE.R, local = TRUE, verbose = FALSE),
    letters[6:10], pi, exp(1)
)
this.path:::.Rscript(c("--default-packages=NULL", "--vanilla", FILE.R,
    this.path::asArgs(letters[16:20], pi, Sys.time())))
this.path:::.Rscript(c("--default-packages=NULL", "--vanilla",
    rbind("-e", readLines(FILE.R)[-2L]),
    this.path::asArgs(letters[16:20], pi, Sys.time())))


# ## with R >= 4.1.0, use the forward pipe operator '|>' to
# ## make calls to withArgs() more intuitive:
# source(FILE.R, local = TRUE, verbose = FALSE) |> this.path::withArgs(
#     letters[6:10], pi, exp(1))


## withArgs() also works with set.sys.path()
sourcelike <- function (file, envir = parent.frame())
{
    file <- set.sys.path(file)
    envir <- as.environment(envir)
    exprs <- parse(n = -1, file = file)
    for (i in seq_along(exprs)) eval(exprs[i], envir)
}
this.path::withArgs(sourcelike(FILE.R), letters[21:26])


unlink(FILE.R)
}
\keyword{internal}
