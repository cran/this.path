cat("working directory: ", if (is.null(getwd()))
    "NULL"
else sQuote(getwd()), "\n", sep = "")
cat("Executing script's path:\n")
cat(sQuote(this.path::this.path(verbose = TRUE)), "\n", sep = "")
## testthat::source_file(file.path(this.path::this.dir(), "test2.R"))
