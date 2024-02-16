local({
    FILE.R <- tempfile(fileext = ".R")
    on.exit(unlink(FILE.R), add = TRUE)
    this.path:::.writeCode(file = FILE.R, {
        if (requireNamespace("microbenchmark")) {
            print(this.path:::.faster_subsequent_times_test())
        } else cat("\n'package:microbenchmark' is not available :(\n")
    })
    cat("\n")
    this.path:::.Rscript(c("--default-packages=NULL", "--vanilla", FILE.R))


    cat("\n> source(FILE.R, chdir = FALSE)\n")
    source(FILE.R, chdir = FALSE)


    cat("\n> source(FILE.R, chdir = TRUE)\n")
    source(FILE.R, chdir = TRUE)
})
