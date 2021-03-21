if (FALSE) {
    (function() {
        pkg <- "this.path"

        odir <- getwd()
        if (!is.null(odir))
            on.exit(setwd(odir))
        setwd("~")

        command <- sprintf("Rcmd INSTALL %s --with-keep.source", pkg); cat("\n")
        cat(normalizePath(getwd()), ">", command, "\n\n", sep = "")
        system(command); cat("\n")

        command <- sprintf("Rcmd build %s", pkg)
        cat(normalizePath(getwd()), ">", command, "\n\n", sep = "")
        system(command)

        command <- sprintf("Rcmd check %s_%s.tar.gz --as-cran",
            pkg, utils::packageVersion(pkg))
        cat(normalizePath(getwd()), ">", command, "\n\n", sep = "")
        system(command)
    })()
}
