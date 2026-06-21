#' @title Add alt text to Quarto figures
#'
#' @description
#' Call this function once at the top of your Quarto document
#' to include an alternative text for all of your ggplot2 charts.
#'
#' @param ... Arguments passed to [generate_alt_text()].
#'
#' @return NULL, invisibly
#'
#' @export
enable_auto_alt_text <- function(...) {
    original_hook <- knitr::knit_hooks$get("plot")
    knitr::knit_hooks$set(plot = function(x, options) {
        if (is.null(options$fig.alt)) {
            p <- ggplot2::last_plot()
            if (inherits(p, "ggplot")) {
                options$fig.alt <- tryCatch(
                    generate_alt_text(p, ...),
                    error = function(e) NULL
                )
            }
        }
        original_hook(x, options)
    })
    invisible(NULL)
}
