#' @title Generate alternative text from a ggplot2 object
#'
#' @description
#' `generate_alt_text()` takes any ggplot2 object and generate
#' a string that describes what contains the chart.
#'
#' The description isn't meant to exactly describe the chart,
#' nor how it was made. Instead, it tries to be short and
#' gives an overview of what's inside the chart such as:
#' - the kind of chart(s)
#' - the number of chart(s) for facets
#' - the title, subtitle and caption
#'
#' Learn more: https://www.section508.gov/create/alternative-text/
#'
#' @param p A ggplot2 chart
#'
#' @return A string
#'
#' @import ggplot2
#'
#' @export
generate_alt_text <- function(p) {
    b <- ggplot_build(p)

    pieces <- c(
        describe_chart_type_sentence(p),
        describe_panel_layout_sentence(b),
        describe_plot_labels_sentences(p)
    )

    paste(pieces[nzchar(pieces)], collapse = " ")
}
