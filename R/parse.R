#' @export
generate_alt_text <- function(p) {
    b <- ggplot2::ggplot_build(p)

    pieces <- c(
        describe_chart_type_sentence(p),
        describe_panel_layout_sentence(b),
        describe_plot_labels_sentences(p)
    )

    paste(pieces[nzchar(pieces)], collapse = " ")
}
