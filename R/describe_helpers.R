#' @keywords internal
describe_chart_type_sentence <- function(p) {
    geom_classes <- vapply(
        p$layers,
        function(layer) class(layer$geom)[1],
        character(1)
    )
    geom_classes <- unique(geom_classes[nzchar(geom_classes)])
    chart_types <- unique(vapply(
        geom_classes,
        geom_class_to_chart_type,
        character(1)
    ))
    chart_types <- chart_types[nzchar(chart_types)]

    if (!length(chart_types)) {
        sentence <- "Chart, without more information."
    } else if (length(chart_types) == 1) {
        sentence <- tools::toTitleCase(paste0(chart_types[1], "."))
    } else {
        sentence <- paste0(
            "Combined chart with ",
            join_with_and(chart_types),
            "."
        )
    }

    return(sentence)
}

#' @keywords internal
describe_panel_layout_sentence <- function(build) {
    layout <- build$layout$layout
    if (is.null(layout) || !nrow(layout) || !"PANEL" %in% names(layout)) {
        return("")
    }

    panel_count <- length(unique(layout$PANEL))
    if (panel_count <= 1) {
        return("")
    }

    has_grid <- all(c("ROW", "COL") %in% names(layout))
    if (!has_grid) {
        return(paste0("The data is shown in ", panel_count, " small charts."))
    }

    n_rows <- max(layout$ROW, na.rm = TRUE)
    n_cols <- max(layout$COL, na.rm = TRUE)

    paste0(
        "The data is split into ",
        panel_count,
        " small charts arranged in a ",
        n_rows,
        " row(s) by ",
        n_cols,
        " col(s) grid."
    )
}

#' @keywords internal
describe_plot_labels_sentences <- function(p) {
    labels <- p$labels
    pieces <- c(
        label_sentence(labels$title, "Title"),
        label_sentence(labels$subtitle, "Subtitle"),
        label_sentence(labels$caption, "Caption")
    )
    pieces[nzchar(pieces)]
}

#' @keywords internal
label_sentence <- function(value, label_name) {
    if (is.null(value)) {
        return("")
    }

    value <- trimws(as.character(value))
    if (!length(value) || !nzchar(value)) {
        return("")
    }

    paste0(label_name, " is '", value, "'.")
}

#' @keywords internal
geom_class_to_chart_type <- function(geom_class) {
    switch(
        geom_class,
        GeomPoint = "scatter plot",
        GeomLine = "line chart",
        GeomPath = "line chart",
        GeomStep = "step chart",
        GeomBar = "bar chart",
        GeomCol = "bar chart",
        GeomArea = "area chart",
        GeomHistogram = "histogram",
        GeomDensity = "density plot",
        GeomBoxplot = "box plot",
        GeomViolin = "violin plot",
        GeomTile = "heatmap",
        GeomRaster = "heatmap",
        GeomSmooth = "smoothed line chart",
        GeomRibbon = "band chart",
        GeomSegment = "segment chart",
        GeomText = "annotated chart",
        GeomLabel = "annotated chart",
        GeomSf = "map",
        "chart"
    )
}

#' @keywords internal
join_with_and <- function(items) {
    items <- items[nzchar(items)]
    n <- length(items)

    if (n == 0) {
        return("")
    }
    if (n == 1) {
        return(items[1])
    }
    if (n == 2) {
        return(paste(items, collapse = " and "))
    }

    paste0(paste(items[1:(n - 1)], collapse = ", "), ", and ", items[n])
}
