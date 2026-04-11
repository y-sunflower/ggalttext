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
    if (length(chart_types) > 1 && "annotated chart" %in% chart_types) {
        chart_types <- setdiff(chart_types, "annotated chart")
    }

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
describe_facet_values_sentence <- function(build) {
    layout <- build$layout$layout
    if (is.null(layout) || !nrow(layout) || !"PANEL" %in% names(layout)) {
        return("")
    }

    reserved <- c("PANEL", "ROW", "COL", "SCALE_X", "SCALE_Y", "COORD")
    facet_vars <- setdiff(names(layout), reserved)
    if (!length(facet_vars)) {
        return("")
    }

    pieces <- character()
    panel_order <- order(layout$PANEL)

    for (facet_var in facet_vars) {
        vals <- as.character(layout[[facet_var]][panel_order])
        vals <- vals[nzchar(trimws(vals))]
        vals <- unique(vals)
        n_vals <- length(vals)
        if (n_vals <= 1) {
            next
        }

        facet_name <- gsub("_", " ", facet_var, fixed = TRUE)
        if (n_vals <= 8) {
            sentence <- paste0(
                "Facets by ",
                facet_name,
                " are ",
                join_with_and(paste0("'", vals, "'")),
                "."
            )
        } else {
            sentence <- paste0(
                "Facets by ",
                facet_name,
                " span ",
                n_vals,
                " values from '",
                vals[1],
                "' to '",
                vals[n_vals],
                "'."
            )
        }
        pieces <- c(pieces, sentence)
    }

    pieces[nzchar(pieces)]
}

#' @keywords internal
describe_discrete_scales_sentence <- function(build) {
    scales <- build$plot$scales$scales
    if (!length(scales)) {
        return("")
    }

    pieces <- character()
    described <- character()

    for (scale in scales) {
        aes <- scale$aesthetics
        if (!length(aes)) {
            next
        }

        aes_key <- intersect(
            aes,
            c("fill", "colour", "color", "linetype", "shape")
        )
        if (!length(aes_key)) {
            next
        }

        aes_key <- aes_key[1]
        if (aes_key %in% described) {
            next
        }

        limits <- tryCatch(scale$get_limits(), error = function(e) character())
        limits <- trimws(as.character(limits))
        limits <- limits[nzchar(limits)]
        if (length(limits) <= 1) {
            next
        }

        title <- scale$name
        if (inherits(title, "waiver")) {
            next
        }
        title <- normalize_label_text(title)

        if (length(limits) <= 6) {
            levels_txt <- join_with_and(paste0("'", limits, "'"))
        } else {
            levels_txt <- paste0(
                "'",
                limits[1],
                "' to '",
                limits[length(limits)],
                "' (",
                length(limits),
                " categories)"
            )
        }

        aes_label <- aesthetic_label(aes_key)
        if (nzchar(title)) {
            sentence <- paste0(
                tools::toTitleCase(aes_label),
                " categories ('",
                title,
                "') run from ",
                levels_txt,
                "."
            )
        } else {
            sentence <- paste0(
                tools::toTitleCase(aes_label),
                " categories run from ",
                levels_txt,
                "."
            )
        }

        pieces <- c(pieces, sentence)
        described <- c(described, aes_key)
    }

    pieces[nzchar(pieces)]
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

    value <- normalize_label_text(value)
    if (!length(value) || !nzchar(value)) {
        return("")
    }

    paste0(label_name, " is '", value, "'.")
}

#' @keywords internal
normalize_label_text <- function(value) {
    if (is.null(value)) {
        return("")
    }

    value <- paste(as.character(value), collapse = " ")
    value <- gsub("(?i)<br\\s*/?>", " ", value, perl = TRUE)
    value <- gsub("<[^>]+>", " ", value, perl = TRUE)

    html_entities <- c(
        "&nbsp;" = " ",
        "&amp;" = "&",
        "&lt;" = "<",
        "&gt;" = ">",
        "&quot;" = "\"",
        "&#39;" = "'"
    )
    for (entity in names(html_entities)) {
        value <- gsub(entity, html_entities[[entity]], value, fixed = TRUE)
    }

    value <- gsub("[[:space:]]+", " ", value, perl = TRUE)
    value <- gsub("[[:space:]]+([,.;:!?])", "\\1", value, perl = TRUE)
    value <- gsub("\\.\\.+", ".", value, perl = TRUE)
    trimws(value)
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
        GeomRichText = "annotated chart",
        GeomCurve = "annotated chart",
        GeomWaffle = "waffle chart",
        GeomSf = "map",
        "chart"
    )
}

#' @keywords internal
aesthetic_label <- function(aesthetic) {
    switch(
        aesthetic,
        fill = "fill",
        colour = "color",
        color = "color",
        linetype = "line type",
        shape = "shape",
        aesthetic
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
