#' @keywords internal
describe_chart_type_sentence <- function(p, lang = "en") {
    geom_classes <- vapply(
        p$layers,
        function(layer) class(layer$geom)[1],
        character(1)
    )
    geom_classes <- unique(geom_classes[nzchar(geom_classes)])
    chart_type_keys <- unique(vapply(
        geom_classes,
        geom_class_to_chart_type_key,
        character(1)
    ))
    chart_type_keys <- chart_type_keys[nzchar(chart_type_keys)]
    if (length(chart_type_keys) > 1 && "annotated_chart" %in% chart_type_keys) {
        chart_type_keys <- setdiff(chart_type_keys, "annotated_chart")
    }

    spec <- language_spec(lang)

    if (!length(chart_type_keys)) {
        sentence <- spec$chart_unknown
    } else if (length(chart_type_keys) == 1) {
        chart_type <- language_lookup(lang, "chart_types", chart_type_keys[1])
        sentence <- paste0(
            apply_language_case(chart_type, spec$single_chart_case),
            "."
        )
    } else {
        chart_types <- language_lookup(lang, "chart_types", chart_type_keys)
        sentence <- render_language_template(
            spec$chart_combined,
            list(types = join_language_items(chart_types, lang))
        )
    }

    return(sentence)
}

#' @keywords internal
describe_panel_layout_sentence <- function(build, lang = "en", chart = NULL) {
    layout <- build$layout$layout
    if (is.null(layout) || !nrow(layout) || !"PANEL" %in% names(layout)) {
        return("")
    }

    panel_count <- length(unique(layout$PANEL))
    if (panel_count <= 1) {
        return("")
    }

    spec <- language_spec(lang)
    template_prefix <- if (is.null(chart)) "panel" else "chart_panel"
    has_grid <- all(c("ROW", "COL") %in% names(layout))
    if (!has_grid) {
        return(render_language_template(
            spec[[paste0(template_prefix, "_simple")]],
            list(chart = chart, panel_count = panel_count)
        ))
    }

    n_rows <- max(layout$ROW, na.rm = TRUE)
    n_cols <- max(layout$COL, na.rm = TRUE)
    row_label <- if (n_rows == 1) {
        spec$panel_row["one"]
    } else {
        spec$panel_row["other"]
    }
    col_label <- if (n_cols == 1) {
        spec$panel_col["one"]
    } else {
        spec$panel_col["other"]
    }

    render_language_template(
        spec[[paste0(template_prefix, "_grid")]],
        list(
            chart = chart,
            panel_count = panel_count,
            n_rows = n_rows,
            n_cols = n_cols,
            row_label = unname(row_label),
            col_label = unname(col_label)
        )
    )
}

#' @keywords internal
describe_discrete_scales_sentence <- function(build, lang = "en") {
    scales <- build$plot$scales$scales
    if (!length(scales)) {
        return("")
    }

    pieces <- character()
    described <- character()
    spec <- language_spec(lang)

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

        aes_key <- aesthetic_key(aes_key[1])
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
            levels_txt <- join_language_items(quote_values(limits), lang)
        } else {
            levels_txt <- render_language_template(
                spec$scale_span,
                list(
                    first_value = limits[1],
                    last_value = limits[length(limits)],
                    n_vals = length(limits)
                )
            )
        }

        aes_label <- aesthetic_label(aes_key, lang = lang)
        aes_label <- apply_language_case(aes_label, spec$aesthetic_case)
        if (nzchar(title)) {
            sentence <- render_language_template(
                spec$scale_with_title,
                list(aesthetic = aes_label, title = title, levels = levels_txt)
            )
        } else {
            sentence <- render_language_template(
                spec$scale_without_title,
                list(aesthetic = aes_label, levels = levels_txt)
            )
        }

        pieces <- c(pieces, sentence)
        described <- c(described, aes_key)
    }

    pieces[nzchar(pieces)]
}

#' @keywords internal
append_plot_title <- function(sentence, p, lang = "en", include_title = TRUE) {
    if (!include_title) {
        return(sentence)
    }

    title <- normalize_label_text(p$labels$title)
    if (!nzchar(title)) {
        return(sentence)
    }

    spec <- language_spec(lang)
    render_language_template(
        spec$chart_with_title,
        list(
            chart = sub("[.]$", "", sentence),
            title = title
        )
    )
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
geom_class_to_chart_type_key <- function(geom_class) {
    switch(
        geom_class,
        GeomPoint = "scatter_plot",
        GeomLine = "line_chart",
        GeomPath = "line_chart",
        GeomStep = "step_chart",
        GeomBar = "bar_chart",
        GeomCol = "bar_chart",
        GeomArea = "area_chart",
        GeomHistogram = "histogram",
        GeomDensity = "density_plot",
        GeomBoxplot = "box_plot",
        GeomViolin = "violin_plot",
        GeomTile = "heatmap",
        GeomRaster = "heatmap",
        GeomSmooth = "smoothed_line_chart",
        GeomRibbon = "band_chart",
        GeomSegment = "segment_chart",
        GeomText = "annotated_chart",
        GeomLabel = "annotated_chart",
        GeomRichText = "annotated_chart",
        GeomCurve = "annotated_chart",
        GeomWaffle = "waffle_chart",
        GeomSf = "map",
        "chart"
    )
}

#' @keywords internal
aesthetic_key <- function(aesthetic) {
    switch(
        aesthetic,
        fill = "fill",
        colour = "color",
        color = "color",
        linetype = "line_type",
        shape = "shape",
        aesthetic
    )
}

#' @keywords internal
aesthetic_label <- function(aesthetic, lang = "en") {
    language_lookup(lang, "aesthetics", aesthetic_key(aesthetic))
}
