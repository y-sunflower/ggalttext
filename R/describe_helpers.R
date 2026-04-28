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
describe_panel_layout_sentence <- function(build, lang = "en") {
    layout <- build$layout$layout
    if (is.null(layout) || !nrow(layout) || !"PANEL" %in% names(layout)) {
        return("")
    }

    panel_count <- length(unique(layout$PANEL))
    if (panel_count <= 1) {
        return("")
    }

    spec <- language_spec(lang)
    has_grid <- all(c("ROW", "COL") %in% names(layout))
    if (!has_grid) {
        return(render_language_template(
            spec$panel_simple,
            list(panel_count = panel_count)
        ))
    }

    n_rows <- max(layout$ROW, na.rm = TRUE)
    n_cols <- max(layout$COL, na.rm = TRUE)

    render_language_template(
        spec$panel_grid,
        list(panel_count = panel_count, n_rows = n_rows, n_cols = n_cols)
    )
}

#' @keywords internal
describe_facet_values_sentence <- function(build, lang = "en") {
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
    spec <- language_spec(lang)

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
            sentence <- render_language_template(
                spec$facet_values,
                list(
                    facet_name = facet_name,
                    values = join_language_items(quote_values(vals), lang)
                )
            )
        } else {
            sentence <- render_language_template(
                spec$facet_span,
                list(
                    facet_name = facet_name,
                    n_vals = n_vals,
                    first_value = vals[1],
                    last_value = vals[n_vals]
                )
            )
        }
        pieces <- c(pieces, sentence)
    }

    pieces[nzchar(pieces)]
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
describe_plot_labels_sentences <- function(p, lang = "en") {
    labels <- p$labels
    pieces <- c(
        label_sentence(labels$title, "title", lang = lang),
        label_sentence(labels$subtitle, "subtitle", lang = lang),
        label_sentence(labels$caption, "caption", lang = lang)
    )
    pieces[nzchar(pieces)]
}

#' @keywords internal
label_sentence <- function(value, label_name, lang = "en") {
    if (is.null(value)) {
        return("")
    }

    value <- normalize_label_text(value)
    if (!length(value) || !nzchar(value)) {
        return("")
    }

    spec <- language_spec(lang)
    render_language_template(
        spec$label,
        list(
            label = language_lookup(lang, "labels", label_name),
            value = value
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
