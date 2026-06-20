#' @keywords internal
alt_text_languages <- function() {
    # Add a language by adding a named spec with the same keys as `en`.
    list(
        en = list(
            conjunction = "and",
            serial_comma = TRUE,
            single_chart_case = "title",
            aesthetic_case = "title",
            chart_types = c(
                scatter_plot = "scatter plot",
                line_chart = "line chart",
                step_chart = "step chart",
                bar_chart = "bar chart",
                area_chart = "area chart",
                histogram = "histogram",
                density_plot = "density plot",
                box_plot = "box plot",
                violin_plot = "violin plot",
                heatmap = "heatmap",
                smoothed_line_chart = "smoothed line chart",
                band_chart = "band chart",
                segment_chart = "segment chart",
                annotated_chart = "annotated chart",
                waffle_chart = "waffle chart",
                map = "map",
                chart = "chart"
            ),
            aesthetics = c(
                fill = "fill",
                color = "color",
                line_type = "line type",
                shape = "shape"
            ),
            labels = c(title = "Title", caption = "Caption"),
            chart_unknown = "Chart, without more information.",
            chart_combined = "Combined chart with {types}.",
            panel_simple = "The data is shown in {panel_count} small charts.",
            panel_grid = paste0(
                "The data is split into {panel_count} small charts ",
                "arranged in a {n_rows} row(s) by {n_cols} col(s) grid."
            ),
            facet_values = "Facets by {facet_name} are {values}.",
            facet_span = paste0(
                "Facets by {facet_name} span {n_vals} values from ",
                "'{first_value}' to '{last_value}'."
            ),
            scale_with_title = paste0(
                "{aesthetic} categories ('{title}') run from {levels}."
            ),
            scale_without_title = "{aesthetic} categories run from {levels}.",
            scale_span = "'{first_value}' to '{last_value}' ({n_vals} categories)",
            label = "{label} is '{value}'."
        ),
        fr = list(
            conjunction = "et",
            serial_comma = FALSE,
            single_chart_case = "sentence",
            aesthetic_case = "none",
            chart_types = c(
                scatter_plot = "nuage de points",
                line_chart = "graphique en lignes",
                step_chart = "graphique en escalier",
                bar_chart = "diagramme en barres",
                area_chart = "graphique en aires",
                histogram = "histogramme",
                density_plot = "graphique de densite",
                box_plot = "boite a moustaches",
                violin_plot = "diagramme en violon",
                heatmap = "carte de chaleur",
                smoothed_line_chart = "courbe lissee",
                band_chart = "graphique a bande",
                segment_chart = "graphique en segments",
                annotated_chart = "graphique annote",
                waffle_chart = "diagramme waffle",
                map = "carte",
                chart = "graphique"
            ),
            aesthetics = c(
                fill = "remplissage",
                color = "couleur",
                line_type = "type de ligne",
                shape = "forme"
            ),
            labels = c(title = "Le titre", caption = "La legende"),
            chart_unknown = "Graphique, sans information supplementaire.",
            chart_combined = "Graphique combine avec {types}.",
            panel_simple = paste0(
                "Les donnees sont affichees dans {panel_count} ",
                "petits graphiques."
            ),
            panel_grid = paste0(
                "Les donnees sont reparties en {panel_count} petits ",
                "graphiques organises dans une grille de {n_rows} ",
                "ligne(s) par {n_cols} colonne(s)."
            ),
            facet_values = "Les facettes par {facet_name} sont {values}.",
            facet_span = paste0(
                "Les facettes par {facet_name} couvrent {n_vals} ",
                "valeurs de '{first_value}' a '{last_value}'."
            ),
            scale_with_title = paste0(
                "Les categories de {aesthetic} ('{title}') vont de {levels}."
            ),
            scale_without_title = paste0(
                "Les categories de {aesthetic} vont de {levels}."
            ),
            scale_span = "'{first_value}' a '{last_value}' ({n_vals} categories)",
            label = "{label} est '{value}'."
        ),
        de = list(
            conjunction = "und",
            serial_comma = FALSE,
            single_chart_case = "sentence",
            aesthetic_case = "none",
            chart_types = c(
                scatter_plot = "Streudiagramm",
                line_chart = "Liniendiagramm",
                step_chart = "Stufendiagramm",
                bar_chart = "Balkendiagramm",
                area_chart = "Flaechendiagramm",
                histogram = "Histogramm",
                density_plot = "Dichtediagramm",
                box_plot = "Boxplot",
                violin_plot = "Violindiagramm",
                heatmap = "Heatmap",
                smoothed_line_chart = "geglaettetes Liniendiagramm",
                band_chart = "Banddiagramm",
                segment_chart = "Segmentdiagramm",
                annotated_chart = "annotiertes Diagramm",
                waffle_chart = "Waffle-Diagramm",
                map = "Karte",
                chart = "Diagramm"
            ),
            aesthetics = c(
                fill = "Fuellung",
                color = "Farbe",
                line_type = "Linientyp",
                shape = "Form"
            ),
            labels = c(title = "Titel", caption = "Beschriftung"),
            chart_unknown = "Diagramm ohne weitere Informationen.",
            chart_combined = "Kombiniertes Diagramm mit {types}.",
            panel_simple = paste0(
                "Die Daten werden in {panel_count} kleinen Diagrammen gezeigt."
            ),
            panel_grid = paste0(
                "Die Daten sind auf {panel_count} kleine Diagramme in ",
                "einem Raster mit {n_rows} Zeile(n) und {n_cols} ",
                "Spalte(n) aufgeteilt."
            ),
            facet_values = "Facetten nach {facet_name} sind {values}.",
            facet_span = paste0(
                "Facetten nach {facet_name} umfassen {n_vals} Werte ",
                "von '{first_value}' bis '{last_value}'."
            ),
            scale_with_title = paste0(
                "Kategorien fuer {aesthetic} ('{title}') reichen von {levels}."
            ),
            scale_without_title = paste0(
                "Kategorien fuer {aesthetic} reichen von {levels}."
            ),
            scale_span = "'{first_value}' bis '{last_value}' ({n_vals} Kategorien)",
            label = "{label} ist '{value}'."
        )
    )
}

#' @keywords internal
match_language <- function(lang) {
    match.arg(lang, choices = names(alt_text_languages()))
}

#' @keywords internal
language_spec <- function(lang) {
    alt_text_languages()[[match_language(lang)]]
}

#' @keywords internal
language_lookup <- function(lang, section, key) {
    values <- language_spec(lang)[[section]][key]
    missing <- is.na(values)

    if (any(missing)) {
        fallback_values <- language_spec("en")[[section]][key[missing]]
        fallback_missing <- is.na(fallback_values)
        fallback_values[fallback_missing] <- gsub(
            "_",
            " ",
            key[missing][fallback_missing],
            fixed = TRUE
        )
        values[missing] <- fallback_values
    }

    unname(values)
}

#' @keywords internal
render_language_template <- function(template, values) {
    for (key in names(values)) {
        template <- gsub(
            paste0("{", key, "}"),
            as.character(values[[key]]),
            template,
            fixed = TRUE
        )
    }

    template
}

#' @keywords internal
apply_language_case <- function(value, case) {
    switch(
        case,
        title = tools::toTitleCase(value),
        sentence = paste0(
            toupper(substr(value, 1, 1)),
            substr(value, 2, nchar(value))
        ),
        none = value,
        value
    )
}

#' @keywords internal
quote_values <- function(values) {
    paste0("'", values, "'")
}

#' @keywords internal
join_language_items <- function(items, lang) {
    spec <- language_spec(lang)
    join_with_conjunction(items, spec$conjunction, spec$serial_comma)
}

#' @keywords internal
join_with_conjunction <- function(items, conjunction, serial_comma) {
    items <- items[nzchar(items)]
    n <- length(items)

    if (n == 0) {
        return("")
    }
    if (n == 1) {
        return(items[1])
    }
    if (n == 2) {
        return(paste(items, collapse = paste0(" ", conjunction, " ")))
    }

    separator <- if (serial_comma) {
        paste0(", ", conjunction, " ")
    } else {
        paste0(" ", conjunction, " ")
    }

    paste0(paste(items[1:(n - 1)], collapse = ", "), separator, items[n])
}
