library(ggplot2)
library(dplyr)
library(babynames)
library(waffle)

test_that("single-geom chart description starts with chart type", {
    p <- ggplot(mtcars, aes(wt, mpg)) +
        geom_point()

    text <- generate_alt_text(p)
    expect_equal(text, "Scatter Plot.")
})

test_that("multi-geom chart lists combined chart types", {
    p <- ggplot(mtcars, aes(wt, mpg)) +
        geom_point() +
        geom_line()

    text <- generate_alt_text(p)
    expect_equal(text, "Combined chart with scatter plot and line chart.")
})

test_that("multi-panel layout is described in plain language", {
    p <- ggplot(mtcars, aes(wt, mpg)) +
        geom_point() +
        facet_wrap(~cyl)

    text <- generate_alt_text(p)
    expect_equal(
        text,
        "Scatter Plot. The data is split into 3 small charts arranged in a 1 row(s) by 3 col(s) grid. Facets by cyl are '4', '6', and '8'."
    )
})

test_that("title and caption are included when present and in the right order", {
    p <- ggplot(mtcars, aes(wt, mpg)) +
        geom_point() +
        labs(
            title = "Fuel efficiency by weight",
            caption = "Source: mtcars"
        )

    text <- generate_alt_text(p)
    expect_equal(
        text,
        "Scatter Plot. Title is 'Fuel efficiency by weight'. Caption is 'Source: mtcars'."
    )
})

test_that("plot labels can be excluded individually", {
    p <- ggplot(mtcars, aes(wt, mpg)) +
        geom_point() +
        labs(title = "Fuel efficiency by weight", caption = "Source: mtcars")

    expect_equal(
        generate_alt_text(p, include_title = FALSE),
        "Scatter Plot. Caption is 'Source: mtcars'."
    )
    expect_equal(
        generate_alt_text(p, include_caption = FALSE),
        "Scatter Plot. Title is 'Fuel efficiency by weight'."
    )
    expect_equal(
        generate_alt_text(p, include_caption = FALSE, include_title = FALSE),
        "Scatter Plot."
    )
    expect_equal(
        generate_alt_text(p, include_caption = FALSE, include_title = FALSE),
        "Scatter Plot."
    )
})

test_that("lang controls generated French text", {
    p <- ggplot(mtcars, aes(wt, mpg)) +
        geom_point() +
        facet_wrap(~cyl) +
        labs(title = "Fuel efficiency")

    text <- generate_alt_text(p, lang = "fr")
    expect_equal(
        text,
        paste(
            "Nuage de points.",
            paste0(
                "Les donnees sont reparties en 3 petits graphiques organises ",
                "dans une grille de 1 ligne(s) par 3 colonne(s)."
            ),
            "Les facettes par cyl sont '4', '6' et '8'.",
            "Le titre est 'Fuel efficiency'."
        )
    )
})

test_that("lang controls generated German text", {
    p <- ggplot(mtcars, aes(factor(cyl), fill = factor(cyl))) +
        geom_bar() +
        scale_fill_discrete(name = "Cylinders")

    text <- generate_alt_text(p, lang = "de")
    expect_equal(
        text,
        paste(
            "Balkendiagramm.",
            paste0(
                "Kategorien fuer Fuellung ('Cylinders') reichen von ",
                "'4', '6' und '8'."
            )
        )
    )
})

test_that("Complex grid with title", {
    p <- babynames |>
        filter(
            name %in%
                c(
                    "Amanda",
                    "Jessica",
                    "Patricia",
                    "Deborah",
                    "Dorothy",
                    "Helen"
                )
        ) |>
        filter(sex == "F") |>
        ggplot(aes(x = year, y = n, group = name, fill = name)) +
        geom_area() +
        theme(legend.position = "none") +
        ggtitle("Popularity of American names in the previous 30 years") +
        theme(
            legend.position = "none",
            panel.spacing = unit(0.1, "lines"),
            strip.text.x = element_text(size = 8)
        ) +
        facet_wrap(~name, scale = "free_y")

    text <- generate_alt_text(p)
    expect_equal(
        text,
        "Area Chart. The data is split into 6 small charts arranged in a 2 row(s) by 3 col(s) grid. Facets by name are 'Amanda', 'Deborah', 'Dorothy', 'Helen', 'Jessica', and 'Patricia'. Title is 'Popularity of American names in the previous 30 years'."
    )

    text <- generate_alt_text(p, lang = "fr")
    expect_equal(
        text,
        "Graphique en aires. Les donnees sont reparties en 6 petits graphiques organises dans une grille de 2 ligne(s) par 3 colonne(s). Les facettes par name sont 'Amanda', 'Deborah', 'Dorothy', 'Helen', 'Jessica' et 'Patricia'. Le titre est 'Popularity of American names in the previous 30 years'."
    )

    text <- generate_alt_text(p, lang = "de")
    expect_equal(
        text,
        "Flaechendiagramm. Die Daten sind auf 6 kleine Diagramme in einem Raster mit 2 Zeile(n) und 3 Spalte(n) aufgeteilt. Facetten nach name sind 'Amanda', 'Deborah', 'Dorothy', 'Helen', 'Jessica' und 'Patricia'. Titel ist 'Popularity of American names in the previous 30 years'."
    )
})

test_that("html labels are normalized into plain text", {
    p <- ggplot(mtcars, aes(wt, mpg)) +
        geom_point() +
        labs(title = "A<br><strong>bold</strong> move &amp; check")

    text <- generate_alt_text(p)
    expect_equal(text, "Scatter Plot. Title is 'A bold move & check'.")
})

test_that("waffle geoms are recognised and discrete fill categories described", {
    d <- data.frame(source = c("Coal", "Oil", "Gas"), value = c(50, 30, 20))

    p <- ggplot(d, aes(fill = source, values = value)) +
        geom_waffle(n_rows = 5) +
        scale_fill_manual(
            values = c(
                Coal = "#111111",
                Oil = "#222222",
                Gas = "#333333"
            ),
            name = "Energy Source"
        )

    text <- generate_alt_text(p)
    expect_equal(
        text,
        "Waffle Chart. Fill categories ('Energy Source') run from 'Coal', 'Gas', and 'Oil'."
    )
})

test_that("patchwork inset does not replace the primary chart description", {
    p <- ggplot(mtcars, aes(wt, mpg)) +
        geom_point() +
        patchwork::inset_element(
            ggplot() +
                ggtext::geom_richtext(aes(x = 0.5, y = 0.5, label = "note")),
            left = 0,
            right = 1,
            bottom = 0,
            top = 1,
            align_to = "full"
        )

    text <- generate_alt_text(p)
    expect_equal(text, "Scatter Plot.")
})

test_that("from = auto uses built-in alt text when available", {
    p <- ggplot(mtcars, aes(wt, mpg)) +
        geom_point() +
        labs(alt = "Built-in alt text.")

    text <- generate_alt_text(p, from = "auto")
    expect_equal(text, "Built-in alt text.")
})

test_that("from = auto falls back to ggalttext when built-in alt text is missing", {
    p <- ggplot(mtcars, aes(wt, mpg)) +
        geom_point()

    text <- generate_alt_text(p, from = "auto")
    expect_equal(text, "Scatter Plot.")
})

test_that("from = origin returns ggplot2 origin alt text as-is", {
    p <- ggplot(mtcars, aes(wt, mpg)) +
        geom_point() +
        labs(alt = NA)

    text <- generate_alt_text(p, from = "origin")
    expect_true(is.na(text))
})

test_that("invalid ggplot object error formats class names", {
    obj <- structure(list(), class = c("foo", "bar"))

    expect_error(
        generate_alt_text(obj),
        "Object is not a valid ggplot2 object: 'foo, bar'\\."
    )
})

test_that("unsupported language errors clearly", {
    p <- ggplot(mtcars, aes(wt, mpg)) +
        geom_point()

    expect_error(
        generate_alt_text(p, lang = "es"),
        "should be one of"
    )
})
