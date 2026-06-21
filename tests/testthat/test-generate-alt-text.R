library(ggplot2)
library(dplyr)
library(babynames)

GeomWaffle <- ggplot2::ggproto("GeomWaffle", ggplot2::GeomPoint)

geom_waffle <- function(mapping) {
    ggplot2::layer(
        mapping = mapping,
        stat = "identity",
        geom = GeomWaffle,
        position = "identity"
    )
}

test_that("single-geom chart description starts with chart type", {
    p <- ggplot(mtcars, aes(wt, mpg)) +
        geom_point()

    text <- generate_alt_text(p)
    expect_equal(text, "Scatter plot.")
})

test_that("multi-geom chart lists combined chart types", {
    p <- ggplot(mtcars, aes(wt, mpg)) +
        geom_point() +
        geom_line()

    text <- generate_alt_text(p)
    expect_equal(text, "Combined chart with scatter plot and line chart.")
})

test_that("univariate charts describe explicitly labelled data", {
    density <- ggplot(faithful, aes(waiting)) +
        geom_density() +
        xlab("Waiting time between eruptions (mins)")
    histogram <- ggplot(faithful, aes(waiting)) +
        geom_histogram(binwidth = 10) +
        xlab("Waiting time")
    horizontal_bar <- ggplot(mtcars, aes(y = factor(cyl))) +
        geom_bar() +
        ylab("Cylinders")

    expect_equal(
        generate_alt_text(density),
        "Density plot of Waiting time between eruptions (mins)."
    )
    expect_equal(generate_alt_text(histogram), "Histogram of Waiting time.")
    expect_equal(generate_alt_text(horizontal_bar), "Bar chart of Cylinders.")
})

test_that("two-variable charts require two explicit labels", {
    labelled <- ggplot(mtcars, aes(wt, mpg)) +
        geom_point() +
        labs(x = "Weight", y = "Mileage")
    partly_labelled <- ggplot(mtcars, aes(wt, mpg)) +
        geom_point() +
        xlab("Weight")
    inferred <- ggplot(mtcars, aes(wt, mpg)) +
        geom_point()

    expect_equal(
        generate_alt_text(labelled),
        "Scatter plot of Mileage by Weight."
    )
    expect_equal(generate_alt_text(partly_labelled), "Scatter plot.")
    expect_equal(generate_alt_text(inferred), "Scatter plot.")
})

test_that("explicit scale names are used as data labels", {
    p <- ggplot(mtcars, aes(wt, mpg)) +
        geom_line() +
        scale_x_continuous(name = "Weight") +
        scale_y_continuous(name = "Mileage")

    expect_equal(generate_alt_text(p), "Line chart of Mileage by Weight.")
})

test_that("univariate box plots use their sole data axis", {
    p <- ggplot(mtcars, aes(y = mpg)) +
        geom_boxplot() +
        ylab("Mileage")

    expect_equal(generate_alt_text(p), "Box plot of Mileage.")
})

test_that("heatmaps describe the fill measure and both axes", {
    labelled <- ggplot(
        mtcars,
        aes(factor(cyl), factor(gear), fill = mpg)
    ) +
        geom_tile() +
        labs(x = "Cylinders", y = "Gears", fill = "Mileage")
    missing_fill <- labelled + labs(fill = NULL)

    expect_equal(
        generate_alt_text(labelled),
        "Heatmap of Mileage by Gears and Cylinders."
    )
    expect_equal(generate_alt_text(missing_fill), "Heatmap.")
})

test_that("compatible combined charts share a data description", {
    p <- ggplot(mtcars, aes(wt, mpg)) +
        geom_point() +
        geom_line() +
        labs(x = "Weight", y = "Mileage")

    expect_equal(
        generate_alt_text(p),
        "Combined chart with scatter plot and line chart of Mileage by Weight."
    )
})

test_that("incompatible combined charts omit the data description", {
    p <- ggplot(mtcars, aes(wt, mpg)) +
        geom_point() +
        geom_density(aes(y = after_stat(density))) +
        labs(x = "Weight", y = "Mileage")

    expect_equal(
        generate_alt_text(p),
        "Combined chart with scatter plot and density plot."
    )
})

test_that("data descriptions are translated", {
    p <- ggplot(mtcars, aes(wt, mpg)) +
        geom_point() +
        labs(x = "Weight", y = "Mileage")

    expect_equal(
        generate_alt_text(p, lang = "fr"),
        "Nuage de points de Mileage en fonction de Weight."
    )
    expect_equal(
        generate_alt_text(p, lang = "de"),
        "Streudiagramm fuer Mileage nach Weight."
    )
})

test_that("data descriptions compose with facets and titles", {
    p <- ggplot(mtcars, aes(wt, mpg)) +
        geom_point() +
        facet_wrap(~cyl) +
        labs(x = "Weight", y = "Mileage", title = "Efficiency")

    expect_equal(
        generate_alt_text(p),
        paste0(
            "Scatter plot of Mileage by Weight split into 3 small charts ",
            "arranged in a 1-row by 3-column grid, titled “Efficiency”."
        )
    )
})

test_that("data descriptions retain the maximum length contract", {
    p <- ggplot(mtcars, aes(wt, mpg)) +
        geom_point() +
        labs(x = "Weight", y = "Mileage")

    expect_warning(
        generate_alt_text(p, max_chars = 20),
        "Alternative text is more than 20 characters"
    )
    expect_error(
        generate_alt_text(
            p,
            max_chars = 20,
            error_on_excess = TRUE
        ),
        "Alternative text is more than 20 characters"
    )
})

test_that("multi-panel layout is described in plain language", {
    p <- ggplot(mtcars, aes(wt, mpg)) +
        geom_point() +
        facet_wrap(~cyl)

    text <- generate_alt_text(p)
    expect_equal(
        text,
        "Scatter plot split into 3 small charts arranged in a 1-row by 3-column grid."
    )
})

test_that("title is included when present and in the right order", {
    p <- ggplot(mtcars, aes(wt, mpg)) +
        geom_point() +
        labs(
            title = "Fuel efficiency by weight",
            caption = "Source: mtcars"
        )

    text <- generate_alt_text(p)
    expect_equal(
        text,
        "Scatter plot, titled \u201cFuel efficiency by weight\u201d."
    )
})

test_that("plot labels can be excluded individually", {
    p <- ggplot(mtcars, aes(wt, mpg)) +
        geom_point() +
        labs(title = "Fuel efficiency by weight", caption = "Source: mtcars")

    expect_equal(
        generate_alt_text(p, include_title = FALSE),
        "Scatter plot."
    )
})

test_that("lang controls generated French text", {
    p <- ggplot(mtcars, aes(wt, mpg)) +
        geom_point() +
        facet_wrap(~cyl) +
        labs(title = "Fuel efficiency")

    text <- generate_alt_text(p, lang = "fr", max_chars = 140)
    expect_equal(
        text,
        paste(
            paste0(
                "Nuage de points reparti en 3 petits graphiques organises ",
                "dans une grille de 1 ligne par 3 colonnes, avec pour titre ",
                "\u00ab Fuel efficiency \u00bb."
            )
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

    text <- generate_alt_text(p, max_chars = 140)
    expect_equal(
        text,
        "Area chart split into 6 small charts arranged in a 2-row by 3-column grid, titled \u201cPopularity of American names in the previous 30 years\u201d."
    )

    text <- generate_alt_text(p, lang = "fr", max_chars = 180)
    expect_equal(
        text,
        "Graphique en aires reparti en 6 petits graphiques organises dans une grille de 2 lignes par 3 colonnes, avec pour titre \u00ab Popularity of American names in the previous 30 years \u00bb."
    )

    text <- generate_alt_text(p, lang = "de", max_chars = 166)
    expect_equal(
        text,
        "Flaechendiagramm, aufgeteilt auf 6 kleine Diagramme in einem Raster mit 2 Zeilen und 3 Spalten mit dem Titel \u201ePopularity of American names in the previous 30 years\u201c."
    )
})

test_that("html labels are normalized into plain text", {
    p <- ggplot(mtcars, aes(wt, mpg)) +
        geom_point() +
        labs(title = "A<br><strong>bold</strong> move &amp; check")

    text <- generate_alt_text(p)
    expect_equal(text, "Scatter plot, titled \u201cA bold move & check\u201d.")
})

test_that("waffle geoms are recognised and discrete fill categories described", {
    d <- data.frame(source = c("Coal", "Oil", "Gas"), value = c(50, 30, 20))

    p <- ggplot(d) +
        geom_waffle(aes(value, value, fill = source)) +
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
        "Waffle chart. Fill categories ('Energy Source') run from 'Coal', 'Gas', and 'Oil'."
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
    expect_equal(text, "Scatter plot.")
})

test_that("source = auto uses built-in alt text when available", {
    p <- ggplot(mtcars, aes(wt, mpg)) +
        geom_point() +
        labs(alt = "Built-in alt text.")

    text <- generate_alt_text(p, source = "auto")
    expect_equal(text, "Built-in alt text.")
})

test_that("source = auto falls back to ggalttext when built-in alt text is missing", {
    p <- ggplot(mtcars, aes(wt, mpg)) +
        geom_point()

    text <- generate_alt_text(p, source = "auto")
    expect_equal(text, "Scatter plot.")
})

test_that("source = origin returns ggplot2 origin alt text as-is", {
    p <- ggplot(mtcars, aes(wt, mpg)) +
        geom_point() +
        labs(alt = NA)

    text <- generate_alt_text(p, source = "origin")
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
        "'arg' should be one of \"en\", \"fr\", \"de\"",
        fixed = TRUE
    )
})
