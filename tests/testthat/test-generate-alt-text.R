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

test_that("title subtitle and caption are included when present", {
    p <- ggplot(mtcars, aes(wt, mpg)) +
        geom_point() +
        labs(
            title = "Fuel efficiency by weight",
            subtitle = "Each point is one car",
            caption = "Source: mtcars"
        )

    text <- generate_alt_text(p)
    expect_equal(
        text,
        "Scatter Plot. Title is 'Fuel efficiency by weight'. Subtitle is 'Each point is one car'. Caption is 'Source: mtcars'."
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
})

test_that("html labels are normalized into plain text", {
    p <- ggplot(mtcars, aes(wt, mpg)) +
        geom_point() +
        labs(subtitle = "A<br><strong>bold</strong> move &amp; check")

    text <- generate_alt_text(p)
    expect_equal(
        text,
        "Scatter Plot. Subtitle is 'A bold move & check'."
    )
})

test_that("waffle geoms are recognised and discrete fill categories described", {
    d <- data.frame(
        source = c("Coal", "Oil", "Gas"),
        value = c(50, 30, 20)
    )

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
        geom_point() +
        labs(alt = NA)

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
