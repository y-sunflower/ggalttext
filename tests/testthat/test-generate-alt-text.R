library(ggplot2)
library(dplyr)
library(babynames)

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
        "Scatter Plot. The data is split into 3 small charts arranged in a 1 row(s) by 3 col(s) grid."
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
        "Area Chart. The data is split into 6 small charts arranged in a 2 row(s) by 3 col(s) grid. Title is 'Popularity of American names in the previous 30 years'."
    )
})
