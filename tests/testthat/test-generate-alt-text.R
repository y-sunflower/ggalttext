library(ggplot2)

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
    expect_match(text, "Title: Fuel efficiency by weight\\.")
    expect_match(text, "Subtitle: Each point is one car\\.")
    expect_match(text, "Caption: Source: mtcars\\.")
})

test_that("text no longer includes trend min max or mapping names", {
    p <- ggplot(mtcars, aes(wt, mpg)) +
        geom_point()

    text <- generate_alt_text(p)
    expect_no_match(text, "Overall,")
    expect_no_match(text, "lowest value")
    expect_no_match(text, "highest")
    expect_no_match(text, "Values range")
    expect_no_match(text, "\\bwt\\b")
    expect_no_match(text, "\\bmpg\\b")
})
