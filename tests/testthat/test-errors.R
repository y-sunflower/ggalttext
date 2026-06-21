test_that("Errors are raised with valid messages", {
    p <- ggplot2::ggplot(faithful) +
        ggplot2::geom_density(ggplot2::aes(x = waiting)) +
        ggplot2::xlab("Waiting time between eruptions (mins)")

    expect_error(
        generate_alt_text(p, "hey"),
        "All arguments (except p, the plot) must be named.",
        fixed = TRUE
    )
})
