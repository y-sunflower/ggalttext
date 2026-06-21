library(ggplot2)

test_that("Errors are raised with valid messages", {
    p <- ggplot(faithful) +
        geom_density(aes(x = waiting)) +
        xlab("Waiting time between eruptions (mins)")

    expect_error(
        generate_alt_text(p, "hey"),
        "All arguments after `p` must be named.",
        fixed = TRUE
    )

    expect_error(
        generate_alt_text(p, typo = "hey"),
        "Unused argument(s): `typo`.",
        fixed = TRUE
    )
})
