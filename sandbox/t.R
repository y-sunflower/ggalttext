library(tidyverse)
library(babynames)
library(streamgraph)
devtools::load_all()

p <- babynames |>
    filter(
        name %in%
            c("Amanda", "Jessica", "Patricia", "Deborah", "Dorothy", "Helen")
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

generate_alt_text(p)
p
