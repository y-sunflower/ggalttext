# ggalt

Make ggplot2 fully accessible by generating alternative text. `ggalt` provides a single function, `generate_alt_text()`, which takes any ggplot2 object and generates a string describing the graph's content.

This text is **not** intended to describe the plot word for word, nor how it was created. Rather, it aims to be concise and provide an overview of the plot's content, for example:

- the kind of chart(s)
- the number of chart(s) for facets
- the title, subtitle and caption

<br>

## Installation

```r
#install.packages("pak")
pak::pak("y-sunflower/ggalt")
```

<br>

## Example

```r
library(ggplot2)
library(babynames)
library(ggalt)

plot <- ggplot(babynames, aes(x = year, y = n, group = name, fill = name)) +
    geom_area() +
    theme(legend.position = "none") +
    ggtitle("Popularity of American names in the previous 30 years") +
    theme(
        legend.position = "none",
        panel.spacing = unit(0.1, "lines"),
        strip.text.x = element_text(size = 8)
    ) +
    facet_wrap(~name, scale = "free_y")
```

![Area Chart. The data is split into 6 small charts arranged in a 2 row(s) by 3 col(s) grid. Title is 'Popularity of American names in the previous 30 years'.](./example.png)

```r
generate_alt_text(plot)
#> "Area Chart. The data is split into 6 small charts arranged
# in a 2 row(s) by 3 col(s) grid. Title is 'Popularity of American
# names in the previous 30 years'."
```
