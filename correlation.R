library(ggplot2)
library(ggalt)
options(scipen = 999)

midwest_subset <-  midwest[midwest$poptotal > 350000 &
                                   midwest$poptotal <= 500000 & 
                                   midwest$area > 0.01 & 
                                   midwest$area < 0.1,]

smoothing <- ggplot(midwest, aes(x = area, y = poptotal)) +
        geom_point(aes(col = state), size = 1.5) +
        geom_smooth(method = "loess") +
        scale_x_continuous(breaks = seq(0, 0.1, 0.02)) +
        ylim(0, 100000) +
        labs(
                title = "Scatter plot with loess curve",
                subtitle = "Area vs Population Total",
                x = "Area",
                y = "Population",
                caption = "Source: Midwest"
        ) + scale_color_brewer(palette = "Spectral") + 
        theme_minimal()

linear <- ggplot(midwest, aes(x = area, y = poptotal)) +
        geom_point(aes(col = state), size = 1.5) +
        geom_smooth(method = "lm", col = "darkred") +
        scale_x_continuous(breaks = seq(0, 0.1, 0.02)) +
        ylim(0, 100000) +
        labs(
                title = "Scatter plot with loess curve",
                subtitle = "Area vs Population Total",
                x = "Area",
                y = "Population",
                caption = "Source: Midwest"
        ) + scale_color_brewer(palette = "Set1") + 
        theme_gray()

encircling <- ggplot(midwest, aes(x = area, y = poptotal)) +
        geom_point(aes(col = state, size = popdensity)) +
        geom_smooth(method = "lm") +
        geom_encircle(
                aes(x = area, y = poptotal),
                data = midwest_subset,
                color = "red",
                size = 2,
                expand = 0.08
        ) +
        xlim(0, 0.1) +
        ylim(0, 500000) 

overlapping_points <- ggplot(mpg, aes(cty, hwy, col = cty)) +
        geom_point() +
        geom_smooth(method = "lm", se = F) +
        labs (
                title = "Scatter plot with overlapping points",
                subtitle = "City vs Highway Mileage",
                x = "City",
                y = "Highway Mileage",
                caption = "Source: Mpg"
        ) 

jitter_plot <- ggplot(mpg, aes(cty, hwy, col = cty)) +
        geom_jitter(width = .5) +
        geom_smooth(method = "lm", col = "red") +
        labs (
                title = "Scatter plot with overlapping points",
                subtitle = "City vs Highway Mileage",
                x = "City",
                y = "Highway Mileage",
                caption = "Source: Mpg"
        ) 

count_plot <- ggplot(mpg, aes(cty, hwy, col = cty)) +
        geom_count(col = "tomato3", show.legend = F) +
        labs(
                title = "Count Plot",
                subtitle = "City vs Highway Mileage",
                x = "City",
                y = "Highway Mileage",
                caption= "Source: Mpg"
        )

bubble_chart <-  ggplot(mpg_subset, aes(displ, cty)) +
        geom_jitter(aes(col = manufacturer, size = hwy)) +
        geom_smooth(aes(col = manufacturer), method = "lm", se = F) +
        labs(   title = "Bubble Chart",
                subtitle = "Displacement vs City Mileage",
                x = "Displacement",
                y = "Highway Mileage"
        )