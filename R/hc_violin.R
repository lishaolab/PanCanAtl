prepare_violin <- function(data) {
    l <- lapply(names(data), function(name) {
        series <- data[[name]]
        index <- match(name, names(data)) - 1
        density <- series %>% density()
        multiplier <- 1 / (2.5 * max(density$y))
        index
        cbind(density$x, -multiplier * density$y + index,
            multiplier * density$y + index)
    })
    names(l) <- names(data)
    l
}
plot_violin <- function(data) {
    hc <- highchart() %>%
        hc_plotOptions(chart = list(inverted = FALSE),
            line = list(linecap = 'square')) %>%
        hc_yAxis(type = 'category',
            min = 0,
            max = length(data) - 1,
            tickLength = 0,
            categories = names(data),
            labels = list(useHTML = TRUE,
                align = 'left',
                reserveSpace = TRUE),
            lineWidth = 0,
            lineColor = 'transparent') %>%
        hc_legend(enabled = FALSE) %>%
        hc_xAxis(tickInterval = 10,
            showFirstLabel = FALSE)

    densities <- prepare_violin(data)
    i <- 0

    delta <- 0.1
    for (name in names(data)) {
        i <- i + 1

        series_data <- data[[name]]
        if (!is.null(data)) {
            # compute violin plot
            values <- data
            max_density <- max(densities[[name]])

            # add violin plot
            hc %<>% hc_add_series(data = densities[[name]],
                type = 'areasplinerange',
                enableMouseTracking = T,
                color = '#437bcc',
                lineColor = 'black',
                lineWidth = 2)

            # # compute percentiles 5 and 95
            # small <- quantile(data[[name]], c(0.05, 0.95)) %>% as.numeric()
            # small <- cbind(small, c(i - 1, i - 1))
            # # add line spanning these percentiles
            # hc %<>% hc_add_series(data = small,
            #     type = 'line',
            #     marker = list(symbol = "circle",
            #         enabled = FALSE),
            #     color = "black",
            #     name = "whiskers",
            #     linkedTo = "0",
            #     lineWidth = 2)
            #
            # # compute percentiles 25, 50, 75
            # big_left <- quantile(data[[name]], c(0.25, 0.50)) %>% as.numeric()
            # big_right <- quantile(data[[name]], c(0.50, 0.75)) %>% as.numeric()
            #
            # # prepare rectangles to show
            # big_left_rect <- cbind(big_left,
            #     c(i - 1 - delta,  i - 1 - delta),
            #     c(i - 1 + delta, i - 1 + delta))
            # big_left_rect <- rbind(c(big_left[1], i - 1, i - 1),
            #     big_left_rect)
            # big_left_rect <- rbind(big_left_rect,
            #     c(big_left[2], i - 1, i - 1))
            #
            # big_right_rect <- cbind(big_right,
            #     c(i - 1 - delta,  i - 1 - delta),
            #     c(i - 1 + delta, i - 1 + delta))
            # big_right_rect <- rbind(c(big_right[1], i - 1, i - 1),
            #     big_right_rect)
            # big_right_rect <- rbind(big_right_rect,
            #     c(big_right[2], i - 1, i - 1))
            # big_center <- cbind(c(big_right[1], big_right[1]),
            #     c(i - 1,  i - 1))
            #
            # # add rectangles
            # hc %<>% hc_add_series(data = big_right_rect,
            #     type = 'areasplinerange',
            #     marker = list(symbol = 'circle',
            #         enabled = FALSE),
            #     color = "white",
            #     fillOpacity = '100%',
            #     lineColor = "black",
            #     zIndex = 5,
            #     enableMouseTracking = FALSE,
            #     lineWidth = 2) %>%
            #     hc_add_series(data = big_center,
            #         type = 'line',
            #         marker = list(symbol = "circle",
            #             enabled = FALSE),
            #         color = "black",
            #         zIndex = 10,
            #         linkedTo = "0",
            #         lineWidth = 0) %>%
            #     hc_add_series(data = big_left_rect,
            #         type = 'areasplinerange',
            #         marker = list(symbol = 'circle',
            #             enabled = FALSE),
            #         color = "white",
            #         fillOpacity = '100%',
            #         lineColor = "black",
            #         zIndex = 5,
            #         enableMouseTracking = FALSE,
            #         lineWidth = 2) %>%
            #     hc_add_series(data = cbind(quantile(data[[name]], c(0.25, 0.50, 0.75)) %>% as.numeric(),
            #         rep(i - 1, length(data))),
            #         type = 'line',
            #         marker = list(symbol = "circle",
            #             enabled = FALSE),
            #         color = "transparant",
            #         zIndex = 10,
            #         lineWidth = 0)
        }
    }
    hc
}
# plot_violin(processed@meta.data %>%
#         select(nCount_RNA, nFeature_RNA, percent.mt))
