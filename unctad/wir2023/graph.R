box::use(ggplot2[...])

#' @export
connected <- function(data, title = "", ytitle = "Billion U.S. Dollar",
                      x1 = 1990, x2 = 2022) {
        box::use(ggplot2[...])
    box::use(hrbrthemes[...])
    box::use(ggrepel[...])

    ggplot(data, aes(x = x, y = y)) +
                geom_line(color = "grey") +
        geom_point(shape=21, color="black", fill="#69b3a2", size=2) +
        scale_x_continuous(breaks = seq(x1, x2, 2)) +
        geom_text_repel(data=data[x >= 2017], aes(label=sprintf("%4.1f", y)), size = 2) +
        ggtitle(title) + xlab("") + ylab(ytitle) +
        theme_ipsum(base_family = "LXGW WenKai",
                                        base_size = 8,
                                        axis_text_size = 6,
                    axis_title_size = 8,
                    plot_title_size = 12)
}

