box::use(ggplot2[...])
box::use(magrittr[`%>%`, `%<>%`, `%T>%`])
box::use(stringr[glue = str_glue])
box::use(data.table[setDT, as.data.table, data.table, setnames, melt])
box::use(lbs)
box::use(ggrepel[geom_text_repel])
box::use(graph/ggplot2/theme)

# 定义生成时间趋势图的函数
generate_time_trend_plot <- function(data, ylist, ...) {
  stopifnot(ncol(data) >= 2)
  o <- list(...)
  varnames <- names(data)

  data_long <- melt(
	  as.data.table(data),
		id.vars = colnames(data)[1],
		variable.name = "Variable",
		value.name = "Value"
	)
  setnames(data_long, varnames[1], "date")
  min <- min(data[[1]], na.rm = FALSE)
  max <- max(data[[1]], na.rm = FALSE)
  data_long[, name_lab := ifelse(date == ..max, as.character(Variable), NA_character_)]
  # if ((max - min) %% 2 == 1) max = max + 1

  or <- lbs::ifthen
  background <- or(o$background, "white")
  title  <- or(o$title, "")
  xtitle <- or(o$xtitle, "")
  ytitle <- or(o$ytitle, "")

  p <-  ggplot(data_long, aes(x = date, y = Value, color = Variable)) +
    geom_vline(
      xintercept = seq(min, max),
      color = "grey91",
      size = .6
    ) +
    geom_segment(
      data = data.table(y = ylist, x1 = min, x2 = max),
      aes(x = x1, xend = x2, y = y, yend = y),
      inherit.aes = FALSE,
      color = "grey91",
      size = .6
    ) +
    geom_line() +
    geom_point(shape = 21, size = 6, color = background, fill = background) +
    geom_point(aes(fill = Variable), shape = 21, size = 4) +
    geom_text_repel(
      aes(color = Variable, label = name_lab),
      family = "Lato",
      fontface = "bold",
      size = 8,
      direction = "y",
      xlim = c(max + 0.8, NA),
      hjust = 0,
      segment.size = .7,
      segment.alpha = .5,
      segment.linetype = "dotted",
      box.padding = .4,
      segment.curvature = -0.1,
      segment.ncp = 3,
      segment.angle = 20
    ) +
    coord_cartesian(
      clip = "off",
      ylim = c(min(ylist), max(ylist)) 
    ) +
    scale_x_continuous(
      expand = c(0, 0),
      limits = c(min, max + 1.5),
      breaks = seq(min, max, by = 2)
    ) +
    labs(x = xtitle, y = title, title = title) +
    theme$minimal(background = background)
}

# box::use(data/trade/uncomtrade)
# box::use(graph/ggplot2/theme)
#
# box::use(graph/ggplot2/theme)
# box::reload(theme)
# box
#
# box::reload(theme)
# box::reload(uncomtrade)
#
# re <- uncomtrade$get_anual_trade_data("CHN", 2008, 2023, partner = "USA")
# p <- re[, .(year, export = Export / 10^9, import = Import / 10^9)] %>%
#   generate_time_trend_plot(ylist = seq(0, 600, by = 100))
# print(p)
#
#
# re <- uncomtrade$get_anual_trade_data("CHN", 2008, 2023)
# p <- re[, .(year, export = Export / 10^9, import = Import / 10^9)] %>%
#   generate_time_trend_plot(ylist = seq(1000, 4000, by = 1000))
# print(p)

