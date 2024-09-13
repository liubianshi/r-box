box::use(ggplot2[...])
box::use(ggtext[element_markdown])


#' @export
minimal <- function(font = "Lato", background = "grey98", ...) {
  theme_minimal(base_family = font) +
  theme(
    aspect.ratio         = 0.618,
    # Remove title for both x and y axes
    axis.title = element_blank(),
    # Axes labels are grey
    axis.text = element_text(color = "grey40"),
    # The size of the axes labels are different for x and y.
    axis.text.x = element_text(size = 20, margin = margin(t = 5)),
    axis.text.y = element_text(size = 17, margin = margin(r = 5)),
    # Also, the ticks have a very light grey color
    axis.ticks = element_line(color = "grey91", size = .5),
    # The length of the axis ticks is increased.
    axis.ticks.length.x = unit(1.3, "lines"),
    axis.ticks.length.y = unit(.7, "lines"),
    # Remove the grid lines that come with ggplot2 plots by default
    panel.grid = element_blank(),
    # Customize margin values (top, right, bottom, left)
    plot.margin = margin(20, 40, 20, 40),
    # Use a light grey color for the background of both the plot and the panel
    plot.background = element_rect(fill = background, color = background),
    panel.background = element_rect(fill = background, color = background),
    # Customize title appearence
    plot.title = element_text(
      color = "grey10",
      size = 28,
      face = "bold",
      margin = margin(t = 15)
    ),
    # Customize subtitle appearence
    plot.subtitle = element_markdown(
      color = "grey30",
      size = 16,
      lineheight = 1.35,
      margin = margin(t = 15, b = 40)
    ),
    # Title and caption are going to be aligned
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.caption = element_text(
      color = "grey30",
      size = 13,
      lineheight = 1.2,
      hjust = 0,
      margin = margin(t = 40) # Large margin on the top of the caption.
    ),
    # Remove legend
    legend.position = "none"
  )
}

