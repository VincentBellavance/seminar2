plot_theme <- function() {
  ggplot2::theme_classic() +
    ggplot2::theme(axis.text = ggplot2::element_text(size = 14),
                   axis.title = ggplot2::element_text(size = 17),
                   plot.title = ggplot2::element_text(size = 20, hjust = 0.5),
                   plot.margin = ggplot2::margin(t = 20, r = 20, b = 20, l = 20, unit = "pt"))
}


ggplot2::ggplot() +
  # plot all taxa trends in green
  ggplot2::geom_line(data = bdi,
                     ggplot2::aes(x = years, y = index),
                     col = "red",
                     lwd = .7) + 
  ggplot2::geom_line(data = hirundo,
                     ggplot2::aes(x = year, y = index),
                     col = "black",
                     lwd = .7) + 
  ggplot2::geom_hline(yintercept = 1,
                      lty = 1,
                      col = "grey20",
                      lwd = .2) +
  ggplot2::labs(y = "Index value", 
                x = "Years", 
                title = "Biodiversity Distribution Index") +
  ggplot2::ylim(0, 1.5) +
  ggplot2::scale_x_continuous(breaks = function(x) seq(ceiling(x[1])+1, floor(x[2]-1), by = 5)) + # round to integer, since the x axis is years 
  plot_theme()
