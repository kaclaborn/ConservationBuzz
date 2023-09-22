
# Plot themes

lollipop.plot.theme <- 
  theme(plot.title = element_text(size = 16,
                                  colour = "#303030",
                                  face = "bold"),
        plot.subtitle = element_text(size = 11,
                                     colour = "#303030", 
                                     face = "italic"),
        axis.ticks.x = element_line(colour = "#C0C0C0"),
        axis.ticks.y = element_blank(),
        panel.background = element_rect(fill = "white",
                                        colour = "#909090"),
        panel.border = element_rect(fill = NA,
                                    linewidth = 0.25,
                                    colour = "#C0C0C0"),
        panel.grid.major.x = element_line(colour = "#C0C0C0",
                                          linewidth = 0.5),
        panel.grid.major.y = element_blank(),
        plot.margin = margin(t = 5, r = 20, b = 5, l = 5, unit = "pt"),
        axis.title = element_text(size = 11,
                                  angle = 0,
                                  face = "bold",
                                  colour = "#303030"),
        axis.text = element_text(size = 10,
                                 angle = 0,
                                 colour = "#303030",
                                 lineheight = 0.7))

focalword.plot.theme <- 
  theme(plot.title = element_text(size = 16,
                                  colour = "#303030",
                                  face = "bold"),
        plot.subtitle = element_text(size = 11,
                                     colour = "#303030", 
                                     face = "italic"),
        axis.ticks.x = element_line(colour = "#C0C0C0"),
        axis.ticks.y = element_blank(),
        panel.background = element_rect(fill = "white",
                                        colour = "#909090"),
        panel.border = element_rect(fill = NA,
                                    linewidth = 0.25,
                                    colour = "#C0C0C0"),
        panel.grid.major.x = element_line(colour = "#C0C0C0",
                                          linewidth = 0.5),
        panel.grid.major.y = element_blank(),
        plot.margin = margin(t = 5, r = 20, b = 5, l = 5, unit = "pt"),
        axis.title = element_text(size = 12,
                                  angle = 0,
                                  face = "bold",
                                  colour = "#303030"),
        axis.text = element_text(size = 12,
                                 angle = 0,
                                 colour = "#303030",
                                 lineheight = 0.7))


time.plot.theme <- 
  theme(plot.title = element_text(size = 14,
                                  colour = "#303030",
                                  face = "bold"),
        plot.subtitle = element_text(size = 10,
                                     colour = "#303030", 
                                     face = "italic"),
        axis.ticks.x = element_line(colour = "#C0C0C0"),
        axis.ticks.y = element_blank(),
        panel.background = element_rect(fill = "white",
                                        colour = "#909090"),
        panel.border = element_rect(fill = NA,
                                    linewidth = 0.25,
                                    colour = "#C0C0C0"),
        panel.grid.major.y = element_line(colour = "#C0C0C0",
                                          linewidth = 0.35),
        panel.grid.major.x = element_blank(),
        plot.margin = margin(t = 5, r = 20, b = 5, l = 5, unit = "pt"),
        axis.title = element_text(size = 10,
                                  angle = 0,
                                  face = "bold",
                                  colour = "#303030"),
        axis.text = element_text(size = 10,
                                 angle = 0,
                                 colour = "#303030",
                                 lineheight = 0.7))

example.buzzword.plot.theme <- 
  theme(axis.ticks.x = element_line(colour = "#C0C0C0"),
        axis.ticks.y = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major.x = element_line(colour = "#C0C0C0",
                                          linewidth = 0.35),
        panel.grid.major.y = element_blank(),
        plot.margin = margin(t = 5, r = 20, b = 5, l = 5, unit = "pt"),
        axis.line.x = element_line(colour = "#909090",
                                   linewidth = 0.35),
        axis.ticks.x.top = element_blank(),
        axis.title = element_text(size = 13,
                                  angle = 0,
                                  face = "bold",
                                  colour = "#303030"),
        axis.text.y = element_text(size = 15,
                                 angle = 0,
                                 colour = "#303030",
                                 lineheight = 0.7),
        axis.text.x = element_text(size = 11,
                                   angle = 0,
                                   colour = "#303030",
                                   lineheight = 1))

# Legend guides
lollipop.legend.guide <- guides(size = guide_legend(order = 1,
                                                    keyheight = unit(0.5, "cm")),
                                color = guide_legend(order = 2))

get_legend <- function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

# Fill colors
fillcols.4categories <- c("#332288", "#6699CC", "#44AA99", "#CC6677")
fillcols.6categories <- c("#332288", "#6699CC", "#44AA99", "#DDCC77", "#CC6677", "#882255")
fillcols.9categories <- c("#332288", "#6699CC", "#88CCEE", "#44AA99", "#117733", 
                          "#DDCC77", "#661100", "#CC6677", "#882255")
