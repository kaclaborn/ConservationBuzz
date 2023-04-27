
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
                                    size = 0.25,
                                    colour = "#C0C0C0"),
        panel.grid.major.x = element_line(colour = "#C0C0C0",
                                          size = 0.5),
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


lollipop.legend.guide <- guides(size = guide_legend(order = 1,
                                                    keyheight = unit(0.5, "cm")),
                                color = guide_legend(order = 2))
