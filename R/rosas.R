library(dplyr)
library(ggplot2)
library(ggthemes)

set.seed(101)





kaleid_rosas <- function(range = 0.5,
                       density = 1000,
                       nbr = 16,
                       col = sample(c("yellow","orange","red","firebrick"), 16, replace = TRUE)){
  
  data <- generate_segments(x = cos(0),
                      y = sin(0),
                      range = range,
                      density = density) %>%
    mutate(color = col[[1]])
  
  for (i in 1:(nbr-1)){
    data <- dplyr::bind_rows(
      data,
      generate_segments(
        x = cos(i*pi/(nbr/2)),
        y = sin(i*pi/(nbr/2)),
        range = range,
        density = density
      ) %>%
        mutate(color = col[[i]])
    )
  }
  
  data <- filter(data, x1 != x2)
  
  return(data)
}



rosas <- kaleid_rosas(range = 0.8,
                    density = 5000,
                    nbr = 16,
                    col = sample(c("yellow","orange","red","firebrick"), 16, replace = TRUE))


curv = 0.1
size = 0.1
alpha = 0.1
col = rep("yellow",8)
bkg = rgb(0.5,0,0.1)

ggplot(rosas, aes(x = x1, xend = x2, y = y1, yend = y2)) +
  geom_curve(color=rosas$color, alpha = alpha, size = size, curvature = curv) +
  coord_equal() +
  theme(line = element_blank(),
        axis.ticks = element_blank(),
        axis.ticks.length = unit(0, "null"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        text = element_blank(),
        legend.position = "none",
        panel.background = element_rect(fill = bkg),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = bkg),
        strip.text = element_blank()) +
  xlim(-3,3) + ylim(-3,3)



japan <- kaleid_rosas(radius = 1,
                      density = 5000,
                      nbr = 8,
                      curv = 0.1,
                      size = 0.1,
                      alpha = 0.1,
                      col = rep("red",8),
                      bkg = "white") +
  xlim(-5,5) + ylim(-4,4)

ggsave("~/Downloads/rosas_tp.png", rosas, width = 100, height = 100, units = "cm", dpi = "print")
