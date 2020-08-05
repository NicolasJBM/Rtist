initialize_polygon <- function(edges = 5) {
  tibble(
    x    = accumulate(1:(edges-1), ~.x+cos(.y*2*pi/edges), .init = 0),
    y    = accumulate(1:(edges-1), ~.x+sin(.y*2*pi/edges), .init = 0),
    xend = accumulate(2:edges,     ~.x+cos(.y*2*pi/edges), .init = cos(2*pi/edges)),
    yend = accumulate(2:edges,     ~.x+sin(.y*2*pi/edges), .init = sin(2*pi/edges)))
}

add_inner_segments <- function(polygon, position, angle, iter, FUN) {
  polygon %>% mutate(
    angles=atan2(yend-y, xend-x) + angle,
    radius = FUN(iter),
    x=position*x+(1-position)*xend,
    y=position*y+(1-position)*yend,
    xend=x+radius*cos(angles),
    yend=y+radius*sin(angles)) %>% 
    select(x, y, xend, yend)
}

connect_points <- function(poligon) {
  poligon %>% mutate(
    x=xend,
    y=yend,
    xend=lead(x, default=first(x)),
    yend=lead(y, default=first(y))) %>% 
    select(x, y, xend, yend)
}




edges <- 5   # Number of edges of the original polygon
niter <- 150 # Number of iterations
step  <- 7  # No of times to draw mid-segments before connect ending points

position <- 0.22  # Weight to calculate the point on the middle of each edge
angle <- 5 # angle of mid-segment with the edge
ratio_f <- function (x) { 1/x }  # To calculate the longitude of mid-segments

alph  <- 0.3 # transparency of curves in geom_curve
curv <- -0   # Curvature of curves
line_color <- "white" # Color of curves in geom_curve
back_color <- "black" # Background of the ggplot





accumulate(.f = function(old, y) {
  if (y%%step!=0) add_inner_segments(old, position, angle, y, FUN = ratio_f) else connect_points(old)
},
1:niter,
.init=initialize_polygon(edges)) %>% 
  bind_rows() -> df


ggplot(df)+
  geom_curve(aes(x=x, y=y, xend=xend, yend=yend),
             curvature = curv,
             color=line_color,
             alpha=alph)+
  coord_equal()+
  theme(legend.position  = "none",
        panel.background = element_rect(fill=back_color),
        plot.background  = element_rect(fill=back_color),
        axis.ticks       = element_blank(),
        panel.grid       = element_blank(),
        axis.title       = element_blank(),
        axis.text        = element_blank())
