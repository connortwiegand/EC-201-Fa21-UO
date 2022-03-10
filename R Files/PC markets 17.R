if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, dplyr, magrittr, ggplot2)
# Must install through github:
# Even if plotting with ggplot2, contains curve_intersect function
library(econocharts)
setwd("~/School/EC 201 Fa21")

# Colors
red_pink <- "#e64173"
met_slate <- "#272822" # metropolis font color 
purple <- "#9370DB"
green <- "#007935"
light_green <- "#7DBA97"
orange <- "#FD5F00"
turquoise <- "#44C1C4"
red <- "#b92e34"

#Theme stuff
#p_load(hrbrthemes, ggthemr, ggthemes)
theme_sd <- theme_grey() + theme(
  axis.line = element_line(color = met_slate),
  #panel.grid = element_blank(),
  #rect = element_blank(),
  strip.text = element_blank(),
  text = element_text(color = met_slate),
  axis.title.x = element_text(hjust = 1, size = 13),
  axis.title.y = element_text(hjust = 1, angle = 0, size = 13),
  #axis.ticks = element_blank()
  plot.title = element_text(hjust=0.55),
  plot.margin = margin(t=2, r=13, b=2, l=2)
)

### Later:

theme_market <- theme_bw() + theme(
  axis.line = element_line(color = met_slate),
  panel.grid = element_blank(),
  rect = element_blank(),
  strip.text = element_blank(),
  text = element_text(color = "black"),
  axis.title.x = element_text(hjust = 1, size = 15),
  axis.title.y = element_text(hjust = 1, angle = 0, size = 15),
  #axis.ticks = element_blank()
  plot.title = element_text(hjust=0.55),
  plot.margin = margin(t=2, r=13, b=2, l=2)
)

x <- seq(0, 8, 0.1)
atc <- function(x) (18 + 12*x - 2*x^2 + (2/3)*x^3)/x
avc <-  function(x) (12*x - 2*x^2 + (2/3)*x^3)/x
marginal_cost <- function(x) 12 - 4*x + 2*x^2

mr1 <- function(x) 30
q1 <- uniroot(function(x) mr1(x) - marginal_cost(x), range(x))$root
p1 <- mr1(1)
cost1 <- atc(q1)

mr2 <- function(x) 18
q2 <- uniroot(function(x) mr2(x) - marginal_cost(x), range(x))$root
p2 <- mr2(1)
cost2 <- atc(q2)

mr3 <- function(x) 13
q3 <- uniroot(function(x) mr3(x) - marginal_cost(x), range(x))$root
p3 <- mr3(1)
cost3 <- atc(q3)


#z <- seq(0, q, 0.001)
p_max_1 <- ggplot() +
  scale_x_continuous(limits = c(0, 7.5), expand=c(0,0), breaks = c(q3,q2,q1), labels = c(4,6,8)) +
  scale_y_continuous(limits = c(0, 37.5), expand=c(0,0), breaks = c(p1,p2,p3), labels = c(12,7,5)) +
  theme_market +
  labs(x = "Q", y = "$", title = "Three Different Prices in a PC Market") +
  geom_line(aes(x=x, y=marginal_cost(x)), size = 1, color = green) +
  annotate("text", label = "MC", x = 4.9, y = 35, color = green, size = 6) +
  geom_point(aes(x = q1, y = p1), color = "black", size = 2) +
  geom_segment(aes(x = q1, xend = q1, y = 0, yend = p1), color = "black", 
               linetype = "dashed", size = 1) +
  geom_segment(aes(x = 0, xend = q1, y = p1, yend = p1), color = red, size = 1) +
  annotate("text", label = "P[1]", x = 4.45, y = p1 - 1, color = red, size = 5.5, parse = T) +
  
  geom_point(aes(x = q2, y = p2), color = "black", size = 2) +
  geom_segment(aes(x = q2, xend = q2, y = 0, yend = p2), color = "black", 
               linetype = "dashed", size = 1) +
  geom_segment(aes(x = 0, xend = q2, y = p2, yend = p2), color = red, size = 1) +
  annotate("text", label = "P[2]", x = 3.2, y = p2 - 1, color = red, size = 5.5, parse = T) +
  
  geom_point(aes(x = q3, y = p3), color = "black", size = 2) +
  geom_segment(aes(x = q3, xend = q3, y = 0, yend = p3), color = "black", 
               linetype = "dashed", size = 1) +
  geom_segment(aes(x = 0, xend = q3, y = p3, yend = p3), color = red,size = 1) +
  annotate("text", label = "P[3]", x = 2.5, y = p3 - 1, color = red, size = 5.5, parse = T)
p_max_1

ggsave("3 prices pc.png", device = "png")




x <- seq(0, 8, 0.1)
atc <- function(x) (18 + 12*x - 2*x^2 + (2/3)*x^3)/x
avc <-  function(x) (12*x - 2*x^2 + (2/3)*x^3)/x
marginal_cost <- function(x) 12 - 4*x + 2*x^2
marginal_revenue <- function(x) 13
q <- uniroot(function(x) marginal_revenue(x) - marginal_cost(x), range(x))$root
p <- marginal_revenue(1)
cost <- atc(q)
z <- seq(0, q, 0.001)

p_max_3 <- ggplot() +
  scale_x_continuous(limits = c(0, 7.5), expand=c(0,0), breaks = c(q), labels = c(4)) +
  scale_y_continuous(limits = c(0, 37.5), expand=c(0,0), breaks = c(cost, p), labels = c(8, 5)) +
  theme_market +
  labs(x = "Q", y = "$") +
  geom_line(aes(x=x, y=atc(x)), size = 1, color = purple) + 
  annotate("text", label = "ATC", x = 6.5, y = 35, color = purple, size = 6) +
  geom_line(aes(x=x, y=avc(x)), size = 1, color = orange) +
  annotate("text", label = "AVC", x = 6.5, y = 23, color = orange, size = 6) +
  geom_line(aes(x=x, y=marginal_cost(x)), size = 1, color = green) +
  annotate("text", label = "MC", x = 1, y = 8, color = green, size = 6) +
  geom_point(aes(x = q, y = p), color = "black", size = 2) +
  geom_segment(aes(x = q, xend = q, y = 0, yend = p), color = "black", 
               linetype = "dashed", size = 1) +
  geom_line(aes(x=x, y=marginal_revenue(x)), size = 1, color = red, type=2) +
  annotate("text", label = "P=MR", x = 6.5, y = p - 2, color = red, size = 6)
p_max_3

ggsave("prod dec 3.png", device = "png")

p_max_3 +
  #stat_function(aes(x), fun = marginal_revenue, color = green, size = 1) +
  geom_point(aes(x = q, y = p), color = "grey30", size = 2) +
  geom_segment(aes(x = q, xend = q, y = 0, yend = p), color = "grey30", 
               linetype = "dashed", size = 1) +
  geom_point(aes(x = q, y = cost), color = met_slate, size = 2) +
  geom_segment(aes(x = 0, xend = q, y = cost, yend = cost), color = met_slate, linetype = "dashed", size = 1)  +
  geom_ribbon(aes(x = z, ymin = p, ymax = cost), fill = red_pink, alpha = 0.2, linetype = "blank") +
  annotate("text", label = "Loss", x = q/2, y = (p + cost)/2, color = red_pink, size = 6) +
  geom_segment(aes(x = q, xend = q, y = 0, yend = cost), color = "black", 
               linetype = "dashed", size = 1)

ggsave("shutdown question.png", device = "png")



x <- seq(0.1,32,0.1)

xlim <- max(x)
ylim <- 36+1
tc <- function(x) (1/8)*(x^3)-4*(x^2)+48*x+96
vc <- function(x) tc(x)-96

atc <- function(x) tc(x)/x
avc <- function(x) vc(x)/x

mc <- function(x) (3/8)*(x^2)-8*x+48

p <- 16 + 4

shut1 <- ggplot() +
  scale_x_continuous(limits = c(0, xlim), expand=c(0,0), breaks = seq(0,xlim,4)) +
  scale_y_continuous(limits = c(0, ylim), expand=c(0,0), breaks = seq(0,ylim,4)) +
  theme_market +
  labs(x = "Q", y = "$") +
  # geom_line(aes(x=x, y=atc(x)), size = 1, color = purple) + 
  # annotate("text", label = "ATC", x = 6.5, y = 35, color = purple, size = 6) +
  geom_line(aes(x=x, y=avc(x)), size = 1, color = orange) +
  annotate("text", label = "AVC", x = 30, y = 34, color = orange, size = 6) +
  # geom_line(aes(x=x, y=marginal_cost(x)), size = 1, color = green) +
  # annotate("text", label = "MC", x = 1, y = 8, color = green, size = 6) +
  # geom_point(aes(x = q, y = p), color = "black", size = 2) +
  # geom_segment(aes(x = q, xend = q, y = 0, yend = p), color = "black", 
  #              linetype = "dashed", size = 1) +
  geom_line(aes(x=x, y=p), size = 1, color = red) +
  annotate("text", label = "P", x = 30, y = p - 2, color = red, size = 6)
shut1

ggsave("bad shut.png", device = "png")

p <- min(avc(x))
ggplot() +
  scale_x_continuous(limits = c(0, xlim), expand=c(0,0), breaks = seq(0,xlim,4)) +
  scale_y_continuous(limits = c(0, ylim), expand=c(0,0), breaks = seq(0,ylim,4)) +
  theme_market +
  labs(x = "Q", y = "$") +
  # geom_line(aes(x=x, y=atc(x)), size = 1, color = purple) + 
  # annotate("text", label = "ATC", x = 6.5, y = 35, color = purple, size = 6) +
  geom_line(aes(x=x, y=avc(x)), size = 1, color = orange) +
  annotate("text", label = "AVC", x = 30, y = 34, color = orange, size = 6) +
  # geom_line(aes(x=x, y=marginal_cost(x)), size = 1, color = green) +
  # annotate("text", label = "MC", x = 1, y = 8, color = green, size = 6) +
  # geom_point(aes(x = q, y = p), color = "black", size = 2) +
  # geom_segment(aes(x = q, xend = q, y = 0, yend = p), color = "black", 
  #              linetype = "dashed", size = 1) +
  geom_line(aes(x=x, y=p), size = 1, color = red) +
  annotate("text", label = "P", x = 30, y = p - 2, color = red, size = 6) +
  annotate("point", x = 16, y= 16, size = 3.5) +
  annotate("segment", x = 16, xend =16, y= 0, yend=p, linetype = 2, size = 1)

ggsave("good shut.png", device = "png")

z <- seq(6,32,0.1)

p <- 28
q <- uniroot(function(z) p - mc(z), range(z))$root

ggplot() +
  scale_x_continuous(limits = c(0, xlim), expand=c(0,0), breaks = seq(0,xlim,4)) +
  scale_y_continuous(limits = c(0, ylim), expand=c(0,0), breaks = seq(0,ylim,4)) +
  theme_market +
  labs(x = "Q", y = "$") +
  geom_line(aes(x=x, y=atc(x)), size = 1, color = purple) +
  annotate("text", label = "ATC", x = 11, y = 34, color = purple, size = 6) +
  geom_line(aes(x=x, y=avc(x)), size = 1, color = orange) +
  annotate("text", label = "AVC", x = 30, y = 34, color = orange, size = 6) +
  geom_line(aes(x=z, y=mc(z)), size = 1, color = green) +
  annotate("text", label = "MC", x = 21, y = 34, color = green, size = 6) +
  geom_line(aes(x=x, y=p), size = 1, color = red) +
  annotate("text", label = "P", x = 30, y = p - 2, color = red, size = 6) +
  annotate("point", x = q, y= p, size = 3.5) +
  annotate("segment", x = q, xend = q, y = 0, yend = p, linetype = 2, size = 1)

ggsave("shut ex1.png", device = "png")

p <- 20
q <- uniroot(function(z) p - mc(z), range(z))$root

ggplot() +
  scale_x_continuous(limits = c(0, xlim), expand=c(0,0), breaks = seq(0,xlim,4)) +
  scale_y_continuous(limits = c(0, ylim), expand=c(0,0), breaks = seq(0,ylim,4)) +
  theme_market +
  labs(x = "Q", y = "$") +
  geom_line(aes(x=x, y=atc(x)), size = 1, color = purple) +
  annotate("text", label = "ATC", x = 11, y = 34, color = purple, size = 6) +
  geom_line(aes(x=x, y=avc(x)), size = 1, color = orange) +
  annotate("text", label = "AVC", x = 30, y = 34, color = orange, size = 6) +
  geom_line(aes(x=z, y=mc(z)), size = 1, color = green) +
  annotate("text", label = "MC", x = 21, y = 34, color = green, size = 6) +
  geom_line(aes(x=x, y=p), size = 1, color = red) +
  annotate("text", label = "P", x = 30, y = p - 2, color = red, size = 6) +
  annotate("point", x = q, y= p, size = 3.5) +
  annotate("segment", x = q, xend = q, y = 0, yend = p, linetype = 2, size = 1)

ggsave("shut ex2.png", device = "png")


p <- 16
q <- uniroot(function(z) p - mc(z), range(z))$root

ggplot() +
  scale_x_continuous(limits = c(0, xlim), expand=c(0,0), breaks = seq(0,xlim,4)) +
  scale_y_continuous(limits = c(0, ylim), expand=c(0,0), breaks = seq(0,ylim,4)) +
  theme_market +
  labs(x = "Q", y = "$") +
  geom_line(aes(x=x, y=atc(x)), size = 1, color = purple) +
  annotate("text", label = "ATC", x = 11, y = 34, color = purple, size = 6) +
  geom_line(aes(x=x, y=avc(x)), size = 1, color = orange) +
  annotate("text", label = "AVC", x = 30, y = 34, color = orange, size = 6) +
  geom_line(aes(x=z, y=mc(z)), size = 1, color = green) +
  annotate("text", label = "MC", x = 21, y = 34, color = green, size = 6) +
  geom_line(aes(x=x, y=p), size = 1, color = red) +
  annotate("text", label = "P", x = 30, y = p - 2, color = red, size = 6) +
  annotate("point", x = q, y= p, size = 3.5) +
  annotate("segment", x = q, xend = q, y = 0, yend = p, linetype = 2, size = 1)

ggsave("shut ex3.png", device = "png")

p <- 14
q <- uniroot(function(z) p - mc(z), range(z))$root

ggplot() +
  scale_x_continuous(limits = c(0, xlim), expand=c(0,0), breaks = seq(0,xlim,4)) +
  scale_y_continuous(limits = c(0, ylim), expand=c(0,0), breaks = seq(0,ylim,4)) +
  theme_market +
  labs(x = "Q", y = "$") +
  geom_line(aes(x=x, y=atc(x)), size = 1, color = purple) +
  annotate("text", label = "ATC", x = 11, y = 34, color = purple, size = 6) +
  geom_line(aes(x=x, y=avc(x)), size = 1, color = orange) +
  annotate("text", label = "AVC", x = 30, y = 34, color = orange, size = 6) +
  geom_line(aes(x=z, y=mc(z)), size = 1, color = green) +
  annotate("text", label = "MC", x = 21, y = 34, color = green, size = 6) +
  geom_line(aes(x=x, y=p), size = 1, color = red) +
  annotate("text", label = "P", x = 30, y = p - 2, color = red, size = 6) +
  annotate("point", x = q, y= p, size = 3.5) +
  annotate("segment", x = q, xend = q, y = 0, yend = p, linetype = 2, size = 1)

ggsave("shut ex4.png", device = "png")  

be <- uniroot(function(x) atc(x) - mc(x), range(x))$root
atc_min <- atc(be)

ggplot() +
  scale_x_continuous(limits = c(0, xlim), expand=c(0,0), breaks = seq(0,xlim,4)) +
  scale_y_continuous(limits = c(0, ylim), expand=c(0,0), breaks = seq(0,ylim,4)) +
  theme_market +
  labs(x = "Q", y = "$") +
  geom_line(aes(x=x, y=atc(x)), size = 1, color = purple) +
  annotate("text", label = "ATC", x = 11, y = 34, color = purple, size = 6) +
  geom_line(aes(x=x, y=avc(x)), size = 1, color = orange) +
  annotate("text", label = "AVC", x = 30, y = 34, color = orange, size = 6) +
  geom_line(aes(x=z, y=mc(z)), size = 1, color = green) +
  annotate("text", label = "MC", x = 21, y = 34, color = green, size = 6) +
  
  geom_ribbon(aes(x = x, ymin = 0, ymax = 16), fill = red, alpha = 0.2) +
  annotate("label", label = "Shut Down", x = 28, y = 14, color = red, size = 4.5) +
  
  geom_ribbon(aes(x = x, ymin = 16, ymax = atc(be)), fill = orange, alpha = 0.2,
              color = "black", size = 1, linetype = 2) +
  annotate("label", label = "Operate at\nLoss", x = 28, y = 19, color = orange, size = 4.5) +
  
  geom_ribbon(aes(x = x, ymin = atc(be), ymax = ylim), fill = green, alpha = 0.2) +
  annotate("label", label = "Positive Profit", x = 28, y = 24, color = green, size = 4.5) +
  
  annotate("point", x = 16, y= 16, size = 3) +
  annotate("point", x = be, y=atc_min, size = 3) 
  
ggsave("full shut.png", device = "png")  


### Fun fact
x <- seq(0, 24, 0.1)

lin_mc <- function(x) 2*x+6
lin_avc <- function(x) x+6


xlim <- 24
y_lim <- 24

ggplot() +
  scale_x_continuous(limits = c(0, xlim), expand=c(0,0), breaks = seq(0,xlim,3)) +
  scale_y_continuous(limits = c(0, ylim), expand=c(0,0), breaks = seq(0,ylim,3)) +
  theme_market +
  labs(x = "Q", y = "$") +
  geom_line(aes(x=x, y=lin_avc(x)), size = 1, color = orange) +
  annotate("text", label = "AVC", x = 22, y = 25, color = orange, size = 6) +
  geom_line(aes(x=x, y=lin_mc(x)), size = 1, color = green) +
  annotate("text", label = "MC", x = 16, y = 34, color = green, size = 6)

ggsave("linear shut.png", device = "png")  



x <- seq(0.1,32,0.1)
z <- seq(6,32,0.1)

xlim <- max(x)
ylim <- 36+1
tc <- function(x) (1/8)*(x^3)-4*(x^2)+48*x+96
vc <- function(x) tc(x)-96

atc <- function(x) tc(x)/x
avc <- function(x) vc(x)/x

mc <- function(x) (3/8)*(x^2)-8*x+48
be <- uniroot(function(x) atc(x) - mc(x), range(x))$root
atc_min <- atc(be)

ggplot() +
  scale_x_continuous(limits = c(0, xlim), expand=c(0,0), breaks = seq(0,xlim,4)) +
  scale_y_continuous(limits = c(0, ylim), expand=c(0,0), breaks = seq(0,ylim,4)) +
  theme_market +
  labs(x = "Q", y = "$") +
  geom_line(aes(x=x, y=atc(x)), size = 1, color = purple) +
  annotate("text", label = "LRATC", x = 12, y = 34, color = purple, size = 6) +
  # geom_line(aes(x=x, y=avc(x)), size = 1, color = orange) +
  # annotate("text", label = "AVC", x = 30, y = 34, color = orange, size = 6) +
  geom_line(aes(x=z, y=mc(z)), size = 1, color = green) +
  annotate("text", label = "MC", x = 21, y = 34, color = green, size = 6) +
  
  geom_ribbon(aes(x = x, ymin = 0, ymax = atc(be)), fill = red, alpha = 0.2) +
  annotate("label", label = "Shut Down", x = 28, y = 14, color = red, size = 4.5) +
  
  # geom_ribbon(aes(x = x, ymin = 16, ymax = atc(be)), fill = orange, alpha = 0.2,
  #             color = "black", size = 1, linetype = 2) +
  # annotate("label", label = "Operate at\nLoss", x = 28, y = 19, color = orange, size = 4.5) +
  
  geom_ribbon(aes(x = x, ymin = atc(be), ymax = ylim), fill = green, alpha = 0.2,
              color = "black", size = 1, linetype = 2, outline.type = "lower") +
  annotate("label", label = "Positive Profit", x = 28, y = 24, color = green, size = 4.5) +
  
  annotate("point", x = be, y=atc_min, size = 3) +
  annotate("text", label = "Zero Profit\nPoint", x = 21, y = 17, color = "grey30", size = 4.25) +
  annotate("segment", x = 19, xend = 17.75, y =19, yend = 21,
           arrow = arrow(length = unit(0.4, "lines"), ends = "last"), 
           colour = "grey50", size = 1)

ggsave("lr shut.png", device = "png")  


###Falling prices

x <- seq(0.1,32,0.1)
z <- seq(6,32,0.1)

xlim <- max(x)
ylim <- 36+1
tc <- function(x) (1/8)*(x^3)-4*(x^2)+48*x+96
vc <- function(x) tc(x)-96

atc <- function(x) tc(x)/x
avc <- function(x) vc(x)/x

mc <- function(x) (3/8)*(x^2)-8*x+48

p1 <- 32
q1 <- uniroot(function(z) p1 - mc(z), range(z))$root

p2 <- 26
q2 <- uniroot(function(z) p2 - mc(z), range(z))$root

be <- uniroot(function(x) atc(x) - mc(x), range(x))$root
atc_min <- atc(be)

ggplot() +
  scale_x_continuous(limits = c(0, xlim), expand=c(0,0), breaks = seq(0,xlim,4)) +
  scale_y_continuous(limits = c(0, ylim), expand=c(0,0), breaks = seq(0,ylim,4)) +
  theme_market +
  labs(x = "Q", y = "$") +
  geom_line(aes(x=x, y=atc(x)), size = 1, color = purple) +
  annotate("text", label = "ATC", x = 12, y = 34, color = purple, size = 6) +
  geom_line(aes(x=x, y=avc(x)), size = 1, color = orange) +
  annotate("text", label = "AVC", x = 30, y = 34, color = orange, size = 6) +
  geom_line(aes(x=z, y=mc(z)), size = 1, color = green) +
  annotate("text", label = "MC", x = 21, y = 34, color = green, size = 6) +
  
  annotate("point", x = q1, y=mc(q1), size = 3) +
  annotate("segment", x = q1, xend = q1, y = 0, yend = p1, linetype = 2, size = 1) +
  geom_ribbon(aes(x = 0:q1, ymin = atc(q1), ymax = p1), fill = green, alpha = 0.05, 
              color = "grey40", linetype = "dashed", size = 1) +
  annotate("text", label = "Profit[Old]", x = q1/2, y = (p1 + atc(q1))/2 - 0.5, 
           color = green, size = 5, parse = T) 

ggsave("old profit.png", device = "png")
  

ggplot() +
  scale_x_continuous(limits = c(0, xlim), expand=c(0,0), breaks = seq(0,xlim,4)) +
  scale_y_continuous(limits = c(0, ylim), expand=c(0,0), breaks = seq(0,ylim,4)) +
  theme_market +
  labs(x = "Q", y = "$") +
  geom_line(aes(x=x, y=atc(x)), size = 1, color = purple) +
  annotate("text", label = "ATC", x = 12, y = 34, color = purple, size = 6) +
  geom_line(aes(x=x, y=avc(x)), size = 1, color = orange) +
  annotate("text", label = "AVC", x = 30, y = 34, color = orange, size = 6) +
  geom_line(aes(x=z, y=mc(z)), size = 1, color = green) +
  annotate("text", label = "MC", x = 21, y = 34, color = green, size = 6) +
  annotate("point", x = q2, y=mc(q2), size = 3) +
  annotate("segment", x = q2, xend = q2, y = 0, yend = p2, linetype = 2, size = 1) +
  geom_ribbon(aes(x = 0:q2, ymin = atc(q2), ymax = p2), fill = green, alpha = 0.2, 
              color = "black", linetype = "dashed", size = 1) +
  annotate("text", label = "Profit[New]", x = q2/2, y = (p2 + atc(q2))/2, 
           color = green, size = 5, parse = T)

ggsave("new profit.png", device = "png")



x <- seq(0.1,32,0.1)
z <- seq(6,32,0.1)

xlim <- max(x)
ylim <- 36+1
tc <- function(x) (1/8)*(x^3)-4*(x^2)+48*x+96
vc <- function(x) tc(x)-96

atc <- function(x) tc(x)/x
avc <- function(x) vc(x)/x

mc <- function(x) (3/8)*(x^2)-8*x+48

shutq <- 16
shutp <- 16
be <- uniroot(function(x) atc(x) - mc(x), range(x))$root
atc_min <- atc(be)

ggplot() +
  scale_x_continuous(limits = c(0, xlim), expand=c(0,0), breaks = seq(0,xlim,4)) +
  scale_y_continuous(limits = c(0, ylim), expand=c(0,0), breaks = seq(0,ylim,4)) +
  theme_market +
  labs(x = "Q", y = "$") +
  geom_line(aes(x=x, y=atc(x)), size = 1, color = purple) +
  annotate("text", label = "ATC", x = 12, y = 34, color = purple, size = 6) +
  geom_line(aes(x=x, y=avc(x)), size = 1, color = orange) +
  annotate("text", label = "AVC", x = 30, y = 34, color = orange, size = 6) +
  geom_line(aes(x=z, y=mc(z)), size = 1, color = green) +
  annotate("text", label = "MC", x = 21, y = 34, color = green, size = 6) +
  annotate("point", x = shutq, y=shutp, size = 3) +
  annotate("segment", x = shutq, xend = shutq, y = 0, yend = shutp, linetype = 2, size = 1)

ggsave("mc supply 1.png", device = "png")

z0 <- seq(6,shutq,0.1)
z1 <- seq(shutq,32,0.1)

ggplot() +
  scale_x_continuous(limits = c(0, xlim), expand=c(0,0), breaks = seq(0,xlim,4)) +
  scale_y_continuous(limits = c(0, ylim), expand=c(0,0), breaks = seq(0,ylim,4)) +
  theme_market +
  labs(x = "Q", y = "$") +
  geom_line(aes(x=x, y=atc(x)), size = 1, color = purple, alpha = 0.35) +
  annotate("text", label = "ATC", x = 12, y = 34, color = purple, size = 6, alpha = 0.35) +
  geom_line(aes(x=x, y=avc(x)), size = 1, color = orange, alpha = 0.35) +
  annotate("text", label = "AVC", x = 30, y = 34, color = orange, size = 6, alpha = 0.35) +
  
  geom_line(aes(x=z0, y=mc(z0)), size = 1, color = green, alpha = 0.35) +
  annotate("text", label = "MC", x = 21, y = 34, color = green, size = 6, alpha = 0.35) +
  
  geom_line(aes(x=z1, y=mc(z1)), size = 1, color = green) +
  annotate("text", label = "MC", x = 21, y = 34, color = green, size = 6) +
  
  annotate("point", x = shutq, y=shutp, size = 3) +
  annotate("segment", x = shutq, xend = shutq, y = 0, yend = shutp, linetype = 2, size = 1)

ggsave("mc supply 2.png", device = "png")
  
  
ggplot() +
  scale_x_continuous(limits = c(0, xlim), expand=c(0,0), breaks = seq(0,xlim,4)) +
  scale_y_continuous(limits = c(0, ylim), expand=c(0,0), breaks = seq(0,ylim,4)) +
  theme_market +
  labs(x = "Q", y = "$") +
  geom_line(aes(x=x, y=atc(x)), size = 1, color = purple, alpha = 0.35) +
  annotate("text", label = "ATC", x = 12, y = 34, color = purple, size = 6, alpha = 0.35) +
  geom_line(aes(x=x, y=avc(x)), size = 1, color = orange, alpha = 0.35) +
  annotate("text", label = "AVC", x = 30, y = 34, color = orange, size = 6, alpha = 0.35) +
  
  geom_line(aes(x=z0, y=mc(z0)), size = 1, color = green, alpha = 0.35) +
  
  geom_line(aes(x=z1, y=mc(z1)), size = 1.5, color = green) +
  
  annotate("segment", x = 0.1, xend = 0.1, y = 0, yend = shutp, 
           size = 1.5, color = green) +
  annotate("text", label = "Supply", x = 22, y = 34, color = green, size = 6) +
  
  annotate("point", x = shutq, y=shutp, size = 3) +
  annotate("segment", x = shutq, xend = shutq, y = 0, yend = shutp, linetype = 2, size = 1)

ggsave("mc supply 3.png", device = "png")


ggplot() +
  scale_x_continuous(limits = c(0, xlim), expand=c(0,0), breaks = seq(0,xlim,4)) +
  scale_y_continuous(limits = c(0, ylim), expand=c(0,0), breaks = seq(0,ylim,4)) +
  theme_market +
  labs(x = "Q", y = "P") +
  geom_line(aes(x=z1, y=mc(z1)), size = 1.5, color = purple) +
  annotate("segment", x = 0.1, xend = 0.1, y = 0, yend = shutp, 
           size = 1.5, color = purple) +
  annotate("text", label = "S", x = 20.5, y = 34, color = purple, size = 6) +
  annotate("segment", x = 0, xend = shutq, y = shutp, yend = shutp, 
           linetype = 2, size = 1, color = purple)

ggsave("mc supply 4.png", device = "png")



supply <- function(x) (3/8)*((x/100)^2)-8*(x/100)+48

z2 <- 1600:3200

zlim <- 3200

ggplot() +
  scale_x_continuous(limits = c(0, zlim), expand=c(0,0), breaks = seq(0,zlim,400)) +
  scale_y_continuous(limits = c(0, ylim), expand=c(0,0), breaks = seq(0,ylim,4)) +
  theme_market +
  labs(x = "Q", y = "P") +
  geom_line(aes(x=z2, y=supply(z2)), size = 1.5, color = purple) +
  annotate("segment", x = 10, xend = 10, y = 0, yend = shutp, 
           size = 1.5, color = purple) +
  annotate("text", label = "S", x = 20.5*100, y = 34, color = purple, size = 6) +
  annotate("segment", x = 0, xend = 1600, y = shutp, yend = shutp, 
           linetype = 2, size = 1, color = purple)

ggsave("mc supply 5.png", device = "png")



ggplot() +
  scale_x_continuous(limits = c(0, xlim), expand=c(0,0), breaks = seq(0,xlim,4)) +
  scale_y_continuous(limits = c(0, ylim), expand=c(0,0), breaks = seq(0,ylim,4)) +
  theme_market +
  labs(x = "Q", y = "$") +
  geom_line(aes(x=x, y=atc(x)), size = 1, color = purple) +
  annotate("text", label = "ATC", x = 12, y = 34, color = purple, size = 6) +
  geom_line(aes(x=z, y=mc(z)), size = 1, color = green) +
  annotate("text", label = "MC", x = 21, y = 34, color = green, size = 6) +
  annotate("point", x = be, y=atc_min, size = 3) +
  annotate("segment", x = be, xend = be, y = 0, yend = atc_min, linetype = 2, size = 1)

ggsave("mc supply 6.png", device = "png")

ggplot() +
  scale_x_continuous(limits = c(0, xlim), expand=c(0,0), breaks = seq(0,xlim,4)) +
  scale_y_continuous(limits = c(0, ylim), expand=c(0,0), breaks = seq(0,ylim,4)) +
  theme_market +
  labs(x = "Q", y = "P") +
  geom_line(aes(x=x, y=atc(x)), size = 1, color = purple, alpha = 0.1) +
  annotate("text", label = "LRATC", x = 12, y = 34, color = purple, size = 6, alpha = 0.1) +
  geom_line(aes(x=seq(be,32,0.1), y=mc(seq(be,32,0.1))), size = 1, color = purple) +
  annotate("text", label = "S[LR]", x = 21, y = 34, color = purple, size = 6, parse = T) +
  annotate("point", x = be, y=atc_min, size = 3) +
  annotate("segment", x = be, xend = be, y = 0, yend = atc_min, linetype = 2, size = 1) +
  annotate("segment", x = 0.1, xend = 0.1, y = 0, yend = mc(be), 
           size = 1.5, color = purple) 

ggsave("mc supply 7.png", device = "png")




  