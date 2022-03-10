if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, dplyr, magrittr, ggplot2)
# Must install through github:
# Even if plotting with ggplot2, contains curve_intersect function
library(econocharts)
#p_load(ggpubr, grid, gridBase, gridExtra)
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



x <- seq(0.1,13,0.1)
xd <- seq(0,13,0.1)

xlim <- 13
ylim <- 26
tc <- function(x) (1/4)*(x^3)-2*(x^2)+12*x+6
atc <- function(x) tc(x)/x
mc <- function(x) (3/4)*(x^2)-4*x+12
demand <- function(x) 24-2*x

#mr <- function(x) 36-2.5*x

z <- seq(1,xlim,0.1)

beq <- uniroot(function(z) atc(z) - mc(z), range(z))$root
bep <- mc(beq)

mono1 <- ggplot() +
  scale_x_continuous(limits = c(0, xlim), expand=c(0,0), breaks = seq(0,xlim,1)) +
  scale_y_continuous(limits = c(0, ylim), expand=c(0,0), breaks = seq(0,ylim,2)) +
  theme_sd +
  labs(x = "Q", y = "$") +
  geom_line(aes(x=x, y=atc(x)), size = 1, color = purple) +
  annotate("text", label = "ATC", x = 12, y = 21, color = purple, size = 6) +
  geom_line(aes(x=z, y=mc(z)), size = 1, color = green) +
  annotate("text", label = "MC", x = 8, y = 23, color = green, size = 6) +
  geom_line(aes(x=xd, y=demand(xd)), size = 1, color = red_pink) +
  annotate("text", label = "D", x = 12, y = 2, color = red_pink, size = 6)
mono1



x <- seq(0,36,0.1)
z <- seq(0.1,36,0.1)

xlim <- 36
ylim <- 36
tc <- function(x) 1/4*(x^2) + 6*x + 144
atc <- function(x) tc(x)/x
mc <- function(x) (1/2)*(x)+ 6
demand <- function(x) 36 - x

#z <- seq(1,xlim,0.1)

#beq <- uniroot(function(z) atc(z) - mc(z), range(z))$root
#bep <- mc(beq)

mono1 <- ggplot() +
  scale_x_continuous(limits = c(0, xlim), expand=c(0,0), breaks = seq(0,xlim,4)) +
  scale_y_continuous(limits = c(0, ylim), expand=c(0,0), breaks = seq(0,ylim,2)) +
  theme_sd +
  labs(x = "Q", y = "$", title = "AMRAAM Missile") +
  geom_line(aes(x=z, y=atc(z)), size = 1, color = purple) +
  annotate("text", label = "ATC", x = 32, y = 16.5, color = purple, size = 6, fontface="bold") +
  geom_line(aes(x=x, y=mc(x)), size = 1, color = green) +
  annotate("text", label = "MC", x = 32, y = 24, color = green, size = 6, fontface="bold") +
  geom_line(aes(x=x, y=demand(x)), size = 1, color = red_pink) +
  annotate("text", label = "D", x = 34, y = 5, color = red_pink, size = 6, fontface="bold")
mono1

ggsave(filename = "lin monop FA3.png", device = "png")





