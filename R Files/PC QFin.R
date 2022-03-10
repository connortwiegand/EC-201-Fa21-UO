if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, dplyr, magrittr, ggplot2)
# Must install through github:
# Even if plotting with ggplot2, contains curve_intersect function
library(econocharts)
p_load(ggpubr, grid, gridBase, gridExtra)
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

x <- seq(0,28,0.1)
z <- seq(0.1,28,0.1)
# z0 <- seq(6,shutq,0.1)
# z1 <- seq(shutq,32,0.1)

xlim <- max(x)
ylim <- 32
tc <- function(x) (5/6)*(x^2)+5*x+120
vc <- function(x) tc(x)-120

atc <- function(x) tc(x)/x
avc <- function(x) vc(x)/x

mc <- function(x) (5/3)*x+5

#beq <- 12
#bep <- 24


firm1 <- ggplot() +
  scale_x_continuous(limits = c(0, xlim), expand=c(0,0), breaks = seq(0,xlim,2)) +
  scale_y_continuous(limits = c(0, ylim), expand=c(0,0), breaks = seq(0,ylim,2)) +
  theme_sd +
  labs(x = "Q", y = "$", title = "Not A Pyramid, LLC") +
  geom_line(aes(x=z, y=atc(z)), size = 1, color = purple) +
  annotate("text", label = "ATC", x = 4.5, y = 29, color = purple, size = 6) +
  geom_line(aes(x=x, y=avc(x)), size = 1, color = orange) +
  annotate("text", label = "AVC", x = 25, y = 23, color = orange, size = 6) +
  geom_line(aes(x=x, y=mc(x)), size = 1, color = green) +
  annotate("text", label = "MC", x = 16.5, y = 29, color = green, size = 6)
firm1


x2 <- seq(0,240,1)
xlim2 <- 240

supply <- function(x) mc(x/10)
demand <- function(x) 32-(12/90)*x

mark1 <- ggplot() +
  scale_x_continuous(limits = c(0, xlim2), expand=c(0,0), breaks = seq(0,xlim2,30)) +
  scale_y_continuous(limits = c(0, ylim), expand=c(0,0), breaks = seq(0,ylim,2)) +
  theme_sd +
  labs(x = "Q", y = "P", title = "Skincare Market") +
  geom_line(aes(x=x2, y=supply(x2)), size = 1, color = purple) +
  annotate("text", label = expression(bold(S[SR])), x = 170, y = 30, color = purple, size = 6)+
  geom_line(aes(x=x2, y=demand(x2)), size = 1, color = red_pink) +
  annotate("text", label = "D", x = 225, y = 4, color = red_pink, size = 6, fontface = "bold")
mark1

figpc <- grid.arrange(firm1, mark1, nrow = 1)
figpc

ggsave(plot = figpc, filename = "PC QFin FA2.png", device = "png", height = 4.36, width = 10)




##Solution
x <- seq(0,28,0.1)
z <- seq(0.1,28,0.1)
# z0 <- seq(6,shutq,0.1)
# z1 <- seq(shutq,32,0.1)

xlim <- max(x)
ylim <- 32
tc <- function(x) (5/6)*(x^2)+5*x+120
vc <- function(x) tc(x)-120

atc <- function(x) tc(x)/x
avc <- function(x) vc(x)/x

mc <- function(x) (5/3)*x+5
#beq <- 12
#bep <- 24


firm1 <- ggplot() +
  scale_x_continuous(limits = c(0, xlim), expand=c(0,0), breaks = seq(0,xlim,2)) +
  scale_y_continuous(limits = c(0, ylim), expand=c(0,0), breaks = seq(0,ylim,2)) +
  theme_sd +
  labs(x = "Q", y = "$", title = "Not A Pyramid, LLC") +
  geom_line(aes(x=z, y=atc(z)), size = 1, color = purple) +
  annotate("text", label = "ATC", x = 4.5, y = 29, color = purple, size = 6) +
  geom_line(aes(x=x, y=avc(x)), size = 1, color = orange) +
  annotate("text", label = "AVC", x = 25, y = 23, color = orange, size = 6) +
  geom_line(aes(x=x, y=mc(x)), size = 1, color = green) +
  annotate("text", label = "MC", x = 16.5, y = 29, color = green, size = 6)
firm1


x2 <- seq(0,240,1)
xlim2 <- 240

supply <- function(x) mc(x/10)
demand <- function(x) 32-(12/90)*x

mark1 <- ggplot() +
  scale_x_continuous(limits = c(0, xlim2), expand=c(0,0), breaks = seq(0,xlim2,30)) +
  scale_y_continuous(limits = c(0, ylim), expand=c(0,0), breaks = seq(0,ylim,2)) +
  theme_sd +
  labs(x = "Q", y = "P", title = "Skincare Market") +
  geom_line(aes(x=x2, y=supply(x2)), size = 1, color = purple) +
  annotate("text", label = expression(bold(S[SR])), x = 170, y = 30, color = purple, size = 6)+
  geom_line(aes(x=x2, y=demand(x2)), size = 1, color = red_pink) +
  annotate("text", label = "D", x = 225, y = 4, color = red_pink, size = 6, fontface = "bold")
mark1

figpc <- grid.arrange(firm1, mark1, nrow = 1)
figpc

ggsave(plot = figpc, filename = "PC QFin FA2.png", device = "png", height = 4.36, width = 10)


