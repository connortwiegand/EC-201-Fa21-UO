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





#Positive Production Externality
demand <- function(Q) 120 - (1/4) * Q
supply <- function(Q) 60 + (1/2) * Q

q_range <- 0:140
x_lim <- max(q_range)
y_lim <- max(demand(min(q_range)), supply(max(q_range)))

eq <- curve_intersect(supply,demand, empirical = F, domain = c(min(q_range),max(q_range)))

meb <- 30
msc <- function(Q) supply(Q) - meb

eq2 <- curve_intersect(demand, msc, empirical = F, 
                       domain = c(min(q_range),max(q_range)))



nomarks <- ggplot() + 
  # Major Functions:
  geom_line(aes(x=q_range, y=demand(q_range)), size = 1, color = red_pink) + 
  geom_line(aes(x=q_range, y=supply(q_range)), size = 1, color = purple) +
  # Axis Labels, Theme, and Function Labels: 
  labs(title = "Wind Power") + 
  xlab("Q") + ylab("P") + theme_sd +
  # Bounds and Scale
  scale_x_continuous(breaks=seq(0, x_lim, 20), expand=c(0,0), limits=c(0, x_lim)) +
  scale_y_continuous(breaks=seq(0, y_lim, 20), expand=c(0,0), limits=c(0, y_lim)) +
  # Market Modification (Shift, Control, Distortion)
  geom_line(aes(x=q_range, y=msc(q_range)), size = 1, color = purple) +
  #S/D Labels:
  annotate("text", x=137.5, y=82, label="D", 
           fontface = "bold", color = red_pink, size = 3.5, angle = -5) + 
  annotate("text", x=137.5, y=124, label="S", 
           fontface = "bold", color = purple, size = 3.5, angle = 20) +
  #msc/MSC Label:
  annotate("text", x=135, y=103, label="MSC", 
           fontface = "bold", color = purple, size = 3.5, angle = 20)
nomarks

base <- nomarks + 
  annotate("segment", x=eq$x, xend=eq$x, y=0, yend=eq$y, linetype=2, size = 1) + #v
  annotate("segment", x=0, xend=eq$x, y=eq$y, yend=eq$y, linetype=2, size = 1) + #h
  annotate("segment", x=eq2$x, xend=eq2$x, y=0, yend=eq2$y+meb, linetype=2, size=1) + #v
  annotate("segment", x=0, xend=eq2$x, y=eq2$y, yend=eq2$y, linetype=2, size=1) + #h
  annotate("segment", x=0, xend=eq$x, y=eq$y-meb, yend=eq$y-meb, linetype=2, size=1) + #h 
  annotate("segment", x=0, xend=eq2$x, y=120, yend=120, linetype=2, size=1) #h 
base

letters <- base + 
  annotate("text", x=20, y=110, label = "a", size = 4.5) + 
  annotate("text", x=20, y=96, label = "b", size = 4.5) + 
  annotate("text", x=20, y=80, label = "c", size = 4.5) + 
  annotate("text", x=4, y=67, label = "d", size = 4.5) + 
  annotate("text", x=20, y=55, label = "e", size = 4.5) + 
  annotate("text", x=40, y=30, label = "f", size = 4.5) + 
  annotate("text", x=100, y=50, label = "g", size = 4.5) + 
  annotate("text", x=60, y=80, label = "h", size = 4.5) + 
  annotate("text", x=77.25, y=94.75, label = "i", size = 4.5) + 
  annotate("text", x=85, y=80, label = "j", size = 4.5) + 
  annotate("text", x=85, y=94.75, label = "k", size = 4.5) + 
  annotate("text", x=105, y=105, label = "m", size = 4.5) +
  annotate("text", x=80, y=112, label = "o", size = 4.5)
letters

#

ggsave("letters test.png", device = "png")



##### Subsidy Incidence
demand <- function(Q) 20 - 0.25 * Q
supply <- function(Q) 2+(2) * Q
q_range <- 0:40


x_lim <- max(q_range)
y_lim <- 25

mec <- 11
msb <- function(Q) demand(Q) - mec

cnext_elas <- ggplot() + 
  # Major Functions:
  geom_line(aes(x=q_range, y=demand(q_range)), size = 1, color = red_pink) + 
  geom_line(aes(x=q_range, y=supply(q_range)), size = 1, color = purple) +
  # Axis Labels, Theme, and Function Labels: 
  labs(x = "Q", y = "P") + theme_market +
  # Bounds and Scale
  scale_x_continuous(breaks=seq(0, x_lim, 4), expand=c(0,0), limits=c(0, x_lim)) +
  scale_y_continuous(breaks=c(0, 2, 10, 20), expand=c(0,0), limits=c(0, y_lim)) +
  # Market Modification (Shift, Control, Distortion)
  geom_line(aes(x=q_range, y=msb(q_range)), size = 1, color = red_pink) +
  #S/D Labels:
  annotate("text", x=36, y=2, label=expression(bold(D[t])), 
           fontface = "bold", color = red_pink, size = 6) + 
  annotate("text", x=12, y=22, label="S", 
           fontface = "bold", color = purple, size = 6) +
  #MSB/MSC Label:
  annotate("text", x=36, y=12.5, label="D", 
           fontface = "bold", color = red_pink, size = 6) 
cnext_elas

ggsave(filename = "cnext elast fin.png", device = "png")



##### Subsidy Incidence
demand <- function(Q) 20 - 0.25 * Q
supply <- function(Q) 2+(1/2) * Q
q_range <- 0:40


x_lim <- max(q_range)
y_lim <- 25

eq <- curve_intersect(supply,demand, empirical = F, 
                      domain = c(min(q_range),max(q_range)))


mec <- 9
msb <- function(Q) demand(Q) - mec

eq2 <- curve_intersect(msb, supply, empirical = F, 
                       domain = c(min(q_range),max(q_range)))

cnext_calc <- ggplot() + 
  # Major Functions:
  geom_line(aes(x=q_range, y=demand(q_range)), size = 1, color = red_pink) + 
  geom_line(aes(x=q_range, y=supply(q_range)), size = 1, color = purple) +
  # Axis Labels, Theme, and Function Labels: 
  labs(x = "Q", y = "P") + theme_market +
  # Bounds and Scale
  scale_x_continuous(breaks=seq(0, x_lim, 4), expand=c(0,0), limits=c(0, x_lim)) +
  scale_y_continuous(breaks=seq(0, y_lim, 10), expand=c(0,0), limits=c(0, y_lim)) +
  # Market Modification (Shift, Control, Distortion)
  geom_line(aes(x=q_range, y=msb(q_range)), size = 1, color = red_pink) +
  #S/D Labels:
  annotate("text", x=36, y=12.5, label="D", 
           fontface = "bold", color = red_pink, size = 6) + 
  annotate("text", x=36, y=22, label="S", 
           fontface = "bold", color = purple, size = 6) +
  #MSB/MSC Label:
  annotate("text", x=36, y=3.5, label=expression(bold(D[t])), 
           color = red_pink, size = 6) +
  annotate("segment", x=eq$x, xend=eq$x, y=0, yend=eq$y, linetype=2, size = 1) + #v
  annotate("segment", x=0, xend=eq$x, y=eq$y, yend=eq$y, linetype=2, size = 1) + #h
  annotate("segment", x=eq2$x, xend=eq2$x, y=0, yend=eq2$y, linetype=2, size = 1) + #v
  annotate("segment", x=0, xend=eq2$x, y=eq2$y, yend=eq2$y, linetype=2, size = 1) #h
cnext_calc





##### AVC Example:
x <- seq(0.1,13,0.1)
#xd <- seq(0,13,0.1)

xlim <- 13
ylim <- 26
tc <- function(x) (1/4)*(x^3)-2*(x^2)+12*x+6
atc <- function(x) tc(x)/x
#mc <- function(x) (3/4)*(x^2)-4*x+12
#demand <- function(x) 24-2*x

#mr <- function(x) 36-2.5*x

#z <- seq(1,xlim,0.1)

#beq <- uniroot(function(z) atc(z) - mc(z), range(z))$root
#bep <- mc(beq)

mono1 <- ggplot() +
  scale_x_continuous(limits = c(0, xlim), expand=c(0,0), breaks = seq(0,xlim,1)) +
  scale_y_continuous(limits = c(0, ylim), expand=c(0,0), breaks = seq(0,ylim,2)) +
  theme_sd +
  labs(x = "Q", y = "$") +
  geom_line(aes(x=x, y=atc(x)), size = 1, color = purple) +
  annotate("text", label = "ATC", x = 12, y = 21, color = purple, size = 6) +
  #annotate("segment", x=2, xend=2, y=0, yend=12, size = 1, linetype=2) +
  annotate("segment", x=0, xend=2, y=12, yend=12, size = 1, linetype=2)
mono1

ggsave(filename = "avc fin.png", device = "png")




