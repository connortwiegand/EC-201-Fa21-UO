if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, dplyr, magrittr, ggplot2)
# Must install through github:
# Even if plotting with ggplot2, contains curve_intersect function
library(econocharts)
p_load(latex2exp)
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



#####
demand <- function(Q) 20 - 0.5 * Q
supply <- function(Q) (0.5) * Q
q_range <- 0:40


x_lim <- max(q_range)
y_lim <- 24

eq <- curve_intersect(supply,demand, empirical = F, 
                      domain = c(min(q_range),max(q_range)))


mec <- 12
msc <- function(Q) supply(Q) + mec

eq2 <- curve_intersect(msc, demand, empirical = F, 
                       domain = c(min(q_range),max(q_range)))

pnext_base <- ggplot() + 
  # Major Functions:
  geom_line(aes(x=q_range, y=demand(q_range)), size = 1, color = red_pink) + 
  geom_line(aes(x=q_range, y=supply(q_range)), size = 1, color = purple) +
  # Axis Labels, Theme, and Function Labels: 
  labs(x = "Q", y = "P", title = "Palm Oil") + theme_sd +
  # Bounds and Scale
  scale_x_continuous(breaks=seq(0, x_lim, 2), expand=c(0,0), limits=c(0, x_lim)) +
  scale_y_continuous(breaks=seq(0, y_lim, 2), expand=c(0,0), limits=c(0, y_lim)) +
  # Market Modification (Shift, Control, Distortion)
  #geom_line(aes(x=q_range, y=msc(q_range)), size = 1, color = purple) +
  # annotate("text", x=190, y=154, label="MSC", 
  #          fontface = "bold", color = purple, size = 4) +
  #S/D Labels:
  annotate("text", x=38, y=2.5, label="D", 
           fontface = "bold", color = red_pink, size = 6) + 
  annotate("text", x=38, y=20.5, label="S", 
           fontface = "bold", color = purple, size = 6)
  #MSB/MSC Label:
pnext_base


ggsave(filename = "pnext FA4.png", device = "png")

