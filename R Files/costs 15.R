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

pf <- function(z) 16*sqrt(z)

range <- seq(0,10, 0.1)

ggplot() + 
  # Major Functions:
  geom_line(aes(x=range, y=pf(range)), size = 1, color = "red") + 
  # Axis Labels, Theme, and Function Labels: 
  labs(title = "Production function for x") + 
  xlab("Total Output") + ylab("# of   \nWorkers") + theme_sd 
ggsave("PF1.png", device = "png")

mpl <- function(z) 8/sqrt(z)
?plotmath

ggplot() + 
  # Major Functions:
  geom_line(aes(x=range, y=mpl(range)), size = 1, color = "blue") + 
  # Axis Labels, Theme, and Function Labels: 
  labs(title = parse(text = "MP[L]")) + 
  xlab("Total Output") + ylab("# of   \nWorkers") + theme_sd 
ggsave("mpl1.png", device = "png")

tc <- c(50,
68,
86,
104,
122,
140,
158,
176,
194,
212)

q <- c(0.0,
       4.0,
       5.6,
       6.9,
       8.0,
       8.9,
       9.7,
       10.5,
       11.3,
       12.0)

plot(q,tc, type = "l", main = "Total Cost Curve", xlab = "Output", ylab = "Total Cost")

#Better version of the previous plot
tc <- function(q) 50+(18/16)* q^2

q_range <- seq(0,12,0.1)

ggplot() + 
  # Major Functions:
  geom_line(aes(x=q_range, y=tc(q_range)), size = 1, color = green) + 
  # Axis Labels, Theme, and Function Labels: 
  labs(title= "Total Cost Curve") + 
  xlab("Output") + ylab("Total Cost") + theme_sd 
ggsave("TC1.png", device = "png")
