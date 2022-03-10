if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, dplyr, magrittr, ggplot2)
# Must install through github:
# Even if plotting with ggplot2, contains curve_intersect function
library(econocharts)


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








### CS/PS

demand <- function(Q) 40 - (1/2) * Q
supply <- function(Q) 8 + (23/82) * Q

q_range <- 0:80
x_lim <- max(q_range)
y_lim <- max(demand(min(q_range)), supply(max(q_range))) + 2

eq <- curve_intersect(supply,demand, empirical = F, domain = c(min(q_range),max(q_range)))

#Mangos, Unreg
ggplot() + 
  # Major Functions:
  geom_line(aes(x=q_range, y=demand(q_range)), size = 1, color = red_pink) + 
  geom_line(aes(x=q_range, y=supply(q_range)), size = 1, color = purple) +
  # Axis Labels, Theme, and Function Labels: 
  labs(title = "Mango Flavored Nicotine") + xlab("Q") + ylab("P") + theme_sd +
  annotate("text", x=79, y=2.5, label="D", fontface = "bold", color = red_pink) + 
  annotate("text", x=79, y=31.5, label="S", fontface = "bold", color = purple)+
  # Bounds and Scale
  scale_x_continuous(breaks=seq(0, x_lim, 10), expand=c(0,0), limits=c(0, x_lim)) +
  scale_y_continuous(breaks=seq(0, y_lim, 4), expand=c(0,0), limits=c(0, y_lim)) +
  # Equilibrium:
  annotate("point", x=eq$x+0.15, y=eq$y, size=4) +
  annotate("text", x=eq$x+8, y=eq$y, 
           label=paste0("(",eq$x,",",eq$y,")"), size=3.1, fontface = "bold") +
  annotate("segment", x=eq$x, xend=eq$x, y=0, yend=eq$y, linetype=2, size = 1) +
  annotate("segment", x=0, xend=eq$x, y=eq$y, yend=eq$y, linetype=2, size = 1)

ggsave("mangos.png", device = "png")

cs <- (41-19.5)*40
ps <- 0.5*(19.5-8)*41
cs+ps



### CS/PS

demand <- function(Q) 600 - Q
supply <- function(Q) 60 + (2) * Q

q_range <- 0:600
x_lim <- max(q_range)
y_lim <- max(demand(min(q_range)), supply(max(q_range))) + 2

eq <- curve_intersect(supply,demand, empirical = F, domain = c(min(q_range),max(q_range)))

#Mangos
ggplot() + 
  # Major Functions:
  geom_line(aes(x=q_range, y=demand(q_range)), size = 1, color = red_pink) + 
  geom_line(aes(x=q_range, y=supply(q_range)), size = 1, color = purple) +
  # Axis Labels, Theme, and Function Labels: 
  labs(title = "Supreme Bucket Hats") + xlab("Q") + ylab("P") + theme_sd +
  annotate("text", x=590, y=40, label="D", fontface = "bold", color = red_pink) + 
  annotate("text", x=300, y=630, label="S", fontface = "bold", color = purple)+
  # Bounds and Scale
  scale_x_continuous(breaks=seq(0, x_lim, 30), expand=c(0,0), limits=c(0, x_lim)) +
  scale_y_continuous(breaks=seq(0, y_lim, 60), expand=c(0,0), limits=c(0, y_lim-600))

ggsave("supreme.png", device = "png")

cs <- (41-19.5)*40
ps <- 0.5*(19.5-8)*41
cs+ps


### CS/PS

demand <- function(Q) 50 - 2*Q
d2 <- function(Q) demand(Q) + 30
supply <- function(Q) 8 + (2) * Q

q_range <- 0:60
x_lim <- max(q_range)
y_lim <- max(demand(min(q_range)), supply(max(q_range))) + 2

#eq <- curve_intersect(supply,demand, empirical = F, domain = c(min(q_range),max(q_range)))

#Mangos
ggplot() + 
  # Major Functions:
  geom_line(aes(x=q_range, y=demand(q_range)), size = 1) + 
  geom_line(aes(x=q_range, y=d2(q_range)), size = 1) +
  # Axis Labels, Theme, and Function Labels: 
  labs(title = "Market for Good Soup") + xlab("Q") + ylab("P") + theme_sd +
  annotate("text", x=25, y=5, label="D", fontface = "bold") + 
  annotate("text", x=40, y=5, label="D'", fontface = "bold") +
  # Bounds and Scale
  scale_x_continuous(breaks=seq(0, x_lim, 5), expand=c(0,0), limits=c(0, 50)) +
  scale_y_continuous(breaks=seq(0, y_lim, 10), expand=c(0,0), limits=c(0, 90)) + 
  annotate("point", x=5, y=40, size=2.5) + 
  annotate("point", x=10, y=30, size=2.5) +
  annotate("point", x=7.5, y=65, size=2.5) + 
  annotate("point", x=30, y=20, size=2.5) +
  annotate("text", x=6, y=43, label="a", fontface = "bold") + 
  annotate("text", x=11, y=33, label="b", fontface = "bold") +
  annotate("text", x=8.5, y=68, label="c", fontface = "bold") + 
  annotate("text", x=31, y=23, label="d", fontface = "bold")

ggsave("elast exam.png", device = "png")



####
#PPF
ppf <- function(Q) 900-1.5*Q

q_range <- 0:600
x_lim <- max(q_range) + 50
y_lim <- max(ppf(q_range)) + 50

#Mangos
ggplot() + 
  # Major Functions:
  geom_line(aes(x=q_range, y=ppf(q_range)), size = 1) + 
  # Axis Labels, Theme, and Function Labels: 
  labs(title = "PPF") + xlab("Cocaine (g)") + ylab("Crack (g)") + theme_sd +
  # Bounds and Scale
  scale_x_continuous(breaks=seq(0, x_lim, 100), expand=c(0,0), limits=c(0, 650)) +
  scale_y_continuous(breaks=seq(0, y_lim, 100), expand=c(0,0), limits=c(0, 950))

ggsave("ppf exam.png", device = "png")
