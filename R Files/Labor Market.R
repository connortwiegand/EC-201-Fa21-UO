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
  plot.margin = margin(t=2, r=8, b=2, l=2)
)


###################
### Labor Markets

demand <- function(Q) 40 - (4/25) * Q
supply <- function(Q) 0 + (1/25) * Q

q_range <- 0:500
x_lim <- max(q_range)
y_lim <- max(demand(min(q_range)), supply(max(q_range))) + 2

eq <- curve_intersect(supply,demand, empirical = F, domain = c(min(q_range),max(q_range)))

#Labor Market, Unreg
ggplot() + 
  # Major Functions:
  geom_line(aes(x=q_range, y=demand(q_range)), size = 1, color = red_pink) + 
  geom_line(aes(x=q_range, y=supply(q_range)), size = 1, color = purple) +
  # Axis Labels, Theme, and Function Labels: 
  labs(title = "Labor Market") + xlab("Q") + ylab("W") + theme_sd +
  annotate("text", x=255, y=1.75, label="D", fontface = "bold", color = red_pink) + 
  annotate("text", x=352, y=15.75, label="S", fontface = "bold", color = purple)+
  # Bounds and Scale
  scale_x_continuous(breaks=seq(0, x_lim-100, 40), expand=c(0,0), limits=c(0, x_lim-140)) +
  scale_y_continuous(breaks=seq(0, y_lim, 4), expand=c(0,0), limits=c(0, y_lim)) +
  # Equilibrium:
  annotate("point", x=eq$x+0.15, y=eq$y, size=4) +
  annotate("text", x=209, y=10.55, label="(200,8)", size=3.1, fontface = "bold") +
  annotate("segment", x=200, xend=200, y=0, yend=8, linetype=2, size = 1) +
  annotate("segment", x=0, xend=200, y=8, yend=8, linetype=2, size = 1)


p_f <- function(Q) 12

qd <- curve_intersect(demand, p_f, empirical = F, domain = c(min(q_range),max(q_range)))
qs <- curve_intersect(supply, p_f, empirical = F, domain = c(min(q_range),max(q_range)))
  
# Effective Price Floor
ggplot() + 
    # Major Functions:
    geom_line(aes(x=q_range, y=demand(q_range)), size = 1, color = red_pink) + 
    geom_line(aes(x=q_range, y=supply(q_range)), size = 1, color = purple) +
    # Axis Labels, Theme, and Function Labels: 
    labs(title = "Labor Market w/ Binding Floor") + xlab("Q") + ylab("W") + theme_sd +
    annotate("text", x=255, y=1.75, label="D", fontface = "bold", color = red_pink) + 
    annotate("text", x=352, y=15.75, label="S", fontface = "bold", color = purple)+
    # Bounds and Scale
    scale_x_continuous(breaks=seq(0, x_lim-100, 40), expand=c(0,0), limits=c(0, x_lim-140)) +
    scale_y_continuous(breaks=seq(0, y_lim, 4), expand=c(0,0), limits=c(0, y_lim)) +
    #Q[D]:
    annotate("point", x=qd$x+0.15, y=qd$y, size=3) +
    annotate("text", x=190, y=14, label="(175,12)", size=3.1, fontface = "bold") +
    annotate("segment", x=175, xend=175, y=0, yend=12, linetype=2, size = 1) +
    annotate("segment", x=0, xend=175, y=12, yend=12, linetype=2, size = 1) +
    # Q[S]:
    annotate("point", x=qs$x+0.15, y=qs$y, size=3) +
    annotate("text", x=295, y=14, label="(300,12)", size=3.1, fontface = "bold") +
    annotate("segment", x=300, xend=300, y=0, yend=12, linetype=2, size = 1) +
    annotate("segment", x=0, xend=300, y=12, yend=12, linetype=2, size = 1) +
    # Market Modification (Shift, Control, Distortion)
    annotate("segment", x=0, xend=x_lim-160, y=12, yend=12, linetype=1, size = 1) +
    annotate("text", x=350, y=11, label=expression(P[F]))  
  

#####Case 2: 

d2 <- function(Q) 45 - (3/20) * Q
s2 <- function(Q) 0 + (3/40) * Q

q_range <- 0:500
x_lim <- max(q_range)
y_lim <- max(d2(min(q_range)), s2(max(q_range))) + 2

eq2 <- curve_intersect(s2,d2, empirical = F, domain = c(min(q_range),max(q_range)))

#Labor Market 2, Unreg
ggplot() + 
  # Major Functions:
  geom_line(aes(x=q_range, y=d2(q_range)), size = 1, color = red_pink) + 
  geom_line(aes(x=q_range, y=s2(q_range)), size = 1, color = purple) +
  # Axis Labels, Theme, and Function Labels: 
  labs(title = "Labor Market 2") + xlab("Q") + ylab("W") + theme_sd +
  annotate("text", x=305, y=1.75, label="D", fontface = "bold", color = red_pink) + 
  annotate("text", x=352, y=28.5, label="S", fontface = "bold", color = purple)+
  # Bounds and Scale
  scale_x_continuous(breaks=seq(0, x_lim-100, 40), expand=c(0,0), limits=c(0, x_lim-140)) +
  scale_y_continuous(breaks=seq(0, y_lim, 4), expand=c(0,0), limits=c(0, y_lim)) +
  # Equilibrium:
  annotate("point", x=eq2$x+0.15, y=eq2$y, size=4) +
  annotate("text", x=209, y=18.25, label="(200,15)", size=3.1, fontface = "bold") +
  annotate("segment", x=200, xend=200, y=0, yend=15, linetype=2, size = 1) +
  annotate("segment", x=0, xend=200, y=15, yend=15, linetype=2, size = 1)


p_f <- function(Q) 12

qd2 <- curve_intersect(d2, p_f, empirical = F, domain = c(min(q_range),max(q_range)))
qs2 <- curve_intersect(s2, p_f, empirical = F, domain = c(min(q_range),max(q_range)))

# Ineffective Price Floor
ggplot() + 
  # Major Functions:
  geom_line(aes(x=q_range, y=d2(q_range)), size = 1, color = red_pink) + 
  geom_line(aes(x=q_range, y=s2(q_range)), size = 1, color = purple) +
  # Axis Labels, Theme, and Function Labels: 
  labs(title = "Labor Market 2 w/ Non-Binding Floor") + xlab("Q") + ylab("W") + theme_sd +
  annotate("text", x=305, y=1.75, label="D", fontface = "bold", color = red_pink) + 
  annotate("text", x=352, y=28.5, label="S", fontface = "bold", color = purple) +
  # Bounds and Scale
  scale_x_continuous(breaks=seq(0, x_lim-100, 40), expand=c(0,0), limits=c(0, x_lim-140)) +
  scale_y_continuous(breaks=seq(0, y_lim, 4), expand=c(0,0), limits=c(0, y_lim)) +
  #Q[D]:
  annotate("point", x=qd2$x+0.15, y=qd2$y, size=3, color="grey50") +
  annotate("text", x=qd2$x + 15, y=14, label="(220,12)", size=3.1, fontface = "bold", color="grey50") +
  annotate("segment", x=qd2$x, xend=qd2$x, y=0, yend=12, linetype=2, size = 1, color="grey50") +
  annotate("segment", x=0, xend=qd2$x, y=12, yend=12, linetype=2, size = 1, color="grey50") +
  # Q[S]:
  annotate("point", x=qs2$x, y=qs2$y, size=3, color="grey50") +
  annotate("text", x=qs2$x-7, y=14, label="(160,12)", size=3.1, fontface = "bold", color="grey50") +
  annotate("segment", x=qs2$x, xend=qs2$x, y=0, yend=12, linetype=2, size = 1, color="grey50") +
  annotate("segment", x=0, xend=qs2$x, y=12, yend=12, linetype=2, size = 1, color="grey50") +
  # True Equilibrium:
  annotate("point", x=eq2$x+0.15, y=eq2$y, size=4) +
  annotate("text", x=209, y=18.25, label="(200,15)", size=3.1, fontface = "bold") +
  annotate("segment", x=200, xend=200, y=0, yend=15, linetype=2, size = 1) +
  annotate("segment", x=0, xend=200, y=15, yend=15, linetype=2, size = 1) +
  # Market Modification (Shift, Control, Distortion)
  annotate("segment", x=0, xend=x_lim-160, y=12, yend=12, linetype=1, size = 1) +
  annotate("text", x=350, y=11, label=expression(P[F]))  


##### Persistent shortage: 

d2 <- function(Q) 45 - (3/20) * Q
s2 <- function(Q) 0 + (3/40) * Q

q_range <- 0:500
x_lim <- max(q_range)
y_lim <- max(d2(min(q_range)), s2(max(q_range))) + 2

eq2 <- curve_intersect(s2,d2, empirical = F, domain = c(min(q_range),max(q_range)))

p_f <- function(Q) 12

qd2 <- curve_intersect(d2, p_f, empirical = F, domain = c(min(q_range),max(q_range)))
qs2 <- curve_intersect(s2, p_f, empirical = F, domain = c(min(q_range),max(q_range)))

#Labor Market 3
ggplot() + 
  # Major Functions:
  geom_line(aes(x=q_range, y=d2(q_range)), size = 1, color = red_pink) + 
  geom_line(aes(x=q_range, y=s2(q_range)), size = 1, color = purple) +
  # Axis Labels, Theme, and Function Labels: 
  labs(title = "Labor Market 2 w/ Shortage") + xlab("Q") + ylab("W") + theme_sd +
  annotate("text", x=305, y=1.75, label="D", fontface = "bold", color = red_pink) + 
  annotate("text", x=352, y=28.5, label="S", fontface = "bold", color = purple) +
  # Bounds and Scale
  scale_x_continuous(breaks=seq(0, x_lim-100, 40), expand=c(0,0), limits=c(0, x_lim-140)) +
  scale_y_continuous(breaks=seq(0, y_lim, 4), expand=c(0,0), limits=c(0, y_lim)) +
  #Q[D]:
  annotate("point", x=qd2$x+0.15, y=qd2$y, size=3, color="grey30") +
  annotate("text", x=qd2$x + 15, y=14, label="(220,12)", size=3.1, fontface = "bold", color="grey30") +
  annotate("segment", x=qd2$x, xend=qd2$x, y=0, yend=12, linetype=2, size = 1, color="grey30") +
  annotate("segment", x=0, xend=qd2$x, y=12, yend=12, linetype=2, size = 1, color="grey30") +
  # Q[S]:
  annotate("point", x=qs2$x, y=qs2$y, size=3, color="grey30") +
  annotate("text", x=qs2$x-7, y=14, label="(160,12)", size=3.1, fontface = "bold", color="grey30") +
  annotate("segment", x=qs2$x, xend=qs2$x, y=0, yend=12, linetype=2, size = 1, color="grey30") +
  annotate("segment", x=0, xend=qs2$x, y=12, yend=12, linetype=2, size = 1, color="grey30") +
  # Market Modification (Shift, Control, Distortion)
  annotate("segment", x=0, xend=x_lim-140, y=12, yend=12, linetype=2, size = 1, color="grey30")

# Movement from demand
ggplot() + 
  # Major Functions:
  geom_line(aes(x=q_range, y=d2(q_range)), size = 1, color = red_pink) + 
  geom_line(aes(x=q_range, y=s2(q_range)), size = 1, color = purple) +
  # Axis Labels, Theme, and Function Labels: 
  labs(title = "Labor Market 2 w/ Shortage, Movement") + xlab("Q") + ylab("W") + theme_sd +
  annotate("text", x=305, y=1.75, label="D", fontface = "bold", color = red_pink) + 
  annotate("text", x=352, y=28.5, label="S", fontface = "bold", color = purple) +
  # Bounds and Scale
  scale_x_continuous(breaks=seq(0, x_lim-100, 40), expand=c(0,0), limits=c(0, x_lim-140)) +
  scale_y_continuous(breaks=seq(0, y_lim, 4), expand=c(0,0), limits=c(0, y_lim)) +
  #Q[D]:
  annotate("point", x=qd2$x+0.15, y=qd2$y, size=3, color="grey30") +
  annotate("segment", x=qd2$x, xend=qd2$x, y=0, yend=12, linetype=2, size = 1, color="grey30") +
  annotate("segment", x=0, xend=qd2$x, y=12, yend=12, linetype=2, size = 1, color="grey30") +
  # Q[S]:
  annotate("point", x=qs2$x, y=qs2$y, size=3, color="grey30") +
  annotate("segment", x=qs2$x, xend=qs2$x, y=0, yend=12, linetype=2, size = 1, color="grey30") +
  annotate("segment", x=0, xend=qs2$x, y=12, yend=12, linetype=2, size = 1, color="grey30") +
  # Market Modification (Shift, Control, Distortion)
  #annotate("segment", x=0, xend=qd2$x, y=12, yend=12, linetype=2, size = 1, color="grey30") +
  # True Equilibrium:
  annotate("point", x=eq2$x+0.15, y=eq2$y, size=4) +
  #annotate("segment", x=200, xend=200, y=0, yend=15, linetype=2, size = 1) +
  #annotate("segment", x=0, xend=200, y=15, yend=15, linetype=2, size = 1)
  # Arrow:
  annotate("segment", x = 218, xend = 206, y = 13, yend = 14.75,
           arrow = arrow(length = unit(0.4, "lines")), colour = green,size = 1)




