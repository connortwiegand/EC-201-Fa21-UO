#Graphing Using Functions
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



##Theme testing example:
demand <- function(Q) 40 - (3/20) * Q
supply <- function(Q) 0 + (7/220) * Q
q_range <- 0:500
x_lim <- max(q_range)
y_lim <- max(demand(min(q_range)), supply(max(q_range))) + 2
eq <- curve_intersect(supply,demand, empirical = F, domain = c(min(q_range),max(q_range)))
## Actual Plot
ggplot() + geom_line(aes(x=q_range, y=demand(q_range)), size = 1, color = red_pink) + 
  geom_line(aes(x=q_range , y = supply(q_range)), size = 1, color = purple) +
  xlab("Q") + ylab("W") +
  theme_sd +
  scale_x_continuous(breaks = seq(-20,x_lim-5,40), expand = c(0, 0), limits = c(0,x_lim-100)) +
  scale_y_continuous(breaks = seq(0,y_lim,4), expand = c(0, 0), limits = c(0,y_lim)) +
  annotate("text", x=270, y=1.5, label = "D") + annotate("text", x=370, y=13, label = "S")+
  geom_hline(yintercept = 12, color = "black", size = 1)


# Geom_line Example:
demand <- function(Q) 20 - 0.5 * Q
supply <- function(Q) 2 + 0.25 * Q
q_range <- seq(0,40,0.01)
x_lim <- max(q_range) + 2
y_lim <- max(demand(min(q_range)), supply(max(q_range))) + 2
eq <- curve_intersect(supply,demand, empirical = F, domain = c(min(q_range),max(q_range)))
## Plot
ggplot() + geom_line(aes(x=q_range , y = demand(q_range ))) + 
  geom_line(aes(x=q_range , y = supply(q_range ))) +
  xlab("Q") + ylab("P") +
  theme(axis.title.x = element_text(hjust=1), 
        axis.title.y = element_text(hjust=1, angle = 360)) +
  scale_x_continuous(breaks = seq(0,40,4), expand = c(0, 0), limits = c(0,x_lim)) +
  scale_y_continuous(breaks = seq(0,20,2), expand = c(0, 0), limits = c(0,y_lim)) +
  annotate("text", x=39, y=1.5, label = "D") + annotate("text", x=39, y=12.5, label = "S")



#Using stat_fun:
demand <- function(Q) 40 - (3/20) * Q
supply <- function(Q) 0 + (7/220) * Q
q_range <- 0:500
x_lim <- max(q_range)
y_lim <- max(demand(min(q_range)), supply(max(q_range))) + 2
eq <- curve_intersect(supply,demand, empirical = F, domain = c(min(q_range),max(q_range)))
## Actual Plot
ggplot(data.frame(x = c(0:500)), aes(x)) + 
  stat_function(aes(color = "D"), fun = demand, size = 1) + 
  stat_function(aes(color = "S"), fun = supply) + xlab("Q") + ylab("W") +
  theme(axis.title.x = element_text(hjust=1), 
        axis.title.y = element_text(hjust=1, angle = 360)) +
  scale_x_continuous(breaks = seq(-20,x_lim-5,40), expand = c(0, 0), limits = c(0,x_lim-120)) +
  scale_y_continuous(breaks = seq(0,y_lim,4), expand = c(0, 0), limits = c(0,y_lim)) +
  annotate("text", x=270, y=1.5, label = "D") + annotate("text", x=370, y=13, label = "S")+
  geom_hline(yintercept = 12, color = "red") + theme_market



#Kyle Raze:
x <- 0:10
floor <- 7
demand <- function(x) 10 - x
supply <- function(x) 1 + x
q <- uniroot(function(x) demand(x) - supply(x), range(x))$root
p <- supply(q)
q_d <- uniroot(function(x) demand(x) - floor, range(x))$root
q_s <- uniroot(function(x) floor - supply(x), range(x))$root
mv_gap <- supply(q_d)
w <- seq(0, q_d, 0.1)
z <- seq(q_d, q, 0.1)
ggplot(data.frame(x = c(0:10)), aes(x)) +
  scale_x_continuous(limits = c(0, 10.5), expand = c(0, 0), breaks = seq(0, 10, 1)) +
  scale_y_continuous(limits = c(0, 10.5), expand = c(0, 0), breaks = seq(0, 10, 1)) +
  labs(x = "Q", y = "P") +
  stat_function(aes(x), fun = supply, size = 1) + # supply function
  stat_function(aes(x), fun = demand, size = 1) +
  theme_market# demand function
annotate("text", label = expression(D), x = 9, y = 1.6,  size = 9) +
  annotate("text", label = expression(S), x = 9, y = 9.5,  size = 9) +
  theme_market +
  geom_segment(aes(x = q, y = 0, xend = q, yend = p), linetype  = "dashed",  size = 1) + # Q*
  geom_segment(aes(x = 0, y = p, xend = q, yend = p), linetype  = "dashed",  size = 1) + # P*
  geom_point(aes(x = q, y = p),  size = 2) # equilibrium bundle



# Step-wise Example
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, dplyr, magrittr, ggplot2)

p <- c(4,6,7,8,10,12,12)

q <- c(6,5,4,3,2,1,0)

#Market
steps <- ggplot() + geom_step(aes(y=p,x=q),col="black") + 
  xlab("Qauntity Demanded") + ylab("Price") + labs(title = "Market Demand") +
  theme(plot.title = element_text(hjust=0.5))
steps

dx <- c(0,3)
dy <- c(8,8)

z <- seq(0,3,0.01)

steps + geom_line(aes(x=dx, y=dy), linetype = 2) + 
  geom_ribbon(aes(x = z, ymin = 8, ymax = if_else(z<2,12,10),
                  fill = "dodgerblue"), alpha = 0.25, show.legend = F) + 
  geom_text(aes(x=1,y=10, label = "CS")) + xlim(0,6) +
  scale_x_continuous(expand = c(0, 0),) + scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values=c("dodgerblue"), name="fill")


dx2 <- c(0,4)
dy2 <- c(7,7)

w <- seq(0,4,0.01)

steps + geom_line(aes(x=dx2, y=dy2), linetype = 2) + 
  geom_ribbon(aes(x = w, ymin = 7, ymax = if_else(w<2,12,if_else(w<3,10,8)),
                  fill = 2), alpha = 0.25, show.legend = F) + 
  geom_text(aes(x=1.5,y=9, label = "CS'")) +
  scale_x_continuous(expand = c(0, 0),) + scale_y_continuous(expand = c(0, 0))

