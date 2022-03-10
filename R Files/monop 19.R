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

##### 
x <- seq(0, 40, 0.1)
atc <- function(x) 180/x
xlim <- max(x)
ylim <- 40

ggplot() +
  scale_x_continuous(limits = c(0, xlim), expand=c(0,0), breaks = seq(0,xlim,5)) +
  scale_y_continuous(limits = c(0, ylim), expand=c(0,0), breaks = seq(0,ylim,5)) +
  theme_market +
  labs(x = "Q", y = "$", title = "Natural Monopoly's ATC") +
  geom_line(aes(x=x, y=atc(x)), size = 1, color = purple) +
  annotate("text", label = "ATC", x = 37.5, y = 7.5, color = purple, size = 6)

ggsave("nat monop atc.png", device = png)



demand <- function(x) 8 - 0.6*x
inv_demand <- function(x) (5/3)*(8 - x)
p_1 <- 7
p_2 <- 6
q_1 <- inv_demand(p_1) 
q_2 <- inv_demand(p_2)
d_m <- ggplot(data = data.frame(x = 0), mapping = aes(x = x)) +
  scale_x_continuous(limits = c(0, 10.5), expand = c(0, 0), breaks = NULL) +
  scale_y_continuous(limits = c(0, 10.5), expand = c(0, 0), breaks = NULL) +
  theme_market +
  labs(x = "Q", y = "P", title = "Demand under Monopoly") +
  stat_function(fun = demand, color = red_pink, size = 1) +
  annotate("text", label = "D", x = 10, y = 2.65, color = red_pink, size = 5) 


d_pc <- ggplot(data = data.frame(x = 0), mapping = aes(x = x)) +
  scale_x_continuous(limits = c(0, 10.5), expand = c(0, 0), breaks = NULL) +
  scale_y_continuous(limits = c(0, 10.5), expand = c(0, 0), breaks = NULL) +
  theme_market +
  labs(x = "Q", y = "P", title = "Demand under PC") +
  geom_hline(yintercept = 5, color = red_pink, size = 1) +
  annotate("text", label = "D", x = 10, y = 5.5, color = red_pink, size = 5)

fig_d <- grid.arrange(d_pc, d_m, nrow = 1)
fig_d

ggsave(plot = fig_d, filename = "D compare.png", device = "png", height = 4.36, width = 10)

#####
demand <- function(x) 10 - x
inv_demand <- function(x) 10 - x
p_1 <- 7
p_2 <- 9
q_1 <- inv_demand(p_1) 
q_2 <- inv_demand(p_2)
ggplot(data = data.frame(x = 0), mapping = aes(x = x)) +
  scale_x_continuous(limits = c(0, 10.5), expand = c(0, 0), breaks = seq(0, 10, 1)) +
  scale_y_continuous(limits = c(0, 10.5), expand = c(0, 0), breaks = seq(0, 10, 1)) +
  theme_market +
  labs(x = "Q", y = "P") +
  geom_rect(aes(xmin = 0, xmax = q_2, ymin = p_1, ymax = p_2), alpha = 0.2, fill = red_pink) +
  geom_rect(aes(xmin = q_1, xmax = q_2, ymin = 0, ymax = p_1, fill = "Output"), alpha = 0.2) +
  stat_function(fun = demand, color = red_pink, size = 1) +
  geom_point(x = q_1, y = p_1, color = met_slate, size = 2) +
  geom_point(x = q_2, y = p_2, color = met_slate, size = 2) +
  geom_segment(x = 0, xend = q_1, y = p_1, yend = p_1, color = met_slate, linetype = "dashed", size = 1) +
  geom_segment(x = 0, xend = q_2, y = p_2, yend = p_2, color = met_slate, linetype = "dashed", size = 1) +
  geom_segment(x = q_1, xend = q_1, y = 0, yend = p_1, color = met_slate, linetype = "dashed", size = 1) +
  geom_segment(x = q_2, xend = q_2, y = 0, yend = p_2, color = met_slate, linetype = "dashed", size = 1) +
  annotate("text", label = "D", x = 10, y = 0.65, color = red_pink, size = 6) +
  scale_fill_manual(values = c("Price Effect"=red_pink, "Output"=purple), 
                    labels = c("Price Effect", "Output Effect"),
                    name = "Effect")

ggsave(filename = "Price eff 1.png", device = "png", height = 4.36, width = 6)


demand <- function(x) 10 - x
inv_demand <- function(x) 10 - x
p_1 <- 4
p_2 <- 6
q_1 <- inv_demand(p_1) 
q_2 <- inv_demand(p_2)
ggplot(data = data.frame(x = 0), mapping = aes(x = x)) +
  scale_x_continuous(limits = c(0, 10.5), expand = c(0, 0), breaks = seq(0, 10, 1)) +
  scale_y_continuous(limits = c(0, 10.5), expand = c(0, 0), breaks = seq(0, 10, 1)) +
  theme_market +
  labs(x = "Q", y = "P") +
  geom_rect(aes(xmin = 0, xmax = q_2, ymin = p_1, ymax = p_2), alpha = 0.2, fill = red_pink) +
  geom_rect(aes(xmin = q_1, xmax = q_2, ymin = 0, ymax = p_1, fill = "Output"), alpha = 0.2) +
  stat_function(fun = demand, color = red_pink, size = 1) +
  geom_point(x = q_1, y = p_1, color = met_slate, size = 2) +
  geom_point(x = q_2, y = p_2, color = met_slate, size = 2) +
  geom_segment(x = 0, xend = q_1, y = p_1, yend = p_1, color = met_slate, linetype = "dashed", size = 1) +
  geom_segment(x = 0, xend = q_2, y = p_2, yend = p_2, color = met_slate, linetype = "dashed", size = 1) +
  geom_segment(x = q_1, xend = q_1, y = 0, yend = p_1, color = met_slate, linetype = "dashed", size = 1) +
  geom_segment(x = q_2, xend = q_2, y = 0, yend = p_2, color = met_slate, linetype = "dashed", size = 1) +
  annotate("text", label = "D", x = 10, y = 0.65, color = red_pink, size = 6) +
  scale_fill_manual(values = c("Price Effect"=red_pink, "Output"=purple), 
                    labels = c("Price Effect", "Output Effect"),
                    name = "Effect")

ggsave(filename = "Price eff 2.png", device = "png", height = 4.36, width = 6)

p_1 <- 1
p_2 <- 3
q_1 <- inv_demand(p_1) 
q_2 <- inv_demand(p_2)
ggplot(data = data.frame(x = 0), mapping = aes(x = x)) +
  scale_x_continuous(limits = c(0, 10.5), expand = c(0, 0), breaks = seq(0, 10, 1)) +
  scale_y_continuous(limits = c(0, 10.5), expand = c(0, 0), breaks = seq(0, 10, 1)) +
  theme_market +
  labs(x = "Q", y = "P") +
  geom_rect(aes(xmin = 0, xmax = q_2, ymin = p_1, ymax = p_2), alpha = 0.2, fill = red_pink) +
  geom_rect(aes(xmin = q_1, xmax = q_2, ymin = 0, ymax = p_1, fill = "Output"), alpha = 0.2) +
  stat_function(fun = demand, color = red_pink, size = 1) +
  geom_point(x = q_1, y = p_1, color = met_slate, size = 2) +
  geom_point(x = q_2, y = p_2, color = met_slate, size = 2) +
  geom_segment(x = 0, xend = q_1, y = p_1, yend = p_1, color = met_slate, linetype = "dashed", size = 1) +
  geom_segment(x = 0, xend = q_2, y = p_2, yend = p_2, color = met_slate, linetype = "dashed", size = 1) +
  geom_segment(x = q_1, xend = q_1, y = 0, yend = p_1, color = met_slate, linetype = "dashed", size = 1) +
  geom_segment(x = q_2, xend = q_2, y = 0, yend = p_2, color = met_slate, linetype = "dashed", size = 1) +
  annotate("text", label = "D", x = 10, y = 0.65, color = red_pink, size = 6) +
  scale_fill_manual(values = c("Price Effect"=red_pink, "Output"=purple), 
                    labels = c("Price Effect", "Output Effect"),
                    name = "Effect")

ggsave(filename = "Price eff 3.png", device = "png", height = 4.36, width = 6)

#####
x <- seq(0, 10.5, 0.1)
demand <- function(x) 10 - x
inv_demand <- function(x) 10 - x
marginal_revenue <- function(x) 10 - 2*x

ggplot() +
  scale_x_continuous(limits = c(0, 10.5), expand = c(0, 0), breaks = seq(0, 10.5, 1)) +
  scale_y_continuous(limits = c(0, 10.5), expand = c(0, 0), breaks = seq(0, 10.5, 1)) +
  theme_market +
  labs(x = "Q", y = "P") +
  geom_line(aes(x=x, y=demand(x)), size = 1, color = red_pink) +
  geom_line(aes(x=x, y=marginal_revenue(x)), size = 1, color = "blue") +
  annotate("text", label = "D", x = 10, y = 0.65, color = red_pink, size = 5) +
  annotate("text", label = "MR", x = 5.5, y = 0.65, color = "blue", size = 5) 

ggsave(filename = "mr and d.png", device = "png", height = 4.36, width = 6)


####
x <- seq(0, 8, 0.1)
atc <- function(x) (20 + 12*x - 2*x^2 + (2/3)*x^3)/x
marginal_cost <- function(x) 12 - 4*x + 2*x^2
demand <- function(x) 37 - 5*x
marginal_revenue <- function(x) 37 - 10*x
q <- uniroot(function(x) marginal_revenue(x) - marginal_cost(x), range(x))$root
not_p <- marginal_revenue(q)
p <- demand(q)
cost <- atc(q)
z <- seq(0, q, 0.001)
p_max_1 <- ggplot() +
  scale_x_continuous(limits = c(0, 7.5), expand=c(0,0), breaks = c(q), labels = c(4)) +
  scale_y_continuous(limits = c(0, 37.5), expand=c(0,0), breaks = c(cost, p), labels = c(10, 16)) +
  theme_market +
  labs(x = "Q", y = "$") +
  geom_line(aes(x=x, y=atc(x)), size = 1, color = purple) +
  annotate("text", label = "ATC", x = 6.5, y = 35, color = purple, size = 6) +
  geom_line(aes(x=x, y=marginal_cost(x)), size = 1, color = green) +
  annotate("text", label = "MC", x = 1, y = 8, color = green, size = 6) +
  geom_line(aes(x=x, y=demand(x)), size = 1, color = red_pink) +
  geom_line(aes(x=x, y=marginal_revenue(x)), size = 1, color = "blue") +
  annotate("text", label = "D", x = 6.25, y = 8, color = red_pink, size = 6) +
  annotate("text", label = "MR", x = 3.5, y = 8, color = "blue", size = 6) +
  geom_point(aes(x = q, y = not_p), size = 3.5) +
  geom_segment(aes(x = q, xend = q, y = 0, yend = not_p), linetype = "dashed", size = 1)
p_max_1

ggsave(filename = "mono 1.png", device = "png", height = 4.36, width = 6)

p_max_2 <- p_max_1 +
  geom_point(aes(x = q, y = p), size = 3.5) +
  geom_segment(aes(x = q, xend = q, y = not_p, yend = p), linetype = "dashed", size = 1) +
  geom_segment(aes(x = 0, xend = q, y = p, yend = p), linetype = "dashed", size = 1)
p_max_2

ggsave(filename = "mono 2.png", device = "png", height = 4.36, width = 6)

p_max_3 <- p_max_2 +
  geom_point(aes(x = q, y = cost), size = 3.5) +
  geom_segment(aes(x = 0, xend = q, y = cost, yend = cost), linetype = "dashed", size = 1) +
  geom_ribbon(aes(x = z, ymin = cost, ymax = p), fill = green, alpha = 0.2, linetype = "blank") +
  annotate("text", label = "Profit", x = q/3+0.2, y = (p + cost)/2+0.2, color = green, size = 6) 
p_max_3

ggsave(filename = "mono 3.png", device = "png", height = 4.36, width = 6)

####

x <- seq(0.1,36,0.1)

xlim <- max(x)-4
ylim <- max(x)+2
tc <- function(x) (1/8)*(x^3)-4*(x^2)+48*x
vc <- function(x) tc(x)

atc <- function(x) tc(x)/x
avc <- function(x) vc(x)/x

mc <- function(x) (3/8)*(x^2)-8*x+48

demand <- function(x) 36-1.25*x
mr <- function(x) 36-2.5*x

z <- seq(6,32,0.1)

q <- uniroot(function(z) mr(z) - mc(z), range(z))$root
p <- mc(q)

mono1 <- ggplot() +
  scale_x_continuous(limits = c(0, xlim), expand=c(0,0), breaks = seq(0,xlim,4)) +
  scale_y_continuous(limits = c(0, ylim), expand=c(0,0), breaks = seq(0,ylim,2)) +
  theme_sd +
  labs(x = "Q", y = "$") +
  geom_line(aes(x=x, y=atc(x)), size = 1, color = purple) +
  annotate("text", label = "ATC", x = 30, y = 34, color = purple, size = 6) +
  # geom_line(aes(x=x, y=avc(x)), size = 1, color = orange) +
  # annotate("text", label = "AVC", x = 30, y = 34, color = orange, size = 6) +
  geom_line(aes(x=z, y=mc(z)), size = 1, color = green) +
  annotate("text", label = "MC", x = 21, y = 34, color = green, size = 6) +
  geom_line(aes(x=x, y=demand(x)), size = 1, color = red_pink) +
  annotate("text", label = "D", x = 29, y = 3, color = red_pink, size = 6) +
  geom_line(aes(x=x, y=mr(x)), size = 1, color = "blue") +
  annotate("text", label = "MR", x = 15, y = 3, color = "blue", size = 6)# +
  # annotate("point", x = q, y= p, size = 3.5) +
  # annotate("segment", x = q, xend = q, y = 0, yend = p, linetype = 2, size = 1) +
  # annotate("segment", x = 0, xend = q, y = p, yend = p, linetype = 2, size = 1)
mono1

ggsave(filename = "mono ex 1.png", device = "png", height = 4.36, width = 6)

mono1 +  
  annotate("point", x = q, y= p, size = 3.5) +
  annotate("segment", x = q, xend = q, y = 0, yend = p, linetype = 2, size = 1) +
  annotate("segment", x = 0, xend = q, y = p, yend = p, linetype = 2, size = 1)

ggsave(filename = "mono ex 2.png", device = "png", height = 4.36, width = 6)


mono1 +  
  annotate("point", x = q, y= p, size = 3.5) +
  annotate("segment", x = q, xend = q, y = 0, yend = demand(q), linetype = 2, size = 1) +
  annotate("segment", x = 0, xend = q, y = p, yend = p, linetype = 2, size = 1) +
  
  annotate("point", x = q, y = demand(q), size = 3.5) +
  annotate("point", x = q, y = atc(q), size = 3.5) +
  annotate("segment", x = 0, xend = q, y = demand(q), yend = demand(q), linetype = 2, size = 1) +
  annotate("segment", x = 0, xend = q, y = atc(q), yend = atc(q), linetype = 2, size = 1) +
  geom_ribbon(aes(x = 0:12, ymin = atc(q), ymax = demand(q)), fill = green, alpha = 0.2, linetype = "blank") +
  annotate("text", label = "Profit", x = 4, y = 19.75, color = green, size = 5) 

ggsave(filename = "mono ex 3.png", device = "png", height = 4.36, width = 6)



#####
x <- seq(0.1,36,0.1)

xlim <- max(x)-4
ylim <- max(x)+2
tc <- function(x) (1/8)*(x^3)-4*(x^2)+48*x
vc <- function(x) tc(x)

atc <- function(x) tc(x)/x
avc <- function(x) vc(x)/x

mc <- function(x) (5/6)*x+(8/3)

demand <- function(x) 36-1.25*x
mr <- function(x) 36-2.5*x

#z <- seq(6,32,0.1)

q <- uniroot(function(x) mr(x) - mc(x), range(x))$root
p <- mc(q)

pcm1 <- ggplot() +
  scale_x_continuous(limits = c(0, xlim), expand=c(0,0), breaks = seq(0,xlim,4)) +
  scale_y_continuous(limits = c(0, ylim), expand=c(0,0), breaks = seq(0,ylim,2)) +
  theme_sd +
  labs(x = "Q", y = "$") +
  # geom_line(aes(x=x, y=atc(x)), size = 1, color = purple) +
  # annotate("text", label = "ATC", x = 30, y = 34, color = purple, size = 6) +
  # geom_line(aes(x=x, y=avc(x)), size = 1, color = orange) +
  # annotate("text", label = "AVC", x = 30, y = 34, color = orange, size = 6) +
  geom_line(aes(x=x, y=mc(x)), size = 1, color = purple) +
  annotate("text", label = "S~MC", x = 29, y = 24, color = purple, size = 6) +
  geom_line(aes(x=x, y=demand(x)), size = 1, color = red_pink) +
  annotate("text", label = "D", x = 29, y = 3, color = red_pink, size = 6)# +
  # geom_line(aes(x=x, y=mr(x)), size = 1, color = "blue") +
  # annotate("text", label = "MR", x = 15, y = 3, color = "blue", size = 6)# +
# annotate("point", x = q, y= p, size = 3.5) +
# annotate("segment", x = q, xend = q, y = 0, yend = p, linetype = 2, size = 1) +
# annotate("segment", x = 0, xend = q, y = p, yend = p, linetype = 2, size = 1)
pcm1

ggsave(filename = "pcm 1.png", device = png)

pcm1 +   
  geom_ribbon(aes(x = 0:16, ymin = 16, ymax = demand(0:16)), fill = red_pink, alpha = 0.2) +
  annotate("text", label = "CS", x = 4, y = 22, color = red_pink, size = 6) + 
  geom_ribbon(aes(x = 0:16, ymin = mc(0:16), ymax = 16), fill = purple, alpha = 0.2) +
  annotate("text", label = "PS", x = 4, y = 12, color = purple, size = 6) +
  annotate("point", x = 16, y= 16, size = 3.5) +
  annotate("segment", x = 16, xend = 16, y = 0, yend = 16, linetype = 2, size = 1) +
  annotate("segment", x = 0, xend = 16, y = 16, yend = 16, linetype = 2, size = 1)
  
ggsave(filename = "pcm 2.png", device = png)

pcm2 <- ggplot() +
  scale_x_continuous(limits = c(0, xlim), expand=c(0,0), breaks = seq(0,xlim,4)) +
  scale_y_continuous(limits = c(0, ylim), expand=c(0,0), breaks = seq(0,ylim,2)) +
  theme_sd +
  labs(x = "Q", y = "$") +
  geom_line(aes(x=x, y=atc(x)), size = 1, color = purple) +
  annotate("text", label = "ATC", x = 30, y = 34, color = purple, size = 6) +
  # geom_line(aes(x=x, y=avc(x)), size = 1, color = orange) +
  # annotate("text", label = "AVC", x = 30, y = 34, color = orange, size = 6) +
  geom_line(aes(x=x, y=mc(x)), size = 1, color = green) +
  annotate("text", label = "MC", x = 29, y = 24, color = green, size = 6) +
  geom_line(aes(x=x, y=demand(x)), size = 1, color = red_pink) +
  annotate("text", label = "D", x = 29, y = 3, color = red_pink, size = 6) +
geom_line(aes(x=x, y=mr(x)), size = 1, color = "blue") +
annotate("text", label = "MR", x = 15, y = 3, color = "blue", size = 6)# +
# annotate("point", x = q, y= p, size = 3.5) +
# annotate("segment", x = q, xend = q, y = 0, yend = p, linetype = 2, size = 1) +
# annotate("segment", x = 0, xend = q, y = p, yend = p, linetype = 2, size = 1)
pcm2

ggsave(filename = "pcm 3.png", device = png)

pcm2 + annotate("point", x = 10, y= 11, size = 3.5) +
  annotate("segment", x = 10, xend = 10, y = 0, yend = 23.5, linetype = 2, size = 1) +
  annotate("segment", x = 0, xend = 10, y = 11, yend = 11, linetype = 2, size = 1) +
  
  annotate("point", x = 10, y= 23.5, size = 3.5) +
  annotate("point", x = 10, y= 20.5, size = 3.5) +
  annotate("segment", x = 0, xend = 10, y = 23.5, yend = 23.5, linetype = 2, size = 1) +
  annotate("segment", x = 0, xend = 10, y = 20.5, yend = 20.5, linetype = 2, size = 1)

ggsave(filename = "pcm 4.png", device = png)

pcm2 + annotate("point", x = 10, y= 11, size = 3.5) +
  annotate("segment", x = 10, xend = 10, y = 0, yend = 23.5, linetype = 2, size = 1) +
  annotate("segment", x = 0, xend = 10, y = 11, yend = 11, linetype = 2, size = 1) +
  
  annotate("point", x = 10, y= 23.5, size = 3.5) +
  annotate("point", x = 10, y= 20.5, size = 3.5) +
  annotate("segment", x = 0, xend = 10, y = 23.5, yend = 23.5, linetype = 2, size = 1) +
  annotate("segment", x = 0, xend = 10, y = 20.5, yend = 20.5, linetype = 2, size = 1) +
  geom_ribbon(aes(x = 0:10, ymin = 23.5, ymax = demand(0:10)), fill = red_pink, alpha = 0.2) +
  annotate("text", label = "CS", x = 2, y = 27, color = red_pink, size = 6) #+
  # geom_ribbon(aes(x = 0:10, ymin = mc(0:10), ymax = 23.5), fill = purple, alpha = 0.2) +
  # annotate("text", label = "PS", x = 4, y = 12, color = purple, size = 6)

ggsave(filename = "pcm 5.png", device = png)

pcm2 + annotate("point", x = 10, y= 11, size = 3.5) +
  annotate("segment", x = 10, xend = 10, y = 0, yend = 23.5, linetype = 2, size = 1) +
  annotate("segment", x = 0, xend = 10, y = 11, yend = 11, linetype = 2, size = 1) +
  
  annotate("point", x = 10, y= 23.5, size = 3.5) +
  annotate("point", x = 10, y= 20.5, size = 3.5) +
  annotate("segment", x = 0, xend = 10, y = 23.5, yend = 23.5, linetype = 2, size = 1) +
  annotate("segment", x = 0, xend = 10, y = 20.5, yend = 20.5, linetype = 2, size = 1) +
  # geom_ribbon(aes(x = 0:10, ymin = 23.5, ymax = demand(0:10)), fill = red_pink, alpha = 0.2) +
  # annotate("text", label = "CS", x = 2, y = 27, color = red_pink, size = 6) #+
  geom_ribbon(aes(x = 0:10, ymin = mc(0:10), ymax = 23.5), fill = purple, alpha = 0.2) +
  annotate("text", label = "PS", x = 2, y = 16, color = purple, size = 6)

ggsave(filename = "pcm 6.png", device = png)


pcm2 + annotate("point", x = 10, y= 11, size = 3.5) +
  annotate("segment", x = 10, xend = 10, y = 0, yend = 23.5, linetype = 2, size = 1) +
  annotate("segment", x = 0, xend = 10, y = 11, yend = 11, linetype = 2, size = 1) +
  
  annotate("point", x = 10, y= 23.5, size = 3.5) +
  annotate("point", x = 10, y= 20.5, size = 3.5) +
  annotate("segment", x = 0, xend = 10, y = 23.5, yend = 23.5, linetype = 2, size = 1) +
  annotate("segment", x = 0, xend = 10, y = 20.5, yend = 20.5, linetype = 2, size = 1) +
  geom_ribbon(aes(x = 0:10, ymin = 23.5, ymax = demand(0:10)), fill = red_pink, alpha = 0.2) +
  annotate("text", label = "CS", x = 2, y = 27, color = red_pink, size = 6) +
  geom_ribbon(aes(x = 0:10, ymin = mc(0:10), ymax = 23.5), fill = purple, alpha = 0.2) +
  annotate("text", label = "PS", x = 2, y = 16, color = purple, size = 6) +
  geom_ribbon(aes(x = 10:16, ymin = mc(10:16), ymax = demand(10:16)), fill = "purple", alpha = 0.2) +
  annotate("text", label = "DWL", x = 12, y = 16, color = "purple", size = 6)

ggsave(filename = "pcm 7.png", device = png)


####

x <- 0:10
marginal_cost <- function(x) 2 + 1.25*x
demand <- function(x) 10 - x
marginal_revenue <- function(x) 10 - 2*x
q <- uniroot(function(x) marginal_revenue(x) - marginal_cost(x), range(x))$root
not_p <- marginal_revenue(q)
p <- demand(q)
q_c <- uniroot(function(x) demand(x) - marginal_cost(x), range(x))$root
p_c <- demand(q_c)
w <- seq(0, q, 0.01)
w_2 <- seq(q, q_c, 0.01)
ggplot() +
  scale_x_continuous(limits = c(0, 10.5), expand = c(0, 0), breaks = c(q, q_c), labels = c(expression(Q[M]), expression(Q[C]))) +
  scale_y_continuous(limits = c(0, 10.5), expand = c(0, 0), breaks = c(p_c, p), labels = c(expression(P[C]), expression(P[M]))) +
  theme_market +
  labs(x = "Q", y = "P") +
  #stat_function(aes(x), fun = marginal_cost, color = purple, size = 1) +
  geom_line(aes(x=x, y=marginal_cost(x)), size = 1, color = green) +
  annotate("text", label = "MC", x = 7.25, y = 9.65, color = green, size = 6) +
  #stat_function(aes(x), fun = demand, color = red_pink, size = 1) +
  geom_line(aes(x=x, y=demand(x)), size = 1, color = red_pink) +
  #stat_function(aes(x), fun = marginal_revenue, color = green, size = 1) +
  geom_line(aes(x=x, y=marginal_revenue(x)), size = 1, color = "blue") +
  annotate("text", label = "D", x = 10.1, y = 0.65, color = red_pink, size = 6) +
  annotate("text", label = "MR", x = 5.75, y = 0.65, color = "blue", size = 6) +
  geom_point(aes(x = q_c, y = p_c), size = 2) +
  geom_segment(aes(x = 0, xend = q_c, y = p_c, yend = p_c), linetype = "dashed", size = 1) +
  geom_segment(aes(x = q_c, xend = q_c, y = 0, yend = p_c), linetype = "dashed", size = 1) +
  geom_point(aes(x = q, y = p), size = 2) +
  geom_point(aes(x = q, y = not_p), size = 2) +
  geom_segment(aes(x = q, xend = q, y = 0, yend = p), linetype = "dashed", size = 1) +
  geom_segment(aes(x = 0, xend = q, y = p, yend = p), linetype = "dashed", size = 1)

ggsave(filename = "price ceil 1.png", device = png)

z <- seq(q_c, 10, 0.01)
not_p2 <- marginal_revenue(q_c)
ggplot() +
  scale_x_continuous(limits = c(0, 10.5), expand = c(0, 0), breaks = c(q_c), labels = c()) +
  scale_y_continuous(limits = c(0, 10.5), expand = c(0, 0), breaks = c(p_c), labels = c(expression(P[M]))) +
  theme_market + theme(axis.text=element_text(size=14, color = "black")) +
  labs(x = "Q", y = "P") +
  #stat_function(aes(x), fun = marginal_cost, color = purple, size = 1) +
  geom_line(aes(x=x, y=marginal_cost(x)), size = 1, color = green) +
  annotate("text", label = "MC", x = 7.25, y = 9.65, color = green, size = 6) +
  #stat_function(aes(x), fun = demand, color = red_pink, size = 1) +
  geom_line(aes(x=x, y=demand(x)), size = 1, color = red_pink) +
  #stat_function(aes(x), fun = marginal_revenue, color = green, size = 1) +
  #geom_line(aes(x=z, y=marginal_revenue(z)), size = 1.5, color = "blue") +
  #geom_segment(aes(x = q_c, y = p_c, xend = q_c, yend = not_p2), color = "blue", size = 1.5) +
  #geom_segment(aes(x = 0, y = p_c, xend = q_c, yend = p_c), color = "blue", size = 1.5) +
  annotate("text", label = "D", x = 10.1, y = 0.65, color = red_pink,size = 6) +
  #annotate("text", label = "MR", x = 5.75, y = 0.65, color = "blue", size = 6) +
  #geom_point(aes(x = q_c, y = p_c), size = 2) +
  #geom_segment(aes(x = q_c, xend = q_c, y = 0, yend = p_c), linetype = "dashed", size = 1) +
  geom_hline(yintercept = p_c, size = 1) +
  annotate("text", label = "Price Ceiling", x = 9, y = p_c - 0.3, size = 5) #+
  # geom_ribbon(aes(x = seq(0,q_c,0.1), ymin = marginal_cost(seq(0,q_c,0.1)), ymax = p_c), fill = purple, alpha = 0.2, linetype = "blank") + # PS
  # geom_ribbon(aes(x = seq(0,q_c,0.1), ymin = p_c, ymax = demand(seq(0,q_c,0.1))), fill = red_pink, alpha = 0.2, linetype = "blank") +
  # annotate("text", label = "PS", x = q/3, y = p_c - 1, color = purple,  size = 5) +
  # annotate("text", label = "CS", x = q/3, y = p_c + 1, color = red_pink, size = 5)

ggsave(filename = "price ceil 2.png", device = png)

ggplot() +
  scale_x_continuous(limits = c(0, 10.5), expand = c(0, 0), breaks = c(q_c), labels = c()) +
  scale_y_continuous(limits = c(0, 10.5), expand = c(0, 0), breaks = c(p_c), labels = c(expression(P[M]))) +
  theme_market + theme(axis.text=element_text(size=14, color = "black")) +
  labs(x = "Q", y = "P") +
  #stat_function(aes(x), fun = marginal_cost, color = purple, size = 1) +
  geom_line(aes(x=x, y=marginal_cost(x)), size = 1, color = green) +
  annotate("text", label = "MC", x = 7.25, y = 9.65, color = green, size = 6) +
  #stat_function(aes(x), fun = demand, color = red_pink, size = 1) +
  geom_line(aes(x=x, y=demand(x)), size = 1, color = red_pink) +
  #stat_function(aes(x), fun = marginal_revenue, color = green, size = 1) +
  geom_line(aes(x=z, y=marginal_revenue(z)), size = 1.5, color = "blue") +
  geom_segment(aes(x = q_c, y = p_c, xend = q_c, yend = not_p2), color = "blue", size = 1.5, linetype = "dashed") +
  geom_segment(aes(x = 0, y = p_c, xend = q_c, yend = p_c), color = "blue", size = 1.5) +
  annotate("text", label = "D", x = 10.1, y = 0.65, color = red_pink,size = 6) +
  annotate("text", label = "MR", x = 5.75, y = 0.65, color = "blue", size = 6) +
  #geom_point(aes(x = q_c, y = p_c), size = 2) +
  #geom_segment(aes(x = q_c, xend = q_c, y = 0, yend = p_c), linetype = "dashed", size = 1) +
  geom_hline(yintercept = p_c, size = 1) +
  annotate("text", label = "Price Ceiling", x = 9, y = p_c - 0.3, size = 5) #+
# geom_ribbon(aes(x = seq(0,q_c,0.1), ymin = marginal_cost(seq(0,q_c,0.1)), ymax = p_c), fill = purple, alpha = 0.2, linetype = "blank") + # PS
# geom_ribbon(aes(x = seq(0,q_c,0.1), ymin = p_c, ymax = demand(seq(0,q_c,0.1))), fill = red_pink, alpha = 0.2, linetype = "blank") +
# annotate("text", label = "PS", x = q/3, y = p_c - 1, color = purple,  size = 5) +
# annotate("text", label = "CS", x = q/3, y = p_c + 1, color = red_pink, size = 5)

ggsave(filename = "price ceil 3.png", device = png)

ggplot() +
  scale_x_continuous(limits = c(0, 10.5), expand = c(0, 0), breaks = c(q_c), labels = c(expression(Q[C]==Q[M]))) +
  scale_y_continuous(limits = c(0, 10.5), expand = c(0, 0), breaks = c(p_c), labels = c(expression(P[M]))) +
  theme_market + theme(axis.text=element_text(size=14, color = "black")) +
  labs(x = "Q", y = "P") +
  #stat_function(aes(x), fun = marginal_cost, color = purple, size = 1) +
  geom_line(aes(x=x, y=marginal_cost(x)), size = 1, color = green) +
  annotate("text", label = "MC", x = 7.25, y = 9.65, color = green, size = 6) +
  #stat_function(aes(x), fun = demand, color = red_pink, size = 1) +
  geom_line(aes(x=x, y=demand(x)), size = 1, color = red_pink) +
  #stat_function(aes(x), fun = marginal_revenue, color = green, size = 1) +
  geom_line(aes(x=z, y=marginal_revenue(z)), size = 1.5, color = "blue") +
  geom_segment(aes(x = q_c, y = p_c, xend = q_c, yend = not_p2), color = "blue", size = 1.5) +
  geom_segment(aes(x = 0, y = p_c, xend = q_c, yend = p_c), color = "blue", size = 1.5) +
  annotate("text", label = "D", x = 10.1, y = 0.65, color = red_pink,size = 6) +
  annotate("text", label = "MR", x = 5.75, y = 0.65, color = "blue", size = 6) +
  geom_point(aes(x = q_c, y = p_c), size = 2) +
  geom_segment(aes(x = q_c, xend = q_c, y = 0, yend = p_c), linetype = "dashed", size = 1) +
  geom_hline(yintercept = p_c, size = 1) +
  annotate("text", label = "Price Ceiling", x = 9, y = p_c - 0.3, size = 5) #+
# geom_ribbon(aes(x = seq(0,q_c,0.1), ymin = marginal_cost(seq(0,q_c,0.1)), ymax = p_c), fill = purple, alpha = 0.2, linetype = "blank") + # PS
# geom_ribbon(aes(x = seq(0,q_c,0.1), ymin = p_c, ymax = demand(seq(0,q_c,0.1))), fill = red_pink, alpha = 0.2, linetype = "blank") +
# annotate("text", label = "PS", x = q/3, y = p_c - 1, color = purple,  size = 5) +
# annotate("text", label = "CS", x = q/3, y = p_c + 1, color = red_pink, size = 5)

ggsave(filename = "price ceil 4.png", device = png)

z <- seq(q_c, 10, 0.01)
not_p2 <- marginal_revenue(q_c)
ggplot() +
  scale_x_continuous(limits = c(0, 10.5), expand = c(0, 0), breaks = c(q_c), labels = c(expression(Q[C]==Q[M]))) +
  scale_y_continuous(limits = c(0, 10.5), expand = c(0, 0), breaks = c(p_c), labels = c(expression(P[M]))) +
  theme_market + theme(axis.text=element_text(size=14, color = "black")) +
  labs(x = "Q", y = "P") +
  #stat_function(aes(x), fun = marginal_cost, color = purple, size = 1) +
  geom_line(aes(x=x, y=marginal_cost(x)), size = 1, color = green) +
  annotate("text", label = "MC", x = 7.25, y = 9.65, color = green, size = 6) +
  #stat_function(aes(x), fun = demand, color = red_pink, size = 1) +
  geom_line(aes(x=x, y=demand(x)), size = 1, color = red_pink) +
  #stat_function(aes(x), fun = marginal_revenue, color = green, size = 1) +
  geom_line(aes(x=z, y=marginal_revenue(z)), size = 1.5, color = "blue") +
  geom_segment(aes(x = q_c, y = p_c, xend = q_c, yend = not_p2), color = "blue", size = 1.5) +
  geom_segment(aes(x = 0, y = p_c, xend = q_c, yend = p_c), color = "blue", size = 1.5) +
  annotate("text", label = "D", x = 10.1, y = 0.65, color = red_pink,size = 6) +
  annotate("text", label = "MR", x = 5.75, y = 0.65, color = "blue", size = 6) +
  geom_point(aes(x = q_c, y = p_c), size = 2) +
  geom_segment(aes(x = q_c, xend = q_c, y = 0, yend = p_c), linetype = "dashed", size = 1) +
  geom_hline(yintercept = p_c, size = 1) +
  annotate("text", label = "Price Ceiling", x = 9, y = p_c - 0.3, size = 5) +
  geom_ribbon(aes(x = seq(0,q_c,0.1), ymin = marginal_cost(seq(0,q_c,0.1)), ymax = p_c), fill = purple, alpha = 0.2, linetype = "blank") + # PS
  geom_ribbon(aes(x = seq(0,q_c,0.1), ymin = p_c, ymax = demand(seq(0,q_c,0.1))), fill = red_pink, alpha = 0.2, linetype = "blank") +
  annotate("text", label = "PS", x = q/3, y = p_c - 1, color = purple,  size = 5) +
  annotate("text", label = "CS", x = q/3, y = p_c + 1, color = red_pink, size = 5)

ggsave(filename = "price ceil 5.png", device = png)


####

x <- 0:10
marginal_cost <- function(x) 1 + x
demand <- function(x) 10 - x
marginal_revenue <- function(x) 10 - 2*x
q <- uniroot(function(x) marginal_revenue(x) - marginal_cost(x), range(x))$root
not_p <- marginal_revenue(q)
p <- demand(q)
q_c <- uniroot(function(x) demand(x) - marginal_cost(x), range(x))$root
p_c <- demand(q_c)
marginal_cost_2 <- function(x) 4 + x
q_2 <- uniroot(function(x) marginal_revenue(x) - marginal_cost_2(x), range(x))$root
not_p_2 <- marginal_revenue(q_2)
p_2 <- demand(q_2)
w_2 <- seq(q_2, q_c, 0.01)

ggplot() +
  scale_x_continuous(limits = c(0, 10.5), expand = c(0, 0), breaks = c(q_2, q_c), labels = c(expression(Q[M]), expression(Q[C]))) +
  scale_y_continuous(limits = c(0, 10.5), expand = c(0, 0), breaks = c(p_c, p_2), labels = c(expression(P[C]), expression(P[M]))) +
  theme_market +
  labs(x = "Q", y = "P") +
  #stat_function(aes(x), fun = marginal_cost, color = purple, size = 1, alpha = 0.5) +
  geom_line(aes(x=x, y=marginal_cost(x)), size = 1, color = green) +
  annotate("text", label = expression(MC), x = 9.75, y = 9.65, color = green, size = 6, alpha = 0.5) +
  
  #stat_function(aes(x), fun = marginal_cost_2, color = gree, size = 1) +
  geom_line(aes(x=x, y=marginal_cost_2(x)), size = 1, color = green) +
  annotate("text", label = expression(MC[Tax]), x = 7.1, y = 9.65, color = green, size = 6) +
  
  #stat_function(aes(x), fun = demand, color = red_pink, size = 1) +
  geom_line(aes(x=x, y=demand(x)), size = 1, color = red_pink) +
  
  #stat_function(aes(x), fun = marginal_revenue, color = green, size = 1) +
  geom_line(aes(x=x, y=marginal_revenue(x)), size = 1, color = "blue") +
  
  annotate("text", label = "D", x = 10.1, y = 0.65, color = red_pink, size = 6) +
  annotate("text", label = "MR", x = 5.75, y = 0.65, color = green, size = 6) +
  geom_point(aes(x = q_c, y = p_c), color = met_slate, size = 2) +
  geom_segment(aes(x = 0, xend = q_c, y = p_c, yend = p_c), color = met_slate, linetype = "dashed", size = 1) +
  geom_segment(aes(x = q_c, xend = q_c, y = 0, yend = p_c), color = met_slate, linetype = "dashed", size = 1) +
  
  geom_point(aes(x = q, y = p), color = met_slate, size = 2, alpha = 0.5) +
  geom_point(aes(x = q, y = not_p), color = met_slate, size = 2, alpha = 0.5) +
  geom_segment(aes(x = q, xend = q, y = 0, yend = p), color = met_slate, linetype = "dashed", size = 1, alpha = 0.5) +
  geom_segment(aes(x = 0, xend = q, y = p, yend = p), color = met_slate, linetype = "dashed", size = 1, alpha = 0.5) +
  
  geom_point(aes(x = q_2, y = p_2), color = met_slate, size = 2) +
  geom_point(aes(x = q_2, y = not_p_2), color = met_slate, size = 2) +
  geom_segment(aes(x = q_2, xend = q_2, y = 0, yend = p_2), color = met_slate, linetype = "dashed", size = 1) +
  geom_segment(aes(x = 0, xend = q_2, y = p_2, yend = p_2), color = met_slate, linetype = "dashed", size = 1) +
  
  geom_ribbon(aes(x = w_2, ymin = marginal_cost(w_2), ymax = p_c), fill = orange, alpha = 0.2) +
  geom_ribbon(aes(x = w_2, ymin = p_c, ymax = demand(w_2)), fill = orange, alpha = 0.2) 


ggplot() +
  scale_x_continuous(limits = c(0, 10.5), expand = c(0, 0), breaks = c(q_2, q_c), labels = c(expression(Q[M]), expression(Q[C]))) +
  scale_y_continuous(limits = c(0, 10.5), expand = c(0, 0), breaks = c(p_c, p_2), labels = c(expression(P[C]), expression(P[M]))) +
  theme_market +
  labs(x = "Q", y = "P") +
  #stat_function(aes(x), fun = marginal_cost, color = purple, size = 1, alpha = 0.5) +
  geom_line(aes(x=x, y=marginal_cost(x)), size = 1, color = green, alpha = 0.5) +
  annotate("text", label = expression(MC), x = 9.75, y = 9.65, color = green, size = 6, alpha = 0.5) +
  
  #stat_function(aes(x), fun = marginal_cost_2, color = gree, size = 1) +
  geom_line(aes(x=x, y=marginal_cost_2(x)), size = 1, color = green) +
  annotate("text", label = expression(MC[Tax]), x = 7.1, y = 9.65, color = green, size = 6) +
  
  #stat_function(aes(x), fun = demand, color = red_pink, size = 1) +
  geom_line(aes(x=x, y=demand(x)), size = 1, color = red_pink) +
  
  #stat_function(aes(x), fun = marginal_revenue, color = green, size = 1) +
  geom_line(aes(x=x, y=marginal_revenue(x)), size = 1, color = "blue") +
  
  annotate("text", label = "D", x = 10.1, y = 0.65, color = red_pink, size = 6) +
  annotate("text", label = "MR", x = 5.75, y = 0.65, color = green, size = 6) +
  geom_point(aes(x = q_c, y = p_c), color = met_slate, size = 2) +
  geom_segment(aes(x = 0, xend = q_c, y = p_c, yend = p_c), color = met_slate, linetype = "dashed", size = 1) +
  geom_segment(aes(x = q_c, xend = q_c, y = 0, yend = p_c), color = met_slate, linetype = "dashed", size = 1) +
  
  geom_point(aes(x = q, y = p), color = met_slate, size = 2, alpha = 0.5) +
  geom_point(aes(x = q, y = not_p), color = met_slate, size = 2, alpha = 0.5) +
  geom_segment(aes(x = q, xend = q, y = 0, yend = p), color = met_slate, linetype = "dashed", size = 1, alpha = 0.5) +
  geom_segment(aes(x = 0, xend = q, y = p, yend = p), color = met_slate, linetype = "dashed", size = 1, alpha = 0.5) +
  
  geom_point(aes(x = q_2, y = p_2), color = met_slate, size = 2) +
  geom_point(aes(x = q_2, y = not_p_2), color = met_slate, size = 2) +
  geom_segment(aes(x = q_2, xend = q_2, y = 0, yend = p_2), color = met_slate, linetype = "dashed", size = 1) +
  geom_segment(aes(x = 0, xend = q_2, y = p_2, yend = p_2), color = met_slate, linetype = "dashed", size = 1) +
  
  geom_ribbon(aes(x = w_2, ymin = marginal_cost(w_2), ymax = p_c), fill = orange, alpha = 0.2) +
  geom_ribbon(aes(x = w_2, ymin = p_c, ymax = demand(w_2)), fill = orange, alpha = 0.2) 
