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


###################
### Dart Tax

demand <- function(Q) 20 - 0.5 * Q
supply <- function(Q) 2 + 0.25 * Q

q_range <- 0:32
x_lim <- max(q_range)
y_lim <- max(demand(min(q_range)), supply(max(q_range))) + 2

eq <- curve_intersect(supply,demand, empirical = F, 
                      domain = c(min(q_range),max(q_range)))

ggsave("Dart mark.png", device = "png")

t <- 6
dt <- function(Q) demand(Q) - t

eq2 <- curve_intersect(dt, supply, empirical = F, 
                       domain = c(min(q_range),max(q_range)))

# Tax
ggplot() + 
  # Major Functions:
  geom_line(aes(x=q_range, y=demand(q_range)), size = 1, color = red_pink) + 
  geom_line(aes(x=q_range, y=supply(q_range)), size = 1, color = purple) +
  # Axis Labels, Theme, and Function Labels: 
  labs(title = "Smokes") + xlab("Q") + ylab("P") + theme_sd +
  annotate("text", x=31.5, y=5.25, label="D", fontface = "bold", color = red_pink) + 
  annotate("text", x=31.5, y=10.5, label="S", fontface = "bold", color = purple)+
  # Bounds and Scale
  scale_x_continuous(breaks=seq(0, x_lim, 4), expand=c(0,0), limits=c(0, x_lim)) +
  scale_y_continuous(breaks=seq(0, y_lim, 2), expand=c(0,0), limits=c(0, y_lim)) +
  # Old Equilibrium:
  annotate("point", x=eq$x, y=eq$y, size=4) +
  annotate("text", x=eq$x+0.5, y=eq$y+1.25, 
           label=paste0("(",eq$x,",",eq$y,")"), size=3.1, fontface = "bold") +
  annotate("segment", x=eq$x, xend=eq$x, y=0, yend=eq$y, linetype=2, size = 1) +
  annotate("segment", x=0, xend=eq$x, y=eq$y, yend=eq$y, linetype=2, size = 1) + 
  # Market Modification (Shift, Control, Distortion)
  geom_line(aes(x=q_range, y=dt(q_range)), size = 1, color = red_pink) + 
  annotate("text", x=28, y=0.75, label=expression(bold(D[t])), color = red_pink) + 
  # New Equilibrium:
  annotate("point", x=eq2$x, y=eq2$y, size=4) +
  annotate("text", x=eq2$x+0.5, y=eq2$y+1.25, 
           label=paste0("(",eq2$x,",",eq2$y,")"), size=3.1, fontface = "bold") +
  annotate("segment", x=eq2$x, xend=eq2$x, y=0, yend=eq2$y, linetype=2, size = 1) +
  annotate("segment", x=0, xend=eq2$x, y=eq2$y, yend=eq2$y, linetype=2, size = 1) +
  annotate("segment", x = 4, xend = 4, y = 12.4, yend = 17.6,
           arrow = arrow(length = unit(0.4, "lines"), ends = "both"), 
           colour = "grey50", size = 1) + 
  annotate("text", x=5, y=15, 
           label="$6", color = "grey50")

ggsave("Dart tax.png", device = "png")


# Tax w/ prices
ggplot() + 
  # Major Functions:
  geom_line(aes(x=q_range, y=demand(q_range)), size = 1, color = red_pink) + 
  geom_line(aes(x=q_range, y=supply(q_range)), size = 1, color = purple) +
  # Axis Labels, Theme, and Function Labels: 
  labs(title = "Welfare Implications for a Cigarette Tax") + xlab("Q") + ylab("P") + theme_sd +
  annotate("text", x=30.5, y=6, label="D~WTP", 
           fontface = "bold", color = red_pink, size = 3.5) + 
  annotate("text", x=31.5, y=10.5, label="S", 
           fontface = "bold", color = purple, size = 3.5)+
  # Bounds and Scale
  scale_x_continuous(breaks=seq(0, x_lim, 4), expand=c(0,0), limits=c(0, x_lim)) +
  scale_y_continuous(breaks=seq(0, y_lim, 2), expand=c(0,0), limits=c(0, y_lim)) +
  # Market Modification (Shift, Control, Distortion)
  geom_line(aes(x=q_range, y=dt(q_range)), size = 1, color = red_pink) + 
  annotate("text", x=28, y=0.75, label=expression(bold(D[t])), color = red_pink) + 
  # New Equilibrium:
  annotate("point", x=eq2$x, y=eq2$y, size=3) +
  annotate("point", x=eq2$x, y=eq2$y+t, size=3) +
  annotate("segment", x=eq2$x, xend=eq2$x, y=0, yend=eq2$y+t, linetype=2, size = 1) +
  annotate("segment", x=0, xend=eq2$x, y=eq2$y, yend=eq2$y, linetype=2, size = 1) +
  annotate("segment", x=0, xend=eq2$x, y=eq2$y+t, yend=eq2$y+t, linetype=2, size = 1) +
  annotate("text", x=19, y=12.5, label="price paid = $12", color = red_pink, size = 3) +
  annotate("text", x=19.5, y=6, label="price received = $6", color = purple, size = 3) +
  #CS
  geom_ribbon(aes(x=0:16, ymin = 12, ymax = demand(0:16)), alpha = 0.2, show.legend = F, 
              fill = "red") +
   annotate("text", x=3, y=15, label="CS", color = "red") +
  #PS
  geom_ribbon(aes(x=0:16, ymin = supply(0:16), ymax = 6), alpha = 0.2, show.legend = F,
              fill = purple) +
  annotate("text", x=3, y=4, label="PS", color = purple) +
  #GR
  geom_ribbon(aes(x=0:16, ymin = 6, ymax = 12), alpha = 0.2, show.legend = F,
              fill = green) +
  annotate("text", x=3, y=9, label="GR", color = green) +
  #DWL
  geom_ribbon(aes(x=16:24, ymin = supply(16:24), ymax = demand(16:24)), alpha = 0.2, show.legend = F,
              fill = "purple") +
  annotate("text", x=19, y=9, label="DWL", color = "purple")


ggsave("Dart tax ts.png", device = "png")

###################
### Solar Sub

demand <- function(Q) 10000 - 0.5 * Q
supply <- function(Q) 4000 + (1/3) * Q

q_range <- 0:20000
x_lim <- max(q_range)
y_lim <- max(demand(min(q_range)), supply(max(q_range))) + 3000

eq <- curve_intersect(supply,demand, empirical = F, domain = c(min(q_range),max(q_range)))

t <- 4000
dt <- function(Q) demand(Q) + t

eq2 <- curve_intersect(dt, supply, empirical = F, 
                       domain = c(min(q_range),max(q_range)))

# Sub
ggplot() + 
  # Major Functions:
  geom_line(aes(x=q_range, y=demand(q_range)), size = 1, color = red_pink) + 
  geom_line(aes(x=q_range, y=supply(q_range)), size = 1, color = purple) +
  # Axis Labels, Theme, and Function Labels: 
  labs(title = "Solar Panels") + xlab("Q") + ylab("P") + theme_sd +
  annotate("text", x=19500, y=900, label="D", fontface = "bold", color = red_pink) + 
  annotate("text", x=19500, y=11000, label="S", fontface = "bold", color = purple)+
  # Bounds and Scale
  scale_x_continuous(breaks=seq(0, x_lim, 2000), expand=c(0,0), limits=c(0, x_lim)) +
  scale_y_continuous(breaks=seq(0, y_lim, 1000), expand=c(0,0), limits=c(0, y_lim)) +
  # Old Equilibrium:
  annotate("point", x=eq$x, y=eq$y, size=4) +
  annotate("text", x=eq$x+200, y=eq$y+1000, 
           label=paste0("(",eq$x,",",eq$y,")"), size=3.1, fontface = "bold") +
  annotate("segment", x=eq$x, xend=eq$x, y=0, yend=eq$y, linetype=2, size = 1) +
  annotate("segment", x=0, xend=eq$x, y=eq$y, yend=eq$y, linetype=2, size = 1) + 
  # Market Modification (Shift, Control, Distortion)
  geom_line(aes(x=q_range, y=dt(q_range)), size = 1, color = red_pink) + 
  annotate("text", x=19500, y=4900, label=expression(bold(D[s])), color = red_pink) + 
  # New Equilibrium:
  annotate("point", x=eq2$x, y=eq2$y, size=4) +
  annotate("text", x=eq2$x+350, y=eq2$y+1000, 
           label=paste0("(",eq2$x,",",eq2$y,")"), size=3.1, fontface = "bold") +
  annotate("segment", x=eq2$x, xend=eq2$x, y=0, yend=eq2$y, linetype=2, size = 1) +
  annotate("segment", x=0, xend=eq2$x, y=eq2$y, yend=eq2$y, linetype=2, size = 1) +
  annotate("segment", x = 2000, xend = 2000, y = 9150, yend = 12850,
           arrow = arrow(length = unit(0.4, "lines"), ends = "both"), 
           colour = "grey50", size = 1) + 
  annotate("text", x=3000, y=10750, 
           label="$4000", color = "grey50")

ggsave("Solar sub.png", device = "png")


ggplot() + 
  # Major Functions:
  geom_line(aes(x=q_range, y=demand(q_range)), size = 1, color = red_pink) + 
  geom_line(aes(x=q_range, y=supply(q_range)), size = 1, color = purple) +
  # Axis Labels, Theme, and Function Labels: 
  labs(title = "Solar Panels") + xlab("Q") + ylab("P") + theme_sd +
  annotate("text", x=19000, y=1500, label="D~WTP", 
           fontface = "bold", color = red_pink, size = 3.5) + 
  annotate("text", x=19500, y=11000, label="S", 
           fontface = "bold", color = purple, size = 3.5)+
  # Bounds and Scale
  scale_x_continuous(breaks=seq(0, x_lim, 2000), expand=c(0,0), limits=c(0, x_lim)) +
  scale_y_continuous(breaks=seq(0, y_lim, 1000), expand=c(0,0), limits=c(0, y_lim)) +
  # Market Modification (Shift, Control, Distortion)
  geom_line(aes(x=q_range, y=dt(q_range)), size = 1, color = red_pink) + 
  annotate("text", x=19500, y=4900, label=expression(bold(D[s])), 
           color = red_pink, size = 3.5) + 
  # New Equilibrium:
  annotate("point", x=eq2$x, y=eq2$y, size=3) +
  annotate("point", x=eq2$x, y=eq2$y-t, size=3) +
  annotate("segment", x=eq2$x, xend=eq2$x, y=0, yend=eq2$y, linetype=2, size = 1) +
  
  annotate("segment", x=0, xend=eq2$x, y=eq2$y, yend=eq2$y, linetype=2, size = 1) +
  annotate("segment", x=0, xend=eq2$x, y=eq2$y-t, yend=eq2$y-t, linetype=2, size = 1) +
  annotate("text", x=13200, y=4400, label="price paid", color = red_pink, size = 3.5) +
  annotate("text", x=14000, y=8100, label="price received", color = purple, size = 3.5) +
  geom_ribbon(aes(x=0:12000, ymin = 4000, ymax = 8000), alpha = 0.05, show.legend = F,
              fill = orange, linetype=1, size = 1.5, color = orange) +
  annotate("text", x=5000, y=6500, label="GE", color = orange) +
  geom_ribbon(aes(x=0:12000, ymin = 4000+100, ymax = demand(0:12000)), alpha = 0.05, show.legend = F,
              fill = "red", size = 1, linetype=1, color = "red") +
  annotate("text", x=7000, y=5000, label="CS", color = "red") +
  geom_ribbon(aes(x=0:12000, ymin = supply(0:12000), ymax = 8000), alpha = 0.05, show.legend = F,
              fill = purple, size = 1.5, linetype=1, color = purple) +
  annotate("text", x=2000, y=6500, label="PS", color = purple) +
  geom_ribbon(aes(x=eq$x:12000, ymin = demand(eq$x:12000), ymax = supply(eq$x:12000)), 
              alpha = 0.05, show.legend = F, linetype=1, 
              size = 1.5, color = "purple", fill = "purple")+
  annotate("text", x=11000, y=6400, label="DWL", color = "purple")

ggsave("Solar sub .png", device = "png")


###################
### Oil Tax

demand <- function(Q) 200 - Q
supply <- function(Q) 40 + (1/3) * Q

q_range <- 0:200
x_lim <- max(q_range)
y_lim <- max(demand(min(q_range)), supply(max(q_range))) + 2

eq <- curve_intersect(supply,demand, empirical = F, domain = c(min(q_range),max(q_range)))

t <- 40
st <- function(Q) supply(Q) + t

eq2 <- curve_intersect(demand, st, empirical = F, 
                       domain = c(min(q_range),max(q_range)))

# Tax
ggplot() + 
  # Major Functions:
  geom_line(aes(x=q_range, y=demand(q_range)), size = 1, color = red_pink) + 
  geom_line(aes(x=q_range, y=supply(q_range)), size = 1, color = purple) +
  # Axis Labels, Theme, and Function Labels: 
  labs(title = "Market for Oil") + xlab("Q (millions/barrels)") + ylab("P($)") + theme_sd +
  annotate("text", x=195, y=14, label="D", fontface = "bold", color = red_pink) + 
  annotate("text", x=195, y=114, label="S", fontface = "bold", color = purple)+
  # Bounds and Scale
  scale_x_continuous(breaks=seq(0, x_lim, 20), expand=c(0,0), limits=c(0, x_lim)) +
  scale_y_continuous(breaks=seq(0, y_lim, 20), expand=c(0,0), limits=c(0, y_lim)) +
  # Old Equilibrium:
  annotate("point", x=eq$x, y=eq$y, size=4) +
  annotate("text", x=eq$x+4, y=eq$y+11, 
           label=paste0("(",eq$x,",",eq$y,")"), size=3.1, fontface = "bold") +
  annotate("segment", x=eq$x, xend=eq$x, y=0, yend=eq$y, linetype=2, size = 1) +
  annotate("segment", x=0, xend=eq$x, y=eq$y, yend=eq$y, linetype=2, size = 1) + 
  # Market Modification (Shift, Control, Distortion)
  geom_line(aes(x=q_range, y=st(q_range)), size = 1, color = purple) + 
  annotate("text", x=195, y=154, label=expression(bold(S[t])), color = purple) + 
  # New Equilibrium:
  annotate("point", x=eq2$x, y=eq2$y, size=4) +
  annotate("text", x=eq2$x+4, y=eq2$y+11, 
           label=paste0("(",eq2$x,",",eq2$y,")"), size=3.1, fontface = "bold") +
  annotate("segment", x=eq2$x, xend=eq2$x, y=0, yend=eq2$y, linetype=2, size = 1) +
  annotate("segment", x=0, xend=eq2$x, y=eq2$y, yend=eq2$y, linetype=2, size = 1) +
  # Height
  annotate("segment", x = 150, xend = 150, y = 91.5, yend = 128.5,
           arrow = arrow(length = unit(0.4, "lines")), 
           colour = "grey50", size = 1) + 
  annotate("text", x=157, y=112, 
           label="$40", color = "grey50")

ggsave("Oil tax.png", device = "png")


ggplot() + 
  # Major Functions:
  geom_line(aes(x=q_range, y=demand(q_range)), size = 1, color = red_pink) + 
  geom_line(aes(x=q_range, y=supply(q_range)), size = 1, color = purple) +
  # Axis Labels, Theme, and Function Labels: 
  labs(title = "Welfare Impact for Oil Tax") + xlab("Q (millions/barrels)") + ylab("P($)") + theme_sd +
  annotate("text", x=195, y=14, label="D", fontface = "bold", color = red_pink) + 
  annotate("text", x=195, y=114, label="S", fontface = "bold", color = purple)+
  # Bounds and Scale
  scale_x_continuous(breaks=seq(0, x_lim, 20), expand=c(0,0), limits=c(0, x_lim)) +
  scale_y_continuous(breaks=seq(0, y_lim, 20), expand=c(0,0), limits=c(0, y_lim)) +
  # Old Equilibrium:
  annotate("segment", x=0, xend=eq2$x, y=70, yend=70, linetype=2, size = 1) + 
  # Market Modification (Shift, Control, Distortion)
  geom_line(aes(x=q_range, y=st(q_range)), size = 1, color = purple) + 
  annotate("text", x=195, y=154, label=expression(bold(S[t])), color = purple) + 
  # New Equilibrium:
  annotate("segment", x=eq2$x, xend=eq2$x, y=0, yend=eq2$y, linetype=2, size = 1) +
  annotate("segment", x=0, xend=eq2$x, y=eq2$y, yend=eq2$y, linetype=2, size = 1) +
  # Height
  annotate("text", x=94, y=120, label=expression(P[paid]), color = red_pink) +
  annotate("text", x=101, y=64, label=expression(P[received]), color = purple) +
  annotate("point", x=eq2$x, y=eq2$y, size=3) +
  annotate("point", x=eq2$x, y=eq2$y-t, size=3) +
  annotate("point", x=eq2$x, y=eq2$y, size=3) +
  #CS
  geom_ribbon(aes(x=0:90, ymin = 110, ymax = demand(0:90)), alpha = 0.2, show.legend = F, 
              fill = "red") +
  annotate("text", x=10, y=150, label="CS", color = "red") +
  #PS
  geom_ribbon(aes(x=0:90, ymin = supply(0:90), ymax = 70), alpha = 0.2, show.legend = F,
              fill = purple) +
  annotate("text", x=10, y=55, label="PS", color = purple) +
  #GR
  geom_ribbon(aes(x=0:90, ymin = 70, ymax = 110), alpha = 0.2, show.legend = F,
              fill = green) +
  annotate("text", x=10, y=95, label="GR", color = green) +
  #DWL
  geom_ribbon(aes(x=90:120, ymin = supply(90:120), ymax = demand(90:120)), alpha = 0.2, show.legend = F,
              fill = "purple") +
  annotate("text", x=100, y=85, label="DWL", color = "purple")

ggsave("Oil tax welfare.png", device = "png")


###################
### Wind Sub

demand <- function(Q) 1500 - (4/3) * Q
supply <- function(Q) 500 + (2) * Q

q_range <- 0:1200
x_lim <- max(q_range)
y_lim <- max(demand(min(q_range)), supply(max(q_range))) + 2

eq <- curve_intersect(supply,demand, empirical = F, domain = c(min(q_range),max(q_range)))

t <- 600
st <- function(Q) supply(Q) - t

eq2 <- curve_intersect(demand, st, empirical = F, domain = c(min(q_range),max(q_range)))

# Sub
ggplot() + 
  # Major Functions:
  geom_line(aes(x=q_range, y=demand(q_range)), size = 1, color = red_pink) + 
  geom_line(aes(x=q_range, y=supply(q_range)), size = 1, color = purple) +
  # Axis Labels, Theme, and Function Labels: 
  labs(title = "Market for Wind Power") + xlab("Q") + ylab("P") + theme_sd +
  annotate("text", x=850, y=500, label="D", fontface = "bold", color = red_pink) + 
  annotate("text", x=850, y=2350, label="S", fontface = "bold", color = purple)+
  # Bounds and Scale
  scale_x_continuous(breaks=seq(0, x_lim, 200), expand=c(0,0), limits=c(0, x_lim-300)) +
  scale_y_continuous(breaks=seq(0, y_lim, 250), expand=c(0,0), limits=c(0, y_lim)) +
  # Old Equilibrium:
  annotate("point", x=eq$x, y=eq$y, size=4) +
  annotate("text", x=eq$x-15, y=eq$y+200, 
           label=paste0("(",eq$x,",",eq$y,")"), size=3.1, fontface = "bold") +
  annotate("segment", x=eq$x, xend=eq$x, y=0, yend=eq$y, linetype=2, size = 1) +
  annotate("segment", x=0, xend=eq$x, y=eq$y, yend=eq$y, linetype=2, size = 1) + 
  # Market Modification (Shift, Control, Distortion)
  geom_line(aes(x=q_range, y=st(q_range)), size = 1, color = purple) + 
  annotate("text", x=850, y=1500, label=expression(bold(S[s])), color = purple) + 
  # New Equilibrium:
  annotate("point", x=eq2$x, y=eq2$y, size=4) +
  annotate("text", x=eq2$x-15, y=eq2$y+190, 
           label=paste0("(",eq2$x,",",eq2$y,")"), size=3.1, fontface = "bold") +
  annotate("segment", x=eq2$x, xend=eq2$x, y=0, yend=eq2$y, linetype=2, size = 1) +
  annotate("segment", x=0, xend=eq2$x, y=eq2$y, yend=eq2$y, linetype=2, size = 1) +
  # Height
  annotate("segment", x = 800, xend = 800, y = 2050, yend = 1550,
           arrow = arrow(length = unit(0.4, "lines")), 
           colour = "grey50", size = 1) + 
  annotate("text", x=840, y=1850,  label="$600", color = "grey50")

ggsave("Wind sub.png", device = "png")


# Sub
ggplot() + 
  # Major Functions:
  geom_line(aes(x=q_range, y=demand(q_range)), size = 1, color = red_pink) + 
  geom_line(aes(x=q_range, y=supply(q_range)), size = 1, color = purple) +
  # Axis Labels, Theme, and Function Labels: 
  labs(title = "Market for Wind Power") + xlab("Q") + ylab("P") + theme_sd +
  annotate("text", x=850, y=500, label="D", fontface = "bold", color = red_pink) + 
  annotate("text", x=850, y=2350, label="S", fontface = "bold", color = purple)+
  # Bounds and Scale
  scale_x_continuous(breaks=seq(0, x_lim, 100), expand=c(0,0), limits=c(0, x_lim-300)) +
  scale_y_continuous(breaks=seq(0, y_lim, 250), expand=c(0,0), limits=c(0, y_lim)) +
  # Old Equilibrium:
  annotate("segment", x=0, xend=eq2$x, y=eq2$y+t, yend=eq2$y+t, linetype=2, size = 1) + 
  # Market Modification (Shift, Control, Distortion)
  geom_line(aes(x=q_range, y=st(q_range)), size = 1, color = purple) + 
  annotate("text", x=850, y=1500, label=expression(bold(S[s])), color = purple) + 
  # New Equilibrium:
  annotate("point", x=eq2$x, y=eq2$y, size=4) +
  annotate("text", x=eq2$x+75, y=eq2$y+30, 
           label=paste0("(",eq2$x,",",eq2$y,")"), size=3.1, fontface = "bold") +
  annotate("segment", x=eq2$x, xend=eq2$x, y=0, yend=eq2$y+t, linetype=2, size = 1) +
  annotate("segment", x=0, xend=eq2$x, y=eq2$y, yend=eq2$y, linetype=2, size = 1) +
  # Height
  annotate("segment", x = 800, xend = 800, y = 2050, yend = 1550,
           arrow = arrow(length = unit(0.4, "lines")), 
           colour = "grey50", size = 1) + 
  annotate("text", x=840, y=1850,  label="$600", color = "grey50") +
  annotate("text", x=250, y=800, label="price paid - $860", color = red_pink, size = 3.5) +
  annotate("text", x=250, y=1550, label="price received - $1460", color = purple, size = 3.5) +
  # geom_ribbon(aes(x=0:eq2$x, ymin = eq2$y, ymax = eq2$y+t), alpha = 0.25,
  #             show.legend = F, fill = orange) +
  # annotate("text", x=200, y=1125, label="GE", color = orange) +
  annotate("point", x=eq2$x, y=eq2$y+t, size=4) +
  annotate("text", x=eq2$x+75, y=eq2$y+10+t, 
           label=paste0("(",eq2$x,",",eq2$y+t,")"), size=3.1, fontface = "bold")

ggsave("Wind sub prices.png", device = "png")

