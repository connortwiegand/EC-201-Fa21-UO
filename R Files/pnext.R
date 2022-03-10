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


# Negative Production Externality
demand <- function(Q) 200 - Q
supply <- function(Q) 40 + (1/3) * Q

q_range <- 0:200
x_lim <- max(q_range)
y_lim <- max(demand(min(q_range)), supply(max(q_range))) + 2

eq <- curve_intersect(supply,demand, empirical = F, domain = c(min(q_range),max(q_range)))

mec <- 40
msc <- function(Q) supply(Q) + mec

eq2 <- curve_intersect(demand, msc, empirical = F, 
                       domain = c(min(q_range),max(q_range)))

nomarks <- ggplot() + 
  # Major Functions:
  geom_line(aes(x=q_range, y=demand(q_range)), size = 1, color = red_pink) + 
  geom_line(aes(x=q_range, y=supply(q_range)), size = 1, color = purple) +
  # Axis Labels, Theme, and Function Labels: 
  labs(title = "Oil") + 
  xlab("Q") + ylab("P") + theme_sd +
  # Bounds and Scale
  scale_x_continuous(breaks=seq(0, x_lim, 20), expand=c(0,0), limits=c(0, x_lim)) +
  scale_y_continuous(breaks=seq(0, y_lim, 40), expand=c(0,0), limits=c(0, y_lim)) +
  # Market Modification (Shift, Control, Distortion)
  geom_line(aes(x=q_range, y=msc(q_range)), size = 1, color = purple) +
  #S/D Labels:
  annotate("text", x=170, y=10, label="D", 
           fontface = "bold", color = red_pink, size = 4) + 
  annotate("text", x=195, y=114, label="S", 
           fontface = "bold", color = purple, size = 4) +
  #MSB/MSC Label:
  annotate("text", x=190, y=154, label="MSC", 
           fontface = "bold", color = purple, size = 4)
nomarks

base <- nomarks + 
  #Old Eq Point and segments
  annotate("point", x=eq$x, y=eq$y, size=3) +
  annotate("segment", x=eq$x, xend=eq$x, y=0, yend=eq$y, linetype=2, size = 1) + #v
  annotate("segment", x=0, xend=eq$x, y=eq$y, yend=eq$y, linetype=2, size = 1) + #h
  annotate("text", x=eq$x, y=eq$y+10, label="bold(E[p])", fontface="bold", parse=T) + 
  #New Eq point and segments
  annotate("point", x=eq2$x, y=eq2$y, size=3) +
  annotate("segment", x=eq2$x, xend=eq2$x, y=0, yend=eq2$y, linetype=2, size=1) + #v
  annotate("segment", x=0, xend=eq2$x, y=eq2$y, yend=eq2$y, linetype=2, size=1) + #h
  annotate("text", x=eq2$x, y=eq2$y+10, 
           label="bold(E[s])", fontface="bold", parse=T) +
  #MEB/MEC grey lines
  annotate("segment", x=170, xend=170, y=supply(170)+1.5, yend=msc(170)-1.5,
           arrow=arrow(length=unit(0.4, "lines"), ends="both"), 
           colour="grey50", size=1) + 
  annotate("text", x=178, y=118, label="MEC\n$40", color = "grey50", size=3.5) #+
base

ggsave("pnext base.png", device = "png")

csps <- base +
  #CS
  annotate("text", x=20, y=150, label="CS", color = "red") +
  geom_ribbon(aes(x=0:eq$x, ymin = 80, ymax = demand(0:eq$x)), alpha = 0.2, show.legend = F, 
              fill = "red") +
  #PS
  annotate("text", x=20, y=64, label="PS", color = purple) +
  geom_ribbon(aes(x=0:eq$x, ymin = supply(0:eq$x), ymax = 80), alpha = 0.2, show.legend = F,
              fill = purple)
csps
ggsave("pnext csps.png", device = "png")

pnext_ec <- base +
  #EC
  annotate("text", x=50, y=75, label="EC", color = orange) +
  geom_ribbon(aes(x=0:eq$x, ymin = supply(0:eq$x), ymax = msc(0:eq$x)), alpha = 0.2, show.legend = F,
              fill = orange)
pnext_ec
ggsave("pnext ec.png", device = "png")

pnext_dwl <- base + 
  #DWL
  annotate("text", x=110, y=105, label="DWL", color = "purple") +
  geom_ribbon(aes(x=eq2$x:eq$x, ymin = demand(eq2$x:eq$x), ymax = msc(eq2$x:eq$x)), alpha = 0.2, show.legend = F,
              fill = "purple")
pnext_dwl
ggsave("pnext dwl.png", device = "png")





#With Tax





nomarks <- ggplot() + 
  # Major Functions:
  geom_line(aes(x=q_range, y=demand(q_range)), size = 1, color = red_pink) + 
  geom_line(aes(x=q_range, y=supply(q_range)), size = 1, color = purple, alpha=0.5) +
  # Axis Labels, Theme, and Function Labels: 
  labs(title = "Oil") + 
  xlab("Q") + ylab("P") + theme_sd +
  # Bounds and Scale
  scale_x_continuous(breaks=seq(0, x_lim, 20), expand=c(0,0), limits=c(0, x_lim)) +
  scale_y_continuous(breaks=seq(0, y_lim, 40), expand=c(0,0), limits=c(0, y_lim)) +
  # Market Modification (Shift, Control, Distortion)
  geom_line(aes(x=q_range, y=msc(q_range)), size = 1, color = purple) +
  #S/D Labels:
  annotate("text", x=170, y=10, label="D", 
           fontface = "bold", color = red_pink, size = 4) + 
  annotate("text", x=195, y=114, label="S", 
           fontface = "bold", color = purple, size = 4) +
  #MSB/MSC Label:
  annotate("text", x=185, y=154, label="bold(S[t]==MSC)", 
           fontface = "bold", color = purple, size = 4, parse=T)
nomarks

base <- nomarks + 
  #Old Eq Point and segments
  annotate("point", x=eq$x, y=eq$y, size=3) +
  annotate("segment", x=eq$x, xend=eq$x, y=0, yend=eq$y, linetype=2, size = 1) + #v
  annotate("segment", x=0, xend=eq$x, y=eq$y, yend=eq$y, linetype=2, size = 1) + #h
  annotate("text", x=eq$x, y=eq$y+10, label="bold(E[p])", fontface="bold", parse=T) + 
  #New Eq point and segments
  annotate("point", x=eq2$x, y=eq2$y, size=3) +
  annotate("segment", x=eq2$x, xend=eq2$x, y=0, yend=eq2$y, linetype=2, size=1) + #v
  annotate("segment", x=0, xend=eq2$x, y=eq2$y, yend=eq2$y, linetype=2, size=1) + #h
  annotate("text", x=eq2$x, y=eq2$y+10, 
           label="bold(E[s])", fontface="bold", parse=T) +
  #MEB/MEC grey lines
  annotate("segment", x=170, xend=170, y=supply(170)+1.5, yend=msc(170)-1.5,
           arrow=arrow(length=unit(0.4, "lines"), ends="last"), 
           colour="grey50", size=1) + 
  annotate("text", x=178, y=118, label="Tax\n$40", color = "grey50", size=3.5) #+
base

ggsave("pnext tax base.png", device = "png")

csps <- base +
  #CS
  annotate("text", x=20, y=150, label="CS", color = "red") +
  geom_ribbon(aes(x=0:eq2$x, ymin = eq2$y, ymax = demand(0:eq2$x)), alpha = 0.2, show.legend = F, 
              fill = "red") +
  #PS
  annotate("text", x=20, y=100, label="PS", color = purple) +
  geom_ribbon(aes(x=0:eq2$x, ymin = msc(0:eq2$x), ymax = eq2$y), alpha = 0.2, show.legend = F,
              fill = purple)
csps
ggsave("pnext tax csps.png", device = "png")

pnext_ec <- base +
  #EC
  annotate("text", x=50, y=75, label="EC", color = orange) +
  geom_ribbon(aes(x=0:eq2$x, ymin = supply(0:eq2$x), ymax = msc(0:eq2$x)), alpha = 0.2, show.legend = F,
              fill = orange)
pnext_ec
ggsave("pnext tax ec.png", device = "png")

pnext_gr <- base + 
  #GR
  annotate("text", x=20, y=95, label="GR", color = green) +
  geom_ribbon(aes(x=0:eq2$x, ymin = eq2$y-mec, ymax = eq2$y), alpha = 0.2, show.legend = F,
              fill = green)
pnext_gr
ggsave("pnext tax gr.png", device = "png")



