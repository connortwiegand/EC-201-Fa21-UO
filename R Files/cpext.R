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



#Positive Consumption Externality

demand <- function(Q) 10000 - 0.5 * Q
supply <- function(Q) 4000 + (1/3) * Q

q_range <- 0:20000
x_lim <- max(q_range)
y_lim <- max(demand(min(q_range)), supply(max(q_range))) + 4000

eq <- curve_intersect(supply,demand, empirical = F, domain = c(min(q_range),max(q_range)))

meb <- 4000
msb <- function(Q) demand(Q) + meb

eq2 <- curve_intersect(supply, msb, empirical = F, 
                       domain = c(min(q_range),max(q_range)))

nomarks <- ggplot() + 
  # Major Functions:
  geom_line(aes(x=q_range, y=demand(q_range)), size = 1, color = red_pink) + 
  geom_line(aes(x=q_range, y=supply(q_range)), size = 1, color = purple) +
  # Axis Labels, Theme, and Function Labels: 
  labs(title = "Vegan Food in a Year") + 
  xlab("Q") + ylab("P") + theme_sd +
  # Bounds and Scale
  scale_x_continuous(breaks=seq(0, x_lim, 2000), expand=c(0,0), limits=c(0, x_lim)) +
  scale_y_continuous(breaks=seq(0, y_lim, 1000), expand=c(0,0), limits=c(0, y_lim)) +
  # Market Modification (Shift, Control, Distortion)
  geom_line(aes(x=q_range, y=msb(q_range)), size = 1, color = red_pink) +
  #S/D Labels:
  annotate("text", x=17000, y=500, label="D~MPB", 
           fontface = "bold", color = red_pink, size = 4) + 
  annotate("text", x=19500, y=11000, label="S", 
           fontface = "bold", color = purple, size = 4) +
  #MSB/MSC Label:
  annotate("text", x=19250, y=5200, label="MSB", 
           fontface = "bold", color = red_pink, size = 4)
nomarks

base <- nomarks + 
  #Old Eq Point and segments
  annotate("point", x=eq$x, y=eq$y, size=3) +
  annotate("segment", x=eq$x, xend=eq$x, y=0, yend=eq$y, linetype=2, size = 1) + #v
  annotate("segment", x=0, xend=eq$x, y=eq$y, yend=eq$y, linetype=2, size = 1) + #h
  annotate("text", x=eq$x-500, y=eq$y-900, label="bold(E[p])", fontface="bold", parse=T) + 
  #New Eq point and segments
  annotate("point", x=eq2$x, y=eq2$y, size=3) +
  annotate("segment", x=eq2$x, xend=eq2$x, y=0, yend=eq2$y, linetype=2, size=1) + #v
  annotate("segment", x=0, xend=eq2$x, y=eq2$y, yend=eq2$y, linetype=2, size=1) + #h
  annotate("text", x=eq2$x, y=eq2$y+800, 
           label="bold(E[s])", fontface="bold", parse=T) +
  #MEB/MEC grey lines
  annotate("segment", x=15000, xend=15000, y=demand(15000)+150, yend=msb(15000)-150,
           arrow=arrow(length=unit(0.4, "lines"), ends="both"), 
           colour="grey50", size=1) + 
  annotate("text", x=15000+1000, y=4000, label="MEB\n$4000", color = "grey50", size=3.5) #+
base

ggsave("cpext base.png", device = "png")

csps <- base +
  #CS
  annotate("text", x=2000, y=7300, label="CS", color = "red") +
  geom_ribbon(aes(x=0:eq$x, ymin=eq$y, ymax = demand(0:eq$x)), alpha = 0.2, show.legend = F, 
              fill = "red") +
  #PS
  annotate("text", x=2000, y=5500, label="PS", color = purple) +
  geom_ribbon(aes(x=0:eq$x, ymin = supply(0:eq$x), ymax = eq$y), alpha = 0.2, show.legend = F,
              fill = purple)
csps
ggsave("cpext csps.png", device = "png")

cpext_eb <- base +
  #EB
  annotate("text", x=4000, y=10000, label="EB", color = green) +
  geom_ribbon(aes(x=0:eq$x, ymin = msb(0:eq$x), ymax = demand(0:eq$x)), alpha = 0.2, show.legend = F,
              fill = green)
cpext_eb
ggsave("cpext eb.png", device = "png")

cpext_dwl <- base + 
  #DWL
  annotate("text", x=9000, y=8500, label="DWL", color = "purple") +
  geom_ribbon(aes(x=eq$x:eq2$x, ymin = supply(eq$x:eq2$x), ymax = msb(eq$x:eq2$x)), alpha = 0.2, show.legend = F,
              fill = "purple")
cpext_dwl
ggsave("cpext dwl.png", device = "png")

cpext_csps_eb <- base +
  #CS
  annotate("text", x=2000, y=7300, label="CS", color = "red") +
  geom_ribbon(aes(x=0:eq$x, ymin=eq$y, ymax = demand(0:eq$x)), alpha = 0.2, show.legend = F, 
              fill = "red") +
  #PS
  annotate("text", x=2000, y=5500, label="PS", color = purple) +
  geom_ribbon(aes(x=0:eq$x, ymin = supply(0:eq$x), ymax = eq$y), alpha = 0.2, show.legend = F,
              fill = purple) +
  #EB
  annotate("text", x=4000, y=10000, label="EB", color = green) +
  geom_ribbon(aes(x=0:eq$x, ymin = msb(0:eq$x), ymax = demand(0:eq$x)), alpha = 0.2, show.legend = F,
              fill = green)
cpext_csps_eb
ggsave("cpext ts.png", device = "png")



#With Tax





nomarks <- ggplot() + 
  # Major Functions:
  geom_line(aes(x=q_range, y=demand(q_range)), size = 1, color = red_pink, alpha = 0.5) + 
  geom_line(aes(x=q_range, y=supply(q_range)), size = 1, color = purple) +
  # Axis Labels, Theme, and Function Labels: 
  labs(title = "Vegan Food in a Year") + 
  xlab("Q") + ylab("P") + theme_sd +
  # Bounds and Scale
  scale_x_continuous(breaks=seq(0, x_lim, 2000), expand=c(0,0), limits=c(0, x_lim)) +
  scale_y_continuous(breaks=seq(0, y_lim, 1000), expand=c(0,0), limits=c(0, y_lim)) +
  # Market Modification (Shift, Control, Distortion)
  geom_line(aes(x=q_range, y=msb(q_range)), size = 1, color = red_pink) +
  #S/D Labels:
  annotate("text", x=17000, y=500, label="D~MPB", 
           fontface = "bold", color = red_pink, size = 4) + 
  annotate("text", x=19500, y=11000, label="S", 
           fontface = "bold", color = purple, size = 4) +
  #MSB/MSC Label:
  annotate("text", x=17000, y=4300, label="bold(D[t]==MSB)", 
           fontface = "bold", color = red_pink, size = 4, parse=T)
nomarks

base <- nomarks + 
  #Old Eq Point and segments
  annotate("point", x=eq$x, y=eq$y, size=3) +
  annotate("segment", x=eq$x, xend=eq$x, y=0, yend=eq$y, linetype=2, size = 1) + #v
  annotate("segment", x=0, xend=eq$x, y=eq$y, yend=eq$y, linetype=2, size = 1) + #h
  annotate("text", x=eq$x-500, y=eq$y-900, label="bold(E[p])", fontface="bold", parse=T) + 
  #New Eq point and segments
  annotate("point", x=eq2$x, y=eq2$y, size=3) +
  annotate("segment", x=eq2$x, xend=eq2$x, y=0, yend=eq2$y, linetype=2, size=1) + #v
  annotate("segment", x=0, xend=eq2$x, y=eq2$y, yend=eq2$y, linetype=2, size=1) + #h
  annotate("text", x=eq2$x, y=eq2$y+800, 
           label="bold(E[s])", fontface="bold", parse=T) +
  #MEB/MEC grey lines
  annotate("segment", x=15000, xend=15000, y=demand(15000)+150, yend=msb(15000)-150,
           arrow=arrow(length=unit(0.4, "lines"), ends="last"), 
           colour="grey50", size=1) + 
  annotate("text", x=15000-900, y=4600, label="Sub.\n$4000", color = "grey50", size=3.5) #+
base

ggsave("cpext tax base.png", device = "png")

csps <- base +
  #CS
  annotate("text", x=2000, y=11000, label="CS", color = "red") +
  geom_ribbon(aes(x=0:eq2$x, ymin=eq2$y, ymax = msb(0:eq2$x)), alpha = 0.2, show.legend = F, 
              fill = "red") +
  #PS
  annotate("text", x=2000, y=5500, label="PS", color = purple) +
  geom_ribbon(aes(x=0:eq2$x, ymin = supply(0:eq2$x), ymax = eq2$y), alpha = 0.2, show.legend = F,
              fill = purple)
csps
ggsave("cpext tax csps.png", device = "png")

cpext_eb <- base +
  #EB
  annotate("text", x=7000, y=9000, label="EB", color = green) +
  geom_ribbon(aes(x=0:eq2$x, ymin = msb(0:eq2$x), ymax = demand(0:eq2$x)), alpha = 0.2, show.legend = F,
              fill = green)
cpext_eb
ggsave("cpext tax eb.png", device = "png")

cpext_ge <- base + 
  #GE
  annotate("text", x=7000, y=7500, label="GE", color = orange) +
  geom_ribbon(aes(x=0:eq2$x, ymin = eq2$y-meb, ymax = eq2$y), alpha = 0.2, show.legend = F,
              fill = orange)
cpext_ge
ggsave("cpext tax ge.png", device = "png")

cpext_csps_eb <- base +
  #CS
  annotate("text", x=2000, y=11000, label="CS", color = "red") +
  geom_ribbon(aes(x=0:eq2$x, ymin=eq2$y, ymax = msb(0:eq2$x)), alpha = 0.2, show.legend = F, 
              fill = "red") +
  #PS
  annotate("text", x=2000, y=5500, label="PS", color = purple) +
  geom_ribbon(aes(x=0:eq2$x, ymin = supply(0:eq2$x), ymax = eq2$y), alpha = 0.2, show.legend = F,
              fill = purple) +
  #EB
  annotate("text", x=7000, y=9000, label="EB", color = green) +
  geom_ribbon(aes(x=0:eq2$x, ymin = msb(0:eq2$x), ymax = demand(0:eq2$x)), alpha = 0.2, show.legend = F,
              fill = green)
cpext_eb
ggsave("cpext tax ts.png", device = "png")




