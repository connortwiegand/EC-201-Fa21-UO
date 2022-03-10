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

#Negative Consumption Externality
demand <- function(Q) 20 - 0.5 * Q
supply <- function(Q) 2 + 0.25 * Q
q_range <- 0:32


x_lim <- max(q_range)
y_lim <- max(demand(min(q_range)), supply(max(q_range))) + 2

eq <- curve_intersect(supply,demand, empirical = F, 
                      domain = c(min(q_range),max(q_range)))


mec <- 6
msb <- function(Q) demand(Q) - mec

eq2 <- curve_intersect(msb, supply, empirical = F, 
                       domain = c(min(q_range),max(q_range)))


nomarks <- ggplot() + 
  # Major Functions:
  geom_line(aes(x=q_range, y=demand(q_range)), size = 1, color = red_pink) + 
  geom_line(aes(x=q_range, y=supply(q_range)), size = 1, color = purple) +
  # Axis Labels, Theme, and Function Labels: 
  labs(title = "Cigarettes") + 
  xlab("Q") + ylab("P") + theme_sd +
  # Bounds and Scale
  scale_x_continuous(breaks=seq(0, x_lim, 4), expand=c(0,0), limits=c(0, x_lim)) +
  scale_y_continuous(breaks=seq(0, y_lim, 2), expand=c(0,0), limits=c(0, y_lim)) +
  # Market Modification (Shift, Control, Distortion)
  geom_line(aes(x=q_range, y=msb(q_range)), size = 1, color = red_pink) +
  #S/D Labels:
  annotate("text", x=30.25, y=6.5, label="D~MPB", 
           fontface = "bold", color = red_pink, size = 4) + 
  annotate("text", x=31.5, y=10.75, label="S", 
           fontface = "bold", color = purple, size = 4) +
  #MSB/MSC Label:
  annotate("text", x=29.5, y=1, label="MSB", 
           fontface = "bold", color = red_pink, size = 4)
nomarks

base <- nomarks + 
  #Old Eq Point and segments
  annotate("point", x=eq$x, y=eq$y, size=3) +
  annotate("segment", x=eq$x, xend=eq$x, y=0, yend=eq$y, linetype=2, size = 1) + #v
  annotate("segment", x=0, xend=eq$x, y=eq$y, yend=eq$y, linetype=2, size = 1) + #h
  annotate("text", x=eq$x, y=eq$y+1.25, label=(expression(bold(E[p]))), fontface="bold") + 
  #New Eq point and segments
  annotate("point", x=eq2$x, y=eq2$y, size=3) +
  annotate("segment", x=eq2$x, xend=eq2$x, y=0, yend=eq2$y, linetype=2, size=1) + #v
  annotate("segment", x=0, xend=eq2$x, y=eq2$y, yend=eq2$y, linetype=2, size=1) + #h
  annotate("text", x=eq2$x-0.75, y=eq2$y-1, 
           label=(expression(bold(E[s]))), fontface = "bold") +
  #MEB/MEC grey lines
  annotate("segment", x=26, xend=26, y=demand(26)-0.25, yend=msb(26)+0.25,
           arrow=arrow(length=unit(0.4, "lines"), ends="both"), 
           colour="grey50", size=1) + 
  annotate("text", x=27.25, y=4, label="MEC\n$6", color = "grey50", size=3.5) #+
base

ggsave("neg c ext base.png", device = "png")

csps <- base +
  #CS
  annotate("text", x=3, y=15, label="CS", color = "red") +
  geom_ribbon(aes(x=0:24, ymin = 8, ymax = demand(0:24)), alpha = 0.2, show.legend = F, 
              fill = "red") +
  #PS
  annotate("text", x=3, y=4, label="PS", color = purple) +
  geom_ribbon(aes(x=0:24, ymin = supply(0:24), ymax = 8), alpha = 0.2, show.legend = F,
              fill = purple)
csps
ggsave("neg c ext csps.png", device = "png")

cnext_ec <- base +
  #EC
  annotate("text", x=12, y=11, label="EC", color = orange) +
  geom_ribbon(aes(x=0:24, ymin = msb(0:24), ymax = demand(0:24)), alpha = 0.2, show.legend = F,
              fill = orange)
cnext_ec
ggsave("neg c ext ec.png", device = "png")

cnext_dwl <- base + 
  #DWL
  annotate("text", x=21, y=5.5, label="DWL", color = "purple") +
  geom_ribbon(aes(x=16:24, ymin = msb(16:24), ymax = supply(16:24)), alpha = 0.2, show.legend = F,
              fill = "purple")
cnext_dwl
ggsave("neg c ext dwl.png", device = "png")




#With Tax





nomarks <- ggplot() + 
  # Major Functions:
  geom_line(aes(x=q_range, y=demand(q_range)), size = 1, color = red_pink, alpha=0.5) + 
  geom_line(aes(x=q_range, y=supply(q_range)), size = 1, color = purple) +
  # Axis Labels, Theme, and Function Labels: 
  labs(title = "Cigarettes") + 
  xlab("Q") + ylab("P") + theme_sd +
  # Bounds and Scale
  scale_x_continuous(breaks=seq(0, x_lim, 4), expand=c(0,0), limits=c(0, x_lim)) +
  scale_y_continuous(breaks=seq(0, y_lim, 2), expand=c(0,0), limits=c(0, y_lim)) +
  # Market Modification (Shift, Control, Distortion)
  geom_line(aes(x=q_range, y=msb(q_range)), size = 1, color = red_pink) +
  #S/D Labels:
  annotate("text", x=30.25, y=6.5, label="D~MPB", 
           fontface = "bold", color = red_pink, size = 4) + 
  annotate("text", x=31.5, y=10.75, label="S", 
           fontface = "bold", color = purple, size = 4) +
  #MSB/MSC Label:
  annotate("text", x=29.5, y=1, label=TeX("\\textbf{$D_t=$MSB}"), 
           fontface = "bold", color = red_pink, size = 4)
nomarks

base <- nomarks + 
  #Old Eq Point and segments
  annotate("point", x=eq$x, y=eq$y, size=3) +
  annotate("segment", x=eq$x, xend=eq$x, y=0, yend=eq$y, linetype=2, size = 1) + #v
  annotate("segment", x=0, xend=eq$x, y=eq$y, yend=eq$y, linetype=2, size = 1) + #h
  annotate("text", x=eq$x, y=eq$y+1.25, label=(expression(bold(E[p]))), fontface="bold") + 
  #New Eq point and segments
  annotate("point", x=eq2$x, y=eq2$y, size=3) +
  annotate("segment", x=eq2$x, xend=eq2$x, y=0, yend=eq2$y, linetype=2, size=1) + #v
  annotate("segment", x=0, xend=eq2$x, y=eq2$y, yend=eq2$y, linetype=2, size=1) + #h
  annotate("text", x=eq2$x-0.75, y=eq2$y-1, 
           label=(expression(bold(E[s]))), fontface = "bold") +
  #MEB/MEC grey lines
  annotate("segment", x=26, xend=26, y=demand(26)-0.25, yend=msb(26)+0.25,
           arrow=arrow(length=unit(0.4, "lines"), ends="last"), 
           colour="grey50", size=1) + 
  annotate("text", x=27.25, y=4, label="Tax\n$6", color = "grey50", size=3.5) #+
base

ggsave("cnext tax base.png", device = "png")

csps_ec <- base +
  #CS
  annotate("text", x=3, y=10, label="CS", color = "red") +
  geom_ribbon(aes(x=0:16, ymin = 6, ymax = msb(0:16)), alpha = 0.2, show.legend = F, 
              fill = "red") +
  #PS
  annotate("text", x=3, y=4, label="PS", color = purple) +
  geom_ribbon(aes(x=0:16, ymin = supply(0:16), ymax = 6), alpha = 0.2, show.legend = F,
              fill = purple)
csps
ggsave("cnext tax csps.png", device = "png")

cnext_ec <- base +
  #EC
  annotate("text", x=12, y=11, label="EC", color = orange) +
  geom_ribbon(aes(x=0:16, ymin = msb(0:16), ymax = demand(0:16)), alpha = 0.2, show.legend = F,
              fill = orange)
cnext_ec
ggsave("cnext tax ec.png", device = "png")

cnext_gr <- base + 
  #DWL
  annotate("text", x=3, y=10, label="GR", color = green) +
  geom_ribbon(aes(x=0:16, ymin = 6, ymax = 12), alpha = 0.2, show.legend = F,
              fill = green)
cnext_gr
ggsave("cnext tax gr.png", device = "png")



