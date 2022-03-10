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
  text = element_text(color = met_slate, size = 12),
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
  plot.title = element_text(hjust=0.55, size = 18),
  plot.margin = margin(t=2, r=13, b=2, l=2)
)


x <- seq(0.1,32,0.1)
z <- seq(6,32,0.1)
z0 <- seq(6,shutq,0.1)
z1 <- seq(shutq,32,0.1)

xlim <- max(x)
ylim <- 36+1
tc <- function(x) (1/8)*(x^3)-4*(x^2)+48*x+96
vc <- function(x) tc(x)-96

atc <- function(x) tc(x)/x
avc <- function(x) vc(x)/x

mc <- function(x) (3/8)*(x^2)-8*x+48
be <- uniroot(function(x) atc(x) - mc(x), range(x))$root
atc_min <- atc(be)



firm1 <- ggplot() +
  scale_x_continuous(limits = c(0, xlim), expand=c(0,0), breaks = seq(0,xlim,4)) +
  scale_y_continuous(limits = c(0, ylim), expand=c(0,0), breaks = seq(0,ylim,4)) +
  theme_market +
  labs(x = "Q", y = "$", title = "Individual Firm") +
  geom_line(aes(x=x, y=atc(x)), size = 1, color = purple) +
  annotate("text", label = "ATC", x = 12, y = 34, color = purple, size = 6) +
  geom_line(aes(x=z, y=mc(z)), size = 1, color = green) +
  annotate("text", label = "MC", x = 21, y = 34, color = green, size = 6) +
  annotate("point", x = be, y=atc_min, size = 3) +
  annotate("segment", x = be, xend = be, y = 0, yend = atc_min, linetype = 2, size = 1)
firm1

mark1 <- ggplot() +
  scale_x_continuous(limits = c(0, xlim), expand=c(0,0), breaks = seq(0,xlim,4)) +
  scale_y_continuous(limits = c(0, ylim), expand=c(0,0), breaks = seq(0,ylim,4)) +
  theme_market +
  labs(x = "Q", y = "P", title = "Market") +
  annotate("segment", x = 0, xend = 31.5, y = mc(be), yend = mc(be), size = 1, color = purple) +
  annotate("text", label = "S[LR]", x = 30.5, y = 20, color = purple, size = 6, parse = T)
mark1

fig1 <- ggarrange(firm1, mark1, ncol = 2, nrow = 1)
fig1

#current.vpTree()
#downViewport('panel-4-6')
pushViewport(dataViewport( yscale=c(0,36), clip='off',xscale=c(0,64)))

grid.lines(x=c(4.75,36), y = c(21.12,21.12), default.units='native', gp=gpar(lty = 2, lwd = 1))

ggexport(fig1, filename = "test3.pdf")


###### Linear Example

x <- seq(0,32,0.1)
z <- seq(0.1,32,0.1)
# z0 <- seq(6,shutq,0.1)
# z1 <- seq(shutq,32,0.1)

xlim <- max(x)
ylim <- 36+1
tc <- function(x) 10*x+(9/20)*(x^2)+45
vc <- function(x) tc(x)-45

atc <- function(x) tc(x)/x
avc <- function(x) vc(x)/x

mc <- function(x) 10+(9/10)*x

be <- 10
atc_min <- 19

ybreaks <- seq(0,ylim,2)
ylabs <- ybreaks
ylabs[(ybreaks %% 4 == 0)] <- ''

firm2 <- ggplot() +
  scale_x_continuous(limits = c(0, xlim), expand=c(0,0), breaks = seq(0,xlim,5)) +
  scale_y_continuous(limits = c(0, ylim), expand=c(0,0), breaks = ybreaks) +
  theme_sd +
  labs(x = "Q", y = "$", title = "Individual Firm") +
  geom_line(aes(x=z, y=atc(z)), size = 1, color = purple) +
  annotate("text", label = "ATC", x = 30, y = 27, color = purple, size = 4.5) +
  geom_line(aes(x=x, y=avc(x)), size = 1, color = orange) +
  annotate("text", label = "AVC", x = 30, y = 21, color = orange, size = 4.5) +
  geom_line(aes(x=x, y=mc(x)), size = 1, color = green) +
  annotate("text", label = "MC", x = 30, y = 35, color = green, size = 4.5)
firm2

ggsave("linex1.png", device = "png")

firm3 <- firm2 +
  annotate("point", x = be, y=atc_min, size = 3) +
  annotate("point", x = 0, y=10, size = 3) +
  annotate("segment", x = be, xend = be, y = 0, yend = atc_min, linetype = 2, size = 1) +
  annotate("segment", x = 0, xend = be, y = atc_min, yend = atc_min, linetype = 2, size = 1)
firm3

gt <- ggplot_gtable(ggplot_build(firm3))
gt$layout$clip[gt$layout$name=="panel"] <- "off"
grid.draw(gt)




supply <- function(x) mc(x/50)
x2 <- seq(0,32*50,1)
xlim2 <- 32*50

mark2 <- ggplot() +
  scale_x_continuous(limits = c(0, xlim2), expand=c(0,0), breaks = seq(0,xlim2,5*50)) +
  scale_y_continuous(limits = c(0, ylim), expand=c(0,0), breaks = seq(0,ylim,2)) +
  theme_sd +
  labs(x = "Q", y = "P", title = "Market") +
  geom_line(aes(x=x2, y=supply(x2)), size = 1, color = purple) +
  annotate("text", label = "S[SR]", x = 29*50, y = 34, color = purple, size = 4.5, parse = T) +
  annotate("segment", x = 0, xend = 31*50, y = mc(be), yend = mc(be), size = 1, color = "purple") +
  annotate("text", label = "S[LR]", x = 29*50, y = 17.5, color = "purple", size = 4.5, parse = T)
mark2

fig2 <- grid.arrange(firm2, mark2, nrow = 1)
fig2

ggsave("test6.png", device = "png")

firm4 <- ggplot() +
  scale_x_continuous(limits = c(0, xlim), expand=c(0,0), breaks = seq(0,xlim,5)) +
  scale_y_continuous(limits = c(0, ylim), expand=c(0,0), breaks = ybreaks) +
  theme_sd +
  labs(x = "Q", y = "$", title = "Individual Firm") +
  geom_line(aes(x=z, y=atc(z)), size = 1, color = purple) +
  annotate("text", label = "ATC", x = 4, y = 35, color = purple, size = 4.5) +
  geom_line(aes(x=x, y=avc(x)), size = 1, color = orange) +
  annotate("text", label = "AVC", x = 4, y = 10, color = orange, size = 4.5) +
  geom_line(aes(x=x, y=mc(x)), size = 1, color = green) +
  annotate("text", label = "MC", x = 25, y = 35, color = green, size = 4.5)
firm4

firm5 <- firm4 +
  annotate("segment", x = 0, xend = 32, y = 30, yend = 30, size = 1, color = red_pink) +
  annotate("text", label = "P=MR", x = 30, y = 31.5, color = red_pink, size = 4.5) +
  annotate("segment", x = 22.222, xend = 32, y = 30, yend = 30, size = 1, linetype = 2) +
  annotate("point", x = 22.222, y=30, size = 4)
firm5

d1 <- function(x) 36-(27/5000)*x

mark5 <- mark2 +  
  geom_line(aes(x=x2, y=d1(x2)), size = 1, color = red_pink) +
  annotate("text", label = "D", x = 29*50, y = 27, color = red_pink, size = 4.5) + 
  annotate("segment", x = 0, xend = 10000/9, y = 30, yend = 30, size = 1, linetype = 2) +
  annotate("point", x = 10000/9, y=30, size = 4)
mark5  

fig3 <- grid.arrange(firm5, mark5, nrow = 1)
fig3

ggsave(plot = fig3, "linex4.png", device = "png")



d2 <- function(x) 36-(14/666.666)*x

mark6 <- mark2 +  
  geom_line(aes(x=x2, y=d2(x2)), size = 1, color = red_pink) +
  annotate("text", label = "D", x = 29*50, y = 8, color = red_pink, size = 4.5) + 
  annotate("segment", x = 0, xend = 666.666, y = 22, yend = 22, size = 1, linetype = 2) +
  annotate("point", x = 666.666, y=22, size = 4) +
  annotate("point", x = 809.524, y=19, size = 4) +
  annotate("text", label = "E[SR]", x = 666.666, y =24.5, size = 4.5, parse = T) +
  annotate("text", label = "E[LR]", x = 809.524, y = 21.5, size = 4.5, parse = T)
mark6  

firm6 <- firm4 +
  annotate("segment", x = 0, xend = 32, y = 22, yend = 22, size = 1, color = red_pink) +
  annotate("text", label = "P=MR", x = 30, y = 20, color = red_pink, size = 4.5) +
  annotate("segment", x = 13.333, xend = 32, y = 22, yend = 22, size = 1, linetype = 2) +
  annotate("point", x = 13.333, y=22, size = 4) + 
  annotate("point", x = 10, y=19, size = 4)
firm6



fig4 <- grid.arrange(firm6, mark6, nrow = 1)
fig4

ggsave(plot = fig4, "linex5.png", device = "png")



####
d2 <- function(x) 36-(14/666.666)*x
s2 <- function(x) supply(x) - (10-4.4285729)

mark7 <- ggplot() +
  scale_x_continuous(limits = c(0, xlim2), expand=c(0,0), breaks = seq(0,xlim2,5*50)) +
  scale_y_continuous(limits = c(0, ylim), expand=c(0,0), breaks = seq(0,ylim,2)) +
  theme_sd +
  labs(x = "Q", y = "P", title = "Market") +
  geom_line(aes(x=x2, y=supply(x2)), size = 1, color = purple, alpha = 0.5) +
  annotate("text", label = "S[SR]", x = 24*50, y = 34, color = purple, 
           size = 4.5, parse = T, alpha = 0.5) +
  annotate("segment", x = 0, xend = 31*50, y = mc(be), yend = mc(be), size = 1, color = "purple") +
  annotate("text", label = "S[LR]", x = 29*50, y = 17.5, color = "purple", 
           size = 4.5, parse = T) +  
  geom_line(aes(x=x2, y=d2(x2)), size = 1, color = red_pink) +
  annotate("text", label = "D", x = 29*50, y = 8, color = red_pink, size = 4.5) + 
  annotate("segment", x = 0, xend = 666.666, y = 22, yend = 22, size = 1, linetype = 2) +
  annotate("point", x = 666.666, y=22, size = 4, alpha = 0.5) +
  annotate("text", label = "E[SR]", x = 666.666, y =24.5, 
           size = 4.5, parse = T, alpha = 0.5) +
  annotate("text", label = "E[LR]", x = 809.524, y = 21.5, size = 4.5, parse = T) +
  
  geom_line(aes(x=x2, y=s2(x2)), size = 1, color = purple) + 
  annotate("text", label = "S*minute[SR]", x = 30*50, y = 29, color = purple, size = 4.5, parse = T) +
  annotate("segment", x = 1050, xend = 1250, y = 28, yend = 28,
           arrow = arrow(length = unit(0.4, "lines"), ends = "last"), 
           colour = "grey70", size = 1) + 
  annotate("segment", x = 250, xend = 500, y = 14, yend = 14,
           arrow = arrow(length = unit(0.4, "lines"), ends = "last"), 
           colour = "grey70", size = 1) + 
  annotate("point", x = 809.524, y=19, size = 4)
mark7  

firm7 <- ggplot() +
  scale_x_continuous(limits = c(0, xlim), expand=c(0,0), breaks = seq(0,xlim,5)) +
  scale_y_continuous(limits = c(0, ylim), expand=c(0,0), breaks = ybreaks) +
  theme_sd +
  labs(x = "Q", y = "$", title = "Individual Firm") +
  geom_line(aes(x=z, y=atc(z)), size = 1, color = purple) +
  annotate("text", label = "ATC", x = 4, y = 35, color = purple, size = 4.5) +
  geom_line(aes(x=x, y=avc(x)), size = 1, color = orange) +
  annotate("text", label = "AVC", x = 4, y = 10, color = orange, size = 4.5) +
  geom_line(aes(x=x, y=mc(x)), size = 1, color = green) +
  annotate("text", label = "MC", x = 25, y = 35, color = green, size = 4.5) +
  annotate("segment", x = 0, xend = 32, y = 22, yend = 22, size = 1, color = red_pink, alpha = 0.5) +
  annotate("text", label = "P=MR", x = 30, y = 21, color = red_pink, size = 4.5, alpha = 0.5) +
  annotate("point", x = 13.333, y=22, size = 4) + 
  annotate("point", x = 10, y=19, size = 4) +

annotate("segment", x = 0, xend = 32, y = 19, yend = 19, size = 1, color = red_pink) +
  annotate("text", label = "P=MR", x = 30, y = 17, color = red_pink, size = 4.5) 
firm7



fig5 <- grid.arrange(firm7, mark7, nrow = 1)
fig5

ggsave(plot = fig5, "linex6.png", device = "png")








































### For next time?
####

x <- seq(0,32,0.1)
z <- seq(0.1,32,0.1)
# z0 <- seq(6,shutq,0.1)
# z1 <- seq(shutq,32,0.1)

xlim <- max(x)
ylim <- 36+1
tc <- function(x) 10*x+(9/20)*(x^2)+45
vc <- function(x) tc(x)-45

atc <- function(x) tc(x)/x
avc <- function(x) vc(x)/x

mc <- function(x) 10+(9/10)*x

be <- 10
atc_min <- 19

ybreaks <- seq(0,ylim,2)
ylabs <- ybreaks
ylabs[(ybreaks %% 4 == 0)] <- ''

firm2 <- ggplot() +
  scale_x_continuous(limits = c(0, xlim), expand=c(0,0), breaks = seq(0,xlim,5)) +
  scale_y_continuous(limits = c(0, ylim), expand=c(0,0), breaks = ybreaks) +
  theme_sd +
  labs(x = "Q", y = "$", title = "Individual Firm") +
  geom_line(aes(x=z, y=atc(z)), size = 1, color = purple) +
  annotate("text", label = "ATC", x = 30, y = 27, color = purple, size = 4.5) +
  geom_line(aes(x=x, y=avc(x)), size = 1, color = orange) +
  annotate("text", label = "AVC", x = 30, y = 21, color = orange, size = 4.5) +
  geom_line(aes(x=x, y=mc(x)), size = 1, color = green) +
  annotate("text", label = "MC", x = 30, y = 35, color = green, size = 4.5)
firm2

#ggsave("linex1.png", device = "png")

firm3 <- firm2 +
  annotate("point", x = be, y=atc_min, size = 3) +
  annotate("point", x = 0, y=10, size = 3) +
  annotate("segment", x = be, xend = be, y = 0, yend = atc_min, linetype = 2, size = 1) +
  annotate("segment", x = 0, xend = be, y = atc_min, yend = atc_min, linetype = 2, size = 1)
firm3

# gt <- ggplot_gtable(ggplot_build(firm3))
# gt$layout$clip[gt$layout$name=="panel"] <- "off"
# grid.draw(gt)




supply <- function(x) mc(x/50)
x2 <- seq(0,32*50,1)
xlim2 <- 32*50

mark2 <- ggplot() +
  scale_x_continuous(limits = c(0, xlim2), expand=c(0,0), breaks = seq(0,xlim2,5*50)) +
  scale_y_continuous(limits = c(0, ylim), expand=c(0,0), breaks = seq(0,ylim,2)) +
  theme_sd +
  labs(x = "Q", y = "P", title = "Market") +
  geom_line(aes(x=x2, y=supply(x2)), size = 1, color = purple) +
  annotate("text", label = "S[SR]", x = 29*50, y = 34, color = purple, size = 4.5, parse = T)
mark2

fig2 <- grid.arrange(firm2, mark2, nrow = 1)
fig2

#ggsave("test6.png", device = "png")

firm4 <- ggplot() +
  scale_x_continuous(limits = c(0, xlim), expand=c(0,0), breaks = seq(0,xlim,5)) +
  scale_y_continuous(limits = c(0, ylim), expand=c(0,0), breaks = ybreaks) +
  theme_sd +
  labs(x = "Q", y = "$", title = "Individual Firm") +
  geom_line(aes(x=z, y=atc(z)), size = 1, color = purple) +
  annotate("text", label = "ATC", x = 4, y = 35, color = purple, size = 4.5) +
  geom_line(aes(x=x, y=avc(x)), size = 1, color = orange) +
  annotate("text", label = "AVC", x = 4, y = 10, color = orange, size = 4.5) +
  geom_line(aes(x=x, y=mc(x)), size = 1, color = green) +
  annotate("text", label = "MC", x = 25, y = 35, color = green, size = 4.5)
firm4

firm5 <- firm4 +
  annotate("segment", x = 0, xend = 32, y = 30, yend = 30, size = 1, color = red_pink) +
  annotate("text", label = "P=MR", x = 30, y = 31.5, color = red_pink, size = 4.5) +
  #annotate("segment", x = 22.222, xend = 32, y = 30, yend = 30, size = 1, linetype = 2) +
  annotate("point", x = 22.222, y=30, size = 4)
firm5

d1 <- function(x) 36-(27/5000)*x

mark5 <- mark2 +  
  geom_line(aes(x=x2, y=d1(x2)), size = 1, color = red_pink) +
  annotate("text", label = "D", x = 29*50, y = 27, color = red_pink, size = 4.5) + 
  annotate("segment", x = 0, xend = 10000/9, y = 30, yend = 30, size = 1, linetype = 2) +
  annotate("point", x = 10000/9, y=30, size = 4)
mark5  

fig3 <- grid.arrange(firm5, mark5, nrow = 1)
fig3

ggsave(plot = fig3, "test who knows.png", device = "png", width = 10, height = 4.36)



d2 <- function(x) 36-(14/666.666)*x

mark6 <- mark2 +  
  geom_line(aes(x=x2, y=d2(x2)), size = 1, color = red_pink) +
  annotate("text", label = "D", x = 29*50, y = 8, color = red_pink, size = 4.5) + 
  annotate("segment", x = 0, xend = 666.666, y = 22, yend = 22, size = 1, linetype = 2) +
  annotate("point", x = 666.666, y=22, size = 4) +
  annotate("point", x = 809.524, y=19, size = 4) +
  annotate("text", label = "E[SR]", x = 666.666, y =24.5, size = 4.5, parse = T) +
  annotate("text", label = "E[LR]", x = 809.524, y = 21.5, size = 4.5, parse = T)
mark6  

firm6 <- firm4 +
  annotate("segment", x = 0, xend = 32, y = 22, yend = 22, size = 1, color = red_pink) +
  annotate("text", label = "P=MR", x = 30, y = 20, color = red_pink, size = 4.5) +
  annotate("segment", x = 13.333, xend = 32, y = 22, yend = 22, size = 1, linetype = 2) +
  annotate("point", x = 13.333, y=22, size = 4) + 
  annotate("point", x = 10, y=19, size = 4)
firm6



fig4 <- grid.arrange(firm6, mark6, nrow = 1)
fig4

ggsave(plot = fig4, "linex5.png", device = "png")
