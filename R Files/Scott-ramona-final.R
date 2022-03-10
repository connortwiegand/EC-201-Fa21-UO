if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, dplyr, magrittr, ggplot2)
# Must install through github:
# Even if plotting with ggplot2, contains curve_intersect function
library(econocharts)
p_load(latex2exp)
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


####Scott/Ramona Graphs

ppf_1 <- function(x) if_else(x<40, 70-(20/40)*x, 50-(50/(115-40))*(x-40))

x <- seq(0,115,0.1)
xlim <- max(x)
ylim <- 90

ppf1 <- ggplot() +
  scale_x_continuous(limits = c(0, xlim), expand=c(0,0), breaks = c(0,40,115)) +
  scale_y_continuous(limits = c(0, ylim), expand=c(0,0), breaks = c(0,50,70)) +
  theme_market +
  labs(x = "Tea", y = "GB", title = "(A)") +
  geom_line(aes(x=x, y=ppf_1(x)), size = 1, color = purple) 
ppf1



ppf_2 <- function(x) if_else(x<75, 70-(50/75)*x, 20-(20/(115-75))*(x-75))

x <- seq(0,115,0.1)
xlim <- 115
ylim <- 90

ppf2 <- ggplot() +
  scale_x_continuous(limits = c(0, xlim), expand=c(0,0), breaks = c(0,75,115)) +
  scale_y_continuous(limits = c(0, ylim), expand=c(0,0), breaks = c(0,20,70)) +
  theme_market +
  labs(x = "Tea", y = "GB", title = "(B)") +
  geom_line(aes(x=x, y=ppf_2(x)), size = 1, color = purple) 
ppf2



ppf_3 <- function(x) (14*sqrt(13225-x^2))/23

x <- seq(0,115,0.1)
xlim <- 115
ylim <- 90

ppf3 <- ggplot() +
  scale_x_continuous(limits = c(0, xlim), expand=c(0,0), breaks = c(0,75,115)) +
  scale_y_continuous(limits = c(0, ylim), expand=c(0,0), breaks = c(0,20,70)) +
  theme_market +
  labs(x = "Tea", y = "GB", title = "(C)") +
  geom_line(aes(x=x, y=ppf_3(x)), size = 1, color = purple) 
ppf3


ppf_4 <- function(x) (1000/(0.608696*x+12.1))-12.1

x <- seq(0.1,115,0.1)
xlim <- 115
ylim <- 90

ppf4 <- ggplot() +
  scale_x_continuous(limits = c(0, xlim), expand=c(0,0), breaks = c(0,75,115)) +
  scale_y_continuous(limits = c(0, ylim), expand=c(0,0), breaks = c(0,20,70)) +
  theme_market +
  labs(x = "Tea", y = "GB", title = "(D)") +
  geom_line(aes(x=x, y=ppf_4(x)), size = 1, color = purple) 
ppf4

fig_ppf <- grid.arrange(ppf1,ppf2,ppf3,ppf4, nrow = 1) 


ggsave(plot = fig_ppf, filename = "GBT PPFs.png", device = "png", height = 4.36, width = 12)






