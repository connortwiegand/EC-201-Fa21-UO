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



theme_market <- theme_sd
total_cost <- function(x) 10 + 10*x - 4*x^2 + x^3
vc <- function(x) 10*x - 4*x^2 + x^3
fc <- function(x) 10

ggplot(data = data.frame(x = 0), mapping = aes(x = x)) +
  scale_x_continuous(limits = c(0, 5.5), expand=c(0,0), breaks = scales::pretty_breaks()) +
  scale_y_continuous(limits = c(0, 65), expand=c(0,0), breaks = scales::pretty_breaks()) +
  theme_sd +
  labs(x = "Q", y = "$") +
  stat_function(fun = fc, color = green, size = 1) +
  stat_function(fun = total_cost, color = purple, size = 1) +
  stat_function(fun = vc, color = red_pink, size = 1) +
  annotate("text", label = "TC", x = 3.6, y = 50, color = purple, size = 7) +
  annotate("text", label = "VC", x = 4.75, y = 50, color = red_pink, size = 7) +
  annotate("text", label = "FC", x = 4.75, y = 13, color = green, size = 7)

ggsave("tc vc fc.png", device = "png")

afc <- function(x) 10/x
ggplot(data = data.frame(x = 0), mapping = aes(x = x)) +
  scale_x_continuous(limits = c(0, 5.5), expand=c(0,0), breaks = scales::pretty_breaks()) +
  scale_y_continuous(limits = c(0, 65), expand=c(0,0), breaks = scales::pretty_breaks()) +
  theme_sd +
  labs(x = "Q", y = "$") +
  stat_function(fun = afc, color = green, size = 1) +
  annotate("text", label = "AFC", x = 4.75, y = 5.5, color = green, size = 7)

ggsave("afc.png", device = "png")

atc <- function(x) (10 + 10*x - 4*x^2 + x^3)/x
avc <- function(x) (10*x - 4*x^2 + x^3)/x
ggplot(data = data.frame(x = 0), mapping = aes(x = x)) +
  scale_x_continuous(limits = c(0, 5.5), expand=c(0,0), breaks = scales::pretty_breaks()) +
  scale_y_continuous(limits = c(0, 65), expand=c(0,0), breaks = scales::pretty_breaks()) +
  theme_market +
  labs(x = "Q", y = "$") +
  stat_function(fun = atc, color = purple, size = 1) +
  annotate("text", label = "ATC", x = 0.7, y = 50, color = purple, size = 7) +
  # stat_function(fun = afc, color = green, size = 1) +
  # annotate("text", label = "AFC", x = 4.75, y = 5.5, color = green, size = 9) +
  stat_function(fun = avc, color = red_pink, size = 1) +
  annotate("text", label = "AVC", x = 0.4, y = 5, color = red_pink, size = 7)

ggsave("atc avc.png", device = "png")

marginal_cost <- function(x) 10 - 8*x + 3*x^2
min_atc <- min(atc(seq(0, 6, 0.1)))

which.min(atc(seq(0, 6, 0.1)))

seq(0, 6, 0.1)[28]

ggplot(data = data.frame(x = 0), mapping = aes(x = x)) +
  scale_x_continuous(limits = c(0, 5.5), expand=c(0,0), breaks = scales::pretty_breaks()) +
  scale_y_continuous(limits = c(0, 65), expand=c(0,0), breaks = scales::pretty_breaks()) +
  theme_market +
  labs(x = "Q", y = "$") +
  stat_function(fun = atc, color = purple, size = 1) +
  annotate("text", label = "ATC", x = 0.7, y = 50, color = purple, size = 7) +
  stat_function(fun = marginal_cost, color = red_pink, size = 1) +
  annotate("text", label = "MC", x = 4.8, y = 50, color = red_pink, size = 7) +
  annotate("point", x=2.7, y=min_atc, size = 4) +
  annotate("text", label = "MC intersects ATC at\n the minimum point of ATC", 
         x = 3.75, y = 6, color = "grey40", size = 4)

ggsave("mc atc.png", device = "png")


cost_data <- tibble(
  q = seq(0, 6, 0.01),
  atc_s = 18 - 8*q + 3*q^2,
  atc_m = 30- 16*q + 3*q^2,
  atc_l = 60 - 24*q + 3*q^2
) %>% 
  mutate(atc = case_when(atc_s <= atc_m & atc_s <= atc_l ~ atc_s,
                         atc_m <= atc_s & atc_m <= atc_l ~ atc_m,
                         atc_l <= atc_s & atc_l <= atc_m ~ atc_l))
cost_data %>% 
  ggplot() +
  scale_x_continuous(limits = c(0, 5.5), expand=c(0,0), breaks = scales::pretty_breaks()) +
  scale_y_continuous(limits = c(0, 27), expand=c(0,0), breaks = scales::pretty_breaks()) +
  geom_line(aes(x = q, y = atc_s), color = red_pink, size = 1, linetype = "solid") +
  geom_line(aes(x = q, y = atc_m), color = green, size = 1, linetype = "solid") +
  geom_line(aes(x = q, y = atc_l), color = orange, size = 1, linetype = "solid") +
  theme_market +
  annotate("text", label = expression(ATC[S]), x = 0.7, y = 16, color = red_pink,  size = 6) +
  annotate("text", label = expression(ATC[M]), x = 2.7, y = 10, color = green,  size = 6) +
  annotate("text", label = expression(ATC[L]), x = 4.95, y = 18, color = orange,  size = 6) +
  labs(x = "Q", y = "$")

ggsave("3 atcs.png", device = "png")

cost_data %>% 
  ggplot() +
  scale_x_continuous(limits = c(0, 5.5), expand=c(0,0), breaks = scales::pretty_breaks()) +
  scale_y_continuous(limits = c(0, 27), expand=c(0,0), breaks = scales::pretty_breaks()) +
  geom_line(aes(x = q, y = atc_s), color = red_pink, size = 1, linetype = "solid") +
  geom_line(aes(x = q, y = atc_m), color = green, size = 1, linetype = "solid") +
  geom_line(aes(x = q, y = atc_l), color = orange, size = 1, linetype = "solid") +
  geom_line(aes(x = q, y = atc), color = purple, size = 1.5, linetype = "solid") +
  theme_market +
  annotate("text", label = expression(ATC[S]), x = 0.7, y = 16, color = red_pink, size = 6) +
  annotate("text", label = expression(ATC[M]), x = 2.7, y = 10, color = green, size = 6) +
  annotate("text", label = expression(LRATC), x = 3.75, y = 8.5, color = purple, size = 7) +
  annotate("text", label = expression(ATC[L]), x = 4.95, y = 18, color = orange, size = 6) +
  labs(x = "Q", y = "$")

ggsave("atcs lratc.png", device = "png")


lratc <- function(x) 10 - 2.75*x + 0.5*x^2
ggplot(data = data.frame(x = 0), mapping = aes(x = x)) +
  scale_x_continuous(limits = c(0, 5.5), expand=c(0,0), breaks = scales::pretty_breaks()) +
  scale_y_continuous(limits = c(0, 11), expand=c(0,0), breaks = scales::pretty_breaks()) +
  theme_market +
  labs(x = "Q", y = "$") +
  stat_function(fun = lratc, color = purple, size = 1) +
  annotate("text", label = "LRATC", x = 4.5, y = 6.4, color = purple,  size = 6)

ggsave("lratc.png", device = "png")

lratc_e <- function(x) 10 - 2.75*x + 0.5*x^2
lratc_c <- function(x) 6.21875
lratc_d <- function(x) 10 - 2.75*(x-2) + 0.5*(x-2)^2

e_range <- seq(0,2.5,0.1)
c_range <- seq(2.5,5,0.1)
d_range <- seq(5,7.5,0.1)

ggplot() +
  scale_x_continuous(limits = c(0, 8), expand=c(0,0), breaks = scales::pretty_breaks()) +
  scale_y_continuous(limits = c(0, 11), expand=c(0,0), breaks = scales::pretty_breaks()) +
  theme_market +
  labs(x = "Q", y = "$", title = "LRATC") +
  geom_line(aes(x=e_range, y=lratc_e(e_range)), size = 1, color = purple) + 
  geom_line(aes(x=c_range, y=lratc_c(c_range)), size = 1, color = purple) + 
  geom_line(aes(x=d_range, y=lratc_d(d_range)), size = 1, color = purple) + 
  geom_vline(xintercept = 2.5, color = "grey30", linetype = 2, size = 1) +
  geom_vline(xintercept = 5, color = "grey30", linetype = 2, size = 1) +
  annotate("text", label = "Economies \nof Scale", x = 1.25, y = 5.4, color = "grey30",  size = 4) +
  annotate("text", label = "Constant Returns \nto Scale", x = 3.75, y = 5.4, color = "grey30",  size = 4) +
  annotate("text", label = "Diseconomies \nof Scale", x = 6.5, y = 5.4, color = "grey30",  size = 4)


ggsave("longer lratc.png", device = "png")


### Later:

theme_market <- theme_bw() + theme(
  axis.line = element_line(color = met_slate),
  panel.grid = element_blank(),
  rect = element_blank(),
  strip.text = element_blank(),
  text = element_text(color = met_slate),
  axis.title.x = element_text(hjust = 1, size = 15),
  axis.title.y = element_text(hjust = 1, angle = 0, size = 15),
  #axis.ticks = element_blank()
  plot.title = element_text(hjust=0.55),
  plot.margin = margin(t=2, r=13, b=2, l=2)
)

x <- seq(0, 8, 0.1)
atc <- function(x) (18 + 12*x - 2*x^2 + (2/3)*x^3)/x
avc <-  function(x) (12*x - 2*x^2 + (2/3)*x^3)/x
marginal_cost <- function(x) 12 - 4*x + 2*x^2
marginal_revenue <- function(x) 30
q <- uniroot(function(x) marginal_revenue(x) - marginal_cost(x), range(x))$root
p <- marginal_revenue(1)
cost <- atc(q)
z <- seq(0, q, 0.001)
p_max_1 <- ggplot() +
  scale_x_continuous(limits = c(0, 7.5), expand=c(0,0), breaks = c(q), labels = c(8)) +
  scale_y_continuous(limits = c(0, 37.5), expand=c(0,0), breaks = c(cost, p), labels = c(8, 12)) +
  theme_market +
  labs(x = "Q", y = "$") +
  #stat_function(aes(x), fun = atc, color = purple, size = 1) +
  geom_line(aes(x=x, y=atc(x)), size = 1, color = purple) + 
  annotate("text", label = "ATC", x = 6.5, y = 35, color = purple, size = 6) +
  #geom_line(aes(x=x, y=avc(x)), size = 1, color = red_pink) +
  #annotate("text", label = "AVC", x = 6.5, y = 8, color = red_pink, size = 7) +
  #stat_function(aes(x), fun = marginal_cost, color = red_pink, size = 1) +
  geom_line(aes(x=x, y=marginal_cost(x)), size = 1, color = green) +
  annotate("text", label = "MC", x = 1, y = 8, color = green, size = 6) +
  geom_point(aes(x = q, y = p), color = "black", size = 2) +
  geom_segment(aes(x = q, xend = q, y = 0, yend = p), color = "black", 
               linetype = "dashed", size = 1) +
  geom_line(aes(x=x, y=marginal_revenue(x)), size = 1, color = red, type=2) +
  annotate("text", label = "P=MR", x = 6.9, y = p - 2, color = red, size = 6)
p_max_1

ggsave("prod dec 1.png", device = "png")

p_max_1 +
  #stat_function(aes(x), fun = marginal_revenue, color = green, size = 1) +
  geom_point(aes(x = q, y = p), color = "grey30", size = 2) +
  geom_segment(aes(x = q, xend = q, y = 0, yend = p), color = "grey30", 
               linetype = "dashed", size = 1) +
  geom_point(aes(x = q, y = cost), color = met_slate, size = 2) +
  geom_segment(aes(x = 0, xend = q, y = cost, yend = cost), color = met_slate, linetype = "dashed", size = 1)  +
  geom_ribbon(aes(x = z, ymin = cost, ymax = p), fill = green, alpha = 0.2, linetype = "blank") +
  annotate("text", label = "Profit", x = q/2, y = (p + cost)/2, color = green, size = 6)

ggsave("pmax1.png", device = "png")

marginal_revenue <- function(x) 18
q <- uniroot(function(x) marginal_revenue(x) - marginal_cost(x), range(x))$root
p <- marginal_revenue(1)
cost <- atc(q)

p_max_2 <- ggplot() +
  scale_x_continuous(limits = c(0, 7.5), expand=c(0,0), breaks = c(q), labels = c(6)) +
  scale_y_continuous(limits = c(0, 37.5), expand=c(0,0), breaks = c(p), labels = c(7)) +
  theme_market +
  labs(x = "Q", y = "$") +
  geom_line(aes(x=x, y=atc(x)), size = 1, color = purple) + 
  annotate("text", label = "ATC", x = 6.5, y = 35, color = purple, size = 6) +
  geom_line(aes(x=x, y=marginal_cost(x)), size = 1, color = green) +
  annotate("text", label = "MC", x = 1, y = 8, color = green, size = 6) +
  geom_point(aes(x = q, y = p), color = "black", size = 2) +
  geom_segment(aes(x = q, xend = q, y = 0, yend = p), color = "black", 
               linetype = "dashed", size = 1) +
  geom_line(aes(x=x, y=marginal_revenue(x)), size = 1, color = red, type=2) +
  annotate("text", label = "P=MR", x = 6.9, y = p - 2, color = red, size = 6)
p_max_2

ggsave("pmax2.png", device = "png")


marginal_revenue <- function(x) 13
q <- uniroot(function(x) marginal_revenue(x) - marginal_cost(x), range(x))$root
p <- marginal_revenue(1)
cost <- atc(q)
z <- seq(0, q, 0.001)

p_max_3 <- ggplot() +
  scale_x_continuous(limits = c(0, 7.5), expand=c(0,0), breaks = c(q), labels = c(4)) +
  scale_y_continuous(limits = c(0, 37.5), expand=c(0,0), breaks = c(cost, p), labels = c(8, 5)) +
  theme_market +
  labs(x = "Q", y = "$") +
  #stat_function(aes(x), fun = atc, color = purple, size = 1) +
  geom_line(aes(x=x, y=atc(x)), size = 1, color = purple) + 
  annotate("text", label = "ATC", x = 6.5, y = 35, color = purple, size = 6) +
  #geom_line(aes(x=x, y=avc(x)), size = 1, color = red_pink) +
  #annotate("text", label = "AVC", x = 6.5, y = 8, color = red_pink, size = 7) +
  #stat_function(aes(x), fun = marginal_cost, color = red_pink, size = 1) +
  geom_line(aes(x=x, y=marginal_cost(x)), size = 1, color = green) +
  annotate("text", label = "MC", x = 1, y = 8, color = green, size = 6) +
  geom_point(aes(x = q, y = p), color = "black", size = 2) +
  geom_segment(aes(x = q, xend = q, y = 0, yend = p), color = "black", 
               linetype = "dashed", size = 1) +
  geom_line(aes(x=x, y=marginal_revenue(x)), size = 1, color = red, type=2) +
  annotate("text", label = "P=MR", x = 6.9, y = p - 2, color = red, size = 6)
p_max_3

ggsave("prod dec 3.png", device = "png")

p_max_3 +
  #stat_function(aes(x), fun = marginal_revenue, color = green, size = 1) +
  geom_point(aes(x = q, y = p), color = "grey30", size = 2) +
  geom_segment(aes(x = q, xend = q, y = 0, yend = p), color = "grey30", 
               linetype = "dashed", size = 1) +
  geom_point(aes(x = q, y = cost), color = met_slate, size = 2) +
  geom_segment(aes(x = 0, xend = q, y = cost, yend = cost), color = met_slate, linetype = "dashed", size = 1)  +
  geom_ribbon(aes(x = z, ymin = p, ymax = cost), fill = red_pink, alpha = 0.2, linetype = "blank") +
  annotate("text", label = "Loss", x = q/2, y = (p + cost)/2, color = red_pink, size = 6) +
  geom_segment(aes(x = q, xend = q, y = 0, yend = cost), color = "black", 
               linetype = "dashed", size = 1)

ggsave("pmax3.png", device = "png")

