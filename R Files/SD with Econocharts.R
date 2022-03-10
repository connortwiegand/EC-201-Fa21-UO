if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, dplyr, magrittr, ggplot2)

library(econocharts)



supply1 <- data.frame(x = c(1, 9), y = c(1, 9))
demand1 <- data.frame(x = c(7, 1), y = c(1, 7))
demand2 <- data.frame(x = c(9, 2), y = c(2, 9))

DR <- sdcurve(supply1, demand1, supply1, demand2,
             equilibrium = TRUE, names = c("S[1]", "D[1]","S[1]", "D[2]"), 
             linescol = c("black", "black", "black", 4),
             ylab="Price", xlab="Quantity", main = "Market for Pizza") 
DR + annotate("segment", x = 1.5, xend = 4, y = 6.75, yend = 6.75,
             arrow = arrow(length = unit(0.3, "lines")), colour = "grey80")+
  annotate("segment", x = 5.33, xend = 7.75, y = 3, yend = 3, # Add arrows
           arrow = arrow(length = unit(0.3, "lines")), colour = "grey80") +
  #theme(axis.title.x = element_text(hjust=1), axis.title.y = element_text(hjust=1, angle = 360)) +
  labs(title = expression(underline("Market for Pizza"))) + theme(plot.title = element_text(hjust=0.40)) +
  theme(axis.line = element_line(size = 1, colour = "black", linetype=1))




supply1 <- data.frame(x = c(1, 9), y = c(1, 9))
demand1 <- data.frame(x = c(7, 1), y = c(1, 7))
demand2 <- data.frame(x = c(9, 2), y = c(2, 9))

DL <- sdcurve(supply1, demand2, supply1, demand1,
              equilibrium = TRUE, names = c("S[1]", "D[1]","S[1]", "D[2]"), 
              linescol = c("black", "black", "black", 4),
              ylab="Price", xlab="Quantity", main = "Market for Pizza") 
DL + annotate("segment", xend = 1.5, x = 4, y = 6.75, yend = 6.75,
              arrow = arrow(length = unit(0.3, "lines")), colour = "grey80")+
  annotate("segment", xend = 5.33, x = 7.75, y = 3, yend = 3, # Add arrows
           arrow = arrow(length = unit(0.3, "lines")), colour = "grey80") +
  #theme(axis.title.x = element_text(hjust=1), axis.title.y = element_text(hjust=1, angle = 360)) +
  labs(title = expression(underline("Market for Pizza"))) + theme(plot.title = element_text(hjust=0.40)) +
  theme(axis.line = element_line(size = 1, colour = "black", linetype=1))


demand1 <- data.frame(x = c(7, 1), y = c(1, 7))
supply1 <- data.frame(x = c(1, 8), y = c(1, 8))
supply2 <- data.frame(x = c(2.5, 9), y = c(0.5, 7))

SR <- sdcurve(supply1, demand1, supply2, demand1,
              equilibrium = TRUE, names = c("S[1]", "D[1]","S[2]", "D[1]"), 
              linescol = c("black", "black", 3, "black"),
              ylab="Price", xlab="Quantity", main = "Market for Pizza") 

SR + annotate("segment", x = 5.75, xend = 7.25, y = 5.5, yend = 5.5,
              arrow = arrow(length = unit(0.3, "lines")), colour = "grey80")+
  annotate("segment", x = 2, xend = 3.5, y = 1.75, yend = 1.75, # Add arrows
           arrow = arrow(length = unit(0.3, "lines")), colour = "grey80") +
  #theme(axis.title.x = element_text(hjust=1), axis.title.y = element_text(hjust=1, angle = 360)) +
  labs(title = expression(underline("Market for Pizza"))) + theme(plot.title = element_text(hjust=0.40)) +
  theme(axis.line = element_line(size = 1, colour = "black", linetype=1))


demand1 <- data.frame(x = c(7, 1), y = c(1, 7))
supply1 <- data.frame(x = c(1, 8), y = c(1, 8))
supply2 <- data.frame(x = c(2.5, 9), y = c(0.5, 7))

SL <- sdcurve(supply2, demand1, supply1, demand1,
              equilibrium = TRUE, names = c("S[1]", "D[1]","S[2]", "D[1]"), 
              linescol = c("black", "black", 3, "black"),
              ylab="Price", xlab="Quantity", main = "Market for Pizza") 

SL + annotate("segment", xend = 5.75, x = 7.25, y = 5.5, yend = 5.5,
              arrow = arrow(length = unit(0.3, "lines")), colour = "grey80")+
  annotate("segment", xend = 2, x = 3.5, y = 1.75, yend = 1.75, # Add arrows
           arrow = arrow(length = unit(0.3, "lines")), colour = "grey80") +
  #theme(axis.title.x = element_text(hjust=1), axis.title.y = element_text(hjust=1, angle = 360)) +
  labs(title = expression(underline("Market for Pizza"))) + theme(plot.title = element_text(hjust=0.40)) +
  theme(axis.line = element_line(size = 1, colour = "black", linetype=1))


demand1 <- data.frame(x = c(7, 1), y = c(1, 7))
demand2 <- data.frame(x = c(7.5, 2), y = c(2, 7.5))
supply1 <- data.frame(x = c(1, 8), y = c(1, 8))
supply2 <- data.frame(x = c(3.5, 10.5), y = c(0.5, 7.5))

SR_DR <- sdcurve(supply1, demand1, supply2, demand2,
              equilibrium = TRUE, names = c("S[1]", "D[1]","S[2]", "D[2]"), 
              linescol = c("black", "black", 3, 4),
              ylab="Price", xlab="Quantity", main = "Market for Seltzer") 

SR_DR + #annotate("segment", xend = 5.75, x = 7.25, y = 5.5, yend = 5.5,
              #arrow = arrow(length = unit(0.3, "lines")), colour = "grey80")+
  #annotate("segment", xend = 2, x = 3.5, y = 1.75, yend = 1.75, # Add arrows
           #arrow = arrow(length = unit(0.3, "lines")), colour = "grey80") +
  #theme(axis.title.x = element_text(hjust=1), axis.title.y = element_text(hjust=1, angle = 360)) +
  labs(title = expression(underline("Market for Seltzer"))) + theme(plot.title = element_text(hjust=0.40)) +
  theme(axis.line = element_line(size = 1, colour = "black", linetype=1))



demand1 <- data.frame(x = c(7, 1), y = c(1, 7))
demand2 <- data.frame(x = c(9, 2), y = c(2, 9))
supply1 <- data.frame(x = c(1, 8), y = c(1, 8))
supply2 <- data.frame(x = c(3.5, 10.5), y = c(0.5, 7.5))

SL_DR <- sdcurve(supply2, demand1, supply1, demand2,
                 equilibrium = TRUE, names = c("S[1]", "D[1]","S[2]", "D[2]"), 
                 linescol = c("black", "black", 3, 4),
                 ylab="Price", xlab="Quantity", main = "Market for Seltzer") 

SL_DR + #annotate("segment", xend = 5.75, x = 7.25, y = 5.5, yend = 5.5,
  #arrow = arrow(length = unit(0.3, "lines")), colour = "grey80")+
  #annotate("segment", xend = 2, x = 3.5, y = 1.75, yend = 1.75, # Add arrows
  #arrow = arrow(length = unit(0.3, "lines")), colour = "grey80") +
  #theme(axis.title.x = element_text(hjust=1), axis.title.y = element_text(hjust=1, angle = 360)) +
  labs(title = expression(underline("Market for Seltzer"))) + theme(plot.title = element_text(hjust=0.40)) +
  theme(axis.line = element_line(size = 1, colour = "black", linetype=1))


########


supply1 <- data.frame(Hmisc::bezier(c(1, 3, 9),
                                    c(9, 3, 1))) 

supply2 <- data.frame(Hmisc::bezier(c(2.5, 4.5, 10.5),
                                    c(10.5, 4.5, 2.5))) 

demand1 <- data.frame(Hmisc::bezier(c(1, 8, 9),
                                    c(1, 5, 9))) 

# Supply and demand curves and arrows
sdcurve(supply1, demand1, supply2, demand1,
        names = c("D[1]", "S[1]","D[2]", "S[1]")) +
  annotate("segment", x = 2.5, xend = 3.5, y = 7, yend = 7, # Add arrows
           arrow = arrow(length = unit(0.3, "lines")), colour = "grey50") +
  annotate("segment", x = 1, xend = 1, y = 3.5, yend = 4.5,               
           arrow = arrow(length = unit(0.3, "lines")), colour = "grey50") +
  annotate("segment", x = 5, xend = 6, y = 1, yend = 1,               
           arrow = arrow(length = unit(0.3, "lines")), colour = "grey50") + 
  theme_gray() + xlab("Quantity") + ylab("Price") + 
  theme(axis.title.x = element_text(hjust=1), axis.title.y = element_text(hjust=1, angle = 360))




#########

supply1 <- data.frame(x = c(1, 9), y = c(1, 9))
supply1

demand1 <- data.frame(x = c(7, 2), y = c(2, 7))
demand1

supply2 <- data.frame(x = c(2, 10), y = c(1, 9))
supply2

demand2 <- data.frame(x = c(8, 2), y = c(2, 8))
demand2

p <- sdcurve(supply1,   # Custom data
             demand1,
             supply2, 
             demand2,
             equilibrium = TRUE, # Calculate the equilibrium
             bg.col = "#fff3cd") # Background color
p + annotate("segment", x = 2.5, xend = 3, y = 6.5, yend = 7,                # Add more layers
             arrow = arrow(length = unit(0.3, "lines")), colour = "grey50") + 
  theme_gray() + xlab("Quantity") + ylab("Price") + 
  theme(axis.title.x = element_text(hjust=1), axis.title.y = element_text(hjust=1, angle = 360)) +
  theme(axis.line = element_line(size = 1, colour = "black", linetype=1))


# Data
demand <- function(Q) 20 - 0.5 * Q-5
supply <- function(Q) 2 + 0.25 * Q
supply_tax <- function(Q) supply(Q) + 5

# Chart
tax_graph(demand, supply, supply, shaded = F)


  


