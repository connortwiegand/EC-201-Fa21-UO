if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, dplyr, magrittr, ggplot2)

p <- c(0,0.5,1,2,3,4,5)
qc <- c(18,10,8,4,2,1,0)

q2 <- c(18,12,8,3,2,1,0)
q3 <- c(24,11,9,7,5,2,1)

diplot <- ggplot() + geom_line(aes(y=p,x=qc),col="green") + 
  xlab("Qauntity Demanded") + ylab("Price")
diplot

diplot <- diplot + geom_line(aes(y=p,x=q2), col = "red")
diplot

diplot <- diplot + geom_line(aes(y=p,x=q3), col = "blue")
diplot

#Market
dplot <- ggplot() + geom_line(aes(y=p,x=qc+q2+q3),col="black")+ 
  xlab("Qauntity Demanded") + ylab("Price")
dplot
#market + individuals 
dplot + 
  geom_line(aes(y=p,x=qc),col="green") + 
  geom_line(aes(y=p,x=q2), col = "red") +
  geom_line(aes(y=p,x=q3), col = "blue")



p <- c(0,5,8,10,12,15,20)

qb <- c(20,10,8,5,3,2,0)
qc <- c(25,15,9,6,4,3,2)

#Market
dplot <- ggplot() + geom_line(aes(y=p,x=qb+qc),col="black")+ 
  xlab("Qauntity Demanded") + ylab("Price")
#market + individuals 
dplot + 
  geom_line(aes(y=p,x=qb),col="green") + 
  geom_line(aes(y=p,x=qc), col = "red")



### Supply 

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, dplyr, magrittr, ggplot2)

p <- c(0,1,2,3,4,5)
qc <- c(0,2,3,4,4.5,5)

q2 <- c(0,1,2,3,4,5)
q3 <- c(0,0,1,1.5,2,2.25)

diplot <- ggplot() + geom_line(aes(y=p,x=qc),col="green") + 
  xlab("Qauntity Demanded") + ylab("Price")
diplot

diplot <- diplot + geom_line(aes(y=p,x=q2), col = "red")
diplot

diplot <- diplot + geom_line(aes(y=p,x=q3), col = "blue")
diplot

#Market
dplot <- ggplot() + geom_line(aes(y=p,x=qc+q2+q3),col="black")+ 
  xlab("Qauntity Demanded") + ylab("Price")
dplot
#market + individuals 
dplot + 
  geom_line(aes(y=p,x=qc),col="green") + 
  geom_line(aes(y=p,x=q2), col = "red") +
  geom_line(aes(y=p,x=q3), col = "blue")





