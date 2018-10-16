library(tidyverse)
library(lubridate)
library(readxl)
library(DBI)
library(zoo)
library(xts)
library(scales)
library(forcats)
library(gridExtra)
library(bizdays)
library(TimeWarp)

create.calendar("Domicile",  financial = FALSE)



table.1 <- data.frame(x = NA, y = NA, t = NA)

for( i in 1:100) {

  x <- rnorm(100)
  y <- x/2 +x/3
  t <- "CS03"
  z <- cbind.data.frame(x = x, y = y, t = t)
  
  table.1 <- rbind(table.1, z)
  
  }


####example 2: faster processing #####

table.2 <- list()
t0 <- 0
for( i in 1:100) {
  
  x <- rnorm(100)
  y <- x/2 + x/3
  t <- "CS03"
  
  z <- cbind.data.frame(x = x, y = y)
  
  table.2[[i]] <- z
  
  e0 <- Sys.time()
  t0  <- (e0 - s0) + t0
  
}


