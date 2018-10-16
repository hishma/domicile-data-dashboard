library(shiny)
library(tidyverse)
library(lubridate)
library(DBI)
library(scales)
library(forcats)
library(dplyr)
library(kableExtra)
library(plotly)

load("/Users/bari/R_files/Domicile/DomProject/Domicile_Metrics_Shiny/dommaster.RData")
load("/Users/bari/R_files/Domicile/DomProject/Domicile_Metrics_Shiny/BookingMaster.RData")
load("/Users/bari/R_files/Domicile/DomProject/Domicile_Metrics_Shiny/DomSummary.RData")

#Color Palette
DomColor <- c("#8B8B00", "#8B636C", "#EE5C42", "#36648B", "#EE9A00", "#5C5C5C", "#CD6839", "#00868B", "#FF4500", "#4F94CD", "#8B2252", "#EEEE00")
DomCol11 <- c("#8B668B", "#6CA6CD", "#FF6347", "#A2CD5A", "#878787", "#CD853F", "#36648B", "#FFC125", "#fb9a99", "#53868B", "#8B795E")

head(dommaster)
glimpse(dommaster)

domdetail <- dommaster %>% mutate(duration = difftime(check_out_date, check_in_date, "days"), 
                                  date = ymd(cut(check_in_date, "month"), tz = ""),
                                  booked = ifelse(status == "confirmed", 1, 0)) 

#Number of bookings by duration.
summary <- domdetail %>% filter(year(date) == 2017, status == "confirmed") %>%
  group_by(neighborhood, Bldg_Name, source, status, date, year(check_in_date), month(check_in_date)) %>% 
  summarize(`<7` = sum(duration <= 7), 
            `7-15` =  sum(duration > 7 & duration < 15), 
            `15-30` = sum(duration >=15 & duration < 30),
            `30+` = sum(duration >= 30),
            revenue = sum(host_payout)) %>% 
  gather(key = Duration, value = Bookings, 8:10 ) %>% 
  ggplot(aes(Duration, Bookings, fill = Bldg_Name)) + 
  facet_grid(.~Bldg_Name) + 
  geom_bar(stat = "identity")+ 
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_manual(values = DomCol11) +
  theme_light() + 
  theme(axis.text.x = element_text(angle = -30),
        panel.grid.major.x = element_blank())

#scatterplot of bookings by building, over time.  
domdetail %>% filter(status == "confirmed") %>% 
  ggplot(aes(x = check_in_date)) +
    geom_point(aes(y = booked, color = Bldg_Name), position = "jitter")  
    geom_line(aes(y = sum(host_payout)))
     

#barchart showing bookings
domdetail %>% filter(status == "confirmed") %>%
  ggplot(aes(x = date, fill = status)) +
  geom_bar(position = "dodge")

TotalRev <- domdetail %>% filter(status == "confirmed") %>% group_by(date) %>% summarise(revenue = sum(host_payout)) %>% ungroup() 

glimpse(domdetail)

DomPlot <- domdetail %>% group_by(status, neighborhood, date) %>% 
  summarize(booking = sum(booked),
            revenue = sum(host_payout)) %>% ungroup() %>% filter(status == "confirmed") %>%
  ggplot(aes(x = date) ) + 
  geom_bar(aes(y = revenue, col = neighborhood, fill = neighborhood), stat = "identity") +
  geom_text(data = TotalRev, aes(x = date, y = revenue, label = paste(dollar(round(revenue / 1000)),"K")), 
            stat = "identity", angle = 90, hjust = -0.2) +
  scale_y_continuous(labels = dollar_format(), expand = c(0, 0), limits = c(0, 500000)) +
  scale_color_manual(values = DomColor) +
  scale_fill_manual(values = DomColor) + 
  ggtitle("Total Revenue per Month") + 
  labs(x = NULL, y = NULL) +
  theme_light() + 
  theme(axis.text = element_text(face = "bold", size = 10),
        plot.background = element_rect(colour = "white", fill = "white"),
        panel.grid = element_blank())
        

ggplotly(DomPlot)
  
domdetail %>% group_by(status, neighborhood, date) %>% 
  summarize(booking = sum(booked),
            revenue = sum(host_payout)) %>% ungroup() %>% filter(status == "confirmed") %>%
write_csv("~/desktop/file.csv")   

##Available units each month (may be wrong since it is counting the units in the bookings talble.  
##There  isn't a master table of units/availability dates)
UnitsAvail <- domdetail %>% group_by(date, Bldg_Name) %>% summarize(units = length(unique(listing_nickname))) %>% 
  summarise(units = sum(units)) %>% ungroup() %>%
  ggplot(aes(date, units)) + 
  geom_bar(stat = "identity")

## 
