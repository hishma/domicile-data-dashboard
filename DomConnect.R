library(tidyverse)
library(lubridate)
library(readxl)
library(DBI)
library(zoo)
library(xts)
library(scales)
library(forcats)
library(gridExtra)
library(plotly)

#connect to database
domconnect <- dbConnect(RPostgreSQL::PostgreSQL(),
                        dbname = "domicile-reports",
                        host = "a9c250aff69d811e8a82b02c40550189-1435924674.us-west-2.elb.amazonaws.com",
                        port = 5432,
                        user = "reporter",
                        password = "0Qo0m@%F2JCh")

dbListTables(domconnect)
#download "bookings" table" from Dbase
dombookings <- dbReadTable(domconnect, "bookings")

#disconnect to dBase
dbDisconnect(domconnect)

dombookings <- dombookings %>% write_csv("~/R_files/Domicile/DomProject/DomData/bookings.csv") %>% 
  mutate(check_in_date = ymd(check_in_date, tz = ""),
         check_out_date = ymd(check_out_date, tz = ""))
#Color Palette
DomColor <- c("#8B668B", "#6CA6CD", "#FF6347", "#A2CD5A", "#878787", "#CD853F", "#36648B", "#FFC125", "#fb9a99", "#53868B", "#8B795E")
DomColor2 <- c("#FD3B36", "#6AA6E2", "#0F425D", "#888888", "#885EA8", "#7ED321", "#FF7F00", "#FFB90F", "#698B69", "#CDBA96", "#EEEE00")
#import, shape, and connect other tables.
dombuildings <- read_csv("~/R_files/Domicile/DomProject/DomData/bldg.csv") 
domlaunch_dates <- read_csv("~/R_files/Domicile/DomProject/DomData/launch_dates.csv")
domneighborhoods <- read_csv("~/R_files/Domicile/DomProject/DomData/Data/Neighborhood.csv")
#there are two values for end_dates for some buildings, which caused join to duplicate data.  Filtered for Max End-Date. This is a temporary solution.
#will need to solve for case when there are multiple start and end dates for a building (rooms going off-line and coming_back on-line)
domlaunch_dates1 <- domlaunch_dates %>% mutate(launch_date = mdy(launch_date, tz = ""), end_date = mdy(end_date, tz = "")) %>% 
  group_by(listing_nickname) %>% summarise(launch_date = min(launch_date), end_date = max(end_date)) %>% ungroup() %>% 
  write_csv("~/R_files/Domicile/DomProject/DomData/domlaunch_dates.csv")

dombdgmaster <- dombuildings %>% left_join(domlaunch_dates1, by = "listing_nickname") %>% 
  write_csv("~/R_files/Domicile/DomProject/DomData/dombdgmaster.csv")

dommaster <- dombookings %>% left_join(dombdgmaster, by = "listing_nickname") %>% left_join(domneighborhoods, by = "Bldg_Name") %>% 
 write_csv("~/R_files/Domicile/DomProject/DomData/master.csv")
head(dommaster)
glimpse(dommaster)
nrow(dommaster)

save(dommaster, file = "~/R_files/Domicile/DomProject/Domicile_Metrics_Shiny/dommaster.RData")

ListingNicknames <- unique(dommaster$listing_nickname)
BuildingNames <- unique(dombuildings$Bldg_Name)

c(BuildingNames)

#8-29-2018 create a list of all of the dates each room is booked and divide revenue by # of nights.  The sequence of dates is reduced by one day to 
#accomodate the fact that revenue is not accrued on the checkout date, and that someone may check-in on the checkout date.
DomBookings <- dommaster %>% filter(status %in% c("confirmed", "reserved") & host_payout > 0 & (!is.na(check_in_date) | !is.na(check_out_date))) %>% 
  mutate(comp_out_date = check_out_date - days(1)) %>%  arrange(Bldg_Name, listing_nickname) %>% 
  select(confirmation_code, source, status, Bldg_Name, listing_nickname, check_in_date, comp_out_date, check_out_date, host_payout) %>%  
  write_csv("~/R_files/Domicile/DomProject/DomData/DomBookings28.csv")

head(DomBookings)
str(DomBookings)
nrow(DomBookings)

#script to create a new DF called "bookedDF".  Spreads the booking and booking revenue into individual days
bookedDF <- data_frame(conf = "abce4x-yyxtz3", listing_nickname = "dummy", booked = ymd("2018-01-31", tz = ""), rev = 0, status = "status", source = "source")

for(i in 1:nrow(DomBookings)) {
  listing_nickname <- DomBookings$listing_nickname[i]
  booked <- seq(DomBookings$check_in_date[i], DomBookings$comp_out_date[i], by = "day")
  rev <- DomBookings$host_payout[i] / as.numeric(difftime(DomBookings$check_out_date[i], DomBookings$check_in_date[i], units = "days"))
  conf <- DomBookings$confirmation_code[i]
  source <- DomBookings$source[i]
  status <- DomBookings$status[i]
  z <- cbind.data.frame(conf, listing_nickname, booked, rev, status, source)
  bookedDF <- rbind(bookedDF, z)
}
 class(bookedDF)
 class(bookedDF$booked)
bookedDF %>% write_csv("~/R_files/Domicile/DomProject/DomData/bookedDF.csv")
glimpse(bookedDF)
#adds data about the bookings back to the file:  building information, launch/end dates, and calculates the available days in 
#each month to use later (as MAX(date)).  This BookingMaster file is used as the base for the charts.
#9/6/18 udpated with new LaunchDF join.  Logic still hasn't been written to calculate the availablility.  need to test code below with this
#new data set and add it in.
BookingMaster <- bookedDF %>% mutate(year = year(booked), 
                                     month = month(booked), 
                                     true = 1, 
                                     rev_mo = rollback(ceiling_date(ymd(cut(booked, "month"), tz = ""), unit = "month", change_on_boundary = T))) %>%
  left_join(dombuildings, by = "listing_nickname") %>%
  left_join(domlaunch_dates1, by = "listing_nickname") %>%
  mutate(avail_days = case_when(
    month(launch_date) == month(booked) & year(launch_date) == year(booked) ~
      as.numeric(difftime(rollback(ceiling_date(booked, unit = "month", change_on_boundary = T)), launch_date, "days")),
    TRUE ~ as.numeric(days_in_month(booked)))) %>% 

###TESTING NEW LAUNCH DATE table and Joins

BookingMasterTEST1 <- bookedDF %>% mutate(year = year(booked), 
  month = month(booked), 
  true = 1, 
  rev_mo = ymd(cut(booked, "month"), tz = "")) %>%
  left_join(dombuildings, by = "listing_nickname") %>% left_join(domneighborhoods, by = c("Bldg_Name", "building")) %>% 
  write_csv("~/R_files/Domicile/DomProject/DomData/BookingMastertest1.csv")

# BookingMasterTEST <- bookedDF %>% mutate(year = year(booked), 
#                                           month = month(booked), 
#                                           true = 1, 
#                                           rev_mo = rollback(ceiling_date(ymd(cut(booked, "month"), tz = ""), unit = "month", change_on_boundary = T))) %>%
#   left_join(dombuildings, by = "listing_nickname") %>% left_join(domneighborhoods, by = c("Bldg_Name", "building")) %>% 
#   write_csv("~/R_files/Domicile/DomProject/DomData/BookingMastertest.csv")
# 
# glimpse(BookingMasterTEST)
  # full_join(LaunchDF, by = c("rev_mo", "listing_nickname")) %>% write_csv("~/R_files/Domicile/DomProject/DomData/BookingMaster.csv")
  
 #### changing code to incorporate the new launchDF table. If it breaks, add the file below back to the above after the mutate clause.... 
  
  # left_join(dombuildings, by = "listing_nickname") %>%
  # left_join(domlaunch_dates1, by = "listing_nickname") %>%
  # mutate(avail_days = case_when(
  #                           month(launch_date) == month(booked) & year(launch_date) == year(booked) ~
  #                             as.numeric(difftime(rollback(ceiling_date(booked, unit = "month", change_on_boundary = T)), launch_date, "days")),
  #                             TRUE ~ as.numeric(days_in_month(booked)))) %>%
  # 

### test code
### 
###Cleaning code for the new launch dates file.  Extends by revenue month in order to join to bookingsDF 
# 9/5/18 this is the NEW UPDATED CODE.  needs to be transfered upward to replace the old code.  
DLT<- read_csv("~/R_files/Domicile/DomProject/DomData/Data/LaunchDateNew.csv")
cleanlaunch <- DLT[!(DLT$launch_date =="1/15/18" & DLT$end_date =="4/30/18" & DLT$building == "marina"), ]
head(cleanlaunch)
 CL <- cleanlaunch %>% mutate(launch_date = mdy(launch_date, tz = ""), 
                              pdlaunch_date = ymd(cut(launch_date, "month"), tz = ""), 
                              end_date = mdy(end_date, tz = ""),
                              pdend_date = ymd(cut(end_date, "month"), tz = "")) %>%
 write_csv("~/R_files/Domicile/DomProject/DomData/clean.csv")
glimpse(CL)

ymd(cut(CL$launch_date, "month"), tz = "")
seq.POSIXt(ymd("2018-01-15", tz = ""), (ymd("2018-05-09", tz = "") + days(1)), by = "day")

roomsDF <- data_frame(listing_nickname = "test",
                      rev_mo = ymd("2018-01-31", tz = ""),
                      launch_date = ymd("2018-01-31", tz = ""),
                      end_date = ymd("2018-01-31", tz = ""),
                      building = "testdummy")

for(i in 1:nrow(CL)) {
  listing_nickname <- CL$listing_nickname[i]
  rev_mo <- seq.POSIXt(CL$pdlaunch_date[i], CL$pdend_date[i], by = "month")
  launch_date <- CL$launch_date[i]
  end_date <- CL$end_date[i]
  building <- CL$building[i]
  z <- cbind.data.frame(listing_nickname, rev_mo, launch_date, end_date, building)
  roomsDF <- rbind(roomsDF, z)
}

roomsDF %>% write_csv("~/R_files/Domicile/DomProject/DomData/roomsDF.csv")


NewJoinTest1 <- BookingMasterTEST1 %>% full_join(roomsDF, by = c("listing_nickname", "building", "rev_mo")) %>%  
  mutate(avail_days = case_when(!is.na(booked) & month(launch_date) == month(booked) & year(launch_date) == year(booked) ~
                                  abs(as.numeric(difftime(rollback(ceiling_date(booked, unit = "month", change_on_boundary = T)), launch_date, "days"))),
                                !is.na(booked) & month(end_date) == month(booked) & year(end_date) == year(booked) ~ 
                                  abs(as.numeric(days_in_month(rev_mo)) - 
                                        as.numeric(difftime(rollback(ceiling_date(booked, unit = "month", change_on_boundary = T)), end_date, "days"))),
                                is.na(booked) & month(launch_date) == month(rev_mo) & year(launch_date) == year(rev_mo) ~
                                  abs(as.numeric(difftime(rollback(ceiling_date(launch_date, unit = "month", change_on_boundary = T)), launch_date, "days"))),
                                is.na(booked) & month(end_date) == month(rev_mo) & year(end_date) == year(rev_mo) ~
                                  abs(as.numeric(days_in_month(rev_mo)) - as.numeric(difftime(rollback(ceiling_date(end_date, unit = "month", change_on_boundary = T)), end_date, "days"))),
                                TRUE ~ as.numeric(days_in_month(rev_mo)))) %>% write_csv("~/R_files/Domicile/DomProject/DomData/NewJoinTest1.csv")


# LaunchDF <- roomsDF %>% mutate(rev_mo = rollback(ceiling_date(rev_mos, unit = "month", change_on_boundary = T))) %>%
#   select(listing_nickname, building, rev_mo, launch_date, end_date) %>% write_csv("~/R_files/Domicile/DomProject/DomData/LaunchDFtest.csv")


#clean original code to reactivate if something goes wrong above 
# roomsDF <- data_frame(listing_nickname = "test",
#                        rev_mos = ymd("2018-01-31", tz = ""),
#                        launch_date = ymd("2018-01-31", tz = ""),
#                        end_date = ymd("2018-01-31", tz = ""),
#                        building = "testdummy")
# 
#  for(i in 1:nrow(CL)) {
#    listing_nickname <- CL$listing_nickname[i]
#    rev_mos <- seq.POSIXt(CL$launch_date[i], CL$end_date[i], by = "month")
#    launch_date <- CL$launch_date[i]
#    end_date <- CL$end_date[i]
#    building <- CL$building[i]
#    z <- cbind.data.frame(listing_nickname, rev_mos, launch_date, end_date, building)
#    roomsDF <- rbind(roomsDF, z)
#  }
# 
#  roomsDF %>% write_csv("~/R_files/Domicile/DomProject/DomData/roomsDF.csv")
#  LaunchDF <- roomsDF %>% mutate(rev_mo = rollback(ceiling_date(rev_mos, unit = "month", change_on_boundary = T))) %>%
#    select(listing_nickname, building, rev_mo, launch_date, end_date) 
 
 ## left_join(dombuildings, by = c("listing_nickname", "building")) %>% 
   
 
 
 
 ###TEST code looking for reasons the CSV file shows -1900779.  this code below is the same and produces the correct answer, 9.   9/11/2018
 # Bad <- BookingMasterTEST1 %>% full_join(roomsDF, by = c("listing_nickname", "building", "rev_mo")) %>% filter(listing_nickname == "MS309" & end_date == ymd("2018-05-09", tz = "") & month(booked) == 5)
 #  glimpse(Bad) 
 # 
 #  as.numeric(days_in_month(Bad$rev_mo[1])) -as.numeric(difftime(rollback(ceiling_date(Bad$booked[1], unit = "month", change_on_boundary = T)), Bad$end_date[1], "days"))
 # 
 # mutate(case_when(month(end_date) == month(booked) & year(end_date) == year(booked) ~ 
 #       as.numeric(days_in_month(rev_mo)) - 
 #       as.numeric(difftime(rollback(ceiling_date(booked, unit = "month", change_on_boundary = T)), end_date, "days")),
 #       TRUE ~ as.numeric(days_in_month(rev_mo)))) %>% write_csv("~/R_files/Domicile/DomProject/DomData/NewJoinTest1.csv")
 
#TEST CODE LOOKING FOR ERRORS IN THE CALCULATION OF AVAILABLE DAYS>
# domlaunch_datesTEST1 <- domlaunch_dates %>% mutate(launch_date = mdy(launch_date, tz = ""), end_date = mdy(end_date, tz = "")) %>%
# group_by(listing_nickname) %>% summarise(launch_date = min(launch_date), end_date = max(end_date)) %>% ungroup() %>%
#   write_csv("~/R_files/Domicile/DomProject/DomData/domlaunch_dates.csv")
# BookingMasterTest <- bookedDF %>% mutate(room = as.character(room),
#                                      listing_nickname = room, year = year(booked),
#                                      month = month(booked),
#                                      true = 1,
#                                      rev_mo = rollback(ceiling_date(ymd(cut(booked, "month"), tz = ""), unit = "month", change_on_boundary = T))) %>%
#   left_join(dombuildings, by = "listing_nickname") %>%
#   left_join(domlaunch_dates1, by = "listing_nickname") %>%
#   mutate(ceilingbooked = ceiling_date(booked, unit = "month", change_on_boundary = T),
#          ceildingenddate = ceiling_date(end_date, unit = "month", change_on_boundary = T),
#          ceilingrevmo = ceiling_date(rev_mo, unit = "month", change_on_boundary = T),
# 
#          revmomo = month(rev_mo),
#          bookingmo = month(booked),
#          avail_days = case_when(
#            month(launch_date) == month(booked) & year(launch_date) == year(booked) ~
#              as.numeric(difftime(rollback(ceiling_date(booked, unit = "month")), launch_date, "days")),
#            month(end_date) == month(booked) & year(end_date) == year(booked) ~ 99,
#            TRUE ~ as.numeric(days_in_month(booked)))) %>%
#   write_csv("~/R_files/Domicile/DomProject/DomData/BookingMasterTEST.csv")
# enddate <- ymd("2018-05-09", tz = "")
# revmo <- ymd("2018-05-01", tz = "") + days(2)
# monthday <- days_in_month(revmo)
# class(days_in_month(enddate))
# class(monthday)
# monthday - (as.numeric(difftime(rollback(ceiling_date(enddate, unit = "month")), enddate, "days")))
# 
# rollback(ceiling_date(ymd((cut(enddate, "month")), tz = ""), unit = "month", change_on_boundary = TRUE))
# month(enddate)
# 
# month(end_date) == month(booked) & year(end_date) == year(booked) ~
#   as.numeric(days_in_month(booked)) - (as.numeric(difftime(rollback(ceiling_date(booked, unit = "month")), end_date, "days"))),
### END TEST CODE
save(BookingMaster, file = "~/R_files/Domicile/DomProject/Domicile_Metrics_Shiny/BookingMaster.RData")
str(BookingMaster)

head(bookedDF)
str(bookedDF)
head(BookingMaster)
str(BookingMaster)

  
  
#Occupancy Rates by Month by Room CODE AND CHART created 8/29/2018
DomSummary <- BookingMaster %>% group_by(Bldg_Name, listing_nickname, year, rev_mo) %>% 
  summarise(days_booked = sum(true), 
            days_avail = max(avail_days),
            occ_rate = days_booked / days_avail, 
            Avg_Rev = sum(rev) / sum(true),
            Rev_Par = Avg_Rev * occ_rate) %>% ungroup() %>% 
  write_csv("~/R_files/Domicile/DomProject/DomData/DomSummary.csv")
save(DomSummary, file = "~/R_files/Domicile/DomProject/Domicile_Metrics_Shiny/DomSummary.RData")
nrow(DomSummary)

# roomlevelheatmap <- 
#     DomSummary %>% filter(Bldg_Name == "Cove") %>%
#   ggplot(aes(x = as.Date(rev_mo), y = listing_nickname, fill = occ_rate, label = percent(occ_rate))) +
#   geom_tile(color = "white", size = 0.25) +
#   geom_vline(data = today, xintercept = today, color = "grey", size = 6, alpha = 0.45) +
#   geom_text(size=3) + 
#   labs(x = NULL, y = NULL, title = "Monthly Occupancy Rates", subtitle = "by building")  + 
#   scale_x_date(date_labels = "%Y-%m", date_breaks = "month", position = "top", expand = c(0,0)) +
#   scale_y_discrete() +
#   scale_fill_gradient(low = "red", high = "green") +
#   theme(axis.text.x = element_text(angle = 45, vjust = 3, size = 6, face = "bold"),
#         axis.text.y = element_text(face = "bold"),
#         panel.grid.minor.y = element_blank(),
#         panel.grid.major.y = element_blank(),
#         panel.grid.major.x = element_blank(),
#         panel.grid.minor.x = element_blank(), 
#         plot.background = element_blank(),
#         panel.border = element_blank(),
#         axis.ticks = element_blank(),
#         legend.position = "top",
#         legend.title = element_blank(),
#         legend.text.align = 0)

today <- ymd(cut(today(), "month"))
class(today)
month(today, label = T, abbr = F)

BuildingHeatMap <- 
  DomSummary %>% group_by(Bldg_Name, rev_mo) %>%  
  summarize(occ = mean(occ_rate)) %>% ungroup() %>% 
  mutate(rev_mo = ymd(rev_mo, tz="")) %>%
  ggplot(aes(x = rev_mo, y = Bldg_Name, fill = occ, label = percent(occ))) +
  geom_tile(color = "white", size = 0.25, show.legend = FALSE) +
  geom_vline(data = today, xintercept = today, color = "grey", size = 6, alpha = 0.45) +
  geom_text(size=3) + 
    labs(x = NULL, y = NULL, title = "Monthly Occupancy Rates", subtitle = "by building")  + 
  scale_x_datetime(date_labels = "%b-%y", date_breaks = "month", position = "top", expand = c(0,0)) +
  scale_y_discrete() +
  scale_fill_gradient(low = "red", high = "green") +
  theme(axis.text.x = element_text(angle = 45, vjust = 3, size = 6, face = "bold"),
        axis.text.y = element_text(face = "bold"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), 
        plot.background = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank())
 


#BOXGRAPH Chart showing Occupancy since launch date by building/room.
OccupancyRateChart2 <- DomSummary %>% group_by(Bldg_Name, listing_nickname, year) %>% summarise(annual_occ = mean(occ_rate)) %>%
  ggplot(aes(x = Bldg_Name, y = annual_occ, color = Bldg_Name)) + 
  geom_boxplot(show.legend = F) +
  geom_point(show.legend = F, position = "jitter") +
  scale_color_manual(values = DomColor) +
  scale_y_continuous(labels = percent) +
  theme(axis.ticks = element_blank()) +
  labs(x = "", y = "Occupancy Rates", title = "Building Occupancy Rates", subtitle = "from launch date through current date, each unit", 
       caption = "middle line represents the median occupancy, dots represent the individual units, box represents 50% of the data") +
  coord_flip()

#INTERACTIVE RevPar Chart UPDATED 8-30-18
RevParChart <- DomSummary %>% mutate(rev_mo = ymd(rev_mo, tz = "")) %>% filter(rev_mo <= today()) %>%
  ggplot(aes(as.Date(rev_mo), Rev_Par, col = Bldg_Name)) +
  geom_point(position = "jitter", size = 3, alpha = 0.45) + 
  geom_line(aes(as.Date(rev_mo), Rev_Par), stat = "summary", fun.y = "median", color = "red", size = 2, alpha = 0.75, show.legend = F) +
  labs(x = "", y = "", title = "Revenue Per Available Room", subtitle = paste("trend through", today()), 
       color = "Building") +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "month") +
  scale_y_continuous(labels = dollar_format()) +
  scale_color_manual(values = DomColor) +
  theme(axis.text.x = element_text(angle = 45),
        axis.text.y = element_text(),
        axis.ticks.y = element_blank())

ggplotly(RevParChart, tooltip = c("x", "y", "colour"))
#TEST REVPAR ENV
# SumData <- reactive({
#   
#   TestA = 2017
#   TestB = "Cove"

#   glimpse(DomSummary)
# #   
#   DomSummary %>% mutate(rev_mo = month(ymd(rev_mo))) %>%
#     filter(year == TestA, Bldg_Name == TestB) %>% 
#     group_by(Bldg_Name, year, rev_mo) %>% 
#     summarize(Booked_Days = sum(days_booked),
#               Available_Days = sum(days_avail),
#               Unbooked_Days = Available_Days - Booked_Days,
#               Occupancy_Rate = percent(Booked_Days / Available_Days),
#               Average_Rev_Night = dollar(round(mean(Avg_Rev))),
#               Rev_PAR = dollar(round(mean(Rev_Par)))) %>%
#     gather(key = Metric, value = value, Booked_Days:Rev_PAR) %>% spread(rev_mo, value, fill = 0) %>% 
#     kable() %>% kable_styling()
 # })
# 
# c("all", BuildingNames)

#NEW CODE
#Average Daily Room Revenue Chart 8-30-2018
AnnualAvgRev <- BookingMaster %>% group_by(Bldg_Name, year) %>% 
                summarise(An_days_booked = sum(true),
                          An_Revenue = sum(rev),
                          An_Avg_Rev = sum(rev) / sum(true)) %>% ungroup()


DomCol11 <- c("#8B668B", "#6CA6CD", "#FF6347", "#A2CD5A", "#878787", "#CD853F", "#36648B", "#FFC125", "#fb9a99", "#53868B", "#8B795E")

AnnualAvgRev %>% filter(!is.na(Bldg_Name)) %>% 
  mutate(Bldg_Name = fct_reorder2(as.factor(Bldg_Name), An_Avg_Rev, year)) %>% 
  ggplot(aes(x = factor(-An_Avg_Rev), y = An_Avg_Rev, color = Bldg_Name , fill = Bldg_Name)) +
  geom_bar(stat = "identity", show.legend = T) + 
  geom_text(aes(label = paste("$", round(An_Avg_Rev)), angle = 90, hjust = -0.5))  +
  facet_grid(. ~ year, scale = "free_x", space = "free_x") +
  scale_color_manual(values = DomCol11) +
  scale_fill_manual(values = DomCol11) +
  scale_y_continuous(labels = dollar_format(), expand = c(0,0)) +
  labs(x = "", y = "Average Daily Rate", title = "Average Daily Rate by Building", 
       subtitle = "YTD 2018, includes future bookings") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid = element_blank())


###  graph for revenue per room overlayed with the occupancy rate.  


RevOcc <- DomSummary %>% mutate(rev_mo = ymd(rev_mo, tz = "")) %>%
  group_by(Bldg_Name, year, rev_mo) %>% 
  summarize(Booked_Days = sum(days_booked),
            Available_Days = sum(days_avail),
            Occupancy_Rate = Booked_Days / Available_Days,
            Average_Rev_Night = round(mean(Avg_Rev)),
            Rev_PAR = round(mean(Rev_Par))) %>% ungroup() %>% select(Bldg_Name, year, rev_mo, Occupancy_Rate, Average_Rev_Night) %>%  
            gather(key = Metric, value = value, 4:5)


#change text in the facet titles 
RevOcc$Metric = factor(RevOcc$Metric, labels = c("Average Revenue Per Night", "Occupancy Rate"))
#create a labeller for Facets
facetlab <- function(x) {
  ifelse(x >= 10, paste("$", x),
         percent(x))
}
facetlab(RevOcc$value)

#plot Average Revenue on top of occupancy rate
  RevOcc %>% 
  ggplot(aes(x = rev_mo, y = value, group = Bldg_Name, color = Metric, linetype = Metric)) +
    geom_line(aes(size = 3), show.legend = FALSE) +
    facet_wrap(~Metric, scale = "free_y", ncol = 1) +
    labs(x = NULL, y = NULL) +
    scale_x_datetime(date_labels = "%b-%y", date_breaks = "2 months",  date_minor_breaks = "year", expand = c(0,0)) +
    scale_y_continuous(labels = facetlab) +
    scale_color_manual(values = c("red", "blue")) +
    labs(title = "Revenue Per Night vs. Occupancy") +
    theme(strip.background = element_rect(fill = "#969696"),
          axis.text.x = element_text(angle = -30),
          strip.text = element_text(color = "white", face = "bold", size = 12),
          panel.grid.major.x = element_blank())
        

  DomColor <- c("#8B8B00", "#8B636C", "#EE5C42", "#36648B", "#EE9A00", "#5C5C5C", "#CD6839", "#00868B", "#FF4500", "#4F94CD", "#8B2252", "#EEEE00")

  
  DomSummary %>% mutate(rev_mo = ymd(rev_mo, tz = "")) %>%
    group_by(Bldg_Name, year, rev_mo) %>% 
    summarize(Booked_Days = sum(days_booked),
              Available_Days = sum(days_avail),
              Occupancy_Rate = Booked_Days / Available_Days,
              Average_Rev_Night = round(mean(Avg_Rev)),
              Rev_PAR = round(mean(Rev_Par))) %>% ungroup() %>% select(Bldg_Name, year, rev_mo, Occupancy_Rate, Average_Rev_Night) %>%  
    gather(key = Metric, value = value, 4:5)
  
  BookedDays <- DomSummary %>% mutate(rev_mo = ymd(rev_mo, tz = "")) %>%
    group_by(Bldg_Name, year, rev_mo) %>% 
    summarize(Booked_Days = sum(days_booked),
              Available_Days = sum(days_avail),
              Occupancy_Rate = Booked_Days / Available_Days,
              Average_Rev_Night = round(mean(Avg_Rev)),
              Rev_PAR = round(mean(Rev_Par))) %>% ungroup()
  

    BookedDays %>% filter(rev_mo == today) %>% 
      summarize(Bookings = sum(Booked_Days), Openings = sum(Available_Days - Booked_Days), Occupancy = mean(Bookings / sum(Available_Days))) %>%
      gather(key = Metric, value = Value, 1:3)
      

  #Table For Domicile Total Summary Stats
  #  
    DomSummary %>% mutate(rev_mo = ymd(rev_mo, tz = "")) %>%
        group_by(year, rev_mo) %>% 
        summarize(Booked_Days = sum(days_booked),
                Available_Nights = sum(days_avail),
                Occupancy_Rate = Booked_Days / Available_Nights,
                Average_Rev_Night = round(mean(Avg_Rev)),
                Rev_PAR = round(mean(Rev_Par))) %>% ungroup() %>% filter(rev_mo == today)   
        
  
#change BookingMaster to an XTS object


##PIVOT TABLE SHOWING SUMMARY STATS BY BUILDING
DomSummary %>% mutate(rev_mo = ymd(rev_mo)) %>%
  filter(year == 2018, Bldg_Name == "Cove") %>% 
  group_by(Bldg_Name, year, rev_mo) %>% 
  summarize(sumBooked = sum(days_booked),
            sumAvail = sum(days_avail),
            totOcc = sumBooked / sumAvail,
            avgRev = mean(Avg_Rev),
            avgRP = mean(Rev_Par)) %>%
  gather(key = key, value = value, sumBooked:avgRP) %>% spread(rev_mo, value, fill = 0)

head(DomSummary)

Summary <- DomSummary %>% filter(year == 2018) %>%
  group_by(year, rev_mo) %>% 
  summarize(Booked_Days = sum(days_booked),
            Available_Nights = sum(days_avail),
            OpenNights = Available_Nights - Booked_Days,
            Occupancy_Rate = Booked_Days / Available_Nights,
            Average_Rev_Night = round(mean(Avg_Rev)),
            Rev_PAR = round(mean(Rev_Par))) %>% ungroup() %>% filter(month(rev_mo) == month(today)) 
today
head(Summary)

as.character(Summary$rev_mo)
as.character(today)
  paste(month(today, label = T, abbr = F), "Domicile Totals")
  

  paste("Nights Booked        ", Summary$Booked_Days)
  paste("Open Nights          ", Summary$OpenNights)
  paste("Month Available      ", Summary$Available_Nights)
  paste("Occupancy Rate       ", percent(Summary$Occupancy_Rate))
  paste("Average Nightly Rate ", dollar(Summary$Average_Rev_Night))
  paste("Rev_Par              ", dollar(Summary$Rev_PAR))

#summarize the bookings to find days booked since launch for each room.
# domsum1 <- dommaster1 %>% filter(status == "confirmed" & check_in_date <= today()) %>% group_by(Bldg_Name, listing_nickname) %>% 
#   summarize(launch_date = max(launch_date),
#             end_date = max(end_date),
#             days_booked_since_launch = round(sum(num_nights)),
#             avail_days_since_launch = round(sum(max(days_since_launch))),
#             ADR = sum(host_payout)/sum(as.numeric(num_nights)),
#             occupancy_since_launch = sum(as.numeric(num_nights)) / sum(as.numeric(max(days_since_launch))),
#             RevPAR = ADR * occupancy_since_launch) %>% ungroup() 
# domsum1 
# domsum1 %>% write_csv("~/R_files/Domicile/DomProject/DomData/domsum1.csv")
# 
# DomMedOcc <- domsum1 %>% group_by(Bldg_Name) %>% summarize(med = median(occupancy_since_launch)*100) %>% ungroup()
# DomMedOcc




  
#bar graph showing average room revenue per building 
AvgRoomRevChart <-
  # dommaster1 %>% filter(status %in% c("confirmed", "future confirmed", "reserved")) %>%  group_by(Bldg_Name, yr) %>%  
  # summarize(Bldg_Avg_Rev = sum(host_payout) /sum(as.numeric(num_nights))) %>% ungroup() %>% 
  # mutate(Bldg_Name = fct_reorder2(as.factor(Bldg_Name), Bldg_Avg_Rev, yr)) %>% 
  # ggplot(aes(x = factor(-Bldg_Avg_Rev), y = Bldg_Avg_Rev, color = Bldg_Name , fill = Bldg_Name)) +
  # geom_bar(stat = "identity", show.legend = T) + 
  # geom_text(aes(label = paste("$", round(Bldg_Avg_Rev)), angle = 90, hjust = -0.5))  +
  # facet_grid(. ~ yr, scale = "free_x", space = "free_x") +
  # scale_color_manual(values = CPCOLS) +
  # scale_fill_manual(values = CPCOLS) +
  # scale_y_continuous(labels = dollar_format(), expand = c(0,0)) +
  # labs(x = "", y = "Average Daily Rate", title = "Average Daily Rate by Building", 
  #      subtitle = "YTD 2018, includes future bookings") +
  # theme(axis.text.x = element_blank(),
  #       axis.ticks.x = element_blank(),
  #       panel.grid = element_blank())
  #       
  #      
  


##OLD CODE BELOW THIS LINE#####
##
#prepare data file for analysis
#calculations for booking durations, days booked since inception, etc.  
#add days booked since inception and % booked since inception

# dommaster1 <- dommaster %>% mutate(num_nights = difftime(check_out_date, check_in_date, units = "days"), 
#                                    days_in_advance = difftime(check_in_date, created_at, units = "days"),
#                                    rev_per_night = host_payout / as.numeric(num_nights),
#                                    days_since_launch = case_when(
#                                      today() < end_date ~ difftime(today(), launch_date, units = "days"),
#                                      today() > end_date ~ difftime(end_date, launch_date, units = "days"),
#                                      TRUE ~ 0),
#                                    days_til_end = case_when(
#                                      today() < end_date ~ difftime(end_date, today(), units = "days"),
#                                      TRUE ~ 0),
#                                    occ_int = interval(check_in_date, check_out_date, tz = ""), 
#                                    rev_month = month(check_in_date),
#                                    yr = year(check_in_date),
#                                    avail_days = case_when(
#                                      month(launch_date) == month(check_in_date) & year(launch_date) == year(check_in_date) ~ as.integer(difftime(rollback(ceiling_date(check_in_date, unit = "month")), launch_date, "days")),
#                                      TRUE ~ days_in_month(check_in_date)),
#                                    day_check = as.integer(difftime(rollback(ceiling_date(check_in_date, unit = "month")), launch_date, "days")),
#                                    YR_MO = cut(check_in_date, "month"),
#                                    status = case_when(check_in_date > today() ~ "future confirmed",
#                                                       TRUE ~ status)) %>% 
#   select(confirmation_code, status, source, Bldg_Name, listing_nickname, occ_int, yr, rev_month, YR_MO, check_in_date, check_out_date, 
#          num_nights, host_payout, rev_per_night, created_at, days_in_advance, days_since_launch, days_til_end, launch_date, 
#          end_date, avail_days)
# 
# glimpse(dommaster1)
# dommaster1 %>% write_csv("~/R_files/Domicile/DomProject/DomData/dom1.csv")
# dommaster1 %>% filter(is.na(Bldg_Name))
# 
# class(dommaster1$rev_month)
# #RevPAR
# RevPar <- dommaster1 %>% filter(status %in% c("confirmed", "reserved", "future reserved")) %>% group_by(Bldg_Name, listing_nickname, YR_MO) %>% 
#   summarize(launch_date = max(launch_date),
#             end_date = max(end_date),
#             days_booked = round(sum(num_nights)),
#             avail_days_in_month = round(sum(max(avail_days))),
#             ADR = sum(host_payout)/sum(as.numeric(num_nights)),
#             occupancy = sum(as.numeric(num_nights)) /sum(avail_days_in_month),
#             RevPAR = mean(ADR * occupancy)) %>% ungroup()
# 
# RevPar %>% group_by(Bldg_Name) %>% summarise(Total_Days_Booked = sum(days_booked), Avail_Days = sum(avail_days_in_month), 
#                                              Occ_Rate = percent(as.numeric(Total_Days_Booked) / Avail_Days)) %>% ungroup()
# 
# 
# RevParChart <- RevPar %>% filter(RevPAR > 0 & occupancy <= 1 & occupancy >= 0) %>% 
#   ggplot(aes(as.Date(YR_MO), RevPAR, col = Bldg_Name)) +
#   geom_point(position = "jitter", size = 3, alpha = 0.45) + 
#   geom_line(aes(as.Date(YR_MO), RevPAR), stat = "summary", fun.y = "median", color = "red", size = 2, alpha = 0.75, show.legend = F) +
#   labs(x = "", y = "", title = "Revenue Per Available Room", subtitle = "trend over time", 
#        caption = "high numbers were filtered out:  bookings spread over multiple months",
#        color = "Building") +
#   scale_x_date(date_labels = "%Y-%m", date_breaks = "month") +
#   scale_color_manual(values = DomColor) +
#   theme(axis.text.x = element_text(angle = 45),
#         axis.text.y = element_text(),
#         axis.ticks.y = element_blank())
# 
# ###boxplot showing occupancy rates by building up until today.  
# OccupancyRateChart <- domsum1 %>% filter(!is.na(Bldg_Name)) %>%  
#   ggplot(aes(x = Bldg_Name, y = occupancy_since_launch, color = Bldg_Name)) + 
#   geom_boxplot(show.legend = F) +
#   geom_point(show.legend = F, position = "jitter") +
#   scale_color_manual(values = DomColor) +
#   scale_y_continuous(labels = percent) +
#   theme(axis.ticks = element_blank()) +
#   labs(x = "", y = "Occupancy Rates", title = "Building Occupancy Rates", subtitle = "from launch date through current date, each unit", 
#        caption = "middle line represents the median occupancy, dots represent the individual units, box represents 50% of the data") +
#   coord_flip()
# #splitting into lists by listing_nickname
# spl1 <- dommaster1 %>% filter(status %in% c("confirmed", "reserved", "future confirmed")) %>% select(confirmation_code, listing_nickname, occ_int, check_in_date, check_out_date)
# 
# spl2 <- split(spl1, spl1$listing_nickname, drop = FALSE)
# 
# str(spl2)
# unique(c(names(spl2), names(Bkg1spl)))
# names(Bkg1spl)
# 
# dommaster1 %>% group_by(Bldg_Name, listing_nickname) %>% summarize(Nights_Booked = round(sum(num_nights)), Avg_Nightly_Revenue = mean(rev_per_night))
# 
# 
# #evaluates to T/F
# as.POSIXct("2018-06-25", tz = "", origin = "1970-01-01") %in% seq(DomBookings[1,4], DomBookings[1,5], by = "day") 
# 
# 
# #create a calendar matrix by room, then convert to a dataframe.
# BldgVec <- c(unique(dommaster1$listing_nickname))
# dateref <- seq(ymd_hms("2015-12-31 00:00:00", tz = ""), ymd_hms("2020-06-30 23:59:59", tz = ""), "day")
# BldgList <- as.list(unique(dommaster1$listing_nickname))
# datelist <- as.list(format(dateref))
# 
# Matrix <- matrix(nrow = length(datelist), ncol = length(BldgVec))
# rownames(Matrix) <- c(format(dateref))
# colnames(Matrix) <- c(BldgVec)
# head(Matrix)
# 
# DFMAT <- as.data.frame(Matrix)
# str(DFMAT)
# 
# tibmat <- tibble::rownames_to_column(DFMAT)
# str(tibmat)
# head(tibmat)
# tibmat[1,2]
# names(tibmat)
# 
# BookingList <- tibmat %>% gather(-1, key = "listing_nickname", value = "booked") %>% 
#   mutate(date = as.POSIXct(rowname, tz = "", origin = "1970-01-01")) %>% select(date, listing_nickname)
# head(BookingList)
# class(BookingList$date)
# 
# 
# #create a calendar matrix by room, then convert to a dataframe.
# BldgVec <- c(unique(dommaster$listing_nickname))
# dateref <- seq(ymd_hms("2015-12-31 00:00:00", tz = ""), ymd_hms("2020-06-30 23:59:59", tz = ""), "day")
# BldgList <- as.list(unique(dommaster$listing_nickname))
# datelist <- as.list(format(dateref))
# 
# Matrix <- matrix(nrow = length(datelist), ncol = length(BldgVec))
# rownames(Matrix) <- c(format(dateref))
# colnames(Matrix) <- c(BldgVec)
# head(Matrix)
# 
# DFMAT <- as.data.frame(Matrix)
# str(DFMAT)
# 
# tibmat <- tibble::rownames_to_column(DFMAT)
# str(tibmat)
# head(tibmat)
# tibmat[1,2]
# names(tibmat)
# 
# RoomCalendar <- tibmat %>% gather(-1, key = "listing_nickname", value = "booked") %>% 
#   mutate(MasterDate = as.POSIXct(rowname, tz = "", origin = "1970-01-01")) %>% select(MasterDate, listing_nickname)
# head(RoomCalendar)
# str(RoomCalendar)
# 
# 
# x <- seq(DomBookings$check_in_date[8], DomBookings$check_out_date[8], by = "day")
# 
# str(newbookings)
# 
# #create XTS
# XTSref <- xts(1:4000,  ymd_hms("2015-12-31 00:00:00", tz = "")+1:4000)
# 
# dateref <- seq(ymd_hms("2015-12-31 00:00:00", tz = ""), ymd_hms("2019-12-31 23:59:59", tz = ""), "day")
# datelist <- as.list(format(dateref))
# domXTS <- as.xts(datesdom, XTSref, order.by = dommaster$check_in_date)  
# 
# 
# 
