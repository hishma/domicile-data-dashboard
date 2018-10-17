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
library(quantreg)
library(stringr)
library(leaflet)
library(broom)

#connect to database
domconnect <- dbConnect(RPostgreSQL::PostgreSQL(),
                        dbname = "domicile-reports",
                        host = "reporting-db.bookdomicile.com",
                        port = 5432,
                        user = "reporter",
                        password = "0Qo0m@%F2JCh")

dbListTables(domconnect)

#download "bookings" table" from Dbase
dombookings <- dbReadTable(domconnect, "bookings")

#disconnect to dBase
dbDisconnect(domconnect)


dombookings <- dombookings %>% 
  mutate(check_in_date = ymd(check_in_date, tz = ""),
         check_out_date = ymd(check_out_date, tz = ""),
         days_in_advance = round(as.numeric(difftime(check_in_date, created_at, tz = "", units = "days"))))

today <- ymd(cut(today(), "month")) 
monthstart <- ymd(paste(year(now()), "-", month(now()), "-01", sep = ""))
ninetyseq <- seq.Date(monthstart, by = "day", length.out = 90)

#Color Palette
DomColor <- c("#8B668B", "#6CA6CD", "#FF6347", "#A2CD5A", "#878787", "#CD853F", "#36648B", "#FFC125", "#fb9a99", "#53868B", "#8B795E")
DomCol11 <- c("#885EA8", "#6AA6E2", "#FF6347", "#878787", "#CD853F", "#36648B", "#FFC125", "#FB9A99", "#589CA1FD", "#B89C76FE", "#A2CD5A")
DomCol12 <- c("#8B668B", "#6CA6CD", "#FF6347", "#A2CD5A", "#878787", "#CD853F", "#36648B", "#FFC125", "#fb9a99", "#53868B", "#8B795E")
DomColSource <- c("#8B668B", "#6CA6CD", "#FF6347", "#A2CD5A", "#878787", "#CD853F", "#36648B")
#import, shape, and connect other tables.
dombuildings <- read_csv("~/R_files/Domicile/DomProject/DomicileDashboardShiny/Data/bldg.csv") 
domneighborhoods <- read_csv("~/R_files/Domicile/DomProject/DomicileDashboardShiny/Data/Data/Neighborhood.csv")
BldgMaster <- dombuildings %>% left_join(domneighborhoods, by = c("Bldg_Name", "building"))
save(domneighborhoods, file = "~/R_files/Domicile/DomProject/DomicileDashboardShiny/Data/domneighborhoods.RData")
save(BldgMaster, file = "~/R_files/Domicile/DomProject/DomicileDashboardShiny/Data/BldgMaster.RData")
ListingNicknames <- unique(BldgMaster$listing_nickname)
BuildingNames <- unique(BldgMaster$Bldg_Name)

#Base DF.  Raw booking data filtered for reserved/confirmed rooms.  Calcs' for number of days and nights.  
#Adds a (Comp day, which aids calcs in later code to lengthen
#bookings over the individual days booked.  Cleans the Source column.

dommaster <- dombookings %>% left_join(BldgMaster, by = "listing_nickname") 

save(dommaster, file = "~/R_files/Domicile/DomProject/DomicileDashboardShiny/Data/dommaster.RData")

DomBookings <- dommaster %>% filter(status %in% c("confirmed", "reserved") & host_payout > 0 & (!is.na(check_in_date) | !is.na(check_out_date))) %>% 
  filter(!(status == "reserved" & check_in_date < ymd(today(), tz = "") )) %>% 
  mutate(comp_out_date = check_out_date - days(1),
          num_nights = as.numeric(difftime(check_out_date, check_in_date, "days")),
         ADR = host_payout / num_nights,
         source = str_to_lower(source),
         source = case_when(
            str_detect(source, "booking.com") ~ "booking.com",
            str_detect(source, "direct") ~ "direct",
            str_detect(source, "manual") ~ "manual",
            str_detect(source, "homeaway") ~ "HomeAway",
            str_detect(source, "expedia") ~ "Expedia",
            str_detect(source, "airbnb") ~ "Airbnb",
            TRUE ~ "other")
          )  %>%  arrange(Bldg_Name, listing_nickname) %>% 
   select(confirmation_code, source, status, Bldg_Name, listing_nickname, check_in_date, 
          comp_out_date, check_out_date, ADR, num_nights, host_payout, created_at, days_in_advance) 
 
save(DomBookings, file = "~/R_files/Domicile/DomProject/DomicileDashboardShiny/Data/DomBookings.RData")

#Base table used in graphs for dashboard.  
DomVel <- DomBookings %>% left_join(domneighborhoods, by = "Bldg_Name") %>% 
   mutate(rev_mo = ymd(cut(check_in_date, breaks = "month"), tz = ""),
          weekday = factor(weekdays(check_in_date, abbreviate = T), 
                           levels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")))

#script to create a new DF called "bookedDF".  Spreads the booking and booking revenue into individual days
bookedDF <- data_frame(conf = "abce4x-yyxtz3", listing_nickname = "dummy", 
                       booked = as.Date(ymd("2018-01-31", tz = "")), rev = 0, 
                       status = "status", source = "source")

for(i in 1:nrow(DomBookings)) {
       listing_nickname <- DomBookings$listing_nickname[i]
       booked <- as.Date(seq(DomBookings$check_in_date[i], DomBookings$comp_out_date[i], by = "day"))
       rev <- DomBookings$ADR[i]
       conf <- DomBookings$confirmation_code[i]
       source <- DomBookings$source[i]
       status <- DomBookings$status[i]
       z <- cbind.data.frame(conf, listing_nickname, booked, rev, status, source)
       bookedDF <- rbind(bookedDF, z)
     }

 
#DomBookings$host_payout[i] / as.numeric(difftime(DomBookings$check_out_date[i], DomBookings$check_in_date[i], units = "days"))
BookedDF <- bookedDF %>% mutate(year = year(booked), 
                                month = month(booked), 
                                BT = 1, 
                                rev_mo = as.Date(ymd(cut(booked, "month"), tz = ""))) %>%
  left_join(BldgMaster, by = "listing_nickname")
  
BookedDF %>% write_csv("~/R_files/Domicile/DomProject/DomData/BookedDF.csv")  

#Read in the Launch Dates and clean to remove duplicates.  Create a new "long" data frame by sequencing by day between launch and end dates and adding a "Rev_Mo" column.  Work around the 
#Lubridate issue of month increments not putting the last rev_mo if there wasn't a full month before the end date by changing both to first of month.  
# DLT <- read_csv("~/R_files/Domicile/DomProject/DomData/Data/LaunchDateNew.csv")
#   
# cleanlaunch <- DLT[!(DLT$launch_date =="1/15/18" & DLT$end_date =="4/30/18" & DLT$building == "marina"), ]




#CL <- cleanlaunch %>% mutate(launch_date = mdy(launch_date, tz = ""), 
                             # pdlaunch_date = ymd(cut(launch_date, "month"), tz = ""), 
                             # end_date = mdy(end_date, tz = ""),
                             # pdend_date = ymd(cut(end_date, "month"), tz = "")) 

##The new file "RoomDetail.csv" should be used as the database file where updates to room launch and end dates, 
##and addition of new rooms are made.  This data originally came from Simon's spreadsheet and was put through a 
##cleaning script to remove duplicates, but this has proved unstable, since the second time, minor changes were made in the spreadsheet 
##which caused downline errors in the code.  The information in this spreadsheet should be transfered to tables in PostgreSQL
CL <- read_csv("~/R_files/Domicile/DomProject/DomicileDashboardShiny/Data/RoomDetail.csv")


# roomsDF <- data_frame(listing_nickname = "test",
#                       rev_mo = as.Date(ymd("2018-01-31", tz = "")),
#                       launch_date = ymd("2018-01-31", tz = ""),
#                       end_date = ymd("2018-01-31", tz = ""),
#                       building = "testdummy")
# 
# for(i in 1:nrow(CL)) {
#     listing_nickname <- CL$listing_nickname[i]
#     rev_mo <- as.Date(seq.POSIXt(CL$pdlaunch_date[i], CL$pdend_date[i], by = "month"))
#     launch_date <- CL$launch_date[i]
#     end_date <- CL$end_date[i]
#     building <- CL$building[i]
#     z <- cbind.data.frame(listing_nickname, rev_mo, launch_date, end_date, building)
#     roomsDF <- rbind(roomsDF, z)
#   }

#### New CODE TO construction a by-day version of the booking master file for analysis by day and week. Foundation for code below.
roomsDFdays <- data_frame(listing_nickname = "test",
                          booked = as.Date(ymd("2018-01-31", tz = "")),
                          launch_date = ymd("2018-01-31", tz = ""),
                          end_date = ymd("2018-01-31", tz = ""),
                          building = "testdummy",
                          AT = 1)

for(i in 1:nrow(CL)) {
  listing_nickname <- CL$listing_nickname[i]
  booked <- as.Date(seq.POSIXt(CL$launch_date[i], CL$end_date[i], by = "day"))
  launch_date <- CL$launch_date[i]
  end_date <- CL$end_date[i]
  building <- CL$building[i]
  AT = 1
  z <- cbind.data.frame(listing_nickname, booked, launch_date, end_date, building, AT)
  roomsDFdays <- rbind(roomsDFdays, z)
}

RoomsDFdays <- roomsDFdays %>% mutate(rev_mo = as.Date(ymd(cut(booked, breaks = "months"), tz = "")))
RoomsDFdays %>% write_csv("~/R_files/Domicile/DomProject/DomData/RoomsDFdays.csv")

#New base dataframe to use for analysis.  Data is displayed on a daily basis rather than monthly.  Other dependencies exist for this data.
DaysDF <- RoomsDFdays %>% full_join(BookedDF, by = c("building", "listing_nickname", "rev_mo", "booked")) %>% 
  mutate(booked = ymd(booked, tz = ""),
         rev_mo = ymd(rev_mo, tz = ""),
         BT = case_when(is.na(BT) ~ 0,
                        TRUE ~ BT),
         AT = case_when(is.na(AT) ~ 0,
                        TRUE ~ AT),
         week = epiweek(booked), weekname = cut.POSIXt(booked, "week", start.on.monday = F), 
         weekdate = ymd_hms(as.character(weekname), tz = ""))

DaysDF %>% write_csv("~/R_files/Domicile/DomProject/DomData/DaysDF.csv")
save(DaysDF, file = "~/R_files/Domicile/DomProject/DomicileDashboardShiny/Data/DaysDF.RData")
ymd_hms(as.character(DaysDF$weekname), tz = "")

DaysDownload <- DaysDF %>% select(neighborhood, cohort, Bldg_Name, 
                                  listing_nickname, conf, status, rev_mo, weekname, 
                                  booked, AT, BT, rev, launch_date, end_date) 

#WeekOcc
#calculates the number of avail days and booked days by unit, then adds back the information about cohort.
#becuase of the way the data is strutured, if you add cohort to initial grouping, then the avail days always 
# = booked days, which is incorrect.   This method calculates correctly.
WeekOcc <- DaysDF %>% group_by(listing_nickname, year = year(weekdate), week, weekdate) %>% 
  summarize(avail_days = sum(AT), 
            booked_days = sum(BT), 
            occ_rate = case_when(avail_days > 0 ~ booked_days / avail_days,
                                 TRUE ~ 0),
             Rev = sum(rev, na.rm = T),
             ADR = case_when(booked_days > 0 ~ sum(rev, na.rm = T) / booked_days,
                             TRUE ~ 0),
             RevPar = occ_rate * ADR) %>% ungroup() %>% 
  left_join(BldgMaster, by = "listing_nickname")
  
BldgLL <- BldgMaster %>% group_by(Bldg_Name) %>% 
  summarize(Latitude = unique(Latitude), Longitude = unique(Longitude))

WeekOcc %>% write_csv("~/R_files/Domicile/DomProject/DomData/WeekOcc.csv")

###Data for Leaflet Map in App
MonthDet <- DaysDF %>% group_by(listing_nickname, rev_mo) %>% 
  summarize(avail_days = sum(AT), 
            booked_days = sum(BT), 
            occ_rate = case_when(avail_days > 0 ~ booked_days / avail_days,
                                 TRUE ~ 0),
            Rev = sum(rev, na.rm = T),
            ADR = case_when(booked_days > 0 ~ sum(rev, na.rm = T) / booked_days,
                            TRUE ~ 0),
            RevPar = occ_rate * ADR) %>% ungroup() %>% 
  left_join(BldgMaster, by = "listing_nickname") %>% 
  group_by(cohort, Bldg_Name, rev_mo) %>% summarize(Rooms = length(unique(listing_nickname)), 
                                                    avail_days = sum(avail_days),
                                                    booked_days = sum(booked_days),
                                                    Rev = sum(Rev),
                                                        ADR = Rev / booked_days,
                                                    Occ = booked_days / avail_days,
                                                    RevPar = ADR * Occ) %>% ungroup() %>%
  left_join(BldgLL, by = "Bldg_Name")

save(MonthDet, file = "~/R_files/Domicile/DomProject/DomicileDashboardShiny/Data/MonthDet.RData")

##Data for graphs attached to LeafLet Map in App
MonthAll <- DaysDF %>% group_by(listing_nickname, rev_mo) %>% 
  summarize(avail_days = sum(AT), 
            booked_days = sum(BT), 
            occ_rate = case_when(avail_days > 0 ~ booked_days / avail_days,
                                 TRUE ~ 0),
            Rev = sum(rev, na.rm = T),
            ADR = case_when(booked_days > 0 ~ sum(rev, na.rm = T) / booked_days,
                            TRUE ~ 0),
            RevPar = occ_rate * ADR) %>% ungroup() %>% 
  left_join(BldgMaster, by = "listing_nickname") %>% 
  group_by(rev_mo) %>% summarize(Rooms = length(unique(listing_nickname)), 
                                 avail_days = sum(avail_days),
                                 booked_days = sum(booked_days),
                                 Rev = sum(Rev),
                                 ADR = Rev / booked_days,
                                 Occ = booked_days / avail_days,
                                 RevPar = ADR * Occ) %>% ungroup() 

MonthAll %>% write_csv("~/R_files/Domicile/DomProject/DomData/MonthAll.csv")
save(MonthAll, file = "~/R_files/Domicile/DomProject/DomicileDashboardShiny/Data/MonthAll.RData")

##RPOcc doesn't have the correct numbers for avail days due to source grouping.
RPOcc <- DaysDF %>% group_by(listing_nickname, source, year = year(weekdate), week, weekdate) %>% 
  summarize(avail_days = sum(AT), 
            booked_days = sum(BT), 
            occ_rate = case_when(avail_days > 0 ~ booked_days / avail_days,
                                TRUE ~ 0),
            Rev = sum(rev, na.rm = T),
            ADR = case_when(booked_days > 0 ~ sum(rev, na.rm = T) / booked_days,
                            TRUE ~ 0),
            RevPar = occ_rate * ADR) %>% ungroup() %>% 
  left_join(BldgMaster, by = "listing_nickname") 

#dataframe used to calculate the weekly rolled up summary by cohort. 
WeekSum <- WeekOcc %>% group_by(cohort, year = year(weekdate), weekdate) %>% 
  summarise(avail = sum(avail_days), 
            booked = sum(booked_days), 
            occ = booked / avail,
            TotRev = sum(Rev, na.rm = T),
            ADR = case_when(booked > 0 ~ sum(Rev, na.rm = T) / booked,
                            TRUE ~ 0), 
            RevPar = ADR * occ,
            occhar = percent(occ),
            adrchar = dollar(round(ADR)),
            rparchar = dollar(round(RevPar))) %>% ungroup() 

WS <- WeekSum %>% filter(year(weekdate) == 2018, month(weekdate) >= month(now())) 

WT <- WS %>% group_by(year, weekdate) %>% 
  summarise(avail = sum(avail), 
            booked = sum(booked), 
            occ = booked / avail, 
            ADR = mean(ADR), 
            occhar = percent(occ),
            adrchar = dollar(ADR)) %>% mutate(cohort = "Total") %>% ungroup() %>% 
  select(cohort, year, weekdate, avail, booked, occ, ADR, occhar, adrchar)

WST <- bind_rows(WS, WT)

save(WeekOcc, file = "~/R_files/Domicile/DomProject/DomicileDashboardShiny/Data/WeekOcc.RData")
save(RPOcc, file = "~/R_files/Domicile/DomProject/DomicileDashboardShiny/Data/RPOcc.RData")
save(WeekSum, file = "~/R_files/Domicile/DomProject/DomicileDashboardShiny/Data/WeekSum.RData")


#At a Month level.  Join the Launch Dates DF to the BookingMaster DF and calculate the days available within the month. Adds weekday as a factor.
BookingMaster <- BookedDF %>% full_join(roomsDF, by = c("listing_nickname", "building", "rev_mo")) %>%  
 mutate(weekday = factor(weekdays(booked, abbreviate = T), levels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")) ,
        avail_days = case_when(!is.na(booked) & month(launch_date) == month(booked) & year(launch_date) == year(booked) ~
                                                 abs(as.numeric(difftime(rollback(ceiling_date(booked, unit = "month", change_on_boundary = T)), launch_date, "days"))) + 1,
                                               !is.na(booked) & month(end_date) == month(booked) & year(end_date) == year(booked) ~ 
                                                 abs(as.numeric(days_in_month(rev_mo)) - 
                                                 as.numeric(difftime(rollback(ceiling_date(booked, unit = "month", change_on_boundary = T)), end_date, "days"))) + 1,
                                               is.na(booked) & month(launch_date) == month(rev_mo) & year(launch_date) == year(rev_mo) ~
                                                abs(as.numeric(difftime(rollback(ceiling_date(launch_date, unit = "month", change_on_boundary = T)), launch_date, "days"))) + 1,
                                              is.na(booked) & month(end_date) == month(rev_mo) & year(end_date) == year(rev_mo) ~
                                                abs(as.numeric(days_in_month(rev_mo)) - as.numeric(difftime(rollback(ceiling_date(end_date, unit = "month", change_on_boundary = T)), end_date, "days"))) + 1,
                                              TRUE ~ as.numeric(days_in_month(rev_mo))))

save(BookingMaster, file = "~/R_files/Domicile/DomProject/Domicile_Metrics_Shiny/BookingMaster.RData")
save(BookingMaster, file = "~/R_files/Domicile/DomProject/DomicileDashboardShiny/Data/BookingMaster.RData")

#Occupancy Rates by Month by Room CODE AND CHART created 8/29/2018
DomSummary <- BookingMaster %>% group_by(Bldg_Name, listing_nickname, year, rev_mo) %>% 
  summarise(days_booked = sum(BT), 
            days_avail = max(avail_days),
            occ_rate = days_booked / days_avail, 
            Avg_Rev = sum(rev) / sum(BT),
            Rev_Par = Avg_Rev * occ_rate) %>% ungroup() 

save(DomSummary, file = "~/R_files/Domicile/DomProject/Domicile_Metrics_Shiny/DomSummary.RData")
save(DomSummary, file = "~/R_files/Domicile/DomProject/DomicileDashboardShiny/Data/DomSummary.RData")


RevOcc <- DomSummary %>% mutate(rev_mo = ymd(rev_mo, tz = "")) %>% 
  group_by(Bldg_Name, year, rev_mo) %>% 
  summarize(Booked_Days = sum(days_booked),
            Available_Days = sum(days_avail),
            Occupancy_Rate = Booked_Days / Available_Days,
           Average_Rev_Night = round(mean(Avg_Rev)),
            Rev_PAR = round(mean(Rev_Par))) %>% ungroup() %>% 
  select(Bldg_Name, year, rev_mo, Occupancy_Rate, Average_Rev_Night) %>%  
  gather(key = Metric, value = value, 4:5)

YTDSummary <- DaysDF  %>%  filter(year(now()) == year(booked), 
                                  booked < floor_date(now(), "month")) %>% 
  summarise(YTDRev = sum(rev, na.rm = T), 
            YTDOcc = sum(BT, na.rm = T) / sum(AT, na.rm = T), 
            YTDADR = sum(rev, na.rm = T)/ sum(BT, na.rm = T), 
            YTDRevPar = YTDADR * YTDOcc)

CurMoSummary <- DaysDF  %>%  filter(year(now()) == year(booked), 
                                    rev_mo == floor_date(now(), "month")) %>% 
  summarise(MoRev = sum(rev, na.rm = T), 
            MoOcc = sum(BT, na.rm = T) / sum(AT, na.rm = T), 
            MoADR = sum(rev, na.rm = T)/ sum(BT, na.rm = T), 
            MoRevPar = MoADR * MoOcc)

