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
library(leaflet.extras)
library(broom)

##these are data files to load for the plots below.  Created in the New_Startup_Script.R file.
load("/Users/bari/R_files/Domicile/DomProject/DomicileDashboardShiny/dommaster.RData")
load("/Users/bari/R_files/Domicile/DomProject/DomicileDashboardShiny/BookingMaster.RData")
load("/Users/bari/R_files/Domicile/DomProject/DomicileDashboardShiny/DomSummary.RData")
load("/Users/bari/R_files/Domicile/DomProject/DomicileDashboardShiny/WeekOcc.RData")
load("/Users/bari/R_files/Domicile/DomProject/DomicileDashboardShiny/RPOcc.RData")
load("/Users/bari/R_files/Domicile/DomProject/DomicileDashboardShiny/DomBookings.RData")
load("/Users/bari/R_files/Domicile/DomProject/DomicileDashboardShiny/WeekSum.RData")
load("/Users/bari/R_files/Domicile/DomProject/DomicileDashboardShiny/DaysDF.RData")
load("/Users/bari/R_files/Domicile/Domproject/DomicileDashboardShiny/domneighborhoods.RData")
load("~/R_files/Domicile/DomProject/DomicileDashboardShiny/MonthDet.RData")
load("~/R_files/Domicile/DomProject/DomicileDashboardShiny/MonthAll.RData")

DomCol11 <- c("#885EA8", "#6AA6E2", "#FF6347", "#878787", "#CD853F", "#36648B", "#FFC125", "#FB9A99", "#589CA1FD", "#B89C76FE", "#A2CD5A")
today <- ymd(cut(today(), "month")) 
monthstart <- ymd(paste(year(now()), "-", month(now()), "-01", sep = ""))
ninetyseq <- seq.Date(monthstart, by = "day", length.out = 90)

DomVel <- DomBookings %>% left_join(domneighborhoods, by = "Bldg_Name") %>% 
  mutate(rev_mo = ymd(cut(check_in_date, breaks = "month"), tz = ""),
         weekday = factor(weekdays(check_in_date, abbreviate = T), levels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")))

###PLOTS BELOW THIS LINE.   ABOVE THE LINE IS CODE THAT NEEDS TO RUN TO BUILD DATA FOR EACH PLOT AND FOR THE DASHBORD

class(ninetyseq)

MonRmDet <- MonthDet %>% select(Bldg_Name, rev_mo, booked_days, avail_days) %>% 
  gather(status, Room_Nights, 3:4) %>% 
  filter(Bldg_Name == "Excelsior", as.Date(rev_mo) <= max(ninetyseq)) %>%
  ggplot(aes(rev_mo, Room_Nights, text = paste("Month:", rev_mo, "<br>", status, ":", Room_Nights))) + 
  geom_col(aes(fill = status), position = "dodge") +
  scale_fill_manual(values = c("#878787", "#6AA6E2")) +
  scale_y_continuous(expand = c(0,0)) +
  theme_light() +
  theme(panel.grid = element_blank()) +
  labs(y = "Room Nights", x = "Month")

ggplotly(MonRmDet, tooltip = c("text"))

MonAvailAll <- MonthAll %>% select(rev_mo, booked_days, avail_days) %>%
                                     gather(status, Room_Nights, 2:3) 

MonRPDet <- MonthDet %>% select(Bldg_Name, rev_mo, ADR, RevPar) %>%
  gather(Metric, Dollars, 3:4) %>%
  filter(Bldg_Name == "Excelsior", as.Date(rev_mo) <= max(ninetyseq)) %>%
  ggplot(aes(rev_mo, Dollars, text = paste(Metric, "$", round(Dollars), "<br>Month", rev_mo))) + 
  geom_col(aes(fill = Metric), position = "dodge") +
  scale_fill_manual(values = c("#B89C76FE", "#A2CD5A")) +
  scale_y_continuous(labels = dollar, expand = c(0,0)) + 
  theme_light() +
  theme(panel.grid = element_blank()) +
  labs(y = "Dollars", x = "Month")

ggplotly(MonRPDet, tooltip= c("text"))

MonthAll %>% select(rev_mo, booked_days, avail_days) %>%
  gather(status, Room_Nights, 2:3) %>% filter(as.Date(rev_mo) <= max(ninetyseq))
MonRpAll <- MonthAll %>% select(rev_mo, ADR, RevPar) %>%
  gather(Metric, Dollars, 2:3)
##TEST RBOKEH  & GGVIS PLOTS not used
library(rbokeh)

bkdata <- MonthDet %>% select(Bldg_Name, rev_mo, booked_days, avail_days) %>% 
  gather(status, Room_Nights, 3:4) %>% filter(month(rev_mo) <= month(now()) + month(3))

f <- figure() %>% 
  ly_bar(rev_mo, Room_Nights, color = status, position = "dodge", 
         data = bkdata, hover = c(TRUE)) %>%
   y_axis("y", label = "Room Nights") %>%
  x_axis("x", label = "Month") %>%
   theme_axis("x", major_label_orientation = -45,
              axis_label_text_alpha = .35,
              axis_label_text_font_size = 1,
              axis_line_alpha = .35) %>% 
   set_palette(discrete_color = pal_color(c("#878787", "#6AA6E2"))) 
    
  
  ggvis(~month, ~Room_Nights, fill = ~status) %>%
  group_by(status) %>%
  layer_bars(stack = F) %>%
  scale_ordinal(property = "fill", range = c("#878787", "#6AA6E2"))

MonthAll %>% select(rev_mo, booked_days, avail_days) %>% 
  gather(status, Room_Nights, 2:3) %>% mutate(month = as.Date(rev_mo)) %>% 
  ggvis(~month, ~Room_Nights, fill = ~status) %>%
  group_by(status) %>%
  layer_bars(stack = F) %>%
  scale_ordinal(property = "fill", range = c("#878787", "#6AA6E2"))
## END TEST GGVIS & RBOKEH GRAPHS

##Leaflet Map of Buildings for Dashboard
Dmapdata <- MonthDet %>% filter(year(now()) == year(rev_mo), month(now()) - 1 == month(rev_mo))
pal <- c("#885EA8", "#6AA6E2", "#FF6347")
CohCol <- colorFactor(pal, Dmapdata$cohort)

icons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = ~CohCol(Dmapdata$cohort),
  library = "ion",
  markerColor = "lightgray"
)
leaflet(Dmapdata) %>% addProviderTiles(providers$Esri.WorldStreetMap) %>% 
  addAwesomeMarkers(icon = icons, 
                    popup = paste("<strong>",Dmapdata$Bldg_Name,"</strong>", "<br>",
                                  "Rooms:", Dmapdata$Rooms, "<br>",
                                  "RevMon:", Dmapdata$rev_mo, "<br>",
                                  "RevPar:", dollar(Dmapdata$RevPar), "<br>",
                                  "Occupancy:", percent(Dmapdata$Occ), "<br>",
                                  "ADR:", dollar(Dmapdata$ADR), "<br>", 
                                  "Total Revenue:", dollar(Dmapdata$Rev)),
                    layerId = Dmapdata$Bldg_Names) %>% 
  addCircles( lat = ~Latitude, 
             lng = ~Longitude, 
             group = ~cohort, 
             color = ~CohCol(cohort),
             radius = ~Rooms * 8 ) %>%
  addLegend(position = "bottomleft", pal = CohCol, values = ~cohort, title = "Cohort", opacity = 1)

#original interactive RevParPlot for Dashboard with Plotly and slider
RP <- RPOcc %>% filter(RevPar > 0 & RevPar < 500, year(weekdate) <= year(now())) %>% 
  ggplot(aes(weekdate, RevPar)) + 
  geom_point(aes(color = cohort), position = "jitter", alpha = 0.35) +
  geom_line(stat = "summary", fun.y = "median", color = "red", size = 2, alpha = 0.75, show.legend = F) +
  scale_x_datetime(date_breaks = "month", date_labels = "%b-%y") + 
  scale_y_continuous(labels = dollar_format()) + 
  scale_color_manual(values = DomCol11) +
  theme_light() +
  theme(axis.text.x = element_text(angle = -30),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  labs(title = "Revenue Per Available Unit", x = NULL, y = "Revenue Per Available Unit", color = "Building")
  

dyn <- ggplotly(RP, dynamicTicks = T)

rangeslider(dyn)


#heatmap showing weekly future outlook Occupancy rates.  filted by Cohort.
WeekOcc %>% filter(year(weekdate) == 2018, month(weekdate) >= month(now()), cohort == "MARINA") %>% 
  ggplot(aes(weekdate, listing_nickname, fill = occ_rate)) +
  geom_tile(show.legend = F) +
  geom_text(aes(label = percent(occ_rate)), size = 3) +
  scale_fill_gradient(low = "dark gray", high = "#885EA8") +
  scale_x_datetime(date_breaks = "week", date_labels = "%b %d", expand = c(0,0), position = "top") +
  theme_light() +
  labs(title = "Occupancy Outlook", y = NULL, x = NULL) +
  theme(axis.text.x = element_text(size = 6, face = "bold"),
        axis.text.y = element_text(face = "bold"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), 
        plot.background = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank())

#Weekly occupancy outlook by cohort heatmap.
WST %>% filter(!is.na(cohort)) %>% 
  ggplot(aes(weekdate, cohort, fill = occ)) +
  geom_tile(show.legend = F) +
  geom_text(aes(label = occhar), size = 3) +
  scale_fill_gradient(low = "dark gray", high = "#885EA8") +
  scale_x_datetime(date_breaks = "week", date_labels = "%b %d", expand = c(0,0), position = "top") +
  theme_light() +
  labs(title = NULL, y = NULL, x = NULL) +
  theme(axis.text.x = element_text(size = 6, face = "bold"),
        axis.text.y = element_text(face = "bold"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), 
        plot.background = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank())

#heatmap of occupancy
WeekSum %>% filter(year(weekdate) == 2018, month(weekdate) >= month(now()), !is.na(cohort)) %>% 
  ggplot(aes(weekdate, cohort, fill = occ)) +
  geom_tile(show.legend = F) +
  geom_text(aes(label = occhar), size = 3) +
  scale_fill_gradient(low = "dark gray", high = "#885EA8") +
  scale_x_datetime(date_breaks = "week", date_labels = "%b %d", expand = c(0,0), position = "top") +
  theme_light() +
  labs(title = NULL, y = NULL, x = NULL) +
  theme(axis.text.x = element_text(size = 6, face = "bold"),
        axis.text.y = element_text(face = "bold"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), 
        plot.background = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank())

#Heatmap showing future ADR
WeekOcc %>% filter(year(weekdate) == 2018, month(weekdate) >= month(now()), cohort == "MARINA") %>% 
  ggplot(aes(weekdate, listing_nickname, fill = ADR)) +
  geom_tile() + 
  geom_text(aes(label = dollar(ADR)), size = 3) + 
  scale_fill_gradient(low = "dark gray", high = "#FF6347") +
  scale_x_datetime(date_breaks = "week", date_labels = "%b %d", expand = c(0,0)) +
  theme_light() +
  labs(title = "Revenue Outlook", y = NULL, x = NULL) +
  theme(axis.text.x = element_text(size = 6, face = "bold"),
        axis.text.y = element_text(face = "bold"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), 
        plot.background = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank())

#weekly ADR outlook by cohort
WeekSum %>% mutate(cohort = factor(cohort)) %>% 
  filter(year(weekdate) == 2018, month(weekdate) >= month(now())) %>% 
  ggplot(aes(weekdate, cohort, fill = ADR)) +
  geom_tile() + 
  geom_text(aes(label = adrchar), size = 3) + 
  scale_fill_gradient(low = "dark gray", high = "#FF6347", limits = c(0, max(WeekSum$ADR) + 50)) +
  scale_x_datetime(date_breaks = "week", date_labels = "%b %d", expand = c(0,0)) +
  theme_light() +
  labs(title = "ADR Outlook", y = NULL, x = NULL) +
  theme(axis.text.x = element_text(size = 6, face = "bold"),
        axis.text.y = element_text(face = "bold"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), 
        plot.background = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank())

max(WeekSum$ADR) + 100
#Revenue Projections
Tot <- WeekSum %>%filter(year(weekdate) == 2018, 
                         month(weekdate) >= month(now()), month(weekdate) <= month(now()) + 2) %>%
  group_by(weekdate) %>% summarise(SumRev = sum(TotRev), TRChar = dollar(round(SumRev)))

##filter(year(weekdate) == 2018, month(weekdate) >= month(now()), month(weekdate) <= month(now()) + 2) 



RevPro <- WeekSum %>% mutate(cohort = factor(cohort)) %>% 
  filter(year(weekdate) %in% c(year(now()), year(now()) + 1), as.Date(weekdate) %in% ninetyseq) %>% 
  ggplot() +
  geom_bar(aes(x = weekdate, y = TotRev, fill = cohort), stat = "identity", show.legend = T) +
  geom_text(data = Tot, aes(x = weekdate, y = SumRev, label = dollar(round(SumRev)))) + 
  scale_x_datetime(date_breaks = "week", date_labels = "%b %d", expand = c(0,0)) +
  scale_fill_manual(values = DomCol11) +
  theme_light() + 
  theme(axis.text.x = element_text(angle = -30)) +
  labs(title = "Revenue Outlook by Week", x = NULL, y = "Total Revenue")



ggplotly(RevPro)

glimpse(WeekSum)
glimpse(WeekOcc)
#Occupancy line & bar graph by cohort
WeekSum %>% filter(year(weekdate) %in% c(year(now()), year(now()) + 1), 
                   as.Date(weekdate) %in% ninetyseq, 
                    !is.na(cohort)) %>% 
  select(cohort, weekdate, occ) %>%
  ggplot(aes(weekdate, occ)) +
  geom_col(aes(color = cohort, fill = cohort), position = "dodge") +
  geom_smooth(stat = "smooth", se = F) +
  scale_x_datetime(date_breaks = "week", date_labels = "%b %d", expand = c(0,0)) +
  scale_color_manual(values = DomCol11) +
  scale_fill_manual(values = DomCol11) + 
  theme_light()

##Facet Grid Bar showing Rooms Avaialble vs. Booked, future outlook.  Used on first page of dashboard.
gb <- DaysDF %>% group_by(listing_nickname, weekdate) %>% 
  summarize(AvailRooms = sum(AT, na.rm = T), BookedRooms = sum(BT, na.rm = T)) %>% ungroup() %>%
  left_join(BldgMaster, by = "listing_nickname") %>% group_by(cohort, weekdate) %>% 
  summarize(Available = sum(AvailRooms, na.rm = T), Booked = sum(BookedRooms, na.rm = T)) %>% ungroup() %>% 
  gather(key = "status", value = "Rooms", Available:Booked) %>% 
  filter(year(weekdate) == 2018, 
         month(weekdate) >= month(now()), month(weekdate) <= month(now()) + 2, !is.na(cohort)) %>% 
  ggplot(aes(weekdate, Rooms)) +
  geom_bar(aes(fill = status), stat = "identity", position = "dodge") + 
  scale_fill_manual(values = c("grey", "#6AA6E2")) +
  scale_x_datetime(date_breaks = "week", date_labels = "%b %d", expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 300)) +
  facet_grid(. ~ cohort) + 
  theme_light() + 
  theme(axis.text.x = element_text(angle = -30)) + 
  labs(title = "Available vs. Booked Rooms", subtitle = "by week for next 3 months", x = NULL)

ggplotly(gb)

###WEEKLY Performance Summary by Cohort### facet grids blue/grey

all <- DaysDF %>% group_by(listing_nickname, weekdate) %>% 
  summarize(AvailRooms = sum(AT, na.rm = T), 
            BookedRooms = sum(BT, na.rm = T)) %>% ungroup() %>%
  group_by(weekdate) %>% summarize(AvailRooms = sum(AvailRooms, na.rm = T), 
                                   BookedRooms = sum(BookedRooms, na.rm = T)) %>% ungroup() %>%
  gather("status", "Rooms", AvailRooms:BookedRooms)

cohortavail <- DaysDF %>% group_by(listing_nickname, weekdate) %>% 
  summarize(AvailRooms = sum(AT, na.rm = T), BookedRooms = sum(BT, na.rm = T)) %>% ungroup() %>%
  left_join(BldgMaster, by = "listing_nickname") %>% group_by(cohort, weekdate) %>% 
  summarize(Available = sum(AvailRooms, na.rm = T), Booked = sum(BookedRooms, na.rm = T)) %>% ungroup() %>% 
  gather(key = "status", value = "Rooms", Available:Booked)

neighborhoodavail <- DaysDF %>% group_by(listing_nickname, weekdate) %>% 
  summarize(AvailRooms = sum(AT, na.rm = T), BookedRooms = sum(BT, na.rm = T)) %>% ungroup() %>%
  left_join(BldgMaster, by = "listing_nickname") %>% group_by(neighborhood, weekdate) %>% 
  summarize(Available = sum(AvailRooms, na.rm = T), Booked = sum(BookedRooms, na.rm = T)) %>% ungroup() %>% 
  gather(key = "status", value = "Rooms", Available:Booked)

bldgavail <-  DaysDF %>% group_by(listing_nickname, weekdate) %>% 
  summarize(AvailRooms = sum(AT, na.rm = T), BookedRooms = sum(BT, na.rm = T)) %>% ungroup() %>%
  left_join(BldgMaster, by = "listing_nickname") %>% group_by(Bldg_Name, weekdate) %>% 
  summarize(Available = sum(AvailRooms, na.rm = T), Booked = sum(BookedRooms, na.rm = T)) %>% ungroup() %>% 
  gather(key = "status", value = "Rooms", Available:Booked)
#barcharts gridded by cohort showing available rooms next to booked rooms.
cohortavail %>% filter(year(weekdate) == 2018, !is.na(cohort)) %>% 
  ggplot(aes(weekdate, Rooms)) +
  geom_col(aes(fill = status), position = "dodge") + 
  scale_fill_manual(values = c("grey", "#6AA6E2")) +
  scale_x_datetime(date_breaks = "week", date_labels = "%b %d", expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 375)) +
  facet_grid(cohort ~ .) + 
  theme_light() + 
  theme(axis.text.x = element_text(angle = -30),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) + 
  labs(title = "Available vs. Booked Rooms", 
       subtitle = "2018 by week", x = NULL)
#barcharts facet gridded by building(select filter Bldg_Name) showing available rooms next to booked rooms.
bldgavail %>% filter(year(weekdate) == 2018, !is.na(Bldg_Name), Bldg_Name == "Chroma SLU") %>% 
  
  all %>% filter(year(weekdate) == 2018) %>% ggplot(aes(weekdate, Rooms)) +
  geom_col(aes(fill = status), position = "dodge") + 
  scale_fill_manual(values = c("grey", "#6AA6E2")) +
  scale_x_datetime(date_breaks = "week", date_labels = "%b %d", expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 850)) +
  theme_light() + 
  theme(axis.text.x = element_text(angle = -30),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) + 
  labs(title = "Available vs. Booked Rooms", 
       subtitle = "2018 by week", x = NULL)
library(ggvis)
abs.Date <- function(x){x}
statcols <- ifelse(unique(all$status) == "AvailRooms", "#878787", "#6AA6E2")
all %>% filter(year(weekdate) == 2018) %>% mutate(weekdate = as.Date(weekdate)) %>%
  ggvis(~weekdate, ~Rooms, fill = ~status) %>%
  group_by(status) %>%
  layer_bars(stack = F, opacity := 0.75) %>%
  scale_ordinal("fill", range = c("#878787", "#6AA6E2")) 
  
  
### REVPar performance charts

TotOcc <- WeekOcc %>% group_by(year = year(weekdate), weekdate) %>% 
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

totadrrp <- TotOcc %>% group_by(year, weekdate) %>% 
  summarize(ADR = sum(ADR), RevPar = sum(RevPar)) %>% 
  gather(Category, Val, ADR:RevPar) %>% 
  filter(year == 2018, epiweek(weekdate) < epiweek(now()))


RPADR <- WeekSum %>% select(cohort, year, weekdate, ADR, RevPar) %>% 
  gather(Category, Val, ADR:RevPar) %>% 
  filter(year == 2018, epiweek(weekdate) < epiweek(now()), cohort == "MARINA") %>%
  ggplot(aes(weekdate, Val)) +
  geom_col(aes(fill = Category), position = "dodge", show.legend = F) +
  scale_fill_manual(values = c("#B89C76FE", "#A2CD5A"), guide = "none") +
  scale_y_continuous(labels = dollar) +
  scale_x_datetime(date_breaks = "month", date_labels = "%b %y", expand = c(0,0)) +
  theme_light() +
  theme(axis.text.x = element_text(angle = -30),
        panel.grid.major.x = element_blank())

ggplotly(RPADR, tooltip = c("x", "color", "y"), layerData = 1)
#CODE TO CHANGE THE COLOR OF THE STRIPS ON A facet grid by changing the GROB layers.  
#Output is not GGPLOTLY friendly
g <- ggplot_gtable(ggplot_build(gb))
strip_t <- which(grepl('strip-t', g$layout$name))
fills <- c("#885EA8", "#6AA6E2", "#FF6347")
k <- 1
for(i in strip_t) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.arrange(g)
###
write_csv("~/R_files/Domicile/DomProject/DomData/barocc.csv")


###REGRESSION MODELS FOR PRICE ANALYSIS 

DomVel %>% filter(days_in_advance >= 0, !is.na(cohort)) %>%
  ggplot(aes(days_in_advance, ADR, color = cohort)) +
  geom_point(alpha = 0.35, position = "jitter", show.legend = T) +
  geom_smooth(method = "lm", formula = y~x, se = T, na.rm = T, color = "gray") + 
  facet_grid(.~cohort, scales = "free_x") + 
  scale_color_manual(values = DomCol11) +
  labs(title = "Price Regression Model", 
       subtitle = "Is price impacted by how far in advance the booking is made?",
       x = "days in advance") +
  scale_y_continuous(labels = dollar_format(), expand = c(0,0)) +
  theme_light() +
  theme(strip.text = element_text(face = "bold", size = 12),
        axis.text = element_text(size = 11))


#HEAT MAP of building occupancy
BuildingHeatMap <- 
  DomSummary %>% filter(year < 2019) %>% group_by(Bldg_Name, rev_mo) %>%   
  summarize(occ = mean(occ_rate)) %>% ungroup() %>% 
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

#plot Average Revenue on top of occupancy rate
#change text in the facet titles 
RevOcc$Metric = factor(RevOcc$Metric, labels = c("Average Revenue Per Night", "Occupancy Rate"))
#create a labeller for Facets
facetlab <- function(x) {
  ifelse(x >= 10, paste("$", x),
         percent(x))
}

RevOcc %>% filter(Bldg_Name == "Marina SLU") %>%
  ggplot(aes(x = rev_mo, y = value, group = Bldg_Name, color = Metric, fill = Metric)) +
  geom_bar(stat = "identity", show.legend = T) +
  geom_smooth(se = F, show.legend = T) +
  facet_wrap(~Metric, scale = "free_y", ncol = 1) +
  labs(x = NULL, y = NULL) +
  scale_x_datetime(date_labels = "%b-%y", date_breaks = "month",  date_minor_breaks = "year", expand = c(0,0)) +
  scale_y_continuous(labels = facetlab) +
  scale_color_manual(values = c("#6AA6E2", "#885EA8")) +
  scale_fill_manual(values = c("#6AA6E2", "#885EA8")) +
  theme_light() +
  theme(strip.background = element_rect(fill = "#969696"),
        axis.text.x = element_text(angle = -30, size = 12, face = "bold"),
        axis.text.y = element_text(size = 12, face = "bold"),
        strip.text = element_text(color = "white", face = "bold", size = 12),
        panel.grid.major.x = element_blank())


###experimental grid plot of rev & occupancy by week
WeekOcc %>% group_by(cohort, neighborhood, weekdate) %>% 
  summarize(avail = sum(avail_days), 
            booked = sum(booked_days), 
            occ =booked/avail, 
            REV = sum(Rev), 
            ADR = REV / booked) %>% 
  ungroup() %>% select(neighborhood, weekdate, occ, ADR) %>% 
  gather(key = Metric, value = Value, occ:ADR) %>% 
  ggplot(aes(weekdate, Value, group = neighborhood, color = Metric, fill = Metric)) + 
  geom_bar(stat = "identity", show.legend = T) +
  facet_wrap(.~Metric, scale = "free_y", ncol = 1)
           
#ANNUAL AVERAGE REVENUE PLOT
AnnualAvgRev <- BookingMaster %>% group_by(cohort, year) %>% 
  summarise(An_days_booked = sum(true),
            An_Revenue = sum(rev),
            An_Avg_Rev = sum(rev) / sum(true),
            Bookings = sum(true)) %>% ungroup()


AnnualAvgRev %>%  
  mutate(cohort = fct_reorder2(as.factor(cohort), An_Avg_Rev, year)) %>% 
  ggplot(aes(x = factor(-An_Avg_Rev), y = An_Avg_Rev, color = cohort , fill = cohort)) +
  geom_bar(stat = "identity", show.legend = T) + 
  geom_text(aes(label = paste("$", round(An_Avg_Rev)), angle = 90, hjust = -0.5))  +
  facet_grid(. ~ year, scale = "free_x", space = "free_x") +
  scale_color_manual(values = DomCol11) +
  scale_fill_manual(values = DomCol11) +
  scale_y_continuous(labels = dollar_format(), expand = c(0,0), limits = c(0, 300)) +
  labs(x = "", y = "Average Daily Rate", title = "Average Daily Rate by Cohort", 
       subtitle = "YTD 2018, includes future bookings") +
  theme_light() + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid = element_blank())


##Price vs. Days in Advance with Linear Regression.  
DomVel %>% filter(days_in_advance >= 0, !is.na(cohort)) %>%
  ggplot(aes(days_in_advance, ADR, color = cohort)) +
    geom_point(alpha = 0.35, position = "jitter", show.legend = T) +
    geom_smooth(method = "lm", formula = y~x, se = F, na.rm = T, color = "gray") + 
    facet_grid(.~cohort, scales = "free_x") + 
    scale_color_manual(values = DomCol11) +
    labs(title = "Price Regression Model", 
         subtitle = "Relationship of price to how far in advance a booking is made",
         x = "days in advance") +
    scale_y_continuous(labels = dollar_format(), expand = c(0,0)) +
    theme_light() +
    theme(strip.text = element_text(face = "bold", size = 12),
          axis.text = element_text(size = 11))

glimpse(DomVel)

ldv <- DomVel %>% filter(days_in_advance >= 0, !is.na(cohort))

cor(DomVel$ADR, DomVel$days_in_advance)
mod <-lm (ADR~days_in_advance, cohort, data = DomVel, na.action = na.omit)
plot <- augment(mod)

plot %>% ggplot(aes(ADR, .fitted)) +
  geom_jitter(width = 0.5, height = 0.5) + 
  facet_grid(.~cohort)

summary(mod)

mod1 <- glm(AT~neighborhood, data = DaysDF, family = binomial)
summary(mod1)
head(DaysDF)
#Boxplot median by source.  

RoomWt <- DomVel %>% group_by(cohort, Bldg_Name) %>% summarise(rooms = n_distinct(listing_nickname))
boxmed <- DomVel %>% left_join(RoomWt, by = c("cohort", "Bldg_Name")) %>% group_by(Bldg_Name) %>% 
  summarise(med = median(ADR))
smed <- DomVel %>% left_join(RoomWt, by = c("cohort", "Bldg_Name")) %>% filter(!is.na(cohort)) %>%
  group_by(source, cohort) %>% 
  summarise(med = median(ADR))

DomVel %>% left_join(RoomWt, by = c("cohort", "Bldg_Name")) %>% 
  filter(days_in_advance >= 0, !is.na(cohort), 
  year(check_in_date) == 2018) %>%
  ggplot(aes(source, ADR, color = source)) +
  geom_point(alpha = 0.35, position = "jitter", show.legend = T) +
  geom_boxplot(aes(y = ADR, weight=rooms), color = "#3E4042FE", varwidth = T) +
  geom_text(data = smed, aes(x=source, y = med, label = dollar(med))) +
  facet_grid(cohort~source, scales = "free_x") + 
  scale_color_manual(values = DomCol11) +
  labs(title = "ADR by Source of Booking", 
       x = NULL) +
  scale_y_continuous(labels = dollar_format(), expand = c(0,0)) +
  theme_light() +
  theme(strip.text = element_text(face = "bold", size = 12),
        axis.text = element_text(size = 11),
        panel.grid.major.x = element_blank()
        ) 

#Price BoxPlot
RoomWt <- DomVel %>% group_by(cohort, Bldg_Name) %>% summarise(rooms = n_distinct(listing_nickname)) 
boxmed <- DomVel %>% left_join(RoomWt, by = c("cohort", "Bldg_Name")) %>% group_by(Bldg_Name) %>% 
  summarise(med = median(ADR))

DomVel %>% left_join(RoomWt, by = c("cohort", "Bldg_Name")) %>% 
  filter(year(rev_mo) == 2018) %>%
  ggplot(aes(x = Bldg_Name)) +
  geom_point(aes(y = ADR, color = cohort), position = "jitter", alpha = 0.25, show.legend = T) +
  geom_boxplot(aes(y = ADR, weight=rooms), color = "dark gray", varwidth = T) +
  geom_text(data = boxmed, aes(x=Bldg_Name, y = med, label = dollar(med))) +
  scale_color_manual(values = DomCol11) + 
  scale_y_continuous(labels = dollar_format()) + 
  theme_light() +
  theme(panel.grid.major.x = element_blank(),
        axis.text.x = element_text(size = 10, face = "bold", angle = -30)) +
        labs(title = "Average Daily Revenue by Building", subtitle = paste(max(year(rev_mo)), "with median ADR labeled"), x = NULL, 
       caption = "width of boxes adjusted by number of rooms in the building \nbox encloses 50% of ADR's\nmin, max and outliers shown by lines and dark dots\nlight_colored dots show individual ADRs by booking")

#weighted mean experimentation for line graph.

totrms <- DomVel %>% summarise(rooms = n_distinct(listing_nickname))
WeightedRooms <- DomVel %>% group_by(cohort, Bldg_Name) %>% 
  summarise(rooms = n_distinct(listing_nickname)) %>% ungroup() %>%
  group_by(cohort) %>% summarize(rooms = sum(rooms), rmwt = sum(rooms / totrms)) %>% ungroup() 

WeekSum %>% left_join(WeightedRooms, by = "cohort") %>%
  mutate(wtdocc = occ * rmwt) %>%
  filter(year(weekdate) %in% c(year(now()), year(now()) + 1), 
                   as.Date(weekdate) %in% ninetyseq, 
                   !is.na(cohort)) %>% 
  select(cohort, weekdate, occ, wtdocc) %>%
  ggplot(aes(weekdate, wtdocc)) +
  geom_col(aes(color = cohort, fill = cohort), position = "dodge") +
  geom_smooth(stat = "smooth", se = F) +
  scale_x_datetime(date_breaks = "week", date_labels = "%b %d", expand = c(0,0)) +
  scale_color_manual(values = DomCol11) +
  scale_fill_manual(values = DomCol11) + 
  theme_light()

WeekSum %>% left_join(WeightedRooms, by = "cohort") %>%
  mutate(wtdocc = occ * rmwt) %>%
  filter(!is.na(cohort), year(weekdate) == 2018) %>% 
  select(cohort, weekdate, occ, wtdocc) %>%
  ggplot(aes(weekdate, occ)) +
  geom_col(aes(color = cohort, fill = cohort), position = "dodge") +
  geom_smooth(stat = "smooth", se = F) +
  scale_x_datetime(date_breaks = "week", date_labels = "%b %d", expand = c(0,0)) +
  scale_color_manual(values = DomCol11) +
  scale_fill_manual(values = DomCol11) + 
  theme_light()

WeekSum %>% left_join(WeightedRooms, by = "cohort")
#TOTAL number of bookings by month, colored by cohort. (9-15-18)
TotBook <- DomVel %>% filter(year(rev_mo) <= 2018) %>% group_by(rev_mo) %>% 
  summarize(booking = n(), revenue = round(sum(host_payout))) %>% ungroup()


DomVel %>% filter(year(rev_mo) <= 2018) %>% group_by(cohort, rev_mo) %>% summarize(bookings = n()) %>% 
  ggplot(aes(rev_mo)) + 
  geom_bar(stat = "identity", aes(y = bookings, color = cohort, fill = cohort), show.legend = T) + 
  scale_color_manual(values = DomCol11) +
  scale_fill_manual(values = DomCol11) +
  scale_x_datetime(date_labels = "%b-%y", date_breaks = "month") +
  theme_light() + 
  theme(axis.text.x = element_text(angle = -30)) +
  labs(title = "Bookings by Month", subtitle = "bookings = count of confirmed bookings checking in each month", 
       x = "Revenue Month") +
  geom_text(data = TotBook, aes(y = booking , label = booking),  vjust = -1)


#Total number of bookings colored by Source
DS <- DomVel %>% filter(year(rev_mo) <= 2018) %>% group_by(source, rev_mo) %>% summarize(bookings = n()) %>% 
  ggplot(aes(rev_mo)) + 
  geom_bar(stat = "identity", aes(y = bookings, color = source, fill = source), show.legend = T) +
  scale_color_manual(values = DomCol11) +
  scale_fill_manual(values = DomCol11) +
  scale_x_datetime(date_labels = "%b-%y", date_breaks = "month") +
  theme_light() + 
  theme(axis.text.x = element_text(angle = -30),
        panel.grid = element_blank()) +
  labs(title = "Bookings by Month", subtitle = "bookings = count of confirmed bookings checking in each month", 
       x = "Revenue Month") +
  geom_text(data = TotBook, aes(y = bookings , label = bookings),  vjust = -4)

ggplotly(DS) 

##Plot to track revenue by cohort and source
DR <- DomVel %>% filter(year(rev_mo) <= 2018) %>% group_by(cohort, rev_mo) %>% 
  summarize(bookings = n(), revenue = sum(host_payout)) %>% 
  ggplot(aes(rev_mo)) + 
  geom_bar(stat = "identity", aes(y = revenue / 1000, color = cohort, fill = cohort), show.legend = T) +
  scale_color_manual(values = DomCol11) +
  scale_fill_manual(values = DomCol11) +
  scale_x_datetime(date_labels = "%b-%y", date_breaks = "month") +
  theme_light() + 
  theme(axis.text.x = element_text(angle = -30),
        panel.grid = element_blank()) +
  geom_text(data = TotBook, aes(y = revenue / 1000 , 
                                label = paste(dollar(round(revenue / 1000)), "K", sep = ""), 
                                angle = 90)) + 
  scale_y_continuous(labels = dollar) +
  labs(title = "Revenue by Month", subtitle = "Revenue = sum of host payout", 
       x = "Revenue Month", y = "$ Thousands") 

ggplotly(DR)

#data summaries used to calculate the value boxes for the KPI page.
YTDSummary <- DaysDF  %>%  filter(year(now()) == year(booked), booked < floor_date(now(), "month")) %>% 
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


WeekOcc %>% select(neighborhood, cohort, Bldg_Name, listing_nickname, year:RevPar)

str(DomVel)
DVmod <- lm(ADR~source, data = DomVel, na.action = na.omit)
summary(DVmod)
df2 <- augment(DVmod)
std_resid <- rstandard(DVmod)
cooks_D <- cooks.distance(DVmod)
hat_vals <- hatvalues(DVmod)
plot(hat_vals, std_resid, cex = 10*sqrt(cooks_D))
abline(h=c(-2.5, 2.5), lty = 2)

sourcemed <- DomVel %>% group_by(source) %>% summarise(medADR = median(ADR)) %>% ungroup()
DomVel %>% ggplot(aes(source, ADR)) +
  geom_point(aes(color = source), alpha = .45, size = 3, position = "jitter") + 
  scale_color_manual(values = DomCol11) +
  geom_boxplot() + 
  geom_text(data = sourcemed, aes(x = source, y = medADR + 10, label = dollar(medADR))) + 
  scale_y_continuous(labels = dollar) +
  theme_light() + 
  theme(panel.grid.major.x = element_blank()) +
  labs(title = "ADR by Source, Median labeled", x = NULL)

glimpse(DomVel)

DomVel %>% mutate(source = factor(source)) %>% select(source, ADR, num_nights, days_in_advance) %>%
  summary()
#number of nights by checkin day of week.
MEDCOH <- DomVel %>% group_by(cohort, weekday) %>% filter(year(check_in_date) == 2018) %>%
  summarise(check_ins = n(), medstay = median(num_nights)) %>% filter(!is.na(cohort)) %>%
  ggplot(aes(weekday, medstay)) + 
  geom_col(aes(fill=cohort), position = "dodge") +
  scale_fill_manual(values = DomCol11) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 4.5)) +
  theme_light() + 
  theme(panel.grid.major.x = element_blank()) +
  labs(title = "Median Number of Nights Stayed", subtitle = "by Check in Day of Week, 2018 YTD",
       y = "median duration of stay", x = "day of check in")

TotObs <- DomVel %>% filter(year(check_in_date) <= 2018) %>% 
  select(ADR, num_nights, days_in_advance, weekday) %>%
  filter(num_nights < 25) %>% group_by(weekday) %>% mutate(N = n())

DomVel %>% filter(year(check_in_date) == 2018, num_nights < 25) %>%
  ggplot(aes(weekday, num_nights)) +
  geom_boxplot() + 

  coord_flip()

#median stay by day of week, points sized by number of checkins.
DomVel %>% group_by(weekday) %>% filter(year(check_in_date) == 2018) %>%
  summarise(check_ins = n(), medstay = median(num_nights)) %>%
  ggplot(aes(weekday, medstay)) + 
  geom_point(aes(size = check_ins), colour = "red") +
  scale_y_continuous(limits = c(1, 5), breaks = c(1:5))

DomVel %>% filter(year(check_in_date) == 2018) %>% 
  select(ADR, num_nights, days_in_advance, weekday) %>% summary()

##histgram _ distribution of number of nights
DomVel %>% filter(year(check_in_date) <= 2018) %>% 
  select(ADR, num_nights, days_in_advance, weekday) %>%
  filter(num_nights < 25) %>%
  ggplot(aes(num_nights)) +
  geom_histogram(fill = "#8B668B", binwidth = 1) +
  scale_x_continuous(breaks = c(0:25), expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_light() + 
  labs(title = "Distribution of Stay Length, 2018 YTD", 
       subtitle = "excluding outliers > 25 days",
       y = "number of bookings", x = "number of nights")

#check in days of week by cohort
DOWFreq <- DomVel %>% group_by(cohort, weekday) %>% 
  summarize(daynum = n()) %>% ungroup() %>% group_by(cohort) %>% 
  mutate(totday = sum(daynum), pct = daynum / sum(daynum)) %>% filter(!is.na(cohort)) %>%
  ggplot(aes(weekday, pct, group = cohort)) + 
  geom_col(aes(fill = cohort), position = "dodge") + 
  geom_text(aes(label = percent(pct), y = pct + 0.005), position = position_dodge(width = 1)) +
  scale_fill_manual(values = DomCol11) +
  scale_y_continuous(label = percent, expand = c(0,0), limits = c(0, 0.25)) +
  theme_light() + 
  theme(panel.grid.major.x = element_blank()) +
  labs(title = "Check In Days of Week by Cohort", subtitle = "% of total", 
       x = "Percentage of Total Check Ins", y = "Day of Check In")

grid.arrange(DOWFreq, MEDCOH)

DomVel %>% group_by(cohort, weekday) %>% 
  summarize(daynum = n()) %>% ungroup() %>% group_by(cohort) %>% 
  mutate(totday = sum(daynum), pct = daynum / sum(daynum)) %>% 
  select(cohort, weekday, daynum, pct) %>%
  gather(Metric, Val, 3:4) %>% 
  ggplot(aes(weekday, Val, group = cohort, fill = cohort)) +
  geom_col(aes(fill = cohort), position = "dodge") + 
  facet_wrap(Metric~., scales = "free_y", ncol = 1)

glimpse(DomVel)

DomVel %>% filter(ADR > 300) %>% 
  ggplot(aes(rev_mo, ADR)) +
  geom_point(aes(color = cohort), position = "jitter", alpha = 0.40, size = 5) +
  scale_x_datetime(date_labels = "%b-%y", date_breaks = "month") + 
  scale_y_continuous(label = dollar) + 
  scale_color_manual(values = DomCol11) +
  theme_light() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()) +
  annotate("text", x = mdy(040117, tz = ""), y = 450, 
           label = c("$300 and above ADR \ntends to happen in the summer"), size = 5) +
  labs(title = "Bookings with ADR > $300", subtitle = "by Revenue Month", 
       x = NULL, y = NULL)

