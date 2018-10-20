library(shiny)
library(shinydashboard)
library(tidyverse)
library(tidyr)
library(dplyr)
library(readr)
library(tibble)
library(ggplot2)
library(purrr)
library(lubridate)
library(DBI)
library(scales)
library(forcats)
library(DT)
library(plotly)
library(leaflet)
library(packrat)

load("./Data/dommaster.RData")
load("./Data/BookingMaster.RData")
load("./Data/DomSummary.RData")
load("./Data/WeekOcc.RData")
load("./Data/RPOcc.RData")
load("./Data/BldgMaster.RData")
load("./Data/WeekSum.RData")
load("./Data/DaysDF.RData")
load("./Data/DomBookings.RData")
load("./Data/domneighborhoods.RData")
load("./Data/MonthDet.RData")
load("./Data/MonthAll.RData")

### comment change
#Time Markers - rolling 90 days filter
monthstart <- ymd(paste(year(now()), "-", month(now()), "-01", sep = ""))
ninetyseq <- seq.Date(monthstart, by = "day", length.out = 90)

#Color Palette
DomColor <- c("#8B8B00", "#8B636C", "#EE5C42", "#36648B", "#EE9A00", "#5C5C5C", "#CD6839", "#00868B", "#FF4500", "#4F94CD", "#8B2252", "#EEEE00")
DomCol11 <- c("#885EA8", "#6AA6E2", "#FF6347", "#878787", "#CD853F", "#36648B", "#FFC125", "#FB9A99", "#589CA1FD", "#B89C76FE", "#A2CD5A")
DomColSource <- c("#8B668B", "#6CA6CD", "#FF6347", "#A2CD5A", "#878787", "#CD853F", "#36648B")

ListingNicknames <- unique(dommaster$listing_nickname)
BuildingNames <- unique(dommaster$Bldg_Name)

YTDSummary <- DaysDF  %>%  filter(year(now()) == year(booked), 
                                  booked < floor_date(now(), "month")) %>% 
  summarise(YTDRev = sum(rev, na.rm = T), 
            YTDOcc = sum(BT, na.rm = T) / sum(AT, na.rm = T), 
            YTDADR = sum(rev, na.rm = T)/ sum(BT, na.rm = T), 
            YTDRevPar = YTDADR * YTDOcc)

CurMoSummary <- DaysDF  %>%  filter(year(now()) == year(booked), 
                                    rev_mo == floor_date(now(), "month")) %>% 
  
  summarise(MoRev = sum(rev, na.rm = TRUE), 
            MoOcc = sum(BT) / sum(AT), 
            MoADR = MoRev / sum(BT), 
            MoRevPar = MoADR * MoOcc) %>% mutate(MoRev = dollar(MoRev),
                                                 MoOcc = percent(MoOcc, accuracy = 1),
                                                 MoADR = dollar(MoADR, accuracy = 1),
                                                 MoRevPar = dollar(MoRevPar, accuracy = 1))


DomVel <- DomBookings %>% left_join(domneighborhoods, by = "Bldg_Name") %>% 
  mutate(rev_mo = ymd(cut(check_in_date, breaks = "month"), tz = ""),
         weekday = factor(weekdays(check_in_date, abbreviate = T), levels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")))

WS <- WeekSum %>% filter(year(weekdate) %in% c(year(now()), year(now()) + 1), 
                         as.Date(weekdate) %in% ninetyseq) 
WT <- WS %>% group_by(year, weekdate) %>% 
  summarise(avail = sum(avail), 
            booked = sum(booked), 
            occ = booked / avail, 
            ADR = mean(ADR), 
            occhar = percent(occ),
            adrchar = dollar(ADR)) %>% mutate(cohort = "ALL") %>% ungroup() %>% 
  select(cohort, year, weekdate, avail, booked, occ, ADR, occhar, adrchar)
WST <- bind_rows(WS, WT)

# Define UI for application that draws a histogram
ui <- dashboardPage(
    skin = "black",
    dashboardHeader(title = "domicile"),
    
    dashboardSidebar(
      sidebarMenuOutput("menu")
    ),
      
    dashboardBody(
      tags$style(".small-box.bg-purple { background-color: #885EA8 !important; color: #DBDAD8 !important; }"),
      tags$style(".small-box.bg-orange { background-color: #FF6347 !important; color: #DBDAD8 !important; }"),
      tags$style(".small-box.bg-light-blue { background-color: #6CA6CD !important; color: #DBDAD8 !important; }"),
      tags$style(".small-box.bg-fuchsia { background-color: #878787 !important; color: #DBDAD8 !important; }"),
      tabItems(
        tabItem(tabName = "map",
                box(title = paste(month(now() - months(1), label = T, abbr = F), "Building Performance (click marker)"), width = 6, 
                    leafletOutput("bldgmap", width = "100%", height = "800px")),
                box(width = 2, actionButton("reset", "Show Total All Buildings")),
                box(width = 6, plotlyOutput("bldgplot", width = "100%", height = "350px")),
                box(width = 6, plotlyOutput("bdrpplot", width = "100%", height = "350px"))
                ),
        tabItem(tabName = "KPI",
                fluidRow(
                  box(title = "Year to Date KPI's", width = 12,
                      valueBoxOutput("YTDRev", width = 3),
                      valueBoxOutput("YTDOcc", width = 3),
                      valueBoxOutput("YTDADR", width = 3),
                      valueBoxOutput("YTDRevPar", width = 3)
                  )),
                  fluidRow(
                    box(title =  paste(month(now(), label = T, abbr = F), "KPI's"), width = 12,
                        valueBoxOutput("MoRev", width = 3),
                        valueBoxOutput("MoOcc", width = 3),
                        valueBoxOutput("MoADR", width = 3),
                        valueBoxOutput("MoRevPar", width = 3)
                    )),
                 fluidRow(
                  box(width = 3, selectInput("CorS", "Select Chart View", c("cohort", "source")))
                ),
                   fluidRow(
                     box(width = 6, plotlyOutput("sourcevel", width = "100%", height = "500px")),
                     box(width = 6, plotlyOutput("revevel", width = "100%", height = "500px"))
                    ),
                fluidRow(
                  box(width = 10, plotlyOutput("roomsbk", width = "100%", height = "300px"))
                )),
        tabItem(tabName = "charts",
      fluidRow(
        box(width = 9, plotOutput("Occupancy")),
        box(width = 3, selectInput("cohort1", "Select Cohort", c("All", "CHROMA", "MARINA", "OG30")))
      ),
      fluidRow(
        box(width = 6, plotlyOutput("availbooked", width = "100%", height = "300px")),
        box(width = 6, plotlyOutput("revparadr", width = "100%", height = "300px"))
        ),
      fluidRow(
        box(dataTableOutput("abtable")),
        box(dataTableOutput("pradrtable")
      )
      )),
      tabItem(tabName = "OccOut",
              fluidRow(
                box(width = 12, plotOutput("WeekOcc", width = "100%", height = "200px"))),
              fluidRow(
                column(width = 3,
                       fluidRow(
                  box(width = 12, selectInput("coh1", "Select Cohort", c("OG30", "MARINA", "CHROMA")))),
                       fluidRow(
                  box(width = 12, downloadButton(outputId = "dlweekocc", label = "Download Occupancy & Revenue Data")))),
                column(width = 9,
                  box(title = "Occupancy Rate by Cohort", width = 12, plotlyOutput("occproj", width = "100%", height = "200px")))),
              fluidRow(
                box(width = 12, height = 900, plotOutput("OccOut", width = "100%", height = "700px"))
                )),
    tabItem(tabName = "RevOut",
            fluidRow(
              box(width = 12, plotOutput("WeekRev", width = "100%", height = "200px"))),
                     
            fluidRow(
              box(width = 3, selectInput("coh2", "Select Cohort", c("OG30", "MARINA", "CHROMA"))),
              box(title = "Total Booked Revenue by Week", width = 9, plotlyOutput("revproj", width = "100%", height = "300px"))),
            fluidRow(
              box(width = 12, plotOutput("RevOut", width = "100%", height = "900px" ))
            )),
    tabItem(tabName = "PRICE",
            fluidRow(
            box(title = "Pricing: Advance Bookings", width = 6, plotOutput("PriceReg")),
            box(title = "Average Daily Revenue", width = 6, plotOutput("boxplotprice"))
            ),
            fluidRow(
              box(title = "ADR by Source of Booking", width = 6, plotOutput("sourceADR"))
            )
           
  ),
  tabItem(tabName = "rawdata",
          box(sliderInput("daterange", "Select Date Range", 
                          min = min(DomBookings$check_in_date), 
                          max = max(DomBookings$check_in_date), 
                          value = c(now()-7776000, now()))),
          box(selectInput("datachoice", "Choose Data Table", c("DomBookings", "WeeklyMetrics"))),
          box(downloadButton(outputId = "dlbooking", label = "Download")),
          box(title = "Bookings Data", width = 12, dataTableOutput("bookingtable"))))))


server <- function(input,output, session) ({

  ##reactive menu script.  
  output$menu <- renderMenu({
      sidebarMenu(
      id = "tabs",
      menuItem("Map With Building Detail", icon = icon("dashboard"), tabName = "map"),
      menuItem("Summary KPI's", icon = icon("dashboard"), tabName = "KPI"),
      menuItem("Weekly Outlook", icon = icon("dashboard"),
               menuSubItem("Occupancy Outlook", tabName = "OccOut"),
               menuSubItem("Revenue Outlook", tabName = "RevOut")),
      menuItem("Annual Performance", tabName = "charts", icon = icon("dashboard")),
      menuItem("Pricing Detail", tabName = "PRICE", icon = icon("dashboard")),
      menuItem("Raw Data", tabName = "rawdata", icon = icon("dashboard"))
      )
  })
  
  #Leaflet Map of buildings
   output$bldgmap <- renderLeaflet({
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
                        popup = paste(Dmapdata$Bldg_Name,"<br>",
                                      "Rooms:", Dmapdata$Rooms, "<br>",
                                      "RevMon:", Dmapdata$rev_mo, "<br>",
                                      "RevPar:", dollar(Dmapdata$RevPar), "<br>",
                                      "Occupancy:", percent(Dmapdata$Occ), "<br>",
                                      "ADR:", dollar(Dmapdata$ADR), "<br>", 
                                      "Total Revenue:", dollar(Dmapdata$Rev)),
                                       layerId = Dmapdata$Bldg_Name) %>% 
      addCircles( lat = ~Latitude, 
                  lng = ~Longitude, 
                  group = ~cohort, 
                  color = ~CohCol(cohort),
                  radius = ~Rooms * 8 ) 
  })
  
  clickRV <- reactiveValues(clickedMarker=NULL)

  observeEvent(input$bldgmap_marker_click, {
        clickRV$clickedMarker <- input$bldgmap_marker_click
        print(clickRV$clickedMarker)
  })
  
  observeEvent(input$reset, {
   clickRV$clickedMarker <- NULL
    })
  
  Bldg <- reactive({ clickRV$clickedMarker })
  
  availData <- reactive({
    if(is.null(Bldg())) {
      
     return(MonthAll %>% select(rev_mo, booked_days, avail_days) %>%
              gather(status, Room_Nights, 2:3) %>% filter(as.Date(rev_mo) <= max(ninetyseq))
     )
      
    } else {
      
      MonthDet %>% select(Bldg_Name, rev_mo, booked_days, avail_days) %>% 
        gather(status, Room_Nights, 3:4) %>% 
        filter(Bldg_Name == Bldg()$id, as.Date(rev_mo) <= max(ninetyseq))
    }
  })
  
  output$bldgplot <- renderPlotly({
    
      title <- ifelse(is.null(Bldg()$id), "All Buildings", Bldg()$id)
      
      MonRmDet <-  availData() %>%
        ggplot(aes(rev_mo, Room_Nights, text = paste("Month:", rev_mo, "<br>", status, ":", Room_Nights))) + 
        geom_col(aes(fill = status), position = "dodge") +
        scale_fill_manual(values = c("#878787", "#6AA6E2")) +
        theme_light() +
        theme(panel.grid = element_blank(),
              legend.title = element_blank()) +
        labs(title = paste("Monthly Available vs. Booked Days", title), y = NULL, x = NULL)
      
      ggplotly(MonRmDet, tooltip = c("text"))
  })
  
  bdrpdata <- reactive({
    if(is.null(Bldg())) {
      
     return(MonthAll %>% select(rev_mo, ADR, RevPar) %>%
                    gather(Metric, Dollars, 2:3) %>% filter(as.Date(rev_mo) <= max(ninetyseq)))
      
    } else {
      
      MonthDet %>% select(Bldg_Name, rev_mo, ADR, RevPar) %>%
        gather(Metric, Dollars, 3:4) %>%
        filter(Bldg_Name == Bldg()$id, as.Date(rev_mo) <= max(ninetyseq))
    }
  })
  output$bdrpplot <- renderPlotly({
    title <- ifelse(is.null(Bldg()$id), "All Buildings", Bldg()$id)
    
    MonRPDet <- 
      bdrpdata() %>% ggplot(aes(rev_mo, Dollars, text = paste(Metric, "$", round(Dollars), 
                                               "<br>Month", rev_mo))) + 
      geom_col(aes(fill = Metric), position = "dodge") +
      scale_fill_manual(values = c("#B89C76FE", "#A2CD5A")) +
      scale_y_continuous(labels = dollar, expand = c(0,0)) + 
      theme_light() +
      theme(panel.grid = element_blank(),
            legend.title = element_blank()) +
      labs(title = paste("Monthly RevPar vs. ADR", title), y = NULL, x = NULL)
    
    ggplotly(MonRPDet, tooltip= c("text"))
    
  })
  #heatmap of monthly occupancy by building
  output$Occupancy <- renderPlot({
    
    today <- ymd(cut(today(), "month"))
  
      DomSummary %>% filter(year < 2019) %>% group_by(Bldg_Name, rev_mo) %>%   
      summarize(occ = mean(occ_rate)) %>% ungroup() %>% 
      ggplot(aes(x = ymd(rev_mo, tz = ""), y = Bldg_Name, fill = occ, label = percent(occ))) +
      geom_tile(color = "white", size = 0.25, show.legend = FALSE) +
      geom_vline(data = today, xintercept = today, color = "grey", size = 6, alpha = 0.45) +
      geom_text(size=3) + 
      labs(x = NULL, y = NULL, title = "Monthly Occupancy Rates", subtitle = "by building")  + 
      scale_x_datetime(date_labels = "%b-%y", date_breaks = "month", position = "top", expand = c(0,0)) +
      scale_y_discrete() +
      scale_fill_gradient(low = "red", high = "green") +
      theme_light() +
      theme(axis.text.x = element_text(angle = 45, vjust = 3, size = 6, face = "bold"),
            axis.text.y = element_text(face = "bold"),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(), 
            plot.background = element_blank(),
            panel.border = element_blank(),
            axis.ticks = element_blank())
    
  })
  
  #reactive data for availbooked
  abdata <- reactive({
    if(input$cohort1 == "All") {
      DaysDF %>% group_by(listing_nickname, weekdate) %>% 
        summarize(AvailRooms = sum(AT, na.rm = T), 
                  BookedRooms = sum(BT, na.rm = T)) %>% ungroup() %>%
        group_by(weekdate) %>% summarize(AvailRooms = sum(AvailRooms, na.rm = T), 
                                         BookedRooms = sum(BookedRooms, na.rm = T)) %>% ungroup() %>%
        gather("status", "Rooms", AvailRooms:BookedRooms) %>% filter(year(weekdate) == 2018)
    } else {
      DaysDF %>% group_by(listing_nickname, weekdate) %>% 
        summarize(AvailRooms = sum(AT, na.rm = T), 
                  BookedRooms = sum(BT, na.rm = T)) %>% ungroup() %>%
        left_join(BldgMaster, by = "listing_nickname") %>% group_by(cohort, weekdate) %>% 
        summarize(Available = sum(AvailRooms, na.rm = T), 
                  Booked = sum(BookedRooms, na.rm = T)) %>% 
        ungroup() %>% 
        gather(key = "status", value = "Rooms", Available:Booked) %>% 
        filter(cohort == input$cohort1, year(weekdate) == 2018)
    }
      })
  #bars showing avail vs. booked filter by cohort
  output$availbooked <- renderPlotly({
  #   bldgavail %>% filter(year(weekdate) == 2018, !is.na(Bldg_Name), Bldg_Name == "Chroma SLU") %>% 
      AB <- abdata() %>% ggplot(aes(weekdate, Rooms, text = paste0("Days: ", Rooms, "<br>", "Week: ", weekdate))) +
      geom_col(aes(fill = status), position = "dodge") + 
      scale_fill_manual(values = c("grey", "#6AA6E2")) +
      scale_x_datetime(date_breaks = "month", date_labels = "%b", expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0), limits = c(0, max(abdata()$Rooms) + 75)) +
      theme_light() + 
      theme(axis.text.x = element_text(angle = -30),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            legend.title = element_blank()) + 
      labs(title = paste("2018 Available vs. Booked Rooms", input$cohort1), 
           subtitle = "2018 by week", x = NULL, y = NULL)
      
      ggplotly(AB, tooltip = "text")
   })
  
    output$abtable <- renderDataTable({
    DaysDF %>% group_by(listing_nickname, weekdate) %>% 
      summarize(AvailRooms = sum(AT, na.rm = T), 
                BookedRooms = sum(BT, na.rm = T)) %>% ungroup() %>%
      left_join(BldgMaster, by = "listing_nickname") %>% group_by(cohort, weekdate) %>% 
      summarize(Available = round(sum(AvailRooms)), na.rm = T, 
                Booked = round(sum(BookedRooms)), na.rm = T) %>% 
      ungroup() 
  })
  
  rpadrdata <- reactive({
    if(input$cohort1 == "All") {
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
      
      TotOcc %>% group_by(year, weekdate) %>% 
        summarize(ADR = sum(ADR), RevPar = sum(RevPar)) %>% 
        gather(Category, Val, ADR:RevPar) %>% 
        filter(year == 2018, epiweek(weekdate) < epiweek(now()))
      
    } else {
      WeekSum %>% select(cohort, year, weekdate, ADR, RevPar) %>% 
        gather(Category, Val, ADR:RevPar) %>% 
        filter(year == 2018, epiweek(weekdate) < epiweek(now()), cohort == input$cohort1)
    }
  })

  #bars showing ADR vs Revpar on Annual Performance Page
  output$revparadr <- renderPlotly({
   RPADR <- rpadrdata() %>%
      ggplot(aes(weekdate, Val, text = paste0(Category, ": ", dollar(Val), 
                                              "<br>Week: ", weekdate))) +
      geom_col(aes(fill = Category), position = "dodge") +
      scale_fill_manual(values = c("#B89C76FE", "#A2CD5A")) +
      scale_y_continuous(expand = c(0,0), labels = dollar) + 
      scale_x_datetime(date_breaks = "month", date_labels = "%b", expand = c(0,0)) +
      theme_light() +
      theme(axis.text.x = element_text(angle = -30),
            panel.grid.major.x = element_blank(),
            legend.title = element_blank()) + 
     labs(title = paste("2018 Weekly ADR vs. RevPar", input$cohort1), 
          subtitle = "2018 by week", 
          x = NULL, y = NULL)
    
    ggplotly(RPADR, tooltip = "text")
  })
  output$pradrtable <- renderDataTable({
    WeekSum %>% select(year, weekdate, cohort, ADR = adrchar, RevPar = rparchar) 
  })
  output$OccBldgRev <- renderPlot({
     
    RevOcc <- DomSummary %>% 
      group_by(Bldg_Name, year, rev_mo) %>% 
      summarize(Booked_Days = sum(days_booked),
                Available_Days = sum(days_avail),
                Occupancy_Rate = Booked_Days / Available_Days,
                ADR = round(mean(Avg_Rev)),
                Rev_PAR = round(mean(Rev_Par))) %>% ungroup() %>% select(Bldg_Name, year, rev_mo, Occupancy_Rate, ADR) %>%  
      gather(key = Metric, value = value, 4:5)
    
    #change text in the facet titles 
    RevOcc$Metric = factor(RevOcc$Metric, labels = c("Average Daily Revenue (ADR)", "Occupancy Rate"))
    #create a labeller for Facets
    facetlab <- function(x) {
      ifelse(x >= 10, paste("$", x),
             percent(x))
    }
    facetlab(RevOcc$value)
    
    #plot Average Revenue on top of occupancy rate
    RevOcc %>% filter(Bldg_Name == "Marina SLU") %>%
      ggplot(aes(x = ymd(rev_mo, tz = ""), y = value, group = Bldg_Name, color = Metric, fill = Metric)) +
      geom_smooth(se = F, show.legend = T) +
      geom_bar(stat = "identity", show.legend = T) +
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
  })
  
  #boxplot that explores prices by cohort and building
  output$boxplotprice <- renderPlot({
    
    RoomWt <- DomVel %>% group_by(cohort, Bldg_Name) %>% summarise(rooms = n_distinct(listing_nickname))
    boxmed <- DomVel %>% left_join(RoomWt, by = c("cohort", "Bldg_Name")) %>% group_by(Bldg_Name) %>% 
      summarise(med = median(ADR))
    
      DomVel %>% left_join(RoomWt, by = c("cohort", "Bldg_Name")) %>% 
      filter(year(rev_mo) == 2018) %>%
      ggplot(aes(x = Bldg_Name)) +
      geom_point(aes(y = ADR, color = cohort), position = "jitter", alpha = 0.25, show.legend = T) +
      geom_boxplot(aes(y = ADR, weight=rooms), color = "dark gray", varwidth = T) +
      geom_text(data = boxmed, aes(x=Bldg_Name, y = med, label = dollar(med)), position = "dodge") +
      scale_color_manual(values = DomCol11) + 
      scale_y_continuous(labels = dollar_format()) + 
      theme_light() +
      theme(panel.grid.major.x = element_blank(),
            axis.text.x = element_text(size = 10, face = "bold", angle = -30)) +
      labs(title = "Average Daily Revenue by Building", subtitle = paste(max(year(rev_mo)), "with median ADR labeled"), x = NULL, 
           caption = "Width of boxes adjusted by number of rooms in the building \nBox encloses 50% of ADR's\nMin, max and outliers shown by lines and grey dots\nColored dots show individual ADRs by booking")
  })
  
#boxplots showing ADR by source & cohort.
 output$sourceADR <- renderPlot({
   RoomWt <- DomVel %>% group_by(cohort, Bldg_Name) %>% summarise(rooms = n_distinct(listing_nickname))
   boxmed <- DomVel %>% left_join(RoomWt, by = c("cohort", "Bldg_Name")) %>% group_by(Bldg_Name) %>% 
     summarise(med = median(ADR))
   smed <- DomVel %>% left_join(RoomWt, by = c("cohort", "Bldg_Name")) %>% filter(!is.na(cohort)) %>%
     group_by(source, cohort) %>% 
     summarise(med = median(ADR))
   
   DomVel %>% 
     filter(days_in_advance >= 0, !is.na(cohort), 
            year(check_in_date) == 2018) %>%
     ggplot(aes(source, ADR)) +
     geom_point(aes(color = source),alpha = 0.35, position = "jitter", show.legend = F) +
     scale_color_manual(values = DomColSource) +
     geom_boxplot(aes(y = ADR), color = "#878787", varwidth = T) +
     geom_text(data = smed, aes(x=source, y = med, label = dollar(med))) +
     facet_grid(cohort~source, scales = "free_x") + 
     labs(title = "2018 ADR by Source of Booking", subtitle = "Median Value Labeled.",
          x = NULL) +
     scale_y_continuous(labels = dollar_format(), expand = c(0,0)) +
     theme_light() +
     theme(strip.text = element_text(face = "bold", size = 12),
           axis.text = element_text(size = 11),
           panel.grid.major.x = element_blank()
     ) 
 })
#plotly Gridded Barchart showing rooms avail vs. booked, by cohort on KPI Page

  output$roomsbk <- renderPlotly({
    
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
      theme(axis.text.x = element_text(angle = -30),
            panel.grid.major.x = element_blank()) + 
      labs(title = "Available vs. Booked Room Nights Next 3 Months", subtitle = "by week for next 3 months", x = NULL)
    
    ggplotly(gb)
    
  })

#reactive BARCHART showing bokings by chort each month, over time.
  output$sourcevel <- renderPlotly({
    TotBook <- DomVel %>% filter(year(rev_mo) <= year(now())) %>% group_by(rev_mo) %>% 
      summarize(booking = n(), revenue = dollar(sum(host_payout))) %>% ungroup()
    
    DS <- DomVel %>% filter(year(rev_mo) <= year(now())) %>% group_by_(input$CorS, "rev_mo") %>% 
      summarize(bookings = n()) %>% ggplot(aes(rev_mo)) + 
      geom_bar(stat = "identity", aes_string(y = "bookings", fill = input$CorS), show.legend = T) +
      scale_fill_manual(values = DomCol11) +
      scale_x_datetime(date_labels = "%b-%y", date_breaks = "month") +
      theme_light() + 
      theme(axis.text.x = element_text(angle = -30),
            panel.grid = element_blank()) +
      labs(title = "Bookings by Month", subtitle = "bookings = count of confirmed bookings checking in each month", 
           x = "Revenue Month") +
      geom_text(data = TotBook, aes(y = booking , label = booking),  vjust = -1)
    
    ggplotly(DS)
    
  })
  
  output$revevel <- renderPlotly({
      TotBook2 <- DomVel %>% filter(year(rev_mo) <= 2018) %>% group_by(rev_mo) %>% 
      summarize(booking = n(), revenue = round(sum(host_payout))) %>% ungroup()
    
    DR <- DomVel %>% filter(year(rev_mo) <= 2018) %>% group_by_(input$CorS, "rev_mo") %>% 
      summarize(bookings = n(), revenue = sum(host_payout)) %>% 
      ggplot(aes(rev_mo, text = paste0(input$Cors, "<br>Revenue:", dollar(revenue)))) + 
      geom_bar(stat = "identity", aes_string(y = "revenue / 1000", 
                                             color = input$CorS, 
                                             fill = input$CorS), show.legend = F) +
      scale_color_manual(values = DomCol11) +
      scale_fill_manual(values = DomCol11) +
      scale_x_datetime(date_labels = "%b-%y", date_breaks = "month") +
      theme_light() + 
      theme(axis.text.x = element_text(angle = -30),
            panel.grid = element_blank()) +
      geom_text(data = TotBook2, aes(y = revenue / 1000 , 
                                    label = paste(dollar(round(revenue / 1000)), "K", sep = ""), 
                                    angle = 90)) + 
      scale_y_continuous(labels = dollar) +
      labs(title = "Revenue by Month", subtitle = "Revenue = sum of host payout", 
           x = "Revenue Month", y = "$ Thousands") 
    
    ggplotly(DR, tooltip = "text")
    
  })

  
  #YTD and Monthly KPIs - boxes.
  output$YTDRev <- renderValueBox({
    valueBox(
      dollar(round(YTDSummary$YTDRev)),
      "YTD Total Revenue", color = "purple"
        )
      })
  
  output$YTDOcc <- renderValueBox({
    valueBox(
      percent(YTDSummary$YTDOcc, trim = T),
      "YTD Occupancy Rate", color = "purple"
    )
  })
  
  output$YTDADR <- renderValueBox({
    valueBox(
      dollar(round(YTDSummary$YTDADR)),
      "YTD Average Daily Revenue", color = "purple"
    )
  })
  
  output$YTDRevPar <- renderValueBox({
    valueBox(
      dollar(round(YTDSummary$YTDRevPar)),
      "YTD Average Revenue Per Available Unit", color = "purple"
    )
  })
 
  output$MoRev <- renderValueBox({
    valueBox(
      CurMoSummary$MoRev,
      paste(month(now(), label = T, abbr = F),"Total Revenue"), color = "fuchsia"
    )
  })
  
  output$MoOcc <- renderValueBox({
    valueBox(
      CurMoSummary$MoOcc,
      paste(month(now(), label = T, abbr = F),"Occupancy Rate"), color = "fuchsia"
    )
  })
  
  output$MoADR <- renderValueBox({
    valueBox(
      CurMoSummary$MoADR,
      paste(month(now(), label = T, abbr = F),"Average Daily Revenue"), color = "fuchsia"
    )
  })
  output$MoRevPar <- renderValueBox({
    valueBox(
      CurMoSummary$MoRevPar,
      paste(month(now(), label = T, abbr = F),"Average RevPar"), color = "fuchsia"
    )
  })
  
  ##regression analysis testing whether price impacted by when the booking is made.
  output$PriceReg <- renderPlot({
    DomVel %>% filter(days_in_advance >= 0, !is.na(cohort)) %>%
      ggplot(aes(days_in_advance, ADR, color = cohort)) +
      geom_point(alpha = 0.35, position = "jitter", show.legend = T) +
      geom_smooth(method = "lm", formula = y~x, se = F, na.rm = T, color = "gray") + 
      facet_grid(.~cohort, scales = "free_x") + 
      scale_color_manual(values = DomCol11) +
      labs(title = "Price Regression Model", 
           subtitle = "Is price impacted by how far in advance the booking is made?",
           x = "days in advance") +
      scale_y_continuous(labels = dollar_format(), expand = c(0,0)) +
      theme_light() +
      theme(strip.text = element_text(face = "bold", size = 12),
            axis.text = element_text(size = 11))
    
  })  
  
  #heatmap Summary of Occupancy rates by cohort.
  output$WeekOcc <- renderPlot({
    WST %>% filter(!is.na(cohort)) %>% 
      ggplot(aes(weekdate, cohort, fill = occ)) +
      geom_tile(show.legend = F) +
      geom_text(aes(label = occhar), size = 4) +
      scale_fill_gradient(low = "dark gray", high = "#885EA8") +
      scale_x_datetime(date_breaks = "week", date_labels = "%b %d", expand = c(0,0), position = "top") +
      theme_light() +
      labs(title = NULL, y = NULL, x = NULL) +
      theme(axis.text.x = element_text(size = 12, face = "bold"),
            axis.text.y = element_text(face = "bold"),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(), 
            plot.background = element_blank(),
            panel.border = element_blank(),
            axis.ticks = element_blank())
  })
  output$dlweekocc <- downloadHandler(
    filename = "weekly_occupancy.csv",
    content = function(file) {
      write.csv(WeekOcc, file, row.names = F)
    }
  )
  #projected occupancy summary bar and line graph.
  output$occproj <- renderPlotly({
    totrms <- DomVel %>% summarise(rooms = n_distinct(listing_nickname))
    WeightedRooms <- DomVel %>% group_by(cohort, Bldg_Name) %>% 
      summarise(rooms = n_distinct(listing_nickname)) %>% ungroup() %>%
      group_by(cohort) %>% summarize(rooms = sum(rooms), rmwt = sum(rooms / totrms)) %>% ungroup() 
    
  COHOCC <-  WeekSum %>% left_join(WeightedRooms, by = "cohort") %>%
      mutate(wtdocc = occ * rmwt) %>%
      filter(year(weekdate) %in% c(year(now()), year(now()) + 1), 
             as.Date(weekdate) %in% ninetyseq, 
             !is.na(cohort)) %>% 
      select(cohort, weekdate, occ, wtdocc) %>%
      ggplot(aes(weekdate, occ)) +
      geom_col(aes(fill = cohort), position = "dodge") +
      scale_x_datetime(date_breaks = "week", date_labels = "%b %d", expand = c(0,0)) +
      scale_y_continuous(labels = percent, expand = c(0,0), limits = c(0, 1.00)) + 
      scale_fill_manual(values = DomCol11) + 
      theme_light() + 
      theme(panel.grid = element_blank(),
            legend.position = "top",
            legend.direction = "horizontal",
            legend.title = element_blank()) +
      labs(y = NULL, x = NULL)
  ggplotly(COHOCC)
  })
  #detailed heatmap of occupancy rates by listing next 90 days.
  output$OccOut <- renderPlot({
    WeekOcc %>% filter(year(weekdate) %in% c(year(now()), year(now()) + 1), 
                       as.Date(weekdate) %in% ninetyseq, 
                       cohort == input$coh1) %>% 
      ggplot(aes(weekdate, listing_nickname, fill = occ_rate)) +
      geom_tile(show.legend = F) +
      geom_text(aes(label = percent(occ_rate)), size = 4) +
      scale_fill_gradient(low = "dark gray", high = "#885EA8") +
      scale_x_datetime(date_breaks = "week", date_labels = "%b %d", expand = c(0,0), position = "top") +
      theme_light() +
      labs(title = paste("Occupancy Outlook", input$coh1), y = NULL, x = NULL) +
      theme(axis.text.x = element_text(size = 12, face = "bold"),
            axis.text.y = element_text(face = "bold"),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(), 
            plot.background = element_blank(),
            panel.border = element_blank(),
            axis.ticks = element_blank())
  })
  
  #BarChart showing revenue projections by week, next 3 months
    output$revproj <- renderPlotly({
    Tot <- WeekSum %>%filter(year(weekdate) %in% c(year(now()), year(now()) + 1), 
                             as.Date(weekdate) %in% ninetyseq) %>%
      group_by(weekdate) %>% summarise(SumRev = sum(TotRev), TRChar = dollar(round(SumRev))) %>% ungroup()
    
   RPROJ <- WeekSum %>% mutate(cohort = factor(cohort)) %>% 
      filter(year(weekdate) == 2018, 
             month(weekdate) >= month(now()), month(weekdate) <= month(now()) + 2) %>% 
      ggplot(aes(x = weekdate)) +
      geom_bar(aes(y = TotRev, fill = cohort), stat = "identity", show.legend = T) +
      geom_text(data = Tot, aes(y = SumRev, label = dollar(round(SumRev))), vjust = -1) + 
      scale_x_datetime(date_breaks = "week", date_labels = "%b %d", expand = c(0,0)) +
      scale_y_continuous(limits = c(0, max(Tot$SumRev)+10000), labels = dollar, expand = c(0,0)) +
      scale_fill_manual(values = DomCol11) +
      theme_light() + 
      theme(axis.text.x = element_text(angle = -30),
            legend.position = "top",
            legend.direction = "horizontal",
            legend.title = element_blank(),
            panel.grid = element_blank()) +
      labs(x = NULL, y = NULL)
    
 ggplotly(RPROJ)
  })
  #Small heatmap showing booked revenue by room, rolled up by cohort/total.
  output$WeekRev <- renderPlot({
    
    WST %>% filter(!is.na(cohort)) %>%
      ggplot(aes(weekdate, cohort, fill = ADR)) +
      geom_tile() + 
      geom_text(aes(label = adrchar), size = 4) + 
      scale_fill_gradient(low = "dark gray", high = "#FF6347", limits = c(0, max(WeekSum$ADR) + 50)) +
      scale_x_datetime(date_breaks = "week", date_labels = "%b %d", expand = c(0,0), position = "top") +
      theme_light() +
      labs(title = "ADR by Cohort", y = NULL, x = NULL) +
      theme(axis.text.x = element_text(size = 12, face = "bold"),
            axis.text.y = element_text(face = "bold"),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(), 
            plot.background = element_blank(),
            panel.border = element_blank(),
            axis.ticks = element_blank())
  })
  #detailed heat map showing room level detail avg revenue/bookings for next 3 months.
  output$RevOut <- renderPlot({
    WeekOcc %>% filter(year(weekdate) %in% c(year(now()), year(now()) + 1), 
                       as.Date(weekdate) %in% ninetyseq, 
                      cohort == input$coh2) %>% 
      ggplot(aes(weekdate, listing_nickname, fill = ADR)) +
      geom_tile() + 
      geom_text(aes(label = dollar(ADR)), size = 5) + 
      scale_fill_gradient(low = "dark gray", high = "#FF6347") +
      scale_x_datetime(date_breaks = "week", date_labels = "%b %d", expand = c(0,0), position = "top") +
      theme_light() +
      labs(title = paste("ADR Outlook for", input$coh2), y = NULL, x = NULL) +
      theme(axis.text.x = element_text(size = 6, face = "bold"),
            axis.text.y = element_text(face = "bold"),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(), 
            plot.background = element_blank(),
            panel.border = element_blank(),
            axis.ticks = element_blank()) 
  })
  
  #table for raw data page

rawData <- reactive({
  
  if(input$datachoice == "DomBookings") {
    
    Data <- DomBookings %>% select(confirmation_code, source, status, Bldg_Name, listing_nickname, check_in_date, check_out_date,
                                   host_payout, num_nights, ADR, created_at, days_in_advance) 
   subset(Data, check_in_date >= input$daterange[1] & check_in_date <=input$daterange[2])
  } else { 
    WeekOcc %>% select(neighborhood, cohort, Bldg_Name, listing_nickname, year:RevPar) %>% 
      filter(weekdate >= input$daterange[1] & weekdate <= input$daterange[2])
} 
  
})
  output$bookingtable <- renderDataTable({
  
    rawData()
  })
  #data table supplied when download button is clicked.
  output$dlbooking <- downloadHandler(
    filename = function() {
      paste(input$datachoice, ".csv", sep = "")
    },
    
    content = function(file) {
        write.csv(rawData(), file, row.names = F)
    }
  )
})
 

shinyApp(ui = ui, server = server)

