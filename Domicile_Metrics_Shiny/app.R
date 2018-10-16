#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(lubridate)
library(DBI)
library(scales)
library(forcats)
library(dplyr)
library(DT)
library(plotly)


load("/Users/bari/R_files/Domicile/DomProject/Domicile_Metrics_Shiny/dommaster.RData")
load("/Users/bari/R_files/Domicile/DomProject/Domicile_Metrics_Shiny/BookingMaster.RData")
load("/Users/bari/R_files/Domicile/DomProject/Domicile_Metrics_Shiny/DomSummary.RData")

#Color Palette
DomColor <- c("#8B8B00", "#8B636C", "#EE5C42", "#36648B", "#EE9A00", "#5C5C5C", "#CD6839", "#00868B", "#FF4500", "#4F94CD", "#8B2252", "#EEEE00")
DomCol11 <- c("#8B668B", "#6CA6CD", "#FF6347", "#A2CD5A", "#878787", "#CD853F", "#36648B", "#FFC125", "#fb9a99", "#53868B", "#8B795E")
ListingNicknames <- unique(dommaster$listing_nickname)

BuildingNames <- unique(dommaster$Bldg_Name)

RevParAll <- DomSummary %>% filter(rev_mo <= today())

RevParTable <- DomSummary %>% mutate(rev_mo = month(ymd(rev_mo)), year = round(year)) %>%
  group_by(Bldg_Name, year, rev_mo) %>% 
  summarize(Booked_Days = sum(days_booked),
            Available_Days = sum(days_avail),
            Occupancy_Rate = percent(Booked_Days / Available_Days),
            Average_Rev_Night = dollar(round(mean(Avg_Rev))),
            Rev_PAR = dollar(round(mean(Rev_Par)))) %>% ungroup() %>%
  gather(key = Metric, value = value, Booked_Days:Rev_PAR) %>% spread(rev_mo, value, fill = 0)

RevOcc <- DomSummary %>% mutate(rev_mo = ymd(rev_mo, tz = "")) %>%
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


today <- ymd(cut(today(), "month"), tz = "")

domdetail <- dommaster %>% mutate(duration = difftime(check_out_date, check_in_date, "days"), 
                                  date = ymd(cut(check_in_date, "month"), tz = ""),
                                  booked = ifelse(status == "confirmed", 1, 0)) 


Summary <- DomSummary %>% filter(year == 2018) %>%
  group_by(year, rev_mo) %>% 
  summarize(Booked_Days = sum(days_booked),
            Available_Nights = sum(days_avail),
            OpenNights = Available_Nights - Booked_Days,
            Occupancy_Rate = Booked_Days / Available_Nights,
            Average_Rev_Night = round(mean(Avg_Rev)),
            Rev_PAR = round(mean(Rev_Par))) %>% ungroup() %>% filter(month(rev_mo) == month(today))    


ui <- fluidPage(
   # Application title
   titlePanel("domicile performance metrics"),
   tabsetPanel(
     tabPanel("Revenue Per Available Unit",
              fluidRow(
                column(12, plotOutput("RevPerMo"))
              ),
              
              wellPanel(
              fluidRow(
          column(3, selectInput("Bldg1",
                                "Select Building",
                                c("all", BuildingNames)
                                )
                 ),
          column(3,  selectInput("Year1",
                                 "Select Year for Table",
                                 c(2016, 2017, 2018, 2019))
                 )
          )),
        fluidRow(
          column(12, plotOutput("RevParPlot"))
              ),
        fluidRow(
          column(12, tableOutput("SummaryRevPar")
        ))
        ),
     
 tabPanel("Building Occupancy",
          fluidRow(
            column(12, 
              plotOutput("OccHeatMap"))
          ),
             wellPanel(
              
         
    fluidRow(
     column(3, 
            br(),
            br(),
            selectInput("Bldg2",
                           "Select Building",
                           c(BuildingNames)))
                  )),
   fluidRow(
     column(7,
            plotOutput("PlotOccRev")
     
          ),
     column(5, 
            h2( paste(month(today, label = T, abbr = F)), "Domicile Totals"),
            h4(paste("Nights Booked.........", Summary$Booked_Days)),
            h4(paste("Open Nights.............", Summary$OpenNights)),
            h4(paste("Total In Month .........", Summary$Available_Nights)),
            h4(paste("Occupancy Rate........", percent(Summary$Occupancy_Rate))),
            h4(paste("Average Nightly Rate...", dollar(Summary$Average_Rev_Night))),
            h4(paste("Rev_Par....................", dollar(Summary$Rev_PAR)))
           
     )),
  fluidRow(
    column(12, 
           tableOutput("BldgPerfTab")
  ))
   
   )))

server <- function(input, output) {
  
  
  RevParColor <- reactive({
    if(input$Bldg1 != "all") {
      
    }
  })
  RevParData <- reactive({
          if(input$Bldg1 != "all") {
            RevParAll %>% filter(Bldg_Name == input$Bldg1)
          } else {
            RevParAll
          }
  })
  
  output$RevParPlot <- renderPlot({
 
         
      

  })
  
  output$RevPerMo <- renderPlot({
    
    TotalRev <- domdetail %>% group_by(status, date) %>% filter(status == "confirmed") %>% 
      summarise(revenue = sum(host_payout)) %>% ungroup()
    
    domdetail %>% group_by(status, neighborhood, date) %>% 
      summarize(booking = sum(booked),
                revenue = sum(host_payout)) %>% ungroup() %>% filter(status == "confirmed") %>%
      ggplot(aes(x = date) ) + 
      geom_bar(aes(y = revenue, col = neighborhood, fill = neighborhood), stat = "identity") +
      geom_text(data = TotalRev, aes(x = date, y = revenue, label = paste(dollar(round(revenue / 1000)),"K")), 
                stat = "identity", angle = 90, hjust = -0.2) +
      scale_y_continuous(labels = dollar_format(), expand = c(0, 0), limits = c(0, 500000)) +
      scale_color_manual(values = DomCol11) +
      scale_fill_manual(values = DomCol11) + 
      ggtitle("Total Revenue per Month") + 
      labs(x = NULL, y = NULL) +
      theme_light() + 
      theme(axis.text = element_text(face = "bold", size = 10),
            plot.background = element_rect(colour = "white", fill = "white"),
            panel.grid = element_blank())
    
  })
  
  SumData <- reactive({
    
    if(input$Bldg1 != "all") {
      RevParTable %>% filter(Bldg_Name == input$Bldg1, year == input$Year1)
    } else { 
      RevParTable %>% filter(year == input$Year1)}
    
    })
  
  
  output$SummaryRevPar <- renderTable({ 
    SumData()}, striped = TRUE, digits = 0
 
      )
  
      output$selected_Bldg2<- renderText( {
    paste("Occupancy Rates for", input$Bldg2)
  })
   
  output$OccHeatMap <- renderPlot({
 
   
    
    DomSummary %>% filter(year < 2019) %>% group_by(Bldg_Name, rev_mo) %>%  
      summarize(occ = mean(occ_rate)) %>% ungroup() %>% 
      mutate(rev_mo = ymd(rev_mo, tz="")) %>%
      ggplot(aes(x = rev_mo, y = Bldg_Name, fill = occ, label = percent(occ))) +
      geom_tile(color = "white", size = 0.25, show.legend = FALSE) +
      geom_vline(data = today, xintercept = today, color = "grey", size = 6, alpha = 0.45) +
      geom_text(size=3) + 
      labs(x = NULL, y = NULL, title = "Monthly Occupancy Rates", subtitle = "by building")  + 
      scale_x_datetime(date_labels = "%b %y", date_breaks = "month", position = "top", expand = c(0,0)) +
      scale_y_discrete() +
      scale_fill_gradient(low = "red", high = "green") +
      theme_light() +
      theme(axis.text.x = element_text(angle = -30, vjust = 6, size = 10, face = "bold"),
            axis.text.y = element_text(face = "bold", size = 12),
            title = element_text(face = "bold", size = 13),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(), 
            plot.background = element_blank(),
            panel.border = element_blank(),
            axis.ticks = element_blank())
    
      })
  
  output$PlotOccRev <- renderPlot({
    #change text in the facet titles 

     RevOcc$Metric = factor(RevOcc$Metric, labels = c("Average Revenue Per Night", "Occupancy Rate"))
    #create a labeller for Facets
    facetlab <- function(x) {
      ifelse(x >= 10, paste("$", x),
             percent(x))
    }
    #plot Average Revenue on top of occupancy rate

 RevOcc %>% filter(Bldg_Name == input$Bldg2) %>%
      ggplot(aes(x = rev_mo, y = value, group = Bldg_Name, color = Metric, fill = Metric)) +
      geom_bar(stat = "identity", show.legend = T) +
      geom_smooth(se = F, show.legend = T) +
      facet_wrap(~Metric, scale = "free_y", ncol = 1) +
      labs(x = NULL, y = NULL) +
      scale_x_datetime(date_labels = "%b-%y", date_breaks = "month",  date_minor_breaks = "year", expand = c(0,0)) +
      scale_y_continuous(labels = facetlab) +
      scale_color_manual(values = c("#6AA6E2", "#885EA8")) +
      scale_fill_manual(values = c("#6AA6E2", "#885EA8")) +
      labs(title = paste("Revenue Per Night vs. Occupancy for ", input$Bldg2 )) +
      theme_light() +
      theme(strip.background = element_rect(fill = "#969696"),
            axis.text.x = element_text(angle = -30, size = 12, face = "bold"),
            axis.text.y = element_text(size = 12, face = "bold"),
            strip.text = element_text(color = "white", face = "bold", size = 12),
            panel.grid.major.x = element_blank())

  })
  
  output$BldgPerfTab <- renderTable({
  RevParTable %>% filter(Bldg_Name == input$Bldg2)  
  }, digits = 0, striped = T)
}

# Run the application 
shinyApp(ui = ui, server = server)

