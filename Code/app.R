# RShiny app for displaying CanFlux map
# Rosie Howard
# 16 October 2025
# Based on https://github.com/norlab/ameriflux-analysis (displayed here:https://bree.shinyapps.io/Canadian-AmeriFlux-Sites/) by Bree Norlander

# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(mailtoR)
library(shinythemes)
library(readr)
library(ggplot2)
library(shinythemes)
library(tidyr)
library(dplyr)
library(janitor)
library(stringr)
library(DT)
library(leaflet)
library(htmltools)
library(shinydashboard)
library(plotly)
library(lubridate)

# Start define rowCallback function
# This is to allow NAs to appear in datatables
# see https://stackoverflow.com/a/58526580/5593458
rowCallback <- c(
  "function(row, data){",
  "  for(var i=0; i<data.length; i++){",
  "    if(data[i] === null){",
  "      $('td:eq('+i+')', row).html('NA')",
  "        .css({'color': 'rgb(151,151,151)', 'font-style': 'italic'});",
  "    }",
  "  }",
  "}"
)
# End define rowCallback function

# Read in datasets - NOT needed for map function only.

# need to somehow read data in... run RShiny vizualization code first and save/point to output data for that database (temporarily using "CanPeat" data so it works)
# This only works locally - cannot publish Shiny app from this (later use cloud or other on McGill server?)
#load("/Users/rosie/Documents/Micromet/CANFLUX_Database/canflux-summary/Data/data_tmp/all_data.RData")  # code must have been run first to create this data set

# ***Eventually site list from loaded data above and site list from Site Information page (tsv file) may match!***
# Temporary solution: rename map sites as sites_map so previous site list is not overwritten
# sites_map <- read_tsv(file = "https://raw.githubusercontent.com/rosiehoward/canflux-summary/refs/heads/main/Data/AmeriFlux-sites-Canadian-ForApp.tsv", show_col_types = FALSE)
sites_map <- read_tsv(file = "https://raw.githubusercontent.com/rosiehoward/canflux-summary-map/refs/heads/main/Data/Canadian-Flux-Sites_DownloadedFromGoogleSheet3Oct2025_withLatLons.tsv", show_col_types = FALSE)

# Filter to just sites in Canada
#sites_map <- sites_map |> 
#  filter(Country == 'Canada') 

# Add some new columns
#sites <- sites |> 
#  mutate(first_year_of_data = str_split(`Years of AmeriFlux BASE Data`, ",", simplify = TRUE)[ , 1]) |> 
#  mutate(last_year_of_data = str_split(`Years of AmeriFlux BASE Data`, ",", simplify = TRUE)[ , -1])


# Assign variables for use within a text block
total_sites <- sites_map |> 
  summarise(x = n_distinct(`Site Name`)) |> 
  pull()

# total_sites_no_end <- sites_map |> 
  # filter(is.na(`Site End`) | as.numeric(`Site End`) > 2023) |> 
  # summarise(x = n_distinct(`Site ID`)) |> 
  # pull()

total_pi <- sites_map |> 
  summarise(x = n_distinct(`Principal Investigator`)) |> 
  pull()

# total_pi_no_end <- sites_map |> 
  # filter(is.na(`Site End`) | as.numeric(`Site End`) > 2023) |> 
  # summarise(x = n_distinct(`Principal Investigator`)) |> 
  # pull()

# Define the labels for the leaflet map
# See https://stackoverflow.com/a/43155126
labs <- lapply(seq(nrow(sites_map)), function(i) {
  paste0('<b>Site ID:</b> ', 
          sites_map[i, "Site ID"], 
          '<br><b>Site Name:</b> ', 
          sites_map[i, "Site Name"],
           '<br><b>Lat (deg):</b> ', 
          sites_map[i, "Lat (deg)"],
           '<br><b>Lon (deg):</b> ', 
          sites_map[i, "Lon (deg)"],
           '<br><b>Elev (m):</b> ', 
          sites_map[i, "Elevation (m)"],
          # '<br><b>Clean Data:</b> ', 
          #sites_map[i, "Years of AmeriFlux BASE Data"],
          '<br><b>PI:</b> ', 
          sites_map[i, "Principal Investigator"])
})

# colour palette for site markers on map
# pal <- colorFactor(
#   # Use a predefined palette:
#   # palette = "Dark2",
#   # 
#   # Or specify individual colors:
#   palette = c("purple", "orange"),
#   domain = sites_map
# )

# Define UI for application
#browser()
#ui <- fluidPage(theme = shinytheme("flatly"),
ui <- dashboardPage(skin = 'black', # Begin UI 
                    
                    dashboardHeader(title = "CanFlux Sites"),

                    dashboardSidebar(sidebarMenu(
                      menuItem("Site Information", tabName = "siteinfo"),
                      #menuItem("Individual sites", tabName = "indiv"),
                      #menuItem("All sites", tabName = "all"),
                      menuItem("About", tabName = "about")
                    )), # End dashboard sidebar

         dashboardBody( # Begin dashboard body

                      # Suppress warnings
                      tags$style(type="text/css",
                                 ".shiny-output-error { visibility: hidden; }",
                                 ".shiny-output-error:before { visibility: hidden; }"),

                      tabItems(
                        tabItem( # Begin 'Site Information' page
                          h1('Site Information'),

                          tabName = "siteinfo",

                          # 1. Site information (map and table)

                          fluidRow(br()),# End fluidrow 
                          fluidRow(column(12,
                                  p(paste0("Sample of ", total_sites, " Canadian eddy-covariance research sites for which 
                                            data has been collected at any time. This sample represents flux sites across Canada 
                                            associated with approximately ", total_pi, " Principal Investigators. Further sites and information will be added as data becomes available.")),
                                  # p(paste0("Note that data included here is the best available at time of publishing and contains uncertainties. If you notice any errors or have site information to add, please contact: "), 
                                  #           mailtoR(email = "ecofluxlab@gmail.com", text = "ecofluxlab@gmail.com", subject = "CanFlux Metadata Inquiry"),"."),
                          )),# End fluidrow 
                          fluidRow(column(8,
                                 h2("Map View")
                          ),
                          column(4)
                          ), # End fluidrow
                          fluidRow(column(12, 
                                           wellPanel(
                                             leafletOutput(outputId = "Map",height = 600)
                                              # leafletOutput(outputId = "Map",height = 600,width = 1050)
                                          )
                          ) # End wellPanel
                          ), # End fluidrow
                          fluidRow(column(8,
                                          h2("Table View")
                          ),
                          column(4)
                          ), # End fluidrow
                          fluidRow(column(12,
                                          DT::dataTableOutput(outputId = "CanFlux_Table")
                          )
                          ), # End fluidRow
                          
                          # Start footer
                          fluidRow(br()),
                          fluidRow(column(12,
                                          tags$footer(
                                            p(paste0("Note that data included here is the best available at time of publishing and contains uncertainties. If you notice any errors or have site information to add, please contact: ecofluxlab (at) gmail (dot) com.")),
                                            class = "footer"
                                          )
                          )
                          )
                          # End footer
                          
                        ),  # End 'Site Information' page (end MAP)

                        tabItem( # Begin 'About' page
                        h1('About'),
                        tabName = "about",

                        h3('Data Sources'),
                        h4(style="text-align: justify;",
                           "The metadata presented in the Site Information display were gathered directly from the ",
                           tags$a(href = 'https://ameriflux.lbl.gov/sites/site-search/#', 'AmeriFlux Site Search', target="_blank"),
                           " table if available, otherwise from PIs, Data Managers, or other pers. comms.. Collection began in August 2025. 
                           Data included here is the best available at time of publishing and contains uncertainties. 
                           If you notice any errors or have site information to add, please contact: ecofluxlab (at) gmail (dot) com."
                                      ),
                           br(),
                           h3('Acknowledgements'),
                           h4(style="text-align: justify;",
                              'The Site Information display is based on code from ', tags$a(href = 'https://github.com/norlab/ameriflux-analysis', 'https://github.com/norlab/ameriflux-analysis', target = 'blank'),'. 
                               Page compiled by Rosie Howard, 2025.'),

                         ))
         )  # End dashboard body 
        # )  # End main panel
 ) # End UI


# Define server logic required 
# server <- function(input, output, session) {
server <- function(input, output) {

     # Begin Create NY5Z map
     # Disabled scroll wheel option. Instuctions a combo from https://gis.stackexchange.com/a/54925
     # and https://gis.stackexchange.com/a/231632
     output$Map <- renderLeaflet({
       leaflet(sites_map %>%
                 distinct(`Site ID`, `Site Name`, `Lat (deg)`, `Lon (deg)`),
               options = leafletOptions(scrollWheelZoom = TRUE)) %>%
               # options = leafletOptions(scrollWheelZoom = FALSE)) %>%
         addTiles() %>%
         addMarkers(lng = ~`Lon (deg)`,
                    lat = ~`Lat (deg)`,
                    label = lapply(labs, htmltools::HTML),
                    clusterOptions = markerClusterOptions()
                    )

     })
     # End Create NY5Z map

    # Start create CanFlux_Table table
     output$CanFlux_Table <- DT::renderDataTable({
       CanFlux_Table_DT_object <- sites_map %>%
         select(`Site ID`, `Site Name`,`Lat (deg)`,`Lon (deg)`,`Elevation (m)`,`Principal Investigator`,`Site Start`,`Site End`,`Fluxes`)
       # DT info: https://datatables.net/reference/option/dom https://rstudio.github.io/DT/
       datatable(
         CanFlux_Table_DT_object,
         rownames = F,
         # bootstrap style must be added when using a shinytheme https://stackoverflow.com/a/60948767
         style = "bootstrap",
         options = list(dom = 'lftipr', rowCallback = JS(rowCallback)) # This adds in null values
        ) #%>%

     })   # End create CanFlux_Table table

} # End server

# Run the application 
shinyApp(ui = ui, server = server)
