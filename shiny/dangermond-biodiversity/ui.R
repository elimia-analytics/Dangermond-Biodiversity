#' ---
#' title: Dangermond Preserve Biodiversity Portal - UI Code
#' ---
#'
#' # Server setup
#' ## Load libraries
library(shiny)
library(shinydashboard)
library(shinyBS)
library(leaflet)
library(leaflet.extras)
library(purrr)
library(shinyjs)
library(sf)
library(shinycssloaders)
library(dygraphs)
library(plotly)
library(readr)
library(DT)

scr <- tags$script(HTML(
  "
Shiny.addCustomMessageHandler(
  'removeleaflet',
  function(x){
    console.log('deleting',x)
    // get leaflet map
    var map = HTMLWidgets.find('#' + x.elid).getMap();
    // remove
    map.removeLayer(map._layers[x.layerid])
  })
"
))

## Load integrated species occurrence data from GitHub repository
integrated_dangermond_occurrences <- read_csv("https://raw.githubusercontent.com/elimia-analytics/Dangermond-Biodiversity/main/data/integrated_occurrences_dangermond.csv")
### Identify dropdown taxon names
records_species_names <- integrated_dangermond_occurrences$scientificName %>% unique() %>% sort()
### Identify dropdown status ranks
records_status_names <- list(
  "California State Rank" = integrated_dangermond_occurrences$California_srank %>% unique() %>% sort(),
  "NatureServe Global Rank" = integrated_dangermond_occurrences$grank %>% unique() %>% sort(),
  "US ESA Status" = integrated_dangermond_occurrences$esa %>% unique() %>% sort(),
  "IUCN Red List Status" = integrated_dangermond_occurrences$iucn %>% unique() %>% sort()
)
  
#' # User Interface
navbarPage(title = HTML("<span style='float: left; display: inline-block; padding-left: 20px;'>
                        <img src = 'tnc_logo.svg', height = '45'></span><span style='display: inline-block; padding: 5px 5px 5px 15px;'>
                        <h1 style = 'font-size: 28px; padding-right: 20px;'><strong>Dangermond Preserve Biodiversity Portal</strong></h1></span>
                        "), collapsible = TRUE, 
           windowTitle = "Dangermond Preserve Biodiversity Portal", 
           id="nav", theme = "style.css",
           
           tabPanel("Application", height = "100%",
                   
                    useShinyjs(),     ## Call to use shinyJS
                    
                    scr,
                    
                    tags$head(
                      HTML("<link href='https://fonts.googleapis.com/css2?family=Roboto&display=swap' rel='stylesheet'>"),
                      HTML("<link href='https://fonts.googleapis.com/css2?family=Cabin&display=swap' rel='stylesheet'>"),
                      HTML("<meta name='viewport' content='width=device-width, initial-scale=1'>")
                    ),
                    
           div(class="outer",
               
               fluidRow(style = "padding: 20px 0 0 20px;",
                        column(width = 8, 
                               fluidRow(
                                 leafletOutput("main_map", height = "50vh"),
                               ),
                               fluidRow(style = "padding-top: 20px;",
                                        column(width = 8, style = "padding-left: 0;",
                                               tabsetPanel(id = "metric_switch", type = "pills",
                                                           tabPanel("Records", height = "100%",
                                                           ),
                                                           tabPanel("Species", height = "100%",
                                                           )
                                               )
                                               ),
                                        column(width = 4, style = "padding-left: 0;",
                                               span(h4(em("Database last updated on Aug 25, 2024")), style = "float: right; padding: 3px 0px 7px 10px;")
                                        )
                               ),
                               fluidRow(style = "overflow-x: scroll;",
                                     shinycssloaders::withSpinner(DT::dataTableOutput("records_table"), type = 4)
                               )
                               
                        ),
                        column(width = 4,
                               h3('Number of Records Over Time', style = "padding: 0 0 0 10px; margin-top: 0;"),
                               fluidRow(style = "padding: 0px 20px 10px 20px;", shinycssloaders::withSpinner(dygraphOutput("time_plot", height = "47vh"), type = 4)),
                               fluidRow(
                                 column(width = 6,
                                        fluidRow(style = "padding-left: 20px;", h3("Select Species", style = "color: #337AB8 !important; font-size: 15px;")),
                                        fluidRow(style = "padding: 20px 20px 10px 20px;", shiny::selectizeInput(inputId = "select_species", label = "", choices = records_species_names, multiple = TRUE))
                                        ),
                                 column(width = 6,
                                        fluidRow(style = "padding-left: 20px;", h3("Select Conservation Status", style = "color: #337AB8 !important; font-size: 15px;")),
                                        fluidRow(style = "padding: 20px 20px 10px 20px;", shiny::selectizeInput(inputId = "select_status", label = "", choices = records_status_names, multiple = TRUE))
                                        )
                               ),
                               fluidRow(style = "padding-left: 20px;", h3("Select Taxon", style = "color: #337AB8 !important; font-size: 15px;")),
                               fluidRow(style = "padding-bottom: 0;", shinycssloaders::withSpinner(plotlyOutput("taxa_donut", width = "100%", height = "100%"), type = 4)),
                               
                        )
               )
           ),
           
           absolutePanel(id = "cond_inputs_panel", 
                         class = "panel panel-default", 
                         top = 85, left = "auto", right = "35vw", bottom = "auto",
                         width = "6em",
                         height = "2.5em",
                         style = "margin: 0; padding: 0; box-shadow: none !important; border-bottom: none; border-color: transparent; background-color: rgba(169, 169, 169, 0); z-index: 1000 !important;", 
                         # span(style = "float: left; padding-right: 5px;",
                         #      actionButton(inputId = "redo_search", label = "Redo search in this area", class = "btn-primary btn-sm", width = "100%"),
                         # ),
                         span(style = "float: left;",
                              actionButton(inputId = "start_over", label = "Start over", class = "btn-primary btn-sm", width = "100%")
                         )
           )
           ),
           tabPanel("Documentation", height = "100%", 
                    div(style = "padding: 20px;",
                      tabsetPanel(id = "docu", type = "pills",
                            tabPanel("Workflow Basics", height = "100%",
                                     fluidRow(
                                       column(width = 6,
                                              h2(em("Click on red-colored squares to zoom to underlying records")),
                                              div(
                                                img(src = 'dangermond-app-gif1.gif', width = '90%'),
                                                style = "padding-bottom: 50px;"
                                              )
                                              ),
                                       column(width = 6,
                                              h2(em("Toggle between number of records or number of species summary")),
                                              div(
                                                img(src = 'dangermond-app-gif2.gif', width = '90%'),
                                                style = "padding-bottom: 50px;"
                                              )
                                              )
                                     ),
                                     fluidRow(
                                       column(width = 6,
                                              h2(em("Drag map to update all analytics in view")),
                                              div(
                                                img(src = 'dangermond-app-gif3.gif', width = '90%'),
                                                style = "padding-bottom: 50px;"
                                              )
                                       ),
                                       column(width = 6,
                                              h2(em("Explore single records by selecting them in the table")),
                                              div(
                                                img(src = 'dangermond-app-gif4.gif', width = '90%'),
                                                style = "padding-bottom: 50px;"
                                              )
                                       )
                                     ),
                                     fluidRow(
                                       column(width = 6,
                                              h2(em("Filter data by selecting individual species")),
                                              div(
                                                img(src = 'dangermond-app-gif5.gif', width = '90%'),
                                                style = "padding-bottom: 50px;"
                                              )
                                       ),
                                       column(width = 6,
                                              h2(em("Filter data by clicking through major taxonomic groupings")),
                                              div(
                                                img(src = 'dangermond-app-gif6.gif', width = '90%'),
                                                style = "padding-bottom: 50px;"
                                              )
                                       )
                                     )
                            ),
                            tabPanel("FAQ", height = "100%",
                                     h2(strong("What data are being displayed in this application?")),
                                     HTML(
                                       '<h3 style = "color: black;">This application displays biodiversity occurrences overlapping The Nature Conservancy’s Jack and Laura Dangermond Preserve, integrated from a number of sources: Global Biodiversity Information Facility (GBIF; <a href="gbif.org" target="_blank">gbif.org</a>), iNaturalist (<a href="inaturalist.org" target="_blank">inaturalist.org</a>), CalFlora (<a href="calflora.org" target="_blank">calflora.org</a>), and iDigBio (<a href="idigbio.org" target="_blank">idigbio.org</a>).</h3>
                                        <h3 style = "color: black;">To facilitate understanding biodiversity within the boundaries of the Preserve in its broader landscape context, the application also displays all occurrences provided by GBIF across Santa Barbara County, California. All biodiversity occurrence datasets displayed within this application can be downloaded via <a href="https://github.com/elimia-analytics/Dangermond-Biodiversity/tree/main/data" target="_blank">this GitHub repository</a>.</h3>
                                        <h3 style = "color: black;">Taxonomic information for all integrated occurrences was harmonized using the <a href="https://www.gbif.org/dataset/d7dddbf4-2cf0-4f39-9b2a-bb099caae36c" target="_blank">GBIF Backbone Taxonomy</a>. Data on conservation status were derived from <a href="https://explorer.natureserve.org/" target="_blank">NatureServe Explorer</a>.</h3>'
                                       
                                     ),
                                     br(),
                                     h2(strong("How frequently are data being updated?")),
                                     HTML(
                                       '<h3 style = "color: black;">Biodiversity occurrences in this application are currently updated once per year. The exact date of last update is indicated on the top of the application user interface.</h3>'  
                                     )
                            )
                    )
                    )
           ),
           absolutePanel(id = "elimia",
                         class = "panel panel-default",
                         top = 6, right = 10, left = "auto", bottom = "auto",
                         width = "5em",
                         height = "5em",
                         style = "border: none; box-shadow: none !important; border-bottom: none; border-color: transparent; background-color: transparent; z-index: 1000 !important; overflow-y: hidden !important; overflow-x: hidden;",
                         HTML("
                         <a href='https://elimia.io' target='_blank'><img src = 'elimia-logo-snail.png', height = '58%'></a>
                              ")
           )
)