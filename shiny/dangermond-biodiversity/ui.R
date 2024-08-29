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

#' # User Interface
navbarPage(title = HTML("<span style='float: left; display: inline-block; padding-left: 20px;'><img src = 'tnc_logo.svg', height = '45'></span><span style='display: inline-block; padding: 5px 5px 35px 15px;'><h1 style = 'font-size: 28px'><strong>Dangermond Preserve Biodiversity Portal</strong></p></span>"), 
           windowTitle = "Dangermond Preserve Biodiversity Portal", 
           id="nav", theme = "style.css",
           
           useShinyjs(),     ## Call to use shinyJS
           
           scr,
           
           tags$head(
             HTML("<link href='https://fonts.googleapis.com/css2?family=Roboto&display=swap' rel='stylesheet'>"),
             HTML("<link href='https://fonts.googleapis.com/css2?family=Cabin&display=swap' rel='stylesheet'>"),
             HTML("<meta name='viewport' content='width=device-width, initial-scale=1'>")
           ),
           
           div(class="outer",
               
               fluidRow(style = "padding-left: 20px;",
                        column(width = 8, 
                               fluidRow(
                                 leafletOutput("main_map", height = "50vh"),
                               ),
                               fluidRow(style = "padding-top: 20px;",
                                        tabsetPanel(id = "metric_switch", type = "pills",
                                                    tabPanel("Records", height = "100%",
                                                             div(style = "overflow-x: scroll;",
                                                                 DT::dataTableOutput("records_table")
                                                             )
                                                    ),
                                                    tabPanel("Species", height = "100%",
                                                             div(style = "overflow-x: scroll;",
                                                                 DT::dataTableOutput("species_table")
                                                             )
                                                    )
                                        )
                               )
                               
                        ),
                        column(width = 4,
                               fluidRow(style = "padding-top: 0px; padding-left: 20px; padding-right: 20px;", dygraphOutput("time_plot", height = "50vh"), type = 7),
                               fluidRow(style = "padding-bottom: 0;", plotlyOutput("taxa_donut", width = "100%", height = "100%"))
                        )
               )
           ),
           
           absolutePanel(id = "cond_inputs_panel", 
                         class = "panel panel-default", 
                         top = 65, left = "auto", right = "35vw", bottom = "auto",
                         width = "6em",
                         height = "2.5em",
                         style = "margin: 0; padding: 0; border-bottom: none; border-color: transparent; background-color: rgba(169, 169, 169, 0); z-index: 1000 !important;", 
                         # span(style = "float: left; padding-right: 5px;",
                         #      actionButton(inputId = "redo_search", label = "Redo search in this area", block = TRUE, class = "btn-primary btn-sm", width = "100%"),
                         # ),
                         span(style = "float: left;",
                              actionButton(inputId = "start_over", label = "Start over", block = TRUE, class = "btn-primary btn-sm", width = "100%")
                         )
           )
           
)