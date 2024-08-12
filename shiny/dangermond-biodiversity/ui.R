#' ---
#' title: Dangermond Preserve Biodiversity Portal - UI Code
#' ---
#'
#' # Server setup
#' ## Load libraries
library(shiny)
library(leaflet)
library(leaflet.extras)
library(purrr)
library(shinyjs)
library(sf)
library(shinycssloaders)
library(dygraphs)
library(plotly)

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
navbarPage(title = HTML("<span style='display: inline-block; padding: 13px 5px 15px 15px;'><p style = 'font-size: 28px'><strong>Dangermond Preserve Biodiversity Portal</strong></p></span>"), 
           windowTitle = "Dangermond Preserve Biodiversity Portal", 
           id="nav", theme = "style.css",
           
           useShinyjs(),     ## Call to use shinyJS
           
           scr,
           
           tags$head(
             HTML("<link href='https://fonts.googleapis.com/css2?family=Roboto&display=swap' rel='stylesheet'>"),
             HTML("<meta name='viewport' content='width=device-width, initial-scale=1'>"),
           ),
           
           div(class="outer",
               
               fluidRow(
                 column(width = 9,
                        fluidRow(
                          shinycssloaders::withSpinner(leafletOutput("main_map"), type = 7) #, height="60vh", width = "100vw"), type = 7),
                        ),
                        fluidRow(
                          column(width = 8,
                                 shinycssloaders::withSpinner(dygraphOutput("time_plot"), type = 7),
                                 ),
                          column(width = 4,
                                 shinycssloaders::withSpinner(plotlyOutput("taxa_donut"), type = 7)
                                 )
                        )
                 )
               ),
               
               absolutePanel(id = "cond_inputs_panel", 
                             class = "panel panel-default", 
                             top = 63, left = 380, right = "auto", bottom = "auto",
                             width = "10vw",
                             height = "4vh",
                             style = "margin-top: 0; padding: 0em 1.8em 1em 3em; border-color: rgba(169, 169, 169, 0); background-color: rgba(169, 169, 169, 0); z-index: 10 !important; overflow-y: hidden !important; overflow-x: hidden;", 
                             actionButton(inputId = "map_filter", label = "Redo search in this area", icon = icon("close"), block = TRUE, class = "btn-primary btn-sm", width = "100%")
               )

           )
               
)