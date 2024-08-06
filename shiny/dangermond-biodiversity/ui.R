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
               
               shinycssloaders::withSpinner(leafletOutput("main_map", height="60vh", width = "100vw"), type = 7),

           )
               
)