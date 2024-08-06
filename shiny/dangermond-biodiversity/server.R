#' ---
#' title: Dangermond Preserve Biodiversity Portal - Server Code
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

# Define server logic required to draw a histogram
function(input, output, session) {

  ## Create web map and add basic elements and functionality
  output$main_map <- renderLeaflet({
    
    leaflet::leaflet(options = leafletOptions(zoomDelta = 0.5, zoomSnap = 0, attributionControl = FALSE, worldCopyJump = FALSE)) %>% # Open new leaflet web map
      leaflet::setView(lng = mean(c(-104.4474, -67.27911)), lat = 50, zoom = 3.5) %>%  # Zoom in on North America
      leaflet::addMapPane("basemap1", zIndex = -100) %>% # Add basemap 1
      leaflet::addProviderTiles(providers$Esri.WorldTerrain, group = "Esri World Terrain", options = list(pathOptions(pane = "basemap1"))) %>%
      leaflet::addMapPane("basemap2", zIndex = -100) %>% # Add basemap 2
      leaflet::addProviderTiles(providers$Esri.WorldImagery, group = "Esri World Imagery", options = list(pathOptions(pane = "basemap2"))) %>%
      leaflet::addMapPane("basemap3", zIndex = -100) %>% # Add basemap 3
      leaflet::addProviderTiles(providers$OpenStreetMap, group = "Open Street Map", options = list(pathOptions(pane = "basemap3"))) %>%
      leaflet::addMapPane("basemap4", zIndex = -100) %>% # Add basemap 4
      leaflet::addProviderTiles(providers$Esri.WorldStreetMap, group = "Esri World Street Map", options = list(pathOptions(pane = "basemap4"))) %>%
      leaflet::addScaleBar(position = "bottomleft") %>% # Add scale bar
      leaflet.extras::addResetMapButton() %>% # Add button to reset map bounds
      leaflet::addLayersControl(baseGroups = c("Esri World Street Map", "Open Street Map", "Esri World Terrain", "Esri World Imagery"), # Add layers control widget
                                # overlayGroups = c("Range Extent", "Occupancy", "Occurrences"),  
                                options = layersControlOptions(collapsed = TRUE), position = "topleft") %>% 
      leafpm::addPmToolbar(toolbarOptions = leafpm::pmToolbarOptions(drawMarker = FALSE, drawCircle = FALSE, drawPolyline = FALSE, editMode = FALSE, cutPolygon = FALSE, removalMode = FALSE), # Add point/polygon drawing tools
                           drawOptions = leafpm::pmDrawOptions(snappable = FALSE, markerStyle = list(draggable = FALSE))
      )
  })
  
}
