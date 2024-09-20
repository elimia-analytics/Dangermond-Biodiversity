#' ---
#' title: Dangermond Preserve Biodiversity Portal - Server Code
#' ---
#'
#' # Server setup
#' ## Load libraries
library(shiny)
library(leaflet)
library(leaflet.extras)
library(leafpop)
library(purrr)
library(shinyjs)
library(sf)
library(sfcentral)
library(shinybusy)
library(dygraphs)
library(plotly)
library(readr)
library(dplyr)
library(terra)
library(DT)
library(shinycssloaders)

# Load data
## Load custom functions
source("functions.R")

## Load integrated species occurrence data from GitHub repository
integrated_dangermond_occurrences <- read_csv("https://raw.githubusercontent.com/elimia-analytics/Dangermond-Biodiversity/main/data/integrated_occurrences_dangermond.csv")
## Add a row ID to identify rows
integrated_dangermond_occurrences <- integrated_dangermond_occurrences %>% 
  dplyr::mutate(rowID = 1:nrow(integrated_dangermond_occurrences),
                period = ifelse(year < 1990, 1, 2)
                )
## Process and create necessary objects
### Transform species occurrence data to sf object
integrated_dangermond_occurrences_sf <- integrated_dangermond_occurrences %>% 
  dplyr::filter(complete.cases(longitude, latitude, classification_path)) %>% 
  dplyr::mutate(lon = longitude,
                lat = latitude) %>% 
  sf::st_as_sf(
    coords = c("longitude", "latitude"),
    crs = 4326
  )

## Load Dangermond Preserve boundary GIS layer
dangermond_preserve <- readRDS("data/dangermond_preserve.rds")
dangermond_preserve_bbox <- st_bbox(dangermond_preserve)

# Create count rasters across area including all observations
occurrences_bbox <- st_bbox(integrated_dangermond_occurrences_sf)
## Create empty raster for the preserve (resolution approximately 4km squared)
occurrences_raster <- terra::rast(xmin = occurrences_bbox[["xmin"]], 
                               ymin = occurrences_bbox[["ymin"]],
                               xmax = occurrences_bbox[["xmax"]], 
                               ymax = occurrences_bbox[["ymax"]],
                               resolution = 2/101
)
occurrences_raster <- terra::extend(occurrences_raster, 1)
terra::values(occurrences_raster) <- NA
occurrences_raster_polys <- occurrences_raster %>% terra::as.polygons(aggregate = FALSE, na.rm = TRUE) %>% sf::st_as_sf()
# Assign each record its cell ID
integrated_dangermond_occurrences <- integrated_dangermond_occurrences %>% 
  dplyr::mutate(cellID = terra::cellFromXY(occurrences_raster, 
                                           xy = integrated_dangermond_occurrences[c("longitude", "latitude")] %>% as.data.frame()
  )
  )
integrated_dangermond_occurrences_sf <- integrated_dangermond_occurrences %>% 
  dplyr::mutate(lon = longitude,
                lat = latitude) %>% 
  sf::st_as_sf(
    coords = c("lon", "lat"),
    crs = 4326
  )
northern_species_names <- integrated_dangermond_occurrences %>% 
  dplyr::filter(latitude <= (dangermond_preserve_bbox$ymax + 0.1)) %>%
  dplyr::group_by(scientificName) %>%
  dplyr::group_split(.keep = TRUE) %>%
  purrr::map(function(sp){
    if (nrow(sp) >= 50){
      sp$scientificName %>% unique()
    } else {
      NULL
    }
  }) %>% unlist()
southern_species_names <- integrated_dangermond_occurrences %>%
  dplyr::filter(latitude >= (dangermond_preserve_bbox$ymin - 0.1)) %>%
  dplyr::group_by(scientificName) %>%
  dplyr::group_split(.keep = TRUE) %>%
  purrr::map(function(sp){
    if (nrow(sp) >= 50){
      sp$scientificName %>% unique()
    } else {
      NULL
    }
  }) %>% unlist()
directionality_species_names <- intersect(
  integrated_dangermond_occurrences %>% dplyr::filter(period == 1) %>% count(scientificName) %>% dplyr::filter(n >= 10) %>% dplyr::pull(scientificName),
  integrated_dangermond_occurrences %>% dplyr::filter(period == 2) %>% count(scientificName) %>% dplyr::filter(n >= 10) %>% dplyr::pull(scientificName)
) %>% na.omit() %>% as.character()

# Define server logic
function(input, output, session) {
  
  # shinybusy::show_modal_spinner("circle", color = "#024b6c") # show the modal window
  
  # Create reactive objects and functions
  ## Species occurrences object reacting to spatial, temporal, and taxonomic filters
  integrated_dangermond_occurrences_filtered <- reactiveValues(occurrences = integrated_dangermond_occurrences_sf)
  dangermond_northern_limit <- reactiveValues(occurrences = integrated_dangermond_occurrences_sf)
  dangermond_southern_limit <- reactiveValues(occurrences = integrated_dangermond_occurrences_sf)
  species_directionality <- reactiveValues(lines = NULL)
  
  ## Metric count raster
  dangermond_rasters <- reactiveValues(records_count = NULL,
                                       species_count = NULL,
                                       limits_north = NULL,
                                       limits_south = NULL
  )
  dangermond_raster_polys <- reactiveValues(records_count = NULL,
                                            species_count = NULL,
                                            limits_north = NULL,
                                            limits_south = NULL,
                                            selected = NULL)
  ## Objects to store clicks and center values from the taxon sunburst chart
  clicked_taxa <- reactiveValues(taxon = vector(mode = "character"))
  center_taxon <- reactiveValues(name = "Life")
  selected_records <- reactiveValues(points = integrated_dangermond_occurrences_sf[1, ][-1, ])
  ## Function to capture each click from the taxon sunburst chart
  clickData <- reactive({
    currentEventData <- unlist(event_data(event = "plotly_sunburstclick", source = "taxa_plot", priority = "event"))
    currentEventData
  })
  
  filtered_data <- reactive({
    
    if (!is.null(input$select_species)){
      
      out <- integrated_dangermond_occurrences_sf %>% 
        dplyr::filter(scientificName == input$select_species)
      
    } else if (center_taxon$name != "Life"){
      
      out <- integrated_dangermond_occurrences_sf %>%
        dplyr::filter_all(any_vars(. %in% center_taxon$name))
      
    } else {
      
      out <- integrated_dangermond_occurrences_sf
      
    }
    
    out
    
  })

  # Create web map and add basic elements and functionality
  output$main_map <- leaflet::renderLeaflet({
    
    shinybusy::show_modal_spinner("circle", color = "#024b6c") # show the modal window
    
    points_bbox <- st_bbox(dangermond_preserve)
    count_pal <- colorNumeric("Reds", dangermond_raster_polys$selected$metric, na.color = grey(.7))
    
    m <- leaflet::leaflet(options = leafletOptions(zoomDelta = 0.5, zoomSnap = 0, attributionControl = FALSE, worldCopyJump = FALSE)) %>% # Open new leaflet web map
      leaflet::fitBounds(points_bbox[[1]], points_bbox[[2]], points_bbox[[3]], points_bbox[[4]]) %>%  # Zoom in on North America
      leaflet::addMapPane("basemap1", zIndex = -100) %>% # Add basemap 1
      leaflet::addProviderTiles(providers$Esri.WorldTerrain, group = "Esri World Terrain", options = list(pathOptions(pane = "basemap1"))) %>%
      leaflet::addMapPane("basemap2", zIndex = -100) %>% # Add basemap 2
      leaflet::addProviderTiles(providers$Esri.WorldImagery, group = "Esri World Imagery") %>% # , options = list(pathOptions(pane = "basemap2"))) %>%
      leaflet::addMapPane("basemap3", zIndex = -100) %>% # Add basemap 3
      leaflet::addProviderTiles(providers$OpenStreetMap, group = "Open Street Map", options = list(pathOptions(pane = "basemap3"))) %>%
      leaflet::addMapPane("basemap4", zIndex = -100) %>% # Add basemap 4
      leaflet::addProviderTiles(providers$Esri.WorldStreetMap, group = "Esri World Street Map", options = list(pathOptions(pane = "basemap4"))) %>%
      leaflet::addScaleBar(position = "bottomleft") %>% # Add scale bar
      leaflet.extras::addResetMapButton() %>% # Add button to reset map bounds
      leaflet::addLayersControl(baseGroups = c("Esri World Imagery", "Esri World Street Map", "Open Street Map", "Esri World Terrain"), # Add layers control widget
                                # overlayGroups = c("Records"),
                                options = layersControlOptions(collapsed = TRUE), position = "topleft") %>%
      leafpm::addPmToolbar(toolbarOptions = leafpm::pmToolbarOptions(drawMarker = FALSE, drawCircle = FALSE, drawPolyline = FALSE, editMode = FALSE, cutPolygon = FALSE, removalMode = FALSE), # Add point/polygon drawing tools
                           drawOptions = leafpm::pmDrawOptions(snappable = FALSE, markerStyle = list(draggable = FALSE))
      ) %>%
      leaflet::addMapPane("metric_raster", zIndex = 500) %>% # Add basemap 3
      leaflet::addPolygons(data = dangermond_raster_polys$selected,
                           layerId = ~ID,
                           color = "black",
                           fillColor = ~count_pal(dangermond_raster_polys$selected$metric),
                           opacity = 0.5,
                           fillOpacity = 0.75,
                           weight = 1,
                           options = pathOptions(pane = "metric_raster"),
                           highlightOptions = highlightOptions(opacity = 0.5, weight = 2, bringToFront = TRUE, fillOpacity = 0.9),
                           label = dangermond_raster_polys$selected$metric, labelOptions = labelOptions(textOnly = TRUE, direction = "center", textsize = "15px", sticky = FALSE, style = list("color" = "black")) # offset = c(-5, 0)),
      ) %>%
      leaflet::addMapPane("preserve_boundary", zIndex = 200) %>% # Add basemap 3
      leaflet::addPolygons(data = dangermond_preserve,
                           color = "black",
                           opacity = 1,
                           fillColor = "transparent",
                           fillOpacity = 0,
                           options = pathOptions(pane = "preserve_boundary"),
                           weight = 3
      )
    
    shinybusy::remove_modal_spinner()
    
    m
    
  })
  
  observe({
    
    if (is.null(input$select_species)){
      
      integrated_dangermond_occurrences_filtered$occurrences <- integrated_dangermond_occurrences_sf
      
    }
    
    if (input$metric_switch == "Records"){

      integrated_dangermond_occurrences_filtered$occurrences <- filtered_data()
      
      ## Generate record count raster
      record_count_raster_output <- get_count_raster(records = filtered_data() %>% sf::st_set_geometry(NULL), base_raster = occurrences_raster, metric = "records")
      
      ## Update reactive objects
      dangermond_rasters$records_count <- record_count_raster_output$metric_raster
      dangermond_raster_polys$records_count <- record_count_raster_output$metric_raster_polys
      dangermond_raster_polys$selected <- record_count_raster_output$metric_raster_polys
      
    }
    
    if (input$metric_switch == "Species"){
      
      ## Generate record count raster
      species_count_raster_output <- get_count_raster(records = filtered_data() %>% sf::st_set_geometry(NULL), base_raster = occurrences_raster, metric = "species")
      
      ## Update reactive objects
      dangermond_rasters$species_count <- species_count_raster_output$metric_raster
      dangermond_raster_polys$species_count <- species_count_raster_output$metric_raster_polys
      dangermond_raster_polys$selected <- species_count_raster_output$metric_raster_polys
      
      shinybusy::remove_modal_spinner()
      
    }
    
    if (input$metric_switch == "Range Limits (Northern)"){
      
      ## Isolate northern limit records
      dangermond_northern_limit$occurrences <- filtered_data() %>% 
        dplyr::filter(latitude <= (dangermond_preserve_bbox$ymax + 0.1)) %>%
        dplyr::group_by(scientificName) %>%
        dplyr::group_split(.keep = TRUE) %>%
        purrr::map(function(sp){
          if (nrow(sp) >= 50){
            sp <- sp %>%
              dplyr::arrange(desc(latitude)) %>%
              head(5)
            sp
          } else {
            NULL
          }
        }) %>%
        bind_rows()
      
      if (nrow(dangermond_northern_limit$occurrences) > 0){
        
      ## Generate record count raster
      limits_north_count_raster_output <- get_count_raster(records = dangermond_northern_limit$occurrences %>% sf::st_set_geometry(NULL), base_raster = occurrences_raster, metric = "records")
      
      ## Update reactive objects
      dangermond_rasters$limits_north <- limits_north_count_raster_output$metric_raster
      dangermond_raster_polys$limits_north <- limits_north_count_raster_output$metric_raster_polys
      dangermond_raster_polys$selected <- limits_north_count_raster_output$metric_raster_polys
      
      } 
      
    }
    
    if (input$metric_switch == "Range Limits (Southern)"){
      
      ## Isolate southern limit records
      dangermond_southern_limit$occurrences <- filtered_data() %>%
        dplyr::filter(latitude >= (dangermond_preserve_bbox$ymin - 0.1)) %>%
        dplyr::group_by(scientificName) %>%
        dplyr::group_split(.keep = TRUE) %>%
        purrr::map(function(sp){
          if (nrow(sp) >= 50){
            sp <- sp %>%
              dplyr::arrange(latitude) %>%
              head(5)
            sp
          } else {
            NULL
          }
        }) %>%
        bind_rows()
      
      if (nrow(dangermond_southern_limit$occurrences) > 0){
        
      ## Generate record count raster
      limits_south_count_raster_output <- get_count_raster(records = dangermond_southern_limit$occurrences %>% sf::st_set_geometry(NULL), base_raster = occurrences_raster, metric = "records")
      
      ## Update reactive objects
      dangermond_rasters$limits_south <- limits_south_count_raster_output$metric_raster
      dangermond_raster_polys$limits_south <- limits_south_count_raster_output$metric_raster_polys
      dangermond_raster_polys$selected <- limits_south_count_raster_output$metric_raster_polys
      
      }
      
    }

    if (input$metric_switch == "Range Shifts"){
    
    ## Calculate center of gravity shifts over time
    ### Identify subset of species with sufficient temporal coverage
    directionality_subset <- filtered_data() %>%
      dplyr::filter(scientificName %in%
    (intersect(
      integrated_dangermond_occurrences %>% dplyr::filter(period == 1) %>% count(scientificName) %>% dplyr::filter(n >= 10) %>% dplyr::pull(scientificName),
      integrated_dangermond_occurrences %>% dplyr::filter(period == 2) %>% count(scientificName) %>% dplyr::filter(n >= 10) %>% dplyr::pull(scientificName)
    ) %>% na.omit() %>% as.character())
      )
    
    if (nrow(directionality_subset) > 0){
      
    ### Calculate species center of gravity changes
    species_directionality$lines <- purrr::map(unique(directionality_subset$scientificName), function(sp){
      dat <- directionality_subset %>%
        dplyr::filter(scientificName %in% sp)
      out <- rbind(dat %>% dplyr::filter(period == 1) %>% sfcentral::st_central_point(method = "median") %>% sf::st_coordinates(),
                   dat %>% dplyr::filter(period == 2) %>% sfcentral::st_central_point(method = "median") %>% sf::st_coordinates()
      ) %>% as.data.frame() %>%
        dplyr::mutate(taxon = sp, period = 1:2)
      out <- out %>% left_join(dat %>% dplyr::distinct(classification_path, .keep_all = TRUE) %>% dplyr::select(scientificName, classification_path, rowID, kingdom, phylum, class, order, family, genus, species), by = c("taxon" = "scientificName"))
      out
    }) %>% set_names(unique(directionality_subset$scientificName))

        m <- leafletProxy("main_map") %>%
          clearShapes() %>%
          clearMarkerClusters() %>%
          clearMarkers()

        for (i in 1:length(species_directionality$lines)){
          
          m <- m %>%
            leaflet::addMapPane("arrows", zIndex = 400) %>%
            leaflet.extras2::addArrowhead(
              lng = species_directionality$lines[[i]]$X,
              lat = species_directionality$lines[[i]]$Y,
              label = species_directionality$lines[[i]]$taxon,
              color = "#ffffbf"
            ) %>%
            leaflet::addMapPane("preserve_boundary", zIndex = 200) %>% # Add basemap 3
            leaflet::addPolygons(data = dangermond_preserve,
                                 color = "black",
                                 opacity = 1,
                                 fillColor = "transparent",
                                 fillOpacity = 0,
                                 options = pathOptions(pane = "preserve_boundary"),
                                 weight = 3
            )
          m
    
        }
        
        if (!is.null(input$select_species)){
          
          map_occ1 <- directionality_subset %>% 
            dplyr::filter(scientificName == input$select_species,
                          period == 1)
          
          m <- m %>%
            leaflet::addMapPane("records1", zIndex = 350) %>%
            clearMarkers() %>% 
            addCircleMarkers(
              data = map_occ1,
              lng = ~longitude,
              lat = ~latitude,
              layerId = ~key,
              fillColor = "#4169E1",
              fillOpacity = 0.90,
              color = "#4169E1",
              options = pathOptions(pane = "records1"),
              group = "Records 1",
              popup = leafpop::popupTable(map_occ1 %>%
                                            st_set_geometry(NULL) %>%
                                            dplyr::mutate(
                                              URL = paste0("<a href='", URL, "' target='_blank' onmousedown='event.stopPropagation();'>", URL, "</a>")
                                            ), row.numbers = FALSE, feature.id = FALSE),
              popupOptions = popupOptions(maxWidth = 300, autoPan = FALSE, keepInView = TRUE)
            )
          
          map_occ2 <- directionality_subset %>% 
            dplyr::filter(scientificName == input$select_species,
                          period == 2)
          
          m <- m %>%
            leaflet::addMapPane("records2", zIndex = 350) %>%
            addCircleMarkers(
              data = map_occ2,
              lng = ~longitude,
              lat = ~latitude,
              layerId = ~key,
              fillColor = "#8b0000",
              fillOpacity = 0.90,
              color = "#8b0000",
              options = pathOptions(pane = "records2"),
              group = "Records 2",
              popup = leafpop::popupTable(map_occ2 %>%
                                            st_set_geometry(NULL) %>%
                                            dplyr::mutate(
                                              URL = paste0("<a href='", URL, "' target='_blank' onmousedown='event.stopPropagation();'>", URL, "</a>")
                                            ), row.numbers = FALSE, feature.id = FALSE),
              popupOptions = popupOptions(maxWidth = 300, autoPan = FALSE, keepInView = TRUE)
            )
          
          m
          
        }
        
    }
    
    }
    
    if (!is.null(input$records_table_rows_selected) | !is.null(input$species_table_rows_selected) | !is.null(input$northern_limits_table_rows_selected) | !is.null(input$southern_limits_table_rows_selected)){

      shinybusy::show_modal_spinner("circle", color = "#024b6c") # show the modal window
      
      if (!is.null(input$records_table_rows_selected)){
        dat <- filtered_data() %>%
          dplyr::filter(latitude >= input$main_map_bounds$south & latitude <= input$main_map_bounds$north & longitude >= input$main_map_bounds$west & longitude <= input$main_map_bounds$east) %>% 
          dplyr::arrange(desc(eventDate))
        selected_records$points <- dat[input$records_table_rows_selected, ]
      } 
      
      if (!is.null(input$species_table_rows_selected)){
        dat <- filtered_data() %>%
          dplyr::filter(latitude >= input$main_map_bounds$south & latitude <= input$main_map_bounds$north & longitude >= input$main_map_bounds$west & longitude <= input$main_map_bounds$east) %>% 
          dplyr::arrange(desc(eventDate %>% as.Date())) %>%
          dplyr::distinct(scientificName, .keep_all = TRUE) %>%
          dplyr::arrange(scientificName)
        selected_records$points <- dat[input$species_table_rows_selected, ]
      } 

      if (!is.null(input$northern_limits_table_rows_selected)){
        dat <- dangermond_northern_limit$occurrences %>% 
          dplyr::filter(latitude >= input$main_map_bounds$south & latitude <= input$main_map_bounds$north & longitude >= input$main_map_bounds$west & longitude <= input$main_map_bounds$east) %>% 
          dplyr::arrange(scientificName, desc(eventDate %>% as.Date())) 
        selected_records$points <- dat[input$northern_limits_table_rows_selected, ]
      } 
      
      if (!is.null(input$southern_limits_table_rows_selected)){
        dat <- dangermond_southern_limit$occurrences %>% 
          dplyr::filter(latitude >= input$main_map_bounds$south & latitude <= input$main_map_bounds$north & longitude >= input$main_map_bounds$west & longitude <= input$main_map_bounds$east) %>% 
          dplyr::arrange(scientificName, desc(eventDate %>% as.Date()))
        selected_records$points <- dat[input$southern_limits_table_rows_selected, ]
      } 
      
      m <- leafletProxy("main_map") %>%
        clearGroup("Selected records") %>% 
        leaflet::addMapPane("records_selected", zIndex = 500) %>%
        addCircleMarkers(
          data = selected_records$points,
          lng = ~longitude,
          lat = ~latitude,
          layerId = ~key,
          fillColor = "#fdae61",
          fillOpacity = 0.9,
          color = "#fdae61",
          options = pathOptions(pane = "records_selected"),
          group = "Selected records",
          popup = leafpop::popupTable(selected_records$points %>%
                                        st_set_geometry(NULL) %>%
                                        dplyr::mutate(
                                          URL = paste0("<a href='", URL, "' target='_blank' onmousedown='event.stopPropagation();'>", URL, "</a>")
                                        ), row.numbers = FALSE, feature.id = FALSE),
          popupOptions = popupOptions(maxWidth = 300, autoPan = FALSE, keepInView = TRUE)
        )
      
      m
      
      shinybusy::remove_modal_spinner()
      
    } else {
      
      m <- leafletProxy("main_map") %>%
        clearGroup("Selected records")
      
      selected_records$points <- integrated_dangermond_occurrences_sf[1, ][-1, ]
      
    }
    
  })
  
  output$records_table <- DT::renderDataTable({
    
    dat <- filtered_data() %>%
      dplyr::filter(latitude >= input$main_map_bounds$south & latitude <= input$main_map_bounds$north & longitude >= input$main_map_bounds$west & longitude <= input$main_map_bounds$east) %>% 
      sf::st_set_geometry(NULL) %>% 
      dplyr::select(scientificName, URL, longitude, latitude, coordinateUncertaintyInMeters, eventDate, source, dataset, classification_path) %>% 
      dplyr::mutate(longitude = round(longitude, 3), latitude = round(latitude, 3),
                    URL = paste0("<a href='", URL, "' target='_blank' onmousedown='event.stopPropagation();'>", URL, "</a>")) %>% 
      dplyr::arrange(desc(eventDate)) %>% 
      dplyr::rename("Scientific name" = scientificName,
                    "Longitude" = longitude, 
                    "Latitude" = latitude,
                    "Coordinate Uncertainty (m)" = coordinateUncertaintyInMeters,
                    "Date" = eventDate,
                    "Source" = source,
                    "Dataset" = dataset,
                    "Taxonomy" = classification_path
      ) %>% 
      datatable(options = list(dom = 'tp', 
                               pageLength = 8,
                               columnDefs = list(list(width = "50%", className = 'dt-left', targets = "_all")),
                               language = list(emptyTable = 'There are no estimates for these taxa')
      ), 
      # filter = list(position = 'top'),
      selection = list(mode = 'multiple', target = 'row', selected = NULL), 
      escape = FALSE, 
      rownames = FALSE
      )
    
  })

  output$species_table <- DT::renderDataTable({

    dat <- filtered_data() %>%
      dplyr::filter(latitude >= input$main_map_bounds$south & latitude <= input$main_map_bounds$north & longitude >= input$main_map_bounds$west & longitude <= input$main_map_bounds$east) %>% 
      sf::st_set_geometry(NULL) %>%
      dplyr::arrange(desc(eventDate %>% as.Date())) %>%
      dplyr::distinct(scientificName, .keep_all = TRUE) %>%
      dplyr::select(scientificName, eventDate, URL, classification_path) %>%
      dplyr::arrange(scientificName) %>% 
      dplyr::mutate(
        URL = paste0("<a href='", URL, "' target='_blank' onmousedown='event.stopPropagation();'>", URL, "</a>")
      ) %>%
      dplyr::rename("Scientific name" = scientificName,
                    "Last Observation Date" = eventDate,
                    "Last Observation URL" = URL,
                    "Taxonomy" = classification_path
      ) %>%
      datatable(options = list(dom = 'tp',
                               pageLength = 8,
                               columnDefs = list(list(width = "50%", className = 'dt-left', targets = "_all")),
                               language = list(emptyTable = 'There are no estimates for these taxa')
      ),
      # filter = list(position = 'top'),
      selection = list(mode = 'multiple', target = 'row', selected = NULL),
      escape = FALSE,
      rownames = FALSE
      )
    
  })

  output$northern_limits_table <- DT::renderDataTable({

    dat <- dangermond_northern_limit$occurrences %>%
      dplyr::filter(latitude >= input$main_map_bounds$south & latitude <= input$main_map_bounds$north & longitude >= input$main_map_bounds$west & longitude <= input$main_map_bounds$east) %>% 
      sf::st_set_geometry(NULL) %>%
      dplyr::arrange(scientificName, desc(eventDate %>% as.Date())) %>% 
      dplyr::select(scientificName, eventDate, URL, classification_path) %>%
      dplyr::mutate(
        URL = paste0("<a href='", URL, "' target='_blank' onmousedown='event.stopPropagation();'>", URL, "</a>")
      ) %>%
      dplyr::rename("Scientific name" = scientificName,
                    "Observation Date" = eventDate,
                    "Observation URL" = URL,
                    "Taxonomy" = classification_path
      ) %>%
      datatable(options = list(dom = 'tp',
                               pageLength = 8,
                               columnDefs = list(list(width = "50%", className = 'dt-left', targets = "_all")),
                               language = list(emptyTable = 'There are no estimates for these taxa')
      ),
      # filter = list(position = 'top'),
      selection = list(mode = 'multiple', target = 'row', selected = NULL),
      escape = FALSE,
      rownames = FALSE
      )

  })

  output$southern_limits_table <- DT::renderDataTable({

    dat <- dangermond_southern_limit$occurrences %>%
      dplyr::filter(latitude >= input$main_map_bounds$south & latitude <= input$main_map_bounds$north & longitude >= input$main_map_bounds$west & longitude <= input$main_map_bounds$east) %>% 
      sf::st_set_geometry(NULL) %>%
      dplyr::arrange(scientificName, desc(eventDate %>% as.Date())) %>% 
      dplyr::select(scientificName, eventDate, URL, classification_path) %>%
      dplyr::mutate(
        URL = paste0("<a href='", URL, "' target='_blank' onmousedown='event.stopPropagation();'>", URL, "</a>")
      ) %>%
      dplyr::rename("Scientific name" = scientificName,
                    "Observation Date" = eventDate,
                    "Observation URL" = URL,
                    "Taxonomy" = classification_path
      ) %>%
      datatable(options = list(dom = 'tp',
                               pageLength = 8,
                               columnDefs = list(list(width = "50%", className = 'dt-left', targets = "_all")),
                               language = list(emptyTable = 'There are no estimates for these taxa')
      ),
      # filter = list(position = 'top'),
      selection = list(mode = 'multiple', target = 'row', selected = NULL),
      escape = FALSE,
      rownames = FALSE
      )

  })
  
  output$range_shifts_table <- DT::renderDataTable({

    dat <- species_directionality$lines %>%
      dplyr::bind_rows() 
    
    if (center_taxon$name != "Life"){
      dat <- dat %>%
        dplyr::filter_all(any_vars(. %in% center_taxon$name))
    }

    dat <- dat %>%
      dplyr::arrange(taxon) %>%
      dplyr::mutate(period = ifelse(period == 1, "pre-2002", "post-2002")) %>%
      dplyr::select(taxon, period, X, Y, classification_path) %>%
      dplyr::rename("Scientific name" = taxon,
                    "Time Period" = period,
                    "Center of Gravity Longitude" = X,
                    "Center of Gravity Latitude" = Y,
                    "Taxonomy" = classification_path
      ) %>%
      datatable(options = list(dom = 'tp',
                               pageLength = 8,
                               columnDefs = list(list(width = "50%", className = 'dt-left', targets = "_all")),
                               language = list(emptyTable = 'There are no estimates for these taxa')
      ), 
      selection = 'none',
      escape = FALSE,
      rownames = FALSE
      ) %>% 
      formatStyle('Time Period', backgroundColor = styleEqual(c('pre-2002', 'post-2002'), c("#4169E1", "#8b0000")))

  })
  
  observeEvent(input$metric_switch, {

    if (input$metric_switch %in% c("Records", "Species")){
      updateSelectizeInput(inputId = "select_species", session = session, choices = integrated_dangermond_occurrences_filtered$occurrences$scientificName %>% sort(), selected = NULL, server = TRUE)
    }

    if (input$metric_switch == "Range Limits (Northern)"){

      updateSelectizeInput(inputId = "select_species", session = session, choices = northern_species_names %>% sort(), selected = NULL, server = TRUE)
    }

    if (input$metric_switch == "Range Limits (Southern)"){

      updateSelectizeInput(inputId = "select_species", session = session, choices = southern_species_names %>% sort(), selected = NULL, server = TRUE)
    }

    if (input$metric_switch == "Range Shifts"){

      updateSelectizeInput(inputId = "select_species", session = session, choices = directionality_species_names %>% sort(), selected = NULL, server = TRUE)
    }

  })

  clicks <- reactiveValues(IDs = vector(mode = "character"))
  
  observeEvent(input$main_map_shape_click, {
    
    # shinyjs::click("redo_search")
    
    clicks$IDs <- c(clicks$IDs, input$main_map_shape_click$id)
    
    click <- as.character(clicks$IDs[(length(clicks$IDs))])
    
    selected_cell <- dangermond_raster_polys$selected[dangermond_raster_polys$selected$ID == as.numeric(click), ]
    
    poly_bbox <- sf::st_bbox(selected_cell)
    
    leafletProxy("main_map") %>%
      leaflet::flyToBounds(lng1 = poly_bbox[[1]], lat1 = poly_bbox[[2]], lng2 = poly_bbox[[3]], lat2 = poly_bbox[[4]], options = list(animate = TRUE, duration = 1, easeLinearity = 0.1, noMoveStart = TRUE))
    
  })

  
  observeEvent({
    input$main_map_zoom
    input$main_map_bounds
  }, {

    if (input$metric_switch != "Range Shifts"){

    if (input$main_map_zoom > 14) {
      
      if (input$metric_switch == "Records") map_occ <- integrated_dangermond_occurrences_filtered$occurrences 
      if (input$metric_switch == "Species") map_occ <- integrated_dangermond_occurrences_filtered$occurrences %>% 
          dplyr::arrange(desc(eventDate %>% as.Date())) %>%
          dplyr::distinct(scientificName, .keep_all = TRUE)
      if (input$metric_switch == "Range Limits (Northern)") map_occ <- dangermond_northern_limit$occurrences %>%
          dplyr::filter(key %in% dangermond_northern_limit$occurrences$key)
      if (input$metric_switch == "Range Limits (Southern)") map_occ <- dangermond_southern_limit$occurrences %>%
          dplyr::filter(key %in% dangermond_southern_limit$occurrences$key)

      map_occ <- map_occ %>%
          dplyr::filter(latitude >= input$main_map_bounds$south & latitude <= input$main_map_bounds$north & longitude >= input$main_map_bounds$west & longitude <= input$main_map_bounds$east)
      
      if (nrow(map_occ) > 0){

      m <- leafletProxy("main_map") %>%
        clearShapes() %>%
        clearMarkerClusters() %>%
        clearMarkers() %>%
        leaflet::addMapPane("records", zIndex = 400) %>%
        addCircleMarkers(
          data = map_occ,
          lng = ~longitude,
          lat = ~latitude,
          layerId = ~key,
          fillColor = "#4169E1",
          fillOpacity = 0.75,
          color = grey(0.15),
          options = pathOptions(pane = "records"),
          group = "Records",
          popup = leafpop::popupTable(map_occ %>%
                                        st_set_geometry(NULL) %>%
                                        dplyr::mutate(
                                          URL = paste0("<a href='", URL, "' target='_blank' onmousedown='event.stopPropagation();'>", URL, "</a>")
                                        ), row.numbers = FALSE, feature.id = FALSE),
          popupOptions = popupOptions(maxWidth = 300, autoPan = FALSE, keepInView = TRUE)
        )
      
      } else {
        m <- leafletProxy("main_map") %>%
          clearShapes() %>%
          clearMarkerClusters() %>%
          clearMarkers()
      }

    } else {

      count_pal <- colorNumeric("Reds", dangermond_raster_polys$selected$metric, na.color = grey(.7))

      m <- leafletProxy("main_map") %>%
        clearShapes() %>%
        clearMarkerClusters() %>%
        clearMarkers() %>%
        leaflet::addMapPane("metric_raster", zIndex = 500) %>% # Add basemap 3
        leaflet::addPolygons(data = dangermond_raster_polys$selected,
                             layerId = ~ID,
                             color = "black",
                             fillColor = ~count_pal(dangermond_raster_polys$selected$metric),
                             opacity = 0.5,
                             fillOpacity = 0.75,
                             weight = 1,
                             options = pathOptions(pane = "metric_raster"),
                             highlightOptions = highlightOptions(opacity = 0.5, weight = 2, bringToFront = TRUE, fillOpacity = 0.9),
                             label = dangermond_raster_polys$selected$metric, labelOptions = labelOptions(textOnly = TRUE, direction = "center", textsize = "15px", sticky = FALSE, style = list("color" = "black")) # offset = c(-5, 0)),
        ) %>%
        leaflet::addMapPane("preserve_boundary", zIndex = 200) %>% # Add basemap 3
        leaflet::addPolygons(data = dangermond_preserve,
                             color = "black",
                             opacity = 1,
                             fillColor = "transparent",
                             fillOpacity = 0,
                             options = pathOptions(pane = "preserve_boundary"),
                             weight = 3
        )

    }

    m
    }

  }, ignoreNULL = TRUE)
  
  output$time_plot <- dygraphs::renderDygraph({

    if (input$metric_switch %in% c("Records", "Species")){
      
    dat <- integrated_dangermond_occurrences_filtered$occurrences %>% 
      dplyr::filter(latitude >= input$main_map_bounds$south & latitude <= input$main_map_bounds$north & longitude >= input$main_map_bounds$west & longitude <= input$main_map_bounds$east)

    dat <- dat %>%
      dplyr::filter(year <= substr(Sys.Date(), 1, 4)) %>%
      dplyr::group_by(eventDate) %>%
      dplyr::summarise(number_records = n()) %>%
      dplyr::filter(complete.cases(eventDate)) %>%
      dplyr::mutate(eventDate = eventDate %>% as.Date())

    records_over_time <- xts::xts(x = dat$number_records, order.by = dat$eventDate)

    start_window <- min(dat$eventDate)
    end_window <- Sys.time()

    dygraphs::dygraph(records_over_time, ylab = "") %>%
      # dygraphs::dyBarChart() %>%
      dygraphs::dyOptions(
        stepPlot = TRUE,
        fillGraph = TRUE,
        drawGrid = FALSE
      ) %>%
      dygraphs::dySeries("V1", label = "Number of records", color = "#1f417d") %>%
      dygraphs::dyAxis(
        "y",
        axisLabelWidth = 0
      ) %>%
      dyAxis(
        name="x",
        axisLabelFormatter = "function(d){ return d.getFullYear() }"
      ) %>%
      dygraphs::dyRangeSelector()
    
    }

  })
  
  observeEvent(input$start_over , {
    
    shinybusy::show_modal_spinner("circle", color = "#024b6c") # show the modal window
    
    points_bbox <- st_bbox(dangermond_preserve)
    count_pal <- colorNumeric("Reds", dangermond_raster_polys$selected$metric, na.color = grey(.7))
    
    leafletProxy("main_map") %>%
      leaflet::flyToBounds(points_bbox[[1]], points_bbox[[2]], points_bbox[[3]], points_bbox[[4]]) %>%  # Zoom in on North America
      clearShapes() %>%
      clearMarkerClusters() %>%
      clearMarkers() %>%
      leaflet::addMapPane("metric_raster", zIndex = 500) %>% # Add basemap 3
      leaflet::addPolygons(data = dangermond_raster_polys$selected,
                           layerId = ~ID,
                           color = "black",
                           fillColor = ~count_pal(dangermond_raster_polys$selected$metric),
                           opacity = 0.5,
                           fillOpacity = 0.75,
                           weight = 1,
                           options = pathOptions(pane = "metric_raster"),
                           highlightOptions = highlightOptions(opacity = 0.5, weight = 2, bringToFront = TRUE, fillOpacity = 0.9),
                           label = dangermond_raster_polys$selected$metric, labelOptions = labelOptions(textOnly = TRUE, direction = "center", textsize = "15px", sticky = FALSE, style = list("color" = "black")) # offset = c(-5, 0)),
      ) %>%
      leaflet::addMapPane("preserve_boundary", zIndex = 200) %>% # Add basemap 3
      leaflet::addPolygons(data = dangermond_preserve,
                           color = "black",
                           opacity = 1,
                           fillColor = "transparent",
                           fillOpacity = 0,
                           options = pathOptions(pane = "preserve_boundary"),
                           weight = 3
      )
    
    #### Species occurrences object reacting to spatial, temporal, and taxonomic filters
    integrated_dangermond_occurrences_filtered <- reactiveValues(occurrences = integrated_dangermond_occurrences_sf)

    center_taxon$name <- "Life"
    
    shinybusy::remove_modal_spinner()
    
  })
  
  output$taxa_donut <- plotly::renderPlotly({

    dat <- integrated_dangermond_occurrences_sf %>%
      sf::st_set_geometry(NULL) %>%
      dplyr::distinct(kingdom, phylum, class, order, family, .keep_all = TRUE)

    idees <- purrr::map(c("kingdom", "phylum", "class", "order", "family", "genus"), function(x) dat[[x]] %>% unique()) %>% unlist() %>% na.omit() %>% as.character()
    parentals <- purrr::map(1:length(idees), function(i){
      target_class_path <- dat$classification_path[grep(idees[i], dat$classification_path)[1]]
      target_class_path_names <- (target_class_path %>% strsplit("\\|"))[[1]]
      target_class_path_names[grep(paste0("^", idees[i], "$"), target_class_path_names) - 1]
    })
    parentals <- c("", purrr::map(parentals, function(x) ifelse(length(x) > 0, x, "Life")) %>% unlist())
    idees <- c("Life", idees)

    trace1 <- list(
      leaf = list(opacity = 1),
      meta = list(columnNames = list(
        ids = "data.0.ids",
        labels = "data.0.labels",
        parents = "data.0.parents"
      )),
      type = "sunburst",
      level = "Life", #center_taxon$name,
      idssrc = "kirudang:0:8e4421",
      ids = idees,
      maxdepth = 3,
      rotation = -4,
      labelssrc = "kirudang:0:21e923",
      labels = idees,
      parentssrc = "kirudang:0:dc20c3",
      parents = parentals,
      hovertemplate = ""
    )
    data <- list(trace1)
    layout <- list(
      font = list(
        size = 12,
        color = "rgb(165, 25, 25)",
        family = "Roboto"
      ),
      xaxis = list(
        range = c(-1, 4),
        autorange = TRUE,
        fixedrange = TRUE
      ),
      yaxis = list(
        range = c(-1, 4),
        autorange = TRUE,
        fixedrange = TRUE
      ),
      height = 400,
      width = 200,
      margin = list(
        b = 50,
        r = 50,
        t = 20,
        pad = 2
      ),
      metasrc = "kirudang:0:023680",
      meta = c("white", "#EF553B", "rgb(251,128,114)", "rgb(251,128,114)", "rgb(251,128,114)", "rgb(251,128,114)", "rgb(251,128,114)", "rgb(251,128,114)", "rgb(251,128,114)", "rgb(251,128,114)", "#ba2020", "#ba2020", "#ba2020", "#ba2020", "#ba2020", "#ba2020", "#ba2020", "#ba2020", "#ba2020", "#ba2020", "rgb(252,195,195)", "rgb(252,195,195)", "#00CC96", "rgb(204,235,197)", "rgb(204,235,197)", "rgb(204,235,197)", "rgb(204,235,197)", "rgb(204,235,197)", "rgb(141,211,199)", "rgb(141,211,199)", "#a3e897", "#a3e897", "#a3e897", "#a3e897", "#a3e897", "#a3e897", "#a3e897", "#ffe600", "#faf693", "#faf693", "#faf693", "#ffd857", "#ffd857", "#ffd857", "#ffd857", "#fff16b", "#fff16b", "#fff16b", "#fff16b", "#ffb300", "#ffb300", "#ffb300", "#ffb300", "#ffb300", "#fcffc2", "#fcffc2", "#ff5ac3", "#ff5ac3", "#ff5ac3", "#ff5ac3", "#45abff", "#45abff", "#45abff", "#45abff", "#45abff", "#45abff", "#45abff", "#45abff", "#AB63FA", "#AB63FA", "#AB63FA"),
      modebar = list(orientation = "v"),
      autosize = FALSE,
      dragmode = "select",
      clickmode = "event",
      hovermode = "x",
      hoverlabel = list(
        font = list(
          size = 12,
          color = "#000",
          family = "Droid Sans"
        ),
        align = "auto",
        bgcolor = "rgb(215, 116, 116)"
      ),
      separators = ", ",
      uniformtext = list(mode = FALSE),
      selectdirection = "v"
    )
    p <- plot_ly(source = "taxa_plot", customdata = idees) %>%
      config(displayModeBar = FALSE)
    p <- add_trace(p, leaf=trace1$leaf, meta=trace1$meta, mode=trace1$mode, type=trace1$type, level=trace1$level, idssrc=trace1$idssrc, ids=trace1$ids, maxdepth=trace1$maxdepth, rotation=trace1$rotation, labelssrc=trace1$labelssrc, labels=trace1$labels, parentssrc=trace1$parentssrc, parents=trace1$parents, hovertemplate=trace1$hovertemplate)
    p <- layout(p, font=layout$font, title=layout$title, xaxis=layout$xaxis, yaxis=layout$yaxis, margin=layout$margin, metasrc=layout$metasrc, meta=layout$meta, modebar=layout$modebar, autosize=layout$autosize, dragmode=layout$dragmode, template=layout$template, clickmode=layout$clickmode, hovermode=layout$hovermode, hoverlabel=layout$hoverlabel, separators=layout$separators, uniformtext=layout$uniformtext, selectdirection=layout$selectdirection, sunburstcolorway=layout$sunburstcolorway, extendsunburstcolors=layout$extendsunburstcolors)
    p <- p %>% event_register("plotly_sunburstclick")

    p

  })


   observeEvent(event_data(event = "plotly_sunburstclick",
                          source = "taxa_plot",
                          priority = "event"), {

                            updateSelectizeInput(inputId = "select_species", session = session, selected = NULL, server = TRUE)
                            
                            dat <- integrated_dangermond_occurrences_sf

                            clicked_taxa$taxon <- c(clicked_taxa$taxon, clickData()[["customdata"]])

                            if (!identical(clicked_taxa$taxon, "Life")){
                              if (length(clicked_taxa$taxon) == 1){
                                center_taxon$name <- clickData()[["customdata"]]
                              }
                              if (length(clicked_taxa$taxon) > 1){
                                last <- clicked_taxa$taxon[length(clicked_taxa$taxon)]
                                last_taxon_path <- dat$classification_path[grep(last, dat$classification_path)[1]]
                                last_taxon_path_names <- c("Life", (last_taxon_path %>% strsplit("\\|"))[[1]])
                                last_taxon_path_names <- last_taxon_path_names[-length(last_taxon_path_names)]
                                beforelast <- clicked_taxa$taxon[(length(clicked_taxa$taxon)-1)]
                                beforelast_taxon_path <- dat$classification_path[grep(beforelast, dat$classification_path)[1]]
                                beforelast_taxon_path_names <- c("Life", (beforelast_taxon_path %>% strsplit("\\|"))[[1]])
                                beforelast_taxon_path_names <- beforelast_taxon_path_names[-length(beforelast_taxon_path_names)]

                                if (last != beforelast){
                                  if (grep(last, last_taxon_path_names) > grep(beforelast, last_taxon_path_names)){
                                    center_taxon$name <- last
                                  } else {
                                    center_taxon$name <- last_taxon_path_names[grep(last, last_taxon_path_names)-1]
                                    clicked_taxa$taxon[(length(clicked_taxa$taxon))] <- center_taxon$name
                                  }
                                }
                                if (last == beforelast){
                                  center_taxon$name <- last_taxon_path_names[grep(last, last_taxon_path_names)-1]
                                  clicked_taxa$taxon[(length(clicked_taxa$taxon))] <- center_taxon$name
                                }
                              }

                            }

                          })




  
  # observeEvent(input$select_species, {
  #   
  #   if (!is.null(input$select_species)){
  # 
  #   integrated_dangermond_occurrences_filtered$occurrences <- integrated_dangermond_occurrences_filtered$occurrences <- integrated_dangermond_occurrences_sf %>%
  #     dplyr::filter(latitude >= input$main_map_bounds$south & latitude <= input$main_map_bounds$north & longitude >= input$main_map_bounds$west & longitude <= input$main_map_bounds$east) %>% 
  #     dplyr::filter(scientificName == input$select_species)
  #     
  #   }
  #   
  #   if (is.null(input$select_species)){
  #     
  #     integrated_dangermond_occurrences_filtered$occurrences <- integrated_dangermond_occurrences_sf %>%
  #       dplyr::filter(latitude >= input$main_map_bounds$south & latitude <= input$main_map_bounds$north & longitude >= input$main_map_bounds$west & longitude <= input$main_map_bounds$east)
  #     
  #     updateSelectizeInput(inputId = "select_species", session = session, choices = integrated_dangermond_occurrences_filtered$occurrences$scientificName)
  #     
  #   }
  #   
  # })
  

 
 # 
 # output$northern_limits_table <- DT::renderDataTable({
 #   
 #   dat <- dangermond_northern_limit_occurrences %>%
 #     dplyr::filter(latitude >= input$main_map_bounds$south & latitude <= input$main_map_bounds$north & longitude >= input$main_map_bounds$west & longitude <= input$main_map_bounds$east)
 #   
 #   if (center_taxon$name != "Life"){
 #     dat <- dat %>%
 #       dplyr::filter(rowID %in% (dat %>%
 #                                   dplyr::select(rowID, kingdom, phylum, class, order, family, genus, species) %>%
 #                                   dplyr::filter_all(any_vars(. %in% center_taxon$name)) %>%
 #                                   dplyr::pull(rowID))
 #       )
 #   }
 #   
 #   updateSelectizeInput(inputId = "select_species", session = session, choices = c("", dat$scientificName))
 #   
 #   dat <- dat %>% 
 #     dplyr::arrange(desc(eventDate %>% as.Date())) %>% 
 #     dplyr::distinct(scientificName, .keep_all = TRUE) %>% 
 #     dplyr::select(scientificName, eventDate, URL, classification_path) %>% 
 #     dplyr::mutate(
 #       URL = paste0("<a href='", URL, "' target='_blank' onmousedown='event.stopPropagation();'>", URL, "</a>")
 #     ) %>% 
 #     dplyr::rename("Scientific name" = scientificName,
 #                   "Last Observation Date" = eventDate,
 #                   "Last Observation URL" = URL,
 #                   "Taxonomy" = classification_path
 #     ) %>% 
 #     datatable(options = list(dom = 'tp', 
 #                              pageLength = 8,
 #                              columnDefs = list(list(width = "50%", className = 'dt-left', targets = "_all")),
 #                              language = list(emptyTable = 'You have not selected any occurrences')
 #     ), 
 #     # filter = list(position = 'top'),
 #     selection = list(mode = 'multiple', target = 'row', selected = NULL), 
 #     escape = FALSE, 
 #     rownames = FALSE
 #     )
 #   
 # })
 # 
 # output$southern_limits_table <- DT::renderDataTable({
 # 
 #   dat <- dangermond_southern_limit_occurrences %>%
 #     dplyr::filter(latitude >= input$main_map_bounds$south & latitude <= input$main_map_bounds$north & longitude >= input$main_map_bounds$west & longitude <= input$main_map_bounds$east)
 # 
 #   if (center_taxon$name != "Life"){
 #     dat <- dat %>%
 #       dplyr::filter(rowID %in% (dat %>%
 #                                   dplyr::select(rowID, kingdom, phylum, class, order, family, genus, species) %>%
 #                                   dplyr::filter_all(any_vars(. %in% center_taxon$name)) %>%
 #                                   dplyr::pull(rowID))
 #       )
 #   }
 # 
 #   updateSelectizeInput(inputId = "select_species", session = session, choices = c("", dat$scientificName))
 #   
 #   dat <- dat %>%
 #     dplyr::arrange(desc(eventDate %>% as.Date())) %>%
 #     dplyr::distinct(scientificName, .keep_all = TRUE) %>%
 #     dplyr::select(scientificName, eventDate, URL, classification_path) %>%
 #     dplyr::mutate(
 #       URL = paste0("<a href='", URL, "' target='_blank' onmousedown='event.stopPropagation();'>", URL, "</a>")
 #     ) %>%
 #     dplyr::rename("Scientific name" = scientificName,
 #                   "Last Observation Date" = eventDate,
 #                   "Last Observation URL" = URL,
 #                   "Taxonomy" = classification_path
 #     ) %>%
 #     datatable(options = list(dom = 'tp',
 #                              pageLength = 8,
 #                              columnDefs = list(list(width = "50%", className = 'dt-left', targets = "_all")),
 #                              language = list(emptyTable = 'You have not selected any occurrences')
 #     ),
 #     # filter = list(position = 'top'),
 #     selection = list(mode = 'multiple', target = 'row', selected = NULL),
 #     escape = FALSE,
 #     rownames = FALSE
 #     )
 # 
 # })
 # 
 # output$range_shifts_table <- DT::renderDataTable({
 #   
 #   dat <- species_directionality_lines %>%
 #     dplyr::bind_rows() %>% 
 #     dplyr::filter(Y >= input$main_map_bounds$south & Y <= input$main_map_bounds$north & X >= input$main_map_bounds$west & X <= input$main_map_bounds$east)
 #   
 #   if (center_taxon$name != "Life"){
 #     dat <- dat %>%
 #       dplyr::filter_all(any_vars(. %in% center_taxon$name))
 #   }
 #   
 #   # updateSelectizeInput(inputId = "select_species", session = session, choices = c("", dat$scientificName))
 #   
 #   dat <- dat %>%
 #     dplyr::arrange(taxon) %>% 
 #     dplyr::mutate(period = ifelse(period == 1, "pre-2002", "post-2002")) %>% 
 #     dplyr::select(taxon, period, X, Y, classification_path) %>%
 #     dplyr::rename("Scientific name" = taxon,
 #                   "Time Period" = period,
 #                   "Center of Gravity Longitude" = X,
 #                   "Center of Gravity Latitude" = Y,
 #                   "Taxonomy" = classification_path
 #     ) %>%
 #     datatable(options = list(dom = 'tp',
 #                              pageLength = 8,
 #                              columnDefs = list(list(width = "50%", className = 'dt-left', targets = "_all")),
 #                              language = list(emptyTable = 'You have not selected any occurrences')
 #     ),
 #     # filter = list(position = 'top'),
 #     selection = list(mode = 'multiple', target = 'row', selected = NULL),
 #     escape = FALSE,
 #     rownames = FALSE
 #     )
 #   
 # })
 

}
