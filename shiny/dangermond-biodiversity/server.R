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

# Define server logic
function(input, output, session) {
  
  # Create reactive objects and functions
  ## Species occurrences object reacting to spatial, temporal, and taxonomic filters
  integrated_dangermond_occurrences_filtered <- reactiveValues(occurrences = integrated_dangermond_occurrences_sf)
  dangermond_northern_limit <- reactiveValues(occurrences = integrated_dangermond_occurrences_sf)
  dangermond_southern_limit <- reactiveValues(occurrences = integrated_dangermond_occurrences_sf)
  species_directionality <- reactiveValues(lines = NULL)
  
  ## Metric count raster
  dangermond_rasters <- reactiveValues(records_count = NULL,
                                       species_count = NULL
  )
  dangermond_raster_polys <- reactiveValues(records_count = NULL,
                                            species_count = NULL,
                                            selected = NULL)
  
  ## Objects to store clicks and center values from the taxon sunburst chart
  clicked_taxa <- reactiveValues(taxon = vector(mode = "character"))
  center_taxon <- reactiveValues(name = "Life")
  selected_records <- reactiveValues(points = integrated_dangermond_occurrences_sf[1, ][-1, ])
  map_bounds_change <- reactiveValues(start_bounds = NULL,
                                      new_bounds = NULL,
                                      end_bounds = NULL
                                      )
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

    if (!is.null(input$select_status)){
      
        out <- out %>%
          dplyr::filter_all(any_vars(. %in% input$select_status))
        
    }
    
    out
    
  })

  # Create web map and add basic elements and functionality
  output$main_map <- leaflet::renderLeaflet({
    
    shinybusy::show_modal_spinner("circle", color = "#024b6c") # show the modal window
    
    points_bbox <- st_bbox(dangermond_preserve)

    m <- leaflet::leaflet(options = leafletOptions(zoomDelta = 0.5, zoomSnap = 0, attributionControl = FALSE, worldCopyJump = FALSE, scrollWheelZoom = FALSE)) %>% # Open new leaflet web map
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
      ) 
    
    shinybusy::remove_modal_spinner()
    
    m
    
  })
  
  observe({


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
                     
                     
                   }
                   
                   count_pal <- colorNumeric("Reds", dangermond_raster_polys$selected$metric, na.color = grey(.7))
                   
                   m <- leafletProxy("main_map") %>%
                     clearShapes() %>%
                     clearMarkerClusters() %>%
                     clearMarkers() %>%
                     clearControls() %>% 
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
                     leaflet::addLegend(
                       position = "bottomright",
                       pal = count_pal,
                       values = dangermond_raster_polys$selected$metric,
                       title = paste0("Number of ", input$metric_switch)
                     )
                 
                 m
      
                 
  })

  observeEvent({
    input$main_map_zoom
    input$main_map_bounds
  }, {
    
    if (is.null(input$select_species) & is.null(input$select_status)){
      
      out <- filtered_data() %>%
        dplyr::filter(latitude >= input$main_map_bounds$south & latitude <= input$main_map_bounds$north & longitude >= input$main_map_bounds$west & longitude <= input$main_map_bounds$east)
      
      updateSelectizeInput(inputId = "select_species", session = session,
                           choices = out %>%
                             dplyr::pull(scientificName) %>%
                             as.character() %>%
                             unique() %>%
                             sort(),
                           server = TRUE)
      
      records_status_names <- list(
        "California State Rank" = out$California_srank %>% unique() %>% sort(),
        "NatureServe Global Rank" = out$grank %>% unique() %>% sort(),
        "US ESA Status" = out$esa %>% unique() %>% sort(),
        "IUCN Red List Status" = out$iucn %>% unique() %>% sort()
      )
      
      records_status_names <- records_status_names[purrr::map_lgl(records_status_names, function(x) length(x) > 0)]
      
      updateSelectizeInput(inputId = "select_status", session = session,
                           choices = records_status_names,
                           server = TRUE)
    }
    
    if (!is.null(input$select_species)){
      
      out <- filtered_data() %>%
        dplyr::filter(latitude >= input$main_map_bounds$south & latitude <= input$main_map_bounds$north & longitude >= input$main_map_bounds$west & longitude <= input$main_map_bounds$east)
      
      records_status_names <- list(
        "California State Rank" = out$California_srank %>% unique() %>% sort(),
        "NatureServe Global Rank" = out$grank %>% unique() %>% sort(),
        "US ESA Status" = out$esa %>% unique() %>% sort(),
        "IUCN Red List Status" = out$iucn %>% unique() %>% sort()
      )
      
      records_status_names <- records_status_names[purrr::map_lgl(records_status_names, function(x) length(x) > 0)]
      
      
      updateSelectizeInput(inputId = "select_status", session = session,
                           choices = records_status_names,
                           server = TRUE)
    }

    if (!is.null(input$select_status)){
    
      out <- filtered_data() %>%
        dplyr::filter(latitude >= input$main_map_bounds$south & latitude <= input$main_map_bounds$north & longitude >= input$main_map_bounds$west & longitude <= input$main_map_bounds$east)
      
    updateSelectizeInput(inputId = "select_species", session = session,
                         choices = out %>%
                           dplyr::pull(scientificName) %>%
                           as.character() %>%
                           unique() %>%
                           sort(),
                         server = TRUE)
    
    }
    
  
      if (input$main_map_zoom > 14) {

        map_occ <- filtered_data() %>%
          dplyr::filter(latitude >= input$main_map_bounds$south & latitude <= input$main_map_bounds$north & longitude >= input$main_map_bounds$west & longitude <= input$main_map_bounds$east)
        
        if (nrow(map_occ) > 0){
          
          m <- leafletProxy("main_map") %>%
            clearShapes() %>%
            clearMarkerClusters() %>%
            clearMarkers() %>%
            clearControls() %>% 
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
            ) %>% 
            leaflet::addLegend(
              position = "bottomright", 
              colors = "#4169E1",
              labels = "Biodiversity Records",
              opacity = 0.75
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
          clearControls() %>% 
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
          ) %>% 
          leaflet::addLegend(
            position = "bottomright",
            pal = count_pal,
            values = dangermond_raster_polys$selected$metric,
            title = paste0("Number of ", input$metric_switch)
          )
        
      }
      
      m

  })

  observeEvent(input$records_table_rows_selected, {

  if (!is.null(input$records_table_rows_selected)){

    shinybusy::show_modal_spinner("circle", color = "#024b6c") # show the modal window

    if (!is.null(input$records_table_rows_selected)){
      dat <- filtered_data() %>%
        dplyr::filter(latitude >= input$main_map_bounds$south & latitude <= input$main_map_bounds$north & longitude >= input$main_map_bounds$west & longitude <= input$main_map_bounds$east) %>%
        dplyr::arrange(desc(eventDate))
      selected_records$points <- dat[input$records_table_rows_selected, ]
    }

    m <- leafletProxy("main_map") %>%
      clearGroup("Selected") %>%
      leaflet::addMapPane("records_selected", zIndex = 500) %>%
      addCircleMarkers(
        data = selected_records$points,
        lng = ~longitude,
        lat = ~latitude,
        fillColor = "#fdae61",
        fillOpacity = 0.9,
        color = "#fdae61",
        options = pathOptions(pane = "records_selected"),
        group = "Selected",
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
      clearGroup("Selected")

    selected_records$points <- integrated_dangermond_occurrences_sf[1, ][-1, ]
    
  }

}, ignoreNULL = FALSE)
  
  output$records_table <- DT::renderDataTable({

    dat <- filtered_data() %>%
      dplyr::filter(latitude >= input$main_map_bounds$south & latitude <= input$main_map_bounds$north & longitude >= input$main_map_bounds$west & longitude <= input$main_map_bounds$east) %>%
      sf::st_set_geometry(NULL) %>%
      dplyr::mutate(longitude = round(longitude, 3), latitude = round(latitude, 3),
                    URL = paste0("<a href='", URL, "' target='_blank' onmousedown='event.stopPropagation();'>", URL, "</a>")
                    ) %>%
      dplyr::select(scientificName, URL, longitude, latitude, coordinateUncertaintyInMeters, eventDate, source, dataset, classification_path, California_srank, grank, esa, iucn) %>%
      dplyr::arrange(desc(eventDate)) %>%
      dplyr::rename("Scientific name" = scientificName,
                    "Longitude" = longitude,
                    "Latitude" = latitude,
                    "Coordinate Uncertainty (m)" = coordinateUncertaintyInMeters,
                    "Date" = eventDate,
                    "Source" = source,
                    "Dataset" = dataset,
                    "Taxonomy" = classification_path,
                    "California State Rank" = California_srank,
                    "NatureServe Global Rank" = grank,
                    "US ESA Status" = esa,
                    "IUCN Red List Category" = iucn
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

  output$time_plot <- dygraphs::renderDygraph({

    dat <- filtered_data() %>%
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

  })
  
  observeEvent(input$start_over, {
    
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
    
      out <- integrated_dangermond_occurrences_sf 
      
      records_status_names <- list(
        "California State Rank" = out$California_srank %>% unique() %>% sort(),
        "NatureServe Global Rank" = out$grank %>% unique() %>% sort(),
        "US ESA Status" = out$esa %>% unique() %>% sort(),
        "IUCN Red List Status" = out$iucn %>% unique() %>% sort()
      )
      
      records_status_names <- records_status_names[purrr::map_lgl(records_status_names, function(x) length(x) > 0)]
      
      
      updateSelectizeInput(inputId = "select_status", session = session,
                           choices = records_status_names,
                           server = TRUE)

      updateSelectizeInput(inputId = "select_species", session = session,
                           choices = out %>%
                             dplyr::pull(scientificName) %>%
                             as.character() %>%
                             unique() %>%
                             sort(),
                           server = TRUE)

  })
  
  output$taxa_donut <- plotly::renderPlotly({

    dat <- dat <- integrated_dangermond_occurrences_sf %>% 
      dplyr::filter(latitude >= input$main_map_bounds$south & latitude <= input$main_map_bounds$north & longitude >= input$main_map_bounds$west & longitude <= input$main_map_bounds$east) %>%
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
      level = center_taxon$name,
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
                            
                            dat <- integrated_dangermond_occurrences_sf %>% 
                              dplyr::filter(latitude >= input$main_map_bounds$south & latitude <= input$main_map_bounds$north & longitude >= input$main_map_bounds$west & longitude <= input$main_map_bounds$east)
                            
                            clicked_taxa$taxon <- c(clicked_taxa$taxon, clickData()[["customdata"]])
                            
                            if (!identical(clicked_taxa$taxon, "Life")){
                              if (length(clicked_taxa$taxon) == 1){
                                center_taxon$name <- clickData()[["customdata"]]
                              }
                              if (length(clicked_taxa$taxon) > 1){
                                
                                if (identical(unique(clicked_taxa$taxon), "Life")){
                                  
                                  center_taxon$name <- "Life"
                                  
                                } else {
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
                              
                            }
                            
                            if (input$main_map_zoom > 14) {
                              
                              map_occ <- filtered_data() %>%
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
                                
                                
                              }
                              
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
                            
                            updateSelectizeInput(inputId = "select_species", session = session, 
                                                 choices = filtered_data() %>%
                                                   dplyr::filter(latitude >= input$main_map_bounds$south & latitude <= input$main_map_bounds$north & longitude >= input$main_map_bounds$west & longitude <= input$main_map_bounds$east) %>% 
                                                   dplyr::pull(scientificName) %>% 
                                                   as.character() %>% 
                                                   unique() %>% 
                                                   sort(),
                                                 selected = NULL, server = TRUE)
                            
                          })
  
}
