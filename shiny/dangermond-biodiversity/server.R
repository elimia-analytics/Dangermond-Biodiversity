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

integrated_dangermond_occurrences <- read_csv("https://raw.githubusercontent.com/elimia-analytics/Dangermond-Biodiversity/main/data/integrated_occurrences_dangermond.csv")
# dangermond_taxa_info_gbif <- read_csv("")
integrated_dangermond_occurrences <- integrated_dangermond_occurrences %>% 
  dplyr::left_join(dangermond_taxa_info_gbif %>% dplyr::select(user_supplied_name, classification_path, kingdom, phylum, class, order, family, genus, species), by = c("scientificName" = "user_supplied_name"))
integrated_dangermond_occurrences_sample <- integrated_dangermond_occurrences %>% 
  dplyr::sample_n(5000)

# Define server logic required to draw a histogram
function(input, output, session) {

  integrated_dangermond_occurrences_sf <- integrated_dangermond_occurrences_sample %>% 
    dplyr::filter(complete.cases(longitude, latitude)) %>% 
    dplyr::mutate(lon = longitude,
                  lat = latitude) %>% 
    sf::st_as_sf(
      coords = c("lon", "lat"),
      crs = 4326
    )
    
  ## Create web map and add basic elements and functionality
  output$main_map <- leaflet::renderLeaflet({
    
    points_bbox <- st_bbox(integrated_dangermond_occurrences_sf)
    
    m <- leaflet::leaflet(options = leafletOptions(zoomDelta = 0.5, zoomSnap = 0, attributionControl = FALSE, worldCopyJump = FALSE)) %>% # Open new leaflet web map
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
      ) %>%
      # flyToBounds(points_bbox[[1]], points_bbox[[2]], points_bbox[[3]], points_bbox[[4]], options = list(animate = TRUE, duration = 1, easeLinearity = 0.1, noMoveStart = TRUE)) %>% 
      addCircleMarkers( 
        data = integrated_dangermond_occurrences_sf,
        lng = ~longitude,
        lat = ~latitude,
        layerId = ~key,
        fillColor = "#4169E1",
        fillOpacity = 0.75,
        color = "#4169E1"
      )
    
    m
    
  })
  
  observeEvent(input$main_map_draw_new_feature, {

      if (input$main_map_draw_new_feature$geometry$type == "Polygon"){
        
        shinybusy::show_modal_spinner("circle", color = "#024b6c") # show the modal window
        
        session$sendCustomMessage("removeleaflet", list(elid="main_map", layerid=input$main_map_draw_new_feature$properties$`_leaflet_id`))
        
        drawn_shape_coordinates <- input$main_map_draw_new_feature$geometry$coordinates[[1]]
        
        pol <- st_polygon(
          list(
            cbind(
              purrr::map(drawn_shape_coordinates, 1) %>% unlist() %>% as.numeric(),
              purrr::map(drawn_shape_coordinates, 2) %>% unlist() %>% as.numeric()
            )
          )
        )
        
        pol_bbox <- st_bbox(pol)
        
        leafletProxy("main_map") %>%
          flyToBounds(pol_bbox[[1]], pol_bbox[[2]], pol_bbox[[3]], pol_bbox[[4]], options = list(animate = TRUE, duration = 1, easeLinearity = 0.1, noMoveStart = TRUE))
        
        shinybusy::remove_modal_spinner()
        
      }
        
    })
  
  output$time_plot <- dygraphs::renderDygraph({
    
    dat <- integrated_dangermond_occurrences_sample %>%
      dplyr::filter(year <= substr(Sys.Date(), 1, 4)) %>% 
      dplyr::group_by(eventDate) %>%
      dplyr::summarise(number_records = n()) %>%
      dplyr::mutate(eventDate = eventDate %>% as.Date()) %>% 
      dplyr::filter(complete.cases(eventDate))
      # dplyr::mutate(eventDate = purrr::map(eventDate, function(d) if (d < Sys.Date()){ d } else { NA } )) %>% 
      # dplyr::filter(complete.cases(eventDate))
    
    records_over_time <- xts::xts(x = dat$number_records, order.by = dat$eventDate)
    
    start_window <- min(dat$eventDate)
    end_window <- Sys.time()
    
    dygraphs::dygraph(records_over_time, ylab = "") %>%
      dygraphs::dyBarChart() %>%
      dygraphs::dySeries("V1", label = "Number of records", color = "#1f417d") %>%
      dygraphs::dyAxis(
        "y",
        axisLabelWidth = 0
      ) %>% 
      dygraphs::dyRangeSelector()#dateWindow = c(start_window %>% as.Date(), end_window %>% as.Date()))
    
  })
  
  observeEvent(input$time_plot_date_window, {
    print(input$time_plot_date_window)
  })
  
  output$taxa_donut <- plotly::renderPlotly({
    
    idees <- purrr::map(c("kingdom", "phylum", "class", "order", "family", "genus", "species"), function(x) integrated_dangermond_occurrences_sample[[x]][1:200] %>% unique()) %>% unlist() %>% na.omit() %>% as.character()
    parentals <- purrr::map(1:length(idees), function(i){
      target_class_path <- integrated_dangermond_occurrences_sample$classification_path[grep(idees[i], integrated_dangermond_occurrences_sample$classification_path)[1]]
      target_class_path_names <- (target_class_path %>% strsplit("\\|"))[[1]]
      target_class_path_names[grep(paste0("^", idees[i], "$"), target_class_path_names) - 1]
    })
    parentals <- c("", purrr::map(parentals, function(x) ifelse(length(x) > 0, x, "Life")) %>% unlist())
    idees <- c("Life", idees)
    
    library(plotly)
    trace1 <- list(
      leaf = list(opacity = 1), 
      meta = list(columnNames = list(
        ids = "data.0.ids", 
        labels = "data.0.labels", 
        parents = "data.0.parents"
      )), 
      mode = "markers", 
      type = "sunburst", 
      level = "Life", 
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
        range = c(-1, 6), 
        autorange = TRUE
      ), 
      yaxis = list(
        range = c(-1, 4), 
        autorange = TRUE
      ), 
      height = 800, 
      margin = list(
        b = 80, 
        r = 72, 
        t = 80, 
        pad = 6
      ), 
      metasrc = "kirudang:0:023680", 
      meta = c("white", "#EF553B", "rgb(251,128,114)", "rgb(251,128,114)", "rgb(251,128,114)", "rgb(251,128,114)", "rgb(251,128,114)", "rgb(251,128,114)", "rgb(251,128,114)", "rgb(251,128,114)", "#ba2020", "#ba2020", "#ba2020", "#ba2020", "#ba2020", "#ba2020", "#ba2020", "#ba2020", "#ba2020", "#ba2020", "rgb(252,195,195)", "rgb(252,195,195)", "#00CC96", "rgb(204,235,197)", "rgb(204,235,197)", "rgb(204,235,197)", "rgb(204,235,197)", "rgb(204,235,197)", "rgb(141,211,199)", "rgb(141,211,199)", "#a3e897", "#a3e897", "#a3e897", "#a3e897", "#a3e897", "#a3e897", "#a3e897", "#ffe600", "#faf693", "#faf693", "#faf693", "#ffd857", "#ffd857", "#ffd857", "#ffd857", "#fff16b", "#fff16b", "#fff16b", "#fff16b", "#ffb300", "#ffb300", "#ffb300", "#ffb300", "#ffb300", "#fcffc2", "#fcffc2", "#ff5ac3", "#ff5ac3", "#ff5ac3", "#ff5ac3", "#45abff", "#45abff", "#45abff", "#45abff", "#45abff", "#45abff", "#45abff", "#45abff", "#AB63FA", "#AB63FA", "#AB63FA"), 
      modebar = list(orientation = "v"), 
      autosize = FALSE, 
      dragmode = "select", 
      template = list(
        data = list(
          bar = list(
            list(
              type = "bar", 
              marker = list(colorbar = list(
                len = 0.2, 
                ticks = "inside", 
                ticklen = 6, 
                tickcolor = "rgb(237,237,237)", 
                outlinewidth = 0
              ))
            )
          ), 
          table = list(
            list(
              type = "table", 
              cells = list(
                fill = list(color = "rgb(237,237,237)"), 
                line = list(color = "white")
              ), 
              header = list(
                fill = list(color = "rgb(217,217,217)"), 
                line = list(color = "white")
              )
            )
          ), 
          carpet = list(
            list(
              type = "carpet", 
              aaxis = list(
                gridcolor = "white", 
                linecolor = "white", 
                endlinecolor = "rgb(51,51,51)", 
                minorgridcolor = "white", 
                startlinecolor = "rgb(51,51,51)"
              ), 
              baxis = list(
                gridcolor = "white", 
                linecolor = "white", 
                endlinecolor = "rgb(51,51,51)", 
                minorgridcolor = "white", 
                startlinecolor = "rgb(51,51,51)"
              )
            )
          ), 
          mesh3d = list(
            list(
              type = "mesh3d", 
              colorbar = list(
                len = 0.2, 
                ticks = "inside", 
                ticklen = 6, 
                tickcolor = "rgb(237,237,237)", 
                outlinewidth = 0
              )
            )
          ), 
          contour = list(
            list(
              type = "contour", 
              colorbar = list(
                len = 0.2, 
                ticks = "inside", 
                ticklen = 6, 
                tickcolor = "rgb(237,237,237)", 
                outlinewidth = 0
              ), 
              autocolorscale = TRUE
            )
          ), 
          heatmap = list(
            list(
              type = "heatmap", 
              colorbar = list(
                len = 0.2, 
                ticks = "inside", 
                ticklen = 6, 
                tickcolor = "rgb(237,237,237)", 
                outlinewidth = 0
              ), 
              autocolorscale = TRUE
            )
          ), 
          scatter = list(
            list(
              type = "scatter", 
              marker = list(colorbar = list(
                len = 0.2, 
                ticks = "inside", 
                ticklen = 6, 
                tickcolor = "rgb(237,237,237)", 
                outlinewidth = 0
              ))
            )
          ), 
          surface = list(
            list(
              type = "surface", 
              colorbar = list(
                len = 0.2, 
                ticks = "inside", 
                ticklen = 6, 
                tickcolor = "rgb(237,237,237)", 
                outlinewidth = 0
              )
            )
          ), 
          heatmapgl = list(
            list(
              type = "heatmapgl", 
              colorbar = list(
                len = 0.2, 
                ticks = "inside", 
                ticklen = 6, 
                tickcolor = "rgb(237,237,237)", 
                outlinewidth = 0
              )
            )
          ), 
          histogram = list(
            list(
              type = "histogram", 
              marker = list(colorbar = list(
                len = 0.2, 
                ticks = "inside", 
                ticklen = 6, 
                tickcolor = "rgb(237,237,237)", 
                outlinewidth = 0
              ))
            )
          ), 
          parcoords = list(
            list(
              line = list(colorbar = list(
                len = 0.2, 
                ticks = "inside", 
                ticklen = 6, 
                tickcolor = "rgb(237,237,237)", 
                outlinewidth = 0
              )), 
              type = "parcoords"
            )
          ), 
          scatter3d = list(
            list(
              type = "scatter3d", 
              marker = list(colorbar = list(
                len = 0.2, 
                ticks = "inside", 
                ticklen = 6, 
                tickcolor = "rgb(237,237,237)", 
                outlinewidth = 0
              ))
            )
          ), 
          scattergl = list(
            list(
              type = "scattergl", 
              marker = list(colorbar = list(
                len = 0.2, 
                ticks = "inside", 
                ticklen = 6, 
                tickcolor = "rgb(237,237,237)", 
                outlinewidth = 0
              ))
            )
          ), 
          choropleth = list(
            list(
              type = "choropleth", 
              colorbar = list(
                len = 0.2, 
                ticks = "inside", 
                ticklen = 6, 
                tickcolor = "rgb(237,237,237)", 
                outlinewidth = 0
              )
            )
          ), 
          scattergeo = list(
            list(
              type = "scattergeo", 
              marker = list(colorbar = list(
                len = 0.2, 
                ticks = "inside", 
                ticklen = 6, 
                tickcolor = "rgb(237,237,237)", 
                outlinewidth = 0
              ))
            )
          ), 
          histogram2d = list(
            list(
              type = "histogram2d", 
              colorbar = list(
                len = 0.2, 
                ticks = "inside", 
                ticklen = 6, 
                tickcolor = "rgb(237,237,237)", 
                outlinewidth = 0
              ), 
              autocolorscale = TRUE
            )
          ), 
          scatterpolar = list(
            list(
              type = "scatterpolar", 
              marker = list(colorbar = list(
                len = 0.2, 
                ticks = "inside", 
                ticklen = 6, 
                tickcolor = "rgb(237,237,237)", 
                outlinewidth = 0
              ))
            )
          ), 
          contourcarpet = list(
            list(
              type = "contourcarpet", 
              colorbar = list(
                len = 0.2, 
                ticks = "inside", 
                ticklen = 6, 
                tickcolor = "rgb(237,237,237)", 
                outlinewidth = 0
              )
            )
          ), 
          scattercarpet = list(
            list(
              type = "scattercarpet", 
              marker = list(colorbar = list(
                len = 0.2, 
                ticks = "inside", 
                ticklen = 6, 
                tickcolor = "rgb(237,237,237)", 
                outlinewidth = 0
              ))
            )
          ), 
          scattermapbox = list(
            list(
              type = "scattermapbox", 
              marker = list(colorbar = list(
                len = 0.2, 
                ticks = "inside", 
                ticklen = 6, 
                tickcolor = "rgb(237,237,237)", 
                outlinewidth = 0
              ))
            )
          ), 
          scatterpolargl = list(
            list(
              type = "scatterpolargl", 
              marker = list(colorbar = list(
                len = 0.2, 
                ticks = "inside", 
                ticklen = 6, 
                tickcolor = "rgb(237,237,237)", 
                outlinewidth = 0
              ))
            )
          ), 
          scatterternary = list(
            list(
              type = "scatterternary", 
              marker = list(colorbar = list(
                len = 0.2, 
                ticks = "inside", 
                ticklen = 6, 
                tickcolor = "rgb(237,237,237)", 
                outlinewidth = 0
              ))
            )
          ), 
          histogram2dcontour = list(
            list(
              type = "histogram2dcontour", 
              colorbar = list(
                len = 0.2, 
                ticks = "inside", 
                ticklen = 6, 
                tickcolor = "rgb(237,237,237)", 
                outlinewidth = 0
              ), 
              autocolorscale = TRUE
            )
          )
        ), 
        layout = list(
          geo = list(
            bgcolor = "white", 
            showland = TRUE, 
            lakecolor = "white", 
            landcolor = "rgb(237,237,237)", 
            showlakes = TRUE, 
            subunitcolor = "white"
          ), 
          font = list(color = "rgb(51,51,51)"), 
          polar = list(
            bgcolor = "rgb(237,237,237)", 
            radialaxis = list(
              ticks = "outside", 
              showgrid = TRUE, 
              gridcolor = "white", 
              linecolor = "white", 
              tickcolor = "rgb(51,51,51)"
            ), 
            angularaxis = list(
              ticks = "outside", 
              showgrid = TRUE, 
              gridcolor = "white", 
              linecolor = "white", 
              tickcolor = "rgb(51,51,51)"
            )
          ), 
          scene = list(
            xaxis = list(
              ticks = "outside", 
              showgrid = TRUE, 
              gridcolor = "white", 
              gridwidth = 2, 
              linecolor = "white", 
              tickcolor = "rgb(51,51,51)", 
              zerolinecolor = "white", 
              showbackground = TRUE, 
              backgroundcolor = "rgb(237,237,237)"
            ), 
            yaxis = list(
              ticks = "outside", 
              showgrid = TRUE, 
              gridcolor = "white", 
              gridwidth = 2, 
              linecolor = "white", 
              tickcolor = "rgb(51,51,51)", 
              zerolinecolor = "white", 
              showbackground = TRUE, 
              backgroundcolor = "rgb(237,237,237)"
            ), 
            zaxis = list(
              ticks = "outside", 
              showgrid = TRUE, 
              gridcolor = "white", 
              gridwidth = 2, 
              linecolor = "white", 
              tickcolor = "rgb(51,51,51)", 
              zerolinecolor = "white", 
              showbackground = TRUE, 
              backgroundcolor = "rgb(237,237,237)"
            )
          ), 
          xaxis = list(
            ticks = "outside", 
            showgrid = TRUE, 
            gridcolor = "white", 
            linecolor = "white", 
            tickcolor = "rgb(51,51,51)", 
            automargin = TRUE, 
            zerolinecolor = "white"
          ), 
          yaxis = list(
            ticks = "outside", 
            showgrid = TRUE, 
            gridcolor = "white", 
            linecolor = "white", 
            tickcolor = "rgb(51,51,51)", 
            automargin = TRUE, 
            zerolinecolor = "white"
          ), 
          ternary = list(
            aaxis = list(
              ticks = "outside", 
              showgrid = TRUE, 
              gridcolor = "white", 
              linecolor = "white", 
              tickcolor = "rgb(51,51,51)"
            ), 
            baxis = list(
              ticks = "outside", 
              showgrid = TRUE, 
              gridcolor = "white", 
              linecolor = "white", 
              tickcolor = "rgb(51,51,51)"
            ), 
            caxis = list(
              ticks = "outside", 
              showgrid = TRUE, 
              gridcolor = "white", 
              linecolor = "white", 
              tickcolor = "rgb(51,51,51)"
            ), 
            bgcolor = "rgb(237,237,237)"
          ), 
          colorway = c("#F8766D", "#A3A500", "#00BF7D", "#00B0F6", "#E76BF3"), 
          hovermode = "closest", 
          colorscale = list(
            diverging = list(c(0, "#40004b"),list(0.1, "#762a83"),list(0.2, "#9970ab"),list(0.3, "#c2a5cf"),list(0.4, "#e7d4e8"),list(0.5, "#f7f7f7"),list(0.6, "#d9f0d3"),list(0.7, "#a6dba0"),list(0.8, "#5aae61"),list(0.9, "#1b7837"),list(1, "#00441b")), 
            sequential = list(c(0, "rgb(20,44,66)"),list(1, "rgb(90,179,244)")), 
            sequentialminus = list(c(0, "rgb(20,44,66)"),list(1, "rgb(90,179,244)"))
          ), 
          plot_bgcolor = "rgb(237,237,237)", 
          paper_bgcolor = "white", 
          shapedefaults = list(
            line = list(width = 0), 
            opacity = 0.3, 
            fillcolor = "black"
          ), 
          annotationdefaults = list(
            arrowhead = 0, 
            arrowwidth = 1
          )
        ), 
        themeRef = "GGPLOT2"
      ), 
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
      # sunburstcolorway = c("#fbb4ae", "#b3cde3", "#ccebc5", "#decbe4", "#fed9a6", "#ffffcc", "#e5d8bd", "#fddaec", "#f2f2f2"), 
      # extendsunburstcolors = FALSE
    )
    p <- plot_ly(source = "taxa_plot", customdata = idees)
    p <- add_trace(p, leaf=trace1$leaf, meta=trace1$meta, mode=trace1$mode, type=trace1$type, level=trace1$level, idssrc=trace1$idssrc, ids=trace1$ids, maxdepth=trace1$maxdepth, rotation=trace1$rotation, labelssrc=trace1$labelssrc, labels=trace1$labels, parentssrc=trace1$parentssrc, parents=trace1$parents, hovertemplate=trace1$hovertemplate)
    p <- layout(p, font=layout$font, title=layout$title, xaxis=layout$xaxis, yaxis=layout$yaxis, margin=layout$margin, metasrc=layout$metasrc, meta=layout$meta, modebar=layout$modebar, autosize=layout$autosize, dragmode=layout$dragmode, template=layout$template, clickmode=layout$clickmode, hovermode=layout$hovermode, hoverlabel=layout$hoverlabel, separators=layout$separators, uniformtext=layout$uniformtext, selectdirection=layout$selectdirection, sunburstcolorway=layout$sunburstcolorway, extendsunburstcolors=layout$extendsunburstcolors)
    p <- p %>% event_register("plotly_hover")
    p
  })
  
  hovered_taxa <- reactiveValues(taxon = vector(mode = "character"))
  clicked_taxa <- reactiveValues(taxon = vector(mode = "character"))
  center_taxon <- reactiveValues(name = vector(mode = "character"))
  
  hoverData <- reactive({
    currentEventData <- unlist(event_data(event = "plotly_hover", source = "taxa_plot", priority = "event"))
    currentEventData
  })
  
  # observeEvent(event_data(event = "plotly_hover",
  #                         source = "taxa_plot",
  #                         priority = "event"), {
  #                           # clicked <- event_data(event = "plotly_sunburstclick",
  #                           #                       source = "taxa_plot",
  #                           #                       priority = "event")
  #                           # hovered <- event_data(event = "plotly_hover",
  #                           #                       source = "taxa_plot",
  #                           #                       priority = "event")
  #                           # 
  #                           #   # print(clicked)
  #                           hovered_taxa$taxon <- c(hovered_taxa$taxon, hoverData()[["customdata"]])
  #                           print(hovered_taxa$taxon)
  #                         })
  
  clickData <- reactive({
    currentEventData <- unlist(event_data(event = "plotly_sunburstclick", source = "taxa_plot", priority = "event"))
    currentEventData
  })
  
  observeEvent(event_data(event = "plotly_sunburstclick",
                          source = "taxa_plot",
                          priority = "event"), {
                            clicked_taxa$taxon <- c(clicked_taxa$taxon, clickData()[["customdata"]])
                            center_taxon <- NULL
                            if (length(clicked_taxa$taxon) == 1){
                              center_taxon <- clickData()[["customdata"]]
                            }
                            if (length(clicked_taxa$taxon) > 1){
                              last <- clicked_taxa$taxon[length(clicked_taxa$taxon)]
                              last_taxon_path <- integrated_dangermond_occurrences_sample$classification_path[grep(last, integrated_dangermond_occurrences_sample$classification_path)[1]]
                              last_taxon_path_names <- c("Life", (last_taxon_path %>% strsplit("\\|"))[[1]])
                              beforelast <- clicked_taxa$taxon[(length(clicked_taxa$taxon)-1)]
                              beforelast_taxon_path <- integrated_dangermond_occurrences_sample$classification_path[grep(beforelast, integrated_dangermond_occurrences_sample$classification_path)[1]]
                              beforelast_taxon_path_names <- c("Life", (beforelast_taxon_path %>% strsplit("\\|"))[[1]])
                              if (last != beforelast){
                                if (grep(last, last_taxon_path_names) > grep(beforelast, last_taxon_path_names)){
                                  center_taxon <- last
                                } else {
                                  center_taxon <- last_taxon_path_names[grep(last, last_taxon_path_names)-1]
                                  clicked_taxa$taxon[(length(clicked_taxa$taxon))] <- center_taxon
                                }
                              }
                              if (last == beforelast){
                                center_taxon <- last_taxon_path_names[grep(last, last_taxon_path_names)-1]
                                clicked_taxa$taxon[(length(clicked_taxa$taxon))] <- center_taxon
                              }
                              print(c(beforelast, last))
                            }
                            print(center_taxon)
                          })

  # output$hoverDataOut <- renderText({
  #   # paste("Hover data:", paste(names(hoverData()), unlist(hoverData()), sep = ": ", collapse = " | "))
  #   paste(hoverData())
  # })
  # 
  # output$clickDataOut <- renderText({
  #   paste("Click data:", paste(names(clickData()), unlist(clickData()), sep = ": ", collapse = " | "))
  # })
  # 
  # output$centerpiece <- renderText({
  #   paste("")
  # })
  
}
