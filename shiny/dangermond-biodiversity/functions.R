### Custom functions for biodiversity portal app
get_count_raster <- function(records, base_raster, metric = c("records", "species", "limits")){
  
  # Ensure metric is one of the three options
  metric <- match.arg(metric)
  
  # Create result raster
  metric_raster <- base_raster
  
  # Calculate "records" metric (i.e. number of records per cell)
  if (metric == "records"){
    metric_counts <- records$cellID %>% table()
    # Update values of results raster with metric counts
    values(metric_raster)[names(metric_counts) %>% as.numeric()] <- metric_counts
  }
  
  # Calculate "species" metric (i.e. number of taxa per cell)
  if (metric == "species"){
    metric_counts <- records %>%
      dplyr::group_by(cellID) %>%
      dplyr::summarise(species_count = n_distinct(scientificName))
    # Update values of results raster with metric counts
    values(metric_raster)[metric_counts$cellID] <- metric_counts$species_count
  }

  # Create polygons from raster cells
  metric_raster_polys <- metric_raster %>% terra::as.polygons(aggregate = FALSE, na.rm = TRUE) %>% sf::st_as_sf()
  
  # Rename metric field
  names(metric_raster_polys)[1] <- "metric"
  
  # Add a polygon ID column
  metric_raster_polys <- metric_raster_polys %>%
    dplyr::mutate(ID = 1:nrow(metric_raster_polys))
  
  # Create output object
  out <- list(metric_raster = metric_raster, metric_raster_polys = metric_raster_polys)
  
  # Return output object
  return(out)
  
}