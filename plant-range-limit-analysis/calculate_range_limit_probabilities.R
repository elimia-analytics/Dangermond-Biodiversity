calculate_range_limit_probabilities <- function(probability_raster, limit = c("southern", "northern")){
  ## Match limit argument
  limit <- match.arg(limit)
  ## Extract probability raster values
  probability_values <- terra::values(probability_raster)
  ## Calculate rowwise summed probability values
  latitudinal_probability <- purrr::map(1:nrow(probability_raster), function(r){
    row_values <- (probability_raster[r, ])[, 1]
    row_values_sum <- sum(row_values, na.rm = TRUE)
    rep(row_values_sum, length(row_values))
  })
  ## Extract cumulative latitudinal probability values
  latitudinal_cumulative_sum <- purrr::map_dbl(latitudinal_probability, function(x) ifelse(x[1] > 0, x[1], 0)) %>% cumsum()
  ## If limit is southern focus on the cumulative lowest latitude 10% and the southern neighboring cells
  if (limit == "southern"){
    limit_cells <- which(latitudinal_cumulative_sum/max(latitudinal_cumulative_sum, na.rm = TRUE) >= .9)
    adjacent_cell_index <- 4
  }
  ## If limit is northern focus on the cumulative highest latitude 10% and the northern neighboring cells
  if (limit == "northern"){
    limit_cells <- which(latitudinal_cumulative_sum/max(latitudinal_cumulative_sum, na.rm = TRUE) <= .1)
    adjacent_cell_index <- 1
  }
  ## Identify cells potentially representing range limit
  limit_cells <- purrr::map(1:length(limit_cells), function(x) cellFromRowCol(probability_raster, row = limit_cells[x], 1:ncol(probability_raster))) %>% unlist()
  limit_cells <- intersect(limit_cells, which(!is.na(terra::values(probability_raster))))
  ## Extract range limit probabilities across potential range limit cells
  limit_probabilities <- purrr::map_dbl(1:length(probability_values), function(focal_cell){
    limit_neighbor <- (terra::adjacent(probability_raster, cells = focal_cell, directions = "rook"))[1, adjacent_cell_index]
    edge_probability <- (probability_values[focal_cell]-probability_values[limit_neighbor])*(1-probability_values[limit_neighbor])
    edge_probability <- ifelse(!is.na(edge_probability) & edge_probability > 0, edge_probability, 0)
    edge_probability
  })
  ## Create range edge raster
  edge_raster <- probability_raster
  values(edge_raster) <- NA
  edge_raster[!is.na(probability_raster)] <- 0
  edge_raster[limit_cells] <- limit_probabilities[limit_cells]
  
  latitudinal_probability_raster <- probability_raster
  values(latitudinal_probability_raster) <- NA
  latitudinal_probability_raster[!is.na(latitudinal_probability_raster)] <- 0
  values(latitudinal_probability_raster)[limit_cells] <- (latitudinal_probability %>% unlist())[limit_cells]
  
  out <- list(latitudinal_probability_raster = latitudinal_probability_raster,
              edge_raster = edge_raster
              )
  return(out)
}
