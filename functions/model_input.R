
read_model_input = function(scientificname = "Mola mola",
                            version = NULL,
                            log_me = NULL,
                            form = c("table", "sf")[2],
                            na.rm = TRUE,
                            path = data_path("model_input")){
  
  #' Reads a model input file given species and path
  #' 
  #' @param scientificname chr, the species name
  #' @param version chr or NULL, if not NULL add this to the filename. A version 
  #'   identifier like "v2.01", etc.
  #' @param log_me NULL or str with one or more variables to take the log of
  #' @param form string, one of 'table' or 'sf'  The are the same except that the
  #'   former lacks spatial information
  #' @param na.rm logical, if TRUE remove any rows with any NA values
  #' @param path chr the path to the data directory
  
  spname = gsub(" ", "_", scientificname, fixed = TRUE)
  fname = if (is.null(version)){
    sprintf("%s-model_input.gpkg", spname)
  } else {
    sprintf("%s-%s-model_input.gpkg", spname, version)
  }
  
  filename = file.path(path[1], fname[1])
  
  if (!file.exists(filename)) {
    message("file not found:", filename[1])
    return(NULL)
  }
  
  x = sf::read_sf(filename)
  if (tolower(form[1]) != "sf") x = sf::st_drop_geometry(x) |> dplyr::as_tibble()
  if ("month" %in% names(x)) x = dplyr::mutate(x, month = factor(month, levels = month.abb))
  if ("class" %in% names(x)) x = dplyr::mutate(x, class = factor(class, levels = c("presence", "background")))
  if (!is.null(log_me)){
    x = x |>
      dplyr::mutate(dplyr::across(dplyr::any_of(log_me), ~ log(.x, base = 10)))
  }
  if (na.rm) x = na.omit(x)
  return(x)
}

write_model_input = function(x,
                             scientificname = "Mola mola",
                             version = NULL,
                             path = data_path("model_input")){
  
  #' @rdname read_model_input
  path = make_path(path)
  spname = gsub(" ", "_", scientificname, fixed = TRUE)
  fname = if (is.null(version)){
    sprintf("%s-model_input.gpkg", spname)
  } else {
    sprintf("%s-%s-model_input.gpkg", spname, version)
  }
  filename = file.path(path, fname)
  x = sf::write_sf(x, filename)
  invisible(x)
}
  
  