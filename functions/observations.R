
read_observations = function(scientificname = "Dermochelys coriacea",
                             minimum_year = 1970,
                             basis_record = c(),
                             target_month = c(),
                             ...){
  
  #' Read raw OBIS data and then filter it
  #' 
  #' @param scientificname chr, the name of the species to read
  #' @param minimum_year num, the earliest year of observation to accept
  #' @param basis_record chr, the type of record to keep
  #' @param target_month chr, July-November (use abbreviations such as "Jul")
  #' @param ... other arguments passed to `read_obis()`
  #' @return a filtered table of observations
  
  # Happy coding!
  
  # read in the raw data
  x = read_obis(scientificname, ...) |>
    dplyr::mutate(month = factor(month, levels = month.abb))|>
    dplyr::filter(!is.na(eventDate))
  
  # if the user provided a non-NULL filter by year
  if (!is.null(minimum_year)){
    x = x |>
      filter(year >= minimum_year)
  }
  
  # if the user provided a non-NULL filter by basis of record
  if (!is.null(basis_record)){
    x = x |> filter(basisOfRecord %in% basis_record)
  }
  
  # if the user provided a non-NULL filter by month
  if (!is.null(target_month)){
    x = x |>filter(month %in% target_month)
  }
  
  return(x)
}
