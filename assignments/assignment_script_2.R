# Tatar Anurakwongsri
# Assignment 2

# For each month select at random one presence and one background point 
# (so, that will be 2 x 12 = 24 points!) from your model input data.  
# Then select three (3) variables in the Brickman present monthly data set, and 
# build a single table that has the three variables for the 24 points.

source("setup.R")
SPECIES = "Dermochelys coriacea"
obs = read_observations(scientificname = SPECIES)

db = brickman_database() |>
  filter(scenario == "STATIC", var == "mask")
mask = read_brickman(db)

# Get all observation counts by month
LON0 = -67
LAT0 = 46
all_counts = count(st_drop_geometry(obs), month)
all_counts

# Thin data to get 1 observation per cell
thinned_obs = sapply(month.abb,
                     function(mon){
                       temp_x = obs |> filter(month == mon)
                       if(nrow(temp_x) == 0) return(NULL)
                       thin_by_cell(obs |> filter(month == mon), mask)
                     }, simplify = FALSE) |> dplyr::bind_rows()

# another count
thinned_counts = count(st_drop_geometry(thinned_obs), month)

# bias map using mask and original observations
bias_map = rasterize_point_density(obs, mask)

# average background point count
nback_avg = mean(all_counts$n) |>
  round()
nback_avg

# sample for background points using thinned observations and bias map
obsbkg = sapply(month.abb,
                function(mon){
                  temp_x = obs |> filter(month == mon)
                  if(nrow(temp_x) == 0) return(NULL)
                  sample_background(thinned_obs |> filter(month == mon), # <- just this month
                                    bias_map,
                                    method = "bias",  # <-- it needs to know it's a bias map
                                    return_pres = TRUE, # <-- give me the obs back, too
                                    n = nback_avg) |>   # <-- how many points
                    mutate(month = mon, .before = 1)
                }, simplify = FALSE) |>
  bind_rows() |>
  mutate(month = factor(month, levels = month.abb))

thinned_obs = sapply(month.abb,
                     function(mon){
                       temp_x = obs |> filter(month == mon)
                       if(nrow(temp_x) == 0) return(NULL)
                       thin_by_cell(obs |> filter(month == mon), mask)}, 
                     simplify = FALSE) |> dplyr::bind_rows()

# check if nback_avg is used every month
count(st_drop_geometry(obsbkg), month, class)

# create table of points categorized as presence or background, grouped by month
write_model_input(obsbkg, scientificname = SPECIES)
x = read_model_input(scientificname = SPECIES)

# group table by month and class
result = x |> 
  dplyr::group_by(month, class) |>
  dplyr::slice_sample() |>
  dplyr::ungroup()

# Load brickman data
DB = brickman_database()
db = DB |>
  dplyr::filter(scenario == "PRESENT",
                interval == "mon",
                var %in% c("MLD","Sbtm","SSS"))

filtered_data = read_brickman(db)

# Get points from result data frame
extracted_data = extract_brickman(filtered_data, result, form = "wide") |>
  print()

