Second Species
================

``` r
obs = read_observations(SPECIES)
obs
```

    ## Simple feature collection with 229 features and 7 fields
    ## Geometry type: POINT
    ## Dimension:     XY
    ## Bounding box:  xmin: -73.65473 ymin: 38.86667 xmax: -66.48333 ymax: 40.8686
    ## Geodetic CRS:  WGS 84
    ## # A tibble: 229 × 8
    ##    id             basisOfRecord eventDate   year month eventTime individualCount
    ##  * <chr>          <chr>         <date>     <dbl> <fct> <chr>               <dbl>
    ##  1 013c4ceb-e772… HumanObserva… 1993-06-13  1993 Jun   <NA>                   NA
    ##  2 017829ee-4e4e… HumanObserva… 2005-09-07  2005 Sep   <NA>                   NA
    ##  3 02265dd6-aa8d… HumanObserva… 2005-08-14  2005 Aug   <NA>                   NA
    ##  4 03f763ee-e34c… HumanObserva… 2005-08-13  2005 Aug   <NA>                   NA
    ##  5 0425839c-1a03… HumanObserva… 2004-10-02  2004 Oct   <NA>                   NA
    ##  6 05d6057d-621d… HumanObserva… 2005-09-07  2005 Sep   <NA>                   NA
    ##  7 0667f697-229f… HumanObserva… 2005-08-12  2005 Aug   <NA>                   NA
    ##  8 09565414-350e… HumanObserva… 2004-10-19  2004 Oct   <NA>                   NA
    ##  9 0a0884fc-36e2… HumanObserva… 2003-08-07  2003 Aug   <NA>                   NA
    ## 10 0a9b2c1f-03ce… HumanObserva… 2000-09-18  2000 Sep   <NA>                   NA
    ## # ℹ 219 more rows
    ## # ℹ 1 more variable: geom <POINT [°]>

``` r
db = brickman_database() |>
  filter(scenario == "STATIC", var == "mask")
mask = read_brickman(db)

all_counts = count(st_drop_geometry(obs), month)

thinned_obs = sapply(month.abb,
                     function(mon){
                       temp_x = obs |> filter(month == mon)
                       if(nrow(temp_x) == 0) return(NULL)
                       thin_by_cell(obs |> filter(month == mon), mask)
                     }, simplify = FALSE) |> dplyr::bind_rows()

# another count
thinned_counts = count(st_drop_geometry(thinned_obs), month)

bias_map = rasterize_point_density(obs, mask)

# average background point count
nback_avg = mean(all_counts$n) |> round()

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

write_model_input(obsbkg, scientificname = SPECIES)
x = read_model_input(scientificname = SPECIES)
```

``` r
db = brickman_database() |>
  dplyr::filter(scenario == "PRESENT", interval == "mon")
present = read_brickman(db)
```

``` r
keep = filter_collinear(present, method = "cor_caret", cutoff = 0.65)
keep = c("depth", "month", keep)
```

``` r
model_input = read_model_input(scientificname = SPECIES)
model_input
```

    ## Simple feature collection with 334 features and 2 fields
    ## Geometry type: POINT
    ## Dimension:     XY
    ## Bounding box:  xmin: -73.65773 ymin: 38.86667 xmax: -66.48333 ymax: 40.8686
    ## Geodetic CRS:  WGS 84
    ## # A tibble: 334 × 3
    ##    month class                      geom
    ##    <fct> <fct>               <POINT [°]>
    ##  1 Jun   presence   (-66.48333 40.46667)
    ##  2 Jun   presence       (-71.00834 39.5)
    ##  3 Jun   presence     (-66.49167 40.575)
    ##  4 Jun   presence     (-71.45833 39.825)
    ##  5 Jun   presence       (-71.175 39.675)
    ##  6 Jun   presence     (-70.975 39.85833)
    ##  7 Jun   presence   (-70.14167 39.64167)
    ##  8 Jun   presence   (-70.00834 39.29167)
    ##  9 Jun   background  (-67.98153 39.7846)
    ## 10 Jun   background (-70.12039 39.62007)
    ## # ℹ 324 more rows

``` r
present = read_brickman(add = c("depth"))
```

``` r
variables = extract_brickman(present, model_input, form = "wide")
variables
```

    ## Simple feature collection with 334 features and 12 fields
    ## Geometry type: POINT
    ## Dimension:     XY
    ## Bounding box:  xmin: -73.65773 ymin: 38.86667 xmax: -66.48333 ymax: 40.8686
    ## Geodetic CRS:  WGS 84
    ## # A tibble: 334 × 13
    ##    .id   month class      depth   MLD  Sbtm   SSS   SST  Tbtm        U         V
    ##    <chr> <fct> <fct>      <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>    <dbl>     <dbl>
    ##  1 p001  Jun   presence   2550.  3.63  35.0  31.8  14.5  3.39 -0.0468  -0.0496  
    ##  2 p002  Jun   presence   2346.  3.86  35.0  32.1  17.2  3.50 -0.126   -0.0322  
    ##  3 p003  Jun   presence   2068.  3.31  35.0  31.6  14.0  3.70 -0.0534  -0.108   
    ##  4 p004  Jun   presence    972.  3.04  34.9  31.5  16.6  4.42 -0.0282  -0.0172  
    ##  5 p005  Jun   presence   1894.  3.39  35.0  31.8  16.9  3.77 -0.0394  -0.0420  
    ##  6 p006  Jun   presence    941.  3.11  34.9  31.6  16.4  4.45 -0.00356 -0.00145 
    ##  7 p007  Jun   presence   2210.  3.52  35.0  32.0  16.7  3.55 -0.0985  -0.00973 
    ##  8 p008  Jun   presence   2628.  4.37  35.0  32.5  17.7  3.23 -0.0374  -0.000418
    ##  9 p009  Jun   background 2718.  3.88  35.0  32.0  16.1  3.29 -0.0582  -0.0118  
    ## 10 p010  Jun   background 2210.  3.52  35.0  32.0  16.7  3.55 -0.0985  -0.00973 
    ## # ℹ 324 more rows
    ## # ℹ 2 more variables: Xbtm <dbl>, geom <POINT [°]>

``` r
cfg = list(
  version = "v1",
  scientificname = SPECIES,
  background = "average of observations per month",
  keep_vars =  keep)
```

``` r
ok = make_path(data_path("models")) # make a directory for models
write_configuration(cfg)            
```

``` r
write_model_input(variables, scientificname = SPECIES, version = "v1")
```
