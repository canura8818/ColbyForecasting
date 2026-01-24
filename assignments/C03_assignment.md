Second Species
================

``` r
source("/home/canura26/ColbyForecasting/setup.R")
for (f in list.files("/home/canura26/ColbyForecasting/functions",
     pattern = glob2rx("*.R"), full.names = TRUE)){
  source(f)
}

SPECIES = "Aurelia aurita"
```

``` r
obs = read_observations(SPECIES)
obs
```

    ## Simple feature collection with 188 features and 6 fields
    ## Geometry type: POINT
    ## Dimension:     XY
    ## Bounding box:  xmin: -74.73 ymin: 38.818 xmax: -66.71948 ymax: 45.08652
    ## Geodetic CRS:  WGS 84
    ## # A tibble: 188 × 7
    ##    id                       basisOfRecord eventDate   year month individualCount
    ##  * <chr>                    <chr>         <date>     <dbl> <fct>           <dbl>
    ##  1 0110251c-5d31-4d94-ac9d… MaterialSamp… 2021-10-14  2021 Oct                NA
    ##  2 014d83b5-ba3a-41eb-876f… MaterialSamp… 2021-05-26  2021 May                NA
    ##  3 03a2a6b6-a9c8-4867-994f… MaterialSamp… 2021-07-14  2021 Jul                NA
    ##  4 0429bdf2-41a3-42ac-8c1c… MaterialSamp… 2019-07-16  2019 Jul                NA
    ##  5 08550e04-8849-4dc5-8875… HumanObserva… 2003-08-05  2003 Aug                NA
    ##  6 08a45452-fd1d-4f96-88f0… MaterialSamp… 2019-12-13  2019 Dec                NA
    ##  7 09219f6d-3cd7-447b-991e… HumanObserva… 2000-08-16  2000 Aug                NA
    ##  8 0aab173e-cd49-4eda-89a5… materialSamp… 2018-01-08  2018 Jan                NA
    ##  9 0b89cfb2-7a4f-476a-a6e7… MaterialSamp… 2021-07-14  2021 Jul                NA
    ## 10 0bb7fbf5-7d05-4bcc-ac9b… MaterialSamp… 2021-07-14  2021 Jul                NA
    ## # ℹ 178 more rows
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
```

    ## Warning in sample_background(filter(thinned_obs, month == mon), bias_map, : There are fewer available cells for raster 'NA' (4 presences) than the requested 17 background points. Only 11 will be returned.

    ## Warning in sample_background(filter(thinned_obs, month == mon), bias_map, : There are fewer available cells for raster 'NA' (2 presences) than the requested 17 background points. Only 11 will be returned.
    ## Warning in sample_background(filter(thinned_obs, month == mon), bias_map, : There are fewer available cells for raster 'NA' (2 presences) than the requested 17 background points. Only 11 will be returned.

    ## Warning in sample_background(filter(thinned_obs, month == mon), bias_map, : There are fewer available cells for raster 'NA' (3 presences) than the requested 17 background points. Only 11 will be returned.

    ## Warning in sample_background(filter(thinned_obs, month == mon), bias_map, : There are fewer available cells for raster 'NA' (2 presences) than the requested 17 background points. Only 11 will be returned.
    ## Warning in sample_background(filter(thinned_obs, month == mon), bias_map, : There are fewer available cells for raster 'NA' (2 presences) than the requested 17 background points. Only 11 will be returned.

    ## Warning in sample_background(filter(thinned_obs, month == mon), bias_map, : There are fewer available cells for raster 'NA' (6 presences) than the requested 17 background points. Only 11 will be returned.

    ## Warning in sample_background(filter(thinned_obs, month == mon), bias_map, : There are fewer available cells for raster 'NA' (3 presences) than the requested 17 background points. Only 11 will be returned.

    ## Warning in sample_background(filter(thinned_obs, month == mon), bias_map, : There are fewer available cells for raster 'NA' (6 presences) than the requested 17 background points. Only 11 will be returned.

    ## Warning in sample_background(filter(thinned_obs, month == mon), bias_map, : There are fewer available cells for raster 'NA' (3 presences) than the requested 17 background points. Only 11 will be returned.

    ## Warning in sample_background(filter(thinned_obs, month == mon), bias_map, : There are fewer available cells for raster 'NA' (2 presences) than the requested 17 background points. Only 11 will be returned.

``` r
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

    ## Simple feature collection with 156 features and 2 fields
    ## Geometry type: POINT
    ## Dimension:     XY
    ## Bounding box:  xmin: -73.5023 ymin: 40.50407 xmax: -66.73293 ymax: 45.08652
    ## Geodetic CRS:  WGS 84
    ## # A tibble: 156 × 3
    ##    month class                      geom
    ##    <fct> <fct>               <POINT [°]>
    ##  1 Jan   presence    (-66.9824 44.93336)
    ##  2 Jan   presence     (-67.0988 45.0853)
    ##  3 Jan   presence    (-67.0064 45.03271)
    ##  4 Jan   presence    (-66.83898 44.9396)
    ##  5 Jan   background (-67.07663 45.04948)
    ##  6 Jan   background (-66.99437 45.04948)
    ##  7 Jan   background (-66.74758 45.04948)
    ##  8 Jan   background (-66.99437 44.96722)
    ##  9 Jan   background (-66.82984 44.96722)
    ## 10 Jan   background   (-70.20265 43.651)
    ## # ℹ 146 more rows

``` r
present = read_brickman(add = c("depth"))
```

``` r
variables = extract_brickman(present, model_input, form = "wide")
variables
```

    ## Simple feature collection with 156 features and 12 fields
    ## Geometry type: POINT
    ## Dimension:     XY
    ## Bounding box:  xmin: -73.5023 ymin: 40.50407 xmax: -66.73293 ymax: 45.08652
    ## Geodetic CRS:  WGS 84
    ## # A tibble: 156 × 13
    ##    .id   month class      depth   MLD  Sbtm   SSS   SST  Tbtm        U       V
    ##    <chr> <fct> <fct>      <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>    <dbl>   <dbl>
    ##  1 p001  Jan   presence    14.8  6.93  30.7  30.4  2.73  3.50  0.00314 0.0226 
    ##  2 p002  Jan   presence    10.6  5.46  30.2  29.8  1.97  2.71 -0.00953 0.0135 
    ##  3 p003  Jan   presence    10.8  4.07  30.2  29.8  1.74  2.70 -0.00966 0.0135 
    ##  4 p004  Jan   presence    23.9 13.5   30.8  30.7  3.32  3.87  0.00289 0.00741
    ##  5 p005  Jan   background  10.6  5.46  30.2  29.8  1.97  2.71 -0.00953 0.0135 
    ##  6 p006  Jan   background  10.8  4.07  30.2  29.8  1.74  2.70 -0.00966 0.0135 
    ##  7 p007  Jan   background  18.6  6.65  30.6  30.4  2.90  3.51  0.00642 0.0118 
    ##  8 p008  Jan   background  14.8  6.93  30.7  30.4  2.73  3.50  0.00314 0.0226 
    ##  9 p009  Jan   background  23.9 13.5   30.8  30.7  3.32  3.87  0.00289 0.00741
    ## 10 p010  Jan   background  14.6  8.41  31.4  31.3  3.13  3.34  0.00213 0.0166 
    ## # ℹ 146 more rows
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
