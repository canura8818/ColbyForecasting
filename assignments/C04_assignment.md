Second Species Model
================

``` r
source("/home/canura26/ColbyForecasting/setup.R")
for (f in list.files("/home/canura26/ColbyForecasting/functions",
     pattern = glob2rx("*.R"), full.names = TRUE)){
  source(f)
}

SPECIES = "Galeocerdo cuvier"
```

``` r
cfg = read_configuration(scientificname = SPECIES, version = "v1")
model_input = read_model_input(scientificname = SPECIES, 
                               version = "v1",
                               log_me = c("depth", "Xbtm")) |>
  dplyr::mutate(month = month_as_number(.data$month)) |>
  select(all_of(c("class", cfg$keep)))
```

``` r
model_input_split = spatial_initial_split(model_input, 
                        prop = 1 / 5,     # 20% for testing
                        strategy = spatial_block_cv) # see ?spatial_block_cv
model_input_split
```

    ## <Training/Testing/Total>
    ## <259/75/334>

``` r
autoplot(model_input_split)
```

![](C04_assignment_files/figure-gfm/initial_split_plot-1.png)<!-- -->

``` r
tr_data = training(model_input_split)
cv_tr_data <- spatial_block_cv(tr_data,
  v = 5,     
  cellsize = grid_cellsize(model_input),
  offset = grid_offset(model_input) + 0.00001
)
autoplot(cv_tr_data)
```

![](C04_assignment_files/figure-gfm/cv_training-1.png)<!-- -->

``` r
one_row_of_training_data = dplyr::slice(tr_data,1)
rec = recipe(one_row_of_training_data, formula = class ~ .)
rec
```

    ## 

    ## ── Recipe ──────────────────────────────────────────────────────────────────────

    ## 

    ## ── Inputs

    ## Number of variables by role

    ## outcome:   1
    ## predictor: 9
    ## coords:    2

``` r
summary(rec)
```

    ## # A tibble: 12 × 4
    ##    variable type      role      source  
    ##    <chr>    <list>    <chr>     <chr>   
    ##  1 depth    <chr [2]> predictor original
    ##  2 month    <chr [2]> predictor original
    ##  3 SSS      <chr [2]> predictor original
    ##  4 U        <chr [2]> predictor original
    ##  5 Sbtm     <chr [2]> predictor original
    ##  6 V        <chr [2]> predictor original
    ##  7 Tbtm     <chr [2]> predictor original
    ##  8 MLD      <chr [2]> predictor original
    ##  9 SST      <chr [2]> predictor original
    ## 10 X        <chr [2]> coords    original
    ## 11 Y        <chr [2]> coords    original
    ## 12 class    <chr [3]> outcome   original

``` r
wflow = workflow_set(
  
  preproc = list(default = rec), # not much happening in our preprocessor
  
  models = list(                 # but we have 4 models to add
    
      # very simple - nothing to tune
      glm = logistic_reg(
          mode = "classification") |>
        set_engine("glm"),
      
      # two knobs to tune
      rf = rand_forest(
          mtry = tune(),
          trees = tune(),
          mode = "classification") |>
        set_engine("ranger", 
                   importance = "impurity"),
      
      # so many things to tune!
      btree = boost_tree(
          mtry = tune(), 
          trees = tune(), 
          tree_depth = tune(), 
          learn_rate = tune(), 
          loss_reduction = tune(), 
          stop_iter = tune(),
          mode = "classification") |>
        set_engine("xgboost"),
    
      # just two again
      maxent = maxent(
          feature_classes = tune(),
          regularization_multiplier = tune(),
          mode = "classification") |>
        set_engine("maxnet")
  )
)
wflow
```

    ## # A workflow set/tibble: 4 × 4
    ##   wflow_id       info             option    result    
    ##   <chr>          <list>           <list>    <list>    
    ## 1 default_glm    <tibble [1 × 4]> <opts[0]> <list [0]>
    ## 2 default_rf     <tibble [1 × 4]> <opts[0]> <list [0]>
    ## 3 default_btree  <tibble [1 × 4]> <opts[0]> <list [0]>
    ## 4 default_maxent <tibble [1 × 4]> <opts[0]> <list [0]>

``` r
metrics = sdm_metric_set(yardstick::accuracy)
metrics
```

    ## A metric set, consisting of:
    ## - `boyce_cont()`, a probability metric | direction: maximize
    ## - `roc_auc()`, a probability metric    | direction: maximize
    ## - `tss_max()`, a probability metric    | direction: maximize
    ## - `accuracy()`, a class metric         | direction: maximize

``` r
wflow <- wflow |>
  workflow_map("tune_grid",
    resamples = cv_tr_data, 
    grid = 3,
    metrics = metrics, 
    verbose = TRUE)
```

    ## i    No tuning parameters. `fit_resamples()` will be attempted

    ## i 1 of 4 resampling: default_glm

    ## ✔ 1 of 4 resampling: default_glm (714ms)

    ## i 2 of 4 tuning:     default_rf

    ## i Creating pre-processing data to finalize 1 unknown parameter: "mtry"

    ## ✔ 2 of 4 tuning:     default_rf (5.7s)

    ## i 3 of 4 tuning:     default_btree

    ## i Creating pre-processing data to finalize 1 unknown parameter: "mtry"

    ## → A | warning: `early_stop` was reduced to 0.

    ## There were issues with some computations   A: x1                                                 → B | warning: the standard deviation is zero
    ## There were issues with some computations   A: x1There were issues with some computations   A: x2   B: x1There were issues with some computations   A: x3   B: x1There were issues with some computations   A: x4   B: x2There were issues with some computations   A: x5   B: x2There were issues with some computations   A: x6   B: x2There were issues with some computations   A: x7   B: x3There were issues with some computations   A: x8   B: x3There were issues with some computations   A: x9   B: x4There were issues with some computations   A: x9   B: x4
    ## ✔ 3 of 4 tuning:     default_btree (11.9s)
    ## i 4 of 4 tuning:     default_maxent
    ## ✔ 4 of 4 tuning:     default_maxent (2.1s)

``` r
autoplot(wflow)
```

![](C04_assignment_files/figure-gfm/plot_wflow-1.png)<!-- -->

``` r
model_fits = workflowset_selectomatic(wflow, model_input_split,
                                  filename = "Galeocerdo_cuvier-v1-model_fits",
                                  path = data_path("models"))
model_fits
```

    ## # A tibble: 4 × 7
    ##   wflow_id      splits           id    .metrics .notes   .predictions .workflow 
    ##   <chr>         <list>           <chr> <list>   <list>   <list>       <list>    
    ## 1 default_glm   <split [259/75]> trai… <tibble> <tibble> <tibble>     <workflow>
    ## 2 default_rf    <split [259/75]> trai… <tibble> <tibble> <tibble>     <workflow>
    ## 3 default_btree <split [259/75]> trai… <tibble> <tibble> <tibble>     <workflow>
    ## 4 default_maxe… <split [259/75]> trai… <tibble> <tibble> <tibble>     <workflow>

``` r
model_fit_metrics(model_fits)
```

    ## # A tibble: 4 × 5
    ##   wflow_id       accuracy boyce_cont roc_auc tss_max
    ##   <chr>             <dbl>      <dbl>   <dbl>   <dbl>
    ## 1 default_glm       0.76      0.523    0.653   0.249
    ## 2 default_rf        0.667     0.783    0.601   0.241
    ## 3 default_btree     0.64      0.0776   0.523   0.116
    ## 4 default_maxent    0.573     0.472    0.642   0.225

``` r
model_fit_confmat(model_fits)
```

![](C04_assignment_files/figure-gfm/model_fit_confmat-1.png)<!-- -->

``` r
model_fit_roc_auc(model_fits)
```

![](C04_assignment_files/figure-gfm/model_fit_roc_auc-1.png)<!-- -->

``` r
model_fit_varimp_plot(model_fits)
```

![](C04_assignment_files/figure-gfm/model_fit_vip-1.png)<!-- -->

``` r
rf = model_fits |>
  filter(wflow_id == "default_rf")
rf
```

    ## # A tibble: 1 × 7
    ##   wflow_id   splits           id       .metrics .notes   .predictions .workflow 
    ##   <chr>      <list>           <chr>    <list>   <list>   <list>       <list>    
    ## 1 default_rf <split [259/75]> train/t… <tibble> <tibble> <tibble>     <workflow>

``` r
autoplot(rf$splits[[1]])
```

![](C04_assignment_files/figure-gfm/rf_splits-1.png)<!-- -->

``` r
rf$.metrics[[1]]
```

    ## # A tibble: 4 × 4
    ##   .metric    .estimator .estimate .config        
    ##   <chr>      <chr>          <dbl> <chr>          
    ## 1 accuracy   binary         0.667 pre0_mod0_post0
    ## 2 boyce_cont binary         0.783 pre0_mod0_post0
    ## 3 roc_auc    binary         0.601 pre0_mod0_post0
    ## 4 tss_max    binary         0.241 pre0_mod0_post0

``` r
rf$.predictions[[1]]
```

    ## # A tibble: 75 × 6
    ##    class      .pred_class .pred_presence .pred_background  .row .config        
    ##    <fct>      <fct>                <dbl>            <dbl> <int> <chr>          
    ##  1 presence   background          0.286             0.714     1 pre0_mod0_post0
    ##  2 presence   background          0.114             0.886     5 pre0_mod0_post0
    ##  3 presence   background          0.161             0.839     6 pre0_mod0_post0
    ##  4 background background          0.192             0.808    13 pre0_mod0_post0
    ##  5 background background          0.273             0.727    26 pre0_mod0_post0
    ##  6 background background          0.0718            0.928    29 pre0_mod0_post0
    ##  7 background background          0.141             0.859    35 pre0_mod0_post0
    ##  8 background background          0.172             0.828    36 pre0_mod0_post0
    ##  9 background background          0.165             0.835    40 pre0_mod0_post0
    ## 10 background background          0.138             0.862    42 pre0_mod0_post0
    ## # ℹ 65 more rows

``` r
rf$.workflow[[1]]
```

    ## ══ Workflow [trained] ══════════════════════════════════════════════════════════
    ## Preprocessor: Recipe
    ## Model: rand_forest()
    ## 
    ## ── Preprocessor ────────────────────────────────────────────────────────────────
    ## 0 Recipe Steps
    ## 
    ## ── Model ───────────────────────────────────────────────────────────────────────
    ## Ranger result
    ## 
    ## Call:
    ##  ranger::ranger(x = maybe_data_frame(x), y = y, mtry = min_cols(~1L,      x), num.trees = ~2000L, importance = ~"impurity", num.threads = 1,      verbose = FALSE, seed = sample.int(10^5, 1), probability = TRUE) 
    ## 
    ## Type:                             Probability estimation 
    ## Number of trees:                  2000 
    ## Sample size:                      259 
    ## Number of independent variables:  9 
    ## Mtry:                             1 
    ## Target node size:                 10 
    ## Variable importance mode:         impurity 
    ## Splitrule:                        gini 
    ## OOB prediction error (Brier s.):  0.2415543

``` r
model_fit_pdp(model_fits, wid = "default_btree", title = "Boosted Tree")
```

![](C04_assignment_files/figure-gfm/pd_plot-1.png)<!-- -->
