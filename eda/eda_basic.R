# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# INTRO ====================================================================
metadatar <- list(script_starttime = Sys.time(), 
                  script_det = list(version_dt = as.Date("2021-10-09"), 
                                    author = "corb", 
                                    proj_name = "my_census", 
                                    script_type = "eda", 
                                    notepad = paste0("initial exploration of tidycensus")), 
                  seed_set = 6)
metadatar

# LOAD LIBRARIES **********************************************************
R.version.string
Sys.info()
getwd()
library(lobstr)
library(rlang)
library(tidyverse)
library(tidylog)
library(lubridate)
library(scales)
library(gt)
library(sf)
library(tidycensus)
library(patchwork)
set.seed(metadatar$seed_set[1])
options(digits = 4, max.print = 99, warnPartialMatchDollar = TRUE, 
        tibble.print_max = 30, scipen = 999, nwarnings = 5)
mem_used()

# basic helper functions ***************************************************

# function to print object size
sizer <- function(x) {
  aaa <- format(object.size(x), "MB")
  return(aaa)}

# function to quickly run garbage collection
trash <- function(x) {
  gc(verbose = TRUE)}

# function to quickly view a sample of a dataframe
viewer <- function(x) {
  if (is.data.frame(x) == FALSE) {
    print("Error, insert a dataframe")
  } else {
    if(nrow(x) < 95) {
      View(x[sample(1:nrow(x), floor(nrow(x) * 0.5)), ])
    } else {
      View(x[sample(1:nrow(x), 100), ])
    }}}

# a function to make a quick data dictionary of a data frame
data_dictionary <- function(aa) {
  dd <- data.frame(column_order = seq(1, ncol(aa)), 
                   column_name_text = colnames(aa), 
                   column_class = sapply(aa, class, simplify = TRUE), 
                   column_nacount = sapply(lapply(aa, is.na), 
                                           sum, simplify = TRUE), 
                   column_uniques = sapply(lapply(aa, unique), 
                                           length, simplify = TRUE), 
                   row_01 = sapply(aa[1, ], as.character, simplify = TRUE), 
                   row_02 = sapply(aa[2, ], as.character, simplify = TRUE),
                   row_03 = sapply(aa[3, ], as.character, simplify = TRUE),
                   row_04 = sapply(aa[4, ], as.character, simplify = TRUE),
                   row_05 = sapply(aa[5, ], as.character, simplify = TRUE),
                   row.names = NULL)
  return(dd)}

# start the clock timer, used for monitoring runtimes
clockin <- function() {
  aa <- Sys.time()
  clock_timer_start <<- aa
  return(aa)}

# end the clock timer, used in conjunction with the clockin fun
clockout <- function(x) {
  aa <- clock_timer_start
  bb <- Sys.time()
  cc <- bb - aa
  return(cc)}

# helps turn a character dollar variable into numeric
#   requires stringr, uncomment last line to turn NA to zero
cash_money <- function(x) {
  aa <- str_remove_all(x, pattern = "\\$")
  bb <- str_remove_all(aa, pattern = ",")
  cc <- as.numeric(bb)
  # cc <- ifelse(is.na(cc), 0, cc)
  return(cc)}

# POST SCRIPT; alt to using paste0() all the time (i saw this on twitter)
'%ps%' <- function(lhs, rhs) {
  return_me <- paste0(lhs, rhs)
  return(return_me)}

# ^ ====================================
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# census key and other setup ------------------------------------------

options(tigris_use_cache = TRUE)

# set api key if needed !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# census_api_key("111", install = TRUE)

# set up the parameters for what to query from the acs5 !!!!!!!!!!!!!!!!!!
census_params <- list(state = "ME",  
                      year = 2019, 
                      variables = data.frame(var_codes = c("B01003_001", 
                                                           "B06011_001",
                                                           "B25105_001", 
                                                           "B25031_001", 
                                                           "B25031_003", 
                                                           "B25031_004"), 
                                             var_detail = c("population estimate", 
                                                            "median income",  
                                                            "median monthly housing costs", 
                                                            "median gross rent", 
                                                            "median gross rent 1bd", 
                                                            "median gross rent 2bd")))

fips_codes <- fips_codes %>% 
  mutate(GEOID = paste0(state_code, county_code))

# variable options from the american community survey !!!!!!!!!!!!!!!!!
# var_options <- load_variables(2019, "acs5", cache = TRUE)
# var_options <- var_options %>%
#   mutate(code_suffix = str_sub(name,
#                                start = str_length(name) - 2),
#          code_suffix = as.double(code_suffix),
#          concept_20 = str_sub(concept, end = 20))

# review the variables that are available to query ::::::::::::::::::::
# var_options %>%
#   filter(code_suffix == 1) %>%
#   View()

# ^ -----

# query the census api to get data with geometries attached --------------

# pull data at a state level
df_state <- get_acs(state = census_params$state, 
                    geography = "state", 
                    year = census_params$year, 
                    variables = census_params$variables$var_codes, 
                    geometry = TRUE, 
                    cache_table = TRUE)
df_state

# pull data at a state level, 5 years ago
df_state_5 <- get_acs(state = census_params$state, 
                      geography = "state", 
                      year = census_params$year - 5, 
                      variables = census_params$variables$var_codes, 
                      geometry = FALSE, 
                      cache_table = TRUE)
df_state_5

# pull data at a county level
df_county <- get_acs(state = census_params$state, 
                     geography = "county", 
                     year = census_params$year, 
                     variables = census_params$variables$var_codes, 
                     geometry = TRUE, 
                     cache_table = TRUE)
head(df_county)

# pull data at a zipcode level
df_zpcd <- get_acs(state = census_params$state, 
                   geography = "zcta", 
                   year = census_params$year, 
                   variables = census_params$variables$var_codes, 
                   geometry = TRUE, 
                   cache_table = TRUE)
head(df_zpcd)

# pull census data by county_subdivision, for NE with big counties
# df_neweng <- get_acs(state = census_params$state, 
#                      geography = "county subdivision", 
#                      year = census_params$year, 
#                      variables = census_params$variables$var_codes, 
#                      geometry = TRUE,
#                      cache_table = TRUE)
# head(df_neweng)


# clean up the dataframes ::::::::::::::::::::::::::::::::::::::::::::

df_state <- df_state %>% 
  rename(var_codes = variable) 
df_state <- left_join(df_state, census_params$variables, 
                      by = 'var_codes')
df_state_5 <- df_state_5 %>% 
  rename(var_codes = variable) %>% 
  mutate(estimate_prior = estimate) %>% 
  select(var_codes, estimate_prior)
df_state <- left_join(df_state, df_state_5, by = "var_codes")
rm(df_state_5)

df_county <- df_county %>% 
  rename(var_codes = variable) %>% 
  mutate(state_code = str_sub(GEOID, start = 1, end = 2))
df_county <- left_join(df_county, census_params$variables, 
                       by = 'var_codes')

df_county <- left_join(df_county, fips_codes, by = "GEOID")
df_county <- df_county %>% 
  mutate(county_name = str_remove(county, " County"))

df_zpcd <- df_zpcd %>% rename(var_codes = variable) 
df_zpcd <- left_join(df_zpcd, census_params$variables, by = 'var_codes')

# df_neweng <- df_neweng %>% rename(var_codes = variable)
# df_neweng <- left_join(df_neweng, census_params$variables, by = 'var_codes')

# cleanup ?????????????????????????????????????????????????????????
ls()
trash()
mem_used()
obj_size(df_zpcd)
sizer(df_zpcd)

# ^ -----

# viz functions ----------------------------------------------------

# create a simple county filled map
fun_county_map <- function(df_func = df_county, 
                           dfv_state = df_state, 
                           measure_var, measure_cap = NA) {
  dfv_state <- dfv_state %>% filter(var_detail == !!measure_var)
  plt_sub <- "State-level " %ps% measure_var %ps% " = " %ps% 
    prettyNum(dfv_state$estimate, big.mark = ",") %ps% "\n" %ps% 
    "5-year growth rate = " %ps% 
    (round(dfv_state$estimate / dfv_state$estimate_prior - 1, 
           digits = 3) * 100) %ps% "%"
  df_func <- df_func %>% filter(var_detail == !!measure_var)
  measure_cap <- ifelse(is.na(measure_cap), 
                        max(df_func$estimate), 
                        measure_cap)
  plt1 <- df_func %>% 
    mutate(estimate = ifelse(estimate > measure_cap, 
                             measure_cap, estimate)) %>% 
    ggplot() + 
    geom_sf(aes(fill = estimate), alpha = 0.85, color = "black") + 
    geom_sf(data = dfv_state, color = "black", size = 1, 
            alpha = 0) + 
    scale_fill_distiller(palette = "Greens", 
                         direction = 1) + 
    guides(fill = guide_colorbar(title.position = "top", 
                                 title.hjust = 0.5, 
                                 barwidth = unit(20, 'lines'), 
                                 barheight = unit(0.5, 'lines'))) + 
    theme_minimal() + theme(legend.position = "top") + 
    labs(title = toupper(measure_var), 
         subtitle = plt_sub, fill = "", 
         caption = "*Value capped at " %ps% measure_cap)
  return(plt1)}

# create a simple zipcode filled map
fun_zip_map <- function(df_func = df_zpcd, 
                        dfv_state = df_state, 
                        measure_var, measure_cap = NA) {
  dfv_state <- dfv_state %>% filter(var_detail == !!measure_var)
  plt_sub <- "State-level " %ps% measure_var %ps% " = " %ps% 
    prettyNum(dfv_state$estimate, big.mark = ",") %ps% "\n" %ps% 
    "5-year growth rate = " %ps% 
    (round(dfv_state$estimate / dfv_state$estimate_prior - 1, 
           digits = 3) * 100) %ps% "%"
  df_func <- df_func %>% filter(var_detail == !!measure_var)
  measure_cap <- ifelse(is.na(measure_cap), 
                        max(df_func$estimate), 
                        measure_cap)
  plt1 <- df_func %>% 
    mutate(estimate = ifelse(estimate > measure_cap, 
                             measure_cap, estimate)) %>% 
    ggplot() + 
    geom_sf(aes(fill = estimate), alpha = 0.85, color = "black") + 
    geom_sf(data = dfv_state, color = "black", size = 1, 
            alpha = 0) + 
    scale_fill_distiller(palette = "Greens", 
                         direction = 1) + 
    guides(fill = guide_colorbar(title.position = "top", 
                                 title.hjust = 0.5, 
                                 barwidth = unit(20, 'lines'), 
                                 barheight = unit(0.5, 'lines'))) + 
    theme_minimal() + theme(legend.position = "top") + 
    labs(title = toupper(measure_var), 
         subtitle = plt_sub, fill = "", 
         caption = "*Value capped at " %ps% measure_cap)
  return(plt1)}

# create a simple county lollipop bar
fun_county_lolli <- function(df_func = df_county, 
                             dfv_state = df_state, 
                             measure_var, show_this_many = 1) {
  dfv_state <- dfv_state %>% filter(var_detail == !!measure_var)
  plt_sub <- "State-level " %ps% measure_var %ps% " = " %ps% 
    prettyNum(dfv_state$estimate, big.mark = ",")
  df_func <- df_func %>% filter(var_detail == !!measure_var)
  plt1 <- df_func %>% 
    arrange(desc(estimate)) %>% 
    slice(1:show_this_many) %>% 
    ggplot(aes(x = estimate, y = reorder(county_name, estimate))) + 
    geom_segment(aes(x = 0, xend = estimate, yend = county_name), 
                 color = "#6ECB63") + 
    geom_point(size = 4, color = "#6ECB63") + 
    geom_label(aes(x = estimate * 1.08, label = estimate), 
               size = 2.5, color = "#6ECB63") + 
    theme_minimal() + theme(legend.position = "none") +
    labs(title = toupper(measure_var),
         subtitle = plt_sub, fill = "", 
         y = "County Name", x = "")
  return(plt1)}

# simple scatterplot of 2 variables from the census query by zip
fun_scatter_zip <- function(df_func = df_zpcd, 
                            mvar_x, mvar_y) {
  df_x <- df_func %>% filter(var_detail == !!mvar_x) %>% 
    rename(mvar_x = estimate) %>% as_tibble() %>% select(-geometry)
  df_y <- df_func %>% filter(var_detail == !!mvar_y) %>%
    rename(mvar_y = estimate) %>% as_tibble() %>% select(-geometry)
  df_gg <- left_join(df_x, df_y, by = 'GEOID')
  plt_capt <- "NA records X = " %ps% sum(is.na(df_gg$mvar_x)) %ps% 
    "; NA records Y = " %ps% sum(is.na(df_gg$mvar_y))
  plt1 <- df_gg %>% 
    ggplot(aes(x = mvar_x, y = mvar_y)) + 
    geom_rug(alpha = 0.25) + 
    geom_vline(aes(xintercept = median(mvar_x, na.rm = TRUE)), 
               linetype = 2, color = "red") + 
    geom_hline(aes(yintercept = median(mvar_y, na.rm = TRUE)), 
               linetype = 2, color = "red") + 
    geom_point(aes(x = median(mvar_x, na.rm = TRUE), 
                   y = median(mvar_y, na.rm = TRUE)), 
               color = "red", size = 5) + 
    geom_point(alpha = 0.7, size = 1.5) + 
    theme_minimal() + 
    labs(x = mvar_x, y = mvar_y, caption = plt_capt)
  return(plt1)}

# ^ -----

# run the visuals ----------------------------------------------------

fun_scatter_zip(df_func = df_zpcd, 
                mvar_x = "median income", 
                mvar_y = "population estimate")

# draw some maps :::::::::::::::::::::::::::::::::::::::::::::::::
(gg1 <- fun_county_map(measure_var = "population estimate", 
                       measure_cap = 500000))

(gg2 <- fun_county_lolli(measure_var = "population estimate", 
                         show_this_many = 15))

(gg3 <- fun_zip_map(measure_var = "population estimate", 
                    measure_cap = 5000))

(gg1 + gg3) / gg2

# ^ -----


