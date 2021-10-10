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

# set api key if needed !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
census_api_key("111", install = TRUE)

# set up the parameters for what to query from the acs5 !!!!!!!!!!!!!!!!!!
census_params <- list(state = "NC",  
                      year = 2019, 
                      variables = data.frame(var_codes = c("B01003_001", 
                                                           "B06011_001", 
                                                           "B25035_001"), 
                                             var_detail = c("population estimate", 
                                                            "median income", 
                                                            "median yr structure built")))

# variable options from the american community survey !!!!!!!!!!!!!!!!!
# var_options <- load_variables(2019, "acs5", cache = TRUE)
var_options <- var_options %>% 
  mutate(code_suffix = str_sub(name, 
                               start = str_length(name) - 2), 
         code_suffix = as.double(code_suffix), 
         concept_20 = str_sub(concept, end = 20))

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

# pull data at a county level
df_county <- get_acs(state = census_params$state, 
                     geography = "county", 
                     year = census_params$year, 
                     variables = census_params$variables$var_codes, 
                     geometry = TRUE, 
                     cache_table = TRUE)
head(df_county)

# clean up the dataframes ::::::::::::::::::::::::::::::::::::::::::::

df_state <- df_state %>% 
  rename(var_codes = variable)
df_state <- left_join(df_state, census_params$variables, 
                      by = 'var_codes')

df_county <- df_county %>% 
  rename(var_codes = variable)
df_county <- left_join(df_county, census_params$variables, 
                       by = 'var_codes')

# cleanup ?????????????????????????????????????????????????????????
ls()
trash()
mem_used()
obj_size(df_county)
sizer(df_county)

# ^ -----

# viz functions ----------------------------------------------------

fun_county_map <- function(df_func = df_county, measure_var) {
  plt1 <- df_func %>% 
    filter(var_detail == !!measure_var) %>% 
    ggplot() + 
    geom_sf(aes(fill = estimate)) + 
    guides(fill = guide_colorbar(title.position = "top", 
                                 title.hjust = 0.5, 
                                 barwidth = unit(20, 'lines'), 
                                 barheight = unit(0.5, 'lines'))) + 
    theme_minimal() + theme(legend.position = "top")
  return(plt1)}


# ^ -----

# run the visuals ----------------------------------------------------

fun_county_map(df_county, measure_var = "population estimate")




# ^ -----