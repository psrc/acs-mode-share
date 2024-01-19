library(tidyverse)
library(psrccensus)
library(tidycensus)

mode_order <- c("Transit", "Work from Home", "Walk, Bike & Other", "Carpooled", "Drove Alone")
geography_order <- c("King County", "Kitsap County", "Pierce County", "Snohomish County", "Region",
                     "Asian or Pacific Islander", "Black or African American",
                     "Hispanic or Latinx", "Some Other Race(s)", "White")
census_year <- c(2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2021, 2022)

process_acs_data <- function(years=c(2022), acs_tbl="B08301", acs_variables="commute-modes", acs_type='acs1') {
  
  # Columns to keep from Tidy Census Pull
  cols_to_keep <- c("name", "variable", "estimate", "moe", "census_geography", "year")
  
  # Variables for dashboard
  variables <- read_csv("data/acs_variables.csv", show_col_types = FALSE)
  
  working_data <- NULL
  for (y in years) {
    print(str_glue("Working on {y}"))
    
    # County & Region data for PSRC region
    # Download the Data
    d <- get_acs_recs(geography = 'county', table.names = acs_tbl, years=y, acs.type = acs_type) 
    # Variables of interest
    d <- d |> filter(.data$variable %in% unique(variables$variable))
    # Clean up columns
    d <- d |> select(all_of(cols_to_keep))
    # Add labels
    d <- left_join(d, variables, by=c("variable"))
    # Consolidate rows based on simple labels
    d <- d |> 
      group_by(.data$name, .data$census_geography, .data$year, .data$simple_label) |>
      summarise(estimate = sum(.data$estimate), moe = moe_sum(moe=.data$moe, estimate=.data$estimate)) |>
      as_tibble() |>
      rename(label = "simple_label")
    # Get totals
    total <- d |> filter(.data$label == "Total") |> select("name", total="estimate")
    # Get Shares
    d <- left_join(d, total, by=c("name")) |> mutate(share=.data$estimate/.data$total) |> select(-"total")
    rm(total)
    
    if(is.null(working_data)) {working_data <- d} else {working_data <- bind_rows(working_data, d)}
    
  }
  
  # Match column names to rtp-dashboard inputs
  working_data <- working_data |> 
    mutate(date=mdy(paste0("12-01-",.data$year)), grouping="All", metric=acs_variables) |>
    mutate(year = as.character(year(.data$date))) |>
    select("year", "date", geography="name", geography_type="census_geography", variable="label", "grouping", "metric", "estimate", "share", "moe")
  
  return(working_data)
}

process_pums_data <- function(years, pums_span=1) {
  
  # Silence the dplyr summarize message
  options(dplyr.summarise.inform = FALSE)
  
  pums_vars_old <- c("JWTR", "JWRIP", "WAGP", "PRACE", "RAC1P", "RAC2P", "ANC1P", "OCCP")
  pums_vars_new <- c("JWTRNS", "JWRIP", "WAGP", "PRACE", "RAC1P", "RAC2P", "ANC1P", "OCCP")
  
  occp_vars_old <- c("JWTR", "JWRIP", "OCCP")
  occp_vars_new <- c("JWTRNS", "JWRIP", "OCCP")
  
  # Process metrics from PUMS data
  processed <- NULL
  for (i in years) {
    
    print(str_glue("Downloading PUMS {pums_span}-year data for {i}. This can take a few minutes."))
    pums <- get_psrc_pums(span = pums_span,
                          dyear = i,
                          level = "p",
                          vars = if(i < 2019) pums_vars_old else pums_vars_new) 
    
    print(str_glue("Downloading Occupations from PUMS {pums_span}-year data for {i}. This can take a few minutes."))
    occs <- get_psrc_pums(span = pums_span,
                          dyear = i,
                          level = "p",
                          vars = if(i < 2019) occp_vars_old else occp_vars_new,
                          labels = FALSE) 
    
    print(str_glue("Cleaning up Occupation attribute names"))
    o <- occs |>
      rename(jwtr = starts_with("JWTR")) |>
      mutate(mode_code = as.numeric(as.character(.data$jwtr))) |>
      mutate(mode = factor(case_when(
        is.na(.data$mode_code) ~ "Not a worker",
        .data$mode_code == 1 & .data$JWRIP == 1 ~ "Drove Alone",
        .data$mode_code == 1 & .data$JWRIP > 1 ~ "Carpooled",
        .data$mode_code <= 6  ~ "Transit",
        .data$mode_code <= 10  ~ "Walk, Bike & Other",
        .data$mode_code == 11  ~ "Work from Home",
        .data$mode_code > 11  ~ "Walk, Bike & Other"))) |>
      mutate(occ_code = as.numeric(as.character(.data$OCCP))) |>
      mutate(occupation = factor(case_when(
        is.na(.data$occ_code) ~ "Not a worker",
        .data$occ_code <= 3550 ~ "Management, Business, Science & Arts",
        .data$occ_code <= 4665 ~ "Services",
        .data$occ_code <= 5940 ~ "Sales & Office",
        .data$occ_code <= 7640 ~ "Resource & Construction",
        .data$occ_code <= 9760 ~ "Production, Transportation & Material Moving",
        .data$occ_code <= 9830 ~ "Military",
        .data$occ_code == 9920 ~ "Unemployed"))) |>
      select(-"OCCP", -"occ_code", -"mode_code", -"jwtr", -"JWRIP")
    
    print(str_glue("Cleaning up attribute names"))
    p <- pums |> 
      rename(mode = starts_with("JWTR"), occupancy = "JWRIP", wage = "WAGP") |> 
      # Clean up Occupancy for use in determining Drove Alone or Carpool
      mutate(occupancy = factor(case_when(is.na(.data$occupancy) ~ NA_character_,
                                          .data$occupancy == 1 ~ "Drove alone",
                                          TRUE ~ "Carpool"))) |> 
      # Clean up Mode Names
      mutate(mode = factor(case_when(is.na(.data$mode) ~ NA_character_,
                                     .data$mode == "Car, truck, or van" & .data$occupancy == "Drove alone" ~ "Drove Alone",
                                     .data$mode == "Car, truck, or van" & .data$occupancy == "Carpool" ~ "Carpooled",
                                     .data$mode == "Bus" ~ "Transit",
                                     .data$mode == "Bus or trolley bus" ~ "Transit",
                                     .data$mode == "Ferryboat" ~ "Transit",
                                     .data$mode == "Streetcar or trolley car (carro publico in Puerto Rico)" ~ "Transit",
                                     .data$mode == "Light rail, streetcar, or trolley" ~ "Transit",
                                     .data$mode == "Railroad" ~ "Transit",
                                     .data$mode == "Long-distance train or commuter rail" ~ "Transit",
                                     .data$mode == "Long-distance train or commuter train" ~ "Transit",
                                     .data$mode == "Other method" ~ "Walk, Bike & Other",
                                     .data$mode == "Taxicab" ~ "Walk, Bike & Other",
                                     .data$mode == "Motorcycle" ~ "Walk, Bike & Other",
                                     .data$mode == "Subway or elevated" ~ "Transit",
                                     .data$mode == "Subway or elevated rail" ~ "Transit",
                                     .data$mode == "Worked at home" ~ "Work from Home",
                                     .data$mode == "Worked from home" ~ "Work from Home",
                                     .data$mode == "Bicycle" ~ "Walk, Bike & Other",
                                     .data$mode == "Walked" ~ "Walk, Bike & Other",
                                     TRUE ~ as.character(.data$mode)))) |>
      
      # Clean up Race/Ethnicity labels
      mutate(race_6cat = factor(case_when(is.na(.data$PRACE) ~ NA_character_,
                                          .data$PRACE == "White alone" ~ "White",
                                          .data$PRACE == "Black or African American alone" ~ "Black or African American",
                                          .data$PRACE == "Asian alone" ~ "Asian or Pacific Islander",
                                          .data$PRACE == "Native Hawaiian and Other Pacific Islander alone" ~ "Asian or Pacific Islander",
                                          .data$PRACE == "Some Other Race alone" ~ "Some Other Race(s)",
                                          .data$PRACE == "Two or More Races" ~ "Some Other Race(s)",
                                          .data$PRACE == "Hispanic or Latino" ~ "Hispanic or Latinx",
                                          .data$PRACE == "American Indian or Alaskan Native Alone" ~ "American Indian or Alaskan Native",
                                          TRUE ~ as.character(.data$PRACE)))) |>
      
      mutate(race_3cat =  factor(case_when(.data$PRACE %in% c("Asian alone","Native Hawaiian and Other Pacific Islander alone") ~ "Asian or Pacific Islander",
                                           .data$PRACE == "White alone" ~ "White",
                                           TRUE ~ "Other people of color"),
                                 levels = c("Asian or Pacific Islander","Other people of color","White"))) |>
      
      mutate(race_5cat =  factor(case_when(.data$PRACE %in% c("Asian alone","Native Hawaiian and Other Pacific Islander alone") ~ "Asian or Pacific Islander",
                                           .data$PRACE == "White alone" ~ "White",
                                           .data$PRACE == "Hispanic or Latino" ~ "Hispanic or Latinx",
                                           .data$PRACE == "Black or African American alone" ~ "Black or African American",
                                           TRUE ~ "Some Other Race(s)"),
                                 levels = c("Asian or Pacific Islander","Black or African American","Hispanic or Latinx","White","Some Other Race(s)"))) |>
      
      mutate(country =  factor(case_when(
        .data$RAC2P == "Chinese, except Taiwanese, alone" ~ "Chinese",
        .data$RAC2P == "Asian Indian alone" ~ "Asian Indian",
        .data$RAC2P == "Filipino alone" ~ "Filipino",
        .data$RAC2P == "Vietnamese alone" ~ "Vietnamese",
        .data$RAC2P == "Korean alone" ~ "Korean",
        .data$RAC2P == "Japanese alone" ~ "Japanese",
        .data$RAC2P == "Cambodian alone" ~ "Cambodian",
        .data$RAC2P == "Taiwanese alone" ~ "Taiwanese",
        .data$RAC2P == "Thai alone" ~ "Thai",
        .data$RAC2P == "Pakistani alone" ~ "Pakistani",
        .data$RAC2P == "Laotian alone" ~ "Laotian",
        .data$RAC2P == "Burmese alone" ~ "Burmese",
        .data$RAC2P == "Indonesian alone" ~ "Indonesian",
        .data$RAC2P == "Hmong alone" ~ "Hmong",
        .data$RAC2P == "Bangladeshi alone" ~ "Bangladeshi",
        .data$RAC2P == "Mongolian alone" ~ "Mongolian",
        .data$RAC2P == "Nepalese alone" ~ "Nepalese",
        .data$RAC2P == "Sri Lankan alone" ~ "Sri Lankan",
        .data$RAC2P == "Bhutanese alone" ~ "Bhutanese",
        TRUE ~ .data$RAC2P))) |>
      
      mutate(aapi_detailed =  factor(case_when(
        .data$race_3cat == "Asian or Pacific Islander" & .data$PRACE=="Native Hawaiian and Other Pacific Islander alone" ~ "Native Hawaiian and Other Pacific Islander",
        .data$race_3cat == "Asian or Pacific Islander" & .data$PRACE!="Native Hawaiian and Other Pacific Islander alone" ~ .data$country))) 
    
    aapi_top_10 <- p |>
      filter(.data$race_3cat == "Asian or Pacific Islander") |>
      psrc_pums_count(., group_vars=c("race_3cat","aapi_detailed")) |>
      filter(!.data$aapi_detailed %in% c("Total","All combinations of Asian races only", "Other Asian alone")) |>
      arrange(desc(.data$count)) |>
      slice_head(n=10) |>
      select("aapi_detailed") |>
      pull()
    
    print(str_glue("Calculating Average Income by Commute Mode to work for PUMS {pums_span}-year data for {i}"))
    mean_wage <- psrc_pums_mean(p, stat_var = "wage", group_vars = c("mode") , incl_na = FALSE) |> dplyr::filter(.data$mode == "Total") |> dplyr::select(dplyr::ends_with("mean")) |> dplyr::pull()
    mean_wage_mode <- psrc_pums_mean(p, stat_var = "wage", group_vars = c("mode"), incl_na = FALSE) |> 
      filter(.data$mode != "Total") |>
      drop_na() |> 
      select(geography = "COUNTY", variable = "mode", year = "DATA_YEAR", estimate = ends_with("mean"), moe = ends_with("moe")) |>
      mutate(metric = "Mean Salary by Mode", year = as.character(.data$year), share = .data$estimate / mean_wage) |>
      select("year", "geography", "variable", "metric", "estimate", "share", "moe")
    
    print(str_glue("Calculating Commute Mode by AAPI 5-Category Race for PUMS {pums_span}-year data for {i}"))
    modes_race <- psrc_pums_count(p, group_vars = c("race_5cat", "mode"), incl_na = FALSE) |>
      rename(race="race_5cat") |>
      filter(.data$race != "Total" & .data$mode != "Total") |>
      select(-"share_moe") |>
      select(geography = "race", variable = "mode", year = "DATA_YEAR", estimate = "count", "share", moe = ends_with("moe")) |>
      mutate(metric = "Commute Mode by Race", year = as.character(.data$year)) |>
      select("year", "geography", "variable", "metric", "estimate", "share", "moe")
    
    print(str_glue("Calculating Commute Mode by Detailed Asian Ancestry for PUMS {pums_span}-year data for {i}"))
    modes_asian <- p |> 
      filter(.data$race_3cat == "Asian or Pacific Islander" & .data$aapi_detailed %in% aapi_top_10) |> 
      psrc_pums_count(p, group_vars = c("race_3cat","aapi_detailed", "mode"), incl_na = FALSE) |>
      filter(!.data$aapi_detailed %in% c("Total","All combinations of Asian races only", "Other Asian alone")) |>
      rename(race="aapi_detailed") |>
      filter(.data$mode != "Total") |>
      select(-"share_moe") |>
      select(geography = "race", variable = "mode", year = "DATA_YEAR", estimate = "count", "share", moe = ends_with("moe")) |>
      mutate(metric = "Commute Mode by Asian Ancestry", year = as.character(.data$year)) |>
      select("year", "geography", "variable", "metric", "estimate", "share", "moe")
    
    print(str_glue("Calculating Commute Mode by Occupation for PUMS {pums_span}-year data for {i}"))
    modes_occupations <- psrc_pums_count(o, group_vars = c("occupation", "mode"), incl_na = FALSE) |>
      filter(!.data$occupation %in% c("Total","Not a worker", "Unemployed")) |>
      filter(!.data$mode %in% c("Total","Not a worker")) |>
      select(-"share_moe") |>
      select(geography = "occupation", variable = "mode", year = "DATA_YEAR", estimate = "count", "share", moe = ends_with("moe")) |>
      mutate(metric = "Commute Mode by Occupation", year = as.character(.data$year)) |>
      select("year", "geography", "variable", "metric", "estimate", "share", "moe")
    
    # Combine summarized tables
    ifelse(is.null(processed), processed <- bind_rows(mean_wage_mode, modes_race, modes_asian, modes_occupations), processed <- bind_rows(processed, mean_wage_mode, modes_race, modes_asian, modes_occupations))

  }
  
  print(str_glue("All done."))
  return(processed)
}

commute_modes_psrc <-  process_acs_data(years = census_year) 
commute_modes_equity <- process_pums_data(years = census_year)

c <- commute_modes_psrc |> 
  filter(variable != "Total") |>
  select(-"date", -"geography_type", -"grouping") |> 
  mutate(variable = case_when(
    variable %in% c("Walk", "Bike", "Other") ~ "Walk, Bike & Other",
    !(variable %in% c("Walk", "Bike", "Other")) ~ variable)) |>
  mutate(variable = factor(variable, levels = mode_order)) |>
  group_by(year, geography, variable, metric) |>
  summarise(estimate = sum(estimate), share=sum(share), moe=tidycensus::moe_sum(moe, estimate=estimate)) |>
  as_tibble() |>
  arrange(year, geography, variable) |>
  mutate(metric = "Commute Mode")

e <- commute_modes_equity |>
  mutate(variable = factor(variable, levels = mode_order)) |>
  arrange(year, geography, variable)

commute_data <- bind_rows(c, e) |>
  mutate(variable = factor(variable, levels = mode_order)) |>
  arrange(year, geography, variable)

saveRDS(commute_data, "data/commute_data.rds")
