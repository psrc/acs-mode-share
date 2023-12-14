library(tidyverse)
library(psrccensus)

mode_order <- c("Transit", "Work from Home", "Walk, Bike & Other", "Carpooled", "Drove Alone")
geography_order <- c("King County", "Kitsap County", "Pierce County", "Snohomish County", "Region",
                     "Asian or Pacific Islander", "Black or African American",
                     "Hispanic or Latinx", "Some Other Race(s)", "White")
census_year <- c(2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2021, 2022)

process_acs_data <- function(years=c(2022), acs_tbl="B08301", acs_variables="commute-modes", acs_type='acs1') {
  
  # Columns to keep from Tidy Census Pull
  cols_to_keep <- c("name", "variable", "estimate", "moe", "census_geography", "year")
  
  # Variables for dashboard
  variables <- readr::read_csv(system.file('extdata', paste0(acs_variables,".csv"), package='psrcrtp'), show_col_types = FALSE)
  
  working_data <- NULL
  for (y in years) {
    print(stringr::str_glue("Working on {y}"))
    
    # County & Region data for PSRC region
    # Download the Data
    d <- psrccensus::get_acs_recs(geography = 'county', table.names = acs_tbl, years=y, acs.type = acs_type) 
    # Variables of interest
    d <- d |> dplyr::filter(.data$variable %in% unique(variables$variable))
    # Clean up columns
    d <- d |> dplyr::select(tidyselect::all_of(cols_to_keep))
    # Add labels
    d <- dplyr::left_join(d, variables, by=c("variable"))
    # Consolidate rows based on simple labels
    d <- d |> 
      dplyr::group_by(.data$name, .data$census_geography, .data$year, .data$simple_label) |>
      dplyr::summarise(estimate = sum(.data$estimate), moe = tidycensus::moe_sum(moe=.data$moe, estimate=.data$estimate)) |>
      dplyr::as_tibble() |>
      dplyr::rename(label = "simple_label")
    # Get totals
    total <- d |> dplyr::filter(.data$label == "Total") |> dplyr::select("name", total="estimate")
    # Get Shares
    d <- dplyr::left_join(d, total, by=c("name")) |> dplyr::mutate(share=.data$estimate/.data$total) |> dplyr::select(-"total")
    rm(total)
    
    if(is.null(working_data)) {working_data <- d} else {working_data <- dplyr::bind_rows(working_data, d)}
    
  }
  
  # Match column names to rtp-dashboard inputs
  working_data <- working_data |> 
    dplyr::mutate(date=lubridate::mdy(paste0("12-01-",.data$year)), grouping="All", metric=acs_variables) |>
    dplyr::mutate(year = as.character(lubridate::year(.data$date))) |>
    dplyr::select("year", "date", geography="name", geography_type="census_geography", variable="label", "grouping", "metric", "estimate", "share", "moe")
  
  return(working_data)
}

process_pums_data <- function(years, pums_span=1) {
  
  # Silence the dplyr summarize message
  options(dplyr.summarise.inform = FALSE)
  
  pums_vars_old <- c("JWTR", "JWRIP", "WAGP", "PRACE")
  pums_vars_new <- c("JWTRNS", "JWRIP", "WAGP", "PRACE")
  
  # Process metrics from PUMS data
  processed <- NULL
  for (i in years) {
    
    print(stringr::str_glue("Downloading PUMS {pums_span}-year data for {i}. This can take a few minutes."))
    pums <- psrccensus::get_psrc_pums(span = pums_span,
                                      dyear = i,
                                      level = "p",
                                      vars = if(i < 2019) pums_vars_old else pums_vars_new) 
    
    print(stringr::str_glue("Cleaning up attribute names"))
    p <- pums |> 
      dplyr::rename(mode = tidyselect::starts_with("JWTR"), occupancy = "JWRIP", wage = "WAGP", race = "PRACE") |> 
      # Clean up Occupancy for use in determining Drove Alone or Carpool
      dplyr::mutate(occupancy = factor(dplyr::case_when(is.na(.data$occupancy) ~ NA_character_,
                                                        .data$occupancy == 1 ~ "Drove alone",
                                                        TRUE ~ "Carpool"))) |> 
      # Clean up Mode Names
      dplyr::mutate(mode = factor(dplyr::case_when(is.na(.data$mode) ~ NA_character_,
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
      dplyr::mutate(race = factor(dplyr::case_when(is.na(.data$race) ~ NA_character_,
                                                   .data$race == "White alone" ~ "White",
                                                   .data$race == "Black or African American alone" ~ "Black or African American",
                                                   .data$race == "Asian alone" ~ "Asian or Pacific Islander",
                                                   .data$race == "Native Hawaiian and Other Pacific Islander alone" ~ "Asian or Pacific Islander",
                                                   .data$race == "Some Other Race alone" ~ "Some Other Race(s)",
                                                   .data$race == "Two or More Races" ~ "Some Other Race(s)",
                                                   .data$race == "Hispanic or Latino" ~ "Hispanic or Latinx",
                                                   .data$race == "American Indian or Alaskan Native Alone" ~ "Some Other Race(s)",
                                                   TRUE ~ as.character(.data$race))))

    print(stringr::str_glue("Calculating Average Income by Commute Mode to work for PUMS {pums_span}-year data for {i}"))
    mean_wage <- psrccensus::psrc_pums_mean(p, stat_var = "wage", group_vars = c("mode") , incl_na = FALSE) |> dplyr::filter(.data$mode == "Total") |> dplyr::select(dplyr::ends_with("mean")) |> dplyr::pull()
    mean_wage_mode <- psrccensus::psrc_pums_mean(p, stat_var = "wage", group_vars = c("mode"), incl_na = FALSE) |> 
      dplyr::filter(.data$mode != "Total") |>
      tidyr::drop_na() |> 
      dplyr::select(geography = "COUNTY", variable = "mode", year = "DATA_YEAR", estimate = dplyr::ends_with("mean"), moe = dplyr::ends_with("moe")) |>
      dplyr::mutate(metric = "Mean Salary by Mode", year = as.character(.data$year), share = .data$estimate / mean_wage) |>
      dplyr::select("year", "geography", "variable", "metric", "estimate", "share", "moe")
    
    print(stringr::str_glue("Calculating Commute Mode by Race for PUMS {pums_span}-year data for {i}"))
    modes_race <- psrccensus::psrc_pums_count(p, group_vars = c("race", "mode"), incl_na = FALSE) |>
      dplyr::filter(.data$race != "Total" & .data$mode != "Total") |>
      dplyr::select(-"share_moe") |>
      dplyr::select(geography = "race", variable = "mode", year = "DATA_YEAR", estimate = "count", "share", moe = dplyr::ends_with("moe")) |>
      dplyr::mutate(metric = "Commute Mode by Race", year = as.character(.data$year)) |>
      dplyr::select("year", "geography", "variable", "metric", "estimate", "share", "moe")
    
    # Combine summarized tables
    ifelse(is.null(processed), processed <- dplyr::bind_rows(mean_wage_mode, modes_race), processed <- dplyr::bind_rows(processed, mean_wage_mode, modes_race))

  }
  
  print(stringr::str_glue("All done."))
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
  mutate(geography = factor(geography, levels = geography_order)) |>
  group_by(year, geography, variable, metric) |>
  summarise(estimate = sum(estimate), share=sum(share), moe=tidycensus::moe_sum(moe, estimate=estimate)) |>
  as_tibble() |>
  arrange(year, geography, variable) |>
  mutate(metric = "Commute Mode")

e <- commute_modes_equity |>
  mutate(geography = factor(geography, levels = geography_order)) |>
  mutate(variable = factor(variable, levels = mode_order)) |>
  arrange(year, geography, variable)

commute_data <- bind_rows(c, e) |>
  mutate(geography = factor(geography, levels = geography_order)) |>
  mutate(variable = factor(variable, levels = mode_order)) |>
  arrange(year, geography, variable)

saveRDS(commute_data, "data/commute_data.rds")

