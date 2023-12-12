library(tidyverse)
library(psrccensus)

mode_order <- c("Transit", "Work from Home", "Walk, Bike & Other", "Carpooled", "Drove Alone")

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

commute_modes_psrc_2020 <- process_acs_data(years = c(2020), acs_type = 'acs5')
commute_modes_psrc <-  process_acs_data(years = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2021, 2022)) 

commute_data <- bind_rows(commute_modes_psrc, commute_modes_psrc_2020) |> 
  filter(geography == "Region" & variable != "Total") |>
  mutate(variable = case_when(
    variable %in% c("Walk", "Bike", "Other") ~ "Walk, Bike & Other",
    !(variable %in% c("Walk", "Bike", "Other")) ~ variable)) |>
  mutate(variable = factor(variable, levels = mode_order)) |>
  group_by(year, geography, variable, metric) |>
  summarise(estimate = sum(estimate), share=sum(share)) |>
  as_tibble() |>
  arrange(year, variable) |>
  mutate(metric = "Commute Mode")
  
saveRDS(commute_data, "data/commute_data.rds")
