echarts4r::e_common(font_family = "Poppins")

tooltip_js <- "
      function(params, ticket, callback) {
      var fmt = new Intl.NumberFormat('en',
      {\"style\":\"percent\",\"minimumFractionDigits\":0,\"maximumFractionDigits\":0,\"currency\":\"USD\"});\n
      var idx = 0;\n
      if (params.name == params.value[0]) {\n
      idx = 1;\n        }\n
      return(params.seriesName + '<br>' + 
      params.marker + ' ' +\n
      params.name + ': ' + fmt.format(parseFloat(params.value[idx]))
      )
      }"

timeline_opts <- function(e) {
  e |>
    echarts4r::e_timeline_opts(autoPlay = FALSE,
                               tooltip = list(show=FALSE),
                               axis_type = "category",
                               top = 15,
                               right = 200,
                               left = 200,
                               #currentIndex = 2,
                               controlStyle=FALSE,
                               lineStyle=FALSE,
                               label = list(show=TRUE,
                                            interval = 0,
                                            color='#4C4C4C',
                                            fontFamily = 'Poppins'),
                               itemStyle = list(color='#BCBEC0'),
                               checkpointStyle = list(label = list(show=FALSE),
                                                      color='#4C4C4C',
                                                      animation = FALSE),
                               progress = list(label = list(show=FALSE),
                                               itemStyle = list (color='#BCBEC0')),
                               emphasis = list(label = list(show=FALSE),
                                               itemStyle = list (color='#4C4C4C')))
  
} 

format_opts <- function(e, esttype, dec) {
  if(esttype == "number") {
    e <- e |> echarts4r::e_tooltip(trigger = "item")
    
  } else {
    
    if(esttype == "currency") {
      curr <- "USD"
    } else {
      curr <- NULL
    }
    
    e <- e |>
      echarts4r::e_y_axis(formatter = echarts4r::e_axis_formatter(esttype, digits = dec)) |>
      echarts4r::e_tooltip(trigger = "item",
                           formatter =  echarts4r::e_tooltip_item_formatter(style = esttype, digits = 0, currency = curr)) |>
      echarts4r::e_tooltip(formatter =  htmlwidgets::JS(tooltip_js))
  }
  return(e)
}

e_basics <- function(e, top_padding, bottom_padding, legend) {
  e <- e |>
    echarts4r::e_grid(left = '15%', top = top_padding, bottom = bottom_padding) |>
    echarts4r::e_x_axis(axisTick=list(show = FALSE)) |>
    echarts4r::e_show_loading()
    
    if(legend == TRUE) {
      e <- e |> 
        echarts4r::e_legend(show = TRUE, bottom=0)
    } else {
      e <- e |> 
        echarts4r::e_legend(show = FALSE)
    }
  
  return(e)
}



# General Information -------------------------------------------------------------
page_information <- function(tbl, page_name, page_section=NULL, page_info) {
  
  if(is.null(page_section)) {
    
    t <- tbl |>
      filter(page == page_name) |>
      select(all_of(page_info)) |>
      pull()
    
  } else {
    
    t <- tbl |>
      filter(page == page_name & section == page_section) |>
      select(all_of(page_info)) |>
      pull()
    
  }
  
  if(is.na(t)) {f <- ""} else {f <- t}
  
  return(f)
  
}

# Line Charts -------------------------------------------------------------
echart_line_chart <- function(df, x, y, fill, tog, dec, esttype, color, y_min=0) {
  
  if (color == "blues") {chart_color <- psrcplot::psrc_colors$blues_inc}
  if (color == "greens") {chart_color <- psrcplot::psrc_colors$greens_inc}
  if (color == "oranges") {chart_color <- psrcplot::psrc_colors$oranges_inc}
  if (color == "purples") {chart_color <- psrcplot::psrc_colors$purples_inc}
  if (color == "jewel") {chart_color <- psrcplot::psrc_colors$pognbgy_5}
  
  # Determine the number of Series to Plot
  bar_fill_values <- df %>% 
    select(all_of(fill)) %>% 
    dplyr::distinct() %>% 
    dplyr::pull() %>% 
    unique
  
  chart_fill <- as.character(bar_fill_values)
  
  top_padding <- 100
  title_padding <- 75
  bottom_padding <- 75
  
  # If the value is a percentage, round differently
  ifelse(esttype == "percent", num_dec <- 4, num_dec <- dec)
  
  # Create the most basic chart
  chart_df <- df %>%
    dplyr::filter(.data[[fill]] %in% chart_fill) %>%
    dplyr::mutate(!!y:= round(.data[[y]], num_dec)) %>%
    dplyr::select(tidyselect::all_of(fill), tidyselect::all_of(x), tidyselect::all_of(y), tidyselect::all_of(tog)) %>%
    tidyr::pivot_wider(names_from = tidyselect::all_of(fill), values_from = tidyselect::all_of(y))
  
  c <- chart_df %>%
    dplyr::group_by(.data[[tog]]) %>%
    echarts4r::e_charts_(x, timeline = TRUE) %>%
    e_toolbox_feature("dataView") %>%
    e_toolbox_feature("saveAsImage")
  
  for(fill_items in chart_fill) {
    c <- c %>%
      echarts4r::e_line_(fill_items, smooth = FALSE)
  }
  
  c <- c |> 
    echarts4r::e_color(chart_color) |>
    e_basics(top_padding, bottom_padding, legend = TRUE)
  
  # Add in the Timeseries Selector
  c <- timeline_opts(c)
  
  # Format the Axis and Hover Text
  c <- format_opts(c, esttype, dec)
  
  c <- c |> echarts4r::e_y_axis(min=y_min)
  
  return(c)
  
}

# Bar Charts --------------------------------------------------------------
echart_bar_chart <- function(df, x, y, tog, title, dec, esttype, color) {
  
  # If the value is a percentage, round differently
  ifelse(esttype == "percent", num_dec <- 4, num_dec <- dec)
  
  top_padding <- 100
  title_padding <- 75
  bottom_padding <- 75
  
  # Create the most basic chart
  c <- df |>
    dplyr::mutate(!!y:= round(.data[[y]], num_dec)) |>
    dplyr::group_by(.data[[tog]]) |>
    echarts4r::e_charts_(x, timeline = TRUE) |>
    e_toolbox_feature("dataView") |>
    e_toolbox_feature("saveAsImage")
  
  color_ramp <- switch(color,
                       "blues" = c('#BFE9E7', '#73CFCB', '#40BDB8', '#00A7A0', '#00716c', '#005753'),
                       "greens" = rep('#8CC63E', 12),
                       "oranges" = c('#FBD6C9', '#F7A489', '#F4835E', '#F05A28', '#9f3913', '#7a2700'),
                       "purples" = c('#E3C9E3', '#C388C2', '#AD5CAB', '#91268F', '#630460', '#4a0048'),
                       "jewel" = rep(c('#91268F', '#F05A28', '#8CC63E', '#00A7A0', '#4C4C4C', '#630460', '#9f3913', '#588527', '#00716c', '#3e4040'), 3))
  
  palette <- paste0('"', paste(color_ramp, collapse='", "'), '"')
  js_color <- paste0("function(params) {var colorList = [", palette, "]; return colorList[params.dataIndex]}")
  
  c <- c %>% 
    echarts4r::e_bar_(y,
                      name = title,
                      itemStyle = list(color = htmlwidgets::JS(js_color))) |>
    e_basics(top_padding, bottom_padding, legend = FALSE)
  
  # Add in the Timeseries Selector
  c <- timeline_opts(c)
  
  # Format the Axis and Hover Text
  c <- format_opts(c, esttype, dec)
    
  c <- c |>
    e_flip_coords()
  
  return(c)
  
}


# Data Download -----------------------------------------------------------
create_public_spreadsheet <- function(table_list) {
  
  hs <- createStyle(
    fontColour = "black",
    border = "bottom",
    fgFill = "#00a7a0",
    halign = "center",
    valign = "center",
    textDecoration = "bold"
  )
  
  table_idx <- 1
  sheet_idx <- 1
  
  wb <- createWorkbook()
  
  for (i in table_list) {
    for (j in names(table_list)) {
      if (names(table_list)[table_idx] == j) {
        
        addWorksheet(wb, sheetName = j)
        writeDataTable(wb, sheet = sheet_idx, x = i, tableStyle = "none", headerStyle = hs, withFilter = FALSE)
        setColWidths(wb, sheet = sheet_idx, cols = 1:length(i), widths = "auto")
        freezePane(wb, sheet = sheet_idx, firstRow = TRUE)
        
      } else {next}
    }
    if (table_idx < length(table_list)) {
      
      table_idx <- table_idx + 1
      sheet_idx <- sheet_idx + 1
      
    } else {break}
  }
  
  return(wb)

}

# Tables ------------------------------------------------------------------
create_source_table <- function(d=source_info) {
  
  # Table with Titles as first row
  t <- rbind(names(d), d)
  
  headerCallbackRemoveHeaderFooter <- c(
    "function(thead, data, start, end, display){",
    "  $('th', thead).css('display', 'none');",
    "}"
  )
  
  summary_tbl <- datatable(t,
                           options = list(paging = FALSE,
                                          pageLength = 30,
                                          searching = FALSE,
                                          dom = 't',
                                          headerCallback = JS(headerCallbackRemoveHeaderFooter),
                                          columnDefs = list(list(targets = c(0,3), className = 'dt-left'))),
                           selection = 'none',
                           callback = JS(
                             "$('table.dataTable.no-footer').css('border-bottom', 'none');"
                           ),
                           class = 'row-border',
                           filter = 'none',              
                           rownames = FALSE,
                           escape = FALSE
  ) 
  
  # Add Section Breaks
  
  summary_tbl <- summary_tbl %>%
    formatStyle(0:ncol(t), valueColumns = "Data Point",
                `border-bottom` = styleEqual(c("Work from Home: City", 
                                               "Traffic Related Deaths and Serious Injuries: Day of Week", 
                                               "Population, Housing Units and Jobs: Near High Capacity Transit",
                                               "Transit Mode to Work: City",
                                               "Bike to Work: City",
                                               "Departure Time to Work: Metro Areas"), "solid 2px"))
  
  summary_tbl <- summary_tbl %>%
    formatStyle(0:ncol(t), valueColumns = "Data Point",
                `border-top` = styleEqual(c("Vehicle Registrations: Region"), "solid 2px"))
  
  return(summary_tbl)
  
}
