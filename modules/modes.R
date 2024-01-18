# Overview ----------------------------------------------------------------------------
mode_overview_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("modeoverview"))
  )
}

mode_overview_server <- function(id) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Text
    output$overview_text <- renderText({page_information(tbl=page_text, page_name="Modes", page_section = "Overview", page_info = "description")})
    
    # Overview UI
    output$modeoverview <- renderUI({
      tagList(
        htmlOutput(ns("overview_text"))
      )
    })
  })  # end moduleServer
}

# Tabs --------------------------------------------------------------------------------
commute_modes_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("modes"))
  )
}

commute_modes_server <- function(id) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Text
    output$modes_text <- renderText({page_information(tbl=page_text, page_name="Modes", page_section = "Modes-Region", page_info = "description")})
    
    output$wages_text <- renderText({page_information(tbl=page_text, page_name="Modes", page_section = "Modes-Wages", page_info = "description")})
    
    output$race_text <- renderText({page_information(tbl=page_text, page_name="Modes", page_section = "Modes-Race", page_info = "description")})
    
    output$aapi_text <- renderText({page_information(tbl=page_text, page_name="Modes", page_section = "Modes-AAPI", page_info = "description")})
    
    output$occupations_text <- renderText({page_information(tbl=page_text, page_name="Modes", page_section = "Modes-Occupation", page_info = "description")})
    
    # Charts
    output$modes_chart <- renderEcharts4r({echart_line_chart(df=commute_data |> filter(geography=="Region" & metric=="Commute Mode"),
                                                             x='year', y='share', fill='variable', tog = 'metric',
                                                             esttype="percent", color = "jewel", dec = 0)})
    
    output$wages_chart <- renderEcharts4r({echart_line_chart(df=commute_data |> filter(geography=="Region" & metric=="Mean Salary by Mode"),
                                                             x='year', y='estimate', fill='variable', tog = 'metric',
                                                             esttype="currency", color = "jewel", dec = 0)})
    
    output$race_chart <- renderEcharts4r({echart_line_chart(df=commute_data |> filter(metric=="Commute Mode by Race" & variable==input$Mode & geography %in% c(input$Race, "White")),
                                                            x='year', y='share', fill='geography', tog = 'variable',
                                                            esttype="percent", color = "jewel", dec = 0) })
    
    output$aapi_chart <- renderEcharts4r({echart_bar_chart(df=commute_data |> 
                                                             filter(metric=="Commute Mode by Asian Ancestry" & year==input$AAPIYear & variable==input$AAPIMode) |> 
                                                             arrange(share) |> 
                                                             mutate(geography = str_wrap(geography, width=15)), 
                                                           tog = 'variable', y='share', x='geography', esttype="percent", dec=0, color = 'greens', title = "Commute Share")})
    
    output$occupations_chart <- renderEcharts4r({echart_bar_chart(df=commute_data |> 
                                                                    filter(metric=="Commute Mode by Occupation" & year==input$OccYear & variable==input$OccMode) |> 
                                                                    arrange(share) |> 
                                                                    mutate(geography = str_wrap(geography, width=15)), 
                                                                  tog = 'variable', y='share', x='geography', esttype="percent", dec=0, color = 'blues', title = "Commute Share")})
    
    # Tab layout
    output$modes <- renderUI({
      tagList(
        h1("Mode to Work in the PSRC Region"),
        htmlOutput(ns("modes_text")) |> withSpinner(color=load_clr),
        fluidRow(column(12,echarts4rOutput(ns("modes_chart")))),
        br(),
        tags$div(class="chart_source","Source: U.S. Census Bureau, American Community Survey (ACS) 1-Year Data Table B08301"),
        hr(style = "border-top: 1px solid #000000;"),
        
        h1("Race & Ethnicity"),
        textOutput(ns("race_text")),
        br(),
        fluidRow(column(6, selectInput(ns("Mode"), label="Select Commute Mode:", choices=travel_modes_list, selected = "Work from Home")),
                 column(6, selectInput(ns("Race"), label="Select Race/Ethnicity:", choices=race_list, selected = "Black or African American"))),
        fluidRow(column(12,echarts4rOutput(ns("race_chart")))),
        br(),
        tags$div(class="chart_source","Source: U.S. Census Bureau, Public Use Microdata Sample (PUMS) 1-Year Data Variables JWTR & PRACE"),
        hr(style = "border-top: 1px solid #000000;"),
        
        h1("Asian Americans and Pacific Islanders (AAPI)"),
        textOutput(ns("aapi_text")),
        br(),
        fluidRow(column(6, selectInput(ns("AAPIYear"), label="Select Year:", choices=year_list, selected = "2022")),
                 column(6, selectInput(ns("AAPIMode"), label="Select Commute Mode:", choices=travel_modes_list, selected = "Work from Home"))),
        fluidRow(column(12,echarts4rOutput(ns("aapi_chart"), height = '600px'))),
        br(),
        tags$div(class="chart_source","Source: U.S. Census Bureau, Public Use Microdata Sample (PUMS) 1-Year Data Variables JWTR & RAC2P"),
        hr(style = "border-top: 1px solid #000000;"),
        
        h1("Occupations"),
        textOutput(ns("occupations_text")),
        br(),
        fluidRow(column(6, selectInput(ns("OccYear"), label="Select Year:", choices=year_list, selected = "2022")),
                 column(6, selectInput(ns("OccMode"), label="Select Commute Mode:", choices=travel_modes_list, selected = "Work from Home"))),
        fluidRow(column(12,echarts4rOutput(ns("occupations_chart"), height = '600px'))),
        br(),
        tags$div(class="chart_source","Source: U.S. Census Bureau, Public Use Microdata Sample (PUMS) 1-Year Data Variables JWTR & OCCP"),
        hr(style = "border-top: 1px solid #000000;"),
        
        h1("Average Wage"),
        textOutput(ns("wages_text")),
        fluidRow(column(12,echarts4rOutput(ns("wages_chart")))),
        br(),
        tags$div(class="chart_source","Source: U.S. Census Bureau, Public Use Microdata Sample (PUMS) 1-Year Data Variables WAGEP & JWTR"),
        hr(style = "border-top: 1px solid #000000;")
        
        
      )
    }) # end of UI
  })  # end moduleServer
}

