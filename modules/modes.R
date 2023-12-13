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
        textOutput(ns("overview_text")),
        br()
      )
    })
  })  # end moduleServer
}

# Climate Tabs --------------------------------------------------------------------------------
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
    output$modes_region <- renderText({page_information(tbl=page_text, page_name="Modes", page_section = "Modes-Region", page_info = "description")})
    
    output$modes_wages <- renderText({page_information(tbl=page_text, page_name="Modes", page_section = "Modes-Wages", page_info = "description")})
    
    # Charts
    output$modes_chart <- renderEcharts4r({echart_line_chart(df=commute_data |> filter(geography=="Region" & metric=="Commute Mode"),
                                                             x='year', y='share', fill='variable', tog = 'metric',
                                                             esttype="percent", color = "jewel", dec = 0)})
    
    output$wages_chart <- renderEcharts4r({echart_line_chart(df=commute_data |> filter(geography=="Region" & metric=="Mean Salary by Mode"),
                                                             x='year', y='estimate', fill='variable', tog = 'metric',
                                                             esttype="number", color = "jewel", dec = 0)})
    
    output$race_chart <- renderEcharts4r({echart_line_chart(df=commute_data |> filter(metric=="Commute Mode by Race" & variable==input$Mode & geography %in% c(input$Race, "White alone")),
                                                            x='year', y='share', fill='geography', tog = 'variable',
                                                            esttype="percent", color = "jewel", dec = 0) })
    
    
    
    # Tab layout
    output$modes <- renderUI({
      tagList(
        h1("Commute Modes the PSRC Region"),
        textOutput(ns("modes_region")) |> withSpinner(color=load_clr),
        fluidRow(column(12,echarts4rOutput(ns("modes_chart")))),
        br(),
        tags$div(class="chart_source","Source: ACS 1yr Data Table B08301"),
        hr(style = "border-top: 1px solid #000000;"),
        
        h1("Average Wages by Commute Mode"),
        textOutput(ns("modes_wages")),
        fluidRow(column(12,echarts4rOutput(ns("wages_chart")))),
        br(),
        tags$div(class="chart_source","Source: PUMS 1yr Data Variables WAGEP & JWTR"),
        hr(style = "border-top: 1px solid #000000;"),
        
        h1("Commute Mode by Race & Ethnicity"),
        br(),
        fluidRow(column(6, selectInput(ns("Mode"), label="Select Commute Mode:", choices=travel_modes_list, selected = "Work from Home")),
                 column(6, selectInput(ns("Race"), label="Select Race/Ethnicity:", choices=race_list, selected = "Black or African American alone"))),
        fluidRow(column(12,echarts4rOutput(ns("race_chart")))),
        br(),
        tags$div(class="chart_source","Source: PUMS 1yr Data Variables JWTR & PRACE"),
        hr(style = "border-top: 1px solid #000000;")
        
      )
    }) # end of UI
  })  # end moduleServer
}

