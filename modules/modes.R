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
    
    output$modes_chart <- renderEcharts4r({echart_line_chart(df=commute_data,
                                                             x='year', y='share', fill='variable', tog = 'metric',
                                                             esttype="percent", color = "jewel", dec = 0)})
    
    # Tab layout
    output$modes <- renderUI({
      tagList(
        h1("Commute Modes the PSRC Region"),
        textOutput(ns("modes_region")) |> withSpinner(color=load_clr),
        br(),
        strong(tags$div(class="chart_title","Commute Mode to Work")),
        fluidRow(column(12,echarts4rOutput(ns("modes_chart"), height = "600px"))),
        br(),
        tags$div(class="chart_source","Source: ACS 1yr Data Table B08301 (5yr for 2020)"),
        hr(style = "border-top: 1px solid #000000;")
        
      )
    }) #|> withSpinner(color="#0dc5c1")
  })  # end moduleServer
}

