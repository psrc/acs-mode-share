# Define server logic
shinyServer(function(input, output) {
  
  # Footer
  footer_server('psrcfooter')
  
  # Modes Page
  banner_server('modeBanner', 
                banner_title = "Mode to Work", 
                banner_subtitle = "Puget Sound Trends",
                banner_url = "https://www.psrc.org/puget-sound-trends")
  
  left_panel_server('leftMode', page_nm = "Modes")
  mode_overview_server('modeOverview')
  commute_modes_server('CommuteMode')
  
  # Data Sources
  source_server('dataSource')
  
  # Data Download
  output$downloadData <- downloadHandler(
    filename = "PSRC-Commute-Mode-Data.xlsx",
    content = function(file) {saveWorkbook(create_public_spreadsheet(download_table_list), file = file)},
    contentType = "application/Excel"
  )
  
})    
