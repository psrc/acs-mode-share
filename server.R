# Define server logic
shinyServer(function(input, output) {
  
  # Footer
  footer_server('psrcfooter')
  
  # Modes Page
  banner_server('modeBanner', 
                banner_title = "ACS Commute Mode to Work", 
                banner_subtitle = "Regional Transportation Plan",
                banner_url = "https://www.psrc.org/planning-2050/regional-transportation-plan")
  
  left_panel_server('leftMode', page_nm = "Modes")
  mode_overview_server('modeOverview')
  commute_modes_server('CommuteMode')
  
  # Data Sources
  source_server('dataSource')
  
  # Data Download
  output$downloadData <- downloadHandler(
    filename = "PSRC RTP Monitoring Data Download.xlsx",
    content = function(file) {saveWorkbook(create_public_spreadsheet(download_table_list), file = file)},
    contentType = "application/Excel"
  )
  
})    
