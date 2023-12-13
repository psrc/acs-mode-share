shinyUI(
  
  navbarPage(
    
    tags$style(HTML("
    .tabbable > .nav > li > a {background-color: #005753;  color:white}
    .tabbable > .nav > li.active > a {background-color: #91268F; color:white}
  ")),
    
    id = "Mode to Work",
    tags$style("@import url(https://use.fontawesome.com/releases/v6.3.0/css/all.css);"),
    title = tags$a(div(tags$img(src='footer-logo.png',
                             style="margin-top: -30px; padding-left: 40px;",
                             height = "80")
                             ), href="https://www.psrc.org", target="_blank"),
             tags$head(
               tags$style(HTML('.navbar-nav > li > a, .navbar-brand {
                            padding-top:25px !important; 
                            padding-bottom:0 !important;
                            height: 75px;
                            }
                           .navbar {min-height:25px !important;}'))
             ),
    
    windowTitle = "Mode to Work - PSRC Region", 
    theme = "styles.css",
    position = "fixed-top",
             
    tabPanel(title=HTML("Commute Mode"),
             value="Mode-Page",
             banner_ui('modeBanner'),
             fluidRow(column(4, style='padding-left:25px; padding-right:0px;', left_panel_ui('leftMode')),
                      column(8, style='padding-left:25px; padding-right:50px;',
                             mode_overview_ui('modeOverview'),
                             hr(style = "border-top: 1px solid #000000;"),
                             commute_modes_ui('CommuteMode')
                             ), # End of Main Panel Modes
                      ), # End of Main Panel Fluid Row for Modes Tab
             ),# End of Tab Panel for Modes
    
    tabPanel(title=icon("info-circle"),
             value="Data-Source-Page",
             hr(style = "border-top: 1px solid #000000;"),
             source_ui('dataSource')),
             hr(style = "border-top: 1px solid #000000;"),
    
    tags$footer(footer_ui('psrcfooter'))
    
    ) # End of NavBar Page
  ) # End of Shiny App
