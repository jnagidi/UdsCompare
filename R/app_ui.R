#' The application User-Interface
#' 
#' 
#' UI is defined with header, left sidebar and body
#' Sidebar has three options: "Home", "Data Explorer", "Operational Dashboard"
#' 
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#'
#' @import shiny
#' @import data.table
#'
#' @noRd
#' 




app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources - adds CSS (www) and Images (img)
    golem_add_external_resources(),
    
    bs4Dash::dashboardPage(
      
      #Title of webpage
      title="ADRC Data Visualization",
      fullscreen = TRUE,
      dark = NULL,
      scrollToTop = TRUE,
      preloader = preloader_spinner(),

      
      header = bs4Dash::dashboardHeader(
        title = tags$a(
          href = 'https://www.uab.edu/medicine/alzheimers/',
          target = "_blank",
          tags$img(
            src = 'www/logo-ADC.png',
            width = "100%",
            style = "padding: 8% 10% 2% 10%", 
            alt = 'UAB logo'
          )
        )
      ),
      
      
      
      ##
      #Sidebar of page - using shinydashboard
      ##
      
      sidebar = bs4Dash::dashboardSidebar(
        id = "main_sidebar",
        skin = 'dark',
        width = 350,
        minified = FALSE,
        fixed = TRUE,
        
        div(
          style = "padding: 20px;",
          shiny::textInput(
            inputId = "id_entry",
            label = "ADC ID",
            width = "100%"
          ),
          shiny::actionButton(
            inputId = "compare_button",
            label = "Get Data",
            width = "100%",
            style = "margin-top: 10px;"
          )
        )
      ),
      
      
      ##
      #Main body of page
      ##
      
      body = bs4Dash::dashboardBody(
        
        #Initialize use of waiter and call custom CSS file
        htmltools::tags$head(
          tags$link(rel="stylesheet", type="text/css", href="ADRC-theme.css"),
          tags$link(rel="stylesheet", type="text/css", href="ADRC-dash.css"),
          tags$style(HTML("
            .sidebar-menu li { margin-top: 20px; }
            .tab-content { padding: 20px; }
            .nav-tabs { margin-bottom: 10px; }
            .comparison-table { width: 100%; margin-bottom: 20px; }
            .comparison-table td, .comparison-table th { padding: 8px; }
            .content-wrapper { margin-left: 350px; }
            .visit-tabs { margin-top: 20px; }
            .visit-tabs .nav-tabs { border-bottom: 2px solid #dee2e6; }
            .visit-tabs .nav-link { 
              margin-right: 10px;
              padding: 10px 20px;
              font-size: 16px;
            }
            .visit-tabs .nav-link.active {
              font-weight: bold;
              border-bottom: 3px solid #007bff;
            }
              /* Additional table styling for better spacing */
            .dataTables_wrapper { width: 100%; }
            .dataTables_wrapper .dataTable { width: 100% !important; }
            .dt-left { text-align: left !important; }
            .dt-center { text-align: center !important; }
            .comparison-container .form-group { margin-bottom: 0px; }
            
            /* Download controls row */
            .comparison-controls {
              display: flex;
              gap: 16px;
              align-items: flex-end;
              flex-wrap: wrap;
              padding: 4px 2px 12px 2px;
              border-bottom: 1px solid #e0e0e0;
              margin-bottom: 14px;
            }
            .comparison-controls .form-group { margin-bottom: 0; }
            .btn-download-pdf {
              height: 38px;
              font-weight: 600;
              letter-spacing: 0.02em;
              display: inline-flex;
              align-items: center;
              gap: 6px;
            }
            /* Print-only header — hidden on screen */
            .pdf-print-header { display: none; }

            /* ── Print styles ── */
            @media print {
              .main-sidebar,
              .main-header,
              .content-header,
              .box-header,
              .comparison-controls,
              .nav-tabs { display: none !important; }

              .content-wrapper { margin-left: 0 !important; }

              .pdf-print-header {
                display: block !important;
                margin-bottom: 18px;
                padding-bottom: 10px;
                border-bottom: 2px solid #333;
              }
              .pdf-print-header h3 { margin: 0 0 4px; font-size: 16pt; }
              .pdf-print-header p  { margin: 0; font-size: 11pt; color: #444; }

              .tab-pane:not(.active) { display: none !important; }

              /* Remove scroll constraints so full table prints */
              .dataTables_scrollBody {
                overflow: visible !important;
                max-height: none !important;
                height: auto !important;
              }
              .dataTables_wrapper { overflow: visible !important; }

              .card, .box {
                border: none !important;
                box-shadow: none !important;
              }

              @page { margin: 1.5cm; }
            }
          ")),
          tags$script(HTML("
            Shiny.addCustomMessageHandler('triggerPrint', function(msg) {
              window.print();
            });
          "))
        ),
        
        fluidRow(
          bs4Dash::box(
            width = 12,
            title = "Participant Data Comparison by Visit",
            div(
              uiOutput("comparison_tables"),
              class = "comparison-container visit-tabs"
            ),
            align = "center"
          )
        )
      )  #End of dashboardBody
      
      
    )  #End of dashboardPage
  )
}















#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd

golem_add_external_resources <- function(){
  
  #Resource path for CSS
  add_resource_path(
    'www', app_sys('app/www/')
  )
  
  #Resource path for Images (e.g. logo in header)
  add_resource_path(
    'img', system.file('app/img/', package = 'ADRCDash')
  )
  
  #Head tags including favicon
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'UDS Comparison Checker'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

