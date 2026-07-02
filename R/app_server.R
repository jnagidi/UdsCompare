#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import data.table
#' @noRd
 
 

app_server <- function( input, output, session ) {
  
  # Hold selected participant data after "Get Data"
  data_id_rv <- reactiveVal(NULL)
  searched_rv <- reactiveVal(FALSE)
  current_id_rv <- reactiveVal("")
  uds_fields <- load_uds_fields_of_interest()
  form_levels <- sort(unique(as.character(uds_fields$uds_form)))
  empty_form_table <- data.frame(
    Question = character(0),
    `Recent Visit` = character(0),
    `First Visit` = character(0),
    check.names = FALSE
  )
  
  #Standard call to a modification of the initial_redcap_process of ADRCDash
  data_curr <- redcap_process()
  labels_curr <- data_curr[["labels"]]
  data_curr <- data_curr[["data"]]
  
  # Build a full label map (for ALL columns), so every form can show labels.
  data_cols <- colnames(data_curr)
  labels_curr <- gsub("\\.{3}\\d+$", "", labels_curr)

  if (!is.null(names(labels_curr)) && length(names(labels_curr)) > 0) {
    # Keep only labels that correspond to actual columns
    labels_map <- labels_curr[names(labels_curr) %in% data_cols]
  } else if (length(labels_curr) == length(data_cols)) {
    labels_map <- labels_curr
    names(labels_map) <- data_cols
  } else {
    # Fallback: no reliable label export; use column names as labels
    labels_map <- stats::setNames(data_cols, data_cols)
  }
  # Ensure every column has a fallback label
  if (!all(data_cols %in% names(labels_map))) {
    missing_cols <- setdiff(data_cols, names(labels_map))
    labels_map <- c(labels_map, stats::setNames(missing_cols, missing_cols))
  }

  # Keep the name `ccc_labels` since you’re using it elsewhere
  ccc_labels <- labels_map
  
  # Initial UI (tabset of forms, empty/ready table)
  output$comparison_tables <- renderUI({
    current_id <- current_id_rv()
    tabs <- lapply(form_levels, function(f) {
      tabPanel(title = toupper(f), value = toupper(f), DT::DTOutput(paste0("table_", f)))
    })
    tagList(
      # Print-only header (hidden on screen via CSS, shown @media print)
      div(
        class = "pdf-print-header",
        tags$h3("ADRC UDS Data Comparison"),
        if (nzchar(current_id))
          tags$p(paste0("Participant ID: ", current_id))
        else
          NULL
      ),
      # Controls row: form picker + download button
      div(
        class = "comparison-controls",
        shiny::selectInput(
          inputId = "form_pick",
          label = "Select Form",
          choices = toupper(form_levels),
          selected = if ("a1" %in% form_levels) "A1" else if (length(form_levels) > 0) toupper(form_levels[[1]]) else NULL,
          width = "180px"
        ),
        shiny::actionButton(
          inputId = "download_pdf",
          label = tagList(shiny::icon("file-pdf"), "Download Report"),
          class = "btn btn-danger btn-download-pdf"
        )
      ),
      do.call(
        tabsetPanel,
        c(
          list(id = "form_tabs", type = "tabs"),
          tabs
        )
      )
    )
  })
        
  for (f in form_levels) {
    local({
      ff <- f
      output[[paste0("table_", ff)]] <- DT::renderDT(empty_form_table, options = build_participant_DT_options(), escape = FALSE)
    })
  }
  
  #Everything else is held within the observe event button
  observeEvent(input$compare_button, {
    
    #Process the ID
    id_curr <- reactive({process_id(input$id_entry)})
    
    
    #Get the current ID of interest
    data_proc <- pull_id(data_curr, id_curr())
    data_id_rv(data_proc)
    searched_rv(TRUE)
    current_id_rv(id_curr())
    
    # Default to a1 after loading (also applies after failed search)
    if ("a1" %in% form_levels) {
      updateTabsetPanel(session, "form_tabs", selected = "A1")
      updateSelectInput(session, "form_pick", selected = "A1")
    } else if (length(form_levels) > 0) {
      updateTabsetPanel(session, "form_tabs", selected = toupper(form_levels[[1]]))
      updateSelectInput(session, "form_pick", selected = toupper(form_levels[[1]]))
    }
    
  })
  
  observeEvent(input$form_pick, {
    if (!is.null(input$form_pick) && nzchar(input$form_pick)) {
      updateTabsetPanel(session, "form_tabs", selected = input$form_pick)
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$form_tabs, {
    if (!is.null(input$form_tabs) && nzchar(input$form_tabs)) {
      updateSelectInput(session, "form_pick", selected = input$form_tabs)
    }
  }, ignoreInit = TRUE)
  
  
  observeEvent(input$download_pdf, {
    session$sendCustomMessage("triggerPrint", list())
  })
  
  # Render each form tab table from UDSv4_fields_of_interest.rds mapping
  for (f in form_levels) {
    local({
      ff <- f
      output[[paste0("table_", ff)]] <- DT::renderDT({
        data_proc <- data_id_rv()
        if (!isTRUE(searched_rv())) return(empty_form_table)
        if (is.null(data_proc)) return(default_table)
        vars <- get_form_vars_from_uds_fields(uds_fields, ff)
        make_participant_compare_table(
          .data_id = data_proc,
          .form = ff,
          .vars = vars,
          .labels = ccc_labels
        )
      }, options = build_participant_DT_options(), escape = FALSE)
    })
  }
  
}
