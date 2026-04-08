#'
#' Like other reports, we begin with some modified calls to ADRCDash for processing purposes
#' Some functions for data processing
#' 
#' @import REDCapR
#'


visit_read_in_alt <- function(token, synth = FALSE, dict = NULL, subtable_dict = NULL, use_redcap_factors = FALSE, ...){
  
  if(synth == FALSE){
    if(is.null(dict)) dict <- ADRCDash:::redcap_dict
    
    #1 - Read in the NACC dataset
    visit_token <- Sys.getenv(token)

    visit_curr <- REDCapR::redcap_read(redcap_uri = "https://redcap.dom.uab.edu/api/", token = visit_token, raw_or_label = "label", guess_type = use_redcap_factors)$data
    if(!(exists("labels_loaded"))) {labels_curr <- colnames(REDCapR::redcap_read(redcap_uri = "https://redcap.dom.uab.edu/api/", token = visit_token, records=1, raw_or_label_headers = "label")$data)
    names(labels_curr) <- colnames(visit_curr)
    } else labels_curr <- labels_loaded
    visit_curr <- visit_curr[,colnames(visit_curr) %in% names(labels_curr)]
    
    #Coerce to data.table for populating
    visit_curr <- data.table::as.data.table(visit_curr)
    setkeyv(visit_curr, dict[["redcap_key"]])
    
    
    #2- Extract subtable if needed (for example, inventory and M1 from )
    if(!is.null(subtable_dict)){
      #INPUT LATER - Currently a second call is made to REDCAP_NACC_API
    } else{
      subtable_curr <- NULL
    }
    
    
    # #3 - Fill down subject_data event and filter to desired event
    # 
    # #First some expected variables from the subject data to the event data
    # #Build the lookup table
    # id_look <- visit_curr[visit_curr[[dict[["event_col"]]]] %in% dict[["subj_event"]],
    #                       .SD[1,], 
    #                       by = eval(dict[["redcap_key"]]),
    #                       .SDcols = dict_copy]
    # #Apply the lookup to the other event
    # visit_curr[visit_curr[[dict[["event_col"]]]] %in% dict[["visit_event"]],
    #            (paste0(dict_copy, dict[["column_annotate"]])) := id_look[.SD, on = dict[["redcap_key"]], mget(dict_copy)]]
    # 
    # #If there are other variables that need to get copied over from the other event, add them to dict_copy above
    # 
    # #Next filter to drop all columns that don't have the desired event annotation
    # cols_to_drop <- grep(paste(c(dict[["visit_col"]], dict[["event_col"]], dict[["redcap_key"]], dict[["column_annotate"]]), collapse="|"), 
    #                      colnames(visit_curr), invert = TRUE)
    # # cols_to_drop <- which(colSums(is.na(visit_curr[visit_curr[[dict[["event_col"]]]] == dict[["visit_event"]],])) == 
    # #                         nrow(visit_curr[visit_curr[[dict[["event_col"]]]] == dict[["visit_event"]],]))
    # 
    # #Assuming it's not length 0, drop those columns
    # if(length(cols_to_drop) > 0){
    #   subj_data_cols <- colnames(visit_curr)[-cols_to_drop]
    #   visit_curr <- visit_curr[,-cols_to_drop, with = FALSE]
    #   labels_curr <- labels_curr[-cols_to_drop]
    # } else {
    #   subj_data_cols <- colnames(visit_curr)
    # }
    
    #Fill down the subject_info rows - this is all done by reference within fill_down_rows so we technically don't need to assign it
    #visit_curr <- ADRCDash:::fill_down_rows(visit_curr, dict = subj_data_cols, fill_key = dict[["redcap_key"]])
    #visit_curr[, (subj_data_cols) := lapply(.SD, zoo::na.locf, na.rm = FALSE), by = eval(dict[["redcap_key"]]), .SDcols = subj_data_cols]
    
    #Finally, drop the undesired event rows and remove the annotation
    visit_curr <- visit_curr[visit_curr[[dict[["event_col"]]]] %in% dict[["visit_event"]],]
    #colnames(visit_curr) <- gsub(dict[["column_annotate"]], "", colnames(visit_curr))
    
  } else{
    visit_curr <- data.table::as.data.table(nacc_synth)
    subtable_curr <- NULL
  }
  
  
  # NOTE: For this app we intentionally keep all rows and do not drop "invalid" rows,
  # since downstream form tabs decide which variables to show and some participants
  # may not have reviewer date fields populated for all forms/visits.
  
  #Finally, reorder the data frames
  data.table::setorderv(visit_curr, cols = c(dict[["adrc_key"]], dict[["visit_col"]]))
  
  
  
  
  
  
  #Return a list with each processed table which can be called as needed
  return(list(visits = visit_curr, subtable = subtable_curr, labels = labels_curr))
}

redcap_process <- function(){
  
  
  #Read in data using modified redcap_readin function from ADRCDash
  #Unfortunately the export_forms_secondary argument isn't really working, causing issues with the downstream merge so we just pull everything from the NACC REDCap
  #.data <- ADRCDash:::redcap_read_in(simple = TRUE, synth = FALSE, use_spinner = FALSE, use_redcap_factors = TRUE)#,
  #export_forms_secondary = c("subject_info", "clinical_consensus_reviewer_1", "clinical_consensus_reviewer_2")
  #.data <- ADRCDash:::visit_read_in(token = "REDCAP_NACC_API_NEW", subtable_dict = NULL, .type = "nacc", synth=FALSE, use_redcap_factors = TRUE)[["visits"]]
  .data_list <- visit_read_in_alt(token = "UDS4_API", dict = uds4_redcap_dict, subtable_dict = NULL, .type = "nacc", synth=FALSE, use_redcap_factors = TRUE)
  .data <- .data_list[["visits"]]
  .labels <- .data_list[["labels"]]
  
  # Keep all rows; do not filter out rows based on reviewer fields.
  
  #Make sure the dataframe is properly sorted according to date
  #.data <- ADRCDash:::redcap_order_rows(.data)
  
  #Build age variable based on DoB
  .data$birthmo <- as.numeric(as.character(gsub("(\\d+)(\\b ).*", "\\1", .data$birthmo)))
  .data[,Age := make_age(.SD, .today=FALSE),by=id_var]
  
  #Make race
  #.data[,race := make_race_var(.SD, .type = "long"),by=id_var]
  
  .labels <- gsub("\\.{3}\\d+$", "", .labels)
  
  #Do a fill down on whatever is left, currently only education and race from A1 based on the new processing done by visit_read_in() in ADRCDash:
  .data <- ADRCDash:::fill_down_rows(.data, dict = c("birthsex", "educ"))
  
  return(list(data = as.data.frame(.data), labels = .labels))
}









#This is the updated version used by ADRCDash, this is just a placeholder until we update the package

make_age <- function(df, yr_var = "birthyr", mo_var = "birthmo", .today = TRUE, alt_date_field = "frmdatea1"){
  
  if(.today == TRUE){
    yr_curr <- lubridate::year(lubridate::today())
    mo_curr <- lubridate::month(lubridate::today())
  } else{
    yr_curr <- lubridate::year(df[[alt_date_field]])
    mo_curr <- lubridate::month(df[[alt_date_field]])
  }
  
  mo_adjust <- as.numeric(mo_curr < df[[mo_var]]) * (-1)
  
  age_curr <- yr_curr - df[[yr_var]] + mo_adjust
  
  return(age_curr)
}




drop_missing_rows <- function(.dat, dict = missing_row_dict){
  
  #Step through the dictionary, identify all rows that are missing all variables in any of the sets (e.g. both reviewer dates or initials are missing)
  #We use negate on the complete.cases to identify any row with an NA in the variable set of interest
  idx_drop <- lapply(missing_row_dict, function(.set){
    which(!complete.cases(.dat[,colnames(.dat) %in% .set]))
  })
  
  #Alternative function that uses apply, this is a slower process though so better to use complete.cases instead of any(is.na())
  # idx_drop <- lapply(missing_row_dict, function(.set){
  #   which(apply(.dat[,colnames(.dat) %in% .set], 1, function(xx){any(is.na(xx))==TRUE}))
  # })
  
  #Use Reduce to get the intersection across all list returns
  idx_drop <- Reduce(intersect, idx_drop)
  
  #Drop the missing rows
  if(length(idx_drop) > 0){
    .dat <- .dat[-idx_drop,]
  }
  
  return(.dat)
  
  
}

#Dictionary for missing rows, we just check to make sure at least one of these pairings exists

missing_row_dict = list(c("frmdated1a_rev1", "frmdated1a_rev2"),
                        c("initialsd1a_rev1", "initialsd1a_rev2"))


#Updated UDS4 dictionary
uds4_redcap_dict <- list(adrc_key = "adc_sub_id",
                         redcap_key = "record_id",
                         subj_event = c("dmsc_only_arm_1", "DMSC Only"),
                         visit_event = c("dmsc_only_arm_1", "DMSC Only"),
                         event_col = "redcap_event_name",
                         visit_col = "redcap_repeat_instance",
                         date_valid = "frmdatea1",
                         column_annotate = "_entry"
)


pull_id <- function(.dat, .id, .id_var = id_var){
  if(!is.null(.id)){
    .out <- .dat[.dat[[.id_var]] %in% .id & !is.na(.dat[[.id_var]]),]
    if(nrow(.out) == 0){ return(NULL)
    } else return(.out)
  } else{
    return(NULL)
  }
}





#Defaults for out of bounds IDs

text_default <- paste("ID not recognized\n",
                      "Please reset and try again")

text_missing <- paste("No Numeric/Syndromal diagnoses listed for ID<br/>",
                      "Reviews assumed blank, please check data")

default_table <- data.frame(V1 = text_default); colnames(default_table) <- ""
missing_table <- data.frame(V1 = " ", V2 = text_missing, V3 = " "); colnames(missing_table) <- c(" ", "  ", "   ")

text_ready <- paste("Tables Ready - Please enter an ADC ID (ADCXXX or number)")
ready_table <- data.frame(V1 = text_ready); colnames(ready_table) <- ""

text_wait <- paste("Please wait, processing ADRC data")
wait_table <- data.frame(V1 = text_wait); colnames(wait_table) <- ""
