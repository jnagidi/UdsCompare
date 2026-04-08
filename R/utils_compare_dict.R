#'
#'
#' Comparison dictionaries and functions to build dictionaries
#'

#Static variables
id_var <- c("adc_sub_id")
event_var <- c("redcap_repeat_instance")
demog_cols <- c("Age", "race", "birthsex", "educ")
reviewer_cols <- c("frmdated1a_rev1", "frmdated1a_rev2", "initialsd1a_rev1", "initialsd1a_rev2")

#Dictionaries for build_header
header_columns_dict <- list(col_var = c(id_var, event_var, "Age", "race", "birthsex", "educ"),
                            numeric_var = c(rep(FALSE, 5), TRUE))

race_recode <- c("Black or African American" = "B/AA", "White" = "W")

#D1 colnames from data dictionary
#Variant using naccDataDict
# colnames_ccc <- paste0("ccc_", c("cogstat_c2", naccDataDict::redcap_nacc_data[["IVP"]][["D1"]], 
#                                  "clin_notes_supp", "clin_notes_anti", "syndrm_stg", "numeric_stg"), "_rev\\d*")
# colnames_ccc_text_str <- "clin_notes|((oth|ftld).*?x)"

#Since naccDataDict is only called here, we're just calling the D1 names here to avoid dispendencies
D1_redcap_names=
  c("dxmethod", "normcog", "scd", "scddxconf", "demented", "mcicritcln", "mcicritimp", "mcicritfun", "mci ", 
    "impnomcifu", "impnomcicg", "impnomclcd", "impnomcio", "impnomciox", "impnomci", 
    "cdommem ", "cdomlang", "cdomattn ", "cdomexec", "cdomvisu", "cdombeh", "cdomaprax", 
    "mbi", "bdommot", "bdomafreg", "bdomimp", "bdomsocial", "bdomthts", "predomsyn", 
    "amndem", "dyexecsyn", "pca", "ppasyn", "ppasynt", "ftdsyn", "lbdsyn", "lbdsynt", 
    "namndem", "pspsyn", "pspsynt", "ctesyn", "cbssyn", "msasyn", "msasynt", "othsyn", "othsynx", 
    "syninfclin", "syninfctst", "syninfbiom", "majdepdx", "majdepdif", "othdepdx", "othdepdif", 
    "bipoldx", "bipoldif", "schizop", "schizoif", "anxiet", "anxietif", "genanx", "panicdisdx", "ocddx", "othanxd", "othanxdx", 
    "ptsddx", "ptsddxif", "ndevdis", "ndevdisif", "delir", "delirif", "othpsy", "othpsyif", "othpsyx", "tbidx", "tbidxif", 
    "epilep", "epilepif", "hyceph", "hycephif", "neop", "neopif", "neopstat", "hiv", "hivif", "postc19", "postc19if", "apneadx", "apneadxif", 
    "othcogill", "othcillif", "othcogillx", "alcdem", "alcdemif", "impsub", "impsubif", "meds", "medsif", "cogoth", "cogothif", 
    "cogothx", "cogoth2", "cogoth2f", "cogoth2x", "cogoth3", "cogoth3f", "cogoth3x", 
    "alzdis", "alzdisif", "lbdis", "lbdif", "ftld", "psp", "pspif", "cort", "cortif", "ftldmo", "ftldmoif", "ftldnos", "ftldnoif", "ftldsubt", "ftldsubx", 
    "cvd", "cvdif", "msa", "msaif", "cte", "cteif", "ctecert", "downs", "downsif", "hunt", "huntif", "prion", "prionif", "caa", "caaif", "late", "lateif", 
    "othcog", "othcogif", "othcogx")

colnames_ccc <- paste0(c("cogstat_c2", D1_redcap_names, "exclude_partic","exclude_notes",
                         #"clin_notes_supp", "clin_notes_anti", "syndrm_stg", "numeric_stg"), "_rev\\d*")
                         "clin_notes_supp", "clin_notes_anti"), "_rev\\d*")
colnames_ccc_text_str <- "clin_notes|((oth|ftld).*?x)"

#The string used to get the columns used in table building
col_pull_grep <- paste("^(", paste(c(id_var, event_var, demog_cols, reviewer_cols, colnames_ccc), collapse = "|"), ")$", sep ="")

# Form -> base columns dictionary.
# This is intentionally minimal: you can expand/replace it with your real mapping.
form_columns_dict <- list(
  a1 = character(0),
  a2 = character(0),
  a3 = character(0),
  a4 = character(0),
  a5d2 = character(0),
  b1 = character(0),
  b4 = character(0),
  b5 = character(0),
  b6 = character(0),
  b7 = character(0),
  b8 = character(0),
  b9 = character(0),
  c2 = character(0),
  d1a = character(0)
)


#Functions to pull dictionary columns for comparisons
get_ccc_cols <- function(.dat){
  cols_curr <- setdiff(colnames(.dat), c(id_var, event_var, demog_cols))
  return(cols_curr)
}


#Function to get REDCap labels
get_redcap_labels <- function(.dat, redcap_cols,
                              subset_string = "_rev1(_entry)?$",
                              rename_string_in = "(.*?)_rev1(_entry)?$", 
                              rename_string_out = "\\1"){
  
  #Extract labels
  if(is.data.frame(.dat)){
    labels_curr <- do.call(c, lapply(.dat[,colnames(.dat) %in% redcap_cols], function(.var){attributes(.var)$label}))
  } else labels_curr <- .dat
  
  #Drop HTML
  labels_curr <- gsub("<.*?>", "", labels_curr)
  
  #Subset based on reviewer 1, name according to redcap variable names using rename_string arguments
  labels_curr <- labels_curr[grep(subset_string, names(labels_curr))]
  names(labels_curr) <- gsub(rename_string_in, rename_string_out, names(labels_curr))
  return(labels_curr)
}






#Function to process the ID
process_id <- function(.id, .head = "ADC"){
  
  #Process header if needed
  if(length(grep(.head, .id, ignore.case = TRUE)) > 0) .id <- as.numeric(gsub(.head, "", .id, ignore.case = TRUE))
  
  #Return NULL if not a valid number
  if(is.na(as.numeric(.id)) | as.numeric(.id) < 0) return(NULL)
  .id <- as.numeric(.id)
  
  #Add leading 0's
  if(.id < 10){ .id <- paste0("000", .id)
  } else if(.id < 100){ .id <- paste0("00", .id)
  } else if(.id < 1000){ .id <- paste0("0", .id)}
  
  return(paste0(.head, .id))
  
}



#Dictionary of variables to copy from one event to another
dict_copy <- c("adc_sub_id", "frmdatea1", "birthmo", "birthyr", "raceaian", "raceasian", "raceblack", "ethispanic", "racemena", "racenhpi", "racewhite", "birthsex", "educ")
