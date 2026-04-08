uds_fields_of_interest_path <- function() {
  # Preferred: if later moved into package extdata
  p1 <- system.file("extdata", "UDSv4_fields_of_interest_updated.rds", package = "UdsCompare")
  if (nzchar(p1) && file.exists(p1)) return(p1)

  # Dev/workspace fallback (current repo layout)
  p2 <- file.path(getwd(), "UDSv4_fields_of_interest_updated.rds")
  if (file.exists(p2)) return(p2)

  # Alternate: project root one level up when running inside package dir
  p3 <- normalizePath(file.path(getwd(), "..", "UDSv4_fields_of_interest_updated.rds"), winslash = "/", mustWork = FALSE)
  if (file.exists(p3)) return(p3)

  ""
}

load_uds_fields_of_interest <- function(path = uds_fields_of_interest_path()) {
  if (!nzchar(path) || !file.exists(path)) {
    stop("Could not find UDSv4_fields_of_interest.rds. Expected at project root or package extdata.")
  }
  x <- readRDS(path)
  if (!all(c("uds_form", "var_name") %in% colnames(x))) {
    stop("UDSv4_fields_of_interest must include columns: uds_form, var_name")
  }
  x
}

get_form_vars_from_uds_fields <- function(.uds_fields, .form) {
  vars <- .uds_fields[.uds_fields$uds_form == .form, "var_name", drop = TRUE]
  vars <- unique(as.character(vars))
  vars <- vars[nzchar(vars)]
  vars
}

