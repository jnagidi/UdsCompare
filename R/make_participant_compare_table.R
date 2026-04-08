make_participant_compare_table <- function(
    .data_id,
    .form,
    .vars,
    .labels = NULL,
    .instance_var = event_var
) {
  if (is.null(.data_id) || nrow(.data_id) == 0) {
    return(default_table)
  }

  if (!.instance_var %in% colnames(.data_id)) {
    stop("Instance variable not found in data.")
  }

  instances <- suppressWarnings(as.numeric(as.character(.data_id[[.instance_var]])))
  if (all(is.na(instances))) {
    return(default_table)
  }

  idx_first <- which(instances == min(instances, na.rm = TRUE))[1]
  idx_last <- which(instances == max(instances, na.rm = TRUE))[1]

  cols_base <- .vars

  if (length(cols_base) == 0) {
    out <- data.frame(
      Question = paste0("No columns configured for form: ", .form),
      `Recent Visit` = "",
      `First Visit` = "",
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    return(out)
  }

  data_cols <- colnames(.data_id)

  # Build questions/labels
  question_text <- cols_base
  if (!is.null(.labels) && length(.labels) > 0 && !is.null(names(.labels))) {
    question_text <- vapply(
      cols_base,
      function(v) {
        lab <- if (v %in% names(.labels)) .labels[[v]] else NA_character_
        if (!is.na(lab) && nzchar(as.character(lab))) as.character(lab) else v
      },
      FUN.VALUE = character(1)
    )
  }

  # Pull values for each requested var_name; if column missing, return blank
  val_last <- vapply(
    cols_base,
    function(v) {
      if (v %in% data_cols) {
        x <- as.character(.data_id[idx_last, v, drop = TRUE])
        ifelse(is.na(x), "", x)
      } else {
        ""
      }
    },
    FUN.VALUE = character(1)
  )
  val_first <- vapply(
    cols_base,
    function(v) {
      if (v %in% data_cols) {
        x <- as.character(.data_id[idx_first, v, drop = TRUE])
        ifelse(is.na(x), "", x)
      } else {
        ""
      }
    },
    FUN.VALUE = character(1)
  )

  out <- data.frame(
    Question = question_text,
    `Recent Visit` = unname(val_last),
    `First Visit` = unname(val_first),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  # Drop rows where both visits are blank
  keep_idx <- !(out[["Recent Visit"]] == "" & out[["First Visit"]] == "")
  out <- out[keep_idx, , drop = FALSE]

#   # Conclusion row matching existing table style
#   if (nrow(out) > 0) {
#     if (all(out$`Recent Visit` == out$`First Visit`)) {
#       conclusion <- c("<b>NO DISCREPANCIES</b>", "", "")
#     } else {
#       conclusion <- c("<b>DIFFERENCES NOTED</b>", "", "")
#     }
#     names(conclusion) <- colnames(out)
#     out <- rbind(out, conclusion)
#   }
# 
#   out
# }

  out
}

build_participant_DT_options <- function() {
  list(
    paging = FALSE,
    autoWidth = FALSE,
    dom = "t",
    columnDefs = list(
      list(width = "15%", targets = 0, searchable = FALSE),
      list(width = "42.5%", targets = 1, searchable = FALSE, className = "dt-left"),
      list(width = "42.5%", targets = 2, searchable = FALSE, className = "dt-left")
    ),
    scrollY = "70vh",
    scrollX = TRUE,
    scrollCollapse = TRUE,
    language = list(
      zeroRecords = "",
      emptyTable = ""
    )
  )
}

