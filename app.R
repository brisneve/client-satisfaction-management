library(shiny)
library(bslib)
library(dplyr)
library(readr)
library(tibble)
library(tidyverse)
library(plotly)
library(openxlsx)

service_file <- if (file.exists("upcebu_services.csv")) {
  "upcebu_services.csv"
} else if (file.exists("/mnt/data/upcebu_services.csv")) {
  "/mnt/data/upcebu_services.csv"
} else {
  stop("upcebu_services.csv not found.")
}

responses_file <- if (file.exists("upcebu_csm_responses.csv")) {
  "upcebu_csm_responses.csv"
} else if (file.exists("/mnt/data/upcebu_csm_responses.csv")) {
  "/mnt/data/upcebu_csm_responses.csv"
} else if (file.exists("upcebu_csm_responses(1).csv")) {
  "upcebu_csm_responses(1).csv"
} else if (file.exists("/mnt/data/upcebu_csm_responses(1).csv")) {
  "/mnt/data/upcebu_csm_responses(1).csv"
} else {
  "upcebu_csm_responses.csv"
}
settings_file <- "upcebu_csm_settings.csv"
users_file <- if (file.exists("upc_csm_users.csv")) {
  "upc_csm_users.csv"
} else if (file.exists("/mnt/data/upc_csm_users.csv")) {
  "/mnt/data/upc_csm_users.csv"
} else {
  stop("upc_csm_users.csv not found.")
}

`%||%` <- function(x, y) {
  if (length(x) == 0 || is.null(x) || all(is.na(x))) y else x
}

normalize_service_columns <- function(df) {
  nms <- names(df)
  nms_clean <- tolower(gsub("[^a-z0-9]+", "_", nms))
  names(df) <- nms_clean
  
  pick_col <- function(candidates) {
    hit <- intersect(candidates, names(df))
    if (length(hit) > 0) hit[[1]] else NULL
  }
  
  seq_office_col <- pick_col(c("seq_office", "office_seq", "office_no", "seq"))
  seq_services_col <- pick_col(c("seq_services", "service_seq", "seq_service", "service_no"))
  office_col <- pick_col(c("office", "office_code", "group", "office_short"))
  office_name_col <- pick_col(c("office_name", "officename", "name_of_office", "office_full_name", "office"))
  office_display_col <- pick_col(c("office_display_name", "office_full_name", "full_office_name", "name_of_office", "office_name", "officename"))
  type_col <- pick_col(c("type", "type_service", "service_type", "transaction_type"))
  services_col <- pick_col(c("services", "service", "service_name", "name_of_service"))
  
  if (is.null(type_col)) {
    df$type <- "External"
    type_col <- "type"
  }
  
  if (is.null(seq_office_col)) df$seq_office <- NA_character_
  if (is.null(seq_services_col)) df$seq_services <- NA_character_
  if (is.null(office_col)) df$office <- NA_character_
  if (is.null(office_name_col)) stop("The services file must contain an office column, such as 'office_name' or 'office'.")
  if (is.null(services_col)) stop("The services file must contain a service column, such as 'services' or 'service'.")
  
  out <- df %>%
    mutate(
      seq_office   = as.character(.data[[seq_office_col %||% "seq_office"]]),
      seq_services = as.character(.data[[seq_services_col %||% "seq_services"]]),
      office       = as.character(.data[[office_col %||% "office"]]),
      office_name  = as.character(.data[[office_name_col]]),
      office_display_name = as.character(.data[[office_display_col %||% office_name_col]]),
      type         = as.character(.data[[type_col]]),
      services     = as.character(.data[[services_col]])
    ) %>%
    mutate(
      office_display_name = dplyr::case_when(
        is.na(office_display_name) | !nzchar(trimws(office_display_name)) ~ office_name,
        nchar(trimws(office_display_name)) < nchar(trimws(office_name)) ~ office_name,
        TRUE ~ office_display_name
      )
    ) %>%
    select(seq_office, seq_services, office, office_name, office_display_name, type, services, everything()) %>%
    arrange(office_display_name, type, services)
  
  out
}

services_df <- read_csv(service_file, show_col_types = FALSE) %>%
  normalize_service_columns()

get_office_choices <- function(df, include_all = FALSE) {
  office_map <- df %>%
    dplyr::select(office, office_name, office_display_name) %>%
    dplyr::mutate(
      office = as.character(office),
      office_name = as.character(office_name),
      office_display_name = dplyr::case_when(
        !is.na(office_display_name) & nzchar(trimws(office_display_name)) ~ as.character(office_display_name),
        !is.na(office_name) & nzchar(trimws(office_name)) ~ as.character(office_name),
        TRUE ~ as.character(office)
      )
    ) %>%
    dplyr::distinct() %>%
    dplyr::filter(!is.na(office) & nzchar(trimws(office))) %>%
    dplyr::arrange(office_display_name)
  
  choices <- stats::setNames(office_map$office, office_map$office_display_name)
  
  if (include_all) {
    choices <- c("All Offices" = "All", choices)
  }
  
  choices
}

get_selected_office_name <- function(df, selected_office) {
  if (is.null(selected_office) || !nzchar(trimws(selected_office))) {
    return(selected_office)
  }
  
  matched <- df %>%
    dplyr::filter(office == selected_office | office_name == selected_office | office_display_name == selected_office) %>%
    dplyr::distinct(office_name) %>%
    dplyr::slice(1)
  
  if (nrow(matched) == 0) selected_office else matched$office_name[[1]]
}


read_admin_users <- function() {
  out <- suppressWarnings(read_csv(users_file, show_col_types = FALSE))
  names(out) <- tolower(names(out))
  
  if (!all(c("username", "password", "office_code") %in% names(out))) {
    stop("upc_csm_users.csv must contain 'username', 'password', and 'office_code' columns.")
  }
  
  out %>%
    transmute(
      username    = trimws(as.character(username)),
      password    = trimws(as.character(password)),
      office_code = trimws(as.character(office_code))
    ) %>%
    filter(nzchar(username), nzchar(password), nzchar(office_code)) %>%
    distinct()
}

admin_users_df <- read_admin_users()

get_selected_office_display_name <- function(df, selected_office) {
  if (is.null(selected_office) || !nzchar(trimws(selected_office))) {
    return(selected_office)
  }
  
  matched <- df %>%
    dplyr::filter(office == selected_office | office_name == selected_office | office_display_name == selected_office) %>%
    dplyr::distinct(office_display_name, office_name, office) %>%
    dplyr::slice(1)
  
  if (nrow(matched) == 0) {
    selected_office
  } else if (!is.na(matched$office_display_name[[1]]) && nzchar(trimws(matched$office_display_name[[1]]))) {
    matched$office_display_name[[1]]
  } else if (!is.na(matched$office_name[[1]]) && nzchar(trimws(matched$office_name[[1]]))) {
    matched$office_name[[1]]
  } else {
    matched$office[[1]]
  }
}

filter_by_office_selection <- function(df, selected_office) {
  if (is.null(selected_office) || !nzchar(trimws(as.character(selected_office))) || identical(selected_office, "All")) {
    return(df)
  }
  
  df %>%
    dplyr::filter(
      office == selected_office |
        office_name == selected_office |
        office_display_name == selected_office
    )
}


is_cc_listed_service <- function(service_name, office_name = NULL, office_display_name = NULL, office_code = NULL, type = NULL) {
  svc <- trimws(as.character(service_name %||% ""))
  if (!nzchar(svc)) return(FALSE)
  
  ref <- services_df %>%
    mutate(
      services = trimws(as.character(services)),
      office = as.character(office),
      office_name = as.character(office_name),
      office_display_name = as.character(office_display_name),
      type = as.character(type)
    )
  
  matched <- ref %>%
    filter(services == svc)
  
  if (!is.null(office_code) && nzchar(trimws(as.character(office_code)))) {
    matched <- matched %>% filter(office == as.character(office_code))
  } else if (!is.null(office_name) && nzchar(trimws(as.character(office_name)))) {
    matched <- matched %>% filter(office_name == as.character(office_name))
  } else if (!is.null(office_display_name) && nzchar(trimws(as.character(office_display_name)))) {
    matched <- matched %>% filter(office_display_name == as.character(office_display_name))
  }
  
  if (!is.null(type) && nzchar(trimws(as.character(type)))) {
    matched <- matched %>% filter(type == as.character(type))
  }
  
  nrow(matched) > 0
}

filter_service_scope <- function(df, scope_mode = "cc_listed_only") {
  if (nrow(df) == 0) return(df)
  
  if (identical(scope_mode, "all_services")) {
    return(df)
  }
  
  office_code_vec <- if ("office" %in% names(df)) as.character(df$office) else if ("office_code" %in% names(df)) as.character(df$office_code) else rep(NA_character_, nrow(df))
  office_name_vec <- if ("office_name" %in% names(df)) as.character(df$office_name) else rep(NA_character_, nrow(df))
  office_display_vec <- if ("office_display_name" %in% names(df)) as.character(df$office_display_name) else rep(NA_character_, nrow(df))
  type_vec <- if ("service_type" %in% names(df)) as.character(df$service_type) else if ("type" %in% names(df)) as.character(df$type) else rep(NA_character_, nrow(df))
  
  keep <- mapply(
    FUN = is_cc_listed_service,
    service_name = as.character(df$service_name),
    office_name = office_name_vec,
    office_display_name = office_display_vec,
    office_code = office_code_vec,
    type = type_vec,
    SIMPLIFY = TRUE,
    USE.NAMES = FALSE
  )
  
  df[keep %in% TRUE, , drop = FALSE]
}

if (!file.exists(responses_file)) {
  write_csv(tibble(), responses_file)
}

read_responses <- function() {
  if (!file.exists(responses_file) || file.info(responses_file)$size == 0) {
    return(tibble())
  }
  
  suppressWarnings(
    read_csv(responses_file, show_col_types = FALSE)
  ) %>%
    mutate(across(everything(), as.character))
}


read_settings <- function() {
  base_cols <- tibble(
    office_name = character(),
    dashboard_year = integer(),
    service_type = character(),
    service_name = character(),
    csm_coverage = character(),
    total_transaction = double(),
    remarks = character(),
    saved_at = character()
  )
  
  if (!file.exists(settings_file) || file.info(settings_file)$size == 0) {
    return(base_cols)
  }
  
  out <- suppressWarnings(read_csv(settings_file, show_col_types = FALSE))
  out <- out %>% mutate(across(everything(), as.character))
  
  if (!"office_name" %in% names(out)) out$office_name <- character(nrow(out))
  if (!"dashboard_year" %in% names(out)) out$dashboard_year <- character(nrow(out))
  if (!"service_type" %in% names(out)) out$service_type <- character(nrow(out))
  if (!"service_name" %in% names(out)) out$service_name <- character(nrow(out))
  if (!"csm_coverage" %in% names(out)) out$csm_coverage <- character(nrow(out))
  if (!"total_transaction" %in% names(out)) out$total_transaction <- character(nrow(out))
  if (!"remarks" %in% names(out)) out$remarks <- character(nrow(out))
  if (!"saved_at" %in% names(out)) out$saved_at <- character(nrow(out))
  
  out %>%
    transmute(
      office_name = as.character(office_name),
      dashboard_year = suppressWarnings(as.integer(dashboard_year)),
      service_type = as.character(service_type),
      service_name = as.character(service_name),
      csm_coverage = as.character(csm_coverage),
      total_transaction = suppressWarnings(as.numeric(total_transaction)),
      remarks = as.character(remarks),
      saved_at = as.character(saved_at)
    )
}

write_settings <- function(df) {
  out <- df %>%
    transmute(
      office_name = as.character(office_name),
      dashboard_year = suppressWarnings(as.integer(dashboard_year)),
      service_type = as.character(service_type),
      service_name = as.character(service_name),
      csm_coverage = as.character(csm_coverage),
      total_transaction = suppressWarnings(as.numeric(total_transaction)),
      remarks = as.character(remarks),
      saved_at = as.character(saved_at)
    )
  
  write_csv(out, settings_file, na = "")
}


get_latest_settings_export <- function() {
  settings_df <- read_settings()
  
  if (nrow(settings_df) == 0) {
    return(tibble(
      office_name = character(),
      dashboard_year = integer(),
      service_type = character(),
      service_name = character(),
      csm_coverage = character(),
      total_transaction = double(),
      remarks = character(),
      saved_at = character()
    ))
  }
  
  settings_df %>%
    mutate(
      office_name = as.character(office_name),
      service_type = as.character(service_type),
      service_name = as.character(service_name),
      dashboard_year = suppressWarnings(as.integer(dashboard_year)),
      total_transaction = suppressWarnings(as.numeric(total_transaction)),
      saved_at_dt = suppressWarnings(as.POSIXct(saved_at, format = "%Y-%m-%d %H:%M:%S", tz = Sys.timezone()))
    ) %>%
    arrange(
      office_name,
      dashboard_year,
      service_type,
      service_name,
      desc(saved_at_dt),
      desc(saved_at)
    ) %>%
    group_by(office_name, dashboard_year, service_type, service_name) %>%
    slice(1) %>%
    ungroup() %>%
    select(
      office_name,
      dashboard_year,
      service_type,
      service_name,
      csm_coverage,
      total_transaction,
      remarks,
      saved_at
    ) %>%
    arrange(office_name, dashboard_year, service_type, service_name)
}


profile_age <- c(
  "19 or lower",
  "20 - 34",
  "35 - 49",
  "50 - 64",
  "65 or higher",
  "Prefer not to disclose"
)

profile_sex <- c("Male", "Female", "Prefer not to disclose")

profile_region <- c(
  "Region I",
  "Region II",
  "Region III",
  "Region IV-A CALABARZON",
  "Region IV-B MIMAROPA",
  "Region V",
  "Region VI",
  "Region VII",
  "Region VIII",
  "Region IX",
  "Region X",
  "Region XI",
  "Region XII",
  "Region XIII",
  "NCR",
  "CAR",
  "BARMM",
  "International/Outside PH",
  "Prefer not to disclose"
)

profile_business <- c("Citizen", "Business", "Government (Employee or Another Agency)", "Prefer not to disclose")


remarks_reason_choices <- c(
  "Absence of requests for the service",
  "Seasonal demand",
  "Low client turnout",
  "Few transactions",
  "Short collection period",
  "Incomplete monitoring of service transactions",
  "Client declined survey",
  "Changes in administrative processes",
  "Other"
)

split_saved_remarks <- function(x) {
  x <- x %||% ""
  x <- trimws(as.character(x))
  if (!nzchar(x)) {
    return(list(selected = character(0), other = ""))
  }
  
  parts <- trimws(unlist(strsplit(x, ";", fixed = TRUE)))
  parts <- parts[nzchar(parts)]
  if (length(parts) == 0) {
    return(list(selected = character(0), other = ""))
  }
  
  other_part <- ""
  selected <- character(0)
  
  for (p in parts) {
    if (grepl("^Other\\s*:", p, ignore.case = TRUE)) {
      selected <- unique(c(selected, "Other"))
      other_part <- trimws(sub("^Other\\s*:\\s*", "", p, ignore.case = TRUE))
    } else if (p %in% remarks_reason_choices) {
      selected <- unique(c(selected, p))
    } else {
      selected <- unique(c(selected, "Other"))
      other_part <- trimws(p)
    }
  }
  
  list(selected = selected, other = other_part)
}

collapse_remarks_input <- function(selected, other_text = "") {
  selected <- as.character(selected %||% character(0))
  selected <- selected[nzchar(selected)]
  other_text <- trimws(as.character(other_text %||% ""))
  
  out <- selected[selected != "Other"]
  if ("Other" %in% selected && nzchar(other_text)) {
    out <- c(out, paste0("Other: ", other_text))
  }
  paste(out, collapse = "; ")
}

cc1_choices <- c(
  "1. I know what a CC is and I saw this office's CC.",
  "2. I know what a CC is but I did not see this office's CC.",
  "3. I learned of the CC only when I saw this office's CC.",
  "4. I do not know what a CC is and I did not see this office's CC."
)

cc2_choices <- c(
  "1. Easy to see",
  "2. Somewhat easy to see",
  "3. Difficult to see",
  "4. Not visible at all"
)

cc3_choices <- c(
  "1. Helped very much",
  "2. Somewhat helped",
  "3. Did not help"
)

likert_levels <- c(
  "Strongly Agree",
  "Agree",
  "Neither Agree nor Disagree",
  "Disagree",
  "Strongly Disagree",
  "Not applicable"
)

default_sqd_value <- "Strongly Agree"

sqd_labels <- c(
  sqd_0 = "0. Overall Satisfaction",
  sqd_1 = "1. Responsiveness",
  sqd_2 = "2. Reliability",
  sqd_3 = "3. Access and Facilities",
  sqd_4 = "4. Communication",
  sqd_5 = "5. Costs",
  sqd_6 = "6. Integrity",
  sqd_7 = "7. Assurance",
  sqd_8 = "8. Outcome"
)

up_maroon <- "#7B1113"
up_gold   <- "#F2A900"
up_cream  <- "#FAF7F2"
up_ink    <- "#2B2B2B"

app_theme <- bs_theme(
  version = 5,
  bg = up_cream,
  fg = up_ink,
  primary = up_maroon,
  secondary = up_gold,
  base_font = font_google("Inter")
)

make_form_card <- function(title = NULL, subtitle = NULL, ..., card_class = NULL) {
  div(
    class = paste("up-card", card_class %||% ""),
    if (!is.null(title)) h4(class = "up-card-title", title),
    if (!is.null(subtitle)) p(class = "up-card-subtitle", subtitle),
    ...
  )
}

service_input_id <- function(service_index, sqd_index) {
  paste0("svc_", service_index, "_sqd_", sqd_index)
}


settings_input_id <- function(field, row_index) {
  paste0("settings_", field, "_", row_index)
}

settings_sample_output_id <- function(row_index) {
  paste0("settings_sample_size_", row_index)
}

settings_other_reason_output_id <- function(row_index) {
  paste0("settings_other_reason_ui_", row_index)
}

settings_other_reason_input_id <- function(row_index) {
  paste0("settings_other_reason_", row_index)
}

settings_remarks_button_id <- function(row_index) {
  paste0("settings_remarks_btn_", row_index)
}

settings_remarks_apply_id <- function(row_index) {
  paste0("settings_remarks_apply_", row_index)
}

settings_remarks_modal_choices_id <- function(row_index) {
  paste0("settings_remarks_modal_choices_", row_index)
}

settings_remarks_modal_other_id <- function(row_index) {
  paste0("settings_remarks_modal_other_", row_index)
}

get_service_sqd_ids <- function(selected_services_vec) {
  if (length(selected_services_vec) == 0) return(character(0))
  unlist(lapply(seq_along(selected_services_vec), function(i) {
    paste0("svc_", i, "_sqd_", 0:8)
  }), use.names = FALSE)
}

make_service_sqd_matrix <- function(service_label, service_index, choices, default_value) {
  tags$div(
    class = "service-sqd-card",
    tags$div(class = "service-sqd-header", paste0("Service: ", service_label)),
    tags$div(
      class = "sqd-matrix-wrap",
      tags$table(
        class = "sqd-matrix-table",
        tags$thead(
          tags$tr(
            tags$th("Service Quality Dimension", class = "sqd-col-question"),
            lapply(choices, function(choice) tags$th(choice, class = "sqd-col-choice"))
          )
        ),
        tags$tbody(
          lapply(seq_along(sqd_labels) - 1, function(idx) {
            input_id <- service_input_id(service_index, idx)
            tags$tr(
              tags$td(unname(sqd_labels[idx + 1]), class = "sqd-row-label"),
              lapply(choices, function(choice) {
                tags$td(
                  class = "sqd-cell",
                  tags$input(
                    type = "radio",
                    name = input_id,
                    value = choice,
                    class = "sqd-radio",
                    `data-service-group` = paste0("svc_", service_index),
                    `data-sqd-index` = as.character(idx),
                    checked = if (identical(choice, default_value)) "checked" else NULL
                  )
                )
              })
            )
          })
        )
      )
    )
  )
}

css_text <- paste0(
  "
html, body {
  width: 100%;
  overflow-x: hidden;
}
body {
  background:
    linear-gradient(180deg, rgba(123,17,19,0.05) 0%, rgba(250,247,242,1) 180px),
    ", up_cream, ";
  color: ", up_ink, ";
}
.container-fluid {
  padding-left: 0 !important;
  padding-right: 0 !important;
}
.main-wrap {
  width: 50vw;
  min-width: 900px;
  max-width: 1400px;
  margin: 0 auto;
  padding-top: 24px;
  padding-bottom: 48px;
  box-sizing: border-box;
}
.up-header-band {
  width: 100%;
  height: 12px;
  border-radius: 16px 16px 0 0;
  background: linear-gradient(90deg, ", up_maroon, " 0%, ", up_gold, " 100%);
}
.up-header {
  width: 100%;
  box-sizing: border-box;
  background: #ffffff;
  border-radius: 0 0 16px 16px;
  padding: 28px 32px 24px 32px;
  box-shadow: 0 10px 30px rgba(42, 32, 24, 0.08);
  border: 1px solid rgba(123,17,19,0.08);
  margin-bottom: 18px;
}
.up-title {
  font-size: 2rem;
  font-weight: 800;
  line-height: 1.15;
  color: ", up_maroon, ";
  margin-bottom: 8px;
}
.up-subtitle {
  color: #5c5c5c;
  margin: 0;
  line-height: 1.55;
}
.up-card {
  width: 100%;
  box-sizing: border-box;
  background: #ffffff;
  border-radius: 16px;
  padding: 24px 28px;
  margin-bottom: 18px;
  box-shadow: 0 10px 30px rgba(42, 32, 24, 0.06);
  border: 1px solid rgba(123,17,19,0.08);
}
.up-card.form-card-centered {
  width: 100%;
  min-width: 0;
  max-width: none;
  margin-left: 0;
  margin-right: 0;
}
.form-band-center,
.form-input-center,
.up-card.form-card-centered .up-card-title,
.up-card.form-card-centered .step-nav {
  width: 60%;
  margin-left: 20%;
  margin-right: 20%;
}
.form-input-center .shiny-input-container {
  width: 100%;
}
.up-card.form-card-centered .up-card-title,
.up-card.form-card-centered .up-card-subtitle {
  box-sizing: border-box;
}
.up-card.form-card-centered .up-card-title {
  margin-bottom: 10px;
}
.up-card.form-card-centered .step-nav {
  justify-content: flex-start;
}
.up-card-title {
  font-size: 1.18rem;
  font-weight: 750;
  color: ", up_maroon, ";
  margin-bottom: 6px;
}
.up-card-subtitle {
  color: #666;
  font-size: 0.96rem;
  line-height: 1.5;
  margin-bottom: 16px;
}
.summary-grid {
  display: grid;
  grid-template-columns: repeat(3, minmax(0, 1fr));
  gap: 14px;
}
.summary-box {
  border-radius: 16px;
  padding: 18px 18px 16px 18px;
  color: #ffffff;
  box-shadow: 0 10px 24px rgba(42, 32, 24, 0.12);
}
.summary-box-title {
  font-size: 0.95rem;
  font-weight: 650;
  margin-bottom: 8px;
}
.summary-box-value {
  font-size: 1.55rem;
  font-weight: 800;
  line-height: 1.15;
}
.summary-forest { background: #228B22; }
.summary-gold { background: #D4A017; color: #2B2B2B; }
.summary-maroon { background: #7B1113; }
.required-star {
  color: ", up_gold, ";
  margin-left: 4px;
}
.shiny-input-container {
  width: 100% !important;
  max-width: 100% !important;
}
.form-label, .control-label {
  font-weight: 650;
  color: #3b2b2c;
  margin-bottom: 6px;
}
.form-control, .selectize-input, .selectize-control.single .selectize-input {
  border-radius: 10px !important;
  border: 1px solid rgba(123,17,19,0.18) !important;
  box-shadow: none !important;
  min-height: 44px;
}
.form-control:focus, .selectize-input.focus {
  border-color: ", up_maroon, " !important;
  box-shadow: 0 0 0 0.18rem rgba(123,17,19,0.12) !important;
}
.checkbox, .radio {
  margin-top: 0;
  margin-bottom: 10px;
}
.checkbox label, .radio label {
  line-height: 1.45;
}
.service-checklist {
  width: 100%;
  box-sizing: border-box;
  border: 1px solid rgba(123,17,19,0.12);
  border-radius: 12px;
  padding: 12px 14px 4px 14px;
  background: #fffdfb;
  max-height: 280px;
  overflow-y: auto;
}
.service-checklist .shiny-input-container {
  margin-bottom: 0 !important;
}
.loading-panel {
  position: relative;
  min-height: 140px;
}
.loading-panel > .recalculating {
  opacity: 0.35;
}
.loading-panel > .recalculating::before {
  content: '';
  position: absolute;
  top: 50%;
  left: 50%;
  width: 56px;
  height: 56px;
  margin-left: -28px;
  margin-top: -28px;
  border-radius: 50%;
  border: 6px solid rgba(123,17,19,0.14);
  border-top-color: ", up_maroon, ";
  animation: up-spin 0.9s linear infinite;
  z-index: 20;
  opacity: 1;
}
.loading-panel > .recalculating::after {
  content: 'Loading results...';
  position: absolute;
  top: calc(50% + 38px);
  left: 50%;
  transform: translateX(-50%);
  font-weight: 600;
  color: ", up_maroon, ";
  background: rgba(255,255,255,0.92);
  padding: 4px 10px;
  border-radius: 999px;
  z-index: 21;
  white-space: nowrap;
}
@keyframes up-spin {
  from { transform: rotate(0deg); }
  to { transform: rotate(360deg); }
}
.remarks-stack {
  display: flex;
  flex-direction: column;
  gap: 12px;
}
.remarks-textbox {
  width: 100%;
}
.remarks-textarea {
  width: 100%;
  min-height: 88px;
  resize: vertical;
  border-radius: 12px;
  border: 1px solid rgba(123,17,19,0.14);
  background: #fffdfb;
  color: #3b2b2c;
  padding: 12px 14px;
  line-height: 1.5;
  box-sizing: border-box;
}
.service-sqd-card {
  width: 100%;
  box-sizing: border-box;
  border: 1px solid rgba(123,17,19,0.10);
  border-radius: 14px;
  margin-bottom: 18px;
  overflow: hidden;
  background: #fff;
}
.service-sqd-header {
  background: linear-gradient(90deg, rgba(123,17,19,0.08) 0%, rgba(242,169,0,0.10) 100%);
  color: ", up_maroon, ";
  font-weight: 750;
  padding: 14px 16px;
  border-bottom: 1px solid rgba(123,17,19,0.10);
}
.sqd-matrix-wrap {
  width: 100%;
  overflow-x: auto;
}
.sqd-matrix-table {
  width: 100%;
  border-collapse: collapse;
  table-layout: fixed;
}
.sqd-matrix-table th, .sqd-matrix-table td {
  border: 1px solid #ece6de;
  padding: 10px 8px;
  text-align: center;
  vertical-align: middle;
  background: #fff;
}
.sqd-matrix-table thead th {
  background: #f8f3eb;
  color: #4b3536;
  font-weight: 700;
  font-size: 0.90rem;
}
.sqd-col-question {
  text-align: left !important;
  width: 28%;
  min-width: 220px;
}
.sqd-col-choice {
  width: 12%;
  font-size: 0.84rem;
  line-height: 1.25;
}
.sqd-row-label {
  text-align: left !important;
  font-weight: 600;
  background: #fffdfa;
  color: #3e3131;
}
.sqd-radio {
  transform: scale(1.15);
  cursor: pointer;
  accent-color: ", up_maroon, ";
}
.privacy-box {
  background: linear-gradient(180deg, rgba(123,17,19,0.03) 0%, rgba(242,169,0,0.06) 100%);
  border: 1px solid rgba(123,17,19,0.10);
  border-radius: 12px;
  padding: 16px 18px;
  line-height: 1.6;
  color: #4c4545;
}
.step-nav {
  display: flex;
  gap: 12px;
  flex-wrap: wrap;
  align-items: center;
}
.btn {
  border-radius: 10px;
  font-weight: 650;
  min-height: 42px;
  padding-left: 18px;
  padding-right: 18px;
}
.btn-primary {
  background-color: ", up_maroon, ";
  border-color: ", up_maroon, ";
}
.btn-primary:hover, .btn-primary:focus {
  background-color: #5f0d10;
  border-color: #5f0d10;
}
.btn-outline-secondary {
  color: ", up_maroon, ";
  border-color: rgba(123,17,19,0.28);
}
.btn-outline-secondary:hover {
  background-color: rgba(123,17,19,0.06);
  color: ", up_maroon, ";
  border-color: rgba(123,17,19,0.32);
}
.btn-outline-dark {
  color: #5a4a34;
  border-color: rgba(242,169,0,0.55);
  background: rgba(242,169,0,0.10);
}
.btn-outline-dark:hover {
  background: rgba(242,169,0,0.18);
  color: #4b3b28;
  border-color: rgba(242,169,0,0.70);
}
.note-chip {
  display: inline-block;
  font-size: 0.84rem;
  font-weight: 700;
  color: ", up_maroon, ";
  background: rgba(242,169,0,0.16);
  border: 1px solid rgba(242,169,0,0.30);
  border-radius: 999px;
  padding: 6px 10px;
  margin-bottom: 12px;
}
.progress-wrap {
  width: 100%;
  box-sizing: border-box;
  background: #ffffff;
  border: 1px solid rgba(123,17,19,0.08);
  border-radius: 16px;
  padding: 16px 20px;
  margin-bottom: 18px;
  box-shadow: 0 10px 30px rgba(42, 32, 24, 0.05);
}
.progress-label {
  font-size: 0.92rem;
  font-weight: 700;
  color: ", up_maroon, ";
  margin-bottom: 10px;
}
.progress {
  height: 10px;
  border-radius: 999px;
  background-color: #efe8df;
}
.progress-bar {
  background: linear-gradient(90deg, ", up_maroon, " 0%, ", up_gold, " 100%);
}
.step-caption {
  color: #6a6161;
  font-size: 0.9rem;
  margin-top: 8px;
}
.nav-tabs {
  display: none !important;
}

.csm-sheet-intro {
  display: flex;
  justify-content: space-between;
  align-items: flex-start;
  gap: 16px;
  padding: 14px 16px;
  margin-bottom: 12px;
  border: 1px solid #d9d0c3;
  border-radius: 12px;
  background: linear-gradient(180deg, #fffdf8 0%, #fbf6ee 100%);
}
.csm-sheet-intro-title {
  font-size: 1rem;
  font-weight: 800;
  color: #4b3536;
  margin-bottom: 4px;
}
.csm-sheet-intro-text {
  color: #6b5a4a;
  font-size: 0.92rem;
  line-height: 1.45;
}
.csm-sheet-chip {
  display: inline-block;
  white-space: nowrap;
  padding: 7px 11px;
  border-radius: 999px;
  background: rgba(123,17,19,0.08);
  border: 1px solid rgba(123,17,19,0.16);
  color: #7B1113;
  font-weight: 750;
  font-size: 0.84rem;
}
.settings-table-card {
  border: 1px solid #cfc6b8;
  border-radius: 10px;
  overflow: auto;
  background: #ffffff;
  box-shadow: inset 0 1px 0 #ffffff, 0 10px 24px rgba(42, 32, 24, 0.05);
  max-height: 68vh;
}
.settings-table-card table.csm-spreadsheet-table {
  width: 100%;
  min-width: 1320px;
  border-collapse: separate;
  border-spacing: 0;
  table-layout: fixed;
  font-size: 0.92rem;
}
.settings-table-card table.csm-spreadsheet-table th,
.settings-table-card table.csm-spreadsheet-table td {
  border-right: 1px solid #d9d0c3;
  border-bottom: 1px solid #d9d0c3;
  padding: 0;
  vertical-align: middle;
  background: #ffffff;
}
.settings-table-card table.csm-spreadsheet-table thead th {
  position: sticky;
  top: 0;
  z-index: 4;
  background: #efe8df;
  color: #3d3131;
  font-weight: 800;
  text-align: center;
  padding: 10px 8px;
  white-space: nowrap;
  box-shadow: inset 0 -1px 0 #cfc6b8;
}
.settings-table-card table.csm-spreadsheet-table tbody tr:nth-child(even) td {
  background: #fffaf4;
}
.settings-table-card table.csm-spreadsheet-table tbody tr:hover td {
  background: #f7efe5;
}
.csm-row-number,
.csm-row-number-header {
  width: 56px;
  min-width: 56px;
  text-align: center !important;
  color: #6b5a4a;
  font-weight: 700;
  background: #f3eee7 !important;
}
.csm-sticky-office,
.csm-sticky-office-header {
  position: sticky;
  left: 0;
  z-index: 3;
  box-shadow: 1px 0 0 #d9d0c3;
}
.csm-sticky-office-header {
  z-index: 5 !important;
}
.csm-cell,
.csm-cell-readonly {
  min-height: 52px;
  padding: 9px 10px;
}
.csm-cell-readonly {
  display: flex;
  align-items: center;
}
.csm-cell-center {
  justify-content: center;
  text-align: center;
}
.csm-service-cell {
  line-height: 1.35;
}
.settings-table-card .form-group,
.settings-table-card .shiny-input-container {
  margin: 0 !important;
  width: 100% !important;
}
.settings-table-card .form-control,
.settings-table-card .selectize-input,
.settings-table-card .selectize-control.single .selectize-input {
  min-height: 38px;
  border: 0 !important;
  border-radius: 0 !important;
  background: transparent !important;
  box-shadow: none !important;
}
.settings-table-card input[type='number'] {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
.settings-static-pill {
  display: inline-block;
  width: 100%;
  text-align: right;
  padding: 4px 2px;
  border-radius: 0;
  background: transparent;
  border: 0;
  font-weight: 700;
  font-variant-numeric: tabular-nums;
}
.csm-sheet-actions {
  display:flex;
  justify-content:flex-end;
  align-items:center;
  gap:10px;
  margin-top: 12px;
  padding-top: 12px;
  border-top: 1px solid #eadfce;
}
@media (max-width: 768px) {
  .csm-sheet-intro {
    flex-direction: column;
  }
  .settings-table-card {
    max-height: 70vh;
  }
}

@media (max-width: 1200px) {
  .main-wrap {
    width: calc(100vw - 32px);
    min-width: 0;
    max-width: none;
    padding-left: 0;
    padding-right: 0;
  }
}
@media (max-width: 768px) {
  .main-wrap {
    width: calc(100vw - 16px);
  }
  .summary-grid {
    grid-template-columns: 1fr;
  }
  .up-header {
    padding: 22px 18px 20px 18px;
  }
  .up-card {
    padding: 20px 16px;
  }
  .up-card.form-card-centered {
    width: 100%;
    min-width: 0;
    max-width: none;
  }
  .form-band-center,
  .form-input-center,
  .up-card.form-card-centered .up-card-title,
  .up-card.form-card-centered .step-nav {
    width: 100%;
    margin-left: 0;
    margin-right: 0;
  }
  .progress-wrap {
    padding: 14px 16px;
  }
  .sqd-matrix-table {
    min-width: 980px;
  }
}
"
)

js_text <- paste0(
  "
const DEFAULT_SQD_VALUE = '", default_sqd_value, "';

Shiny.addCustomMessageHandler('resetSQDMatrix', function(message) {
  if (message.ids) {
    message.ids.forEach(function(id) {
      const radios = document.querySelectorAll('input[name=\"' + id + '\"]');
      radios.forEach(function(r) { r.checked = false; });
      if (window.Shiny && Shiny.setInputValue) {
        Shiny.setInputValue(id, null, {priority: 'event'});
      }
    });
  }
});

Shiny.addCustomMessageHandler('initializeSQDMatrixValue', function(message) {
  if (message.ids && message.value) {
    message.ids.forEach(function(id) {
      const alreadyChecked = document.querySelector('input[name=\"' + id + '\"]:checked');
      if (!alreadyChecked) {
        const target = document.querySelector('input[name=\"' + id + '\"][value=\"' + message.value + '\"]');
        if (target) {
          target.checked = true;
          if (window.Shiny && Shiny.setInputValue) {
            Shiny.setInputValue(id, message.value, {priority: 'event'});
          }
        }
      }
    });
  }
});

document.addEventListener('change', function(e) {
  if (e.target && e.target.classList.contains('sqd-radio')) {
    const inputName = e.target.name;
    const inputValue = e.target.value;
    const serviceGroup = e.target.dataset.serviceGroup;
    const sqdIndex = e.target.dataset.sqdIndex;

    if (window.Shiny && Shiny.setInputValue) {
      Shiny.setInputValue(inputName, inputValue, {priority: 'event'});
    }

    if (sqdIndex === '0' && serviceGroup) {
      const groupRadios = document.querySelectorAll('input.sqd-radio[data-service-group=\"' + serviceGroup + '\"][value=\"' + inputValue + '\"]');
      groupRadios.forEach(function(radio) {
        radio.checked = true;
        if (window.Shiny && Shiny.setInputValue) {
          Shiny.setInputValue(radio.name, inputValue, {priority: 'event'});
        }
      });
    }
  }
});

.rating-dot {
  display: inline-block;
  width: 12px;
  height: 12px;
  border-radius: 50%;
  margin-right: 8px;
  vertical-align: middle;
  border: 1px solid rgba(43, 43, 43, 0.18);
  box-shadow: 0 0 0 2px rgba(255,255,255,0.92);
}
.dashboard-table-wrap {
  border: 1px solid rgba(123,17,19,0.14);
  border-radius: 16px;
  overflow: hidden;
  background: #ffffff;
  box-shadow: 0 10px 24px rgba(42, 32, 24, 0.05);
}
.dashboard-table {
  width: 100%;
  border-collapse: collapse;
  background: #ffffff;
}
.dashboard-table th, .dashboard-table td {
  border: 1px solid #ece6de;
  padding: 12px 14px;
  vertical-align: middle;
}
.dashboard-table thead th {
  background: #f8f3eb;
  color: #4b3536;
  font-weight: 700;
}
.dashboard-table tbody tr:nth-child(even) {
  background: #fcfaf7;
}
.dashboard-table tbody tr:hover {
  background: #f6efe6;
}
.score-pill {
  display: inline-block;
  min-width: 74px;
  text-align: center;
  padding: 4px 10px;
  border-radius: 999px;
  background: #f7f2eb;
  border: 1px solid #eadfce;
  font-weight: 600;
}
.rating-text {
  font-weight: 600;
}
.dashboard-datatable table.dataTable,
.dashboard-datatable .dataTables_wrapper,
.dashboard-datatable .dt-container {
  border: 1px solid #d9d0c3 !important;
}
.dashboard-datatable table.dataTable thead th,
.dashboard-datatable table.dataTable tbody td {
  border-color: #e7ded2 !important;
}
.settings-table-card {
  border: 1px solid rgba(123,17,19,0.14);
  border-radius: 16px;
  overflow: hidden;
  background: #ffffff;
  box-shadow: 0 10px 24px rgba(42, 32, 24, 0.05);
}
.settings-table-card table {
  width: 100%;
  border-collapse: collapse;
}
.settings-table-card th,
.settings-table-card td {
  border: 1px solid #ece6de;
  padding: 10px 12px;
  vertical-align: top;
}
.settings-table-card thead th {
  background: #f8f3eb;
  color: #4b3536;
  font-weight: 700;
}
.settings-table-card tbody tr:nth-child(even) {
  background: #fcfaf7;
}
.settings-table-card .form-group {
  margin-bottom: 0;
}
.settings-table-card .shiny-input-container {
  width: 100%;
}
.settings-service-meta {
  color: #7b6b5c;
  font-size: 0.88rem;
  margin-top: 4px;
}
.settings-office {
  font-weight: 600;
  color: #4b3536;
}
.settings-service {
  color: #2f2a24;
}
.settings-type-badge {
  display: inline-block;
  padding: 4px 8px;
  border-radius: 999px;
  font-size: 0.82rem;
  font-weight: 700;
  line-height: 1.1;
  border: 1px solid transparent;
}
.settings-type-badge.internal {
  background: #eef4ff;
  color: #284b8f;
  border-color: #d6e2fb;
}
.settings-type-badge.external {
  background: #edf9f1;
  color: #1f6a3a;
  border-color: #cfead9;
}
.settings-static-pill {
  display: inline-block;
  min-width: 64px;
  text-align: center;
  padding: 4px 10px;
  border-radius: 999px;
  background: #f7f2eb;
  border: 1px solid #eadfce;
  font-weight: 600;
}

/* CSM Coverage spreadsheet refinements */
.settings-table-card {
  overflow: auto !important;
  max-height: 68vh !important;
  background: #ffffff !important;
}
.settings-table-card table.csm-spreadsheet-table {
  min-width: 1060px !important;
  border-collapse: separate !important;
  border-spacing: 0 !important;
}
.settings-table-card table.csm-spreadsheet-table th,
.settings-table-card table.csm-spreadsheet-table td {
  padding: 0 !important;
}
.csm-sticky-type,
.csm-sticky-type-header {
  position: sticky !important;
  left: 56px;
  z-index: 6;
  box-shadow: 1px 0 0 #d9d0c3;
}
.csm-sticky-service,
.csm-sticky-service-header {
  position: sticky !important;
  left: 176px;
  z-index: 6;
  box-shadow: 1px 0 0 #d9d0c3;
}
.csm-sticky-type-header,
.csm-sticky-service-header {
  z-index: 8 !important;
  background: #efe8df !important;
}
.csm-sticky-type,
.csm-sticky-service {
  background: inherit !important;
}
.csm-remarks-cell .selectize-input,
.csm-remarks-cell .selectize-control.multi .selectize-input,
.csm-remarks-cell .selectize-control.single .selectize-input {
  background: #ffffff !important;
  border: 1px solid #d9d0c3 !important;
  border-radius: 6px !important;
  box-shadow: none !important;
  min-height: 38px !important;
}
.csm-remarks-cell .selectize-dropdown,
.csm-remarks-cell .selectize-dropdown-content,
.csm-remarks-cell .selectize-dropdown .option {
  background: #ffffff !important;
  color: #2B2B2B !important;
}
.csm-remarks-cell .selectize-dropdown {
  border: 1px solid #cfc6b8 !important;
  box-shadow: 0 10px 24px rgba(42, 32, 24, 0.18) !important;
  z-index: 9999 !important;
}
.csm-remarks-cell .selectize-dropdown .active {
  background: #f8f3eb !important;
  color: #2B2B2B !important;
}

.csm-remarks-modal,
.csm-remarks-modal .form-control,
.csm-remarks-modal .checkbox,
.csm-remarks-modal .checkbox label,
.csm-remarks-modal .shiny-input-container {
  background: #ffffff !important;
  color: #2B2B2B !important;
}
.csm-remarks-modal .form-control {
  border: 1px solid #cfc6b8 !important;
}
.csm-remarks-modal .checkbox input[type='checkbox'] {
  cursor: pointer;
}
.modal-footer .btn-primary {
  background-color: #7B1113 !important;
  border-color: #7B1113 !important;
  color: #ffffff !important;
}

.admin-login-note {
  color: #6b5a4a;
  margin-bottom: 8px;
}
"
)

ui <- page_fluid(
  theme = app_theme,
  tags$head(
    tags$style(HTML(css_text)),
    tags$script(HTML(js_text))
  ),
  
  div(
    class = "main-wrap",
    
    div(class = "up-header-band"),
    
    tabsetPanel(
      id = "main_nav",
      type = "tabs",
      
      tabPanel(
        title = "Survey Form",
        div(
          class = "up-header",
          div(class = "up-title", 
              "Client Satisfaction Measurement Survey")
        ),
        uiOutput("progress_ui"),
        
        tabsetPanel(
          id = "wizard",
          type = "hidden",
          selected = "step1",
          
          tabPanel(
            title = "Step 1",
            value = "step1",
            make_form_card(
              title = tagList("Client Information"),
              card_class = "form-card-centered",
              div(
                class = "form-input-center",
                textInput("name", "Name", placeholder = "Enter full name"),
                textInput("email", "Email", placeholder = "Enter email address"),
                selectInput("age_group", "Age", choices = profile_age, selected = "Prefer not to disclose"),
                selectInput("sex", "Sex Assigned at Birth", choices = profile_sex, selected = "Prefer not to disclose"),
                selectInput("region", "Region", choices = profile_region, selected = "Prefer not to disclose"),
                selectInput("client_type", "Client Type", choices = profile_business, selected = "Prefer not to disclose")
              ),
              div(class = "step-nav", actionButton("next_1", "Next", class = "btn-primary"))
            )
          ),
          
          tabPanel(
            title = "Step 2",
            value = "step2",
            make_form_card(
              title = tagList("Citizen's Charter (CC)", span("*", class = "required-star")),
              card_class = "form-card-centered",
              div(
                class = "form-input-center",
                selectInput("cc1", "CC1. Awareness of CC", choices = cc1_choices, selected = cc1_choices[1]),
                conditionalPanel(
                  condition = "input.cc1 !== '4. I do not know what a CC is and I did not see this office\'s CC.' && input.cc1 !== ''",
                  selectInput("cc2", "CC2. If aware of CC, would you say that the CC of this office was...?", choices = cc2_choices, selected = cc2_choices[1]),
                  selectInput("cc3", "CC3. If aware of CC, how much did the CC help you in your transaction?", choices = cc3_choices, selected = cc3_choices[1])
                )
              ),
              div(
                class = "step-nav",
                actionButton("back_2", "Back", class = "btn-outline-secondary"),
                actionButton("next_2", "Next", class = "btn-primary")
              )
            )
          ),
          
          tabPanel(
            title = "Step 3",
            value = "step3",
            make_form_card(
              title = tagList("Transaction Details", span("*", class = "required-star")),
              card_class = "form-card-centered",
              div(
                class = "form-input-center",
                dateInput("transaction_date", "Date of Transaction", value = Sys.Date(), format = "yyyy-mm-dd", autoclose = TRUE),
                selectInput("office_name", "Office", choices = c("Select office" = "", get_office_choices(services_df)), selected = ""),
                uiOutput("type_ui"),
                uiOutput("service_ui")
              ),
              div(
                class = "step-nav",
                actionButton("back_3", "Back", class = "btn-outline-secondary"),
                actionButton("next_3", "Next", class = "btn-primary")
              )
            )
          ),
          
          tabPanel(
            title = "Step 4",
            value = "step4",
            make_form_card(
              title = tagList("Service Quality Dimensions (SQD)", span("*", class = "required-star")),
              uiOutput("sqd_service_ui"),
              tags$hr(),
              textAreaInput(
                "remarks",
                "Suggestions on how we can improve our services (Optional)",
                placeholder = "Enter your suggestions, comments, or recommendations...",
                width = "100%",
                rows = 4
              ),
              div(
                class = "step-nav",
                actionButton("back_4", "Back", class = "btn-outline-secondary"),
                actionButton("next_4", "Next", class = "btn-primary")
              )
            )
          ),
          
          tabPanel(
            title = "Step 5",
            value = "step5",
            tagList(
              make_form_card(
                title = "Confirmation",
                HTML("By submitting this form, I confirm that the information I provided is correct and I consent to the collection and use of my responses for UP Cebu client satisfaction monitoring, reporting, and service improvement."),
                div(
                  class = "step-nav",
                  actionButton("back_5", "Back", class = "btn-outline-secondary"),
                  actionButton("submit", "Submit", class = "btn-primary")
                )
              )
            )
          )
        )
      ),
      
      tabPanel(
        title = "Admin Panel",
        uiOutput("admin_panel_ui")
      )
    )
  )
)

server <- function(input, output, session) {
  
  privacy_agreed <- reactiveVal(FALSE)
  
  show_privacy_modal <- function() {
    showModal(
      modalDialog(
        title = "Data Privacy Notice",
        div(
          class = "privacy-box",
          HTML(
            "
  <p>
  Dear Client, 
  </p>
  
  <p>
  We value your feedback. This Client Satisfaction Measurement (CSM) survey helps improve government services based on your recent transaction. Any personal information you provide will be kept confidential and used only for client satisfaction monitoring, reporting, and service improvement, in accordance with the Data Privacy Act of 2012.
  </p>

  <p>
  By clicking Agree, you confirm that you have read and understood this notice and voluntarily consent to the collection and processing of your information for these purposes. Participation is voluntary, and you may choose not to answer any question.  
  </p>
  "
          )
        ),
        easyClose = FALSE,
        footer = tagList(
          actionButton("privacy_agree", "Agree", class = "btn-primary")
        )
      )
    )
  }
  
  observe({
    if (!isTRUE(privacy_agreed())) {
      show_privacy_modal()
    }
  })
  
  observeEvent(input$privacy_agree, {
    privacy_agreed(TRUE)
    removeModal()
    goto_step(1)
  })
  
  
  current_step_num <- reactiveVal(1)
  step_values <- c("step1", "step2", "step3", "step4", "step5")
  admin_unlocked <- reactiveVal(FALSE)
  admin_scope <- reactiveVal("all")
  settings_saved <- reactiveVal(NULL)
  remarks_selected_state <- reactiveValues()
  remarks_other_state <- reactiveValues()
  active_remarks_key <- reactiveVal(NULL)
  active_remarks_row <- reactiveVal(NULL)
  remarks_refresh <- reactiveVal(0)
  
  normalize_key <- function(x) {
    x <- tolower(trimws(as.character(x %||% "")))
    gsub("[^a-z0-9]+", "_", x)
  }
  
  resolve_admin_scope <- function(username_value, password_value) {
    username_value <- trimws(as.character(username_value %||% ""))
    password_value <- trimws(as.character(password_value %||% ""))
    
    if (!nzchar(username_value) || !nzchar(password_value)) {
      return(list(valid = FALSE, scope = NULL, label = NULL))
    }
    
    matched_user <- admin_users_df %>%
      filter(username == username_value, password == password_value) %>%
      slice(1)
    
    if (nrow(matched_user) == 0) {
      return(list(valid = FALSE, scope = NULL, label = NULL))
    }
    
    office_code_value <- normalize_key(matched_user$office_code[[1]])
    
    if (!nzchar(office_code_value)) {
      return(list(valid = FALSE, scope = NULL, label = NULL))
    }
    
    if (office_code_value == "all") {
      return(list(valid = TRUE, scope = "all", label = "All Offices"))
    }
    
    office_lookup <- services_df %>%
      distinct(office, office_name, office_display_name) %>%
      mutate(
        office_key = normalize_key(office),
        office_name_key = normalize_key(office_name),
        office_display_key = normalize_key(office_display_name)
      )
    
    matched_office <- office_lookup %>%
      filter(
        office_key == office_code_value |
          office_name_key == office_code_value |
          office_display_key == office_code_value
      ) %>%
      slice(1)
    
    if (nrow(matched_office) == 0) {
      return(list(valid = FALSE, scope = NULL, label = NULL))
    }
    
    list(
      valid = TRUE,
      scope = matched_office$office[[1]],
      label = matched_office$office_display_name[[1]]
    )
  }
  
  goto_step <- function(n) {
    n <- max(1, min(5, n))
    current_step_num(n)
    updateTabsetPanel(session, "wizard", selected = step_values[n])
  }
  
  selected_services <- reactive({
    services <- input$service_name %||% character(0)
    
    if ("Other/s" %in% services) {
      other_text <- trimws(input$service_other %||% "")
      services <- setdiff(services, "Other/s")
      
      if (nzchar(other_text)) {
        other_split <- unlist(strsplit(other_text, ";|,"))
        other_split <- trimws(other_split)
        other_split <- other_split[nzchar(other_split)]
        services <- c(services, other_split)
      }
    }
    
    unique(services)
  })
  
  observeEvent(input$admin_login, {
    auth <- resolve_admin_scope(input$admin_username, input$admin_password)
    
    if (!isTRUE(auth$valid)) {
      admin_unlocked(FALSE)
      admin_scope("all")
      showNotification("Invalid username or password.", type = "error")
      return()
    }
    
    admin_unlocked(TRUE)
    admin_scope(auth$scope)
    
    office_map <- get_office_choices(services_df)
    
    if (identical(auth$scope, "all")) {
      updateSelectInput(
        session,
        "dashboard_office",
        choices = c("All Offices" = "All", office_map),
        selected = "All"
      )
    } else {
      allowed_choice <- office_map[unname(office_map) == auth$scope]
      
      if (length(allowed_choice) == 0) {
        allowed_choice <- stats::setNames(auth$scope, auth$scope)
      }
      
      updateSelectInput(
        session,
        "dashboard_office",
        choices = allowed_choice,
        selected = auth$scope
      )
    }
    
    updateSelectInput(session, "remarks_service", selected = "All")
    showNotification(
      paste0("Admin access granted: ", auth$label),
      type = "message"
    )
  })
  
  observeEvent(input$admin_logout, {
    admin_unlocked(FALSE)
    admin_scope("all")
    updateTextInput(session, "admin_username", value = "")
    updateTextInput(session, "admin_password", value = "")
    showNotification("Admin session closed.", type = "message")
  })
  
  output$admin_panel_ui <- renderUI({
    if (!isTRUE(admin_unlocked())) {
      return(
        make_form_card(
          title = "Sign in to Access the Admin Panel",
          card_class = "form-card-centered",
          div(
            class = "form-input-center",
            div(class = "admin-login-note"),
            textInput("admin_username", "Username", placeholder = "Enter username"),
            passwordInput("admin_password", "Password", placeholder = "Enter password")
          ),
          div(
            class = "step-nav",
            actionButton("admin_login", "Sign in", class = "btn-primary")
          )
        )
      )
    }
    
    tagList(
      make_form_card(
        layout_columns(
          col_widths = c(4, 4, 4),
          uiOutput("dashboard_office_ui"),
          uiOutput("dashboard_year_ui"),
          selectInput(
            "dashboard_service_scope",
            "Service Scope",
            choices = c(
              "CC-listed services" = "cc_listed_only",
              "CC-listed  + Other services" = "all_services"
            ),
            selected = "cc_listed_only"
          )
        )
      ),
      tabsetPanel(
        id = "dashboard_tabs",
        type = "tabs",
        tabPanel(
          title = "CSM Summary",
          make_form_card(
            div(
              class = "loading-panel",
              uiOutput("csm_dashboard_ui")
            )
          )
        ),
        tabPanel(
          title = "Demographics",
          make_form_card(
            tabsetPanel(
              type = "tabs",
              tabPanel(
                "Age",
                div(class = "loading-panel", plotly::plotlyOutput("demographics_age_plot", height = "420px"))
              ),
              tabPanel(
                "Sex",
                div(class = "loading-panel", plotly::plotlyOutput("demographics_sex_plot", height = "420px"))
              ),
              tabPanel(
                "Region",
                div(class = "loading-panel", plotly::plotlyOutput("demographics_region_plot", height = "520px"))
              ),
              tabPanel(
                "Client Type",
                div(class = "loading-panel", plotly::plotlyOutput("demographics_client_type_plot", height = "420px"))
              )
            )
          )
        ),
        tabPanel(
          title = "SQD Scores",
          make_form_card(
            div(
              class = "loading-panel",
              DT::DTOutput("sqd_service_office_tbl")
            )
          )
        ),
        tabPanel(
          title = "Remarks",
          make_form_card(
            uiOutput("remarks_service_ui"),
            div(
              class = "loading-panel",
              uiOutput("remarks_box_ui")
            )
          )
        ),
        tabPanel(
          title = "CSM Coverage",
          make_form_card(
            title = "CSM Coverage Worksheet",
            subtitle = "Update CSM coverage, total transactions, and remarks in a spreadsheet-style grid.",
            div(
              class = "loading-panel",
              uiOutput("settings_table_ui")
            ),
            div(
              class = "csm-sheet-actions",
              actionButton("save_settings", "Save Worksheet", class = "btn-primary")
            ),
            uiOutput("settings_status_ui")
          )
        ),
        tabPanel(
          title = "Insights",
          make_form_card(
            div(
              class = "loading-panel",
              plotly::plotlyOutput("insights_plot", height = "520px")
            )
          )
        )
      ),
      div(
        style = "display:flex; justify-content:flex-end; align-items:center; gap:10px; margin-top: 14px; flex-wrap:wrap;",
        downloadButton("download_admin_excel", "Download Excel", class = "btn-primary"),
        actionButton("admin_logout", "Sign out", class = "btn-outline-secondary")
      )
    )
  })
  
  reset_form <- function() {
    ids <- get_service_sqd_ids(isolate(selected_services()))
    
    updateTextInput(session, "name", value = "")
    updateTextInput(session, "email", value = "")
    updateSelectInput(session, "age_group", selected = "Prefer not to disclose")
    updateSelectInput(session, "sex", selected = "Prefer not to disclose")
    updateSelectInput(session, "region", selected = "Prefer not to disclose")
    updateSelectInput(session, "client_type", selected = "Citizen")
    
    updateSelectInput(session, "cc1", selected = cc1_choices[1])
    updateSelectInput(session, "cc2", selected = cc2_choices[1])
    updateSelectInput(session, "cc3", selected = cc3_choices[1])
    
    updateTextAreaInput(session, "remarks", value = "")
    
    updateDateInput(session, "transaction_date", value = Sys.Date())
    updateSelectInput(session, "office_name", selected = unname(unlist(get_office_choices(services_df)[1])))
    
    if (!is.null(input$service_type)) {
      updateSelectInput(session, "service_type", selected = input$service_type)
    }
    
    updateCheckboxGroupInput(session, "service_name", selected = character(0))
    updateTextInput(session, "service_other", value = "")
    # updateCheckboxInput(session, "consent", value = FALSE)
    
    if (length(ids) > 0) {
      session$sendCustomMessage("resetSQDMatrix", list(ids = ids))
    }
    
    updateTabsetPanel(session, "main_nav", selected = "Survey Form")
    goto_step(1)
  }
  
  validate_step1 <- function() {
    TRUE
  }
  
  validate_step2 <- function() {
    if (is.null(input$cc1) || !nzchar(input$cc1)) {
      showNotification("Please answer CC1.", type = "error")
      return(FALSE)
    }
    if (input$cc1 != cc1_choices[4]) {
      if (is.null(input$cc2) || !nzchar(input$cc2)) {
        showNotification("Please answer CC2.", type = "error")
        return(FALSE)
      }
      if (is.null(input$cc3) || !nzchar(input$cc3)) {
        showNotification("Please answer CC3.", type = "error")
        return(FALSE)
      }
    }
    TRUE
  }
  
  validate_step3 <- function() {
    raw_services <- input$service_name %||% character(0)
    
    if (is.null(input$transaction_date)) {
      showNotification("Please select the date of transaction.", type = "error")
      return(FALSE)
    }
    if (is.null(input$office_name) || !nzchar(input$office_name)) {
      showNotification("Please select an office.", type = "error")
      return(FALSE)
    }
    if (is.null(input$service_type) || !nzchar(input$service_type)) {
      showNotification("Please select a type of service.", type = "error")
      return(FALSE)
    }
    if (length(raw_services) == 0) {
      showNotification("Please select at least one service availed.", type = "error")
      return(FALSE)
    }
    if ("Other/s" %in% raw_services) {
      other_text <- trimws(input$service_other %||% "")
      if (!nzchar(other_text)) {
        showNotification("Please specify the other service availed.", type = "error")
        return(FALSE)
      }
    }
    if (length(selected_services()) == 0) {
      showNotification("Please specify at least one valid service.", type = "error")
      return(FALSE)
    }
    TRUE
  }
  
  validate_step4 <- function() {
    if (length(selected_services()) == 0) {
      showNotification("Please select at least one service first.", type = "error")
      return(FALSE)
    }
    
    for (i in seq_along(selected_services())) {
      for (j in 0:8) {
        val <- input[[service_input_id(i, j)]] %||% default_sqd_value
        if (is.null(val) || !nzchar(val)) {
          showNotification("Please complete all SQD ratings for each selected service.", type = "error")
          return(FALSE)
        }
      }
    }
    TRUE
  }
  
  output$progress_ui <- renderUI({
    step <- current_step_num()
    pct <- step / 5 * 100
    labels <- c(
      "1 of 5 · Client Information",
      "2 of 5 · Citizen's Charter",
      "3 of 5 · Transaction Details",
      "4 of 5 · Service Quality Dimensions",
      "5 of 5 · Consent and Submit"
    )
    
    div(
      class = "progress-wrap",
      div(class = "progress-label", labels[step]),
      div(
        class = "progress",
        div(class = "progress-bar", role = "progressbar", style = paste0("width: ", pct, "%;"))
      )
    )
  })
  
  output$type_ui <- renderUI({
    req(input$office_name)
    validate(need(nzchar(input$office_name), "Please select an office first."))
    
    type_choices <- filter_by_office_selection(services_df, input$office_name) %>%
      distinct(type) %>%
      pull(type) %>%
      sort()
    
    selectInput(
      "service_type",
      "Type of Service",
      choices = c("Select type of service" = "", stats::setNames(type_choices, type_choices)),
      selected = ""
    )
  })
  
  output$service_ui <- renderUI({
    req(input$office_name, input$service_type)
    
    service_choices <- filter_by_office_selection(services_df, input$office_name) %>%
      filter(type == input$service_type) %>%
      distinct(services) %>%
      pull(services) %>%
      sort()
    
    service_choices <- c(service_choices, "Other/s")
    
    tagList(
      div(
        #class = "service-checklist",
        checkboxGroupInput(
          "service_name",
          "Service/s Availed",
          choices = service_choices,
          selected = character(0)
        )
      ),
      uiOutput("service_other_ui")
    )
  })
  
  output$service_other_ui <- renderUI({
    if (!isTRUE("Other/s" %in% (input$service_name %||% character(0)))) {
      return(NULL)
    }
    
    textInput(
      "service_other",
      "Please specify other service/s",
      placeholder = "Enter service not listed; use comma or semicolon for multiple entries"
    )
  })
  
  output$sqd_service_ui <- renderUI({
    if (length(selected_services()) == 0) {
      return(
        div(
          class = "privacy-box",
          "Select at least one service availed in Transaction Details to display the corresponding SQD grids."
        )
      )
    }
    
    tagList(
      lapply(seq_along(selected_services()), function(i) {
        make_service_sqd_matrix(
          service_label = selected_services()[i],
          service_index = i,
          choices = likert_levels,
          default_value = default_sqd_value
        )
      })
    )
  })
  
  observeEvent(selected_services(), {
    ids <- get_service_sqd_ids(selected_services())
    if (length(ids) == 0) return()
    
    session$onFlushed(function() {
      session$sendCustomMessage(
        "initializeSQDMatrixValue",
        list(ids = ids, value = default_sqd_value)
      )
    }, once = TRUE)
  }, ignoreInit = TRUE)
  
  observe({
    req(input$office_name)
    
    type_choices <- filter_by_office_selection(services_df, input$office_name) %>%
      distinct(type) %>%
      pull(type) %>%
      sort()
    
    if (!is.null(input$service_type) &&
        nzchar(input$service_type) &&
        !(input$service_type %in% type_choices)) {
      updateSelectInput(session, "service_type", selected = "")
    }
  })
  
  observeEvent(list(input$office_name, input$service_type), {
    if (!is.null(input$service_name) && length(input$service_name) > 0) {
      updateCheckboxGroupInput(session, "service_name", selected = character(0))
    }
    if (!is.null(input$service_other) && nzchar(input$service_other)) {
      updateTextInput(session, "service_other", value = "")
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$next_1, {
    if (validate_step1()) goto_step(2)
  })
  
  observeEvent(input$next_2, {
    if (validate_step2()) goto_step(3)
  })
  
  observeEvent(input$next_3, {
    if (validate_step3()) goto_step(4)
  })
  
  observeEvent(input$next_4, {
    if (validate_step4()) goto_step(5)
  })
  
  observeEvent(input$back_2, { goto_step(1) })
  observeEvent(input$back_3, { goto_step(2) })
  observeEvent(input$back_4, { goto_step(3) })
  observeEvent(input$back_5, { goto_step(4) })
  
  observeEvent(input$submit, {
    if (!validate_step1()) return()
    if (!validate_step2()) return()
    if (!validate_step3()) return()
    if (!validate_step4()) return()
    
    #  if (!isTRUE(input$consent)) {
    #    showNotification("Please confirm the consent statement before submitting.", type = "error")
    #    return()
    #  }
    
    submission_time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    submission_id_base <- paste0("CSM-", format(Sys.time(), "%Y%m%d%H%M%S"))
    
    selected_rows <- services_df %>%
      filter(
        office == input$office_name,
        type == input$service_type,
        services %in% selected_services()
      )
    
    rows_to_save <- lapply(seq_along(selected_services()), function(i) {
      svc_name <- selected_services()[i]
      
      svc_row <- selected_rows %>%
        filter(services == svc_name) %>%
        slice(1)
      
      tibble(
        submission_id = paste0(submission_id_base, "-", sprintf("%02d", i)),
        submission_group_id = submission_id_base,
        submitted_at = submission_time,
        transaction_date = as.character(input$transaction_date),
        name = trimws(input$name),
        email = trimws(input$email),
        age_group = input$age_group,
        sex = input$sex,
        region = input$region,
        client_type = input$client_type,
        seq_office = svc_row$seq_office %||% NA_character_,
        office_code = svc_row$office %||% NA_character_,
        office_name = get_selected_office_name(services_df, input$office_name),
        office_display_name = get_selected_office_display_name(services_df, input$office_name),
        service_type = input$service_type,
        seq_services = svc_row$seq_services %||% NA_character_,
        service_name = svc_name,
        selected_services_all = paste(selected_services(), collapse = "; "),
        cc1 = input$cc1,
        cc2 = if (input$cc1 == cc1_choices[4]) NA_character_ else input$cc2,
        cc3 = if (input$cc1 == cc1_choices[4]) NA_character_ else input$cc3,
        sqd_0 = input[[service_input_id(i, 0)]] %||% default_sqd_value,
        sqd_1 = input[[service_input_id(i, 1)]] %||% default_sqd_value,
        sqd_2 = input[[service_input_id(i, 2)]] %||% default_sqd_value,
        sqd_3 = input[[service_input_id(i, 3)]] %||% default_sqd_value,
        sqd_4 = input[[service_input_id(i, 4)]] %||% default_sqd_value,
        sqd_5 = input[[service_input_id(i, 5)]] %||% default_sqd_value,
        sqd_6 = input[[service_input_id(i, 6)]] %||% default_sqd_value,
        sqd_7 = input[[service_input_id(i, 7)]] %||% default_sqd_value,
        sqd_8 = input[[service_input_id(i, 8)]] %||% default_sqd_value,
        remarks = trimws(input$remarks)
      )
    })
    
    new_rows <- bind_rows(rows_to_save)
    all_responses <- bind_rows(read_responses(), new_rows)
    write_csv(all_responses, responses_file)
    
    entry_text <- if (nrow(new_rows) == 1) "entry was" else "entries were"
    
    showModal(
      modalDialog(
        title = "Thank you",
        HTML(sprintf(
          "Thank you for submitting your response. <strong>%s</strong> service-specific %s recorded successfully.",
          nrow(new_rows),
          entry_text
        )),
        easyClose = FALSE,
        footer = tagList(
          actionButton("submit_another", "Submit another response", class = "btn-primary"),
          modalButton("Close")
        )
      )
    )
  })
  
  observeEvent(input$submit_another, {
    removeModal()
    reset_form()
  })
  
  output$download_template <- downloadHandler(
    filename = function() {
      "upcebu_csm_blank_template.csv"
    },
    content = function(file) {
      tibble(
        transaction_date = character(),
        name = character(),
        email = character(),
        age_group = character(),
        sex = character(),
        region = character(),
        client_type = character(),
        office_name = character(),
        service_type = character(),
        service_name = character(),
        cc1 = character(),
        cc2 = character(),
        cc3 = character(),
        sqd_0 = character(),
        sqd_1 = character(),
        sqd_2 = character(),
        sqd_3 = character(),
        sqd_4 = character(),
        sqd_5 = character(),
        sqd_6 = character(),
        sqd_7 = character(),
        sqd_8 = character(),
        remarks = character()
      ) %>%
        write_csv(file)
    }
  )
  
  
  blank_to_na <- function(v) {
    v <- as.character(v)
    v[is.na(v) | !nzchar(trimws(v))] <- NA_character_
    v
  }
  
  extract_csm_year <- function(x) {
    x_chr <- trimws(as.character(x %||% NA_character_))
    x_chr[!nzchar(x_chr)] <- NA_character_
    
    # The uploaded upcebu_csm_responses.csv stores transaction_date as
    # M/D/YY, e.g., 1/1/25 and 1/1/26. as.Date() alone returns NA for this.
    y_mdy <- suppressWarnings(lubridate::year(lubridate::mdy(x_chr)))
    y_ymd <- suppressWarnings(lubridate::year(lubridate::ymd(x_chr)))
    y_dmy <- suppressWarnings(lubridate::year(lubridate::dmy(x_chr)))
    y_text4 <- suppressWarnings(as.integer(stringr::str_extract(x_chr, "(?<!\\d)(19|20)\\d{2}(?!\\d)")))
    
    # Final fallback for two-digit years at the end of M/D/YY strings.
    y_text2 <- suppressWarnings(as.integer(stringr::str_match(x_chr, "(?:^|[/.-])(\\d{2})$")[, 2]))
    y_text2 <- dplyr::case_when(
      is.na(y_text2) ~ NA_integer_,
      y_text2 <= 68 ~ 2000L + y_text2,
      TRUE ~ 1900L + y_text2
    )
    
    out <- dplyr::coalesce(y_mdy, y_ymd, y_dmy, y_text4, y_text2)
    out[is.na(out) | out < 1900 | out > 2100] <- NA_integer_
    out
  }
  
  get_response_id_vec <- function(df) {
    n <- nrow(df)
    id_group <- if ("submission_group_id" %in% names(df)) blank_to_na(df$submission_group_id) else rep(NA_character_, n)
    id_submission <- if ("submission_id" %in% names(df)) blank_to_na(df$submission_id) else rep(NA_character_, n)
    id_submitted <- if ("submitted_at" %in% names(df)) blank_to_na(df$submitted_at) else rep(NA_character_, n)
    out <- dplyr::coalesce(id_group, id_submission, id_submitted)
    missing_id <- is.na(out) | !nzchar(trimws(out))
    out[missing_id] <- as.character(seq_len(n))[missing_id]
    out
  }
  
  dashboard_responses <- reactive({
    # Source of truth for all Admin dashboard computations:
    # upcebu_csm_responses.csv
    df <- read_responses()
    
    if (nrow(df) == 0) {
      return(tibble())
    }
    
    n <- nrow(df)
    office_code_vec <- dplyr::coalesce(
      if ("office" %in% names(df)) blank_to_na(df$office) else rep(NA_character_, n),
      if ("office_code" %in% names(df)) blank_to_na(df$office_code) else rep(NA_character_, n)
    )
    office_name_vec <- if ("office_name" %in% names(df)) blank_to_na(df$office_name) else office_code_vec
    office_display_vec <- if ("office_display_name" %in% names(df)) blank_to_na(df$office_display_name) else office_name_vec
    service_type_vec <- if ("service_type" %in% names(df)) blank_to_na(df$service_type) else if ("type" %in% names(df)) blank_to_na(df$type) else rep(NA_character_, n)
    service_name_vec <- if ("service_name" %in% names(df)) blank_to_na(df$service_name) else if ("services" %in% names(df)) blank_to_na(df$services) else rep(NA_character_, n)
    transaction_date_vec <- if ("transaction_date" %in% names(df)) df$transaction_date else rep(NA_character_, n)
    
    df %>%
      mutate(
        office = as.character(office_code_vec),
        office_code = as.character(office_code_vec),
        office_name = as.character(dplyr::coalesce(office_name_vec, office_code_vec)),
        office_display_name = as.character(dplyr::coalesce(office_display_vec, office_name_vec, office_code_vec)),
        service_type = as.character(service_type_vec),
        service_name = as.character(service_name_vec),
        # IMPORTANT: Year filter is based on transaction_date, not submitted_at.
        dashboard_year = extract_csm_year(transaction_date_vec),
        dashboard_response_id = get_response_id_vec(.)
      )
  })
  
  sqd_cols <- paste0("sqd_", 0:8)
  current_dashboard_year <- as.integer(format(Sys.Date(), "%Y"))
  required_dashboard_years <- c(2026L, 2025L)
  
  safe_pct <- function(num, den) {
    if (is.null(den) || length(den) == 0 || is.na(den) || den == 0) {
      return(0)
    }
    round((num * 100) / den, 2)
  }
  
  compute_sqd_score <- function(x) {
    x <- as.character(x)
    x <- x[!is.na(x) & nzchar(x)]
    
    denominator <- sum(x != "Not applicable")
    numerator <- sum(x %in% c("Agree", "Strongly Agree"))
    
    if (denominator == 0) {
      return(0)
    }
    
    round((numerator * 100) / denominator, 2)
  }
  
  rating_info <- function(score) {
    if (is.na(score)) {
      return(list(label = "NA", color = "#bdbdbd"))
    }
    if (score < 60) {
      return(list(label = "Poor", color = "#7B1113"))
    }
    if (score < 80) {
      return(list(label = "Fair", color = "#A64D59"))
    }
    if (score < 90) {
      return(list(label = "Satisfactory", color = "#F2A900"))
    }
    if (score < 95) {
      return(list(label = "Very Satisfactory", color = "#90B77D"))
    }
    list(label = "Outstanding", color = "#228B22")
  }
  
  
  current_settings_table <- reactive({
    if (!isTRUE(admin_unlocked())) {
      return(tibble())
    }
    
    req(input$dashboard_year)
    
    office_value <- input$dashboard_office %||% "All"
    scope_mode <- input$dashboard_service_scope %||% "cc_listed_only"
    settings_df <- read_settings()
    
    if (identical(office_value, "All")) {
      service_scope <- services_df
    } else {
      service_scope <- filter_by_office_selection(services_df, office_value)
    }
    
    service_scope <- service_scope %>%
      distinct(office, office_display_name, office_name, type, service_name = services) %>%
      arrange(office_display_name, type, service_name)
    
    if (identical(scope_mode, "all_services")) {
      other_scope <- dashboard_responses() %>%
        filter(!is.na(dashboard_year), dashboard_year == suppressWarnings(as.integer(input$dashboard_year))) %>%
        { if (identical(office_value, "All")) . else filter(., office == office_value | office_name == office_value | office_display_name == office_value) } %>%
        mutate(
          listed_flag = mapply(
            FUN = is_cc_listed_service,
            service_name = as.character(service_name),
            office_name = as.character(office_name),
            office_display_name = as.character(office_display_name),
            office_code = as.character(office),
            type = as.character(service_type),
            SIMPLIFY = TRUE,
            USE.NAMES = FALSE
          )
        ) %>%
        filter(!listed_flag) %>%
        transmute(
          office = as.character(office),
          office_display_name = as.character(office_display_name),
          office_name = as.character(office_name),
          type = as.character(service_type),
          service_name = as.character(service_name)
        ) %>%
        distinct()
      
      service_scope <- bind_rows(service_scope, other_scope) %>%
        distinct() %>%
        arrange(office_display_name, type, service_name)
    }
    
    if (nrow(service_scope) == 0) {
      return(tibble())
    }
    
    saved_scope <- settings_df %>%
      filter(
        dashboard_year == suppressWarnings(as.integer(input$dashboard_year)),
        office_name %in% unique(service_scope$office_display_name),
        service_type %in% unique(service_scope$type),
        service_name %in% unique(service_scope$service_name)
      ) %>%
      arrange(desc(saved_at)) %>%
      group_by(office_name, service_type, service_name) %>%
      slice(1) %>%
      ungroup() %>%
      rename(
        office_display_name = office_name,
        type = service_type
      )
    
    response_scope <- dashboard_filtered_responses() %>%
      count(office_display_name, service_type, service_name, name = "response_count") %>%
      rename(type = service_type)
    
    service_scope %>%
      left_join(saved_scope, by = c("office_display_name", "type", "service_name")) %>%
      left_join(response_scope, by = c("office_display_name", "type", "service_name")) %>%
      mutate(
        total_transaction = suppressWarnings(as.numeric(total_transaction)),
        total_transaction = dplyr::coalesce(total_transaction, 0),
        response_count = dplyr::coalesce(response_count, 0L),
        csm_coverage = dplyr::case_when(
          response_count > 0 ~ "Yes",
          is.na(csm_coverage) | !nzchar(csm_coverage) ~ "No Client",
          TRUE ~ csm_coverage
        ),
        sample_size = ifelse(
          is.na(total_transaction) | total_transaction <= 0,
          0,
          round(total_transaction * 384.16 / (((total_transaction - 1) + 384.16)), 0)
        ),
        remarks = dplyr::coalesce(remarks, ""),
        saved_at = dplyr::coalesce(saved_at, "")
      )
  })
  
  output$settings_table_ui <- renderUI({
    req(isTRUE(admin_unlocked()))
    settings_tbl <- current_settings_table()
    
    if (nrow(settings_tbl) == 0) {
      return(
        div(
          class = "privacy-box",
          "No listed services found for the selected office and year."
        )
      )
    }
    
    rows_ui <- lapply(seq_len(nrow(settings_tbl)), function(i) {
      parsed_remarks <- split_saved_remarks(settings_tbl$remarks[[i]] %||% "")
      response_n <- suppressWarnings(as.numeric(settings_tbl$response_count[[i]] %||% 0))
      total_txn <- suppressWarnings(as.numeric(settings_tbl$total_transaction[[i]] %||% 0))
      sample_n <- suppressWarnings(as.numeric(settings_tbl$sample_size[[i]] %||% 0))
      coverage_value <- if (!is.na(response_n) && response_n > 0) {
        "Yes"
      } else {
        as.character(settings_tbl$csm_coverage[[i]] %||% "No Client")
      }
      
      tags$tr(
        tags$td(class = "csm-row-number", tags$div(class = "csm-cell-readonly csm-cell-center", i)),
        tags$td(class = "csm-sticky-type",
                tags$div(class = "csm-cell-readonly csm-cell-center",
                         tags$span(
                           class = paste(
                             "settings-type-badge",
                             ifelse(tolower(as.character(settings_tbl$type[[i]] %||% "")) == "external", "external", "internal")
                           ),
                           as.character(settings_tbl$type[[i]] %||% "")
                         )
                )
        ),
        tags$td(class = "csm-sticky-service",
                tags$div(
                  class = "csm-cell-readonly csm-service-cell settings-service",
                  tags$strong(as.character(settings_tbl$service_name[[i]]))
                )
        ),
        tags$td(class = "csm-edit-cell",
                div(class = "csm-cell",
                    selectInput(
                      inputId = settings_input_id("coverage", i),
                      label = NULL,
                      choices = c("Yes", "No Client"),
                      selected = coverage_value,
                      width = "100%"
                    )
                )
        ),
        tags$td(class = "csm-edit-cell",
                div(class = "csm-cell",
                    numericInput(
                      inputId = settings_input_id("transactions", i),
                      label = NULL,
                      value = ifelse(is.na(total_txn), 0, total_txn),
                      min = 0,
                      step = 1,
                      width = "100%"
                    )
                )
        ),
        tags$td(
          div(class = "csm-cell-readonly csm-cell-center", div(class = "settings-static-pill", ifelse(is.na(sample_n), "0", format(round(sample_n), big.mark = ","))))
        ),
        tags$td(
          div(class = "csm-cell-readonly csm-cell-center", div(class = "settings-static-pill", ifelse(is.na(response_n), "0", format(round(response_n), big.mark = ","))))
        ),
        tags$td(class = "csm-edit-cell csm-remarks-cell",
                div(class = "csm-cell",
                    actionButton(
                      inputId = settings_remarks_button_id(i),
                      label = "Select or type reason/s",
                      class = "csm-remarks-picker-btn",
                      onclick = sprintf("Shiny.setInputValue('open_remarks_modal_row', %d, {priority: 'event'});", i)
                    ),
                    div(
                      class = "settings-service-meta",
                      textOutput(settings_other_reason_output_id(i), inline = TRUE)
                    )
                )
        )
      )
    })
    
    tagList(
      div(
        class = "settings-table-card",
        tags$table(
          class = "csm-spreadsheet-table",
          tags$thead(
            tags$tr(
              tags$th(class = "csm-row-number-header", "#"),
              tags$th(class = "csm-sticky-type-header", style = "width:120px;", "Type"),
              tags$th(class = "csm-sticky-service-header", style = "width:330px;", "Service"),
              tags$th(style = "width:150px;", "Coverage"),
              tags$th(style = "width:170px;", "Total Transaction"),
              tags$th(style = "width:180px;", "Minimum Sample Size"),
              tags$th(style = "width:160px;", "No. of Response"),
              tags$th(style = "width:310px;", "Remarks")
            )
          ),
          tags$tbody(rows_ui)
        )
      )
    )
  })
  
  observe({
    req(isTRUE(admin_unlocked()))
    settings_tbl <- current_settings_table()
    if (nrow(settings_tbl) == 0) return()
    
    lapply(seq_len(nrow(settings_tbl)), function(i) {
      local({
        row_i <- i
        row_key <- paste(
          input$dashboard_year %||% "",
          settings_tbl$office_display_name[[row_i]] %||% settings_tbl$office_name[[row_i]] %||% "",
          settings_tbl$type[[row_i]] %||% "",
          settings_tbl$service_name[[row_i]] %||% "",
          sep = "__"
        )
        saved <- split_saved_remarks(settings_tbl$remarks[[row_i]] %||% "")
        
        if (is.null(remarks_selected_state[[row_key]])) {
          remarks_selected_state[[row_key]] <- saved$selected
        }
        if (is.null(remarks_other_state[[row_key]])) {
          remarks_other_state[[row_key]] <- saved$other
        }
        
        output[[settings_other_reason_output_id(row_i)]] <- renderText({
          remarks_refresh()
          selected_vals <- remarks_selected_state[[row_key]] %||% character(0)
          other_val <- trimws(as.character(remarks_other_state[[row_key]] %||% ""))
          display_vals <- selected_vals
          if ("Other" %in% selected_vals && nzchar(other_val)) {
            display_vals <- c(setdiff(display_vals, "Other"), paste0("Other: ", other_val))
          }
          display_vals <- display_vals[nzchar(display_vals)]
          if (length(display_vals) == 0) {
            "No reason selected"
          } else {
            paste(display_vals, collapse = "; ")
          }
        })
        
      })
    })
  })
  
  observeEvent(input$open_remarks_modal_row, {
    row_i <- suppressWarnings(as.integer(input$open_remarks_modal_row))
    req(!is.na(row_i), row_i > 0)
    
    settings_tbl <- current_settings_table()
    req(nrow(settings_tbl) >= row_i)
    
    row_key <- paste(
      input$dashboard_year %||% "",
      settings_tbl$office_display_name[[row_i]] %||% settings_tbl$office_name[[row_i]] %||% "",
      settings_tbl$type[[row_i]] %||% "",
      settings_tbl$service_name[[row_i]] %||% "",
      sep = "__"
    )
    
    saved <- split_saved_remarks(settings_tbl$remarks[[row_i]] %||% "")
    if (is.null(remarks_selected_state[[row_key]])) remarks_selected_state[[row_key]] <- saved$selected
    if (is.null(remarks_other_state[[row_key]])) remarks_other_state[[row_key]] <- saved$other
    
    active_remarks_key(row_key)
    active_remarks_row(row_i)
    
    showModal(
      modalDialog(
        title = settings_tbl$service_name[[row_i]],
        div(
          class = "csm-remarks-modal",
          checkboxGroupInput(
            inputId = "settings_remarks_modal_choices",
            label = "Reasons for Not Meeting the Minimum Sample Size",
            choices = remarks_reason_choices,
            selected = remarks_selected_state[[row_key]] %||% character(0)
          ),
          conditionalPanel(
            condition = "input.settings_remarks_modal_choices && input.settings_remarks_modal_choices.indexOf('Other') !== -1",
            textInput(
              inputId = "settings_remarks_modal_other",
              label = "Please specify other reason",
              value = remarks_other_state[[row_key]] %||% "",
              placeholder = "Type other reason here"
            )
          )
        ),
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton("settings_remarks_apply", "Apply", class = "btn-primary")
        )
      )
    )
  }, ignoreInit = TRUE)
  
  observeEvent(input$settings_remarks_apply, {
    row_i <- suppressWarnings(as.integer(active_remarks_row()))
    row_key <- active_remarks_key()
    req(!is.na(row_i), row_i > 0, !is.null(row_key), nzchar(row_key))
    
    selected_vals <- input$settings_remarks_modal_choices %||% character(0)
    selected_vals <- as.character(selected_vals)
    selected_vals <- unique(selected_vals[nzchar(selected_vals)])
    
    other_val <- trimws(as.character(input$settings_remarks_modal_other %||% ""))
    
    if (!("Other" %in% selected_vals)) {
      other_val <- ""
    } else if (!nzchar(other_val)) {
      showNotification("Please type the other reason or uncheck Other.", type = "error")
      return()
    }
    
    remarks_selected_state[[row_key]] <- selected_vals
    remarks_other_state[[row_key]] <- other_val
    remarks_refresh(isolate(remarks_refresh()) + 1)
    
    removeModal()
    active_remarks_key(NULL)
    active_remarks_row(NULL)
    showNotification("Remarks applied to this row. Click Save to store changes.", type = "message")
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  observeEvent(input$save_settings, {
    req(isTRUE(admin_unlocked()))
    req(input$dashboard_year)
    
    settings_tbl <- current_settings_table()
    req(nrow(settings_tbl) > 0)
    
    rows_to_save <- lapply(seq_len(nrow(settings_tbl)), function(i) {
      response_n <- suppressWarnings(as.numeric(settings_tbl$response_count[[i]] %||% 0))
      row_key <- paste(
        input$dashboard_year %||% "",
        settings_tbl$office_display_name[[i]] %||% settings_tbl$office_name[[i]] %||% "",
        settings_tbl$type[[i]] %||% "",
        settings_tbl$service_name[[i]] %||% "",
        sep = "__"
      )
      saved_remarks <- split_saved_remarks(settings_tbl$remarks[[i]] %||% "")
      selected_reasons <- remarks_selected_state[[row_key]] %||% saved_remarks$selected
      other_reason <- remarks_other_state[[row_key]] %||% saved_remarks$other
      
      tibble(
        office_name = settings_tbl$office_display_name[[i]] %||% settings_tbl$office_name[[i]],
        dashboard_year = suppressWarnings(as.integer(input$dashboard_year)),
        service_type = settings_tbl$type[[i]],
        service_name = settings_tbl$service_name[[i]],
        csm_coverage = if (!is.na(response_n) && response_n > 0) {
          "Yes"
        } else {
          as.character(input[[settings_input_id("coverage", i)]] %||% settings_tbl$csm_coverage[[i]] %||% "No Client")
        },
        total_transaction = suppressWarnings(as.numeric(input[[settings_input_id("transactions", i)]] %||% settings_tbl$total_transaction[[i]] %||% 0)),
        remarks = collapse_remarks_input(selected_reasons, other_reason),
        saved_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      )
    })
    
    new_rows <- bind_rows(rows_to_save)
    
    settings_df <- read_settings() %>%
      anti_join(
        new_rows %>% distinct(office_name, dashboard_year, service_type, service_name),
        by = c("office_name", "dashboard_year", "service_type", "service_name")
      )
    
    write_settings(bind_rows(settings_df, new_rows))
    settings_saved(format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
    showNotification("Settings saved successfully.", type = "message")
  })
  
  output$settings_status_ui <- renderUI({
    settings_tbl <- current_settings_table()
    
    if (nrow(settings_tbl) == 0) {
      return(
        div(
          class = "privacy-box",
          "No saved settings yet for the selected office and year."
        )
      )
    }
    
    last_saved <- settings_tbl$saved_at[settings_tbl$saved_at != ""]
    last_saved <- if (length(last_saved) == 0) "Not available" else max(last_saved)
    
    div(
      HTML(sprintf(
        "<br/>Services listed: %s<br/>Last saved: %s",
        format(nrow(settings_tbl), big.mark = ",", trim = TRUE),
        last_saved
      ))
    )
  })
  
  output$dashboard_office_ui <- renderUI({
    req(admin_unlocked())
    
    current_scope <- admin_scope()
    office_map <- get_office_choices(services_df)
    
    if (identical(current_scope, "all")) {
      selectInput(
        "dashboard_office",
        "Office",
        choices = c("All Offices" = "All", office_map),
        selected = "All"
      )
    } else {
      allowed_choice <- office_map[unname(office_map) == current_scope]
      
      if (length(allowed_choice) == 0) {
        allowed_choice <- stats::setNames(current_scope, current_scope)
      }
      
      selectInput(
        "dashboard_office",
        "Office",
        choices = allowed_choice,
        selected = current_scope
      )
    }
  })
  
  output$dashboard_year_ui <- renderUI({
    if (!isTRUE(admin_unlocked())) {
      return(NULL)
    }
    
    response_years <- dashboard_responses() %>%
      pull(dashboard_year) %>%
      unique()
    response_years <- response_years[!is.na(response_years)]
    
    years <- sort(unique(c(required_dashboard_years, response_years)), decreasing = TRUE)
    if (length(years) == 0) {
      years <- sort(unique(c(required_dashboard_years, current_dashboard_year)), decreasing = TRUE)
    }
    
    current_year <- as.integer(format(Sys.Date(), "%Y"))
    
    default_year <- if (current_year %in% years) {
      current_year
    } else if (length(response_years) > 0) {
      max(response_years, na.rm = TRUE)
    } else {
      years[[1]]
    }
    
    selectInput(
      "dashboard_year",
      "Year",
      choices = stats::setNames(as.character(years), as.character(years)),
      selected = as.character(default_year)
    )
  })
  
  dashboard_filtered_responses <- reactive({
    if (!isTRUE(admin_unlocked())) {
      return(tibble())
    }
    
    df <- dashboard_responses()
    if (nrow(df) == 0) {
      return(tibble())
    }
    
    # 1. Office filter
    if (!is.null(input$dashboard_office) && input$dashboard_office != "All") {
      df <- df %>%
        filter(
          office == input$dashboard_office |
            office_code == input$dashboard_office |
            office_name == input$dashboard_office |
            office_display_name == input$dashboard_office
        )
    }
    
    # 2. Year filter based on transaction_date
    if (!is.null(input$dashboard_year) && nzchar(as.character(input$dashboard_year))) {
      selected_year <- suppressWarnings(as.integer(input$dashboard_year))
      df <- df %>% filter(!is.na(dashboard_year), dashboard_year == selected_year)
    }
    
    # 3. Service Scope filter
    scope_mode <- input$dashboard_service_scope %||% "cc_listed_only"
    df <- filter_service_scope(df, scope_mode = scope_mode)
    
    df
  })
  
  output$remarks_service_ui <- renderUI({
    req(isTRUE(admin_unlocked()))
    
    df <- dashboard_filtered_responses()
    
    service_choices <- df %>%
      mutate(service_name = as.character(service_name)) %>%
      filter(!is.na(service_name), nzchar(trimws(service_name))) %>%
      distinct(service_name) %>%
      arrange(service_name) %>%
      pull(service_name)
    
    selectInput(
      "remarks_service",
      "Service",
      choices = c("All" = "All", stats::setNames(service_choices, service_choices)),
      selected = "All"
    )
  })
  
  remarks_filtered <- reactive({
    req(isTRUE(admin_unlocked()))
    
    df <- dashboard_filtered_responses()
    
    if (nrow(df) == 0) {
      return(tibble())
    }
    
    df <- df %>%
      mutate(
        remarks = if ("remarks" %in% names(.)) as.character(.data$remarks) else "",
        service_name = if ("service_name" %in% names(.)) as.character(.data$service_name) else "",
        submitted_at = if ("submitted_at" %in% names(.)) as.character(.data$submitted_at) else "",
        transaction_date = if ("transaction_date" %in% names(.)) as.character(.data$transaction_date) else ""
      ) %>%
      filter(!is.na(remarks), nzchar(trimws(remarks)))
    
    if (!is.null(input$remarks_service) && nzchar(input$remarks_service) && input$remarks_service != "All") {
      df <- df %>% filter(service_name == input$remarks_service)
    }
    
    df %>%
      arrange(desc(submitted_at), desc(transaction_date))
  })
  
  output$remarks_box_ui <- renderUI({
    Sys.sleep(0.4)
    df <- remarks_filtered()
    
    if (nrow(df) == 0) {
      return(
        div(
          class = "privacy-box",
          "No remarks or suggestions found for the selected filters."
        )
      )
    }
    
    tagList(
      div(
        class = "remarks-stack",
        lapply(seq_len(nrow(df)), function(i) {
          div(
            class = "remarks-textbox",
            tags$textarea(
              class = "remarks-textarea",
              readonly = "readonly",
              rows = max(3, min(8, stringr::str_count(df$remarks[[i]] %||% "", "\n") + 2)),
              df$remarks[[i]] %||% ""
            )
          )
        })
      )
    )
  })
  
  official_dashboard_filtered_responses <- reactive({
    # dashboard_filtered_responses() already applies Office + Year + Service Scope.
    # Do not inner_join again here, because display-name mismatches can drop all rows.
    dashboard_filtered_responses()
  })
  
  service_score_summary <- reactive({
    df <- official_dashboard_filtered_responses()
    
    offered_services_df <- current_settings_table()
    if (nrow(offered_services_df) == 0) {
      offered_services_df <- if (!is.null(input$dashboard_office) && input$dashboard_office != "All") {
        filter_by_office_selection(services_df, input$dashboard_office) %>% distinct(office, office_display_name, type, service_name = services)
      } else {
        services_df %>% distinct(office, office_display_name, type, service_name = services)
      }
    }
    total_services <- offered_services_df %>% distinct(office, office_display_name, type, service_name) %>% nrow()
    
    if (nrow(df) == 0) {
      return(list(outstanding_n = 0L, total_services = total_services))
    }
    
    sqd_cols_present <- intersect(sqd_cols, names(df))
    
    scored_tbl <- if (length(sqd_cols_present) == 0) {
      tibble(service_name = character(), service_type = character(), sqd_score_pct = numeric())
    } else {
      df %>%
        group_by(office, office_display_name, service_type, service_name) %>%
        group_modify(~{
          sqd_values <- unlist(.x[, sqd_cols_present, drop = FALSE], use.names = FALSE)
          sqd_values <- as.character(sqd_values)
          sqd_values <- sqd_values[!is.na(sqd_values) & nzchar(sqd_values)]
          numerator <- sum(sqd_values %in% c("Agree", "Strongly Agree"))
          denominator <- sum(sqd_values != "Not applicable")
          
          tibble(
            sqd_score_pct = if (denominator == 0) NA_real_ else round((numerator * 100) / denominator, 2)
          )
        }) %>%
        ungroup()
    }
    
    outstanding_n <- scored_tbl %>% filter(!is.na(sqd_score_pct), sqd_score_pct >= 95) %>% nrow()
    
    list(outstanding_n = outstanding_n, total_services = total_services)
  })
  
  dashboard_metrics <- reactive({
    df <- official_dashboard_filtered_responses()
    settings_tbl <- current_settings_table()
    
    total_responses <- if (nrow(df) == 0) 0L else dplyr::n_distinct(df$dashboard_response_id)
    total_transactions <- if (nrow(settings_tbl) == 0) 0 else sum(settings_tbl$total_transaction, na.rm = TRUE)
    total_services <- if (nrow(settings_tbl) == 0) 0 else settings_tbl %>% distinct(office, office_display_name, type, service_name) %>% nrow()
    
    response_rate <- if (is.na(total_transactions) || total_transactions <= 0) {
      0
    } else {
      safe_pct(total_responses, total_transactions)
    }
    
    cc_awareness <- if (total_responses == 0 || !("cc1" %in% names(df))) {
      0
    } else {
      safe_pct(sum(df$cc1 %in% cc1_choices[1:3], na.rm = TRUE), total_responses)
    }
    
    cc2_den <- if ("cc2" %in% names(df)) sum(!is.na(df$cc2) & nzchar(trimws(as.character(df$cc2)))) else 0
    cc_visibility <- if (cc2_den == 0) 0 else safe_pct(sum(df$cc2 %in% cc2_choices[1], na.rm = TRUE), cc2_den)
    
    cc3_den <- if ("cc3" %in% names(df)) sum(!is.na(df$cc3) & nzchar(trimws(as.character(df$cc3)))) else 0
    cc_helpfulness <- if (cc3_den == 0) 0 else safe_pct(sum(df$cc3 %in% cc3_choices[1], na.rm = TRUE), cc3_den)
    
    sqd_cols_present <- intersect(sqd_cols, names(df))
    overall_sqd_score <- if (length(sqd_cols_present) == 0 || nrow(df) == 0) {
      0
    } else {
      compute_sqd_score(unlist(df[, sqd_cols_present, drop = FALSE], use.names = FALSE))
    }
    
    service_summary <- service_score_summary()
    
    tibble(
      metric = c(
        "No. of Responses",
        "Total Transactions",
        "Response Rate (%)",
        "CC Awareness (%)",
        "CC Visibility (%)",
        "CC Helpfulness (%)",
        "Overall SQD Score (%)",
        "Outstanding Services",
        "Total No. of Services"
      ),
      value = c(
        total_responses,
        total_transactions,
        response_rate,
        cc_awareness,
        cc_visibility,
        cc_helpfulness,
        overall_sqd_score,
        service_summary$outstanding_n,
        total_services
      )
    )
  })
  
  metric_value_num <- function(metrics, label) {
    val <- metrics$value[metrics$metric == label]
    if (length(val) == 0) return(NA_real_)
    suppressWarnings(as.numeric(val[[1]]))
  }
  
  format_pct_value <- function(x) {
    if (is.null(x) || length(x) == 0 || is.na(x)) {
      return("0.00%")
    }
    paste0(sprintf("%.2f", as.numeric(x)), "%")
  }
  
  format_num_value <- function(x) {
    if (is.null(x) || length(x) == 0 || is.na(x)) {
      return("0")
    }
    format(as.numeric(x), big.mark = ",", scientific = FALSE, trim = TRUE)
  }
  
  summary_box_ui <- function(title, value, cls) {
    div(
      class = paste("summary-box", cls),
      div(class = "summary-box-title", title),
      div(class = "summary-box-value", value)
    )
  }
  
  output$csm_dashboard_ui <- renderUI({
    metrics <- dashboard_metrics()
    
    if (nrow(metrics) == 0) {
      return(
        div(
          class = "privacy-box",
          "No submitted responses found for the selected filters."
        )
      )
    }
    
    div(
      class = "summary-grid",
      summary_box_ui("No. of Responses", format_num_value(metric_value_num(metrics, "No. of Responses")), "summary-forest"),
      summary_box_ui("Total Transactions", format_num_value(metric_value_num(metrics, "Total Transactions")), "summary-forest"),
      summary_box_ui("Response Rate", format_pct_value(metric_value_num(metrics, "Response Rate (%)")), "summary-forest"),
      summary_box_ui("CC Awareness", format_pct_value(metric_value_num(metrics, "CC Awareness (%)")), "summary-gold"),
      summary_box_ui("CC Visibility", format_pct_value(metric_value_num(metrics, "CC Visibility (%)")), "summary-gold"),
      summary_box_ui("CC Helpfulness", format_pct_value(metric_value_num(metrics, "CC Helpfulness (%)")), "summary-gold"),
      summary_box_ui("Overall SQD Score", format_pct_value(metric_value_num(metrics, "Overall SQD Score (%)")), "summary-maroon"),
      summary_box_ui("Outstanding Services", format_num_value(metric_value_num(metrics, "Outstanding Services")), "summary-maroon"),
      summary_box_ui("Total No. of Services", format_num_value(metric_value_num(metrics, "Total No. of Services")), "summary-maroon")
    )
  })
  
  demographic_display_data <- reactive({
    df <- official_dashboard_filtered_responses()
    if (nrow(df) == 0) return(tibble())
    
    df %>%
      mutate(
        response_id = if ("dashboard_response_id" %in% names(.)) as.character(.data$dashboard_response_id) else dplyr::coalesce(submission_id, submission_group_id),
        service_bucket = dplyr::case_when(
          stringr::str_detect(stringr::str_to_lower(dplyr::coalesce(service_type, "")), "internal") ~ "Internal",
          TRUE ~ "External"
        ),
        age_group = dplyr::case_when(is.na(age_group) | !nzchar(trimws(age_group)) | age_group == "Prefer not to disclose" ~ "Did Not Specify", TRUE ~ age_group),
        sex = dplyr::case_when(is.na(sex) | !nzchar(trimws(sex)) | sex == "Prefer not to disclose" ~ "Did Not Specify", TRUE ~ sex),
        region = dplyr::case_when(is.na(region) | !nzchar(trimws(region)) | region == "Prefer not to disclose" ~ "Did not specify", TRUE ~ region),
        client_type = dplyr::case_when(is.na(client_type) | !nzchar(trimws(client_type)) | client_type == "Prefer not to disclose" ~ "Did not specify", client_type == "Government (Employee or Another Agency)" ~ "Government", TRUE ~ client_type)
      ) %>%
      distinct(response_id, .keep_all = TRUE)
  })
  
  demographic_orders <- list(
    age_group = c("19 or lower", "20 - 34", "35 - 49", "50 - 64", "65 or higher", "Did Not Specify"),
    sex = c("Male", "Female", "Did Not Specify"),
    region = c("Region I", "Region II", "Region III", "Region IV-A CALABARZON", "Region IV-B MIMAROPA", "Region V", "Region VI", "Region VII", "Region VIII", "Region IX", "Region X", "Region XI", "Region XII", "Region XIII", "NCR", "CAR", "BARMM", "International/Outside PH", "Did not specify"),
    client_type = c("Citizen", "Business", "Government", "Did not specify")
  )
  
  demographic_distribution <- function(df, variable) {
    levels_vec <- demographic_orders[[variable]]
    tibble(Category = levels_vec) %>%
      mutate(
        External = purrr::map_dbl(Category, ~sum(df$service_bucket == "External" & df[[variable]] == .x, na.rm = TRUE)),
        Internal = purrr::map_dbl(Category, ~sum(df$service_bucket == "Internal" & df[[variable]] == .x, na.rm = TRUE)),
        Total = External + Internal
      )
  }
  
  render_demographics_plot <- function(variable, title_text) {
    plotly::renderPlotly({
      df <- demographic_display_data()
      validate(need(nrow(df) > 0, "No submitted responses found for the selected filters."))
      
      plot_df <- demographic_distribution(df, variable) %>%
        tidyr::pivot_longer(
          cols = c(External, Internal),
          names_to = "Series",
          values_to = "Count"
        ) %>%
        mutate(
          Category = as.character(Category),
          Series = factor(Series, levels = c("External", "Internal")),
          Count = as.integer(dplyr::coalesce(Count, 0)),
          hover_text = paste0(
            "<b>", Category, "</b><br>",
            "Group: ", Series, "<br>",
            "Count (No. of Response): ", Count
          )
        )
      
      validate(need(nrow(plot_df) > 0, "No demographic data found for the selected filters."))
      
      plotly::plot_ly(
        data = plot_df,
        x = ~Count,
        y = ~factor(Category, levels = rev(demographic_orders[[variable]])),
        color = ~Series,
        colors = c("External" = "forestgreen", "Internal" = "#7B1113"),
        type = "bar",
        orientation = "h",
        #text = ~hover_text
        hoverinfo = "text"
      ) %>%
        plotly::layout(
          title = list(text = title_text),
          barmode = "stack",
          xaxis = list(title = "Count (No. of Response)", rangemode = "tozero"),
          yaxis = list(title = ""),
          legend = list(orientation = "h", x = 0, y = 1.12),
          margin = list(l = 120, r = 30, t = 70, b = 50)
        )
    })
  }
  
  output$demographics_age_plot <- render_demographics_plot("age_group", "Age Distribution")
  output$demographics_sex_plot <- render_demographics_plot("sex", "Sex Distribution")
  output$demographics_region_plot <- render_demographics_plot("region", "Region Distribution")
  output$demographics_client_type_plot <- render_demographics_plot("client_type", "Client Type Distribution")
  
  sqd_service_office_tbl_reactive <- reactive({
    sqd_cols_present <- intersect(sqd_cols, names(dashboard_responses()))
    
    if (length(sqd_cols_present) == 0) {
      return(tibble(
        Services = "No SQD columns found in saved responses.",
        Type = "",
        Score = "",
        Rating = ""
      ))
    }
    
    offered_services_df <- current_settings_table() %>%
      distinct(office, office_display_name, type, service_name)
    
    if (nrow(offered_services_df) == 0) {
      return(tibble(
        Services = "No services found for the selected filters.",
        Type = "",
        Score = "",
        Rating = ""
      ))
    }
    
    df <- dashboard_filtered_responses()
    
    scored_tbl <- if (nrow(df) == 0) {
      tibble(
        office = character(),
        office_display_name = character(),
        service_type = character(),
        service_name = character(),
        sqd_score_pct = numeric()
      )
    } else {
      df %>%
        group_by(office, office_display_name, service_type, service_name) %>%
        group_modify(~{
          sqd_values <- unlist(.x[, sqd_cols_present, drop = FALSE], use.names = FALSE)
          sqd_values <- as.character(sqd_values)
          sqd_values <- sqd_values[!is.na(sqd_values) & nzchar(sqd_values)]
          
          numerator <- sum(sqd_values %in% c("Agree", "Strongly Agree"))
          denominator <- sum(sqd_values != "Not applicable")
          
          tibble(
            sqd_score_pct = if (denominator == 0) NA_real_ else round((numerator * 100) / denominator, 2)
          )
        }) %>%
        ungroup() %>%
        rename(type = service_type)
    }
    
    offered_services_df %>%
      left_join(scored_tbl, by = c("office", "office_display_name", "type", "service_name")) %>%
      mutate(
        Services = service_name,
        Type = type,
        rating = vapply(sqd_score_pct, function(x) rating_info(x)$label, character(1)),
        color = vapply(sqd_score_pct, function(x) rating_info(x)$color, character(1)),
        Score = ifelse(
          is.na(sqd_score_pct),
          "<span class='score-pill'>Not yet rated</span>",
          paste0("<span class='score-pill'>", sprintf("%.2f", sqd_score_pct), "%</span>")
        ),
        Rating = paste0(
          "<div class='rating-cell'><span class='rating-dot' style='background:", color,
          ";'></span><span class='rating-text'>", rating, "</span></div>"
        )
      ) %>%
      arrange(office_display_name, desc(!is.na(sqd_score_pct)), desc(sqd_score_pct), type, service_name) %>%
      select(Services, Type, Score, Rating)
  })
  
  response_rate_coverage_tbl <- reactive({
    settings_tbl <- current_settings_table() %>%
      distinct(office, office_display_name, type, service_name, total_transaction, sample_size, response_count, csm_coverage, remarks)
    
    if (nrow(settings_tbl) == 0) {
      return(tibble(
        office = character(),
        office_display_name = character(),
        type = character(),
        service_name = character(),
        total_transaction = numeric(),
        sample_size = numeric(),
        response_count = numeric(),
        response_rate_pct = numeric(),
        percent_sample_difference = numeric(),
        sqd_score_pct = numeric(),
        rating = factor(character(), levels = c("Outstanding", "Very Satisfactory", "Satisfactory", "Fair", "Poor", "NA")),
        color = character(),
        hover_text = character()
      ))
    }
    
    sqd_cols_present <- intersect(sqd_cols, names(dashboard_responses()))
    df <- dashboard_filtered_responses()
    
    scored_tbl <- if (nrow(df) == 0 || length(sqd_cols_present) == 0) {
      tibble(
        office = character(),
        office_display_name = character(),
        service_type = character(),
        service_name = character(),
        sqd_score_pct = numeric()
      )
    } else {
      df %>%
        group_by(office, office_display_name, service_type, service_name) %>%
        group_modify(~{
          sqd_values <- unlist(.x[, sqd_cols_present, drop = FALSE], use.names = FALSE)
          sqd_values <- as.character(sqd_values)
          sqd_values <- sqd_values[!is.na(sqd_values) & nzchar(sqd_values)]
          
          numerator <- sum(sqd_values %in% c("Agree", "Strongly Agree"))
          denominator <- sum(sqd_values != "Not applicable")
          
          tibble(
            sqd_score_pct = if (denominator == 0) NA_real_ else round((numerator * 100) / denominator, 2)
          )
        }) %>%
        ungroup()
    }
    
    rating_levels <- c("Outstanding", "Very Satisfactory", "Satisfactory", "Fair", "Poor", "NA")
    
    settings_tbl %>%
      left_join(
        scored_tbl %>% rename(type = service_type),
        by = c("office", "office_display_name", "type", "service_name")
      ) %>%
      mutate(
        total_transaction = suppressWarnings(as.numeric(total_transaction)),
        sample_size = suppressWarnings(as.numeric(sample_size)),
        response_count = suppressWarnings(as.numeric(response_count)),
        total_transaction = dplyr::coalesce(total_transaction, 0),
        sample_size = dplyr::coalesce(sample_size, 0),
        response_count = dplyr::coalesce(response_count, 0),
        response_rate_pct = dplyr::case_when(
          total_transaction <= 0 ~ 0,
          TRUE ~ round((response_count * 100) / total_transaction, 2)
        ),
        percent_sample_difference = dplyr::case_when(
          total_transaction <= 0 ~ 0,
          sample_size <= 0 ~ 0,
          TRUE ~ round(((response_count - sample_size) * 100) / sample_size, 2)
        ),
        rating = vapply(sqd_score_pct, function(x) rating_info(x)$label, character(1)),
        rating = factor(rating, levels = rating_levels),
        color = vapply(sqd_score_pct, function(x) rating_info(x)$color, character(1)),
        hover_service = vapply(
          as.character(service_name),
          function(x) paste(strwrap(htmltools::htmlEscape(x), width = 34), collapse = "<br>"),
          character(1)
        ),
        hover_office = vapply(
          as.character(office_display_name),
          function(x) paste(strwrap(htmltools::htmlEscape(x), width = 34), collapse = "<br>"),
          character(1)
        ),
        hover_type = vapply(
          as.character(type),
          function(x) paste(strwrap(htmltools::htmlEscape(x), width = 34), collapse = "<br>"),
          character(1)
        ),
        hover_text = paste0(
          "<b>", hover_service, "</b>",
          "<br>Office: ", hover_office,
          "<br>Type: ", hover_type,
          "<br>Response Rate: ", sprintf("%.2f", response_rate_pct), "%",
          "<br>Percent Sample Difference: ", sprintf("%.2f", percent_sample_difference), "%",
          "<br>Total Response: ", format(response_count, trim = TRUE, scientific = FALSE),
          "<br>Total Transaction: ", format(total_transaction, trim = TRUE, scientific = FALSE),
          "<br>Minimum Sample Size: ", format(sample_size, trim = TRUE, scientific = FALSE),
          "<br>SQD Score: ", ifelse(is.na(sqd_score_pct), "Not yet rated", paste0(sprintf("%.2f", sqd_score_pct), "%")),
          "<br>Rating: ", as.character(rating)
        )
      ) %>%
      arrange(office_display_name, type, service_name)
  })
  
  output$insights_plot <- plotly::renderPlotly({
    Sys.sleep(0.4)
    
    plot_df <- response_rate_coverage_tbl()
    validate(need(nrow(plot_df) > 0, "No services found for the selected filters."))
    
    legend_levels <- c("Outstanding", "Very Satisfactory", "Satisfactory", "Fair", "Poor", "NA")
    legend_colors <- c(
      "Outstanding" = "#228B22",
      "Very Satisfactory" = "#90B77D",
      "Satisfactory" = "#F2A900",
      "Fair" = "#A64D59",
      "Poor" = "#7B1113",
      "NA" = "#bdbdbd"
    )
    
    plot_df$rating <- factor(as.character(plot_df$rating), levels = legend_levels)
    
    x_range <- range(plot_df$response_rate_pct, na.rm = TRUE)
    y_range <- range(plot_df$percent_sample_difference, na.rm = TRUE)
    
    if (!all(is.finite(x_range))) x_range <- c(0, 1)
    if (!all(is.finite(y_range))) y_range <- c(-1, 1)
    if (diff(x_range) == 0) x_range <- x_range + c(-1, 1)
    if (diff(y_range) == 0) y_range <- y_range + c(-1, 1)
    
    x_pad <- diff(x_range) * 0.05
    y_pad <- diff(y_range) * 0.05
    x_range <- c(x_range[1] - x_pad, x_range[2] + x_pad)
    y_range <- c(y_range[1] - y_pad, y_range[2] + y_pad)
    
    p <- plotly::plot_ly()
    
    for (i in seq_along(legend_levels)) {
      level <- legend_levels[[i]]
      level_df <- plot_df %>% filter(as.character(rating) == level)
      
      if (nrow(level_df) > 0) {
        p <- p %>% plotly::add_trace(
          data = level_df,
          x = ~response_rate_pct,
          y = ~percent_sample_difference,
          type = "scatter",
          mode = "markers",
          name = level,
          legendgroup = level,
          legendrank = i,
          showlegend = TRUE,
          text = ~hover_text,
          hovertemplate = "%{text}<extra></extra>",
          marker = list(
            color = unname(legend_colors[[level]]),
            size = 9,
            opacity = 0.95,
            symbol = "circle"
          )
        )
      } else {
        p <- p %>% plotly::add_trace(
          x = x_range[[1]],
          y = y_range[[1]],
          type = "scatter",
          mode = "markers",
          name = level,
          legendgroup = level,
          legendrank = i,
          showlegend = TRUE,
          visible = "legendonly",
          hoverinfo = "skip",
          marker = list(
            color = unname(legend_colors[[level]]),
            size = 9,
            symbol = "circle"
          )
        )
      }
    }
    
    p %>%
      plotly::layout(
        showlegend = TRUE,
        legend = list(
          title = list(text = ""),
          traceorder = "normal",
          orientation = "v",
          groupclick = "togglegroup"
        ),
        hoverlabel = list(
          bgcolor = "rgba(255,255,255,0.75)",
          align = "left",
          font = list(color = "#000000"),
          namelength = -1
        ),
        xaxis = list(
          title = "Response Rate (%)",
          range = x_range,
          ticksuffix = "%",
          zeroline = FALSE
        ),
        yaxis = list(
          title = "Percent Sample Difference (%)",
          range = y_range,
          ticksuffix = "%",
          zeroline = FALSE
        ),
        shapes = list(
          list(
            type = "line",
            x0 = x_range[1],
            x1 = x_range[2],
            y0 = 0,
            y1 = 0,
            line = list(color = "#000000", width = 1, dash = "dash")
          )
        )
      )
  })
  
  csm_summary_export <- reactive({
    metrics <- dashboard_metrics()
    
    if (nrow(metrics) == 0) {
      tibble(Metric = character(), Value = character())
    } else {
      metrics %>% transmute(Metric = metric, Value = as.character(value))
    }
  })
  
  csm_coverage_export <- reactive({
    current_settings_table() %>%
      mutate(
        `Estimated Response Rate (%)` = ifelse(
          is.na(total_transaction) | total_transaction <= 0 | response_count > total_transaction,
          NA_real_,
          round((response_count / total_transaction) * 100, 2)
        )
      ) %>%
      transmute(
        Office = office_display_name,
        Service = service_name,
        Type = type,
        Coverage = csm_coverage,
        `Total Transaction` = total_transaction,
        `Sample Size` = sample_size,
        `No. of Response` = response_count,
        `Estimated Response Rate (%)`,
        Remarks = remarks
      ) %>%
      arrange(Office, Type, Service)
  })
  
  sqd_scores_export <- reactive({
    sqd_cols_present <- intersect(sqd_cols, names(dashboard_responses()))
    
    offered_services_df <- current_settings_table() %>%
      distinct(office, office_display_name, type, service_name)
    
    if (nrow(offered_services_df) == 0) {
      return(tibble(Office = character(), Services = character(), Type = character(), Score = character(), Rating = character()))
    }
    
    df <- dashboard_filtered_responses()
    
    scored_tbl <- if (nrow(df) == 0 || length(sqd_cols_present) == 0) {
      tibble(
        office = character(),
        office_display_name = character(),
        service_type = character(),
        service_name = character(),
        sqd_score_pct = numeric()
      )
    } else {
      df %>%
        group_by(office, office_display_name, service_type, service_name) %>%
        group_modify(~{
          sqd_values <- unlist(.x[, sqd_cols_present, drop = FALSE], use.names = FALSE)
          sqd_values <- as.character(sqd_values)
          sqd_values <- sqd_values[!is.na(sqd_values) & nzchar(sqd_values)]
          numerator <- sum(sqd_values %in% c("Agree", "Strongly Agree"))
          denominator <- sum(sqd_values != "Not applicable")
          
          tibble(
            sqd_score_pct = if (denominator == 0) NA_real_ else round((numerator * 100) / denominator, 2)
          )
        }) %>%
        ungroup() %>%
        rename(type = service_type)
    }
    
    offered_services_df %>%
      left_join(scored_tbl, by = c("office", "office_display_name", "type", "service_name")) %>%
      mutate(
        Office = office_display_name,
        Services = service_name,
        Type = type,
        Score = ifelse(is.na(sqd_score_pct), "Not yet rated", sprintf("%.2f%%", sqd_score_pct)),
        Rating = vapply(sqd_score_pct, function(x) rating_info(x)$label, character(1))
      ) %>%
      arrange(Office, desc(!is.na(sqd_score_pct)), desc(sqd_score_pct), Type, service_name) %>%
      select(Office, Services, Type, Score, Rating)
  })
  
  output$download_admin_excel <- downloadHandler(
    
    filename = function() {
      paste0(
        "upcebu_csm_admin_",
        format(Sys.Date(), "%Y%m%d"),
        ".xlsx"
      )
    },
    
    contentType =
      "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    
    content = function(file) {
      
      req(requireNamespace("openxlsx", quietly = TRUE))
      
      raw_df <- dashboard_filtered_responses()
      
      demographics_raw <- demographic_display_data()
      
      demographics_df <- if (nrow(demographics_raw) == 0) {
        tibble(
          Category = character(),
          External = numeric(),
          Internal = numeric(),
          Total = numeric(),
          Section = character()
        )
      } else {
        bind_rows(
          demographic_distribution(demographics_raw, "age_group") %>%
            mutate(Section = "D.1. Age", .before = 1),
          
          demographic_distribution(demographics_raw, "sex") %>%
            mutate(Section = "D.2. Sex", .before = 1),
          
          demographic_distribution(demographics_raw, "region") %>%
            mutate(Section = "D.3. Region", .before = 1),
          
          demographic_distribution(demographics_raw, "client_type") %>%
            mutate(Section = "D.4. Customer Type", .before = 1)
        )
      }
      
      summary_df  <- csm_summary_export()
      sqd_df      <- sqd_scores_export()
      coverage_df <- csm_coverage_export()
      
      wb <- openxlsx::createWorkbook()
      
      openxlsx::addWorksheet(wb, "Demographics")
      openxlsx::addWorksheet(wb, "Raw Data")
      openxlsx::addWorksheet(wb, "CSM Summary")
      openxlsx::addWorksheet(wb, "SQD Scores")
      openxlsx::addWorksheet(wb, "CSM Coverage")
      
      openxlsx::writeData(wb, "Demographics", demographics_df)
      openxlsx::writeData(wb, "Raw Data", raw_df)
      openxlsx::writeData(wb, "CSM Summary", summary_df)
      openxlsx::writeData(wb, "SQD Scores", sqd_df)
      openxlsx::writeData(wb, "CSM Coverage", coverage_df)
      
      openxlsx::saveWorkbook(
        wb,
        file = file,
        overwrite = TRUE
      )
    }
  )
  
  output$sqd_service_office_tbl <- DT::renderDT({
    DT::datatable(
      sqd_service_office_tbl_reactive(),
      rownames = FALSE,
      escape = FALSE,
      class = 'cell-border stripe hover order-column compact dashboard-datatable',
      options = list(
        pageLength = 10,
        autoWidth = TRUE,
        dom = 'tip',
        ordering = TRUE,
        columnDefs = list(list(className = 'dt-left', targets = c(0, 1, 2, 3)))
      )
    )
  })
  
  session$onFlushed(function() {
    goto_step(1)
  }, once = TRUE)
}

shinyApp(ui, server)
