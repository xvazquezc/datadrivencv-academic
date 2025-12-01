# This file contains all the code needed to parse and print various sections of your CV
# from data. Feel free to tweak it as you desire!

source("code/glue_templates.R")

#' Create a CV_Printer object.
#'
#' @param data_location Path of the spreadsheets holding all your data. This can be
#'   either a URL to a google sheet with multiple sheets containing the four
#'   data types or a path to a folder containing four `.csv`s with the neccesary
#'   data.
#' @param source_location Where is the code to build your CV hosted?
#' @param pdf_mode Is the output being rendered into a pdf? Aka do links need
#'   to be stripped?
#' @param sheet_is_publicly_readable If you're using google sheets for data,
#'   is the sheet publicly available? (Makes authorization easier.)
#' @return A new `CV_Printer` object.
create_CV_object <-  function(data_location,
                              pdf_mode = FALSE,
                              long_cv = TRUE,
                              sheet_is_publicly_readable = TRUE) {

  cv <- list(
    pdf_mode = pdf_mode,
    links = c()
  )

  is_google_sheets_location <- stringr::str_detect(data_location, "docs\\.google\\.com")

  if(is_google_sheets_location){
    if(sheet_is_publicly_readable){
      # This tells google sheets to not try and authenticate. Note that this will only
      # work if your sheet has sharing set to "anyone with link can view"
      googlesheets4::gs4_deauth()
    } else {
      # My info is in a public sheet so there's no need to do authentication but if you want
      # to use a private sheet, then this is the way you need to do it.
      # designate project-specific cache so we can render Rmd without problems
      options(gargle_oauth_cache = ".secrets")
    }

    read_gsheet <- function(sheet_id){
      googlesheets4::read_sheet(data_location, sheet = sheet_id, skip = 1, col_types = "c")
    }
    cv$entries_data  <- read_gsheet(sheet_id = "entries")
    cv$skills        <- read_gsheet(sheet_id = "skills")
    cv$text_blocks   <- read_gsheet(sheet_id = "text_blocks")
    cv$contact_info  <- read_gsheet(sheet_id = "contact_info")
    cv$teaching      <- read_gsheet(sheet_id = "teaching")
    cv$grants_awards <- read_gsheet(sheet_id = "grants_awards")
    cv$training      <- read_gsheet(sheet_id = "training")
    cv$service       <- read_gsheet(sheet_id = "service")
    cv$supervision   <- read_gsheet(sheet_id = "supervision")
    
  } else {
    # Want to go old-school with csvs?
    # TBC
    # cv$entries_data <- readr::read_csv(paste0(data_location, "entries.csv"), skip = 1)
    # cv$skills       <- readr::read_csv(paste0(data_location, "language_skills.csv"), skip = 1)
    # cv$text_blocks  <- readr::read_csv(paste0(data_location, "text_blocks.csv"), skip = 1)
    # cv$contact_info <- readr::read_csv(paste0(data_location, "contact_info.csv"), skip = 1)
    cv$entries_data  <- readxl::read_excel(data_location, "entries", skip = 1, col_types = c("text", "text", "text", "text", "date", "date", "text", "text", "text", "text"))
    cv$teaching      <- readxl::read_excel(data_location, "teaching", skip = 1, col_types = c("text", "text", "text", "text", "date", "date", "text", "text", "text", "text"))
    cv$grants_awards <- readxl::read_excel(data_location, "grants_awards", skip = 1, col_types = c("text", "text", "text", "text", "date", "date", "text", "text", "text", "text", "text"))
    cv$training      <- readxl::read_excel(data_location, "training", skip = 1, col_types = c("text", "text", "date", "date", "text", "text", "text", "text", "text"))
    cv$service       <- readxl::read_excel(data_location, "service", skip = 1, col_types = c("text", "text", "text", "text", "date", "date", "text", "text"))
    cv$skills        <- readxl::read_excel(data_location, "skills", skip = 1, col_types = c("text", "text", "text", "text"))
    cv$text_blocks   <- readxl::read_excel(data_location, "text_blocks", skip = 1, col_types = c("text", "text"))
    cv$contact_info  <- readxl::read_excel(data_location, "contact_info", skip = 1, col_types = c("text", "text", "text"))
    
    # cv$entries_data  <- xlsx::read.xlsx(data_location, sheetName = "entries", startRow = 2, colClasses = "character")
    # cv$skills        <- xlsx::read.xlsx(data_location, sheetName = "language_skills", startRow = 2, colClasses = "character")
    # cv$text_blocks   <- xlsx::read.xlsx(data_location, sheetName = "text_blocks", startRow = 2, colClasses = "character")
    # cv$contact_info  <- xlsx::read.xlsx(data_location, sheetName = "contact_info", startRow = 2, colClasses = "character")
    # cv$teaching      <- xlsx::read.xlsx(data_location, sheetName = "teaching", startRow = 2, colClasses = "character")
    # cv$grants_awards <- xlsx::read.xlsx(data_location, sheetName = "grants_awards", startRow = 2, colClasses = "character")
    # cv$training      <- xlsx::read.xlsx(data_location, sheetName = "training", startRow = 2, colClasses = "character")
    # cv$service       <- xlsx::read.xlsx(data_location, sheetName = "service", startRow = 2, colClasses = "character")
  }


  extract_year <- function(dates){
    date_year <- stringr::str_extract(dates, "(20|19)[0-9]{2}")
    date_year[is.na(date_year)] <- lubridate::year(lubridate::ymd(Sys.Date())) + 10

    date_year
  }

  parse_dates <- function(dates){

    date_month <- stringr::str_extract(dates, "(\\w+|\\d+)(?=(\\s|\\/|-)(20|19)[0-9]{2})")
    date_month[is.na(date_month)] <- "1"

    paste("1", date_month, extract_year(dates), sep = "-") %>%
      lubridate::dmy()
  }

  # Clean up entries dataframe to format we need it for printing
  cv$entries_data %<>%
    {if(long_cv == TRUE) tidyr::unite(., tidyr::starts_with('description'), col = "description_bullets", sep = "\n", na.rm = TRUE) else dplyr::mutate(., description_bullets = description_1)} %>%
    dplyr::mutate(
      description_bullets = ifelse(description_bullets != "", paste0("- ", stringr::str_replace(stringr::str_replace_all(description_bullets, "\n", "\n- "), "\n- $", "\n")), ""),
      start = ifelse(start == "NULL", NA, format(lubridate::ymd(start), "%b %Y")),
      end = ifelse(end == "NULL", NA, format(lubridate::ymd(end), "%b %Y")),
      start_year = extract_year(start),
      end_year = extract_year(end),
      no_start = is.na(start),
      has_start = !no_start,
      no_end = is.na(end),
      has_end = !no_end,
      timeline = dplyr::case_when(
        no_start  & no_end     ~ "N/A",
        no_start  & has_end    ~ as.character(end_year),
        has_start & no_end     ~ paste("Current", "-", start_year),
        start_year == end_year ~ as.character(end_year), 
        TRUE                   ~ paste(end_year, "-", start_year)
      )
    ) %>%
    dplyr::arrange(dplyr::desc(parse_dates(end))) %>%
    dplyr::mutate_all(~ ifelse(is.na(.), 'N/A', .))
  
  cv$teaching %<>%
    {if(long_cv == TRUE) tidyr::unite(., tidyr::starts_with('description'), col = "description_bullets", sep = "\n", na.rm = TRUE) else dplyr::mutate(., description_bullets = description_1)} %>%
    dplyr::mutate(
      description_bullets = ifelse(description_bullets != "", paste0("- ", stringr::str_replace(stringr::str_replace_all(description_bullets, "\n", "\n- "), "\n- $", "\n")), ""),
      start = ifelse(start == "NULL", NA, format(lubridate::ymd(start), "%Y %b %d")),
      end = ifelse(end == "NULL", NA, format(lubridate::ymd(end), "%Y %b %d"))
    ) %>%
    tidyr::separate(end,
                    into = c("end_year", "end_month", "end_day"),
                    sep = " ", extra = "merge", remove = FALSE) %>%
    tidyr::separate(start,
                    into = c("start_year", "start_month", "start_day"),
                    sep = " ", extra = "merge", remove = FALSE) %>%
    dplyr::mutate(
      start_day = stringr::str_remove(start_day, "^0"),
      end_day = stringr::str_remove(end_day, "^0"),
      no_start = is.na(start),
      has_start = !no_start,
      no_end = is.na(end),
      has_end = !no_end,
      timeline = dplyr::case_when(
        no_start  & no_end     ~ "N/A",
        no_start  & has_end    ~ as.character(end_year),
        has_start & no_end     ~ paste("Current", "-", start_year),
        start_year == end_year ~ as.character(end_year), 
        TRUE                   ~ paste(end_year, "-", start_year)
      )
    ) %>%
    dplyr::mutate(
      date = dplyr::case_when(
        has_start & start_month == end_month ~ paste0(start_day, "--", end_day, " ", end_month),
        has_start & start_month != end_month ~ paste0(start_day, " ", start_month, "--", end_day, " ", end_month),
        no_start ~ paste0(end_day, " ", end_month)
      )
    ) %>%
    dplyr::arrange(dplyr::desc(parse_dates(end))) %>%
    dplyr::mutate_all(~ ifelse(is.na(.), 'N/A', .)) %>%
    dplyr::group_by(section,end_year) %>%
    dplyr::mutate(count = 1:dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(timeline = dplyr::case_when(count != 1 ~ "N/A", TRUE ~ as.character(end_year)))
  
  
  cv$grants_awards %<>%
    {if(long_cv == TRUE) tidyr::unite(., tidyr::starts_with('description'), col = "description_bullets", sep = "\n", na.rm = TRUE) else dplyr::mutate(., description_bullets = description_1)} %>%
    tidyr::separate(end,
                    into = c("end_year", "end_month", "end_day"),
                    sep = " ", extra = "merge", remove = FALSE) %>%
    tidyr::separate(start,
                    into = c("start_year", "start_month", "start_day"),
                    sep = " ", extra = "merge", remove = FALSE) %>%
    dplyr::mutate(
      description_bullets = ifelse(description_bullets != "", paste0("- ", stringr::str_replace(stringr::str_replace_all(description_bullets, "\n", "\n- "), "\n- $", "\n")), ""),
      start = ifelse(start == "NULL", NA, format(lubridate::ymd(start), "%b %Y")),
      end = ifelse(end == "NULL", NA, format(lubridate::ymd(end), "%b %Y")),
      start_year = extract_year(start),
      end_year = extract_year(end),
      no_start = is.na(start),
      has_start = !no_start,
      no_end = is.na(end),
      has_end = !no_end,
      timeline = dplyr::case_when(
        no_start  & no_end     ~ "N/A",
        no_start  & has_end    ~ as.character(end_year),
        has_start & no_end     ~ paste("Current", "-", start_year),
        start_year == end_year ~ as.character(end_year), 
        TRUE                   ~ paste(end_year, "-", start_year)
      )
    ) %>%
    dplyr::arrange(dplyr::desc(parse_dates(end))) %>%
    dplyr::mutate_all(~ ifelse(is.na(.), 'N/A', .))
  
  
  cv$training_hours <- # hours of training/profdev in last 2y
    cv$training %>%
    dplyr::filter(lubridate::ymd(cv$training$end) > (lubridate::today() - lubridate::years(2))) %>%
    dplyr::summarise(total = sum(as.numeric(duration), na.rm = TRUE)) %>%
    dplyr::pull() %>% round(., -1)
  
  cv$training %<>%
    dplyr::filter(in_resume == "TRUE") %>%
    tidyr::separate(end,
                    into = c("year", "month", "day"),
                    sep = "-", extra = "merge") %>%
    dplyr::arrange(dplyr::desc(year), desc(month), desc(day)) %>%
    dplyr::mutate_all(~ ifelse(is.na(.), 'N/A', .)) %>%
    dplyr::group_by(year, section) %>%
    dplyr::mutate(count = 1:dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(timeline = dplyr::case_when(count != 1 ~ "N/A", TRUE ~ as.character(year)))
  
  
  cv$service %<>%
    dplyr::filter(in_resume == "TRUE") %>%
    dplyr::mutate(
      start = ifelse(nchar(start) == 4, paste0(start, "-01-01"), start),
      start_year = ifelse(stringr::str_detect(start, "\\-"),
                     format(lubridate::ymd(start), "%Y"), NA),
      end = ifelse(nchar(end) == 4, paste0(end, "-01-01"), end),
      end_year = ifelse(stringr::str_detect(end, "\\-"),
                   format(lubridate::ymd(end), "%Y"), NA),
      no_start = is.na(start),
      has_start = !no_start,
      no_end = is.na(end),
      has_end = !no_end,
      timeline = dplyr::case_when(
        no_start  & no_end  ~ 'NA',
        (no_start  & has_end) | (start_year == end_year) ~ end_year,
        has_start & no_end  ~ paste0(start_year, "--", "Current"),
        has_start & has_end ~ paste0(start_year, "--", end_year)
      )
    ) %>% 
    dplyr::arrange(end_year, title, what) %>%
    dplyr::mutate_all(~ ifelse(is.na(.), 'N/A', .)) %>%
    dplyr::group_by(what) %>%
    dplyr::mutate(count = 1:dplyr::n()) %>%
    dplyr::ungroup() %>%
    # ## extra/complicated part to make a table into a multi-level bulleted list
    dplyr::arrange(dplyr::if_else(section == "service_conf",
                                  dplyr::desc(as.double(xtfrm(end_year))),
                                  as.double(xtfrm(what)))) %>%
    dplyr::mutate(count = 1:dplyr::n()) %>%
    dplyr::group_by(what, .drop = TRUE) %>%
    dplyr::summarise(
      where = where,
      count = count,
      section = section,
      subtext = dplyr::case_when(
        section == "reviews" ~ 
          title,
        section == "membership_inst" ~
          paste0(glue::glue("    - {title} at the {what} ({timeline}).", .trim = FALSE), collapse = "\n"),
        section == "membership_prof" | section == "service_conf" ~
          paste0(glue::glue("    - {title} ({timeline}).", .trim = FALSE), collapse = "\n"))
    ) %>%
    dplyr::distinct(what, section, subtext, .keep_all = TRUE) %>%
    dplyr::mutate(
      display_text = dplyr::case_when(
        section == "reviews" ~ 
          what,
        section == "membership_inst" ~
          paste0("- __", where, "__\n", subtext, collapse = "\n"),
        section == "membership_prof" | section == "service_conf" ~
          paste0("- __", what, "__\n", subtext, collapse = "\n"))) %>%
    dplyr::ungroup()%>%
    dplyr::group_by(section) %>%
    dplyr::arrange(count) %>%
    dplyr::summarize(
      count = count, 
      section = section, 
      bullets = paste(display_text, collapse = "\n"),
      subtext = subtext
    ) %>%
    dplyr::distinct(section, bullets, .keep_all = TRUE)

  ### supervision
  #extract stats
  informal_super <- 
    cv$supervision %>%
    dplyr::filter(section == "non_formal") 
  
  super_levels <- 
    informal_super %>% 
    dplyr::distinct(level) %>%
    dplyr::pull(level) %>%
    glue::glue_collapse(", ", last = ", and ")
  
  cv$student_count_per_level <-
    informal_super %>% 
    dplyr::group_by(level) %>%
    dplyr::summarise(n = dplyr::n()) %>%
    glue::glue_data("{n} {level}") %>%
    glue::glue_collapse(", ", last = ", and ")
  
  cv$student_count <- 
    informal_super %>% 
    nrow()
  
  cv$supervision %<>%
    dplyr::mutate(
      role = paste0("Role: ", what),
      start = ifelse(nchar(start) == 4, paste0(start, "-01-01"), start),
      start = ifelse(stringr::str_detect(start, "\\-"),
                          format(lubridate::ymd(start), "%Y"), NA),
      end = ifelse(nchar(end) == 4, paste0(end, "-01-01"), end),
      end = ifelse(stringr::str_detect(end, "\\-"),
                        format(lubridate::ymd(end), "%Y"), NA),
      no_start = is.na(start),
      has_start = !no_start,
      no_end = is.na(end),
      has_end = !no_end,
      timeline = dplyr::case_when(
        no_start  & no_end  ~ 'NA',
        (no_start & has_end) | (start == end) ~ end,
        has_start & no_end  ~ paste("Current", "-", start),
        has_start & has_end ~ paste(end, "-", start)
      )) %>%
    dplyr::arrange(dplyr::desc(start), level, title) %>%
    dplyr::mutate_all(~ ifelse(is.na(.), 'N/A', .))

  cv
}


create_PUB_object <- function(data_location,
                              aliases = "aliases.txt",
                              long_cv = TRUE,
                              pdf_mode = FALSE) {
  pub <- list(
    pdf_mode = pdf_mode,
    links = c()
  )
  
  pub_types <- c("peer-reviewed", "conf_oral", "conf_poster", "under_review", "conf_coauth", "reports", "preprints", "invited_talks")
  pubtype_fix <- gsub("-", "_", pub_types)
  
  for (i in 1:length(pub_types)) {
    file_path <- paste0(data_location, pub_types[[i]], ".json")
    if (file.exists(file_path)) {
      pub[[pubtype_fix[[i]]]] <- jsonlite::read_json(file_path, simplifyVector = T)$items %>% 
        dplyr::select(!starts_with("relations"))
    }
  }
  
  aliases <- readLines(aliases)
  # Want to go old-school with csvs?
  # if(file.exists(paste0(data_location, "peer-reviewed.json"))) {
  #   pub$peer_reviewed <- jsonlite::read_json(paste0(data_location, "peer-reviewed.json"), simplifyVector = T)$items
  # }
  # if(file.exists(paste0(data_location, "conf_oral.json"))) {
  #   pub$conf_oral     <- jsonlite::read_json(paste0(data_location, "conf_oral.json"), simplifyVector = T)$items
  # }
  # if(file.exists(paste0(data_location, "conf_poster.json"))) {
  #   pub$conf_poster   <- jsonlite::read_json(paste0(data_location, "conf_poster.json"), simplifyVector = T)$items
  # }
  # if(file.exists(paste0(data_location, "under_review.json"))) {
  #   pub$under_review  <- jsonlite::read_json(paste0(data_location, "under_review.json"), simplifyVector = T)$items
  # }
  # if(file.exists(paste0(data_location, "conf_coauth.json"))) {
  #   pub$conf_coauth   <- jsonlite::read_json(paste0(data_location, "conf_coauth.json"), simplifyVector = T)$items
  # }
  # if(file.exists(paste0(data_location, "reports.json"))) {
  #   pub$reports       <- jsonlite::read_json(paste0(data_location, "reports.json"), simplifyVector = T)$items
  # }  
  # if(file.exists(paste0(data_location, "preprints.json"))) {
  #   pub$preprints     <- jsonlite::read_json(paste0(data_location, "preprints.json"), simplifyVector = T)$items
  # }
  # if(file.exists(paste0(data_location, "invited_talks.json"))) {
  #   pub$invited_talks <- jsonlite::read_json(paste0(data_location, "invited_talks.json"), simplifyVector = T)$items
  # }
  
  
  extract_year <- function(dates){
    date_year <- stringr::str_extract(dates, "(20|19)[0-9]{2}")
    date_year[is.na(date_year)] <- lubridate::year(lubridate::ymd(Sys.Date())) + 10
    
    date_year
  }
  
  parse_dates <- function(dates){
    date_month <- stringr::str_extract(dates, "(\\w+|\\d+)(?=(\\s|\\/|-)(20|19)[0-9]{2})")
    date_month[is.na(date_month)] <- "1"
    
    paste("1", date_month, extract_year(dates), sep = "-") %>%
      lubridate::dmy()
  }
  
  parse_creators <- function(reference){
    reference$authors <- NULL
    for (i in 1:nrow(reference)) {
      reference$authors[[i]] <- reference$creators[[i]] %>%
        tidyr::separate(firstName, into = c("given", "middle"), sep = " ") %>%
        dplyr::mutate(given = paste0(substr(given, 1, 1), "."),
                      middle = ifelse(is.na(middle), NA, paste0(substr(middle, 1, 1),"."))) %>%
        dplyr::mutate(initials = apply(cbind(.$given, .$middle),1,  function(x) paste(x[!is.na(x)], collapse = " ")),
                      full_name = paste(initials, lastName),
                      authors = sapply(full_name, toString)) %>%
        dplyr::pull(authors) %>%
        toString(.)
    }
    reference$authors %<>%
      stringi::stri_replace_last_fixed(., pattern = ", ", replacement = " & ") %>%
      #make bold any of the aliases if they are followed by comma, " &" or they are the last author
      # this avoids issues with an alias being a partial match of another
      stringr::str_replace_all(., 
                               setNames(paste0("**", aliases, "**"), 
                                        paste0(aliases, "(?=(,|$| &))")))
    
    
    reference$authors
  }
  
  
  # Clean up entries dataframe to format we need it for printing
  ## peer-reviewed papers
  pub$peer_reviewed$section <- "peer_reviewed"
  
  pub$peer_reviewed_total <- nrow(pub$peer_reviewed)
  
  pub$peer_reviewed$authors <- parse_creators(pub$peer_reviewed)

  pub$peer_reviewed %<>%
    dplyr::mutate(
      description_bullets = NA,
      doi = paste0("<a href=\'https://doi.org/", DOI, "\'><i class=\'ai ai-doi\'></i></a>"),
      journal_numbers = dplyr::case_when(is.na(issue) ~
                                           paste0(volume, ", ", pages),
                                         !is.na(issue) ~ 
                                           paste0(volume, "(", issue, "), ", pages)),
      journal = publicationTitle,
      tags = sapply(.$tag, function(x) paste(x[!is.na(x)], collapse = "-")),
      authors = ifelse(stringr::str_detect(tags, "cofirst"), 
                       paste(
                         stringr::str_replace(authors, ", ", ",zzz") %>% #mark first 2 occurrences
                           stringr::str_replace(., ", ", ",zzz") %>%
                           stringr::str_replace_all(., ",zzz", "\\\\*, "), #change them back
                         "[* co-first authors]"), # add "[* co-first authors]" note at the end
                       authors)
    ) %>%
    tidyr::separate(date,
                    into = c("year", "month", "day"),
                    sep = " ", extra = "merge", remove = FALSE) %>%
    dplyr::arrange(dplyr::desc(lubridate::ymd(date)))%>%
    dplyr::mutate_all(~ ifelse(is.na(.), 'N/A', .))
  
  ifelse(
    long_cv == TRUE,
    (pub$peer_reviewed %<>%
       dplyr::group_by(year) %>%
       dplyr::mutate(count = 1:dplyr::n()) %>%
       dplyr::ungroup() %>%
       dplyr::mutate(timeline = dplyr::case_when(count != 1 ~ "N/A", TRUE ~ as.character(year)))
    ),
    (pub$peer_reviewed %<>%
       dplyr::filter(stringr::str_detect(tags, "selected"))  %>%
       dplyr::group_by(year) %>%
       dplyr::mutate(count = 1:dplyr::n()) %>%
       dplyr::ungroup() %>%
       dplyr::mutate(timeline = dplyr::case_when(count != 1 ~ "N/A", TRUE ~ as.character(year)))
    )
  )
  
  pub$peer_reviewed_sel_count <- nrow(pub$peer_reviewed)
  
  ## papers under review (without preprint)
  pub$under_review$section <- "under_review"
  
  pub$under_review$authors <- parse_creators(pub$under_review)
  
  pub$under_review %<>%
    dplyr::mutate(
      description_bullets = NA,
      doi = NA,
      journal_numbers = volume,
      journal = publicationTitle,
      tags = sapply(.$tag, function(x) paste(x[!is.na(x)], collapse = "-")),
      authors = ifelse(stringr::str_detect(tags, "cofirst"), 
                       paste(
                         stringr::str_replace(authors, ", ", ",zzz") %>% #mark first 2 occurrences
                           stringr::str_replace(., ", ", ",zzz") %>%
                           stringr::str_replace_all(., ",zzz", "\\\\*, "), #change them back
                         "[* co-first authors]"), # add "[* co-first authors]" note at the end
                       authors)
    ) %>%
    tidyr::separate(date,
                    into = c("year", "month", "day"),
                    sep = " ", extra = "merge", remove = FALSE) %>%
    dplyr::arrange(dplyr::desc(lubridate::ymd(date)))%>%
    dplyr::mutate_all(~ ifelse(is.na(.), 'N/A', .)) %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(count = 1:dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(timeline = dplyr::case_when(count != 1 ~ "N/A", TRUE ~ as.character(year)))
  
  ## reports
  pub$reports$section <- "reports"

  pub$reports$authors <- parse_creators(pub$reports)
  
  pub$reports %<>%
    dplyr::mutate(
      description_bullets = NA,
      journal = reportType,
      tags = sapply(.$tag, function(x) paste(x[!is.na(x)], collapse = "-")),
      authors = ifelse(stringr::str_detect(tags, "cofirst"), 
                       paste(
                         stringr::str_replace(authors, ", ", ",zzz") %>% #mark first 2 occurrences
                           stringr::str_replace(., ", ", ",zzz") %>%
                           stringr::str_replace_all(., ",zzz", "\\\\*, "), #change them back
                         "[* co-first authors]"), # add "[* co-first authors]" note at the end
                       authors)
    ) %>%
    tidyr::separate(date,
                    into = c("year", "month", "day"),
                    sep = " ", extra = "merge", remove = FALSE) %>%
    dplyr::arrange(dplyr::desc(year), dplyr::desc(factor(month, levels = month.abb))) %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(count = 1:dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(timeline = dplyr::case_when(count != 1 ~ "N/A", TRUE ~ as.character(year)),
                  day = stringr::str_replace(day, "-", "--"))
  
  ## preprints (only if there is some current ones)
  if(length(pub[["preprints"]]) != 0){
    pub$preprints$section <- "preprints"
    
    pub$preprints$authors <- parse_creators(pub$preprints)
    
    pub$preprints %<>%
      dplyr::mutate(
        description_bullets = NA,
        doi = paste0("<a href=\'https://doi.org/", DOI, "\'><i class=\'ai ai-doi\'></i></a>"),
        journal_numbers = archiveID,
        journal = repository,
        tags = sapply(.$tag, function(x) paste(x[!is.na(x)], collapse = "-")),
        authors = ifelse(stringr::str_detect(tags, "cofirst"), 
                         paste(
                           stringr::str_replace(authors, ", ", ",zzz") %>% #mark first 2 occurrences
                             stringr::str_replace(., ", ", ",zzz") %>%
                             stringr::str_replace_all(., ",zzz", "\\\\*, "), #change them back
                           "[* co-first authors]"), # add "[* co-first authors]" note at the end
                         authors)
      ) %>%
      tidyr::separate(date,
                      into = c("year", "month", "day"),
                      sep = " ", extra = "merge", remove = FALSE) %>%
      dplyr::arrange(dplyr::desc(year), dplyr::desc(factor(month, levels = month.abb))) %>%
      dplyr::group_by(year) %>%
      dplyr::mutate(count = 1:dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(timeline = dplyr::case_when(count != 1 ~ "N/A", TRUE ~ as.character(year)),
                    day = stringr::str_replace(day, "-", "--"))
  }
  

  ## conferences - oral
  pub$conf_oral$section <- "conf_oral"

  pub$conf_oral$authors <- parse_creators(pub$conf_oral)
  
  pub$conf_oral_count <- nrow(pub$conf_oral)
  
  pub$conf_oral %<>%
    dplyr::mutate(
      description_bullets = NA,
      doi = ifelse(is.na(DOI), NA_character_, paste0("<a href=\'https://doi.org/", DOI, "\'><i class=\'ai ai-doi\'></i></a>"))
    ) %>%
    tidyr::separate(date,
                    into = c("year", "month", "day"),
                    sep = " ", extra = "merge") %>%
    dplyr::arrange(dplyr::desc(year), desc(month), desc(day)) %>%
    dplyr::mutate_all(~ ifelse(is.na(.), 'N/A', .))
         
  ifelse(long_cv == TRUE, 
         (pub$conf_oral %<>%
           dplyr::group_by(year) %>%
           dplyr::mutate(count = 1:dplyr::n()) %>%
           dplyr::ungroup() %>%
           dplyr::mutate(timeline = dplyr::case_when(count != 1 ~ "N/A", TRUE ~ as.character(year)),
                         day = stringr::str_replace(day, "-", "--"))),
         (pub$conf_oral %<>%
            dplyr::mutate(tags = invisible(sapply(.$tag, function(x) paste(x[!is.na(x)], collapse = "-")))) %>%
            dplyr::select(place, year, month, day, conferenceName, title, authors, tags, section, doi) %>%
            dplyr::filter(stringr::str_detect(tags, "selected")))
  )
         
  
  ## conferences - posters
  pub$conf_poster$section <- "conf_poster"
  
  pub$conf_poster$authors <- parse_creators(pub$conf_poster)
  
  pub$conf_poster_count <- nrow(pub$conf_poster)
  
  pub$conf_poster %<>%
    dplyr::mutate(
      description_bullets = NA,
      doi = ifelse(is.na(DOI), NA_character_, paste0("<a href=\'https://doi.org/", DOI, "\'><i class=\'ai ai-doi\'></i></a>"))
    ) %>%
    tidyr::separate(date,
                    into = c("year", "month", "day"),
                    sep = " ", extra = "merge") %>%
    dplyr::arrange(dplyr::desc(year), dplyr::desc(factor(month, levels = month.abb))) %>%
    dplyr::mutate_all(~ ifelse(is.na(.), 'N/A', .))
  
  ifelse(long_cv == TRUE, 
         (pub$conf_poster %<>%
            dplyr::group_by(year) %>%
            dplyr::mutate(count = 1:dplyr::n()) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(timeline = dplyr::case_when(count != 1 ~ "N/A", TRUE ~ as.character(year)),
                          day = stringr::str_replace(day, "-", "--"))),
         (pub$conf_poster %<>%
            dplyr::mutate(tags = sapply(.$tag, function(x) paste(x[!is.na(x)], collapse = "-"))) %>%
            dplyr::select(place, year, month, day, conferenceName, title, authors, tags, section, doi) %>%
            dplyr::filter(stringr::str_detect(tags, "selected")))
  )
  
  #join if short cv
  if(long_cv != TRUE){
    pub$conferences <- rbind(pub$conf_poster, pub$conf_oral) %>%
      dplyr::mutate(
        authors = dplyr::case_when(
          section == "conf_oral"   ~ paste(authors, '[oral presentation]'),
          section == "conf_poster" ~ paste(authors, '[poster]')),
        section = "conferences"
      ) %>%
      dplyr::arrange(dplyr::desc(year), dplyr::desc(factor(month, levels = month.abb))) %>%
      dplyr::group_by(year) %>%
      dplyr::mutate(count = 1:dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(timeline = dplyr::case_when(count != 1 ~ "N/A", TRUE ~ as.character(year)),
                    day = stringr::str_replace(day, "-", "--"))
  }
  
  pub$conf_sel_count <- nrow(pub$conferences)
  
  pub$conf_total <- pub$conf_oral_count + pub$conf_poster_count
  
  ## conferences - coauthored contrib
  pub$conf_coauth$section <- "conf_coauth"
  
  pub$conf_coauth_total <- nrow(pub$conf_coauth)
  
  pub$conf_coauth$authors <- parse_creators(pub$conf_coauth)
  
  pub$conf_coauth %<>%
    dplyr::mutate(
      description_bullets = NA,
      doi = ifelse(is.na(DOI), NA_character_, paste0("<a href=\'https://doi.org/", DOI, "\'><i class=\'ai ai-doi\'></i></a>")),
      tags = sapply(.$tag, function(x) paste(x[!is.na(x)], collapse = "-"))
    ) %>%
    dplyr::filter(stringr::str_detect(tags, "selected")) %>%
    tidyr::separate(date,
                    into = c("year", "month", "day"),
                    sep = " ", extra = "merge") %>%
    dplyr::arrange(dplyr::desc(year), dplyr::desc(factor(month, levels = month.abb))) %>%
    dplyr::mutate_all(~ ifelse(is.na(.), 'N/A', .)) %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(count = 1:dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(timeline = dplyr::case_when(count != 1 ~ "N/A", TRUE ~ as.character(year)))
  
  pub$conf_coauth_sel_count <- nrow(pub$conf_coauth)
  
  ## invited talks
  pub$invited_talks$section <- "invited_talks"
  
  pub$invited_talks %<>%
    dplyr::mutate(
      description_bullets = NA,
      url = ifelse(is.na(url), NA_character_, paste0("<a href=\'", url, "\'><i class=\'fa fa-link\'></i></a>"))
    ) %>%
    tidyr::separate(date,
                    into = c("year", "month", "day"),
                    sep = " ", extra = "merge") %>%
    dplyr::arrange(dplyr::desc(year), dplyr::desc(factor(month, levels = month.abb))) %>%
    dplyr::mutate_all(~ ifelse(is.na(.), 'N/A', .)) %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(count = 1:dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(timeline = dplyr::case_when(count != 1 ~ "N/A", TRUE ~ as.character(year)))
  
  pub
}



#' @description Take a position data frame and the section id desired and prints the section to markdown.
#' @param section_id ID of the entries section to be printed as encoded by the `section` column of the `entries` table
print_section <- function(cv, section_id, glue_template = "default"){
  ## put all possible template in a separate file
  glue_template = dplyr::case_when(glue_template == "default" ~
                                     template_default,
                                   glue_template == "teaching" ~
                                     template_teaching,
                                   glue_template == "grants" ~
                                     template_grants,
                                   glue_template == "training" ~
                                     template_training,
                                   glue_template == "pubs" ~
                                     template_pubs,
                                   glue_template == "under_review" ~
                                     template_under_review,
                                   glue_template == "preprints" ~
                                     template_preprints,
                                   glue_template == "reports" ~
                                     template_reports,
                                   glue_template == "conferences" ~
                                     template_conferences,
                                   glue_template == "invited_talks" ~
                                     template_invited_talks,
                                   glue_template == "supervision" ~
                                     template_supervision,
                                   glue_template == "service_gen" ~
                                     template_service_gen,
                                   glue_template == "reviews" ~
                                     template_reviews
  )
  section_data <- dplyr::filter(cv, section == section_id)
  
  print(glue::glue_data(section_data, glue_template, .trim = FALSE))
  
  invisible(cv)
}


#' @description Prints out text block identified by a given label.
#' @param label ID of the text block to print as encoded in `label` column of `text_blocks` table.
print_text_block <- function(cv, label){
  text_block <- dplyr::filter(cv$text_blocks, loc == label) %>%
    dplyr::pull(text)
  
  cat(glue::glue(text_block, .trim = FALSE))
  
  invisible(cv)
}

print_text_block_aside <- function(cv, label){
  text_block <- dplyr::filter(cv$text_blocks, loc == label) %>%
    dplyr::pull(text)
  
  cat(glue::glue(paste0("::: aside\n", text_block, "\n:::"), .trim = FALSE))
  
  invisible(cv)
}


#' @description Construct a bar chart of skills
#' @param out_of The relative maximum for skills. Used to set what a fully filled in skill bar is.
print_skill_bars <- function(cv, out_of = 5, bar_color = "#969696", bar_background = "#d9d9d9", glue_template = "default"){

  if(glue_template == "default"){
    glue_template <- "
<div
  class = 'skill-bar'
  style = \"background:linear-gradient(to right,
                                      {bar_color} {width_percent}%,
                                      {bar_background} {width_percent}% 100%)\"
>{skill}</div>"
  }
  cv$skills %>%
    dplyr::mutate(width_percent = round(100*as.numeric(level)/out_of)) %>%
    glue::glue_data(glue_template) %>%
    print()

  invisible(cv)
}

print_skill_list <- function(cv, section_id){
  cv$skills %>% 
    # differentiate between technical and language skills (ALS)    
    dplyr::filter(in_resume == TRUE) %>%
    dplyr::filter(section == section_id) %>%
    # choose entries to add to CV (ALS)
    dplyr::group_by(section, .drop = TRUE) %>%
    dplyr::mutate(
      skills = paste0(glue::glue("  - • {skill}{ifelse(is.na(level), '', paste0(': ', level, '.'))}", .trim = FALSE), collapse = "\n"),
      section = dplyr::case_when(
        section == "language" ~ "<i class='fa fa-language'></i> Languages",
        section == "coding"   ~ "<i class='fa fa-terminal'></i> Coding skills",
        section == "comp_bio" ~ "<i class='fa fa-dna'></i> Computational Biology")) %>%
    dplyr::distinct(section, skills) %>%
    glue::glue_data(
      "## {{section}}\n",
      "{{skills}}\n", .trim = FALSE, .open = "{{", .close = "}}", sep = "\n"
    ) %>%
    print()
  
  invisible(cv)
}

#' @description List of all links in document labeled by their superscript integer.
print_links <- function(cv) {
  n_links <- length(cv$links)
  if (n_links > 0) {
    cat("
Links {data-icon=link}
--------------------------------------------------------------------------------

<br>


")

    purrr::walk2(cv$links, 1:n_links, function(link, index) {
      print(glue::glue('{index}. {link}'))
    })
  }

  invisible(cv)
}


#' @description Contact information section with icons
print_contact_info <- function(cv){
  glue::glue_data(
    cv$contact_info,
    "- <i class='{icon}'></i> {contact}"
  ) %>% print()

  invisible(cv)
}
