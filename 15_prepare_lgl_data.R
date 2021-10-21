#'@description Reads html copies of LGL website and returns lgl data.


# load libraries ---------------------------------------------------------------

library(rvest)
library(tidyverse)

# utils ------------------------------------------------------------------------

clean_up <- function(x) {
  x <- stringr::str_replace_all(x, "[\r\t\n]", "")
  stringr::str_replace_all(x, "  ", "")
}

css <- '#tableLandkreise > caption:nth-child(1)'

# Get the new report files for a dataframe with a Dateiname column
get_new_reports <- function(report_list, data) {
  if (!("Dateiname" %in% names(data)))
    return(report_list)
  new_report_list <- list(Dateiname = report_list)
  new_report_list <- as.list(
    setdiff(
      tibble(Dateiname=new_report_list$Dateiname), 
      data %>% distinct(Dateiname)
    )
  )
  return(new_report_list)
}

# Get table from lgl html page 
get_table <- function(filename, table_name) {
  ## reset values
  headerText <- NULL
  timeStamp <- NULL
  lgl_data_temp <- NULL
  html <- NULL
  
  ## load data
  html <- read_html(paste0(lgl_input, filename))
  # parse ----------------------------------------------------------------------
  
  ## time stamp
  timeStamp_time <- html %>% 
    html_nodes(css) %>% 
    html_text() %>% 
    str_match(., "\\d\\d\\:\\d\\d Uhr")
  
  timeStamp_date <- html %>%
    html_nodes("#content_1c") %>%
    html_text() %>%
    str_match(., "\\d\\d\\.\\d\\d\\.\\d\\d\\d\\d")
  
  ## Get case table
  table_id <- html %>%
    html_nodes("table") %>%
    grep(table_name, .)
  
  table_data <- html %>%
    html_nodes("table") %>%
    .[table_id] %>%
    html_table(fill = TRUE) %>%
    as.data.frame() %>%
    mutate(Datum = timeStamp_date[[1]][1],
           Uhrzeit = timeStamp_time[[1]][1],
           Dateiname = filename)
  
  return(table_data)
}

# build lgl data ---------------------------------------------------------------

# all files ending with ".htm" or ".html"
all_reports_list <- list.files(lgl_input)
all_reports_list <- all_reports_list[grep("\\.html?$", all_reports_list)]

lgl_data <- readRDS(paste0(derived_data, "lgl_data.rds"))
lgl_data_hospitalizations <- readRDS(paste0(derived_data, "lgl_data_hospitalizations.rds"))
lgl_data_icu <- readRDS(paste0(derived_data, "lgl_data_icu.rds"))

for (filename in get_new_reports(all_reports_list, lgl_data)$Dateiname) {
  ## get the new table
  lgl_data_cases <- get_table(filename, "kreisfreien")
  
  ## append cases data
  lgl_data <- rbind(lgl_data_cases, lgl_data)
}

## Hospitalizations
for (filename in get_new_reports(
  all_reports_list, 
  lgl_data_hospitalizations)$Dateiname) {
  if (file.info(paste0(lgl_input, filename))$ctime > 
      as.POSIXct("2021-09-01 00:00:00")) {
    
    ## get the new table
    lgl_data_hospitalizations_tmp <- get_table(
      filename, "Hospitalisierungen und schwere Verläufe")
    
    print(lgl_data_hospitalizations_tmp)
    
    ## append cases data
    lgl_data_hospitalizations <- rbind(lgl_data_hospitalizations_tmp, lgl_data_hospitalizations)
  }
}

## ICU
for (filename in get_new_reports(all_reports_list, lgl_data_icu)$Dateiname) {
  if (file.info(paste0(lgl_input, filename))$ctime > 
      as.POSIXct("2021-09-01 00:00:00")) {
    
    ## get the new table
    lgl_data_icu_tmp <- get_table(
      filename, "Intensivmedizinische Versorgungskapazitäten")
    
    colnames(lgl_data_icu_tmp)[1] <- gsub(
      'Intensivmedizinische.', '', colnames(lgl_data_icu_tmp)[1])
    
    if (nrow(lgl_data_icu_tmp) == 0) {
      lgl_data_icu_tmp <- get_table(
        filename, "Stationäre Versorgungskapazitäten")
      
      colnames(lgl_data_icu_tmp)[1] <- gsub(
        'Stationäre.', '', colnames(lgl_data_icu_tmp)[1])
    }
    
    ## append cases data
    lgl_data_icu <- rbind(lgl_data_icu_tmp, lgl_data_icu)
  }
}


# Exports for SAE --------------------------------------------------------------

## reshape data for Branddirektion
lgl_branddir_tab <- lgl_data_hospitalizations %>%
  group_by(Datum) %>%
  filter(Datum == max(Datum)) %>%
  ungroup() %>%
  mutate(KPI = gsub("1)", "", Hospitalisierungen.und.schwere.Verläufe)) %>%
  mutate(KPI = gsub("2)", "", KPI)) %>%
  mutate(KPI = gsub("\\(|\\)", "", KPI)) %>%
  mutate(KPI = gsub("Tages", "Tage", KPI)) %>%
  mutate(Tagesaktueller.Wert = gsub(",", ".", Tagesaktueller.Wert)) %>%
  dplyr::select(KPI, Tagesaktueller.Wert, Datum) %>%
  unique() %>%
  mutate(Tagesaktueller.Wert = as.numeric(as.character(Tagesaktueller.Wert))) %>%
  pivot_wider(names_from = KPI, values_from = Tagesaktueller.Wert) %>%
  full_join(
    .,
    lgl_data_icu %>%
      group_by(Datum) %>%
      filter(Datum == max(Datum)) %>%
      ungroup() %>%
      filter(grepl("Belegung der Intensiv", Versorgungskapazitäten)) %>% 
      mutate(ICU_Belegung = as.numeric(as.character(Tagesaktueller.Wert))) %>%
      dplyr::select(ICU_Belegung, Datum) %>%
      unique() %>%
      arrange(Datum),
    by = c("Datum")
  ) %>%
  mutate(date = as.Date(Datum, format = "%d.%m.%Y")) %>%
  arrange(date) %>%
  dplyr::select(-date)

## exoprt to BFM
try(
  write.table(
    lgl_branddir_tab,
    paste0(brand_data, 'lgl_KPIs.csv'),
    sep = ";",
    row.names = FALSE,
    fileEncoding="UTF-8",
    dec = ",")
  , silent = TRUE
)

# General Exports --------------------------------------------------------------

# export
saveRDS(lgl_data_hospitalizations, paste0(derived_data, "lgl_data_hospitalizations.rds"))
saveRDS(lgl_data_hospitalizations, paste0(shared_data, "lgl_data_hospitalizations.rds"))

saveRDS(lgl_data_icu, paste0(derived_data, "lgl_data_icu.rds"))
saveRDS(lgl_data_icu, paste0(shared_data, "lgl_data_icu.rds"))

saveRDS(lgl_data, paste0(derived_data, "lgl_data.rds"))
saveRDS(lgl_data, paste0(shared_data, "lgl_data.rds"))

saveRDS(lgl_branddir_tab, paste0(derived_data, "lgl_branddir_tab.rds"))
saveRDS(lgl_branddir_tab, paste0(shared_data, "lgl_branddir_tab.rds"))
