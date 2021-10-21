#'@description Prepare RKI data for the daily reports

# setup ------------------------------------------------------------------------

## load libraries
library(dplyr)

if (third_party == FALSE) {
  
  ## load data
  rkiCOVID_data <- readr::read_csv(paste0(user_input, "RKI_COVID19.csv"))
  
} else {
  
  library(data.table)
  
  rkiCOVID_data <-
    
    fread("https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.csv", encoding = "UTF-8")
  
}

## exclude NeuerFall -1
rkiCOVID_data <- rkiCOVID_data %>%
  filter(NeuerFall != -1)

#'@details laut Metdaten entpsricht die aktuelle Zahl der Fälle der summe aller 
#' Fälle bei denen NewCase != -1 ist, Quelle:
#' https://www.arcgis.com/home/item.html?id=dd4580c810204019a7b8eb3e0b329dd6

# functions --------------------------------------------------------------------

## helper funciton (as in the script for preparing octo_data)
remove.leading.NAs <- function(input_data) {
  
  input_data <- input_data %>% arrange(Datum)
  
  for (j in colnames(input_data)) {
    
    if (length(which(!is.na(input_data[, j]))) > 0) {
      
      ## replace leading NAs by 0
      firstNotNA <- min(which(!is.na(input_data[, j])))
      
      if (firstNotNA > 1) {
        input_data[1:(firstNotNA- 1), j] <- 0
      }
      
      ## replace NA by last observed value
      input_data[, j] <- input_data[, j] %>%
        zoo::na.locf()
      
    }
    
  }
  
  return(input_data)
  
}


# Count cases ------------------------------------------------------------------

rki_cases <- 
  
  rkiCOVID_data %>%
  mutate(Datum = as.Date(Meldedatum)) %>%
  group_by(Datum) %>%
  summarise(casesGermany = sum(AnzahlFall)) %>%
  
  
  full_join(
    
    rkiCOVID_data %>%
      filter(Landkreis == 'SK München') %>%
      mutate(Datum = as.Date(Meldedatum)) %>%
      group_by(Datum) %>%
      summarise(casesMunich = sum(AnzahlFall)),
    
    by = c("Datum") 
    
  ) %>%
  
  full_join(
    
    rkiCOVID_data %>%
      filter(Bundesland == 'Bayern') %>%
      mutate(Datum = as.Date(Meldedatum)) %>%
      group_by(Datum) %>%
      summarise(casesBavaria = sum(AnzahlFall)),
    
    by = c("Datum")
    
  ) %>%
  
  replace(is.na(.), 0) %>%
  mutate(
    casesGermanyKum = cumsum(casesGermany),
    casesMunichKum = cumsum(casesMunich),
    casesBavariaKum = cumsum(casesBavaria)
  )


# fatalities -------------------------------------------------------------------

rki_cases_dead <- 
  
  rkiCOVID_data %>%
  filter(NeuerTodesfall %in% c(0,1)) %>%
  mutate(Datum = as.Date(Meldedatum)) %>%
  group_by(Datum) %>%
  summarise(casesGermany_dead = sum(AnzahlTodesfall)) %>%
  
  
  full_join(
    
    rkiCOVID_data %>%
      filter(Landkreis == 'SK München',
             NeuerTodesfall %in% c(0,1)) %>%
      mutate(Datum = as.Date(Meldedatum)) %>%
      group_by(Datum) %>%
      summarise(casesMunich_dead = sum(AnzahlTodesfall)),
    
    by = c("Datum") 
    
  ) %>%
  
  full_join(
    
    rkiCOVID_data %>%
      filter(Bundesland == 'Bayern',
             NeuerTodesfall %in% c(0,1)) %>%
      mutate(Datum = as.Date(Meldedatum)) %>%
      group_by(Datum) %>%
      summarise(casesBavaria_dead = sum(AnzahlTodesfall)),
    
    by = c("Datum")
    
  ) %>%
  
  replace(is.na(.), 0) %>%
  mutate(
    casesGermanyKum_dead = cumsum(casesGermany_dead),
    casesMunichKum_dead = cumsum(casesMunich_dead),
    casesBavariaKum_dead = cumsum(casesBavaria_dead)
  )


# cured cases ------------------------------------------------------------------

rki_cases_cured <- 
  
  rkiCOVID_data %>%
  filter(Landkreis == 'SK München',
         NeuGenesen %in% c(0,1)) %>%
  mutate(Datum = as.Date(Meldedatum)) %>%
  group_by(Datum) %>%
  summarise(casesMunich_cured = sum(AnzahlGenesen)) %>%
  arrange(Datum) %>%
  mutate(
    casesMunichKum_cured = cumsum(casesMunich_cured)
  )


# combine data------------------------------------------------------------------

#'@details We combine the case count with the age groups data.

rki_cases_data <- rki_cases %>%
  
  full_join(
    rki_cases_dead,
    
    by = c('Datum')
    
  ) %>%
  
  full_join(
    rki_cases_cured,
    
    by = c('Datum')
  )


## not all dates from min to max are present
allDates <- as.Date(
  min(rki_cases_data$Datum):max(rki_cases_data$Datum),
  origin = '1970-01-01'
) %>% 
  as.data.frame() %>%
  magrittr::set_colnames("Datum")

rki_cases_data <- full_join(
  allDates, rki_cases_data, by = c("Datum")) %>%
  arrange(Datum) %>%
  remove.leading.NAs()

# save -------------------------------------------------------------------------

saveRDS(rki_cases_data,
        paste0(derived_data, 'rki_cases_data.rds'))

write.table(rki_cases_data,
            paste0(derived_data, 'rki_cases_data.csv'), 
            sep = ";",
            row.names = FALSE)

saveRDS(rki_cases_data,
        paste0(shared_data, 'rki_cases_data.rds'))

write.table(rki_cases_data,
            paste0(shared_data, 'rki_cases_data.csv'), 
            sep = ";",
            row.names = FALSE)

# archive ----------------------------------------------------------------------

## read archive
rki_7days_archive_prev <- 
  readRDS(paste0(derived_data, 'rki_7days_archive.rds'))

## update archive
rki_7days_archive <- 
  rbind(
  rki_7days_archive_prev,
  rki_cases_data %>%
    tail(7) %>%
    mutate(Datenstand = max(Datum),
           Session = Sys.time())
  )

## export archive
saveRDS(rki_7days_archive, paste0(derived_data, 'rki_7days_archive.rds'))
saveRDS(rki_7days_archive, paste0(shared_data, 'rki_7days_archive.rds'))
