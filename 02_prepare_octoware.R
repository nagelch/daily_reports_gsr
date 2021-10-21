
# setup ------------------------------------------------------------------------

library(readr)
library(dplyr)

# functions --------------------------------------------------------------------

#'@details removes leading NAs by the either inserting 0 (if NAs occur
#'before the first value is observed) or the latest observed value before the
#'NA value.

#'@param input_data [data.frame] The data set with the values to be replaced

remove.leading.NAs <- function(input_data) {
  
  input_data <- input_data %>% arrange(Datum)
  
  for (j in colnames(input_data)) {
    
    ## replace leading NAs by 0
    firstNotNA <- min(which(!is.na(input_data[, j])))
    if (firstNotNA > 1 & firstNotNA != Inf) {
      input_data[1:(firstNotNA- 1), j] <- 0
    }
    # when they are all NA
    if (firstNotNA == Inf) {
      input_data[, j] <- 0
    }
    
    ## replace NA by last observed value
    input_data[, j] <- input_data[, j] %>%
      zoo::na.locf()
    
  }
  
  return(input_data)
  
}

# set of relevant columns ------------------------------------------------------

spaltennamen_unverzichtbar <- c(
  "ID",
  "Erstmeldung",
  "Tod",
  "Sterbedatum",
  "hospitalisiert",
  "aktuell_hospitalisiert",
  "Hospitalisierung_Meldedatum",
  "Hospitalisierung_Krankenhaus_Aufnahme",
  "Hospitalisierung_Krankenhaus_Entlassung",
  "Hospitalisierung_Krankenhaus_ITS",
  "Hospitalisierung_Krankenhaus_ITS_bis",
  "Hospitalisierung_Krankenhaus_ITS_von",
  "Merkmal_DatumLetzterImpfung",
  "Merkmal_Impfstoff",
  "Alter_Erstmeldung",
  "Person_Geschlecht",
  "WohnAnschrift_PLZ",
  "WohnAnschrift_Ort",
  "WohnAnschrift_OT",
  "Merkmal_OnsetOfDisease",
  "Merkmal_HalsschmerzenEntzuendung",
  "Merkmal_Husten",
  "Merkmal_Pneumonie",
  "Merkmal_Schnupfen",
  "Merkmal_SchwereARDS",
  "Merkmal_BeatmungspflAtemwegserkr",
  "Merkmal_Dyspnoe",
  "Merkmal_Fieber",
  "Merkmal_allgemeine_Krankheitszeichen",
  "Merkmal_Durchfall_nicht_naeher_bezeichnet",
  "Merkmal_Geruchsverlust",
  "Merkmal_Geschmacksverlust",
  "Merkmal_Tachypnoe",
  "Merkmal_Tachykardie",
  "Merkmal_SymptomfreiheitPatient",
  "Merkmal_SymptomfreiheitGA",
  "letzter_Infektionsort",
  "Merkmal_StatusPatientSetting",
  "Merkmal_VirusVariante_Mutation",
  "Merkmal_Details_VirusVariante",
  "Merkmal_AnzahlImpfungen",
  "letzter_Infektionsort_Art",
  "Meldekreis",
  "Vorgangsnummer",
  "Anzahl_Kontaktpersonen",
  "Merkmal_JemalsGeimpft"
)


# Read data --------------------------------------------------------------------

## Do we have complete data available?
complete_data_available <- FALSE

if (complete_data_available == TRUE) {

  ## read data (complete)
  octo_data <-
    readxl::read_excel(
      paste0(user_input, "octoware.xlsx"),
      guess_max = 100000
    )
  
  
  ## move ID column to the front
  id_pos <- which(colnames(octo_data) == "ID")
  
  if (length(id_pos) == 1) {
    x <- "ID"
    octo_data <- octo_data[, c(x, setdiff(names(octo_data), x))]
    
  } else {
    
    ## assert first column is ID (sometimes labeld as 'rur' for some reason)
    if ((colnames(octo_data)[1] == "rur")) {colnames(octo_data)[1] <- "ID"} 
    
  }
  
  
  octo_data <- octo_data %>%
    rename(Erstmeldung = Meldedatum) %>% 
    rename(letzter_Infektionsort = letzte_Exposition) %>% 
    rename(letzter_Infektionsort_Art = letzte_Exposition_Art) %>% 
    mutate(Merkmal_StatusPatientSetting = "",
           Datum = as.Date(Erstmeldung, format = "%d.%m.%Y"))
  
  
} else {
  
  ## read data (complete)
  octo_data <- readRDS(paste0(derived_data,"octo_data_history.rds"))
  octo_data <- octo_data %>% filter(Erstmeldung <= Sys.Date())

}

#'@param octo_substitute_check, boolean, did we compose octo_data from
#'octo_data and octo_data_recent? - yes or no. The boolean is used
#'in the 'combine data' section of the script to truncate the data
#'(remove the last date) correctly.

octo_substitute_check <- FALSE
octo_data <- octo_data %>% dplyr::select(all_of(spaltennamen_unverzichtbar))

if (file.exists(paste0(user_input, "octoware_kurz.xlsx"))) {
  
  ## read data (recent)
  octo_data_recent <-
    readxl::read_excel(
      paste0(user_input, "octoware_kurz.xlsx"),
      guess_max = 100000
    )
  
  ## prepare variable names and date
  octo_data <- octo_data %>%
    mutate(Datum = as.Date(Erstmeldung, format = "%d.%m.%Y"))
  
  octo_data_recent <- octo_data_recent %>% 
    rename(Erstmeldung = Meldedatum) %>% 
    rename(letzter_Infektionsort = letzte_Exposition) %>% 
    rename(letzter_Infektionsort_Art = letzte_Exposition_Art) %>% 
    mutate(Merkmal_StatusPatientSetting = "")
  
  octo_data_recent <- octo_data_recent %>%
    mutate(Datum  = as.Date(Erstmeldung, format = "%d.%m.%Y"))
  
  ## select columns
  octo_data_recent <- octo_data_recent %>% dplyr::select(colnames(octo_data))
  
  
  if (max(octo_data$Datum) <= max(octo_data_recent$Datum)) {
    
    octo_substitute_check <- TRUE
    
    ## join data
    octo_data <- rbind(
      octo_data_recent,
      octo_data %>%
        filter(Datum < min(octo_data_recent$Datum))
    )
    
    ## remove 'Datum' because it will be generated again subsequently
    octo_data$Datum <- NULL
    
  }
  
}

# Dublettenentfernung
octo_data <- octo_data %>% distinct(ID, .keep_all = TRUE)

# quick fix: divers to NA, since it breaks our tables
octo_data$Person_Geschlecht[
  which(octo_data$Person_Geschlecht == "divers")] <- NA

saveRDS(octo_data, paste0(derived_data,"octo_data_history.rds"))

# Fatalities  ------------------------------------------------------------------

## read data pertaining to fatalities 
date_temp_1 <- file.info(paste0(user_input, "octoware_tote.xlsx"))$ctime %>%
  as.Date() 

date_temp_2 <- file.info(paste0(user_input, "octoware_kurz.xlsx"))$ctime %>%
  as.Date()   

octo_data_fatalities <-
  readxl::read_excel(
    paste0(user_input, "octoware_tote.xlsx"),
    guess_max = 100000
  )

octo_data_fatalities <- octo_data_fatalities %>% 
  rename(Erstmeldung = Meldedatum) %>% 
  rename(letzter_Infektionsort = letzte_Exposition) %>% 
  rename(letzter_Infektionsort_Art = letzte_Exposition_Art) %>% 
  mutate(Merkmal_StatusPatientSetting = "")


# Fälle in München -------------------------------------------------------------

cases_summary_data <-
  
  ## casesMunich_Inz
  octo_data %>%
  mutate(Datum  =
           as.Date(Erstmeldung, format = "%d.%m.%Y")) %>%
  group_by(Datum) %>%
  summarise(casesMunich_Inz = n()) %>%
  arrange(Datum) %>%
  
  full_join(
    
    ## casesMunich
    octo_data %>%
      mutate(Datum  =
               as.Date(Erstmeldung, format = "%d.%m.%Y")) %>%
      group_by(Datum) %>%
      summarise(n = n()) %>%
      arrange(Datum) %>%
      mutate(casesMunich = cumsum(n)) %>%
      dplyr::select(Datum, casesMunich)
    ,
    by = c("Datum")
    
  ) %>%
  
  full_join(
    
    ## casesMunich_dead
    octo_data_fatalities %>%
      filter(Tod == "ja") %>%
      mutate(Datum  =
               as.Date(Sterbedatum, format = "%Y-%m-%d")) %>%
      filter(!is.na(Datum)) %>%
      group_by(Datum) %>%
      summarise(n = n()) %>%
      arrange(Datum) %>%
      mutate(casesMunich_dead = cumsum(n)) %>%
      dplyr::select(Datum, casesMunich_dead)
    ,
    by = c("Datum")
    
  ) %>%
  
  full_join(
    
    ## casesMunich_stat
    octo_data %>%
      filter(hospitalisiert == "ja") %>%
      mutate(Datum  =
               as.Date(Hospitalisierung_Meldedatum, format = "%d.%m.%Y")) %>%
      group_by(Datum) %>%
      summarise(n = n()) %>%
      arrange(Datum) %>%
      mutate(casesMunich_stat = cumsum(n)) %>%
      dplyr::select(Datum, casesMunich_stat)
    ,
    by = c("Datum")
    
  ) %>%
  
  full_join(
    
    ## casesMunich_amb
    octo_data %>%
      filter(hospitalisiert == "nein") %>%
      mutate(Datum  =
               as.Date(Erstmeldung, format = "%d.%m.%Y")) %>%
      group_by(Datum) %>%
      summarise(n = n()) %>%
      arrange(Datum) %>%
      mutate(casesMunich_amb = cumsum(n)) %>%
      dplyr::select(Datum, casesMunich_amb),
    by = c("Datum")
    
  ) %>%
  arrange(Datum) %>%
  remove.leading.NAs() %>%
  ## filter for plausible dates
  filter(Datum < Sys.Date(),
         Datum >= "2019-11-01")

# Altersgruppen ----------------------------------------------------------------

## Alter Erstmeldung
Alter_Erstmeldung_var <- round(
  mean(as.numeric(octo_data$Alter_Erstmeldung), na.rm = T), 1)

Alter_Erstmeldung_prev <- round(
  mean(as.numeric(
    subset(octo_data, Erstmeldung == Sys.Date()-1)$Alter_Erstmeldung), 
    na.rm = TRUE), 1)

## prepare grouping variables (categories instead of integers)
octo_data_ageGroups <- octo_data %>%
  mutate(Datum  =
           as.Date(Erstmeldung, format = "%d.%m.%Y")) %>%
  mutate(
    age_0to5_sum  =
      as.numeric((findInterval(Alter_Erstmeldung, c(0, 6)) == 1)),
    age_6to10_sum =
      as.numeric((findInterval(Alter_Erstmeldung, c(6, 11)) == 1)),
    age_11to20_sum =
      as.numeric((findInterval(Alter_Erstmeldung, c(11, 21)) == 1)),
    age_21to40_sum =
      as.numeric((findInterval(Alter_Erstmeldung, c(21, 41)) == 1)),
    age_41to60_sum =
      as.numeric((findInterval(Alter_Erstmeldung, c(41, 61)) == 1)),
    age_61to80_sum =
      as.numeric((findInterval(Alter_Erstmeldung, c(61, 81)) == 1)),
    age_81plus_sum =
      as.numeric((findInterval(Alter_Erstmeldung, c(81, 150)) == 1)),
    age_unknown_sum = as.numeric(is.na(Alter_Erstmeldung))
    
  ) %>%
  arrange(Datum)


## select variabels for the dashboard team from Branddirektion
#'@details We trim the raw data to the most relevant variables.
branddir_vars <- c(
  "Person_Geschlecht",
  "WohnAnschrift_PLZ", 
  "WohnAnschrift_Ort", 
  "WohnAnschrift_OT",
  "Erstmeldung",
  "Alter_Erstmeldung",
  "age_0to5_sum",
  "age_6to10_sum",
  "age_11to20_sum",
  "age_21to40_sum",
  "age_41to60_sum",
  "age_61to80_sum",
  "age_81plus_sum",
  "ID")

octo_branddir <- octo_data_ageGroups[, branddir_vars]


# Altersverteilung berechnen
agegroups <- c(
  "age_0to5",
  "age_6to10",
  "age_11to20",
  "age_21to40",
  "age_41to60",
  "age_61to80",
  "age_81plus",
  "age_unknown"
)

octo_agegroups_plain <- octo_data %>%
  mutate(Datum = as.Date(Erstmeldung, format = "%d.%m.%Y")) %>%
  # Altersgruppen finden
  mutate(
    Altersgruppe_index = findInterval(
      Alter_Erstmeldung, 
      c(0, 6, 11, 21, 41, 61, 81, 150)
    )
  ) %>% 
  mutate(Altersgruppe = agegroups[Altersgruppe_index]) %>%
  mutate(Altersgruppe = replace_na(Altersgruppe, "age_unknown")) %>%
  # Geschlecht vorbereiten, damit es für _master.Rmd passt
  mutate(Person_Geschlecht = replace_na(Person_Geschlecht, "unbekannt")) %>%
  mutate(Person_Geschlecht = str_sub(Person_Geschlecht, 1, 1)) %>%
  mutate(Person_Geschlecht = replace(Person_Geschlecht, str_ends(Person_Geschlecht, "u"), "na")) %>%
  # Zählen, wie viel es von jeder Kombination aus Datum, Altersgruppe und Geschlecht gibt (Neuinfektionen)
  group_by(Datum, Altersgruppe, Person_Geschlecht) %>%
  summarise(n = n()) %>%
  # Dummy-Datum mit 0 Fällen, um sicher zu gehen, dass alle Spalten angelegt werden
  bind_rows(
    crossing(
      Datum=as.Date("2020-01-28"), 
      Altersgruppe=agegroups, 
      Person_Geschlecht=c("m", "w", "na"), 
      n=0
    )
  )

octo_agegroups_fatalities <- octo_data_fatalities %>% 
  filter(Tod == "ja") %>%
  mutate(Datum= as.Date(Sterbedatum, format = "%Y-%m-%d")) %>%
  filter(!is.na(Datum)) %>%
  # Altersgruppen finden
  mutate(
    Altersgruppe_index = findInterval(
      Alter_Erstmeldung, 
      c(0, 6, 11, 21, 41, 61, 81, 150)
    )
  ) %>% 
  mutate(Altersgruppe = agegroups[Altersgruppe_index]) %>%
  mutate(Altersgruppe = replace_na(Altersgruppe, "age_unknown")) %>%
  # Geschlecht vorbereiten, damit es für _master.Rmd passt
  mutate(Person_Geschlecht = replace_na(Person_Geschlecht, "unbekannt")) %>%
  mutate(Person_Geschlecht = str_sub(Person_Geschlecht, 1, 1)) %>%
  mutate(Person_Geschlecht = replace(Person_Geschlecht, str_ends(Person_Geschlecht, "u"), "na")) %>%
  # Zählen, wie viel es von jeder Kombination aus Datum, Altersgruppe und Geschlecht gibt (Neutodesfälle)
  group_by(Datum, Altersgruppe, Person_Geschlecht) %>%
  summarise(n = n()) %>%
  # Dummy-Datum mit 0 Fällen, um sicher zu gehen, dass alle Spalten angelegt werden
  bind_rows(
    crossing(
      Datum=as.Date("2020-01-28"), 
      Altersgruppe=agegroups, 
      Person_Geschlecht=c("m", "w", "na"), 
      n=0
    )
  )

octo_agegroups <- octo_agegroups_plain %>%
  # Kumulierte Summe der Fälle nach Altersgruppe und Geschlecht
  group_by(Altersgruppe, Person_Geschlecht) %>%
  arrange(Datum) %>%
  mutate(cumsum_n = cumsum(n)) %>%
  unite("Altersgruppe_geschlecht", c(Altersgruppe, Person_Geschlecht)) %>%
  dplyr::select(Datum, Altersgruppe_geschlecht, cumsum_n) %>% 
  spread(Altersgruppe_geschlecht, cumsum_n) %>% 
  arrange(Datum) %>%
  # LOCF
  fill(everything()) %>% 
  mutate(age_0to5_sum = rowSums(dplyr::select(., starts_with("age_0to5_")))) %>% 
  mutate(age_6to10_sum = rowSums(dplyr::select(., starts_with("age_6to10_")))) %>% 
  mutate(age_11to20_sum = rowSums(dplyr::select(., starts_with("age_11to20_")))) %>% 
  mutate(age_21to40_sum = rowSums(dplyr::select(., starts_with("age_21to40_")))) %>% 
  mutate(age_41to60_sum = rowSums(dplyr::select(., starts_with("age_41to60_")))) %>% 
  mutate(age_61to80_sum = rowSums(dplyr::select(., starts_with("age_61to80_")))) %>% 
  mutate(age_81plus_sum = rowSums(dplyr::select(., starts_with("age_81plus_")))) %>% 
  mutate(age_unknown_sum = rowSums(dplyr::select(., starts_with("age_unknown_")))) %>% 
  full_join(
    # Kumulierte Summe der Todesfälle nach Altersgruppe und Geschlecht
    octo_agegroups_fatalities %>%
      group_by(Altersgruppe, Person_Geschlecht) %>%
      arrange(Datum) %>%
      mutate(cumsum_n = cumsum(n)) %>%
      unite("Altersgruppe_geschlecht", c(Altersgruppe, Person_Geschlecht)) %>%
      mutate(Altersgruppe_geschlecht = str_glue("{Altersgruppe_geschlecht}_dead")) %>%
      dplyr::select(Datum, Altersgruppe_geschlecht, cumsum_n) %>%
      spread(Altersgruppe_geschlecht, cumsum_n) %>%
      arrange(Datum) %>%
      # LOCF
      fill(everything()) %>% 
      mutate(age_0to5_sum_dead = rowSums(dplyr::select(., starts_with("age_0to5_")))) %>% 
      mutate(age_6to10_sum_dead = rowSums(dplyr::select(., starts_with("age_6to10_")))) %>% 
      mutate(age_11to20_sum_dead = rowSums(dplyr::select(., starts_with("age_11to20_")))) %>% 
      mutate(age_21to40_sum_dead = rowSums(dplyr::select(., starts_with("age_21to40_")))) %>% 
      mutate(age_41to60_sum_dead = rowSums(dplyr::select(., starts_with("age_41to60_")))) %>% 
      mutate(age_61to80_sum_dead = rowSums(dplyr::select(., starts_with("age_61to80_")))) %>% 
      mutate(age_81plus_sum_dead = rowSums(dplyr::select(., starts_with("age_81plus_")))) %>% 
      mutate(age_unknown_sum_dead = rowSums(dplyr::select(., starts_with("age_unknown_"))))
  ) %>%
  arrange(Datum) %>%
  fill(everything()) %>% 
  full_join(
    # Neuinfektionen nach Altersgruppe und Geschlecht
    octo_agegroups_plain %>%
      unite("Altersgruppe_geschlecht", c(Altersgruppe, Person_Geschlecht)) %>%
      mutate(Altersgruppe_geschlecht = str_glue("{Altersgruppe_geschlecht}_new")) %>%
      dplyr::select(Datum, Altersgruppe_geschlecht, n) %>%
      mutate(n = replace_na(n, 0)) %>%
      spread(Altersgruppe_geschlecht, n) %>%
      mutate(across(everything(), replace_na, 0)) %>%
      ungroup() %>%
      mutate(age_0to5_sum_new = rowSums(dplyr::select(., starts_with("age_0to5_")))) %>%
      mutate(age_6to10_sum_new = rowSums(dplyr::select(., starts_with("age_6to10_")))) %>%
      mutate(age_11to20_sum_new = rowSums(dplyr::select(., starts_with("age_11to20_")))) %>%
      mutate(age_21to40_sum_new = rowSums(dplyr::select(., starts_with("age_21to40_")))) %>%
      mutate(age_41to60_sum_new = rowSums(dplyr::select(., starts_with("age_41to60_")))) %>%
      mutate(age_61to80_sum_new = rowSums(dplyr::select(., starts_with("age_61to80_")))) %>%
      mutate(age_81plus_sum_new = rowSums(dplyr::select(., starts_with("age_81plus_")))) %>%
      mutate(age_unknown_sum_new = rowSums(dplyr::select(., starts_with("age_unknown_"))))
  )

# Infektionsort ----------------------------------------------------------------

Infektionsort_tab <- octo_data %>%
  mutate(Datum  = as.Date(Erstmeldung, format = "%d.%m.%Y")) %>%
  filter(Datum < Sys.Date()) %>%
  filter(Datum > max(Datum)-14) %>%
  group_by(letzter_Infektionsort_Art) %>%
  summarise(Anzahl = n(), .groups = 'drop') %>%
  arrange(startsWith(letzter_Infektionsort_Art, "sonst"), desc(Anzahl)) %>%
  replace_na(list(letzter_Infektionsort_Art = "Unbekannt")) %>%
  magrittr::set_colnames(c('Infektionsort', 'akut Infizierte'))


# Reiserückkehrer --------------------------------------------------------------

#'@description We want to filter all the "letzte_Infektionsort" names in 
#'ocotoware such that we get a list of non-German locations. Who got infected
#'abroad? Towards this end, we create a huge look up list with German 
#'locations.

## list of all locations in Germany
plz_names_data <- read.table(
  paste0(intern_input, 'zuordnung_plz_ort_landkreis.csv'),
  sep = ',', fileEncoding = "UTF-8", header = TRUE)

## prepare strings for matching
plz_names_data <- plz_names_data %>%
  dplyr::select(ort, landkreis, bundesland) %>%
  mutate(ort = as.character(ort)) %>%
  mutate(landkreis = as.character(landkreis)) %>%
  mutate(bundesland = as.character(bundesland)) %>%
  mutate_all(~ (gsub("[[:punct:]]", "", .))) %>% 
  mutate_all(~ (toupper(.))) %>%
  mutate_all(~ (gsub(" ", "", .))) %>%
  mutate_all(~ (gsub("LANDKREIS", "", .))) %>%
  mutate_all(~ (gsub("STÄDTEREGION", "", .)))

meldekreis_data <- octo_data %>%
  dplyr::select(Meldekreis) %>%
  mutate(Meldekreis = as.character(Meldekreis)) %>%
  mutate_all(~ (gsub("[[:punct:]]", "", .))) %>% 
  mutate_all(~ (toupper(.))) %>%
  mutate_all(~ (gsub(" ", "", .))) %>%
  mutate_all(~ (gsub("LANDKREIS", "", .))) %>%
  mutate_all(~ (gsub("STÄDTEREGION", "", .)))

all_locations_list <- c(
  plz_names_data$landkreis[!duplicated(plz_names_data$landkreis)],
  plz_names_data$ort[!duplicated(plz_names_data$ort)],
  plz_names_data$bundesland[!duplicated(plz_names_data$bundesland)],
  meldekreis_data$Meldekreis[!duplicated(meldekreis_data$Meldekreis)],
  c("DEUTSCHLAND")) %>%
  .[!duplicated(.)]


octo_data$location <- octo_data %>%
  dplyr::select(letzter_Infektionsort) %>%
  mutate(letzter_Infektionsort = as.character(letzter_Infektionsort)) %>%
  mutate_all(~ (gsub("[[:punct:]]", "", .))) %>% 
  mutate_all(~ (toupper(.))) %>%
  mutate_all(~ (gsub(" ", "", .))) %>%
  mutate_all(~ (gsub("LANDKREIS", "", .))) %>%
  mutate_all(~ (gsub("STÄDTEREGION", "", .))) %>%
  mutate_all(~ (gsub("KREISFREIESTADT", "", .))) %>%
  mutate_all(~ (gsub("KREISFREIELANDESHAUPTSTADT", "", .))) %>%
  mutate(letzter_Infektionsort = toupper(letzter_Infektionsort)) %>%
  as.data.frame() %>%
  .[, "letzter_Infektionsort"]

## filter
ExpLand_recent_tab <- octo_data %>%
  mutate(Datum  = as.Date(Erstmeldung, format = "%d.%m.%Y")) %>%
  filter(Datum < Sys.Date()) %>%
  filter(Datum > max(Datum)-14) %>%
  filter(!(location %in% all_locations_list)) %>%
  filter(stringr::str_detect(letzter_Infektionsort, "[[:punct:]]") == FALSE) %>%
  filter(letzter_Infektionsort != "<unbekannt>") %>%
  group_by(letzter_Infektionsort) %>%
  summarise(location_infected = n(), .groups = 'drop') %>%
  drop_na() %>%
  magrittr::set_colnames(c('Letzter Infektionsort',
                           'akut Infizierte')) %>%
  arrange(desc(`akut Infizierte`))

ExpLand_new_tab <- octo_data %>%
  mutate(Datum  = as.Date(Erstmeldung, format = "%d.%m.%Y")) %>%
  filter(Datum < Sys.Date()) %>%
  filter(Datum == max(Datum)) %>%
  filter(!(location %in% all_locations_list)) %>%
  filter(stringr::str_detect(letzter_Infektionsort, "[[:punct:]]") == FALSE) %>%
  filter(letzter_Infektionsort != "<unbekannt>") %>%
  group_by(letzter_Infektionsort) %>%
  summarise(location_infected = n(), .groups = 'drop') %>%
  drop_na() %>%
  magrittr::set_colnames(c('Letzter Infektionsort',
                           'neu Infizierte')) %>%
  arrange(desc(`neu Infizierte`))


# Plausibility -----------------------------------------------------------------

## check
sumThose1 <-
  
  c("age_0to5_sum",
    "age_6to10_sum",
    "age_11to20_sum",
    "age_21to40_sum",
    "age_41to60_sum",
    "age_61to80_sum",
    "age_81plus_sum",
    "age_unknown_sum")

sumThose2 <-
  
  c("age_0to5_m",
    "age_6to10_m",
    "age_11to20_m",
    "age_21to40_m",
    "age_41to60_m",
    "age_61to80_m",
    "age_81plus_m",
    "age_unknown_m")


sumThose3 <-
  c("age_0to5_w",
    "age_6to10_w",
    "age_11to20_w",
    "age_21to40_w",
    "age_41to60_w",
    "age_61to80_w",
    "age_81plus_w",
    "age_unknown_w")


sumThose4 <-
  c("age_0to5_na",
    "age_6to10_na",
    "age_11to20_na",
    "age_21to40_na",
    "age_41to60_na",
    "age_61to80_na",
    "age_81plus_na",
    "age_unknown_na")

## Note that there is 1 case missing in the male age groups!
sum(octo_agegroups[nrow(octo_agegroups),sumThose1])
sum(octo_agegroups[nrow(octo_agegroups),sumThose2])
sum(octo_agegroups[nrow(octo_agegroups),sumThose3])
sum(octo_agegroups[nrow(octo_agegroups),sumThose4])


# combine data -----------------------------------------------------------------

octoware_summary_data <- full_join(
  cases_summary_data, octo_agegroups, by = c("Datum"))

## not all dates from min to max are present
allDates <- as.Date(
  min(octoware_summary_data$Datum):max(octoware_summary_data$Datum),
  origin = '1970-01-01'
) %>%
  as.data.frame() %>%
  magrittr::set_colnames("Datum")

octo_complete <- full_join(allDates, octoware_summary_data, by = c("Datum"))

## replace missing values
octo_complete <- remove.leading.NAs(octo_complete)

## remove today's date (ongoing entries in the productive environment)
if (octo_substitute_check == FALSE) {
  octo_complete <- octo_complete %>%
    filter(Datum < Sys.Date())
} else if (octo_substitute_check == TRUE) {
  
  octo_complete <- octo_complete %>%
    filter(Datum < Sys.Date())
  
}


## selection for branddirketion
octo_branddir_summary <- 
  octo_complete[, !(colnames(octo_complete) %in% 
                      c("casesMunich_Inz",
                        "casesMunich_stat", 
                        "casesMunich_amb"))]

## add incident cases of dead people
octo_branddir_summary$casesMunich_dead_Inz <- 
  c(0, diff(octo_branddir_summary$casesMunich_dead))


# Altersgruppen im Zeitverlauf -------------------------------------------------

## mean age per week
mean_age_per_week <- octo_data %>%
  mutate(Datum  =
           as.Date(Erstmeldung, format = "%d.%m.%Y")) %>%
  filter(Datum <= max(octo_complete$Datum)) %>%
  ## total
  mutate(week = lubridate::isoweek(Datum)) %>%
  mutate(year = lubridate::isoyear(Datum)) %>%
  group_by(year, week) %>%
  summarise(meanAge = mean(as.numeric(Alter_Erstmeldung), na.rm = TRUE) %>%
              round(., digits = 2),
            .groups = 'drop_last')

## mean age per day
mean_age_per_date <- octo_data %>%
  mutate(Datum  =
           as.Date(Erstmeldung, format = "%d.%m.%Y")) %>%
  filter(Datum <= max(octo_complete$Datum)) %>%
  group_by(Datum) %>%
  summarise(meanAge = mean(as.numeric(Alter_Erstmeldung), na.rm = TRUE) %>%
              round(., digits = 2),
            .groups = 'drop_last') 


# Patientensetting -------------------------------------------------------------

## aggregate data (recent)
StatusPS_recent <-
  octo_data %>%
  mutate(Datum  = as.Date(Erstmeldung, format = "%d.%m.%Y")) %>%
  filter(Datum < Sys.Date()) %>%
  filter(Datum > max(Datum)-14) %>%
  group_by(Merkmal_StatusPatientSetting) %>%
  summarise(PatientenSetting_recent = n(), .groups = 'drop')

## aggregate data (total)
StatusPS_total <- 
  octo_data %>%
  mutate(Datum  = as.Date(Erstmeldung, format = "%d.%m.%Y")) %>%
  filter(Datum < Sys.Date()) %>%
  group_by(Merkmal_StatusPatientSetting) %>%
  summarise(PatientenSetting_total = n(), .groups = 'drop')

## join tables
StatusPS_joined <- merge(StatusPS_total,
                         StatusPS_recent, 
                         by = c('Merkmal_StatusPatientSetting'),
                         all = TRUE)

StatusPS_joined <- rbind(
  StatusPS_joined %>%
    filter(!(Merkmal_StatusPatientSetting %in% 
               c('ohne Bezug zu §§23, 33, 36, 42 IfSG',
                 'nicht erhoben',
                 'nicht ermittelbar'))),
  
  StatusPS_joined %>%
    filter(Merkmal_StatusPatientSetting %in% 
             c('ohne Bezug zu §§23, 33, 36, 42 IfSG',
               'nicht erhoben',
               'nicht ermittelbar')) %>%
    
    .[match( c('nicht ermittelbar',
               'ohne Bezug zu §§23, 33, 36, 42 IfSG',
               'nicht erhoben'), .$Merkmal_StatusPatientSetting),]
)

## replace NAs by 0 
StatusPS_joined[is.na(StatusPS_joined)] <- 0

## relabel values
colnames(StatusPS_joined) <- c('Einrichtung', 
                               'kumulativ', 
                               'davon akut Infizierte')
rownames(StatusPS_joined) <- NULL

# Nowcast data -----------------------------------------------------------------

#'@description Perpare ocot_data for nowcast and R(t)

## prepare data
dat_mod_temp <- octo_data %>%
  dplyr::select(Erstmeldung, 
                Merkmal_OnsetOfDisease, 
                Alter_Erstmeldung,
                Merkmal_HalsschmerzenEntzuendung,
                Merkmal_Husten,
                Merkmal_Pneumonie,
                Merkmal_Schnupfen,
                Merkmal_SchwereARDS,
                Merkmal_BeatmungspflAtemwegserkr,
                Merkmal_Dyspnoe,
                Merkmal_Fieber,
                Merkmal_allgemeine_Krankheitszeichen,
                Merkmal_Durchfall_nicht_naeher_bezeichnet,
                Merkmal_Geruchsverlust,
                Merkmal_Geschmacksverlust,
                Merkmal_Tachypnoe,
                Merkmal_Tachykardie,
                Merkmal_SymptomfreiheitPatient,
                Merkmal_SymptomfreiheitGA)

merkmale_list <-  c('Merkmal_HalsschmerzenEntzuendung',
                    'Merkmal_Husten',
                    'Merkmal_Pneumonie',
                    'Merkmal_Schnupfen',
                    'Merkmal_SchwereARDS',
                    'Merkmal_BeatmungspflAtemwegserkr',
                    'Merkmal_Dyspnoe',
                    'Merkmal_Fieber',
                    'Merkmal_allgemeine_Krankheitszeichen',
                    'Merkmal_Durchfall_nicht_naeher_bezeichnet',
                    'Merkmal_Geruchsverlust',
                    'Merkmal_Geschmacksverlust',
                    'Merkmal_Tachypnoe',
                    'Merkmal_Tachykardie')

dat_mod_temp[, merkmale_list] <- data.frame(
  lapply(dat_mod_temp[, merkmale_list], function(x) {gsub("ja", "1", x)}))

dat_mod_temp[, merkmale_list] <- data.frame(
  lapply(dat_mod_temp[, merkmale_list], function(x) {gsub("nein", "0", x)}))

dat_mod_temp[, merkmale_list] <- data.frame(
  lapply(dat_mod_temp[, merkmale_list], function(x) {as.numeric(x)}))

dat_mod_temp$sympt_expl <- FALSE
dat_mod_temp$sympt_expl[which(
  rowSums(dat_mod_temp[, merkmale_list]) > 0)] <- TRUE

dat_mod_temp$no_sympt_expl <- FALSE
dat_mod_temp$no_sympt_expl[which(
  dat_mod_temp$Merkmal_SymptomfreiheitPatient == "ja")] <- TRUE

## set all symptoms to zero, if the patient was initially asymptomatic
dat_mod_temp$sympt_expl[which(dat_mod_temp$no_sympt_expl == TRUE)] <- FALSE 

## select data
dat_mod <- dat_mod_temp %>%
  mutate(rep_date_reg = as.Date(Erstmeldung),
         disease_start = as.Date(Merkmal_OnsetOfDisease),
         age = Alter_Erstmeldung
  ) %>%
  dplyr::select(rep_date_reg, disease_start, age, sympt_expl, no_sympt_expl)


# Data quality -----------------------------------------------------------------

#'@details There are every now and then missing dates or missing death 
#'notifications in the data. We identify the corresponding IDs and give them
#'to GS.

temp_list_1 <- NULL
temp_list_2 <- NULL
temp_list_3 <- NULL
temp_list_4 <- NULL


quality_death_notifications <- NULL

temp_list_1 <- octo_data_fatalities %>%
  filter(is.na(Sterbedatum),
         Tod == "ja") %>%
  mutate(quality_issue = "Status verstorben aber kein Sterbedatum") %>%
  dplyr::select(Vorgangsnummer, quality_issue, Erstmeldung, Sterbedatum, 
                Merkmal_OnsetOfDisease)

temp_list_2 <- octo_data_fatalities %>%
  filter(!is.na(Sterbedatum),
         Tod == "nein") %>%
  mutate(quality_issue = "Status nicht verstorben aber Sterbedatum") %>%
  dplyr::select(Vorgangsnummer, quality_issue, Erstmeldung, Sterbedatum, 
                Merkmal_OnsetOfDisease)

temp_list_3 <- octo_data_fatalities %>%
  filter(!is.na(Sterbedatum)) %>%
  filter(as.Date(Erstmeldung) > as.Date(Sterbedatum)) %>%
  mutate(quality_issue = "Sterbedatum vor Erstmeldung") %>%
  dplyr::select(Vorgangsnummer, quality_issue, Erstmeldung, Sterbedatum, 
                Merkmal_OnsetOfDisease)

temp_list_4 <-  octo_data_fatalities %>%
      filter(!is.na(Sterbedatum)) %>%
      filter(as.Date(Merkmal_OnsetOfDisease) > as.Date(Sterbedatum)) %>%
      mutate(quality_issue = "Erkrankungsbeginn nach Sterbedatum") %>%
      dplyr::select(Vorgangsnummer, quality_issue, Erstmeldung, Sterbedatum, 
                Merkmal_OnsetOfDisease)    
    

quality_death_notifications <- rbind(temp_list_1, 
                                     temp_list_2, 
                                     temp_list_3,
                                     temp_list_4)

## alternative fatalities
fatalities_a1 <- octo_data_fatalities %>%
  filter(Erstmeldung <= max(octo_complete$Datum),
         Tod == "ja") %>% nrow()

fatalities_a2 <- octo_data_fatalities %>%
  filter(Erstmeldung <= max(octo_complete$Datum),
         !is.na(Sterbedatum)) %>% nrow()

fatalities_a3 <- octo_data_fatalities %>%
  filter(Erstmeldung <= max(octo_complete$Datum),
         !is.na(Sterbedatum) | Tod == "ja") %>% nrow()


#'@details Dates which exceed the plausible range  We identify the 
#'corresponding IDs and give them to GS.
kh_data_quality <- octo_data_recent %>%
  mutate(
    Erstmeldung = as.Date(
      Erstmeldung, 
      format = "%d.%m.%Y"),
    Hospitalisierung_Meldedatum = as.Date(
      Hospitalisierung_Meldedatum, 
      format = "%d.%m.%Y"),
    Hospitalisierung_Krankenhaus_Aufnahme = as.Date(
      Hospitalisierung_Krankenhaus_Aufnahme, 
      format = "%d.%m.%Y"),
    Hospitalisierung_Krankenhaus_Entlassung = as.Date(
      Hospitalisierung_Krankenhaus_Entlassung, 
      format = "%d.%m.%Y"),
    Hospitalisierung_Krankenhaus_ITS_bis = as.Date(
      Hospitalisierung_Krankenhaus_ITS_bis, 
      format = "%d.%m.%Y"),
    Hospitalisierung_Krankenhaus_ITS_von = as.Date(
      Hospitalisierung_Krankenhaus_ITS_von, 
      format = "%d.%m.%Y")
    ) %>%
  dplyr::select(Vorgangsnummer, Erstmeldung, Hospitalisierung_Meldedatum,
                Hospitalisierung_Krankenhaus_Aufnahme,
                Hospitalisierung_Krankenhaus_Entlassung,
                Hospitalisierung_Krankenhaus_ITS_bis,
                Hospitalisierung_Krankenhaus_ITS_von) %>%
  mutate(Entlass_vor_Aufnahme = case_when(
    Hospitalisierung_Krankenhaus_Entlassung < 
      Hospitalisierung_Krankenhaus_Aufnahme ~ 1,
    Hospitalisierung_Krankenhaus_Entlassung >= 
      Hospitalisierung_Krankenhaus_Aufnahme ~ 0,
    )) %>%
  mutate(ITS_Ende_vor_Beginn = case_when(
    Hospitalisierung_Krankenhaus_ITS_bis < 
      Hospitalisierung_Krankenhaus_ITS_von ~ 1,
    Hospitalisierung_Krankenhaus_ITS_bis >= 
      Hospitalisierung_Krankenhaus_ITS_von ~ 0,
  )) %>%
  mutate(HospMeldung_in_Zukunft = case_when(
    Sys.Date() < Hospitalisierung_Meldedatum ~ 1,
    Sys.Date() >= Hospitalisierung_Meldedatum ~ 0
  )) %>% 
  mutate(Aufnahme_in_Zukunft = case_when(
    Sys.Date() < Hospitalisierung_Krankenhaus_Aufnahme ~ 1,
    Sys.Date() >= Hospitalisierung_Krankenhaus_Aufnahme ~ 0
  )) %>% 
  mutate(ITS_in_Zukunft = case_when(
    Sys.Date() < Hospitalisierung_Krankenhaus_ITS_von ~ 1,
    Sys.Date() >= Hospitalisierung_Krankenhaus_ITS_von ~ 0
  )) %>% 
  filter(
    Entlass_vor_Aufnahme == 1 | ITS_Ende_vor_Beginn == 1 | 
      HospMeldung_in_Zukunft == 1 | Aufnahme_in_Zukunft == 1 |
    ITS_in_Zukunft == 1 ) %>%
  replace_na(list(Entlass_vor_Aufnahme = 0,
                  ITS_Ende_vor_Beginn = 0,
                  HospMeldung_in_Zukunft = 0,
                  Aufnahme_in_Zukunft = 0,
                  ITS_in_Zukunft = 0))

# Export data ------------------------------------------------------------------

## write to tables
write.table(octo_complete,
            paste0(derived_data,"octoware_data.csv"),
            sep = ";",
            row.names = FALSE)

write.table(octo_complete,
            paste0(shared_data,"octoware_data.csv"),
            sep = ";",
            row.names = FALSE)


## export quality of death notifications table
if (!is.null(quality_death_notifications)) {
  
  quality_death_notifications <- quality_death_notifications %>%
    mutate(ID_number = Vorgangsnummer) %>%
    pivot_wider(names_from = quality_issue, values_from = ID_number) 
  
  ## write to tables
  write.table(quality_death_notifications,
              paste0(derived_data,"quality_death_notifications.csv"),
              sep = ";",
              row.names = FALSE)
  
  write.table(quality_death_notifications,
              paste0(shared_data,"quality_death_notifications.csv"),
              sep = ";",
              row.names = FALSE)
  
}

if (!is.null(kh_data_quality)) {
  
  ## write to tables
  write.table(kh_data_quality ,
              paste0(derived_data,"quality_hospital_data.csv"),
              sep = ";",
              row.names = FALSE)
  
  write.table(kh_data_quality ,
              paste0(shared_data,"quality_hospital_data.csv"),
              sep = ";",
              row.names = FALSE)
  
}


try(
  
  write.table(octo_branddir,
              paste0(brand_data, "octoware_raw.csv"),
              
              sep = ";",
              row.names = FALSE,
              fileEncoding = "utf-8",
              na = "0",
              dec = ",")
  
  , silent = TRUE
  
)

try(
  
  write.table(octo_branddir_summary,
              paste0(brand_data,"octoware_summary.csv"),
              
              sep = ";",
              row.names = FALSE,
              fileEncoding = "utf-8",
              na = "0",
              dec = ",")
  
  , silent = TRUE
  
)

try(
  
  write.table(
    
    Infektionsort_tab,
    
    paste0(brand_data, 'Infektionsorte_data.csv'), 
    
    sep = ";",
    row.names = FALSE,
    fileEncoding="UTF-8", 
    na = "0", 
    dec = ",")
  
  , silent = TRUE
  
)

try(
  
  write.table(Infektionsort_tab ,
              
              paste0(excel_path, 'Infektionsorte_data.csv'), 
              
              sep = ";",
              row.names = FALSE,
              fileEncoding="UTF-8", 
              na = "0", 
              dec = ",")
  
  , silent = TRUE
  
)

## export as .rds
saveRDS(octo_complete, paste0(derived_data,"octoware_data.rds"))
saveRDS(octo_data, paste0(derived_data,"octo_data.rds"))
saveRDS(dat_mod, paste0(derived_data,"nowcast_input_data.rds"))
saveRDS(ExpLand_new_tab, paste0(derived_data,"ExpLand_new_tab.rds"))
saveRDS(ExpLand_recent_tab, paste0(derived_data,"ExpLand_recent_tab.rds"))
saveRDS(mean_age_per_week, paste0(derived_data,"mean_age_per_week.rds"))
saveRDS(mean_age_per_date, paste0(derived_data,"mean_age_per_date.rds"))
saveRDS(fatalities_a1, paste0(derived_data,"fatalities_a1.rds"))
saveRDS(fatalities_a2, paste0(derived_data,"fatalities_a2.rds"))
saveRDS(fatalities_a3, paste0(derived_data,"fatalities_a3.rds"))
saveRDS(StatusPS_joined, paste0(derived_data,"StatusPS_joined.rds"))
saveRDS(Infektionsort_tab, paste0(derived_data,"Infektionsort_tab.rds"))
saveRDS(Alter_Erstmeldung_var, paste0(derived_data,"Alter_Erstmeldung_var.rds"))
saveRDS(Alter_Erstmeldung_prev, paste0(derived_data,"Alter_Erstmeldung_prev.rds"))
saveRDS(all_locations_list, file = paste0(derived_data, "all_locations_list.rds"))

## export as .rds
saveRDS(octo_complete, paste0(shared_data,"octoware_data.rds"))
saveRDS(dat_mod, paste0(shared_data,"nowcast_input_data.rds"))
saveRDS(ExpLand_new_tab, paste0(shared_data,"ExpLand_new_tab.rds"))
saveRDS(ExpLand_recent_tab, paste0(shared_data,"ExpLand_recent_tab.rds"))
saveRDS(mean_age_per_week, paste0(shared_data,"mean_age_per_week.rds"))
saveRDS(mean_age_per_date, paste0(shared_data,"mean_age_per_date.rds"))
saveRDS(fatalities_a1, paste0(shared_data,"fatalities_a1.rds"))
saveRDS(fatalities_a2, paste0(shared_data,"fatalities_a2.rds"))
saveRDS(fatalities_a3, paste0(shared_data,"fatalities_a3.rds"))
saveRDS(StatusPS_joined, paste0(shared_data,"StatusPS_joined.rds"))
saveRDS(Infektionsort_tab, paste0(shared_data,"Infektionsort_tab.rds"))
saveRDS(Alter_Erstmeldung_var, paste0(shared_data,"Alter_Erstmeldung_var.rds"))
saveRDS(Alter_Erstmeldung_prev, paste0(shared_data,"Alter_Erstmeldung_prev.rds"))
saveRDS(all_locations_list, file = paste0(shared_data, "all_locations_list.rds"))
