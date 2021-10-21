library(synthpop)
library(tidyverse)
library(lubridate)

# helpers ----------------------------------------------------------------------

#'@details The original data has far more levels than the algorithm can handle
#'on the fly.

reduce_levels <- function(data, col_name, number_of_levels = 20) {
  col_name <- enquo(col_name)
  data %>%
    count(!!col_name) %>%
    arrange(-n) %>%
    top_n(number_of_levels) %>%
    slice_sample(n = nrow(data), weight_by=n, replace = TRUE) %>%
    dplyr::select(!!col_name)
}

## new variable names (14.09.2021)
octo_cols <- c(
  "ID",
  "Vorgangsnummer",
  "Meldedatum",
  "Tod",
  "Sterbedatum",
  "hospitalisiert",
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
  "letzte_Exposition",
  "letzte_Exposition_Art",
  "Meldekreis",
  "Merkmal_VirusVariante_Mutation",
  "Merkmal_Details_VirusVariante",
  "Anzahl_Kontaktpersonen"
)

# function synthesize_octo_recent ----------------------------------------------

## with "new" variable names
synthesize_octo_recent <- function(data) {
  octo_data_converted <- data %>% 
    dplyr::select(all_of(octo_cols)) %>%
    mutate(Alter_Erstmeldung = as.integer(Alter_Erstmeldung)) %>%
    mutate(Anzahl_Kontaktpersonen = as.integer(Anzahl_Kontaktpersonen)) %>%
    # Berechne die Anzahl der Tage, die zwischen Meldedatum und dem jeweiligen
    # Datumsfeld liegen (1 Tag = 60s*60*24 = 86400s)
    # Synthpop kann zwar keine Datumsangaben synthetisieren, aber dafür Zahlen
    mutate(SterbedatumOffset = as.integer(
      as.Date(Meldedatum) %--% as.Date(Sterbedatum, format = "%Y-%m-%d") / 86400
    )) %>%
    mutate(Hospitalisierung_MeldedatumOffset = as.integer(
      (as.Date(Meldedatum) %--% 
         as.Date(Hospitalisierung_Meldedatum, format = "%d.%m.%Y")) / 86400
    )) %>%
    mutate(Merkmal_OnsetOfDiseaseOffset = as.integer(
      (as.Date(Meldedatum) %--% 
         as.Date(Merkmal_OnsetOfDisease, format = "%d.%m.%Y")) / 86400
    )) %>%
    mutate(Hospitalisierung_Krankenhaus_AufnahmeOffset = as.integer(
      (as.Date(Meldedatum) %--% 
         as.Date(Hospitalisierung_Krankenhaus_Aufnahme, format = "%d.%m.%Y")) / 86400
    )) %>%
    mutate(Hospitalisierung_Krankenhaus_EntlassungOffset = as.integer(
      (as.Date(Meldedatum) %--% 
         as.Date(Hospitalisierung_Krankenhaus_Entlassung, format = "%d.%m.%Y")) / 86400
    )) %>%
    mutate(Hospitalisierung_Krankenhaus_ITS_bisOffset = as.integer(
      (as.Date(Meldedatum) %--% 
         as.Date(Hospitalisierung_Krankenhaus_ITS_bis, format = "%d.%m.%Y")) / 86400
    )) %>%
    mutate(Hospitalisierung_Krankenhaus_ITS_vonOffset = as.integer(
      (as.Date(Meldedatum) %--% 
         as.Date(Hospitalisierung_Krankenhaus_ITS_von, format = "%d.%m.%Y")) / 86400
    )) %>%
    mutate(Merkmal_DatumLetzterImpfungOffset = as.integer(
      (as.Date(Meldedatum) %--% 
         as.Date(Merkmal_DatumLetzterImpfung, format = "%d.%m.%Y")) / 86400
    )) %>%
    
    dplyr::select(
      - WohnAnschrift_PLZ,
      - WohnAnschrift_Ort,
      - WohnAnschrift_OT,
      - letzte_Exposition,
      - Meldekreis,
      - Sterbedatum,
      - Hospitalisierung_Meldedatum,
      - Merkmal_OnsetOfDisease,
      - Hospitalisierung_Krankenhaus_Aufnahme,
      - Hospitalisierung_Krankenhaus_Entlassung,
      - Hospitalisierung_Krankenhaus_ITS_bis,
      - Hospitalisierung_Krankenhaus_ITS_von,
      - Merkmal_DatumLetzterImpfung
    ) %>%
    bind_cols(
      data %>% reduce_levels(WohnAnschrift_PLZ, number_of_levels = 20),
      data %>% reduce_levels(letzte_Exposition, number_of_levels = 5),
      data %>% reduce_levels(Meldekreis, number_of_levels = 20)
    ) %>% 
    relocate(
      WohnAnschrift_PLZ, 
      Meldekreis, 
      letzte_Exposition, 
      Merkmal_OnsetOfDiseaseOffset, 
      Hospitalisierung_MeldedatumOffset, 
      SterbedatumOffset,
      Hospitalisierung_Krankenhaus_AufnahmeOffset,
      Hospitalisierung_Krankenhaus_EntlassungOffset,
      Hospitalisierung_Krankenhaus_ITS_bisOffset,
      Hospitalisierung_Krankenhaus_ITS_vonOffset,
      Merkmal_DatumLetzterImpfungOffset
      
    )
  
  
  octo_data_left <- octo_data_converted %>%
    dplyr::select(ID, Vorgangsnummer, Meldedatum)
  octo_data_right <- octo_data_converted %>%
    dplyr::select(-ID, -Vorgangsnummer, -Meldedatum) %>% 
    syn(., maxfaclevels = 60, seed = 42)
  
  set.seed(42)
  
  octo_data_left %>%
    bind_cols(octo_data_right$syn) %>% 
    # Sollte für alle Münchner Fälle auch "München" sein
    mutate(WohnAnschrift_Ort = "München") %>%
    # WohnAnschrift_OT stimmt meistens mit Meldekreis überein
    # Wenn sie sich unterscheiden, dann nur in Kleinigkeiten wie z.B. der 
    # Benutzung von Leerzeichen statt Bindestrichen etc.  
    mutate(WohnAnschrift_OT = Meldekreis) %>%
    # Addiere Offsets auf Erstmeldung, um Datumsfelder zu erhalten
    mutate(Sterbedatum = as.Date(Meldedatum) + SterbedatumOffset) %>%
    mutate(Hospitalisierung_Meldedatum = 
             as.Date(Meldedatum) + Hospitalisierung_MeldedatumOffset) %>%
    mutate(Hospitalisierung_Krankenhaus_Aufnahme = 
             as.Date(Meldedatum) + Hospitalisierung_Krankenhaus_AufnahmeOffset) %>%
    mutate(Hospitalisierung_Krankenhaus_Entlassung = 
             as.Date(Meldedatum) + Hospitalisierung_Krankenhaus_EntlassungOffset) %>%
    mutate(Hospitalisierung_Krankenhaus_ITS_bis = 
             as.Date(Meldedatum) + Hospitalisierung_Krankenhaus_ITS_bisOffset) %>%
    mutate(Hospitalisierung_Krankenhaus_ITS_von = 
             as.Date(Meldedatum) + Hospitalisierung_Krankenhaus_ITS_vonOffset) %>%
    mutate(Merkmal_OnsetOfDisease = 
             as.Date(Meldedatum) + Merkmal_OnsetOfDiseaseOffset) %>%
    mutate(Merkmal_DatumLetzterImpfung = 
             as.Date(Meldedatum) + Merkmal_DatumLetzterImpfungOffset) %>%
    # Format der Datumsangaben wiederherstellen
    mutate(Meldedatum = format.Date(Meldedatum, "%d.%m.%Y")) %>%
    mutate(Hospitalisierung_Meldedatum = 
             format.Date(Hospitalisierung_Meldedatum, "%d.%m.%Y")) %>%
    mutate(Hospitalisierung_Krankenhaus_Aufnahme = 
             format.Date(Hospitalisierung_Krankenhaus_Aufnahme, "%d.%m.%Y")) %>%
    mutate(Hospitalisierung_Krankenhaus_Entlassung = 
             format.Date(Hospitalisierung_Krankenhaus_Entlassung, "%d.%m.%Y")) %>%
    mutate(Hospitalisierung_Krankenhaus_ITS_bis = 
             format.Date(Hospitalisierung_Krankenhaus_ITS_bis, "%d.%m.%Y")) %>%
    mutate(Hospitalisierung_Krankenhaus_ITS_von = 
             format.Date(Hospitalisierung_Krankenhaus_ITS_von, "%d.%m.%Y")) %>%
    mutate(Merkmal_DatumLetzterImpfung = 
             format.Date(Merkmal_DatumLetzterImpfung, "%d.%m.%Y")) %>%
    mutate(Merkmal_OnsetOfDisease = 
             format.Date(Merkmal_OnsetOfDisease, "%d.%m.%Y")) %>%
    mutate(Sterbedatum = format.Date(Sterbedatum, "%Y-%m-%d")) %>%
    # Hilfsspalten entfernen
    dplyr::select(all_of(octo_cols))
}

# read, synthesize and export octo_data_recent ---------------------------------


octo_data_recent <- readxl::read_excel(
  paste0(user_input, "octoware_kurz.xlsx"),
  guess_max = 100000
)

octo_data <- readxl::read_excel(
  paste0(user_input, "octoware.xlsx"),
  guess_max = 100000
)

octo_data_recent_syn <- synthesize_octo_recent(octo_data_recent)
octo_data_syn <- synthesize_octo_recent(octo_data)


## prepare 'Datum'
octo_data_recent_syn <- octo_data_recent_syn %>%
  mutate(Meldedatum = as.Date(Meldedatum, format = "%d.%m.%Y"),
         Sterbedatum = as.Date(Sterbedatum, format = "%d.%m.%Y"),
         Hospitalisierung_Meldedatum = as.Date(Hospitalisierung_Meldedatum, 
                                               format = "%d.%m.%Y"),
         Hospitalisierung_Krankenhaus_Aufnahme = 
           as.Date(Hospitalisierung_Krankenhaus_Aufnahme, 
                                               format = "%d.%m.%Y"),
         Hospitalisierung_Krankenhaus_Entlassung = 
           as.Date(Hospitalisierung_Krankenhaus_Entlassung, 
                                               format = "%d.%m.%Y"),
         Hospitalisierung_Krankenhaus_ITS_bis = 
           as.Date(Hospitalisierung_Krankenhaus_ITS_bis, 
                                               format = "%d.%m.%Y"),
         Hospitalisierung_Krankenhaus_ITS_von = 
           as.Date(Hospitalisierung_Krankenhaus_ITS_von, 
                                               format = "%d.%m.%Y"),
         Merkmal_DatumLetzterImpfung = 
           as.Date(Merkmal_DatumLetzterImpfung, 
                   format = "%d.%m.%Y"),
    
         Merkmal_OnsetOfDisease = as.Date(Merkmal_OnsetOfDisease, 
                                          format = "%d.%m.%Y"))

## complete data (without recent)
octo_data_syn <- octo_data_syn %>%
  mutate(Meldedatum = as.Date(Meldedatum, format = "%d.%m.%Y"),
         Sterbedatum = as.Date(Sterbedatum, format = "%d.%m.%Y"),
         Hospitalisierung_Meldedatum = as.Date(Hospitalisierung_Meldedatum, 
                                               format = "%d.%m.%Y"),
         Hospitalisierung_Krankenhaus_Aufnahme = 
           as.Date(Hospitalisierung_Krankenhaus_Aufnahme, 
                   format = "%d.%m.%Y"),
         Hospitalisierung_Krankenhaus_Entlassung = 
           as.Date(Hospitalisierung_Krankenhaus_Entlassung, 
                   format = "%d.%m.%Y"),
         Hospitalisierung_Krankenhaus_ITS_bis = 
           as.Date(Hospitalisierung_Krankenhaus_ITS_bis, 
                   format = "%d.%m.%Y"),
         Hospitalisierung_Krankenhaus_ITS_von = 
           as.Date(Hospitalisierung_Krankenhaus_ITS_von, 
                   format = "%d.%m.%Y"),
         Merkmal_DatumLetzterImpfung = 
           as.Date(Merkmal_DatumLetzterImpfung, 
                   format = "%d.%m.%Y"),
         Merkmal_OnsetOfDisease = as.Date(Merkmal_OnsetOfDisease, 
                                          format = "%d.%m.%Y"))

# add 'mutpol' -----------------------------------------------------------------

octo_data_syn$Bemerkung <- sample(1:3, size = nrow(octo_data_syn), 
       replace = TRUE, prob = c(0.4,0.3,0.3))

octo_data_syn$Bemerkung[octo_data_syn$Bemerkung == 1] <- "MutPol pos"
octo_data_syn$Bemerkung[octo_data_syn$Bemerkung == 2] <- "MutPol neg"
octo_data_syn$Bemerkung[octo_data_syn$Bemerkung == 3] <- NA


octo_data_recent_syn$Bemerkung <- sample(1:3, size = nrow(octo_data_recent_syn), 
                                  replace = TRUE, prob = c(0.4,0.3,0.3))

octo_data_recent_syn$Bemerkung[octo_data_recent_syn$Bemerkung == 1] <- "MutPol pos"
octo_data_recent_syn$Bemerkung[octo_data_recent_syn$Bemerkung == 2] <- "MutPol neg"
octo_data_recent_syn$Bemerkung[octo_data_recent_syn$Bemerkung == 3] <- NA


## export data
writexl::write_xlsx(octo_data_recent_syn, 
                    str_glue("{shared_input}octoware_kurz.xlsx"))

## export
writexl::write_xlsx(octo_data_syn, str_glue("{shared_input}octoware.xlsx"))


# read, synthesize and export octoware_tote ------------------------------------

octo_data_recent <- readxl::read_excel(
  paste0(user_input, "octoware_tote.xlsx"),
  guess_max = 100000
)

octo_data_recent_syn <- synthesize_octo_recent(octo_data_recent)

## prepare 'Datum'
octo_data_recent_syn <- octo_data_recent_syn %>%
  mutate(Meldedatum = as.Date(Meldedatum, format = "%d.%m.%Y"),
         Sterbedatum = as.Date(Sterbedatum, format = "%d.%m.%Y"),
         Hospitalisierung_Meldedatum = as.Date(Hospitalisierung_Meldedatum, 
                                               format = "%d.%m.%Y"),
         Merkmal_OnsetOfDisease = as.Date(Merkmal_OnsetOfDisease, 
                                          format = "%d.%m.%Y"))

## export data
writexl::write_xlsx(octo_data_recent_syn, 
                    str_glue("{shared_input}octoware_tote.xlsx"))


# read, synthesize and export mutanten -----------------------------------------


voc_positive_data <- readxl::read_excel(
  paste0(user_input, "VOC_positiv.xlsx"),
  guess_max = 100000
)

voc_negative_data <- readxl::read_excel(
  paste0(user_input, "VOC_negativ.xlsx"),
  guess_max = 100000
)  

voc_unklar_data <- readxl::read_excel(
  paste0(user_input, "VOC_unklar.xlsx"),
  guess_max = 100000
)

# voc_positive_data_syn <- synthesize_octo_recent(voc_positive_data)
# voc_negative_data_syn <- synthesize_octo_recent(voc_negative_data)
# voc_unklar_data_syn <- synthesize_octo_recent(voc_unklar_data)

relevanct_columns <- c('Meldedatum', 
                       'Merkmal_VirusVariante_Mutation', 
                       'Merkmal_Details_VirusVariante')

voc_positive_data <- voc_positive_data[, relevanct_columns]
voc_negative_data <- voc_negative_data[, relevanct_columns]
voc_unklar_data <- voc_unklar_data[, relevanct_columns]

voc_positive_data %>%
  writexl::write_xlsx(., str_glue("{shared_input}VOC_positiv.xlsx"))

voc_negative_data %>% 
  writexl::write_xlsx(., str_glue("{shared_input}VOC_negativ.xlsx"))

voc_unklar_data %>%
  writexl::write_xlsx(., str_glue("{shared_input}VOC_unklar.xlsx"))
