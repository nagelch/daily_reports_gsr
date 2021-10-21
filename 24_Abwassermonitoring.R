
# path variables ---------------------------------------------------------------

## main path
path_tagesberichte <- "L:/Thematische-Ordner/Corona/R_Projekte/Tagesbericht/"
path_tagesberichte_branddir <- "M:/BFM/Austausch-RGU-BD/Reports_RGU/"

source(paste0(path_tagesberichte, "scripts/22_path_variables.R"),
       encoding = 'utf-8')

# read data --------------------------------------------------------------------

## read data
octo_locations_data <-
  readxl::read_excel(
    paste0(user_input, "Untersuchungsgebiete_Adressen_Q1_2021_mit_Meldedatum.xlsx"),
    guess_max = 100000
  )

## target areas
all_locations_sewage <- readRDS(
  paste0(derived_data, "all_locations_sewage.rds"))

## read existing data
octo_sewage_db <- readRDS(paste0(derived_data,"octo_sewage.rds"))

# prepare data -----------------------------------------------------------------

## initialize column
octo_locations_data$STR_NAME_HAUS_NR <- paste0(
  octo_locations_data$`Straße (Wohn/Sitz)`,
  octo_locations_data$`Hausnr./PF (Wohn/Sitz)`)

## clean up strings
octo_locations_data <- octo_locations_data %>%
  mutate(lookup = octo_locations_data$STR_NAME_HAUS_NR %>%
           str_replace(., "[[:punct:]]", "") %>%
           toupper(.) %>%
           str_replace(., " ", "") %>%
           str_replace(., "STRAßE", "STR") %>%
           str_replace(., "-", "") 
           )

## clean up strings
all_locations_sewage <- all_locations_sewage %>%
  mutate(lookup = all_locations_sewage$STR_NAME_HAUS_NR %>%
           str_replace(., "[[:punct:]]", "") %>%
           toupper(.) %>%
           str_replace(., " ", "") %>%
           str_replace(., "STRAßE", "STR") %>%
           str_replace(., "-", "") 
  ) %>%
  dplyr::select(lookup, Nummer, Gebiet)

## merge
octo_sewage <- octo_locations_data %>%
  left_join(
    all_locations_sewage %>%
      dplyr::select(lookup, Nummer, Gebiet),
    by = c("lookup")
  ) %>%
  mutate(Datum = as.Date(Meldedatum, format = "%d.%m.%Y"),
         Gebiet = as.factor(Gebiet)) %>%
  group_by(Datum, Gebiet, .drop = FALSE) %>%
  count(., .drop = FALSE) %>%
  arrange(Datum)

## overwrite old data by newer data
octo_sewage <- rbind(
  octo_sewage_db %>%
    filter(Datum < min(octo_sewage$Datum)),
  octo_sewage)

## plausicheck -----------------------------------------------------------------

## calculate incidences from exports
octo_sewage_inz <-aggregate(
  octo_sewage_db$n,
  by = list(Datum = octo_sewage_db$Datum),
  FUN = sum) %>%
  magrittr::set_colnames(c("Datum", "Inzidenzen_Abwasser_plausi"))

nrow(octo_sewage_inz)


## compare to daily exports
octo_inz <- readRDS(paste0(derived_data,"octoware_data.rds"))

octo_inz %>%
  filter(Datum >= min(octo_sewage_db$Datum)) %>%
  dplyr::select(Datum, casesMunich_Inz) %>%
  right_join(octo_sewage_inz,
             by = c("Datum")) %>%
  View()


## export ----------------------------------------------------------------------

## update rds
saveRDS(octo_sewage, paste0(derived_data,"octo_sewage.rds"))

## write to tables
write.table(octo_sewage,
            paste0(path_tagesberichte_branddir,"Abwassermonitoring.csv"),
            sep = ";",
            row.names = FALSE)

