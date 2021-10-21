#'@title all_locations_list
#'@description We want to filter all the "letzte_Infektionsort" names in 
#'ocotoware such that we get a list of non-German locations. Who got infected
#'abroad? Towards this end, we create a huge look up list with German 
#'locations.

# read data --------------------------------------------------------------------

## export as .rds
octo_data <- readRDS(paste0(derived_data,"octo_data.rds"))

# list of all locations --------------------------------------------------------

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

# export list ------------------------------------------------------------------

## save list
saveRDS(all_locations_list, 
        file = paste0(derived_data, "all_locations_list.rds"))
saveRDS(all_locations_list, 
        file = paste0(shared_data, "all_locations_list.rds"))

