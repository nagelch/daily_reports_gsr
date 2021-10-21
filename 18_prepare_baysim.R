library(lubridate)


# read baysim data -------------------------------------------------------------

## reduce to these columns
baysim_cols <- c(
  "(Nicht ändern) Fallakte",
  "Erstellt am",
  "Kategorie",
  "Ort",
  "Absonderung ausgesprochen",
  "Absonderung ausgesprochen am",
  "Absonderung bis"
)

## read data (complete)
baysim_cases_recent <-
  readxl::read_excel(
    paste0(user_input, "baysim_fallakten.xlsx"),
    guess_max = 100000
  )

## reduce columns
baysim_cases_recent <- baysim_cases_recent[, baysim_cols]

## adjust date format
baysim_cases_recent$`Erstellt am` <- as.Date(baysim_cases_recent$`Erstellt am`, format="%Y-%m-%d")
baysim_cases_recent$`Absonderung ausgesprochen am` <- as.Date(baysim_cases_recent$`Absonderung ausgesprochen am`, format="%Y-%m-%d")
baysim_cases_recent$`Absonderung bis` <- as.Date(baysim_cases_recent$`Absonderung bis`, format="%Y-%m-%d")


## read baysim 2020 data
baysim_cases_2020 <-
  readxl::read_excel(
    paste0(user_input, "baysim_fallakten_2020.xlsx"),
    guess_max = 100000
  )

## reduce columns
baysim_cases_2020 <- baysim_cases_2020[, baysim_cols]

## adjust date format
baysim_cases_2020$`Erstellt am` <- as.Date(baysim_cases_2020$`Erstellt am`, format="%Y-%m-%d")
baysim_cases_2020$`Absonderung ausgesprochen am` <- as.Date(baysim_cases_2020$`Absonderung ausgesprochen am`, format="%Y-%m-%d")
baysim_cases_2020$`Absonderung bis` <- as.Date(baysim_cases_2020$`Absonderung bis`, format="%Y-%m-%d")


## join datasets
baysim_cases <- bind_rows(baysim_cases_2020, baysim_cases_recent)


## prepare data ----------------------------------------------------------------

#' We derive 'today' from the modification date of the original data. When
#' the raw data is not available, set 'today' by means of the Sys.Date().

if (third_party == FALSE) {
  
  today <- as.Date(file.mtime(paste0(user_input, "baysim_fallakten.xlsx")))
  
} else {
  
  today <- Sys.Date()
  
}


baysim_absonderung_days <- baysim_cases %>%
  filter(Ort == "München") %>%
  filter(Kategorie %in% c('KP I', 'KP I A', 'KP I B')) %>%
  filter(`Absonderung ausgesprochen` == "Ja") %>%
  # Wenn es kein Absonderungsende gibt, setze es auf heute, 
  # um ein Enddatum beim generieren der Tage zu haben 
  replace_na(list(`Absonderung bis` = today)) %>% 
  # Absonderung darf nicht enden, bevor sie ausgesprochen wurde
  filter(as.Date(`Absonderung ausgesprochen am`) <= as.Date(`Absonderung bis`)) %>%
  # Pandemie beginnt in 2020 - alles davor ist implausibel
  filter(as.Date(`Absonderung ausgesprochen am`) >= as.Date('2020-01-01')) %>%
  # Absonderung kann nicht in der Zukunft ausgesprochen werden
  filter(as.Date(`Absonderung ausgesprochen am`) <= today) %>%
  # Wie lange kann man maximal in Quarantäne sein? +30 ist Platzhalter
  filter(as.Date(`Absonderung bis`) <= (today + 30)) %>%
  mutate(Absonderungszeitraum = 
           as.Date(`Absonderung ausgesprochen am`) %--%
           as.Date(`Absonderung bis`)
  ) %>%
  dplyr::select(
    `Absonderung ausgesprochen am`, 
    `Absonderung bis`, 
    Absonderungszeitraum
  )

minDate <- min(baysim_absonderung_days$`Absonderung ausgesprochen am`)
maxDate <- max(baysim_absonderung_days$`Absonderung bis`)

## Wie viele KP in Absonderung pro Tag sind
## https://stackoverflow.com/a/55909192
baysim_daily_quarant <- tibble(Datum = seq(minDate, maxDate, "1 day")) %>% 
  rowwise() %>%
  mutate(KP1_quarant_total=sum(Datum %within% baysim_absonderung_days$Absonderungszeitraum)) %>%
  ungroup() %>%
  # Änderung der Anzahl der sich in Absonderung befindlichen KP1 zum Vortag
  mutate(KP1_quarant_inz = c(0, diff(KP1_quarant_total)))

## ermittelte KP1 gesamt
## KP I setzt sich zusammen aus KP I, KP I A und KP I B
baysim_cases_total <- baysim_cases %>%
  filter(Kategorie %in% c('KP I', 'KP I A', 'KP I B')) %>%
  mutate(Datum = as.Date(`Erstellt am`)) %>%
  group_by(Datum) %>%
  summarise(kategorie_KP = n()) %>%
  full_join(
    baysim_daily_quarant,
    by = c('Datum')
  ) %>%
  mutate(kategorie_KP = replace_na(kategorie_KP, 0)) %>%
  mutate(Kum_kategorie_KP = cumsum(kategorie_KP)) %>%
  ## trim recent date
  arrange(Datum) %>%
  filter(Datum < today)


## export data -----------------------------------------------------------------

saveRDS(baysim_cases_total, paste0(derived_data, "baysim_data.rds"))
