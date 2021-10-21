library(lubridate)

# read data --------------------------------------------------------------------

## reduce to these columns
octoware_cols <- c(
  "ID",
  "erzeugt_am",
  "WohnAnschrift_Ort",
  "Merkmal_Absonderung",
  "Merkmal_AbsonderungBeginn",
  "Merkmal_AbsonderungEnde",
  "Anzahl_Kontaktpersonen"
)

## read CTT data (complete)
ctt_cases <-
  readxl::read_excel(
    paste0(user_input, "ctt_data.xlsx"),
    guess_max = 100000
  )

## read octoware history (complete)
octo_data <- readRDS(paste0(derived_data,"octo_data_history.rds"))

## reduce columns
ctt_cases <- ctt_cases[, octoware_cols]

## adjust date format
ctt_cases <- ctt_cases %>%
  mutate(
    erzeugt_am = as.Date(erzeugt_am, format="%Y-%m-%d"),
    Merkmal_AbsonderungBeginn = as.Date(Merkmal_AbsonderungBeginn, 
                                        format="%Y-%m-%d"),
    Merkmal_AbsonderungEnde = as.Date(Merkmal_AbsonderungEnde, 
                                      format="%Y-%m-%d")
  )
         
## exclude data before 30.04.21
#'@details For a short period of time, BaySim and Octoware where used in 
#'parallel. This means, cases where entered in both systems. However, we do
#'not want to count them twice. We therefore regard BaySim as a source for cases
#'entered no later than the 30th of April 2021.  On the otherhand, we exclude
#'cases from Octoware, which were entered before the 1st of May 2021.

ctt_cases <- ctt_cases %>%
  filter(erzeugt_am >= "2021-05-01") %>%
  filter(Merkmal_AbsonderungEnde > "2021-04-30")
  

## prepare data: CTT cases -----------------------------------------------------

#' We derive 'today' from the modification date of the original data. When
#' the raw data is not available, set 'today' by means of the Sys.Date().

if (third_party == FALSE) {
  
  today <- as.Date(file.mtime(paste0(user_input, "ctt_data.xlsx")))
  
} else {
  
  today <- Sys.Date()
  
}

ctt_absonderung_days <- ctt_cases %>%
  filter(WohnAnschrift_Ort == "München") %>%
  filter(Merkmal_Absonderung == "ja") %>%
  # Wenn es kein Absonderungsende gibt, setze es auf heute, 
  # um ein Enddatum beim generieren der Tage zu haben 
  replace_na(list(Merkmal_AbsonderungEnde = today)) %>% 
  # Absonderung darf nicht enden, bevor sie ausgesprochen wurde
  filter(as.Date(Merkmal_AbsonderungBeginn) <= 
           as.Date(Merkmal_AbsonderungEnde)) %>%
  # Pandemie beginnt in 2020 - alles davor ist implausibel
  filter(as.Date(Merkmal_AbsonderungBeginn) >= as.Date('2020-01-01')) %>%
  # Absonderung kann nicht in der Zukunft ausgesprochen werden
  filter(as.Date(Merkmal_AbsonderungBeginn) <= today) %>%
  # Wie lange kann man maximal in Quarantäne sein? +30 ist Platzhalter
  filter(as.Date(Merkmal_AbsonderungEnde) <= (today + 30)) %>%
  mutate(Absonderungszeitraum = 
           as.Date(Merkmal_AbsonderungBeginn) %--%
           as.Date(Merkmal_AbsonderungEnde)
  ) %>%
  dplyr::select(
    Merkmal_AbsonderungBeginn, 
    Merkmal_AbsonderungEnde, 
    Absonderungszeitraum
  )

minDate <- min(ctt_absonderung_days$Merkmal_AbsonderungBeginn)
maxDate <- max(ctt_absonderung_days$Merkmal_AbsonderungEnde)

## Wie viele KP in Absonderung pro Tag sind
## https://stackoverflow.com/a/55909192
ctt_daily_quarant <- tibble(Datum = seq(minDate, maxDate, "1 day")) %>% 
  rowwise() %>%
  mutate(KP1_quarant_total=sum(
    Datum %within% ctt_absonderung_days$Absonderungszeitraum)) %>%
  ungroup() %>%
  # Änderung der Anzahl der sich in Absonderung befindlichen KP1 zum Vortag
  mutate(KP1_quarant_inz = c(0, diff(KP1_quarant_total)))



## ermittelte KP gesamt
ctt_cases_total <- ctt_cases %>%
  mutate(Datum = as.Date(erzeugt_am)) %>%
  group_by(Datum) %>%
  summarise(KP1_erzeugt = n()) %>%
  full_join(
    ctt_daily_quarant,
    by = c('Datum')
  ) %>%
  mutate(KP1_erzeugt = replace_na(KP1_erzeugt, 0)) %>%
  ## trim recent date
  arrange(Datum) %>%
  filter(Datum < today)

## prepare data: IP cases ------------------------------------------------------

## Wie viele Kontaktfälle entfallen auf eine IP?
IP_cases <- octo_data

IP_cases_total <- IP_cases %>%
  mutate(
    Datum = Erstmeldung,
    Anzahl_Kontaktpersonen = as.numeric(Anzahl_Kontaktpersonen)) %>%
  group_by(Datum) %>%
  summarise(
    IP_n = n(),
    IP_sum_kontakte = sum(Anzahl_Kontaktpersonen),
    IP_min_kontakte = min(Anzahl_Kontaktpersonen),
    IP_max_kontakte = max(Anzahl_Kontaktpersonen),
    IP_q50 = round(quantile(Anzahl_Kontaktpersonen, c(0.50), q = c(0.50))),
    IP_q75 = round(quantile(Anzahl_Kontaktpersonen, c(0.75), q = c(0.75))),
    IP_q90 = round(quantile(Anzahl_Kontaktpersonen, c(0.90), q = c(0.90))),
    IP_q95 = round(quantile(Anzahl_Kontaktpersonen, c(0.95), q = c(0.95))),
  ) %>%
  ## trim recent date
  arrange(Datum) %>%
  filter(Datum < today)

# Merge tables -----------------------------------------------------------------

ctt_cases_total <- ctt_cases_total %>%
  full_join(
    IP_cases_total,
    by = c("Datum")
  )

## export data -----------------------------------------------------------------

saveRDS(ctt_cases_total, paste0(derived_data, "ctt_cases_total.rds"))
saveRDS(ctt_cases_total, paste0(shared_data, "ctt_cases_total.rds"))

