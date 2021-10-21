#'@title daily_reports_data
#'@details load data from for the daily reports
#'- Tagesbericht
#'- Tagesbericht_extended
#'- Tagesbericht_Montag
#'- aktuelle_kennzahlen

# functions --------------------------------------------------------------------

#'@details We need to report the delta to the previous day. This delta needs
#' to have a prefix indicating either an increase "+" or decrease "-". The
#' sign_char function retrieves the sign and turns it into a character.
#'@param x integer array

sign_char <- function(x) {
  
  sign_x_char <- NULL
  sign_x <- NULL
  
  sign_x <- sign(x)
  
  for (i in seq(length(sign_x))) {
    
    if ((sign_x[i]) >= 0) {
      sign_x_char[i] <- "+"
    } else if (sign_x[i] < 0) {
      sign_x_char[i] <- "-"
    }
  }
  return(sign_x_char)
}


# global variables -------------------------------------------------------------

#'@param EW_MUC, this variable is used througout all reports.
## EW_MUC <- 1484226
EW_MUC <- 1488202 ## changed: 30.08.21

# prepare data: cases ----------------------------------------------------------

rki_7days_archive <- 
  readr::read_rds(
    paste0(wd_path1,"rki_7days_archive.rds")
  ) %>% 
  group_by(Datum) %>%
  top_n(Session, n=-1) %>%
  ungroup()

# Berechne 7-Tage-Inzidenz aus RKI-Daten
rki_7days_archive_inz <- readRDS(paste0(wd_path1,'rki_7days_archive.rds')) %>%
  ## - für jeden Datenstand: filtere und behalte nur die höchste Session ID.
  ## - für jeden Datenstand: bilde die 7 Tage Inzidenz
  group_by(Datenstand) %>%
  top_n(Session, n=1) %>%
  top_n(Datum, n=7) %>%
  summarise(Inz7days_RKI = round(sum(casesMunich) / (EW_MUC / 100000), 
                                 digits = 2)) %>%
  arrange(Datenstand)


## load and sort data
input_data <-  readr::read_rds(paste0(wd_path1,"joined_rki_octoware_data.rds"))

input_data <- input_data %>% 
  left_join(
    rki_7days_archive,
    by = c("Datum" = "Datum"),
    suffix = c("", "_archive")
  )

## incident cases and daily age groups
input_data <- input_data %>%
  mutate(casesMunich_Inz = c(0, diff(casesMunich)),
         casesGermany_Inz = c(0, diff(casesGermanyKum)),
         casesBavaria_Inz = c(0, diff(casesBavariaKum)),
         age_0to5_sum_new = age_0to5_sum - lag(age_0to5_sum), 
         age_6to10_sum_new = age_6to10_sum - lag(age_6to10_sum), 
         age_11to20_sum_new = age_11to20_sum - lag(age_11to20_sum),
         age_21to40_sum_new = age_21to40_sum - lag(age_21to40_sum), 
         age_41to60_sum_new = age_41to60_sum - lag(age_41to60_sum), 
         age_61to80_sum_new = age_61to80_sum - lag(age_61to80_sum), 
         age_81plus_sum_new = age_81plus_sum - lag(age_81plus_sum), 
         age_unknown_sum_new = age_unknown_sum - lag(age_unknown_sum), 
         age_0to5_m_new = age_0to5_m - lag(age_0to5_m), 
         age_6to10_m_new = age_6to10_m - lag(age_6to10_m), 
         age_11to20_m_new = age_11to20_m - lag(age_11to20_m),
         age_21to40_m_new = age_21to40_m - lag(age_21to40_m), 
         age_41to60_m_new = age_41to60_m - lag(age_41to60_m), 
         age_61to80_m_new = age_61to80_m - lag(age_61to80_m), 
         age_81plus_m_new = age_81plus_m - lag(age_81plus_m), 
         age_unknown_m_new = age_unknown_m - lag(age_unknown_m), 
         age_0to5_w_new = age_0to5_w - lag(age_0to5_w), 
         age_6to10_w_new = age_6to10_w - lag(age_6to10_w), 
         age_11to20_w_new = age_11to20_w - lag(age_11to20_w),
         age_21to40_w_new = age_21to40_w - lag(age_21to40_w), 
         age_41to60_w_new = age_41to60_w - lag(age_41to60_w), 
         age_61to80_w_new = age_61to80_w - lag(age_61to80_w), 
         age_81plus_w_new = age_81plus_w - lag(age_81plus_w), 
         age_unknown_w_new = age_unknown_w - lag(age_unknown_w), 
         age_0to5_na_new = age_0to5_na - lag(age_0to5_na), 
         age_6to10_na_new = age_6to10_na - lag(age_6to10_na), 
         age_11to20_na_new = age_11to20_na - lag(age_11to20_na),
         age_21to40_na_new = age_21to40_na - lag(age_21to40_na), 
         age_41to60_na_new = age_41to60_na - lag(age_41to60_na), 
         age_61to80_na_new = age_61to80_na - lag(age_61to80_na), 
         age_81plus_na_new = age_81plus_na - lag(age_81plus_na), 
         age_unknown_na_new = age_unknown_na - lag(age_unknown_na)
  ) %>%
  arrange(Datum)


## subtract dead cases from ambulant cases (to be in line with previous
## reporting from GS)
input_data <- input_data %>%
  mutate(casesMunich_amb = casesMunich_amb - casesMunich_dead)

## focus on three most recent days / Session IDs are used if ties occurre
recent_data_3d <- input_data %>%
  arrange(Datum) %>% 
  filter(row_number() >= (n() - 2)) %>% 
  as.data.frame()

## focus on seven most recent days / Session IDs are used if ties occurre
recent_data_7d <- input_data %>%
  arrange(Datum) %>% 
  filter(row_number() >= (n() - 6)) %>% 
  as.data.frame()


fatalities_a1 <- readRDS(paste0(wd_path1,"fatalities_a1.rds"))
fatalities_a2 <- readRDS(paste0(wd_path1,"fatalities_a2.rds"))
fatalities_a3 <- readRDS(paste0(wd_path1,"fatalities_a3.rds"))

# prepare: Patientensetting ----------------------------------------------------

StatusPS_joined <- readRDS(paste0(wd_path1,"StatusPS_joined.rds"))


# prepare: Infektionsort -------------------------------------------------------

Infektionsort_tab <- readRDS(paste0(wd_path1,"Infektionsort_tab.rds"))


# prepare: Impfungen -----------------------------------------------------------


## Aicher Data (Impfungen der Bevölkerung)
aicher_vacc_data <- readxl::read_xlsx(
  paste0(wd_path6, "ImpfenAAU.xlsx"), 
  guess_max = 100000
)

## copy to shared folder
writexl::write_xlsx(aicher_vacc_data, str_glue("{shared_data}ImpfenAAU.xlsx"))

## prepare data
aicher_vacc_data <- aicher_vacc_data %>%
  group_by(as.Date(`Datum`)) %>%
  filter(`ObjectID` == max(`ObjectID`)) %>%
  filter(!is.na(`Datum`)) %>% 
  ungroup() %>% 
  arrange(`Datum`)

aicher_vacc_data <- aicher_vacc_data %>%
  mutate(`Mobil Erste Impfung` = ifelse(is.na(`Mobil Erste Impfung`) == TRUE, 0, `Mobil Erste Impfung`)) %>%
  mutate(`Mobil Zweite Impfung` = ifelse(is.na(`Mobil Zweite Impfung`) == TRUE, 0, `Mobil Zweite Impfung`)) %>%
  mutate(`Messe Erste Impfung` = ifelse(is.na(`Messe Erste Impfung`) == TRUE, 0, `Messe Erste Impfung`)) %>%
  mutate(`Messe Zweite Impfung` = ifelse(is.na(`Messe Zweite Impfung`) == TRUE, 0, `Messe Zweite Impfung`)) %>%
  mutate(`Tagessumme` = `Mobil Erste Impfung` + `Mobil Zweite Impfung` + `Messe Erste Impfung` + `Messe Zweite Impfung`) %>%
  mutate(`Impfdosen erhalten kum` = cumsum(`Impfdosen erhalten`)) %>%
  mutate(`an Kliniken abgegeben kum` = cumsum(`an Kliniken abgegeben`)) %>%
  mutate(`Restbestand diff` = `Restbestand` - lag(`Restbestand`)) %>%
  mutate(`Verwurf kum` = cumsum(`Verwurf`)) %>%
  mutate(`Mobil Erste Impfung kum` = cumsum(`Mobil Erste Impfung`)) %>%
  mutate(`Mobil Zweite Impfung kum` = cumsum(`Mobil Zweite Impfung`)) %>%
  mutate(`Messe Erste Impfung kum` = cumsum(`Messe Erste Impfung`)) %>%
  mutate(`Messe Zweite Impfung kum` = cumsum(`Messe Zweite Impfung`)) %>%
  mutate(`Tagessumme kum` = cumsum(`Tagessumme`)) 


## Impfquote Data

iq_data <- readxl::read_xlsx(
  paste0(wd_path6, "Impfquote.xlsx"), 
  guess_max = 100000
)

## copy raw data
vacc_data <- iq_data

## copy to shared folder
writexl::write_xlsx(iq_data, str_glue("{shared_data}Impfquote.xlsx"))

## prepare data

octo_complete_history <- readRDS(paste0(wd_path1, "octo_data_history.rds"))

octo_kurz <-
  readxl::read_excel(
    paste0(user_input, "octoware_kurz.xlsx"),
    guess_max = 100000
  )

octo <-
  readxl::read_excel(
    paste0(user_input, "octoware.xlsx"),
    guess_max = 100000
  )

octo_ID <- rbind(
  octo_kurz,
  octo %>%
    filter(Meldedatum < min(octo_kurz$Meldedatum))
  )

## see script 25_ function add_vaccination_status()
octo_ID <- add_vaccination_status(octo_ID)
octo_ID$Impfdurchbruch <- octo_ID$vollstaendig_geimpft

iq_data <- iq_data %>%
  group_by(as.Date(`Datum`)) %>%
  filter(`ObjectID` == max(`ObjectID`)) %>%
  filter(!is.na(`Datum`)) %>% 
  ungroup() %>% 
  arrange(`Datum`) %>%
  
  full_join(octo_ID %>%
              filter(Impfdurchbruch == 1) %>%
              mutate(Datum  =
                       as.Date(Meldedatum, format = "%Y-%m-%d")) %>%
              filter(!is.na(Datum)) %>%
              group_by(Datum) %>%
              summarise(n = n()) %>%
              arrange(Datum) %>%
              mutate(casesMunich_ID = cumsum(n)) %>%
              dplyr::select(Datum, casesMunich_ID),
            by = c("Datum"))

iq_data <- iq_data %>%
  arrange(Datum)

iq_data$casesMunich_ID <- ifelse(is.na(iq_data$casesMunich_ID) == TRUE, 
                                 na.locf(iq_data$casesMunich_ID), 
                                 iq_data$casesMunich_ID)

iq_data <- iq_data %>%
  mutate(`Erstimpfung Kliniken Plus` = `Erstimpfung Kliniken` - lag(`Erstimpfung Kliniken`)) %>%
  mutate(`Erstimpfung Praxen Plus` = `Erstimpfung Praxen` - lag(`Erstimpfung Praxen`)) %>%
  mutate(`Erstimpfung Impfzentrum Messe Plus` = `Erstimpfung Impfzentrum Messe` - lag(`Erstimpfung Impfzentrum Messe`)) %>%
  mutate(`Erstimpfung Mobil Plus` = `Erstimpfung Mobil` - lag(`Erstimpfung Mobil`)) %>%
  mutate(`Erstimpfung Isarklinikum Plus` = `Erstimpfung Isarklinikum` - lag(`Erstimpfung Isarklinikum`)) %>%
  mutate(`Erstimpfung Impfzentrum Gesamt` = `Erstimpfung Impfzentrum Messe` + `Erstimpfung Mobil` + `Erstimpfung Isarklinikum`) %>%
  mutate(`Erstimpfung Impfzentrum Gesamt Plus` = `Erstimpfung Impfzentrum Gesamt` - lag(`Erstimpfung Impfzentrum Gesamt`)) %>%
  mutate(`Erstimpfung Gesamt` = `Erstimpfung Kliniken` + `Erstimpfung Praxen` + 
           `Erstimpfung Impfzentrum Messe` + `Erstimpfung Mobil` + `Erstimpfung Isarklinikum`) %>%
  mutate(`Erstimpfung Gesamt Plus` = `Erstimpfung Gesamt` - lag(`Erstimpfung Gesamt`)) %>%
  mutate(`Erstimpfung Quote alle` = round(`Erstimpfung Gesamt` * 100 / EW_MUC, 1)) %>%
  mutate(`Erstimpfung Quote 12+` = round(`Erstimpfung Gesamt` * 100 / 1319090, 1)) %>%
  mutate(`Erstimpfung Quote 18+` = round(`Erstimpfung Gesamt` * 100 / 1250071, 1)) %>%
  mutate(`Zweitimpfung Kliniken Plus` = `Zweitimpfung Kliniken` - lag(`Zweitimpfung Kliniken`)) %>%
  mutate(`Zweitimpfung Praxen Plus` = `Zweitimpfung Praxen` - lag(`Zweitimpfung Praxen`)) %>%
  mutate(`Zweitimpfung Impfzentrum Messe Plus` = `Zweitimpfung Impfzentrum Messe` - lag(`Zweitimpfung Impfzentrum Messe`)) %>%
  mutate(`Zweitimpfung Mobil Plus` = `Zweitimpfung Mobil` - lag(`Zweitimpfung Mobil`)) %>%
  mutate(`Zweitimpfung Isarklinikum Plus` = `Zweitimpfung Isarklinikum` - lag(`Zweitimpfung Isarklinikum`)) %>%
  mutate(`Zweitimpfung Impfzentrum Gesamt` = `Zweitimpfung Impfzentrum Messe` + `Zweitimpfung Mobil` + `Zweitimpfung Isarklinikum`) %>%
  mutate(`Zweitimpfung Impfzentrum Gesamt Plus` = `Zweitimpfung Impfzentrum Gesamt` - lag(`Zweitimpfung Impfzentrum Gesamt`)) %>%
  mutate(`Zweitimpfung Gesamt` = `Zweitimpfung Kliniken` + `Zweitimpfung Praxen` + 
           `Zweitimpfung Impfzentrum Messe` + `Zweitimpfung Mobil` + `Zweitimpfung Isarklinikum`) %>%
  mutate(`Zweitimpfung Gesamt Plus` = `Zweitimpfung Gesamt` - lag(`Zweitimpfung Gesamt`)) %>%
  mutate(`Zweitimpfung Quote alle` = round(`Zweitimpfung Gesamt` * 100 / EW_MUC, 1)) %>%
  mutate(`Zweitimpfung Quote 12+` = round(`Zweitimpfung Gesamt` * 100 / 1319090, 1)) %>%
  mutate(`Zweitimpfung Quote 18+` = round(`Zweitimpfung Gesamt` * 100 / 1250071, 1)) %>%
  mutate(`Impfung Gesamt` = `Erstimpfung Gesamt` + `Zweitimpfung Gesamt`) %>%
  mutate(`Impfung Gesamt Plus` = `Erstimpfung Gesamt Plus` + `Zweitimpfung Gesamt Plus`) %>%
  mutate(`Impfdurchbrueche` = `casesMunich_ID`) %>%
  mutate(`Impfdurchbrueche Plus` = `casesMunich_ID` - lag(`casesMunich_ID`)) %>%
  mutate(`Impfdurchbrueche Quotient` = round(`casesMunich_ID` * 100 / lag(`Zweitimpfung Gesamt`, 15), 2)) %>%
  mutate(`Impfdurchbrueche letzte 28 Tage` = `casesMunich_ID` - lag(`casesMunich_ID`, 29)) %>%
  mutate(`Impfdurchbrueche Quotient2` = round(`Impfdurchbrueche letzte 28 Tage` * 100 / sum(octo_ID$Meldedatum > Sys.Date() -29), 2))

iq_data <- iq_data %>%
  filter(Datum < Sys.Date()) 
  
iq_data_3d <- iq_data %>%
  arrange(Datum) %>% 
  filter(row_number() >= (n() - 2)) %>% 
  as.data.frame()

## BIK Data (Impfungen von Krankenhauspersonal)

Impfungen_data <- read.csv(
  paste0(wd_path2, "Impfungen_Muenchen.csv"), sep=";")

## copy to shared folder
write.table(Impfungen_data, str_glue("{shared_data}Impfungen_Muenchen.csv"),
            sep = ";")
## copy to Branddirektion
write.table(Impfungen_data, str_glue("{wd_path4}Impfungen_Muenchen.csv"),
            sep = ";")

names_vacc <- colnames(Impfungen_data)
names_vacc <- names_vacc[which(!(names_vacc %in%  c("ID", "Datum")))]

Impfungen_data <- Impfungen_data %>%
  mutate(Datum = as.Date(Datum, format = "%d.%m.%Y")) %>%
  arrange(Datum)

Impfungen_temp <- Impfungen_data
Impfungen_temp <- apply(Impfungen_temp[, names_vacc], 2, cumsum)
Impfungen_temp <- as.data.frame(Impfungen_temp)
colnames(Impfungen_temp) <- paste0(colnames(Impfungen_temp), "_kum")

Impfungen_data <- cbind(Impfungen_data, Impfungen_temp)

Impfungen_data_3d <- Impfungen_data %>%
  arrange(Datum) %>% 
  filter(row_number() >= (n() - 2)) %>% 
  as.data.frame()

# prepare: age over time -------------------------------------------------------

## load mean age
mean_age_per_week <- readRDS(paste0(wd_path1,"mean_age_per_week.rds"))
mean_age_per_date <- readRDS(paste0(wd_path1,"mean_age_per_date.rds"))

Alter_Erstmeldung_var <- readRDS(
  paste0(wd_path1,"Alter_Erstmeldung_var.rds"))

Alter_Erstmeldung_prev <- readRDS(
  paste0(wd_path1,"Alter_Erstmeldung_prev.rds"))


## age by group over time
age_grouped_series_data <- input_data %>%
  dplyr::select(Datum, 
                age_0to5_sum_new,
                age_6to10_sum_new,
                age_11to20_sum_new,
                age_21to40_sum_new,
                age_41to60_sum_new,
                age_61to80_sum_new,
                age_81plus_sum_new) %>%
  filter(Datum <= max(recent_data_3d$Datum)) %>%
  mutate(
    
    ## 7 days
    
    age_0to5_7d = 
      zoo::rollapply(input_data$age_0to5_sum_new, 7, 
                     sum, fill = NA, 
                     align = "right"),
    age_6to10_7d = 
      zoo::rollapply(input_data$age_6to10_sum_new, 7, 
                     sum, fill = NA, align = "right"),
    age_11to20_7d = 
      zoo::rollapply(input_data$age_11to20_sum_new, 7, 
                     sum, fill = NA, align = "right"),
    age_21to40_7d = 
      zoo::rollapply(input_data$age_21to40_sum_new, 7,
                     sum, fill = NA, align = "right"),
    age_41to60_7d = 
      zoo::rollapply(input_data$age_41to60_sum_new, 7, 
                     sum, fill = NA, align = "right"),
    age_61to80_7d = 
      zoo::rollapply(input_data$age_61to80_sum_new, 7, 
                     sum, fill = NA, align = "right"),
    age_81plus_7d = 
      zoo::rollapply(input_data$age_81plus_sum_new, 7, 
                     sum, fill = NA, align = "right"),
    
    ## 14 days
    
    age_0to5_14d = 
           zoo::rollapply(input_data$age_0to5_sum_new, 14, 
                          sum, fill = NA, 
                          align = "right"),
         age_6to10_14d = 
           zoo::rollapply(input_data$age_6to10_sum_new, 14, 
                          sum, fill = NA, align = "right"),
         age_11to20_14d = 
           zoo::rollapply(input_data$age_11to20_sum_new, 14, 
                          sum, fill = NA, align = "right"),
         age_21to40_14d = 
           zoo::rollapply(input_data$age_21to40_sum_new, 14,
                          sum, fill = NA, align = "right"),
         age_41to60_14d = 
           zoo::rollapply(input_data$age_41to60_sum_new, 14, 
                          sum, fill = NA, align = "right"),
         age_61to80_14d = 
           zoo::rollapply(input_data$age_61to80_sum_new, 14, 
                          sum, fill = NA, align = "right"),
         age_81plus_14d = 
           zoo::rollapply(input_data$age_81plus_sum_new, 14, 
                          sum, fill = NA, align = "right")) %>%
  arrange(Datum)

## replace NAs
age_grouped_series_data[is.na(age_grouped_series_data)] <- 0

# octoware data ----------------------------------------------------------------

## fetch octoware data for calculating 'Verdopplungszahl'
octo_complete <- readRDS(paste0(wd_path1, 'octoware_data.rds'))

## function 'Verdopplungszahl'
doubling_days <- function(cases_vector, current_only = FALSE) {
  
  ## T
  t_T <- length(cases_vector)
  
  if (current_only == TRUE) {
    ## just the current value
    days_till_doubled <- t_T - 
      max(which(cases_vector <= 0.5 * cases_vector[t_T]))
  } else if (current_only == FALSE) {
    ## track development
    days_till_doubled <- list()
    for (i in (2:t_T)) {
      
      days_till_doubled[i - 1] <-
        i - max(which(cases_vector <= 0.5*cases_vector[i]))
    }
    days_till_doubled <- c(0,days_till_doubled)
  }
  days_till_doubled <- unlist(days_till_doubled)
  days_till_doubled
}

## add Verdopplungszahl to data
octo_complete <- octo_complete %>%
  arrange(Datum) %>%
  mutate(VerdopplungsZahlMUC = doubling_days(octo_complete$casesMunich))

## recent version
octo_complete_recent <- octo_complete %>%
  arrange(Datum) %>% 
  filter(row_number() >= (n() - 2)) %>% 
  as.data.frame()


# prepare data: Rt -------------------------------------------------------------

## read R(t) values
Rt_muenchen <- readRDS(paste0(wd_path1,"Rt_muenchen.rds"))
Rt_bayern <- readRDS(paste0(wd_path1,"Rt_bayern.rds"))

## covert to date type
Rt_muenchen$Datum <- as.Date(Rt_muenchen$Datum)
Rt_bayern$Datum <- as.Date(Rt_bayern$Datum)


# prepare data: Aicher ---------------------------------------------------------

## read latest aicher_data
aicher_data <- readr::read_csv(
  paste0(wd_path2, "results-survey669457.csv"))

## filter according to highest session value
input_data_aicher <- aicher_data %>%
  group_by(as.Date(`Datum des Datenstandes`)) %>%
  filter(`Antwort ID` == max(`Antwort ID`)) %>%
  filter(!is.na(`Datum des Datenstandes`))


## read testungen data
testungen_data <- readxl::read_xlsx(
  paste0(wd_path6, "TestungenAAU.xlsx"), 
  guess_max = 100000
  )

schnelltests_historie <- readxl::read_xlsx(
  paste0(wd_path6, "Schnelltests_AAU_Historie.xlsx"), 
  guess_max = 100000
)

## copy to shared folder
writexl::write_xlsx(testungen_data, str_glue("{shared_data}TestungenAAU.xlsx"))
writexl::write_xlsx(schnelltests_historie, str_glue("{shared_data}Schnelltests_AAU_Historie.xlsx"))

## prepare data
testungen_data <- testungen_data %>%
  group_by(as.Date(`Datum auswählen`)) %>%
  filter(`ObjectID` == max(`ObjectID`)) %>%
  filter(!is.na(`Datum auswählen`)) %>% 
  ungroup() %>%
  arrange(`Datum auswählen`)

testungen_data$`Theresienwiese Schnelltest stat. Einrichtungen – angemeldet` [1:77] <- 
  testungen_data$`Theresienwiese Schnelltest - angemeldet` [1:77]

testungen_data$`Theresienwiese Schnelltest stat. Einrichtungen – getestet` [1:77] <- 
  testungen_data$`Theresienwiese Schnelltest - getestet` [1:77]  

testungen_data$`Theresienwiese Schnelltest Schule – angemeldet` [1:77] <-
  schnelltests_historie$`Theresienwiese Schnelltest Schule – angemeldet` [1:77]

testungen_data$`Theresienwiese Schnelltest Schule – getestet` [1:77] <-
  schnelltests_historie$`Theresienwiese Schnelltest Schule – getestet` [1:77]

testungen_data$`Theresienwiese Schnelltest Jedermann – angemeldet` [1:77] <-
  schnelltests_historie$`Theresienwiese Schnelltest Jedermann – angemeldet` [1:77]

testungen_data$`Theresienwiese Schnelltest Jedermann – getestet` [1:77] <-
  schnelltests_historie$`Theresienwiese Schnelltest Jedermann – getestet` [1:77]

testungen_data <- testungen_data %>% 
  mutate(`Mobile Entnahmen durchgeführt` = `Mobile Einzeltestung` + `Mobile Reihentestung`) %>%
  mutate(`Jedermann-Testungen/Corona-Warn-App durchgeführt` = 
           `Theresienwiese "Jedermann" freiwillig - getestet` + 
           `Theresienwiese "Jedermann" COVID-APP - getestet` + 
           `Theresienwiese "Jedermann" Rückreise - getestet`) %>%
  # Kumulierte Summe beginnend mit Datenstand vom 27.01.2021 (+) und ggf. Korrektur für Fehleingaben seit 27.01.2021 (-)
  mutate(`Mobile Einzeltestung_kum` = cumsum(`Mobile Einzeltestung`) + 10935) %>%
  mutate(`Mobile Reihentestung_kum` = cumsum(`Mobile Reihentestung`) + 6094) %>%
  mutate(`Mobile Entnahmen durchgeführt_kum` = `Mobile Einzeltestung_kum` + `Mobile Reihentestung_kum`) %>%
  mutate(`Theresienwiese "Jedermann" - Anmeldungen_kum` = cumsum(`Theresienwiese "Jedermann" - Anmeldungen`) + 192029) %>%
  mutate(`Theresienwiese "Jedermann" freiwillig - getestet_kum` = cumsum(`Theresienwiese "Jedermann" freiwillig - getestet`) + 126072) %>%
  mutate(`Theresienwiese "Jedermann" COVID-APP - getestet_kum` = cumsum(`Theresienwiese "Jedermann" COVID-APP - getestet`) + 7888) %>%
  mutate(`Theresienwiese "Jedermann" Rückreise - getestet_kum` = cumsum(`Theresienwiese "Jedermann" Rückreise - getestet`) + 22294) %>%
  mutate(`Jedermann-Testungen/Corona-Warn-App durchgeführt_kum` = 
           `Theresienwiese "Jedermann" freiwillig - getestet_kum` + 
           `Theresienwiese "Jedermann" COVID-APP - getestet_kum` + 
           `Theresienwiese "Jedermann" Rückreise - getestet_kum`) %>%
  mutate(`Theresienwiese KP1 - getestet_kum` = cumsum(`Theresienwiese KP1 - getestet`) + 10257) %>%
  mutate(`Theresienwiese KRITIS - getestet_kum` = cumsum(`Theresienwiese KRITIS - getestet`) + 1093) %>%
  mutate(`RBS - getestet_kum` = cumsum(`RBS - getestet`) + 758) %>%
  mutate(`Theresienwiese Schnelltest stat. Einrichtungen angemeldet_kum` = cumsum(`Theresienwiese Schnelltest stat. Einrichtungen – angemeldet`) + 754 - 51) %>%
  mutate(`Theresienwiese Schnelltest stat. Einrichtungen getestet_kum` = cumsum(`Theresienwiese Schnelltest stat. Einrichtungen – getestet`) + 668 - 43) %>%
  mutate(`Theresienwiese Schnelltest Schule angemeldet_kum` = cumsum(`Theresienwiese Schnelltest Schule – angemeldet`)) %>%
  mutate(`Theresienwiese Schnelltest Schule getestet_kum` = cumsum(`Theresienwiese Schnelltest Schule – getestet`)) %>%
  mutate(`Theresienwiese Schnelltest Jedermann angemeldet_kum` = cumsum(`Theresienwiese Schnelltest Jedermann – angemeldet`)) %>%
  mutate(`Theresienwiese Schnelltest Jedermann getestet_kum` = cumsum(`Theresienwiese Schnelltest Jedermann – getestet`)) %>%
  mutate(`Theresienwiese - PCR-Test nach pos. Schnelltest` = ifelse(is.na(`Theresienwiese - PCR-Test nach pos. Schnelltest`) == TRUE, 0, `Theresienwiese - PCR-Test nach pos. Schnelltest`)) %>%
  mutate(`Theresienwiese - PCR-Test nach pos. Schnelltest_kum` = cumsum(`Theresienwiese - PCR-Test nach pos. Schnelltest`)) %>%
  mutate(`Mobile Schultestung - geplant_kum` = cumsum(`Mobile Schultestung - geplant`) + 4636) %>%
  mutate(`Mobile Schultestung - getestet_kum` = cumsum(`Mobile Schultestung - getestet`) + 2764) %>%
 
  # Kumulierte Summe ab 16.03.2020
  mutate(`GSR-Theresienwiese (seit 16.03.)` = cumsum(`Theresienwiese KP1 - getestet`) + cumsum(`Theresienwiese KRITIS - getestet`) + 14282) %>%
  mutate(`Mobile Entnahmen (seit 16.03.)` = cumsum(`Mobile Einzeltestung`) + cumsum(`Mobile Reihentestung`) + 29284)


## prepare data: Selbsttests Schulen

selbsttests_schulen <- readxl::read_xlsx(
  paste0(wd_path2, "Selbsttests_Schulen.xlsx"), 
  guess_max = 100000
)

## copy to shared folder
writexl::write_xlsx(selbsttests_schulen, str_glue("{shared_data}Selbsttests_Schulen.xlsx"))


## use last 3 weeks only

selbsttests_schulen <- subset(selbsttests_schulen, is.na(`Tests durchgeführt`) == FALSE)

selbsttests_schulen <- selbsttests_schulen %>% 
  mutate(`Positivrate` = round(100 * `Tests positiv` / `Tests durchgeführt`, 2)) 
  

# prepare data: CoVe (and RKI Risk)---------------------------------------------

## read latest CoVe data
cove_data_gesamt <- readRDS(paste0(wd_path1, "cove_data_bericht_gesamt.rds"))
cove_data_muc <- readRDS(paste0(wd_path1, "cove_data_bericht_muenchen.rds"))

## most recent data from CoVe
recent_cove_data_gesamt <- cove_data_gesamt %>%
  as.data.frame() %>%
  arrange(as.Date(temp_date)) %>%
  filter(row_number() >= (n() - 2))

recent_cove_data_muc <- cove_data_muc %>%
  as.data.frame() %>%
  arrange(as.Date(temp_date)) %>%
  filter(row_number() >= (n() - 2))

## prepare data: BaySIM --------------------------------------------------------

## siehe 18er skript
ctt_cases_total <- readRDS(paste0(wd_path1, 'ctt_cases_total.rds'))

# prepare data: Einrichtungen --------------------------------------------------

## read last einrichtungen_data
einrichtungen_data <- readr::read_csv(
  paste0(wd_path2,
         "results-survey938677.csv"))

## new variables
facilities_data <- einrichtungen_data %>%
  mutate(Datum = `Für welches Datum werden Ausbrüche gemeldet?`,
         Team = `Diese Eingabe erfolgt durch Team...`,
         Sum_Outbreaks = `Um wie viele Ausbrüche handelt es sich insgesamt?`,
         Type_Facility = `Um welche Einrichtungsart handelt es sich?`,
         Sum_Facilities = `Wie viele Einrichtungen dieser Art sind derzeit von Ausbrüchen betroffen?`,
         Sum_Staff = `Wie viele Mitarbeiter*innen / Lehrer*innen sind PCR-bestätigte Fälle bei den aktuellen Ausbrüchen?`,
         Sum_Patients = `Wie viele Bewohner*innen / Patient*innen / Schüler*innen / KiTa-Kinder / Reiseteilnehmer*innen / ggf. assoziierte Personen sind PCR-bestätigte Fälle bei den aktuellen Ausbrüchen?`
  ) %>%
  dplyr::select(`Antwort ID`, Datum, Team, Sum_Outbreaks, Type_Facility, 
                Sum_Facilities, Sum_Staff, Sum_Patients)


## most recent data from facilities
recent_einrichtungen_data <- facilities_data %>%
  ungroup() %>%
  ## newer entries per day replace older ones
  group_by(as.Date(Datum), Type_Facility) %>%
  filter(`Antwort ID` == max(`Antwort ID`)) %>%
  ungroup() %>%
  filter(as.Date(Datum) == max(as.Date(Datum), na.rm = TRUE)) %>%
  ## alphabetical order
  arrange(Type_Facility)


## prepare export to bfm
facilities_bfm_data <- facilities_data %>%
  ungroup() %>%
  ## newer entries per day replace older ones
  group_by(as.Date(Datum), Type_Facility) %>%
  filter(`Antwort ID` == max(`Antwort ID`)) %>%
  ungroup() %>%
  dplyr::select(-`as.Date(Datum)`) %>%
  ## alphabetical order
  arrange(as.Date(Datum), Type_Facility) %>%
  .[!is.na(.$Datum), ]
  

# prepare data: Ausland --------------------------------------------------------

ExpLand_new_tab <- readRDS(paste0(wd_path1,"ExpLand_new_tab.rds"))
ExpLand_recent_tab <- readRDS(paste0(wd_path1,"ExpLand_recent_tab.rds"))

# prepare data: LGL ------------------------------------------------------------

lgl_branddir_tab <- readRDS(paste0(wd_path1, "lgl_branddir_tab.rds"))

# prepare data: IVENA (Bettenmeldung) ------------------------------------------

ivena_data <- readRDS(paste0(wd_path1, 'ivena_data.rds')) %>% 
  arrange(export_datetime) %>%
  mutate(export_date = as.Date(export_datetime)) %>%
  group_by(art, export_date) %>%
  slice_max(export_datetime, n = 1, with_ties = FALSE) %>%
  ungroup() %>% 
  group_by(art) %>%
  mutate(ist_inz = c(0, diff(ist))) %>% 
  mutate(max_inz = c(0, diff(max))) %>%
  mutate(belegt_inz = c(0, diff(belegt))) %>%
  mutate(belegt_covid_bestaetigt_inz = c(0, diff(belegt_covid_bestaetigt))) %>%
  mutate(belegt_covid_verdacht_inz = c(0, diff(belegt_covid_verdacht))) %>%
  mutate(belegt_ecmo_inz = c(0, diff(belegt_ecmo))) %>%
  mutate(frei_inz = c(0, diff(frei))) %>%
  mutate(frei_covid_inz = c(0, diff(frei_covid))) %>%
  mutate(frei_ecmo_inz = c(0, diff(frei_ecmo))) %>%
  slice_max(export_date, n=1) %>%
  ungroup() %>% 
  mutate(ist = sprintf('%d (%+d)', ist, ist_inz)) %>% 
  mutate(max = sprintf('%d (%+d)', max, max_inz)) %>%
  mutate(belegt = sprintf('%d (%+d)', belegt, belegt_inz)) %>%
  mutate(belegt_covid_bestaetigt = sprintf('%d (%+d)', belegt_covid_bestaetigt, belegt_covid_bestaetigt_inz)) %>%
  mutate(belegt_covid_verdacht = sprintf('%d (%+d)', belegt_covid_verdacht, belegt_covid_verdacht_inz)) %>%
  mutate(belegt_ecmo = sprintf('%d (%+d)', belegt_ecmo, belegt_ecmo_inz)) %>%
  mutate(frei = sprintf('%d (%+d)', frei, frei_inz)) %>%
  mutate(frei_covid = sprintf('%d (%+d)', frei_covid, frei_covid_inz)) %>%
  mutate(frei_ecmo = sprintf('%d (%+d)', frei_ecmo, frei_ecmo_inz)) %>%
  mutate(frei_ecmo = replace(frei_ecmo, grep("NA ", frei_ecmo), "")) %>%
  mutate(belegt_ecmo = replace(belegt_ecmo, grep("NA ", belegt_ecmo), ""))
ivena_data_date <- max(ivena_data$export_date)
ivena_data <- ivena_data %>%
  dplyr::select(-export_datetime, -export_date, -ends_with('_inz')) %>%
  pivot_longer(c(-art)) %>% 
  pivot_wider(names_from = c(art), values_from = value) %>%
  rename(Kategorie=name)


# export data to BFM -----------------------------------------------------------

#'@details Tagesbericht (all versions) export these files to BFM. Only the 
#'aktuelle_kennzahlen file does not automatically export data.


  
  #'@details We export certain features to BFM.
  
  ## daily number of infections, 7 days incidence, actively infected
  new_cases_bfm_munich <-  input_data %>%
    dplyr::select(Datum, casesMunich,
                  casesMunich_Inz) %>%
    mutate(casesMunichKum = casesMunich) %>%
    mutate(Inz7days = zoo::rollapply(input_data$casesMunich_Inz, 
                                     7, sum, fill = NA, align = "right") %>%
             # ## Einwohnerzahl aus RKI Dashboard
             '/'(EW_MUC / 100000) %>% tail(., n = (nrow(input_data))) %>%
             round(., digits = 2)) %>%
    mutate(akut_infiziert = c(rep(NA, 13), 
                              zoo::rollapply(casesMunich_Inz, 14, sum))) %>%
    na.omit() %>%
    dplyr::select(Datum, casesMunichKum, casesMunich_Inz, Inz7days, akut_infiziert)
  
  new_cases_bfm_bavaria <-  input_data %>%
    dplyr::select(Datum, casesBavariaKum,
                  casesBavaria_Inz) %>%
    mutate(Inz7days = zoo::rollapply(input_data$casesBavaria_Inz, 
                                     7, sum, fill = NA, align = "right") %>%
             ## Neue Zahl, 08.10.2020
           '/'(131.24737) %>% tail(., n = (nrow(input_data))) %>%
             round(., digits = 2)) %>%
    mutate(akut_infiziert = c(rep(NA, 13), 
                              zoo::rollapply(casesBavaria_Inz, 14, sum))) %>%
    na.omit()
  
  new_cases_bfm_germany <-  input_data %>%
    dplyr::select(Datum, casesGermanyKum,
                  casesGermany_Inz) %>%
    mutate(Inz7days = zoo::rollapply(input_data$casesGermany_Inz, 
                                     7, sum, fill = NA, align = "right") %>%
             ## Neue Zahl, 08.10.2020
             '/'(831.66711) %>% tail(., n = (nrow(input_data))) %>%
             round(., digits = 2)) %>%
    mutate(akut_infiziert = c(rep(NA, 13), 
                              zoo::rollapply(casesGermany_Inz, 14, sum))) %>%
    na.omit()
  
  ## Anzahl der Tests pro Tag
  testungen_bfm_data <- input_data_aicher %>%
    dplyr::select(`Antwort ID`, 
                  `Anzahl der durchgeführten Testungen: KVB Testung`,
                  `Anzahl der durchgeführten Testungen: Drive in Kritis`,
                  `Anzahl der durchgeführten Testungen: Drive in KP1`,
                  `Anzahl der durchgeführten Testungen: Mobile Entnahmen`,
                  `Testungen gesamt seit dem 16.03.2020: KVB Theresienwiese`,
                  `Testungen gesamt seit dem 16.03.2020: RGU Theresienwiese`,
                  `Testungen gesamt seit dem 16.03.2020: Mobile Entnahmen`) %>%
    rename(`Anzahl der Jedermann-Testungen / Corona Warn App` = 
             `Anzahl der durchgeführten Testungen: KVB Testung`,
           `Anzahl der durchgefuehrten Testungen Drive in Kritis` = 
             `Anzahl der durchgeführten Testungen: Drive in Kritis`,
           `Anzahl der durchgefuehrten Testungen Drive in KP1` = 
             `Anzahl der durchgeführten Testungen: Drive in KP1`,
           `Anzahl der durchgefuehrten Testungen Mobile Entnahmen` = 
             `Anzahl der durchgeführten Testungen: Mobile Entnahmen`,
           `Testungen gesamt seit dem 30.07.2020 Jedermann-Testungen / Corona Warn App` = 
             `Testungen gesamt seit dem 16.03.2020: KVB Theresienwiese`,
           `Testungen gesamt seit dem 16.03.2020 RGU Theresienwiese` = 
             `Testungen gesamt seit dem 16.03.2020: RGU Theresienwiese`,
           `Testungen gesamt seit dem 16.03.2020 Mobile Entnahmen` = 
             `Testungen gesamt seit dem 16.03.2020: Mobile Entnahmen`)
  
  ## Automatische Berechnung der kumulativen Fallzahlen 
  ## (ab 1.1.2021 (Zeile 197), da vorher Inkonsistenzen vorhanden)
  
  testungen_bfm_data$`Testungen gesamt seit dem 30.07.2020 Jedermann-Testungen / Corona Warn App`[197:nrow(testungen_bfm_data)] <- 0
  testungen_bfm_data$`Testungen gesamt seit dem 16.03.2020 RGU Theresienwiese`[197:nrow(testungen_bfm_data)] <- 0
  testungen_bfm_data$`Testungen gesamt seit dem 16.03.2020 Mobile Entnahmen`[197:nrow(testungen_bfm_data)] <- 0
  
  
  for (i in 197:nrow(testungen_bfm_data)) {
    testungen_bfm_data$`Testungen gesamt seit dem 30.07.2020 Jedermann-Testungen / Corona Warn App`[i] <-
      sum(testungen_bfm_data$`Testungen gesamt seit dem 30.07.2020 Jedermann-Testungen / Corona Warn App`[i-1],
          testungen_bfm_data$`Anzahl der Jedermann-Testungen / Corona Warn App`[i], 
          na.rm = T)
  }
  
  
  for (i in 197:nrow(testungen_bfm_data)) {
    testungen_bfm_data$`Testungen gesamt seit dem 16.03.2020 RGU Theresienwiese`[i] <-
      sum(testungen_bfm_data$`Testungen gesamt seit dem 16.03.2020 RGU Theresienwiese`[i-1],
          testungen_bfm_data$`Anzahl der durchgefuehrten Testungen Drive in Kritis`[i], 
          testungen_bfm_data$`Anzahl der durchgefuehrten Testungen Drive in KP1`[i], 
          na.rm = T)
  }
  
  
  for (i in 197:nrow(testungen_bfm_data)) {
    testungen_bfm_data$`Testungen gesamt seit dem 16.03.2020 Mobile Entnahmen`[i] <-
      sum(testungen_bfm_data$`Testungen gesamt seit dem 16.03.2020 Mobile Entnahmen`[i-1],
          testungen_bfm_data$`Anzahl der durchgefuehrten Testungen Mobile Entnahmen`[i], 
          na.rm = T)
  }
  
  ## get rid of grouping variable
  testungen_bfm_data$Datum <- testungen_bfm_data$"as.Date(`Datum des Datenstandes`)"
  testungen_bfm_data$"as.Date(`Datum des Datenstandes`)" <- NULL
  
  ## System changed at 30.07.2020. Since then, we have new headers for
  ## the columns.
  testungen_bfm_data <- testungen_bfm_data %>%
    filter(as.Date(Datum) > "2020-07-30" )
  
  testungen_bfm_data <- testungen_bfm_data %>%
    bind_rows(
      testungen_data %>% 
        transmute(
          `Antwort ID` = `ObjectID`,
          `Anzahl der Jedermann-Testungen / Corona Warn App` = `Jedermann-Testungen/Corona-Warn-App durchgeführt`,
          `Anzahl der durchgefuehrten Testungen Drive in Kritis` = `Theresienwiese KRITIS - getestet`,
          `Anzahl der durchgefuehrten Testungen Drive in KP1` = `Theresienwiese KP1 - getestet`,
          `Anzahl der durchgefuehrten Testungen Mobile Entnahmen` = `Mobile Entnahmen durchgeführt`,
          `Datum` = as.Date(`Datum auswählen`),
        ) %>% 
        filter(Datum > max(testungen_bfm_data$Datum)) %>%
        mutate(
          `Testungen gesamt seit dem 30.07.2020 Jedermann-Testungen / Corona Warn App` = cumsum(`Anzahl der Jedermann-Testungen / Corona Warn App`) + max(testungen_bfm_data$`Testungen gesamt seit dem 30.07.2020 Jedermann-Testungen / Corona Warn App`, na.rm = TRUE),
          `Testungen gesamt seit dem 16.03.2020 RGU Theresienwiese` = cumsum(`Anzahl der durchgefuehrten Testungen Drive in Kritis`) + cumsum(`Anzahl der durchgefuehrten Testungen Drive in KP1`) + max(testungen_bfm_data$`Testungen gesamt seit dem 16.03.2020 RGU Theresienwiese`, na.rm = TRUE),
          `Testungen gesamt seit dem 16.03.2020 Mobile Entnahmen` = cumsum(`Anzahl der durchgefuehrten Testungen Mobile Entnahmen`) + max(testungen_bfm_data$`Testungen gesamt seit dem 16.03.2020 Mobile Entnahmen`, na.rm = TRUE)
        )
    )
  
  
  ## most recent data from Aicher
  recent_aicher_data <- cbind(testungen_bfm_data %>%
    ungroup() %>%
    group_by(as.Date(`Datum`)) %>% 
    filter(`Antwort ID` == max(`Antwort ID`)) %>% 
    ungroup() %>% 
    arrange(as.Date(`Datum`)) %>% 
    filter(row_number() >= (n() - 2)) %>% 
    as.data.frame(),
    input_data_aicher[(nrow(input_data_aicher)-2):nrow(input_data_aicher), 17:18])
  

if (export_to_bmf == TRUE) {
  
  ## export to BFM
  
  try(
    
    write.table(new_cases_bfm_munich,
                
                paste0(wd_path4, 'new_cases_munich_data.csv'),
                
                sep = ";",
                row.names = FALSE,
                fileEncoding="UTF-8",
                na = "0", 
                dec = ",")
    
    , silent = TRUE
  )
  
  try(
    
    write.table(new_cases_bfm_bavaria,
                
                paste0(wd_path4, 'new_cases_bavaria_data.csv'),
                
                sep = ";",
                row.names = FALSE,
                fileEncoding="UTF-8", 
                na = "0", 
                dec = ",")
    
    , silent = TRUE
  )
  
  try(
    
    write.table(new_cases_bfm_germany,
                
                paste0(wd_path4, 'new_cases_germany_data.csv'),
                
                sep = ";",
                row.names = FALSE,
                fileEncoding="UTF-8",
                na = "0",
                dec = ",")
    
    , silent = TRUE
  )
  
  try(
    
    write.table(rki_7days_archive_inz,
                
                paste0(wd_path4, 
                       'inz7days_rki.csv'), 
                
                sep = ";",
                row.names = FALSE,
                fileEncoding="UTF-8", 
                na = "0", 
                dec = ",")
    
    , silent = TRUE
  )
  
  try(
    
    write.table(testungen_bfm_data,
                
                paste0(wd_path4, 
                       #prefix_bfm,
                       'testungen_data.csv'), 
                
                sep = ";",
                row.names = FALSE,
                fileEncoding="UTF-8", 
                na = "0", 
                dec = ",")
    
    , silent = TRUE
  )
  
  try(
    
    write.table(facilities_bfm_data,
                
                paste0(wd_path4, 
                       #prefix_bfm,
                       'ausbrueche_data.csv'), 
                
                sep = ";",
                row.names = FALSE,
                fileEncoding="UTF-8", 
                na = "0", 
                dec = ",")
    
    , silent = TRUE
    
  )
  

  try(
    
    write.table(age_grouped_series_data,
                
                paste0(wd_path4, 'Altersgruppen_Zeitverlauf_data.csv'), 
                
                sep = ";",
                row.names = FALSE,
                fileEncoding="UTF-8", 
                na = "0", 
                dec = ",")
    
    , silent = TRUE
    
  )
  
  try(
    
    write.table(mean_age_per_week,
                
                paste0(wd_path4, 'Durchschnittsalter_Woche_data.csv'), 
                
                sep = ";",
                row.names = FALSE,
                fileEncoding="UTF-8", 
                na = "0", 
                dec = ",")
    
    , silent = TRUE
    
  )
  
  
  try(
    
    write.table(mean_age_per_date,
                
                paste0(wd_path4, 'Durchschnittsalter_Tag_data.csv'), 
                
                sep = ";",
                row.names = FALSE,
                fileEncoding="UTF-8", 
                na = "0", 
                dec = ",")
    
    , silent = TRUE
    
  )
  
  
  
  try(
    
    write.table(testungen_data,
                
                paste0(wd_path4, 'testungen_since_022021.csv'), 
                
                sep = ";",
                row.names = FALSE,
                fileEncoding="UTF-8", 
                na = "0", 
                dec = ",")
    
    , silent = TRUE
    
  )
  
  
}
  
# export data to muenchen.de ---------------------------------------------------

  
if (export_to_bmf == TRUE) {  
    
  ## update only until 14:00
  if ((lubridate::now() %>% lubridate::hour(.)) <= 13) {
    
    try(
      
      write.table(new_cases_bfm_munich,
                  
                  paste0(wd_path_muc, 'new_cases_munich_data.csv'),
                  
                  sep = ";",
                  row.names = FALSE,
                  fileEncoding="UTF-8",
                  na = "0", 
                  dec = ",")
      
      , silent = TRUE
    ) 
    
  }
  
   
    
}
    
# Export to excel-directory ----------------------------------------------------

try(
  
  write.table(new_cases_bfm_munich,
              
              paste0(excel_path, 'new_cases_munich_data.csv'),
              
              sep = ";",
              row.names = FALSE,
              fileEncoding="UTF-8",
              na = "0", 
              dec = ",")
  
  , silent = TRUE
)

try(
  
  write.table(new_cases_bfm_bavaria,
              
              paste0(excel_path, 'new_cases_bavaria_data.csv'),
              
              sep = ";",
              row.names = FALSE,
              fileEncoding="UTF-8", 
              na = "0", 
              dec = ",")
  
  , silent = TRUE
)

try(
  
  write.table(new_cases_bfm_germany,
              
              paste0(excel_path, 'new_cases_germany_data.csv'),
              
              sep = ";",
              row.names = FALSE,
              fileEncoding="UTF-8",
              na = "0",
              dec = ",")
  
  , silent = TRUE
)

  try(
    
    write.table(rki_7days_archive_inz,
                
                paste0(excel_path, 
                       'inz7days_rki.csv'), 
                
                sep = ";",
                row.names = FALSE,
                fileEncoding="UTF-8", 
                na = "0", 
                dec = ",")
    
    , silent = TRUE
  )
  
try(
  
  write.table(testungen_bfm_data,
              
              paste0(excel_path, 
                     'testungen_data.csv'), 
              
              sep = ";",
              row.names = FALSE,
              fileEncoding="UTF-8", 
              na = "0", 
              dec = ",")
  
  , silent = TRUE
)

try(
  
  write.table(facilities_bfm_data,
              
              paste0(excel_path, 
                     'ausbrueche_data.csv'), 
              
              sep = ";",
              row.names = FALSE,
              fileEncoding="UTF-8", 
              na = "0", 
              dec = ",")
  
  , silent = TRUE
  
)


try(
  
  write.table(input_data ,
              
              paste0(excel_path, 'input_data.csv'), 
              
              sep = ";",
              row.names = FALSE,
              fileEncoding="UTF-8", 
              na = "0", 
              dec = ",")
  
  , silent = TRUE
  
)

  
# Export to shared-directory ---------------------------------------------------
  
  try(
    
    write.table(new_cases_bfm_munich,
                
                paste0(shared_data, 'new_cases_munich_data.csv'),
                
                sep = ";",
                row.names = FALSE,
                fileEncoding="UTF-8",
                na = "0", 
                dec = ",")
    
    , silent = TRUE
  )
  
  try(
    
    write.table(new_cases_bfm_bavaria,
                
                paste0(shared_data, 'new_cases_bavaria_data.csv'),
                
                sep = ";",
                row.names = FALSE,
                fileEncoding="UTF-8", 
                na = "0", 
                dec = ",")
    
    , silent = TRUE
  )
  
  try(
    
    write.table(new_cases_bfm_germany,
                
                paste0(shared_data, 'new_cases_germany_data.csv'),
                
                sep = ";",
                row.names = FALSE,
                fileEncoding="UTF-8",
                na = "0",
                dec = ",")
    
    , silent = TRUE
  )
  
  try(
    
    write.table(rki_7days_archive_inz,
                
                paste0(shared_data, 
                       'inz7days_rki.csv'), 
                
                sep = ";",
                row.names = FALSE,
                fileEncoding="UTF-8", 
                na = "0", 
                dec = ",")
    
    , silent = TRUE
  )
  
  try(
    
    write.table(testungen_bfm_data,
                
                paste0(shared_data, 
                       'testungen_data.csv'), 
                
                sep = ";",
                row.names = FALSE,
                fileEncoding="UTF-8", 
                na = "0", 
                dec = ",")
    
    , silent = TRUE
  )
  
  try(
    
    write.table(facilities_bfm_data,
                
                paste0(shared_data, 
                       'ausbrueche_data.csv'), 
                
                sep = ";",
                row.names = FALSE,
                fileEncoding="UTF-8", 
                na = "0", 
                dec = ",")
    
    , silent = TRUE
    
  )
  
  
  try(
    
    write.table(input_data ,
                
                paste0(shared_data, 'input_data.csv'), 
                
                sep = ";",
                row.names = FALSE,
                fileEncoding="UTF-8", 
                na = "0", 
                dec = ",")
    
    , silent = TRUE
    
  )

  
# Make a Preload with the latest objects created by 11_
objects_to_be_saved <- setdiff(ls(), "params")
  
save(file = paste0(wd_path1, "11_preload.RData"),
     list = objects_to_be_saved)
