

## read data
rki_7days_archive <- readRDS(paste0(wd_path1,'rki_7days_archive.rds'))
lgl_data <- readRDS(paste0(wd_path1,'lgl_data.rds'))


## Berechne 7-Tage-Inzidenz aus RGU-Daten
input_data_inz <- recent_data_7d %>% 
  mutate(
    Inz7days_RGU = zoo::rollapply(
      input_data$casesMunich_Inz, 
      7, 
      sum, 
      fill = NA, 
      align = "right"
    ) %>%
    '/'(14.88202) %>% 
    tail(n = 7) %>% 
    round(digits = 2) 
  ) %>%
  dplyr::select(Datum, casesMunich_Inz, Inz7days_RGU)

# Berechne 7-Tage-Inzidenz aus RKI-Daten
rki_7days_archive_inz <- rki_7days_archive %>%
  ## - für jeden Datenstand: filtere und behalte nur die höchste Session ID.
  ## - für jeden Datenstand: bilde die 7 Tage Inzidenz
  group_by(Datenstand) %>%
  top_n(Session, n=1) %>%
  top_n(Datum, n=7) %>%
  summarise(Inz7days_RKI = round(sum(casesMunich) / 14.88202, digits = 2)) %>%
  arrange(Datenstand)

# Berechne 7-Tage-Inzidenz aus LGL-Daten
lgl_data_inz <- lgl_data %>%
  filter(Landkreis.Stadt == 'München Stadt') %>%
  ## Zeitstempel erstellen, um direkt nach der höchsten Uhrzeit
  ## filtern zu können
  mutate(timeStamp = ((paste0(Datum, Uhrzeit) %>%
                         gsub(" Uhr", "", .)) %>%
                        as.POSIXct(., format = c("%d.%m.%Y %H:%M")))) %>%
  ## Größte Uhrzeit für jedes Datum
  group_by(Datum) %>%
  filter(timeStamp == max(timeStamp)) %>%
  filter(!duplicated(timeStamp)) %>%
  ## Davon jeweils nur ein Exemplar (keine Dublikate)
  ungroup() %>%
  # Name analog zu rki_cases_data
  rename(Inz7days_LGL = X7.Tage.Inzidenz.pro.100.000.Einwohner) %>%
  ## rki_cases_data benutzt Date als Datumstyp
  ## ziehe davon einen Tag ab, denn die Daten beziehen sich auf Datum t-1.
  mutate(isoDatum = (as.Date(Datum, format="%d.%m.%Y") - 1)) %>%
  ## Komma zu Punkt und anschließend Umwandlung in numeric
  mutate(Inz7days_LGL = (as.numeric(gsub(",", ".", Inz7days_LGL)))) %>%
  dplyr::select(isoDatum, Inz7days_LGL)

## Join
all_data_7days_inz <- input_data_inz %>% 
  ## left_join um auch am Wochende eine Tabelle zu bekommen, die dann beim
  ## LGL Fehlende Werte (NA) enthält.
  left_join(
    lgl_data_inz,
    by = c("Datum" = "isoDatum")
  ) %>% 
  left_join(
    rki_7days_archive_inz,
    by = c("Datum" = "Datenstand")
  ) %>%
  tail(7)
