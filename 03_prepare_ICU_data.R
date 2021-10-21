# load data --------------------------------------------------------------------

capa_data <- read.csv(paste0(intern_input, "rgu_-_datenexport.csv"), sep=";")
colnames(capa_data) <- gsub("\\.", "_", toupper(colnames(capa_data)))

capa_data <- capa_data %>%
  mutate(LADE_DATUM = as.character(LADE_DATUM)) %>%
  mutate(LADE_DATUM = as.POSIXct(LADE_DATUM, 
                                 format = "%d.%m.%Y %H:%M:%S",
                                 tz = Sys.timezone())) %>%
  arrange(KH_NAME, LADE_DATUM) %>%
  mutate(TOTAL_CAPACITY = BELEGT_BETTEN_MIT_COVID + 
           DAVON_FREIE_BETTEN_FUER_COVID)

# prepare data: filter and aggregate -------------------------------------------

## reshape
capa_ICU <- capa_data %>%
  arrange(KH_NAME, LADE_DATUM) %>%
  filter(BETT_TYP == "ICU") %>%
  mutate(TOTAL_CAPACITY = BELEGT_BETTEN_MIT_COVID + 
           DAVON_FREIE_BETTEN_FUER_COVID) %>%
  group_by(KH_NAME) %>%
  ## only those with planned capacities > 0
  mutate(ICU_KH = ifelse(LADE_DATUM == max(LADE_DATUM), 
                         TOTAL_CAPACITY, 0)) %>%
  mutate(ICU_KH = max(ICU_KH)) %>%
  filter(ICU_KH > 0) %>%
  ungroup()


## univariate ts 
capa_ICU_univariate <- capa_ICU %>%
  aggregate(BELEGT_BETTEN_MIT_COVID ~ LADE_DATUM, data = ., FUN = sum)

## univariate ts (daily aggregates)
capa_ICU_univariate_daily <- capa_ICU %>%
  mutate(DATUM = as.Date(LADE_DATUM)) %>%
  aggregate(BELEGT_BETTEN_MIT_COVID ~ DATUM + KH_NAME, 
            data = ., FUN = mean) %>%
  aggregate(BELEGT_BETTEN_MIT_COVID ~ DATUM, data = ., FUN = sum) %>%
  magrittr::set_colnames(c("date", "cases_ICU"))

#'@details We filter only those which have had a COVID-19 positive case so far
#'and (otpionally) aggregate by day.

## aggregate by day
capa_ICU_ts_days <- capa_ICU %>%
  dplyr::select(LADE_DATUM, KH_NAME, BELEGT_BETTEN_MIT_COVID) %>%
  group_by(KH_NAME) %>%
  filter(max(BELEGT_BETTEN_MIT_COVID) > 0) %>%
  ungroup() %>%
  group_by(KH_NAME, days = lubridate::day(LADE_DATUM)) %>% 
  mutate(BELEGT_BETTEN_MIT_COVID = mean(BELEGT_BETTEN_MIT_COVID)) %>%
  ungroup() %>%
  group_by(KH_NAME, days) %>%
  mutate(day_id = row_number()) %>%
  filter(day_id == 1) %>%
  ungroup() %>%
  dplyr::select(LADE_DATUM, KH_NAME, BELEGT_BETTEN_MIT_COVID) %>%
  group_split(KH_NAME)

## no aggregation  
capa_ICU_ts <- capa_ICU %>%
  dplyr::select(LADE_DATUM, KH_NAME, BELEGT_BETTEN_MIT_COVID) %>%
  group_by(KH_NAME) %>%
  filter(max(BELEGT_BETTEN_MIT_COVID) > 0) %>%
  ungroup() %>%
  group_split(KH_NAME)

# export -----------------------------------------------------------------------

saveRDS(capa_ICU_univariate_daily, 
        paste0(derived_data,"capa_ICU_univariate_daily.rds"))

## export data
saveRDS(capa_data, paste0(derived_data, "capa_data.rds"))