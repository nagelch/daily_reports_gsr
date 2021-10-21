## 16_prepare_AgeGroups_data.R

#'@description aggregates data from octoware and kh by age groups, like we
#'report them in our daily reports.

## load data
kh_data <-  readRDS(paste0(wd_path1, 'kh_data_clean.rds'))

## functions -------------------------------------------------------------------

remove.leading.NAs <- function(input_data) {
  
  input_data <- input_data %>% arrange(Datum)
  
  for (j in colnames(input_data)) {
    
    ## replace leading NAs by 0
    firstNotNA <- min(which(!is.na(input_data[, j])))
    
    if (firstNotNA > 1) {
      input_data[1:(firstNotNA- 1), j] <- 0
    }
    
    ## replace NA by last observed value
    input_data[, j] <- input_data[, j] %>%
      zoo::na.locf()
    
  }
  
  return(input_data)
  
}

# OCOTWARE ---------------------------------------------------------------------

## age groups ------------------------------------------------------------------

octo_data_ageGroups_dead <- octo_data %>%
  
  ## filter fatalities
  filter(
    !is.na(Sterbedatum),
    Tod == "ja"
  ) %>%
  mutate(Datum  =
           as.Date(Sterbedatum, format = "%Y-%m-%d")) %>%
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
      as.numeric((findInterval(Alter_Erstmeldung, c(81, 150)) == 1))
    
  ) %>%
  arrange(Datum)


## count groups and join data
octo_AgeGroups_dead <-
  
  octo_data_ageGroups_dead %>%
  filter(age_0to5_sum == 1) %>%
  group_by(Datum) %>%
  summarise(n = n()) %>%
  mutate(age_0to5_sum = cumsum(n)) %>%
  dplyr::select(Datum, age_0to5_sum) %>%
  
  full_join(
    octo_data_ageGroups_dead %>%
      filter(age_6to10_sum == 1) %>%
      group_by(Datum) %>%
      summarise(n = n()) %>%
      mutate(age_6to10_sum = cumsum(n)) %>%
      dplyr::select(Datum, age_6to10_sum),
    by = c("Datum")
  ) %>%
  
  full_join(
    octo_data_ageGroups_dead %>%
      filter(age_11to20_sum == 1) %>%
      group_by(Datum) %>%
      summarise(n = n()) %>%
      mutate(age_11to20_sum = cumsum(n)) %>%
      dplyr::select(Datum, age_11to20_sum),
    by = c("Datum")
  ) %>%
  
  
  
  full_join(
    octo_data_ageGroups_dead %>%
      filter(age_21to40_sum == 1) %>%
      group_by(Datum) %>%
      summarise(n = n()) %>%
      mutate(age_21to40_sum = cumsum(n)) %>%
      dplyr::select(Datum, age_21to40_sum),
    by = c("Datum")
  ) %>%
  
  
  
  full_join(
    octo_data_ageGroups_dead %>%
      filter(age_41to60_sum == 1) %>%
      group_by(Datum) %>%
      summarise(n = n()) %>%
      mutate(age_41to60_sum = cumsum(n)) %>%
      dplyr::select(Datum, age_41to60_sum),
    by = c("Datum")
  ) %>%
  
  
  full_join(
    octo_data_ageGroups_dead %>%
      filter(age_61to80_sum == 1) %>%
      group_by(Datum) %>%
      summarise(n = n()) %>%
      mutate(age_61to80_sum = cumsum(n)) %>%
      dplyr::select(Datum, age_61to80_sum),
    by = c("Datum")
  ) %>%
  
  
  full_join(
    octo_data_ageGroups_dead %>%
      filter(age_81plus_sum == 1) %>%
      group_by(Datum) %>%
      summarise(n = n()) %>%
      mutate(age_81plus_sum = cumsum(n)) %>%
      dplyr::select(Datum, age_81plus_sum),
    by = c("Datum")
  ) %>%
  
  
  
  ## age groups filtered for male persons
  
  full_join(
    octo_data_ageGroups_dead %>%
      filter(Person_Geschlecht == "männlich") %>%
      filter(age_0to5_sum == 1) %>%
      group_by(Datum) %>%
      summarise(n = n()) %>%
      mutate(age_0to5_m = cumsum(n)) %>%
      dplyr::select(Datum, age_0to5_m),
    by = c("Datum")
  ) %>%
  
  
  full_join(
    octo_data_ageGroups_dead %>%
      filter(Person_Geschlecht == "männlich") %>%
      filter(age_6to10_sum == 1) %>%
      group_by(Datum) %>%
      summarise(n = n()) %>%
      mutate(age_6to10_m = cumsum(n)) %>%
      dplyr::select(Datum, age_6to10_m),
    by = c("Datum")
  ) %>%
  
  
  full_join(
    octo_data_ageGroups_dead %>%
      filter(Person_Geschlecht == "männlich") %>%
      filter(age_11to20_sum == 1) %>%
      group_by(Datum) %>%
      summarise(n = n()) %>%
      mutate(age_11to20_m = cumsum(n)) %>%
      dplyr::select(Datum, age_11to20_m),
    by = c("Datum")
  ) %>%
  
  
  full_join(
    octo_data_ageGroups_dead %>%
      filter(Person_Geschlecht == "männlich") %>%
      filter(age_21to40_sum == 1) %>%
      group_by(Datum) %>%
      summarise(n = n()) %>%
      mutate(age_21to40_m = cumsum(n)) %>%
      dplyr::select(Datum, age_21to40_m),
    by = c("Datum")
  ) %>%
  
  
  full_join(
    octo_data_ageGroups_dead %>%
      filter(Person_Geschlecht == "männlich") %>%
      filter(age_41to60_sum == 1) %>%
      group_by(Datum) %>%
      summarise(n = n()) %>%
      mutate(age_41to60_m = cumsum(n)) %>%
      dplyr::select(Datum, age_41to60_m),
    by = c("Datum")
  ) %>%
  
  
  full_join(
    octo_data_ageGroups_dead %>%
      filter(Person_Geschlecht == "männlich") %>%
      filter(age_61to80_sum == 1) %>%
      group_by(Datum) %>%
      summarise(n = n()) %>%
      mutate(age_61to80_m = cumsum(n)) %>%
      dplyr::select(Datum, age_61to80_m),
    by = c("Datum")
  ) %>%
  
  
  full_join(
    octo_data_ageGroups_dead %>%
      filter(Person_Geschlecht == "männlich") %>%
      filter(age_81plus_sum == 1) %>%
      group_by(Datum) %>%
      summarise(n = n()) %>%
      mutate(age_81plus_m = cumsum(n)) %>%
      dplyr::select(Datum, age_81plus_m),
    by = c("Datum")
  ) %>%
  
  
  
  ## age groups filtered for female persons
  
  full_join(
    octo_data_ageGroups_dead %>%
      filter(Person_Geschlecht == "weiblich") %>%
      filter(age_0to5_sum == 1) %>%
      group_by(Datum) %>%
      summarise(n = n()) %>%
      mutate(age_0to5_w = cumsum(n)) %>%
      dplyr::select(Datum, age_0to5_w),
    by = c("Datum")
  ) %>%
  
  
  full_join(
    octo_data_ageGroups_dead %>%
      filter(Person_Geschlecht == "weiblich") %>%
      filter(age_6to10_sum == 1) %>%
      group_by(Datum) %>%
      summarise(n = n()) %>%
      mutate(age_6to10_w = cumsum(n)) %>%
      dplyr::select(Datum, age_6to10_w),
    by = c("Datum")
  ) %>%
  
  
  full_join(
    octo_data_ageGroups_dead %>%
      filter(Person_Geschlecht == "weiblich") %>%
      filter(age_11to20_sum == 1) %>%
      group_by(Datum) %>%
      summarise(n = n()) %>%
      mutate(age_11to20_w = cumsum(n)) %>%
      dplyr::select(Datum, age_11to20_w),
    by = c("Datum")
  ) %>%
  
  
  full_join(
    octo_data_ageGroups_dead %>%
      filter(Person_Geschlecht == "weiblich") %>%
      filter(age_21to40_sum == 1) %>%
      group_by(Datum) %>%
      summarise(n = n()) %>%
      mutate(age_21to40_w = cumsum(n)) %>%
      dplyr::select(Datum, age_21to40_w),
    by = c("Datum")
  ) %>%
  
  
  full_join(
    octo_data_ageGroups_dead %>%
      filter(Person_Geschlecht == "weiblich") %>%
      filter(age_41to60_sum == 1) %>%
      group_by(Datum) %>%
      summarise(n = n()) %>%
      mutate(age_41to60_w = cumsum(n)) %>%
      dplyr::select(Datum, age_41to60_w),
    by = c("Datum")
  ) %>%
  
  
  full_join(
    octo_data_ageGroups_dead %>%
      filter(Person_Geschlecht == "weiblich") %>%
      filter(age_61to80_sum == 1) %>%
      group_by(Datum) %>%
      summarise(n = n()) %>%
      mutate(age_61to80_w = cumsum(n)) %>%
      dplyr::select(Datum, age_61to80_w),
    by = c("Datum")
  ) %>%
  
  
  full_join(
    octo_data_ageGroups_dead %>%
      filter(Person_Geschlecht == "weiblich") %>%
      filter(age_81plus_sum == 1) %>%
      group_by(Datum) %>%
      summarise(n = n()) %>%
      mutate(age_81plus_w = cumsum(n)) %>%
      dplyr::select(Datum, age_81plus_w),
    by = c("Datum")
  )

## replace missing values
for (i in seq(ncol(octo_AgeGroups_dead))) {
  
  ## if all are missing, replace by 0
  if (all(is.na(octo_AgeGroups_dead[, i]))) {
    octo_AgeGroups_dead[, i] <- 0
  }
  
}

## not all dates from min to max are present
allDates <- as.Date(
  min(octo_AgeGroups_dead$Datum):max(octo_AgeGroups_dead$Datum),
  origin = '1970-01-01'
) %>%
  as.data.frame() %>%
  magrittr::set_colnames("Datum")

octo_complete_dead <- full_join(allDates, octo_AgeGroups_dead, by = c("Datum"))

## replace missing values
octo_complete_dead <- remove.leading.NAs(octo_complete_dead)


# mean age ---------------------------------------------------------------------

Fatalities_meanAge <- octo_data %>%
  ## filter fatalities
  filter(
    !is.na(Sterbedatum),
    Tod == "ja"
  ) %>%
  mutate(meanAgeDead = mean(as.numeric(as.character(Alter_Erstmeldung)), 
                            na.rm = TRUE)) %>%
  dplyr::select(meanAgeDead) %>% tail(., 1)

# KLINIKMELDUNGEN --------------------------------------------------------------

kh_data_ageGroups <- kh_data %>%
  
  # ## filter fatalities
  # filter(
  #   !is.na(Sterbedatum),
  #   Tod == "ja"
  # ) %>%
  mutate(Datum  =
           as.Date(AUFNAHMEDATUM, format = "%d.%m.%Y")) %>%
  mutate(
    age_0to5_sum  =
      as.numeric((findInterval(ALTERINJAHRENAMAUFNAHMETAG, c(0, 6)) == 1)),
    age_6to10_sum =
      as.numeric((findInterval(ALTERINJAHRENAMAUFNAHMETAG, c(6, 11)) == 1)),
    age_11to20_sum =
      as.numeric((findInterval(ALTERINJAHRENAMAUFNAHMETAG, c(11, 21)) == 1)),
    age_21to40_sum =
      as.numeric((findInterval(ALTERINJAHRENAMAUFNAHMETAG, c(21, 41)) == 1)),
    age_41to60_sum =
      as.numeric((findInterval(ALTERINJAHRENAMAUFNAHMETAG, c(41, 61)) == 1)),
    age_61to80_sum =
      as.numeric((findInterval(ALTERINJAHRENAMAUFNAHMETAG, c(61, 81)) == 1)),
    age_81plus_sum =
      as.numeric((findInterval(ALTERINJAHRENAMAUFNAHMETAG, c(81, 150)) == 1))
    
  ) %>%
  arrange(Datum)


## count groups and join data
kh_data_AgeGroups <-
  
  kh_data_ageGroups %>%
  filter(age_0to5_sum == 1) %>%
  group_by(Datum) %>%
  summarise(n = n()) %>%
  mutate(age_0to5_sum = cumsum(n)) %>%
  dplyr::select(Datum, age_0to5_sum) %>%
  
  full_join(
    kh_data_ageGroups %>%
      filter(age_6to10_sum == 1) %>%
      group_by(Datum) %>%
      summarise(n = n()) %>%
      mutate(age_6to10_sum = cumsum(n)) %>%
      dplyr::select(Datum, age_6to10_sum),
    by = c("Datum")
  ) %>%
  
  full_join(
    kh_data_ageGroups %>%
      filter(age_11to20_sum == 1) %>%
      group_by(Datum) %>%
      summarise(n = n()) %>%
      mutate(age_11to20_sum = cumsum(n)) %>%
      dplyr::select(Datum, age_11to20_sum),
    by = c("Datum")
  ) %>%
  
  
  
  full_join(
    kh_data_ageGroups %>%
      filter(age_21to40_sum == 1) %>%
      group_by(Datum) %>%
      summarise(n = n()) %>%
      mutate(age_21to40_sum = cumsum(n)) %>%
      dplyr::select(Datum, age_21to40_sum),
    by = c("Datum")
  ) %>%
  
  
  
  full_join(
    kh_data_ageGroups %>%
      filter(age_41to60_sum == 1) %>%
      group_by(Datum) %>%
      summarise(n = n()) %>%
      mutate(age_41to60_sum = cumsum(n)) %>%
      dplyr::select(Datum, age_41to60_sum),
    by = c("Datum")
  ) %>%
  
  
  full_join(
    kh_data_ageGroups %>%
      filter(age_61to80_sum == 1) %>%
      group_by(Datum) %>%
      summarise(n = n()) %>%
      mutate(age_61to80_sum = cumsum(n)) %>%
      dplyr::select(Datum, age_61to80_sum),
    by = c("Datum")
  ) %>%
  
  
  full_join(
    kh_data_ageGroups %>%
      filter(age_81plus_sum == 1) %>%
      group_by(Datum) %>%
      summarise(n = n()) %>%
      mutate(age_81plus_sum = cumsum(n)) %>%
      dplyr::select(Datum, age_81plus_sum),
    by = c("Datum")
  ) %>%
  
  
  
  ## age groups filtered for male persons
  
  full_join(
    kh_data_ageGroups %>%
      filter(GESCHLECHT == "M") %>%
      filter(age_0to5_sum == 1) %>%
      group_by(Datum) %>%
      summarise(n = n()) %>%
      mutate(age_0to5_m = cumsum(n)) %>%
      dplyr::select(Datum, age_0to5_m),
    by = c("Datum")
  ) %>%
  
  
  full_join(
    kh_data_ageGroups %>%
      filter(GESCHLECHT == "M") %>%
      filter(age_6to10_sum == 1) %>%
      group_by(Datum) %>%
      summarise(n = n()) %>%
      mutate(age_6to10_m = cumsum(n)) %>%
      dplyr::select(Datum, age_6to10_m),
    by = c("Datum")
  ) %>%
  
  
  full_join(
    kh_data_ageGroups %>%
      filter(GESCHLECHT == "M") %>%
      filter(age_11to20_sum == 1) %>%
      group_by(Datum) %>%
      summarise(n = n()) %>%
      mutate(age_11to20_m = cumsum(n)) %>%
      dplyr::select(Datum, age_11to20_m),
    by = c("Datum")
  ) %>%
  
  
  full_join(
    kh_data_ageGroups %>%
      filter(GESCHLECHT == "M") %>%
      filter(age_21to40_sum == 1) %>%
      group_by(Datum) %>%
      summarise(n = n()) %>%
      mutate(age_21to40_m = cumsum(n)) %>%
      dplyr::select(Datum, age_21to40_m),
    by = c("Datum")
  ) %>%
  
  
  full_join(
    kh_data_ageGroups %>%
      filter(GESCHLECHT == "M") %>%
      filter(age_41to60_sum == 1) %>%
      group_by(Datum) %>%
      summarise(n = n()) %>%
      mutate(age_41to60_m = cumsum(n)) %>%
      dplyr::select(Datum, age_41to60_m),
    by = c("Datum")
  ) %>%
  
  
  full_join(
    kh_data_ageGroups %>%
      filter(GESCHLECHT == "M") %>%
      filter(age_61to80_sum == 1) %>%
      group_by(Datum) %>%
      summarise(n = n()) %>%
      mutate(age_61to80_m = cumsum(n)) %>%
      dplyr::select(Datum, age_61to80_m),
    by = c("Datum")
  ) %>%
  
  
  full_join(
    kh_data_ageGroups %>%
      filter(GESCHLECHT == "M") %>%
      filter(age_81plus_sum == 1) %>%
      group_by(Datum) %>%
      summarise(n = n()) %>%
      mutate(age_81plus_m = cumsum(n)) %>%
      dplyr::select(Datum, age_81plus_m),
    by = c("Datum")
  ) %>%
  
  
  
  ## age groups filtered for female persons
  
  full_join(
    kh_data_ageGroups %>%
      filter(GESCHLECHT == "W") %>%
      filter(age_0to5_sum == 1) %>%
      group_by(Datum) %>%
      summarise(n = n()) %>%
      mutate(age_0to5_w = cumsum(n)) %>%
      dplyr::select(Datum, age_0to5_w),
    by = c("Datum")
  ) %>%
  
  
  full_join(
    kh_data_ageGroups %>%
      filter(GESCHLECHT == "W") %>%
      filter(age_6to10_sum == 1) %>%
      group_by(Datum) %>%
      summarise(n = n()) %>%
      mutate(age_6to10_w = cumsum(n)) %>%
      dplyr::select(Datum, age_6to10_w),
    by = c("Datum")
  ) %>%
  
  
  full_join(
    kh_data_ageGroups %>%
      filter(GESCHLECHT == "W") %>%
      filter(age_11to20_sum == 1) %>%
      group_by(Datum) %>%
      summarise(n = n()) %>%
      mutate(age_11to20_w = cumsum(n)) %>%
      dplyr::select(Datum, age_11to20_w),
    by = c("Datum")
  ) %>%
  
  
  full_join(
    kh_data_ageGroups %>%
      filter(GESCHLECHT == "W") %>%
      filter(age_21to40_sum == 1) %>%
      group_by(Datum) %>%
      summarise(n = n()) %>%
      mutate(age_21to40_w = cumsum(n)) %>%
      dplyr::select(Datum, age_21to40_w),
    by = c("Datum")
  ) %>%
  
  
  full_join(
    kh_data_ageGroups %>%
      filter(GESCHLECHT == "W") %>%
      filter(age_41to60_sum == 1) %>%
      group_by(Datum) %>%
      summarise(n = n()) %>%
      mutate(age_41to60_w = cumsum(n)) %>%
      dplyr::select(Datum, age_41to60_w),
    by = c("Datum")
  ) %>%
  
  
  full_join(
    kh_data_ageGroups %>%
      filter(GESCHLECHT == "W") %>%
      filter(age_61to80_sum == 1) %>%
      group_by(Datum) %>%
      summarise(n = n()) %>%
      mutate(age_61to80_w = cumsum(n)) %>%
      dplyr::select(Datum, age_61to80_w),
    by = c("Datum")
  ) %>%
  
  
  full_join(
    kh_data_ageGroups %>%
      filter(GESCHLECHT == "W") %>%
      filter(age_81plus_sum == 1) %>%
      group_by(Datum) %>%
      summarise(n = n()) %>%
      mutate(age_81plus_w = cumsum(n)) %>%
      dplyr::select(Datum, age_81plus_w),
    by = c("Datum")
  )

## replace missing values
for (i in seq(ncol(kh_data_AgeGroups))) {
  
  ## if all are missing, replace by 0
  if (all(is.na(kh_data_AgeGroups[, i]))) {
    kh_data_AgeGroups[, i] <- 0
  }
  
}

## not all dates from min to max are present
allDates <- as.Date(
  min(kh_data_AgeGroups$Datum):max(kh_data_AgeGroups$Datum),
  origin = '1970-01-01'
) %>%
  as.data.frame() %>%
  magrittr::set_colnames("Datum")

kh_data_complete <- full_join(allDates, kh_data_AgeGroups, by = c("Datum"))

## replace missing values
kh_data_complete <- remove.leading.NAs(kh_data_complete)


# mean age ---------------------------------------------------------------------

KH_meanAge <- kh_data %>%
  mutate(meanAgeKH = mean(as.numeric(as.character(ALTERINJAHRENAMAUFNAHMETAG)), 
                            na.rm = TRUE)) %>%
  dplyr::select(meanAgeKH) %>% tail(., 1)
