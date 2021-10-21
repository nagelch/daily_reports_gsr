#'@description Prepare RKI data for the daily reports

# setup ------------------------------------------------------------------------

## load libraries
library(dplyr)

## load data
rki_cases_data <- readRDS(paste0(derived_data, 'rki_cases_data.rds'))

## read R(t) values
Rt_bayern <- read.csv(
  paste0(user_input,"R_bayern.csv"), 
  quote="", comment.char="#")

Rt_muenchen <- read.csv(
  paste0(user_input,"R_muenchen.csv"), 
  quote="", comment.char="#")

colnames(Rt_bayern) <- c('Datum', 
                         'KI_lower_bayern', 
                         'KI_upper_bayern', 
                         'R_t_bayern')

colnames(Rt_muenchen) <- c('Datum', 
                         'KI_lower_muenchen', 
                         'KI_upper_muenchen', 
                         'R_t_muenchen')


## covert to date type
Rt_muenchen$Datum <- as.Date(Rt_muenchen$Datum)
Rt_bayern$Datum <- as.Date(Rt_bayern$Datum)

Rt_muenchen$Datum_inhaltlich <- Rt_muenchen$Datum 
Rt_bayern$Datum_inhaltlich <- Rt_bayern$Datum 

## Datenstand
Rt_bayern$Datum <- Rt_bayern$Datum + 13
Rt_muenchen$Datum <- Rt_muenchen$Datum + 13

## save as rds
saveRDS(Rt_muenchen, paste0(derived_data,"Rt_muenchen.rds"))
saveRDS(Rt_bayern, paste0(derived_data,"Rt_bayern.rds"))

saveRDS(Rt_muenchen, paste0(shared_data,"Rt_muenchen.rds"))
saveRDS(Rt_bayern, paste0(shared_data,"Rt_bayern.rds"))

# functions --------------------------------------------------------------------

## helper funciton (as in the script for preparing octo_data)
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

#'@param cases_vector [numermic vector] vector of cumlative cases
#'@param current_only [boolean] if TRUE estimate the doubling days only for 
#' the latest observation, if FALSE estimate the doubling days for all values 
#' excluding the first

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

# join R(t) --------------------------------------------------------------------

## complete time span
allDates <- as.Date(
  min(rki_cases_data$Datum):max(rki_cases_data$Datum),
  origin = '1970-01-01'
) %>%
  as.data.frame() %>%
  magrittr::set_colnames("Datum")

## add R(t) information
rki_cases_data <- rki_cases_data %>% 
  
  full_join(
    
    allDates, by = c("Datum")
  ) %>%
  
  arrange(Datum) %>%
  
  full_join(
    Rt_bayern,
    
    by = c("Datum")
  ) %>%
  
  left_join(
    
    Rt_muenchen,
    
    by = c("Datum")
    
  ) %>%
  
  arrange(Datum)

# Verdopplungszahl -------------------------------------------------------------

rki_cases_data <- rki_cases_data %>%
  mutate(RateDoubled_Germany = doubling_days(casesGermanyKum),
         RateDoubled_Munich = doubling_days(casesMunichKum),
         RateDoubled_Bavaria = doubling_days(casesBavariaKum)) %>%
  arrange(Datum)


# Trim observations ------------------------------------------------------------

## set last 3 observations to NA
#'@details The last three days are volatile. We exclude them here, as we also
#'exclude them from our simulations.

# k <- nrow(rki_cases_data)
# if (k > 3) {
#   
#   rki_cases_data[
#     (k-2):k,
#     c('RateDoubled_Munich',
#       'RateDoubled_Bavaria',
#       'RateDoubled_Germany')
#     ] <- NA
#   
# }

## rename into rki_reporting_data
rki_reporting_data <- rki_cases_data %>%
  arrange(Datum)
  
# save -------------------------------------------------------------------------

saveRDS(rki_reporting_data,
        paste0(derived_data, 'rki_reporting_data.rds'))


write.table(rki_reporting_data,
            paste0(derived_data, 'rki_reporting_data.csv'), 
            sep = ";",
            row.names = FALSE)

saveRDS(rki_reporting_data,
        paste0(shared_data, 'rki_reporting_data.rds'))


write.table(rki_reporting_data,
            paste0(shared_data, 'rki_reporting_data.csv'), 
            sep = ";",
            row.names = FALSE)


## export tables for Branddirektion
try(
  
  write.table(Rt_muenchen %>% na.omit(),
              paste0(brand_data,
                     #prefix_bfm,
                     "Rt_muenchen.csv"),
              sep = ";",
              row.names = FALSE,
              na = "0", 
              dec = ",")
  
  , silent = TRUE
  
)

try(
  
  write.table(Rt_bayern %>% na.omit(),
              paste0(brand_data,
                     #prefix_bfm,
                     "Rt_bayern.csv"),
              sep = ";",
              row.names = FALSE,
              na = "0", 
              dec = ",")
  
  , silent = TRUE
  
)
