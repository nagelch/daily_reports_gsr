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


# function synthesize_baysim ---------------------------------------------------

synthesize_baysim <- function(data) {
  data_converted <- data %>% 
    dplyr::select(all_of(baysim_cols)) %>%
    mutate(
      AbsonderungBeginnOffset = as.integer(
        as.Date(`Absonderung ausgesprochen am`)
      )
    ) %>% 
    mutate(
      AbsonderungEndeOffset = as.integer(
        as.Date(`Absonderung bis`)
      )
    ) %>%
    mutate(
      ErstelltOffset = as.integer(
        as.Date(`Erstellt am`)
      )
    ) %>%
    dplyr::select(
      - `Erstellt am`,
      - `Absonderung ausgesprochen am`,
      - `Absonderung bis`,
      - Ort
    ) %>%
    bind_cols(
      data %>% reduce_levels(Ort, number_of_levels = 5),
    ) %>%
    relocate(
      Ort
    )
  
  
  data_left <- data_converted %>%
    dplyr::select(`(Nicht ändern) Fallakte`)
  data_right <- data_converted %>%
    dplyr::select(-`(Nicht ändern) Fallakte`) %>% 
    syn(., maxfaclevels = 60, seed = 42)
  
  set.seed(42)
  
  data_left %>%
    bind_cols(data_right$syn) %>% 
    # Addiere Offsets auf Erstmeldung, um Datumsfelder zu erhalten
    mutate(`Absonderung ausgesprochen am` = 
             as.Date(AbsonderungBeginnOffset, origin="1970-01-01")) %>%
    mutate(`Absonderung bis` = 
             as.Date(AbsonderungEndeOffset, origin="1970-01-01")) %>%
    mutate(`Erstellt am` = 
             as.Date(ErstelltOffset, origin="1970-01-01")) %>%
    # Hilfsspalten entfernen
    dplyr::select(all_of(baysim_cols))
}

# read data --------------------------------------------------------------------

baysim_cases_2020 <-
  readxl::read_excel(
    paste0(user_input, "baysim_fallakten_2020.xlsx"),
    guess_max = 200000
  )

baysim_cases <-
  readxl::read_excel(
    paste0(user_input, "baysim_fallakten.xlsx"),
    guess_max = 200000
  )

# synthesize data --------------------------------------------------------------

baysim_cases_2020_syn <- baysim_cases_2020 %>% synthesize_baysim()
baysim_cases_syn <- baysim_cases %>% synthesize_baysim()


# export data ------------------------------------------------------------------

writexl::write_xlsx(baysim_cases_2020_syn, str_glue("{shared_input}baysim_fallakten_2020.xlsx"))
writexl::write_xlsx(baysim_cases_syn, str_glue("{shared_input}baysim_fallakten.xlsx"))
