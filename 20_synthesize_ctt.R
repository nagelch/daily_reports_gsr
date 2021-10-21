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
octoware_cols <- c(
  "ID",
  "erzeugt_am",
  "WohnAnschrift_Ort",
  "Merkmal_Absonderung",
  "Merkmal_AbsonderungBeginn",
  "Merkmal_AbsonderungEnde",
  "Anzahl_Kontaktpersonen"
)


# function synthesize_ctt ---------------------------------------------------

synthesize_ctt <- function(data) {
  data_converted <- data %>% 
    dplyr::select(all_of(octoware_cols)) %>%
    mutate(
      AbsonderungBeginnOffset = as.integer(
        as.Date(`Merkmal_AbsonderungBeginn`)
      )
    ) %>% 
    mutate(
      AbsonderungEndeOffset = as.integer(
        as.Date(`Merkmal_AbsonderungEnde`)
      )
    ) %>%
    mutate(
      ErstelltOffset = as.integer(
        as.Date(`erzeugt_am`)
      )
    ) %>%
    dplyr::select(
      - `erzeugt_am`,
      - `Merkmal_AbsonderungBeginn`,
      - `Merkmal_AbsonderungEnde`,
      - WohnAnschrift_Ort
    ) %>%
    bind_cols(
      data %>% reduce_levels(WohnAnschrift_Ort, number_of_levels = 5),
    ) %>%
    relocate(
      WohnAnschrift_Ort
    )
  
  data_left <- data_converted %>%
    dplyr::select(ID)
  data_right <- data_converted %>%
    dplyr::select(-ID) %>% 
    syn(., maxfaclevels = 60, seed = 42)
  
  set.seed(42)
  
  data_left %>%
    bind_cols(data_right$syn) %>% 
    # Addiere Offsets auf Erstmeldung, um Datumsfelder zu erhalten
    mutate(`Merkmal_AbsonderungBeginn` = 
             as.Date(AbsonderungBeginnOffset, origin="1970-01-01")) %>%
    mutate(`Merkmal_AbsonderungEnde` = 
             as.Date(AbsonderungEndeOffset, origin="1970-01-01")) %>%
    mutate(`erzeugt_am` = 
             as.Date(ErstelltOffset, origin="1970-01-01")) %>%
    # Hilfsspalten entfernen
    dplyr::select(all_of(octoware_cols))
}

# read data --------------------------------------------------------------------

octoware_cases <-
  readxl::read_excel(
    paste0(user_input, "ctt_data.xlsx"),
    guess_max = 200000
  )

# synthesize data --------------------------------------------------------------

octoware_cases_syn <- octoware_cases %>% synthesize_ctt()


# export data ------------------------------------------------------------------

writexl::write_xlsx(octoware_cases_syn, str_glue("{shared_input}ctt_data.xlsx"))
