# Read data ---------------------------------------------------------------------

## read data
octo_complete <- readr::read_rds(paste0(derived_data,"octoware_data.rds"))

rki_data <- readr::read_rds(paste0(derived_data, "rki_reporting_data.rds"))

# join RKI variables -----------------------------------------------------------

## combine with octoware data
octo_complete <- 
  
  octo_complete %>%
  
  left_join(
    rki_data %>%
      dplyr::select(Datum, 
                    casesGermanyKum,
                    casesGermanyKum_dead,
                    casesBavariaKum,
                    casesBavariaKum_dead,
                    casesMunichKum_cured,
                    R_t_bayern,
                    R_t_muenchen,
                    RateDoubled_Germany,
                    RateDoubled_Bavaria,
                    RateDoubled_Munich),
    
    by = c('Datum')
    
  ) %>%
  arrange(Datum)# %>%
  #remove.leading.NAs()


# Export data ------------------------------------------------------------------

## write to tables
write.table(octo_complete,
            paste0(derived_data,"joined_rki_octoware_data.csv"),
            sep = ";",
            row.names = FALSE)

## export as .rds
saveRDS(octo_complete, paste0(derived_data,"joined_rki_octoware_data.rds"))
