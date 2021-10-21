
## build file names
icu_beds_filename <- paste0(user_input, "ivena_icu.xlsx")
imc_beds_filename <- paste0(user_input, "ivena_imc.xlsx")
normal_beds_filename <- paste0(user_input, "ivena_normal.xlsx")


## finds the relevant information in the IVENA table
find_sums_in_table <- function(input_data, ICU = FALSE) {
  
  list_of_organisations <- input_data$Organisationseinheiten
  
  ## positon of ILS MUC
  muc_position <- grepl("eitstelle München", list_of_organisations, 
                        fixed = TRUE) %>%
    which(. == 1)
  
  ## position of rowsums 
  muc_sum_postion <-  grepl("Summe", list_of_organisations, 
                            fixed = TRUE) %>%
    which(. == 1)
  
  ## find target row
  target_row <- muc_sum_postion[which(muc_sum_postion > muc_position)][1]
  
  
  ## find relevant columns
  relevant_cols <- grepl("etten", colnames(input_data), fixed = TRUE) %>% 
    which(. == 1)
  
  ## colnames
  col_names_icu <- c('ist', 
                 'max', 
                 'belegt', 
                 'belegt_covid_bestaetigt', 
                 'belegt_covid_verdacht', 
                 'belegt_ecmo', 
                 'frei', 
                 'frei_covid', 
                 'frei_ecmo')
  
  col_names_normal = c(
    'ist', 
    'max', 
    'belegt', 
    'belegt_covid_bestaetigt', 
    'belegt_covid_verdacht', 
    'frei', 
    'frei_covid'
  )
  

  
  ## slice data
  if (ICU == TRUE) {
    
    data_temp <- input_data[target_row, min(relevant_cols):max(relevant_cols)] %>%
      magrittr::set_colnames(col_names_icu)
    
  } else  {
    
    data_temp <- input_data[target_row, min(relevant_cols):max(relevant_cols)] %>%
      magrittr::set_colnames(col_names_normal)
    
  }
  
  return(data_temp)
  
}



## read data
today_icu_beds <- readxl::read_xlsx(icu_beds_filename)
today_imc_beds <- readxl::read_xlsx(imc_beds_filename)
today_normal_beds <- readxl::read_xlsx(normal_beds_filename)

## copy to shared folder
writexl::write_xlsx(today_icu_beds, str_glue("{shared_input}ivena_icu.xlsx"))
writexl::write_xlsx(today_imc_beds, str_glue("{shared_input}ivena_imc.xlsx"))
writexl::write_xlsx(today_normal_beds, str_glue("{shared_input}ivena_normal.xlsx"))

## extract sums

today_icu_beds <- find_sums_in_table(today_icu_beds, ICU = TRUE)
today_imc_beds <- find_sums_in_table(today_imc_beds)
today_normal_beds <- find_sums_in_table(today_normal_beds)


## prepare data
today_icu_beds <- today_icu_beds %>% 
  mutate(art = "ICU") %>%
  mutate(export_datetime = file.mtime(icu_beds_filename))

today_imc_beds <- today_imc_beds %>% 
  mutate(art = "IMC") %>%
  mutate(export_datetime = file.mtime(imc_beds_filename))

today_normal_beds <- today_normal_beds %>% 
  mutate(art = "Normalpflege") %>%
  mutate(export_datetime = file.mtime(normal_beds_filename))

today_beds <- bind_rows(today_icu_beds, today_imc_beds, today_normal_beds)

if (nrow(today_beds %>% count(as.Date(export_datetime))) != 1) {
  print(today_beds)
  stop("IVENA-Exports wurden nicht am selben Datum angefertigt... Siehe oben.")
}

## derived
if (file.exists(paste0(derived_data, "ivena_data.rds"))) {
  saveRDS(
    bind_rows(
      readRDS(paste0(derived_data, "ivena_data.rds")),
      today_beds
    ), 
    paste0(derived_data, "ivena_data.rds")
  )
} else {
  saveRDS(
    today_beds,
    paste0(derived_data, "ivena_data.rds")
  )
}

## shared 
if (file.exists(paste0(shared_data, "ivena_data.rds"))) {
  saveRDS(
    bind_rows(
      readRDS(paste0(shared_data, "ivena_data.rds")),
      today_beds
    ), 
    paste0(shared_data, "ivena_data.rds")
  )
} else {
  saveRDS(
    today_beds,
    paste0(shared_data, "ivena_data.rds")
  )
}
