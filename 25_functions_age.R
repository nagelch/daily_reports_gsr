# setup ------------------------------------------------------------------------

## libraries
library(tidyverse)
library(kableExtra)
library(checkmate)

# ## path variables
# path_tagesberichte <- "L:/Thematische-Ordner/Corona/R_Projekte/Tagesbericht/"
# path_tagesberichte_branddir <- "M:/BFM/Austausch-RGU-BD/Reports_RGU/"
# shared_data <- paste0(path_tagesberichte, 'data/shared/')
# 
# 
# source(paste0(path_tagesberichte, "scripts/22_path_variables.R"),
#        encoding = 'utf-8')
# 
# ## read data (recent)
# octo_data <-
#   readxl::read_excel(
#     paste0(user_input, "octoware_kurz.xlsx"),
#     guess_max = 100000
#   )


# functions --------------------------------------------------------------------

#'@description Organizes input data by age and date
#'@param input_data data object; the octo_data table
#'@param subgroup character; one of "aktuell ICU", "ICU", 
#'"aktuell hospitalisiert", "hospitalisiert","keine"
#'@param min_date date YYYY-MM-DD; from this date on, default is min_date == max_date
#'@param max_date date YYYY-MM-DD; the upper date, default is max date in input data
#'@param detailed_per_date boolean; if TRUE, a long detailed list is created
#'@param age_intervals numeric; lower interval boundaries of age-groups
#'@param vacc_status character; one of "vollständig geimpft", "teilweise geimpft", 
#'"alle", "geimpft, "ungeimpft"
#'@param return_filtered boolean; if TRUE, returns the filtered data instead
#'of any results. Might be useful when assessing plausibility.
#'@return dataset with age groups and dates

gather_age_groups <- function(
  input_data, 
  subgroup = "keine",
  min_date = NULL,
  max_date = NULL,
  detailed_per_date = FALSE,
  age_intervals = c(0,6,12,16,20,35,60,80),
  vacc_status = 'alle',
  return_filtered = FALSE) {
  
  ## prepare age
  agegroups <- make_agegroups(age_intervals)
  
  ## add Erstmeldung / Meldedatum, if not present
  input_data <- replace_meldedatum(input_data)
  
  ## prepare dates and restrict for plausibility
  input_data <-  input_data %>%   
    filter(Meldedatum < Sys.Date())
  
  if (is.null(max_date)) { max_date <- max(input_data$Meldedatum) %>% 
    as.Date() } else { max_date <- as.Date(max_date) %>% assert_date() }
  
  if (is.null(min_date)) { min_date <- max_date } else { 
    min_date <- as.Date(min_date) %>% assert_date() }
  
  
  ## assertions
  assert_true(min_date <= max_date)
  assert_true(min_date >= min(input_data$Meldedatum))
  assertChoice(subgroup, c("keine",
                           "hospitalisiert",
                           "ICU",
                           "aktuell ICU", 
                           "aktuell hospitalisiert",
                           "in Zeitraum aufgenommen und noch hospitalisiert",
                           "in Zeitraum aufgenommen und noch ICU"))
  
  ## prepare time stamps
  data_temp <- input_data %>%
    
    {if (subgroup %in% c('hospitalisiert',
                         'in Zeitraum aufgenommen und noch hospitalisiert')) {
      filter(., hospitalisiert == "ja")
    } else { . }
    } %>%
    
    {if (subgroup %in% c('ICU',
                         'in Zeitraum aufgenommen und noch ICU')) {
      filter(., Hospitalisierung_Krankenhaus_ITS == "ja")
    } else { . }
    } %>%
    
    {if (subgroup %in% c('aktuell hospitalisiert')) {
      filter(., aktuell_hospitalisiert == "ja")
    } else { . }
    } %>%
    
    {if (subgroup %in% c('aktuell ICU')) {
      filter(., Hospitalisierung_Krankenhaus_ITS == "ja")
    } else { . }
    } %>%
    
    mutate(
      start_date = as.Date(
        Hospitalisierung_Krankenhaus_Aufnahme, format = c("%d.%m.%Y")),
      end_date = as.Date(
        Hospitalisierung_Krankenhaus_Entlassung, format = c("%d.%m.%Y")),
      start_date_its = as.Date(
        Hospitalisierung_Krankenhaus_ITS_von, format = c("%d.%m.%Y")),
      end_date_its = as.Date(
        Hospitalisierung_Krankenhaus_ITS_bis, format = c("%d.%m.%Y")),
      Alter_Erstmeldung = as.numeric(as.character(Alter_Erstmeldung))
    )
  
  ## set date variable
  input_date_var <- find_date_var(subgroup. = subgroup)
  
  ## filter for vaccination status
  data_temp <- add_vaccination_status(input_data = data_temp,
                                      vacc_status. = vacc_status)
  
  
  ## modify gender
  z <- which(data_temp$Person_Geschlecht == "divers")
  data_temp$Person_Geschlecht[z] <- NA
  
  ## reorganize data
  filtered_data <- data_temp %>%
    
    {if (subgroup == 'in Zeitraum aufgenommen und noch hospitalisiert') {
      
      filter(., is.na(end_date) | 
               as.Date(end_date, format = c("%d.%m.%Y") ) > max_date)
    } else { . }   
      
    } %>%
    
    {if (subgroup == 'in Zeitraum aufgenommen und noch ICU') {
      
      filter(., !is.na(start_date_its) & 
               (is.na(end_date_its) | 
                  as.Date(end_date_its, format = c("%d.%m.%Y") ) > max_date))
    } else { . }   
      
    } 
  
  ## handle dates (many NAs, however - we need them all when the variable
  ## is 'aktuell ...')
  
  if (!(grepl('aktuell ', subgroup))) {
    
    filtered_data <- filtered_data %>%
      filter(!is.na(get(input_date_var)))
    
  } else {
    
    ## set NAs to 2020-01-01
    missing_dates <- which(is.na(filtered_data[, input_date_var]))
    filtered_data[missing_dates, input_date_var] <- as.Date('2020-01-01')
    
  }
  
  
  age_data <- filtered_data %>%
    
    group_by(get(input_date_var)) %>%
    mutate(Datum = as.Date(get(input_date_var), 
                           format = "%d.%m.%Y")) %>%
    mutate(
      Altersgruppe_index = findInterval(
        Alter_Erstmeldung, 
        agegroups[[3]]
      )
    ) %>% 
    
    mutate(Altersgruppe = agegroups[[1]][Altersgruppe_index]) %>%
    mutate(Altersgruppe = replace_na(Altersgruppe, "age_unknown")) %>%
    mutate(Person_Geschlecht = replace_na(Person_Geschlecht, "unbekannt")) %>%
    mutate(Person_Geschlecht = str_sub(Person_Geschlecht, 1, 1)) %>%
    mutate(Person_Geschlecht = replace(Person_Geschlecht, 
                                       str_ends(Person_Geschlecht, "u"), 
                                       "na")) %>%
    group_by(Datum, Altersgruppe, Person_Geschlecht) %>%
    summarise(n = n()) %>%
    bind_rows(
      crossing(
        Datum = as.Date("2020-01-28"), 
        Altersgruppe = agegroups[[1]], 
        Person_Geschlecht = c("m", "w", "na"), 
        n = 0
      )
    ) %>%
    ungroup()
  
  
  ## expand data to all dates (and restrict again later)
  join_vars <- c('Datum', 'Altersgruppe', 'Person_Geschlecht')
  date_vector <- as.Date(min(age_data$Datum):max(age_data$Datum), 
                         origin = '1970-01-01')
  
  all_dates <- expand.grid(
    date_vector,
    agegroups[[1]], c('m','w','na')) %>%
    magrittr::set_colnames(join_vars)
  
  ## granular
  age_data_details <- all_dates %>%
    full_join(
      age_data,
      by = join_vars
    ) %>%
    arrange(desc(Datum)) %>%
    replace_na(list(n = 0)) %>%
    { if (!(subgroup %in% c('aktuell hospitalisiert', 'aktuell ICU'))) {
      filter(., Datum <= max_date, Datum >= min_date) 
    } else { . }
    }
  
  
  ## aggregated
  age_data_sum <- age_data_details %>%
    group_by(Altersgruppe, Person_Geschlecht) %>%
    summarise(n = sum(n)) %>%
    ungroup() %>%
    { if (subgroup %in% c('aktuell hospitalisiert', 'aktuell ICU')) {
      mutate(., Datum = max(input_data$Meldedatum))
    } else {
      mutate(., Datum = max(age_data$Datum))
    }
    }
  
  ## in case the date restrictions exceed the data
  if (nrow(age_data_sum) == 0) {
    
    age_data_details <- all_dates %>%
      filter(Datum == max(Datum)) %>%
      mutate(Datum = max_date,
             n = NA)
    
    age_data_sum <- age_data_details
    
  }
  
  if (min_date != max_date | 
      subgroup %in% c('aktuell hospitalisiert', 'aktuell ICU')) {
    
    age_data_sum <- age_data_sum %>%
      mutate(marker = "kumuliert")
    
    ## order data
    age_data <- age_data_sum[, c(
      'Datum','Altersgruppe','Person_Geschlecht','n', 'marker')]
    
  }
  
  ## return results
  if (return_filtered == FALSE) {
    
    if (detailed_per_date == FALSE) {
      
      return(age_data_sum)
      
    } else if (detailed_per_date == TRUE) {
      
      return(age_data_details)
      
    }
    
  } else {
    
    return(filtered_data)
    
  }
  
}

#'@description prepares data for reporting a results table
#'@param data; data.frame; the output of gather_age_groups
#'@param k; positive integer; shifts the date 'k' days into the past
#'@return returns a summary of the data organized by age and gender

get_age_table <- function(data, k) {
  
  ## get age intervals in data  
  age_intervals. <- infer_age_intervals(data)
  agegroups <- make_agegroups(age_intervals = age_intervals.)
  
  ## if NOT cumulative
  if (!('marker' %in% colnames(data))) {
    
    data <- data %>%
      arrange(Datum) %>%
      filter(Datum == max(Datum)-k) %>%
      dplyr::select(-Datum) %>%
      mutate(Altersgruppe = gsub("age_", "", Altersgruppe)) %>%
      rename(Geschlecht = Person_Geschlecht) %>%
      full_join(
        
        expand.grid(
          agegroups[[1]], 
          c('m','w','na')
        ) %>%
          mutate(Var1 = gsub("age_", "", Var1)) %>%
          magrittr::set_colnames(c('Altersgruppe', 'Geschlecht')),
        
        by = c('Altersgruppe', 'Geschlecht')
        
      ) %>%
      replace_na(list(n = 0)) 
    
    
  } else {
    data <- data %>%
      rename(Geschlecht = Person_Geschlecht)    
  }
  
  ## prepare table with lables
  age_labels <- cbind(agegroups[[1]], agegroups[[3]]) %>%
    as.data.frame() %>%
    magrittr::set_colnames(c("Altersgruppe", "Numeric")) %>%
    mutate(Altersgruppe = gsub("age_", "", Altersgruppe),
           Numeric = as.numeric(Numeric))
  
  ## prepare table with precentages
  age_tab_data <-   data %>%
    mutate(Altersgruppe = gsub("age_", "", Altersgruppe)) %>%
    spread(Geschlecht, n) %>%
    full_join(.,
              age_labels,
              by = c("Altersgruppe")
    ) %>%
    arrange(Numeric) %>%
    dplyr::select(-Numeric) %>%
    mutate(sum = m + na + w) %>%
    # Anteile berechnen
    # Wenn Summe 0 ist, dann nimm 1, um Divison durch 0 zu vermeiden
    mutate(m_perc = m / max(sum(m), 1)) %>%
    mutate(w_perc = w / max(sum(w), 1)) %>%
    mutate(na_perc = `na` / max(sum(`na`), 1)) %>%
    mutate(sum_perc = `sum` / max(sum(`sum`), 1)) %>%
    # Prozentangaben formatieren: 
    # %...f% bedeutet dass hier eine Kommazahl eingesetzt wird, 
    # %1.2f% meint mindestens eine Stelle vor dem Komma
    # und maximal zwei Stellen nach dem Komma
    mutate(across(ends_with("perc"), ~sprintf("%1.2f%%", .x * 100))) %>%
    # Finalisieren für Layout im Tagesbericht
    dplyr::select(sum, sum_perc, w, w_perc, m, m_perc, `na`, na_perc) %>%
    magrittr::set_colnames(c("Gesamt", 
                             "Gesamt %" , 
                             "W", 
                             "W %", 
                             "M" , 
                             "M %", 
                             "NA", 
                             "NA %")) %>% 
    as.data.frame() %>%
    magrittr::set_rownames(c(agegroups[[2]]))
  
  
  return(age_tab_data)
}

#'@descripiton Turns the previously prepared results into a html table
#'@oaram data; data.frame as returned by gahter_age_groups()
#'@param k; postivie integer; see get_age_table()
#'@return html table with the results

make_age_tab <- function(data, k = 0) {
  
  ## reference date
  reference_date <- data %>%
    filter(Datum == max(Datum)-k) %>%
    tail(1) %>% 
    mutate(Datum = format(Datum, "%d.%m.%Y")) %>%
    dplyr::select(Datum)
  
  ## knit table
  age_tab <- data %>%
    
    get_age_table(., k) %>%
    
    ## kable
    kable() %>%
    kable_styling(c("bordered", "condensed", "striped", "hover"), 
                  full_width = TRUE) %>%
    footnote(general = reference_date,
             general_title  = "Referenzdatum: ",
             title_format = c("italic", "underline")) %>%
    footnote(general = "Gesundheitsreferat, Meldewesen",
             general_title  = "Quelle: ",
             title_format = c("italic", "underline")) %>%
    footnote(general = paste0("NA steht hier für nicht ", 
                              "angegebene Information zum Geschlecht."),
             general_title  = " ",
             title_format = c("italic", "underline")) 
  
  
  return(age_tab)
  
}


#'@description prepares groups and labels per age-group
#'@param age_intervals numeric; interval boundaries of age categories
#'returns age-groups, their numeric values  labels

make_agegroups <- function(age_intervals) {
  
  a <- age_intervals
  a <- a[!is.na(a)]
  
  ## assertions
  assert_numeric(a)
  assert_true(all(a >= 0))
  
  ## groups numeric
  a <- c(0, a)
  a <- sort(a) %>% unique()
  a_numeric <- c(a, 150)
  
  ## groups
  a_temp <- paste0("age_", a, "to", a[-1]-1)
  a_temp[length(a_temp)] <- paste0("age_", a[length(a)], "plus") 
  a_groups <- c(a_temp, "age_unknown")
  
  ## labels
  a_labels <- gsub("age_", "", a_groups) %>%
    gsub("to", " bis ", .)
  a_labels[length(a_groups)-1] <- paste0(a[length(a)], " oder älter")
  a_labels[length(a_groups)] <- "ohne Angabe"
  
  a_out <- list(a_groups, a_labels, a_numeric)
  
  return(a_out)
  
}

#'@description Filters the input data according to the status of 
#'vaccination
#'@param input_data, data.frame; some verson of octo_data
#'@param vacc_status character; one of "vollständig geimpft","teilweise geimpft","alle"
#'returns a filtered version of the input_data

add_vaccination_status <- function(input_data, vacc_status. = 'alle') {
  
  ## assertion on input
  assert_true(vacc_status. %in% c('vollständig geimpft', 
                                  'teilweise geimpft', 
                                  'alle',
                                  'ungeimpft',
                                  'geimpft'))
  
  out_data <- input_data %>%
    
    ## prepare data
    mutate(
      Anzahl_Impfungen = as.numeric(as.character(Merkmal_AnzahlImpfungen)) %>%
             suppressWarnings(),
      vacc_known = case_when(
             Merkmal_JemalsGeimpft %in% c('ja', 'nein') ~ 1,
             !(Merkmal_JemalsGeimpft %in% c('ja', 'nein')) ~ 0),
      time_since_vacc = as.numeric(
        as.Date(Meldedatum) - as.Date(Merkmal_DatumLetzterImpfung))
    ) %>%
    
    ## prepare status
    mutate(vollstaendig_geimpft = ifelse(
      (Anzahl_Impfungen >= 2) & (time_since_vacc > 14), 1, 0), ) %>%
    mutate(einmal_geimpft  = ifelse(
      (((Anzahl_Impfungen == 1) & (time_since_vacc > 14)) | 
         ((Anzahl_Impfungen >= 2) & (time_since_vacc <= 14)) |
         ((Anzahl_Impfungen >= 2) & (is.na(time_since_vacc)))), 1, 0), ) %>%
    mutate(ungeimpft = ifelse(((vacc_known == 1 & einmal_geimpft == 0 & 
                                  vollstaendig_geimpft == 0) |
                                 ((vacc_known == 1) & is.na(einmal_geimpft) & 
                                    is.na(vollstaendig_geimpft))), 1, 0)) %>%
    replace_na(list(einmal_geimpft = 0, vollstaendig_geimpft = 0)) %>%
    mutate(geimpft = einmal_geimpft + vollstaendig_geimpft) %>%
    
    ## handle still remaining missings in 'ungeimpft' 
    mutate(ungeimpft = ifelse(
      (einmal_geimpft == 0 & vollstaendig_geimpft == 0
       & vacc_known == 1), 1, ungeimpft)) %>%
    
    ## filter status
    {if (vacc_status. == 'vollständig geimpft') {
      filter(., vollstaendig_geimpft == 1)
    } else if (vacc_status. == 'teilweise geimpft') {
      filter(., einmal_geimpft == 1) 
    } else if (vacc_status. == 'geimpft') {
      filter(., geimpft == 1) 
    } else if (vacc_status. == 'ungeimpft') {
      filter(., ungeimpft == 1) } else { . } 
    }
  
  return(out_data)
  
}

#'@description helper function to infer age groups from data
#'@param data, data as it results from gather_age_groups()
#'returns age intervals

infer_age_intervals <- function(data) {
  
  assert_true("Altersgruppe" %in% colnames(data))
  
  age_groups <- data$Altersgruppe %>% unique()
  age_groups <- gsub("age_", "", age_groups)
  age_groups <- vapply(strsplit(age_groups, "to", fixed = TRUE), "[", "", 1)
  age_groups <- gsub("plus", "", age_groups)
  age_groups <- age_groups[which(age_groups != "unknown")]
  age_groups <- as.numeric(age_groups) %>% sort()
  
  return(age_groups)
  
}

#'@description helper function for choosing the correct date variable
#'@param subgroup., see subgroup in gather_age_groups()
#'returns the appropriate age variable according to the subgroup

find_date_var <- function(subgroup. = subgroup) {
  
  assertChoice(subgroup., c("keine",
                            "hospitalisiert",
                            "ICU",
                            "aktuell hospitalisiert",
                            "aktuell ICU",
                            "in Zeitraum aufgenommen und noch hospitalisiert",
                            "in Zeitraum aufgenommen und noch ICU"))
  
  if (subgroup. %in% c('in Zeitraum aufgenommen und noch ICU',
                       'ICU',
                       'aktuell ICU')) {
    
    input_date_var <- "start_date_its"
    
  } else if (subgroup. %in% c('in Zeitraum aufgenommen und noch hospitalisiert',
                              'hospitalisiert',
                              'aktuell hospitalisiert')) {
    
    input_date_var <- "start_date"
    
  } else if (subgroup. %in% c("keine")) {
    
    input_date_var <- "Meldedatum"
    
  }
  
  return(input_date_var)
  
}

#'@details The column "Meldedatum" is not present when working with the
#'newer data (the API changed). This helper functions adds a column
#'"Erstmeldung" to the data.
#'@param input_data, data.frame; some type of octo_data
# returns a version of octo_data which surely contains a 'Erstmeldung' column
replace_meldedatum <- function(input_data = octo_data) {
  
  ## when working with octoware_kurz, Erstmeldung is named
  ## Meldedatum
  
  if (("Erstmeldung" %in% colnames(input_data))) {
    
    input_data$Meldedatum <- as.Date(input_data$Erstmeldung)
    
  } else if (("Meldedatum" %in% colnames(input_data))) {
    
    input_data$Erstmeldung <- as.Date(input_data$Meldedatum)
    
  }
  
  return(input_data)
  
}


#'@description Calculates the 7-days incidence for unvaccinated and vaccinated population
#'@param octo_data; data object, the octoware table
#'@param input_date_var character; the date columns form octo_data to be used
#'@param age_lower positive integer; set a lower bound on age
#'@param age_upper postive integer; set an upper bound on age
#'@param vacc_status The vaccination status. Either "alle", "geimpft" or "ungeimpft"
#'@return data.frame; organized by date, incidence and beds
make_7daysInz_vacc <- function(
  octo_data, 
  input_date_var = 'Erstmeldung',
  age_lower = NULL,
  age_upper = NULL,
  vacc_status = "alle") {
  
  assert_true(vacc_status %in% c('alle',
                                 'ungeimpft',
                                 'geimpft'))
  
  ## add Erstmeldung / Meldedatum, if not present
  octo_data <- replace_meldedatum(octo_data)
  
  population_size <- 1488202
  population_size_100T <- 1488202 / 100000
  
  ##fit population size for vaccinated and non-vaccinated population
  vacc_data <- readxl::read_excel(paste0(shared_data, "Impfquote.xlsx"))
  last_row <- tail(vacc_data, n = 1)
  vacc_pop <- last_row$`Zweitimpfung Impfzentrum Messe`+
    last_row$`Zweitimpfung Isarklinikum`+last_row$`Zweitimpfung Kliniken`+ 
    last_row$`Zweitimpfung Mobil`+ last_row$`Zweitimpfung Praxen`
  
  if(vacc_status=="geimpft") {
    population_size_100T <- vacc_pop / 100000
  } else if(vacc_status=="ungeimpft") {
    population_size_100T <- (population_size-vacc_pop) / 100000
  }
  
  return(make_7daysInz(octo_data,
                       input_date_var,
                       age_lower,
                       age_upper,
                       population_size_100T,
                       vacc_status))
}

#'@description calculates the 7 days incidence for according to vaccination
#'status based on the number of vaccinated people observed 14 days ago;
#'vaccination status is similar to our definitions in add_vaccination_status().
#'However, we cannot distinguish between vaccinations which only require one 
#'shot instead of multiple shots.
#'@param all, see make_7daysInz
#'returns a data.frame with the 7 days incidence of the (filtered) input data

make_7daysInz_vacc_history <- function(
  input_data = octo_data, 
  input_date_var = 'Erstmeldung',
  age_lower = NULL,
  age_upper = NULL,
  vacc_status = "alle",
  EW_MUC = 14.88202,
  vacc_path = shared_data,
  vacc_data_input = NULL) {
  
  ## add Erstmeldung / Meldedatum, if not present
  input_data <- replace_meldedatum(input_data)
  
  ## initialize variables for passing them on
  vacc_status. <- vacc_status
  age_upper. <- age_upper
  age_lower. <- age_lower
  input_date_var. <- input_date_var
  
  ## load vaccination data
  if (is.null(vacc_data_input)) {
    
    vacc_data <- readxl::read_excel(paste0(vacc_path, "Impfquote.xlsx"))
    
  } else {
    
    vacc_data <- vacc_data_input
    
  }
  
  ## helper function
  devide_by_100K <- function(x) {x / 100000}
  
  ## asure we only use the most recent entry per date
  vacc_temp <- vacc_data %>%
    group_by(Datum) %>%
    filter(ObjectID == max(ObjectID)) %>%
    ungroup() %>%
    ## gather those vaccinated at least once and those vaccinated twice
    mutate(
      pop_erst = rowSums(
        dplyr::select(., contains("Erstimpfung")), 
        na.rm = TRUE),
      pop_zweit = rowSums(
        dplyr::select(., contains("Zweitimpfung")),
        na.rm = TRUE)
    ) %>%
    ## the number of those only vaccinated once
    mutate(pop_geimpft = pop_erst,
           pop_ungeimpft = (EW_MUC * 100000) - pop_geimpft) %>%
    ## get those who only got vaccinated once
    mutate(pop_einmal = pop_erst - pop_zweit) %>%
    ## shift the date into the future (by 14 days)
    mutate(Datum_original = as.Date(Datum),
           Datum = Datum_original + 14) %>%
    dplyr::select(Datum, Datum_original, pop_erst, pop_zweit, pop_geimpft,
                  pop_ungeimpft, pop_einmal) %>%
    ## devide by 100.000 and round
    mutate_at(vars(contains('pop_')), devide_by_100K) %>%
    ## add a general population size for MUC
    mutate(pop_alle = EW_MUC) %>%
    
    ## select relevant columns by vacc_status
    {if (vacc_status. == 'vollständig geimpft') {
      dplyr::select(., c('Datum', 'Datum_original', 'pop_zweit'))
    } else if (vacc_status. == 'teilweise geimpft') {
      dplyr::select(., c('Datum', 'Datum_original', 'pop_einmal'))
    } else if (vacc_status. == 'geimpft') {
      dplyr::select(., c('Datum', 'Datum_original', 'pop_geimpft'))
    } else if (vacc_status. == 'ungeimpft') {
      dplyr::select(., c('Datum', 'Datum_original', 'pop_ungeimpft'))
    } else if (vacc_status == 'alle') {
      dplyr::select(., c('Datum', 'Datum_original', 'pop_alle'))
    } 
    }
  
  ## get 7 days incidence
  seven_days_inz <- make_7daysInz(
    octo_data = input_data, input_date_var = input_date_var., 
    vacc_status = vacc_status.,
    age_lower = age_lower., age_upper = age_upper.) %>% 
    dplyr::select(Datum, contains('_Sum7days_'), contains('_n_')) %>%
    magrittr::set_colnames(c('Datum', 'sum7days', 'n')) %>%
    inner_join(
      vacc_temp %>%
        dplyr::select(Datum, contains('pop_')) %>%
        magrittr::set_colnames(c('Datum', 'pop')),
      by = c('Datum')) %>%
    mutate(inz7days = round(sum7days / pop, digits = 2))
  
  ## adjust colnames
  new_names <- paste0(c('sum7days', 'n', 'pop', 'inz7days'), 
                      '_', vacc_status., '_', input_date_var)
  
  seven_days_inz <- seven_days_inz %>% 
    magrittr::set_colnames(c('Datum', new_names))
  
  return(seven_days_inz)
  
}

#'@description Calculates the 7-days incidence
#'@param octo_data; data object, the octoware table
#'@param input_date_var character; the date columns form octo_data to be used
#'@param age_lower positive integer; set a lower bound on age
#'@param age_upper postive integer; set an upper bound on age
#'@param population_size_100T positive number; the population size per 100.000
#'@param return_filtered boolean; if TRUE, returns the filtered data instead
#'of any results. Might be useful when assessing plausibility.
#'@return data.frame; organzed by date, incidence and beds
make_7daysInz <- function(
  octo_data, 
  input_date_var = 'Erstmeldung',
  age_lower = NULL,
  age_upper = NULL,
  population_size_100T = 14.88202,
  vacc_status = "alle",
  return_filtered = FALSE
) {
  
  ## add Erstmeldung, if not present
  octo_data <- replace_meldedatum(octo_data)
  
  ## filter for vaccination status
  octo_modified <- add_vaccination_status(input_data = octo_data,
                                          vacc_status. = vacc_status)
  
  ## restrict dates for plausibility
  octo_modified <- octo_modified %>%
    filter(Erstmeldung < Sys.Date())
  
  
  ## filter age
  if (!(is.null(age_lower) & is.null(age_upper))) {
    
    if (is.null(age_lower)) {age_lower <- 0}
    if (is.null(age_upper)) {age_upper <- 200}
    
    ## start filtering
    octo_temp <- octo_modified %>%
      mutate(age = as.numeric(as.character(Alter_Erstmeldung))) %>%
      filter(!is.na(get(input_date_var)),
             age >= age_lower,
             age <= age_upper)
    
  } else {
    
    octo_temp <- octo_modified
    
  }
  
  ## 7 - days incidence
  temp_data <- octo_temp %>%
    group_by(get(input_date_var)) %>%
    summarise(n = n()) %>%
    mutate(Datum = `get(input_date_var)`) %>%
    dplyr::select(Datum, n) %>%
    distinct(Datum, .keep_all = TRUE) %>%
    mutate(Datum = 
             as.Date(Datum, 
                     format = "%d.%m.%Y")) %>%
    arrange(Datum)
  
  temp_data <- octo_data %>%
    distinct(Erstmeldung, .keep_all = TRUE) %>%
    mutate(Datum = Erstmeldung) %>%
    dplyr::select(Datum) %>%
    full_join(., temp_data, by = c('Datum')) %>%
    replace_na(list(n = 0)) %>%
    arrange(Datum)
  
  inz_data <- temp_data %>%
    mutate(Inz7days = zoo::rollapply(
      temp_data$n, 
      7, sum, fill = NA, align = "right") %>%
        '/'(population_size_100T) %>% tail(., n = (nrow(temp_data))) %>%
        round(., digits = 2),
      
      Sum7days = zoo::rollapply(
        temp_data$n, 
        7, sum, fill = NA, align = "right")
    ) %>%
    filter(Datum <= (max(octo_data$Erstmeldung)-1))
  
  
  new_names <- paste0(input_date_var, "_", 
                      colnames(inz_data)[2:4], "_", 
                      vacc_status) %>% gsub(" ", "", .)
  
  colnames(inz_data)[2:4] <- new_names
  
  
  ## return results
  if (return_filtered == FALSE) {
    
    return(inz_data)
    
  } else {
    
    return(octo_modified)
    
  }
  
}

#'@description gahters data of all, vaccinated and none-vaccinated people
#'and calculates the 7 days incidence statistics
#'@param input_data, data.frame; the octoware data
#'@param input_date_var character; name of date variable
#'@param vacc_data_input data.frame; vaccination data
#'@param vacc_status_ref character; name of reference group (e.g. 'geimpft') 
#'returns a data.frame with the 7 days inzidences

gather_inz_data <- function(input_data = octo_data,
                            input_date_var = 'Meldedatum',
                            vacc_data_input = NULL,
                            vacc_status_ref_1 = 'teilweise geimpft',
                            vacc_status_ref_2 = 'vollständig geimpft') {
  
  ## add Erstmeldung / Meldedatum, if not present
  input_data <- replace_meldedatum(input_data)
  
  ## initalize params for passing them on
  input_data. <- input_data
  input_date_var. <- input_date_var
  vacc_data_input. <- vacc_data_input
  
  ## group data
  inz_alle <- make_7daysInz_vacc_history(
    input_data = input_data., 
    input_date_var = input_date_var.,
    vacc_data_input = vacc_data_input.,
    vacc_status = 'alle')
  
  inz_geimpft_1 <- make_7daysInz_vacc_history(
    input_data = input_data., 
    input_date_var = input_date_var.,
    vacc_data_input = vacc_data_input.,
    vacc_status = vacc_status_ref_1)
  
  inz_geimpft_2 <- make_7daysInz_vacc_history(
    input_data = input_data., 
    input_date_var = input_date_var.,
    vacc_data_input = vacc_data_input.,
    vacc_status = vacc_status_ref_2)
  
  inz_ungeimpft <- make_7daysInz_vacc_history(
    input_data = input_data., 
    input_date_var = input_date_var.,
    vacc_data_input = vacc_data_input.,
    vacc_status = 'ungeimpft')
  
  ## join Inzidenzen
  inz_data <- inz_alle %>%
    full_join(., inz_geimpft_1, by = c('Datum')) %>%
    full_join(., inz_geimpft_2, by = c('Datum')) %>%
    full_join(., inz_ungeimpft, by = c('Datum')) %>%
    filter(Datum >= (max(as.Date(Datum)) - 6)) %>%
    # mutate_at(., vars( starts_with("n_") ), 
    #           funs( if_else( is.na(.), 0, .) )) %>%
    arrange(Datum)
  
  ## results
  return(inz_data)
  
}


#'@description prepares a html table with the incidences
#'@param inz_data, data.frame, results of gather_inz_data()
#'@param footnote_general, character; any comment
#'@param vacc_status_ref character; name of reference group (e.g. 'geimpft') 
#'returns a html table of the incidences

make_inz_tab <- function(inz_data,
                         footnote_general = "",
                         vacc_status_ref_1 = 'teilweise geimpft',
                         vacc_status_ref_2 = 'vollständig geimpft') {
  
  inz_tab <- inz_data %>%
    ## throw in blank columns
    mutate(BlankColumn = "",
           BlankColumn2 = "") %>%
    dplyr::select(Datum,
                  contains('n_alle'),
                  BlankColumn,
                  contains(paste0('n_', vacc_status_ref_1)),
                  contains(paste0('n_', vacc_status_ref_2)),
                  contains('n_ungeimpft'),
                  BlankColumn2,
                  contains('inz7days_alle'),
                  contains(paste0('inz7days_', vacc_status_ref_1)),
                  contains(paste0('inz7days_', vacc_status_ref_2)),
                  contains('inz7days_ungeimpft')) %>%  
    mutate(Datum = format(Datum, "%d.%m.%Y")) %>%
    t() %>%
    magrittr::set_colnames(.[1, ]) %>%
    .[2:nrow(.), ] %>%
    magrittr::set_rownames(c(
      "Neue Fälle in München",
      "Neue Fälle nach Impfstatus",
      paste0(" - ", vacc_status_ref_1),
      paste0(" - ", vacc_status_ref_2),
      " - ungeimpft",
      "Inzidenz pro 100.000",
      " - gesamt",
      paste0(" - ", vacc_status_ref_1),
      paste0(" - ", vacc_status_ref_2),
      " - ungeimpft"
    )) %>%
    kable() %>%
    kable_styling(c("bordered", "condensed", "striped", "hover"), 
                  full_width = TRUE) %>%
    footnote(general = "Gesundheitsreferat, Meldewesen",
             general_title  = "Quelle: ",
             title_format = c("italic", "underline")) %>%     
    footnote(general = footnote_general,
             general_title  = "Note:",
             title_format = c("italic", "underline"))
  
  return(inz_tab)
}


