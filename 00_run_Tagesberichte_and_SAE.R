
# libraries --------------------------------------------------------------------

install.packages("kableExtra")
install.packages("synthpop")
install.packages("writexl")
install.packages("stringr")
install.packages("xlsx")
install.packages("zoo")

library(deSolve)
library(R0)
library(tidyverse)
library(useful)
library(ggthemes)
library(knitr)
library(zoo)

options(scipen = 999)

Sys.setenv(RSTUDIO_PANDOC = "C:/Program Files/RStudio/bin/pandoc")

# boolean for synthesizing and copying data -----------------------------------

rm(list=ls())

synthesize_data <- TRUE

third_party <- FALSE


# paths ------------------------------------------------------------------------

if (third_party == FALSE) {
  
  ## main path
  path_tagesberichte <- "L:/Thematische-Ordner/Corona/R_Projekte/Tagesbericht/"
  path_tagesberichte_branddir <- "M:/BFM/Austausch-RGU-BD/Reports_RGU/"
  
  source(paste0(path_tagesberichte, "scripts/22_path_variables.R"),
         encoding = 'utf-8')
  
} else {
  
  ## main path
  # path_tagesberichte <- "L:/Thematische-Ordner/Corona/R_Projekte/Tagesbericht/"
  # path_tagesberichte_branddir <- "M:/BFM/Austausch-RGU-BD/Reports_RGU/"
  
  path_tagesberichte <- "../"
  
  path_tagesberichte_branddir <- "../branddir/"
  
  if(!dir.exists(path_tagesberichte_branddir)){
    
    dir.create(path_tagesberichte_branddir)
    
  }
  
  source(paste0(path_tagesberichte, "22_path_variables_STATUP.R"),
         
         encoding = 'utf-8')
  
  synthesize_data <- FALSE
  
}

## all path variables
path_vars_list <- c(ls(), 'path_vars_list')


# copy files -------------------------------------------------------------------


if (!file.exists(paste0(excel_path, 'baysim_fallakten.xlsx')) || file.mtime(paste0(excel_path, 'baysim_fallakten.xlsx')) < file.mtime(paste0(user_input, 'baysim_fallakten.xlsx')) ) {
  print('Kopiere baysim_fallakten.xlsx nach data/excel....')
  file.copy(
    from = paste0(user_input, 'baysim_fallakten.xlsx'),
    to = paste0(excel_path, 'baysim_fallakten.xlsx'),
    overwrite = TRUE
  )
}

if (!file.exists(paste0(excel_path, 'Inz_RKI_LGL.xlsx')) || file.mtime(paste0(excel_path, 'Inz_RKI_LGL.xlsx')) < file.mtime(paste0(user_input, 'Inz_RKI_LGL.xlsx')) ) {
  print('Kopiere Inz_RKI_LGL.xlsx nach data/excel....')
  file.copy(
    from = paste0(user_input, 'Inz_RKI_LGL.xlsx'),
    to = paste0(excel_path, 'Inz_RKI_LGL.xlsx'),
    overwrite = TRUE
  )
}

if (!file.exists(paste0(excel_path, 'octoware.xlsx')) || file.mtime(paste0(excel_path, 'octoware.xlsx')) < file.mtime(paste0(user_input, 'octoware.xlsx')) ) {
  print('Kopiere octoware.xlsx nach data/excel....')
  file.copy(
    from = paste0(user_input, 'octoware.xlsx'),
    to = paste0(excel_path, 'octoware.xlsx'),
    overwrite = TRUE
  )
}

if (!file.exists(paste0(excel_path, 'octoware_kurz.xlsx')) || file.mtime(paste0(excel_path, 'octoware_kurz.xlsx')) < file.mtime(paste0(user_input, 'octoware_kurz.xlsx')) ) {
  print('Kopiere octoware_kurz.xlsx nach data/excel....')
  file.copy(
    from = paste0(user_input, 'octoware_kurz.xlsx'),
    to = paste0(excel_path, 'octoware_kurz.xlsx'),
    overwrite = TRUE
  )
}

if (third_party == FALSE) {
  
  if (!file.exists(paste0(excel_path, 'baysim_unterkunft.xlsx')) || file.mtime(paste0(excel_path, 'baysim_unterkunft.xlsx')) < file.mtime(paste0(user_input, 'baysim_unterkunft.xlsx')) ) {
    print('Kopiere baysim_unterkunft.xlsx nach data/excel....')
    file.copy(
      from = paste0(user_input, 'baysim_unterkunft.xlsx'),
      to = paste0(excel_path, 'baysim_unterkunft.xlsx'),
      overwrite = TRUE
    )
  }
  
}

# synthesize data for debugging ------------------------------------------------

#'@details Wenn STATUP für die Fehlerbehebung 'Rohdaten' benötigt, dann die
#'Variable synthesize_data auf TRUE setzen. ACHTUNG: Das verlängert die Laufzeit 
#'erheblich! 

if (synthesize_data == TRUE) {
  
  ## synthesize octoware data
  source(file = paste0(path_tagesberichte, "scripts/19_synthesize_octo.R"),
         encoding = 'utf-8')
  
  ## synthesize ctt data
  source(file = paste0(path_tagesberichte, "scripts/20_synthesize_ctt.R"),
         encoding = 'utf-8')
}


# copy to input_shared ---------------------------------------------------------

#'@details copy data from input to input_shared

if (synthesize_data == TRUE) {
  
  ## tables to copy
  input_copy_list <- c(
    #"RKI_COVID19.csv",
    #"Bettenbelegung.xlsx",
    "Impfungen_Muenchen.csv",
    "Inz_RKI_LGL.xlsx",
    "R_bayern.csv",            
    "R_muenchen.csv",           
    "results-survey114441.csv", 
    "results-survey669457.csv",
    "results-survey938677.csv" 
  )        
  
  
  for (i in seq(length(input_copy_list))) {
    
    ## copy tables
    file.copy(
      from = paste0(user_input, input_copy_list[i]),
      to = paste0(shared_input,  input_copy_list[i]),
      overwrite = TRUE
    )
    
  }
  
  ## get most recent copy of lgl export
  lgl_files <- file.info(list.files(path = lgl_input,
                                    pattern = ".htm$", full.names = TRUE))
  
  copy_lgl <- lgl_files %>%
    filter(ctime == max(lgl_files$ctime)) %>%
    rownames()
  
  copy_lgl_name <-  gsub(
    "L:/Thematische-Ordner/Corona/R_Projekte/Tagesbericht/data/input/lgl/",
    "", copy_lgl)
  
  ## copy it to shared_input
  file.copy(
    from = copy_lgl,
    to = paste0(shared_input, "lgl/", copy_lgl_name),
    overwrite = TRUE
  )
  
}

# check prerequisits -----------------------------------------------------------

#'@details A prerequisit for meaningful joins is that RKI_COVID19 amd
#'octoware.csv are fresh. Check if they have the same date (23.10.2020: not necessary any more!). 
#'Check, if the export from octoware is complete (bigger than 3k). 

if(
    ## check, if size is reasonable. It's either 3k or complete, but
    ## we set the value to 100000, just to be safe.
    file.info(
      paste0(user_input, "octoware.xlsx"))$size > 100000 
) {
  
  
  # prepare data ---------------------------------------------------------------
  
  ## Prepare RKI data
  source(file = paste0(path_tagesberichte, "scripts/01_prepare_RKI_cases.R"),
         encoding = 'utf-8')
  
  ## Prepare octoware data
  source(file = paste0(path_tagesberichte, "scripts/02_prepare_octoware.R"),
         encoding = 'utf-8')
  
  if (
    octo_complete$Datum %>% max() !=
    rki_cases_data$Datum %>% max()
  ) {
    print(
      paste0(
        "Main data sources are out of sync! Octoware ", 
        octo_complete$Datum %>% max(), 
        " vs RKI ", 
        rki_cases_data$Datum %>% max()
      )
    )
  }
  
  if (
    octo_complete$Datum %>% max() <
    rki_cases_data$Datum %>% max()
  ) {
    print("Octoware Data older than RKI Data")
  } else {
    

    rm(list=setdiff(ls(), path_vars_list))
    
    ## add R(t) and Verdopplungszahl to RKI cases data
    source(paste0(path_tagesberichte, "scripts/08_add_Rt_and_Verdopplungszahl.R"),
           encoding = 'utf-8')
    
    rm(list=setdiff(ls(), path_vars_list))
    
    ## Prepare octoware data
    source(paste0(path_tagesberichte, "scripts/09_join_octoware_and_RKI.R"),
           encoding = 'utf-8')
    
    rm(list=setdiff(ls(), path_vars_list))

    ## Prepare lgl data
    
    try(
    
    source(file = paste0(path_tagesberichte, "scripts/15_prepare_lgl_data.R"), 
           encoding = 'utf-8')
    , silent = TRUE
    
    )
    
    rm(list=setdiff(ls(), path_vars_list))
    
    try(
    
    source(paste0(path_tagesberichte, "scripts/18_prepare_ctt.R"),
           encoding = 'utf-8')
    , silent = TRUE
    
    )
    
    rm(list=setdiff(ls(), path_vars_list))
    
    source(paste0(path_tagesberichte, "scripts/21_prepare_ivena.R"),
           encoding = 'utf-8')
    
    rm(list=setdiff(ls(), path_vars_list))
    
    # build markdown -------------------------------------------------------------
    
    if (third_party == FALSE) {
      
      ## Tagesbericht no TOC
      rmarkdown::render(
        input = paste0(path_tagesberichte, "scripts/Tagesbericht_master.Rmd"),
        output_format = "html_document",
        output_file = paste0(path_tagesberichte, "docs/Tagesbericht"),
        params = list(
          evalEinrichtungen = TRUE,
          evalPatientenumfeld = FALSE,
          evalKontaktumfeld = FALSE,
          evalExpositionsland = FALSE,
          evalImpfungenKliniken = FALSE,
          evalAbbildung1 = FALSE,
          evalAbbildung2 = FALSE,
          evalAbbildung3 = FALSE,
          evalPreload = FALSE
        )
      )
      
      rm(list=setdiff(ls(), path_vars_list))
      
      ## Aktuelle Kennzahlen
      rmarkdown::render(
        input = paste0(path_tagesberichte, "scripts/Tagesbericht_master.Rmd"),
        output_format = "html_document",
        output_file = paste0(path_tagesberichte, "docs/Aktuelle_Kennzahlen"),
        params = list(
          evalAltersverteilungNeuinfektionen = FALSE,
          evalKontaktpersonen = FALSE,
          evalEinrichtungen = FALSE,
          evalAbbildung1 = FALSE,
          evalAbbildung2 = FALSE,
          evalAbbildung3 = FALSE,
          evalTagesberichtHeader = FALSE,
          evalAktuelleKennzahlenHeader = TRUE
        )
      )
      
      rm(list=setdiff(ls(), path_vars_list))
      
      ## Tagesbericht with interactive graphics
      rmarkdown::render(
        input = paste0(path_tagesberichte,
                       "scripts/Tagesbericht_master.Rmd"),
        output_format = "html_document",
        output_file = paste0(path_tagesberichte,
                             "docs/Tagesbericht_komplett"),
        params = list(evalKontaktpersonen = TRUE)
        )
      
      rm(list=setdiff(ls(), path_vars_list))
      
      ## Sonderauswertung Altersverteilung
      rmarkdown::render(
        input = paste0(path_tagesberichte,
                       "scripts/Sonderauswertung_Alter.Rmd"),
        output_format = "html_document",
        output_file = paste0(path_tagesberichte,
                             "docs/Altersverteilung"))
      
      ## Sonderauswertung Gesundheitsbeirat
      rmarkdown::render(
        input = paste0(path_tagesberichte,
                       "scripts/Sonderauswertung_Gesundheitsbeirat.Rmd"),
        output_format = "html_document",
        output_file = paste0(path_tagesberichte,
                             "docs/Gesundheitsbeirat"))
      
      
      ## Sonderauswertung Impfdurchbrueche
#      rmarkdown::render(
#        input = paste0(path_tagesberichte,
#                       "scripts/Sonderauswertung_Impfdurchbrueche.Rmd"),
#        output_format = "html_document",
#        output_file = paste0(path_tagesberichte,
#                             "docs/Impfdurchbrueche"))
      
    } else if (third_party == TRUE) {
      
      ## Tagesbericht no TOC
      rmarkdown::render(
        input = paste0(path_tagesberichte, "scripts/Tagesbericht_master.Rmd"),
        output_format = "html_document",
        output_file = paste0(path_tagesberichte, "docs/Tagesbericht"),
        params = list(
          evalThirdParty = TRUE,
          evalEinrichtungen = TRUE,
          evalPatientenumfeld = FALSE,
          evalKontaktumfeld = FALSE,
          evalExpositionsland = FALSE,
          evalImpfungenKliniken = FALSE,
          evalAbbildung1 = FALSE,
          evalAbbildung2 = FALSE,
          evalAbbildung3 = FALSE
        )
      )
      
      rm(list=setdiff(ls(), path_vars_list))
      
      ## Aktuelle Kennzahlen
      rmarkdown::render(
        input = paste0(path_tagesberichte, "scripts/Tagesbericht_master.Rmd"),
        output_format = "html_document",
        output_file = paste0(path_tagesberichte, "docs/Aktuelle_Kennzahlen"),
        params = list(
          evalThirdParty = TRUE,
          evalAltersverteilungNeuinfektionen = FALSE,
          evalKontaktpersonen = FALSE,
          evalEinrichtungen = FALSE,
          evalAbbildung1 = FALSE,
          evalAbbildung2 = FALSE,
          evalAbbildung3 = FALSE,
          evalTagesberichtHeader = FALSE,
          evalAktuelleKennzahlenHeader = TRUE
        )
      )
      
      rm(list=setdiff(ls(), path_vars_list))
      
      ## Tagesbericht with interactive graphics
      rmarkdown::render(
        input = paste0(path_tagesberichte,
                       "scripts/Tagesbericht_master.Rmd"),
        output_format = "html_document",
        output_file = paste0(path_tagesberichte,
                             "docs/Tagesbericht_komplett"),
        params = list(
          evalThirdParty = TRUE,
          evalKontaktpersonen = TRUE
        ))
      
      rm(list=setdiff(ls(), path_vars_list))
      
      ## Sonderauswertung Altersverteilung
      rmarkdown::render(
        input = paste0(path_tagesberichte,
                       "scripts/Sonderauswertung_Alter.Rmd"),
        output_format = "html_document",
        output_file = paste0(path_tagesberichte,
                             "docs/Altersverteilung"),
        params = list(evalThirdParty = TRUE)
      )
      
      ## Sonderauswertung Gesundheitsbeirat
      rmarkdown::render(
        input = paste0(path_tagesberichte,
                       "scripts/Sonderauswertung_Gesundheitsbeirat.Rmd"),
        output_format = "html_document",
        output_file = paste0(path_tagesberichte,
                             "docs/Gesundheitsbeirat"),
        params = list(evalThirdParty = TRUE)
      )
      
    }
    
  }
  
}


