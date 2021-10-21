
# r scripts --------------------------------------------------------------------

## path to user input
user_input <- paste0(path_tagesberichte, 'data/input/')

## path to derived data
derived_data <- paste0(path_tagesberichte, 'data/derived/')

## path to shared data
shared_data <- paste0(path_tagesberichte, 'data/shared/')

## path to shared input
shared_input <- paste0(path_tagesberichte, 'data/shared/input_shared/')

## path to input for internal sources
intern_input <- paste0(path_tagesberichte, 'data/intern/')

## path to folder for Branddirektion
brand_data <- path_tagesberichte_branddir

## path to simulations project
prognosen_input <- "L:/Thematische-Ordner/Corona/R_Projekte/Prognosen/input/"

## path to raw CoVe data
cove_input <- paste0(path_tagesberichte, 'data/intern/CoVe/')

## path to raw lgl data
lgl_input <- paste0(path_tagesberichte, 'data/input/lgl/')

## path to excel directory
excel_path <- paste0(path_tagesberichte, 'data/excel/')

# rmd scripts ------------------------------------------------------------------

## path variables
wd_path <- paste0(path_tagesberichte, "docs//")

wd_path1 <- paste0(path_tagesberichte, "//data//derived//")

wd_path2 <- paste0(path_tagesberichte, "data//input//")

wd_path3 <- paste0(path_tagesberichte, "//data//img//")

letter_lhm_ablage <- stringr::str_sub(path_tagesberichte_branddir, start = 1, end = 1)

wd_path4 <- paste0(letter_lhm_ablage, ":/BFM/Austausch-RGU-BD/Reports_RGU/")

wd_path5 <- "./"

wd_path6 <- paste0(letter_lhm_ablage, ":/BFM/Austausch-RGU-BD/Reports_BD/")

wd_path_muc <- paste0(letter_lhm_ablage, "://DIR//Coronazahlen//")



## shared_data (defined above)
