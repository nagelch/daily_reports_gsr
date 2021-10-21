library(readr)
library(readxl)
library(stringr)
library(xlsx)
library(dplyr)


cut_date_mut <- Sys.Date()-14
cut_date_history <- as.Date("2021-06-01")


## read data 

octo_kurz <-
  readxl::read_excel(
    paste0(user_input, "octoware_kurz.xlsx"),
    guess_max = 100000
  )

octo <-
  readxl::read_excel(
    paste0(user_input, "octoware.xlsx"),
    guess_max = 100000
  )

#octo_VOC <- rbind(
#  octo_kurz,
#  octo %>%
#    filter(Meldedatum < min(octo_kurz$Meldedatum))
#  )

#VOC_pos <- subset(octo_VOC, grepl("MutPol pos", Bemerkung) == TRUE)
#VOC_pos <- subset(VOC_pos, as.Date(Meldedatum) < Sys.Date())

#VOC_neg <- subset(octo_VOC, grepl("MutPol neg", Bemerkung) == TRUE)
#VOC_neg <- subset(VOC_neg, as.Date(Meldedatum) < Sys.Date())


octo_data_recent <- readRDS(paste0(derived_data, "octo_data_history.rds"))
octo_data_recent <- octo_data_recent %>%
  mutate(Meldedatum = as.Date(Erstmeldung))

octo_data_recent <- subset(octo_data_recent, as.Date(Meldedatum) < Sys.Date())


## prepare data ----------------------------------------------------------------

inf_recent <- sum(as.Date(octo_data_recent$Meldedatum) >= cut_date_mut) 

#VOC_pos_recent <- nrow(subset(VOC_pos, as.Date(Meldedatum) >= cut_date_mut))
#VOC_pos_old <- nrow(subset(VOC_pos, as.Date(Meldedatum) < cut_date_mut))

#VOC_neg_recent <- nrow(subset(VOC_neg, as.Date(Meldedatum) >= cut_date_mut))
#VOC_neg_old <- nrow(subset(VOC_neg, as.Date(Meldedatum) < cut_date_mut))

b117_recent <- nrow(subset(octo_data_recent, 
                           str_detect(Merkmal_VirusVariante_Mutation, "B.1.1.7") == TRUE & 
                             as.Date(Meldedatum) >= cut_date_mut))

b117_old <- nrow(subset(octo_data_recent,
                        str_detect(Merkmal_VirusVariante_Mutation, "B.1.1.7") == TRUE & 
                          as.Date(Meldedatum) < cut_date_mut))

b1351_recent <- nrow(subset(octo_data_recent, 
                            (str_detect(Merkmal_VirusVariante_Mutation, "B.1.351") == TRUE |
                               (str_detect(Merkmal_Details_VirusVariante, "B.1.351") == TRUE  & 
                                  str_detect(Merkmal_Details_VirusVariante, "oder") == FALSE)) &
                            as.Date(Meldedatum) >= cut_date_mut))

b1351_old <- nrow(subset(octo_data_recent, 
                         (str_detect(Merkmal_VirusVariante_Mutation, "B.1.351") == TRUE |
                            (str_detect(Merkmal_Details_VirusVariante, "B.1.351") == TRUE  & 
                               str_detect(Merkmal_Details_VirusVariante, "oder") == FALSE)) & 
                         as.Date(Meldedatum) < cut_date_mut))

p1_recent <- nrow(subset(octo_data_recent,
                         (str_detect(Merkmal_VirusVariante_Mutation, "B.1.1.28.1") == TRUE |
                            (str_detect(Merkmal_Details_VirusVariante, "B.1.1.28.1") == TRUE  & 
                               str_detect(Merkmal_Details_VirusVariante, "oder") == FALSE)) &
                         as.Date(Meldedatum) >= cut_date_mut))
                  
p1_old <- nrow(subset(octo_data_recent,
                      (str_detect(Merkmal_VirusVariante_Mutation, "B.1.1.28.1") == TRUE |
                        (str_detect(Merkmal_Details_VirusVariante, "B.1.1.28.1") == TRUE  & 
                           str_detect(Merkmal_Details_VirusVariante, "oder") == FALSE)) &
                      as.Date(Meldedatum) < cut_date_mut))

b1617_recent <- nrow(subset(octo_data_recent,
                            (str_detect(Merkmal_VirusVariante_Mutation, "617") == TRUE |
                              (str_detect(Merkmal_Details_VirusVariante, "617") == TRUE  & 
                                 str_detect(Merkmal_Details_VirusVariante, "oder") == FALSE)) &
                            as.Date(Meldedatum) >= cut_date_mut))

b1617_old <- nrow(subset(octo_data_recent,
                         (str_detect(Merkmal_VirusVariante_Mutation, "617") == TRUE |
                           (str_detect(Merkmal_Details_VirusVariante, "617") == TRUE  & 
                              str_detect(Merkmal_Details_VirusVariante, "oder") == FALSE)) &
                         as.Date(Meldedatum) < cut_date_mut))

## subsets of b1617
b1617_1_recent <- nrow(subset(octo_data_recent,
                            (str_detect(Merkmal_VirusVariante_Mutation, "617.1") == TRUE |
                               (str_detect(Merkmal_Details_VirusVariante, "617.1") == TRUE  & 
                                  str_detect(Merkmal_Details_VirusVariante, "oder") == FALSE)) &
                              as.Date(Meldedatum) >= cut_date_mut))

b1617_1_old <- nrow(subset(octo_data_recent,
                         (str_detect(Merkmal_VirusVariante_Mutation, "617.1") == TRUE |
                            (str_detect(Merkmal_Details_VirusVariante, "617.1") == TRUE  & 
                               str_detect(Merkmal_Details_VirusVariante, "oder") == FALSE)) &
                           as.Date(Meldedatum) < cut_date_mut))

b1617_2_recent <- nrow(subset(octo_data_recent,
                              (str_detect(Merkmal_VirusVariante_Mutation, "617.2") == TRUE |
                                 (str_detect(Merkmal_Details_VirusVariante, "617.2") == TRUE  & 
                                    str_detect(Merkmal_Details_VirusVariante, "oder") == FALSE) |
                                 str_detect(Merkmal_Details_VirusVariante, "AY.") == TRUE) &
                                as.Date(Meldedatum) >= cut_date_mut))

b1617_2_old <- nrow(subset(octo_data_recent,
                         (str_detect(Merkmal_VirusVariante_Mutation, "617.2") == TRUE |
                            (str_detect(Merkmal_Details_VirusVariante, "617.2") == TRUE  & 
                               str_detect(Merkmal_Details_VirusVariante, "oder") == FALSE) |
                            str_detect(Merkmal_Details_VirusVariante, "AY.") == TRUE) &
                           as.Date(Meldedatum) < cut_date_mut))




## count B.1.351, P.1 and B.1.617 separately -------------------------------------

octo_b1351 <- subset(octo_data_recent, 
                     str_detect(Merkmal_VirusVariante_Mutation, "B.1.351") == TRUE |
                       (str_detect(Merkmal_Details_VirusVariante, "B.1.351") == TRUE  & 
                          str_detect(Merkmal_Details_VirusVariante, "oder") == FALSE),
                     select = c(ID, Vorgangsnummer, Meldedatum, Merkmal_VirusVariante_Mutation, Merkmal_Details_VirusVariante))

octo_b1351$Meldedatum <- format(octo_b1351$Meldedatum, "%d.%m.%Y")

write.xlsx(octo_b1351, 
           paste0(path_tagesberichte,
                  "docs/B1351.xlsx"))



octo_p1 <- subset(octo_data_recent, 
                  str_detect(Merkmal_VirusVariante_Mutation, "B.1.1.28.1") == TRUE |
                    (str_detect(Merkmal_Details_VirusVariante, "B.1.1.28.1") == TRUE  & 
                       str_detect(Merkmal_Details_VirusVariante, "oder") == FALSE),
                  select = c(ID, Vorgangsnummer, Meldedatum, Merkmal_VirusVariante_Mutation, Merkmal_Details_VirusVariante))

octo_p1$Meldedatum <- format(octo_p1$Meldedatum, "%d.%m.%Y")

write.xlsx(octo_p1, 
           paste0(path_tagesberichte,
                  "docs/P1.xlsx"))



octo_b1617 <- subset(octo_data_recent, 
                     str_detect(Merkmal_VirusVariante_Mutation, "617") == TRUE |
                       (str_detect(Merkmal_Details_VirusVariante, "617") == TRUE  & 
                          str_detect(Merkmal_Details_VirusVariante, "oder") == FALSE) |
                       str_detect(Merkmal_Details_VirusVariante, "AY.") == TRUE,
                     select = c(ID, Vorgangsnummer, Meldedatum, Merkmal_VirusVariante_Mutation, Merkmal_Details_VirusVariante))

octo_b1617$Meldedatum <- format(octo_b1617$Meldedatum, "%d.%m.%Y")

write.xlsx(octo_b1617, 
           paste0(path_tagesberichte,
                  "docs/B1617.xlsx"))


### export potentially implausible entries

#octo_impl <- subset(octo_VOC, 
#                    (grepl("MutPol neg", Bemerkung) == TRUE &
#                     ((str_detect(Merkmal_VirusVariante_Mutation, "B.1.351") == TRUE |
#                         (str_detect(Merkmal_Details_VirusVariante, "B.1.351") == TRUE  & 
#                            str_detect(Merkmal_Details_VirusVariante, "oder") == FALSE)) |
#                        (str_detect(Merkmal_VirusVariante_Mutation, "B.1.1.28.1") == TRUE |
#                           (str_detect(Merkmal_Details_VirusVariante, "B.1.1.28.1") == TRUE  & 
#                              str_detect(Merkmal_Details_VirusVariante, "oder") == FALSE)) |
#                        (str_detect(Merkmal_VirusVariante_Mutation, "617") == TRUE |
#                           (str_detect(Merkmal_Details_VirusVariante, "617") == TRUE  & 
#                              str_detect(Merkmal_Details_VirusVariante, "oder") == FALSE)))) |
#                      (grepl("MutPol pos", Bemerkung) == TRUE &
#                         ((str_detect(Merkmal_VirusVariante_Mutation, "B.1.351") == FALSE &
#                             (str_detect(Merkmal_Details_VirusVariante, "B.1.351") == FALSE)) &
#                            (str_detect(Merkmal_VirusVariante_Mutation, "B.1.1.28.1") == FALSE &
#                               (str_detect(Merkmal_Details_VirusVariante, "B.1.1.28.1") == FALSE)) &
#                            (str_detect(Merkmal_VirusVariante_Mutation, "617") == FALSE &
#                               (str_detect(Merkmal_Details_VirusVariante, "617") == FALSE)))),
#                    select = c(ID, Vorgangsnummer, Meldedatum, Bemerkung, Merkmal_VirusVariante_Mutation, Merkmal_Details_VirusVariante))

#octo_impl$Meldedatum <- format(octo_impl$Meldedatum, "%d.%m.%Y")

#write.xlsx(octo_impl, 
#           paste0(path_tagesberichte,
#                  "docs/VOC_implausible.xlsx"))
