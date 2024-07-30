###########################
##### Ad-hoc analyses #####
###########################
  
if(text_or_pers=="pers") {
  # Produce the dynamic person network
  source(paste0(Data_path, "produce_dynamic_pers_network.R"))
  
} else if (text_or_pers=="text") {
  
  # Produce the dynamic text network
  source(paste0(Data_path, "produce_dynamic_text_network.R"))
}

# Produce results for Table 1

table_1_results <- table(QDC_es$Quality)

# Produce results about texts published in 1762 and 1763 and their references

texts_62_63 <- QDC_es[which(QDC_es$slice>17619 & QDC_es$slice<17640),]

