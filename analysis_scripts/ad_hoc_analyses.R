###########################
##### Ad-hoc analyses #####
###########################
  
if(text_or_pers=="pers") {
  # Produce the dynamic person network
  source(paste0(Data_path, "analysis_scripts\\produce_dynamic_pers_network.R"))
  
} else if (text_or_pers=="text") {
  
  # Produce the dynamic text network
  source(paste0(Data_path, "analysis_scripts\\produce_dynamic_text_network.R"))
}

# Produce results for Table 1

table_1_results <- table(QDC_es$Quality)
table_1_results <- as.numeric(table_1_results)
names(table_1_results) <- c("An explicit negative reference (-2)",
                                                          "An implicit negative reference (-1)",
                                                          "A neutral reference (0)",
                                                          "An implicit positive reference (1)",
                                                          "An explicit positive reference (2)",
                                                          "An ambivalent reference (3)",
                                                          "A form of republication (4)",
                                                          "A sequel or supplement (5)",
                                                          "A response to a catalyst (6)")

# Produce results about texts published in 1762 and 1763 and their references

texts_62_63 <- QDC_es[which(QDC_es$slice>17619 & QDC_es$slice<17640),]

