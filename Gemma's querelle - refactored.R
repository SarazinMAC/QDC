library(intergraph)
library(rio)
library(sna)
library(statnet.common)
library(network)
library(ndtv)
library(networkDynamic)
library(dplyr)

# Configurable values

#Data_path <- "C:\\Users\\sarazinm\\Documents\\Gen\\Gemma\\"
Data_name <- "QDC_2024_06_07.xlsx"
Data_path <- "D:\\Git_Repos\\QDC\\"

# Do you want to use slices (i.e. years * 10) or original years to create visualisations or statistics?

slice_or_year <- "slice" # write "slice" or "year" here

# Set start and end slice
# Normally, the start slice is 17619 for the "slice" option and 1761 for the "year" option
# While the end slice is 17899 for the "slice" option and 1789 for the "year" option

start_slice <- 17619
end_slice <- 17899
slice_interval <- 1 # this value ordinarily does not change.


# Do you want to produce outputs from the Text or Person networks?

text_or_pers <- "text" # write "text" or "pers" here

# Do you want to produce dynamic visualisations from this network?

produce_dynamic_visuals <- FALSE # will produce dynamic visuals if value is TRUE, otherwise not

# Do you want to produce slice-by-slice or year-by-year statistics from this network?

produce_statistics_by_slice_or_year <- FALSE # will produce statistics if value is TRUE, otherwise not

# Do you want to produce a histogram of centrality distributions from this network?

produce_centrality_histogram <- TRUE # will produce histogram if value is TRUE, otherwise not
histogram_measure <- "outdegree" # the centrality measure to use for the histogram. Valid values are "degree", "outdegree", and "indegree"








##### Processing #####

# import dataset

QDC_file <- import(paste0(Data_path, "original_qdc_datasets\\", Data_name))

# Import custom functions

source(paste0(Data_path, "Custom_functions_dyn_data_visuals.R"))


# Clean dataset of empty rows

empty_rows <- apply(QDC_file, 1, function(x) {all(is.na(x))}) # records TRUE if the row is full of NAs
QDC <- QDC_file[!empty_rows,] # Only keep rows that return FALSE to the line above
QDC$order <- 1:nrow(QDC)


##### Creating datasets #####

## Restricting dataset to 1762 - 1789
QDC_62_89 <- QDC[which(QDC$Date>1761),]




##### Run files #####

if (produce_dynamic_visuals==TRUE) {
  
  if(text_or_pers=="pers") {

    # Produce the dynamic person network
    source(paste0(Data_path, "produce_dynamic_pers_network.R"))
    
  } else if (text_or_pers=="text") {
    
    # Produce the dynamic text network
    source(paste0(Data_path, "produce_dynamic_text_network.R"))
  }
}

if (produce_centrality_histogram == TRUE) {
  source(paste0(Data_path, "QDC_stats_by_slice.R"))
}
















