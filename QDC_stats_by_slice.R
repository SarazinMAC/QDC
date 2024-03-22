########################################
##### QDC - Descriptive Statistics #####
########################################

library(tsna)
library(writexl)

options(scipen = 999)

## set configurable values

export_path <- "C:\\Users\\sarazinm\\Documents\\Gen\\Gemma\\"

# Start and end slices (periods), and slice intervals, on which to calculate network/node statistics

start_slice <- 17620
end_slice <- 17899
slice_interval <- 1
  
# Create dynamic network object
### tSnaStats doesn't seem to work with changing vertex activity - so recreate network with all nodes present in 17620

QDC_vs_onset_62 <- QDC_vs
QDC_vs_onset_62$onset <- 17620

## Remove the base_net argument below as otherwise the tSnaStats function ignores edge spells 
QDC_text_dyn_onset_62 <- networkDynamic(vertex.spells = QDC_vs_onset_62[,1:5], edge.spells = QDC_es, create.TEAs = TRUE)

# vertex attributes

for (col in colnames(QDC_text_attr_dyn)) {
  QDC_text_dyn_onset_62 %v% col <- QDC_text_attr_dyn[[col]]
}

# Create dynamic network object, but with tie direction inversed - to calculate directed closeness centrality

QDC_es_inversed <- QDC_es
QDC_es_inversed <- QDC_es_inversed[,c("onset", "terminus", "Tie_code", "Actor_code")]
colnames(QDC_es_inversed) <- c("onset", "terminus", "Actor_code", "Tie_code")
all(QDC_es_inversed$Actor_code==QDC_es$Tie_code); all(QDC_es_inversed$Tie_code==QDC_es$Actor_code)

QDC_text_dyn_onset_62_inversed <- networkDynamic(vertex.spells = QDC_vs_onset_62[,1:5], edge.spells = QDC_es_inversed[,1:4], create.TEAs = FALSE)


## Because base_net argument removed, set vertex names manually

vertex_names <- QDC_vs[, c("Actor_code", "Actor_text")]
vertex_names <- vertex_names[order(vertex_names$Actor_code),]
vertex_names <- vertex_names$Actor_text

QDC_text_dyn_onset_62 %v% "vertex.names" <- vertex_names
QDC_text_dyn_onset_62_inversed %v% "vertex.names" <- vertex_names

# Create dynamic net of negative and ambivalent ties

QDC_es_neg <- QDC_es[QDC_es$Quality %in% c(1, 2, 6),]

QDC_text_dyn_neg <- networkDynamic(vertex.spells = QDC_vs_onset_62[,1:5], edge.spells = QDC_es_neg, create.TEAs = TRUE)

QDC_text_dyn_neg %v% "vertex.names" <- vertex_names


# Calculate statistics

net_stats <- tErgmStats(QDC_text_dyn, formula = "~ edges + density + ttriple", start = start_slice, end = end_slice, time.interval = slice_interval)
net_stats <- as.data.frame(net_stats)

net_stats_neg <- tErgmStats(QDC_text_dyn_neg, formula = "~ edges + density", start = start_slice, end = end_slice, time.interval = slice_interval) %>%
  as.data.frame()

colnames(net_stats_neg) <- paste0(colnames(net_stats_neg), "_negative")

transitivity <- tSnaStats(QDC_text_dyn, snafun = "gtrans", start = start_slice, end = end_slice, time.interval = slice_interval)
mutuality <- tSnaStats(QDC_text_dyn, snafun = "mutuality", start = start_slice, end = end_slice, time.interval = slice_interval)
centralization_degree <- tSnaStats(QDC_text_dyn, snafun = "centralization", start = start_slice, end = end_slice, time.interval = slice_interval, FUN = "degree", cmode = "freeman")
centralization_indegree <- tSnaStats(QDC_text_dyn, snafun = "centralization", start = start_slice, end = end_slice, time.interval = slice_interval, FUN = "degree", cmode = "indegree")
centralization_outdegree <- tSnaStats(QDC_text_dyn, snafun = "centralization", start = start_slice, end = end_slice, time.interval = slice_interval, FUN = "degree", cmode = "outdegree")

components <- tSnaStats(QDC_text_dyn, snafun = "components", start = start_slice, end = end_slice, time.interval = slice_interval, connected = "weak")

degree <- tSnaStats(QDC_text_dyn_onset_62, snafun = "degree", start = start_slice, end = end_slice, time.interval = slice_interval)
indegree <- tSnaStats(QDC_text_dyn_onset_62, snafun = "degree", start = start_slice, end = end_slice, time.interval = slice_interval, cmode="indegree")
outdegree <- tSnaStats(QDC_text_dyn_onset_62, snafun = "degree", start = start_slice, end = end_slice, time.interval = slice_interval, cmode="outdegree")
degree_neg <- tSnaStats(QDC_text_dyn_neg, snafun = "degree", start = start_slice, end = end_slice, time.interval = slice_interval)
indegree_neg <- tSnaStats(QDC_text_dyn_neg, snafun = "degree", start = start_slice, end = end_slice, time.interval = slice_interval, cmode="indegree")
outdegree_neg <- tSnaStats(QDC_text_dyn_neg, snafun = "degree", start = start_slice, end = end_slice, time.interval = slice_interval, cmode="outdegree")

#directed_closeness <- tSnaStats(QDC_text_dyn_onset_62, snafun = "closeness", start = start_slice, end = end_slice, time.interval = slice_interval, cmode = "suminvdir")
undirected_closeness <- tSnaStats(QDC_text_dyn_onset_62, snafun = "closeness", start = start_slice, end = end_slice, time.interval = slice_interval, cmode = "suminvundir", rescale = TRUE)

# calculated directed closeness, but with ties inversed: Do actors receive ties direct or more distantly?
directed_closeness_inversed <- tSnaStats(QDC_text_dyn_onset_62_inversed, snafun = "closeness", start = start_slice, end = end_slice, time.interval = slice_interval, cmode = "suminvdir", rescale = TRUE)

# Calculate the proportions of ties (both overall and negative only) sent to Rousseau

Rousseau_indegree <- indegree[, grepl("Rousseau", colnames(indegree))]
net_stats$prop_sent_to_rousseau <- Rousseau_indegree/net_stats$edges 

Rousseau_indegree_neg <- indegree_neg[, grepl("Rousseau", colnames(indegree_neg))]
net_stats_neg$prop_sent_to_rousseau <- Rousseau_indegree_neg/net_stats$edges 

# Return first three largest receivers of ties in overall network

find_Nth_largest <- function(x, N) {
  # First find column indices of columns that have the Nth largest values in each row
  indices_list <- apply(x, 1, function(y) {
    # if there are fewer unique values (actors) than N, then just return NA
    if (length(unique(y))<N) {
      NA
    } else {
      n <- length(unique(y))
      which(y==sort(unique(y),partial=n-N+1)[n-N+1]) 
    }
  })
  # Get column names (i.e. actor names) corresponding to these column indices
  names_list <- list()
  for (i in 1:length(indices_list)) {
    names_list[[i]] <- paste(names(indices_list[[i]]), collapse = "; ")
  }
  
  names <- unlist(names_list)
  return(names)
}

largest_receivers <- find_Nth_largest(x = indegree, N = 1)
second_largest_receivers <- find_Nth_largest(x = indegree, N = 2)
third_largest_receivers <- find_Nth_largest(x = indegree, N = 3)

largest_receivers_negative <- find_Nth_largest(x = indegree_neg, N = 1)
second_largest_receivers_negative <- find_Nth_largest(x = indegree_neg, N = 2)
third_largest_receivers_negative <- find_Nth_largest(x = indegree_neg, N = 3)
fourth_largest_receivers_negative <- find_Nth_largest(x = indegree_neg, N = 3)
fifth_largest_receivers_negative <- find_Nth_largest(x = indegree_neg, N = 3)

largest_senders_negative <- find_Nth_largest(x = outdegree_neg, N = 1)
second_largest_senders_negative <- find_Nth_largest(x = outdegree_neg, N = 2)
third_largest_senders_negative <- find_Nth_largest(x = outdegree_neg, N = 3)
fourth_largest_senders_negative <- find_Nth_largest(x = outdegree_neg, N = 3)
fifth_largest_senders_negative <- find_Nth_largest(x = outdegree_neg, N = 3)


for (col in c("largest_receivers", "second_largest_receivers", "third_largest_receivers")){
  net_stats[[col]] <- get(col)
}

for (col in c("largest_receivers_negative", 
              "largest_senders_negative",
              "second_largest_receivers_negative",
              "second_largest_senders_negative",
              "third_largest_receivers_negative",
              "third_largest_senders_negative",
              "fourth_largest_receivers_negative",
              "fourth_largest_senders_negative",
              "fifth_largest_receivers_negative",
              "fifth_largest_senders_negative")) {
  net_stats_neg[[col]] <- get(col)
}



#Combine stats from overall and negative only networks

net_stats <- cbind(net_stats, net_stats_neg)

eigenvector <- tSnaStats(QDC_text_dyn_onset_62_undirected, snafun = "evcent", start = start_slice, end = end_slice, time.interval = slice_interval, gmode = "graph")

## Create export of statistics

stats_to_export <- c("net_stats", "degree", "indegree", "outdegree", "undirected_closeness", "directed_closeness_inversed")
# Set row names for exported statistics, calling them "slice"

row_names <- seq(from = start_slice, to = end_slice, by = slice_interval)

stats_list <- list()
for (stat in stats_to_export) {
  stats_list[[stat]] <- cbind("slice" = row_names, as.data.frame(get(stat)))
}

date <- format(Sys.Date(), "%Y_%m_%d")

write_xlsx(stats_list, path = paste0(export_path,"Actor_stats_by_slice_", date, "_text_net.xlsx"))

write.csv(net_stats, paste0(export_path, "network_stats_by_slice_", date, "_text_net.csv"))