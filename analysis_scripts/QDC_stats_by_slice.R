########################################
##### QDC - Descriptive Statistics #####
########################################

source(paste0(Data_path, "analysis_scripts\\stats_and_communities_helper.R"))

### tSnaStats doesn't work with changing vertex activity - so recreate network with all nodes present in 17620

QDC_vs_onset_62 <- QDC_vs
QDC_vs_onset_62$onset <- start_slice

## Remove the base_net argument below as otherwise the tSnaStats function ignores edge spells 
QDC_dyn_onset_62 <- networkDynamic(vertex.spells = QDC_vs_onset_62[,1:4], edge.spells = QDC_es[,1:4], create.TEAs = TRUE)

# Add multiple edges - note, should only be relevant with person network; might even return an error with text net
if (text_or_pers == "pers") {
  QDC_dyn_onset_62 <- add_multiple_edges_active(netdyn = QDC_dyn_onset_62)
}

# vertex attributes

if (text_or_pers == "text") {
  QDC_attr_dyn <- QDC_text_attr[base::order(QDC_text_attr$Text_Name),]
} else if (text_or_pers == "pers") {
  QDC_attr_dyn <- QDC_pers_attr[base::order(QDC_pers_attr$Pers_Name),]
}

for (col in colnames(QDC_attr_dyn)) {
  QDC_dyn_onset_62 %v% col <- QDC_attr_dyn[[col]]
}


## Because base_net argument removed, set vertex names manually

if (text_or_pers == "text") {
  vertex_names <- QDC_vs[, c("Actor_code", "Actor_text")]
  vertex_names <- vertex_names[base::order(vertex_names$Actor_code),]
  vertex_names <- vertex_names$Actor_text
} else if (text_or_pers == "pers") {
  vertex_names <- QDC_vs[, c("Actor_code", "Actor_pers")]
  vertex_names <- vertex_names[base::order(vertex_names$Actor_code),]
  vertex_names <- vertex_names$Actor_pers
}

QDC_dyn_onset_62 %v% "vertex.names" <- vertex_names

# Create dynamic network object, but with tie direction inversed - to calculate directed closeness centrality

QDC_es_inversed <- QDC_es
QDC_es_inversed <- QDC_es_inversed[,c("onset", "terminus", "Tie_code", "Actor_code")]
colnames(QDC_es_inversed) <- c("onset", "terminus", "Actor_code", "Tie_code")
all(QDC_es_inversed$Actor_code==QDC_es$Tie_code); all(QDC_es_inversed$Tie_code==QDC_es$Actor_code)

QDC_dyn_onset_62_inversed <- networkDynamic(vertex.spells = QDC_vs_onset_62[,1:4], edge.spells = QDC_es_inversed[,1:4], create.TEAs = FALSE)
QDC_dyn_onset_62_inversed %v% "vertex.names" <- vertex_names

# Add multiple edges - note, should only be relevant with person network; might even return an error with text net
if (text_or_pers == "pers") {
  QDC_dyn_onset_62_inversed <- add_multiple_edges_active(
    netdyn = QDC_dyn_onset_62_inversed, edge_spells = QDC_es_inversed)
}

# Create undirected version of network

QDC_es_undirected <- rbind(QDC_es[,1:4], QDC_es_inversed)

QDC_dyn_onset_62_undirected <- networkDynamic(vertex.spells = QDC_vs_onset_62[,1:4], edge.spells = QDC_es_undirected[,1:4], create.TEAs = TRUE)
QDC_dyn_onset_62_undirected %v% "vertex.names" <- vertex_names

# Create dynamic net of negative and ambivalent ties

QDC_es_neg <- QDC_es[QDC_es$Quality %in% c(1, 2, 6),]
QDC_dyn_neg <- networkDynamic(vertex.spells = QDC_vs_onset_62[,1:4], edge.spells = QDC_es_neg[,1:4], create.TEAs = TRUE)
QDC_dyn_neg %v% "vertex.names" <- vertex_names

# Add multiple edges - note, should only be relevant with person network; might even return an error with text net
if (text_or_pers == "pers") {
  QDC_dyn_neg <- add_multiple_edges_active(
    netdyn = QDC_dyn_neg, edge_spells = QDC_es_neg)
}



# Calculate statistics

# Note: tErgmStats function doesn't take into account multiple ties or loops
net_stats <- tErgmStats(QDC_dyn, formula = "~ edges + density + ttriple + mutual", start = start_slice, end = end_slice, time.interval = slice_interval)
net_stats <- as.data.frame(net_stats)

# the below works with valued edges
#summary_formula(QDC_dyn ~ edges + sum, at = 17620:17900, response = "edges_times_2")

net_stats_neg <- tErgmStats(QDC_dyn_neg, formula = "~ edges + density", start = start_slice, end = end_slice, time.interval = slice_interval) %>%
  as.data.frame()

colnames(net_stats_neg) <- paste0(colnames(net_stats_neg), "_negative")

transitivity <- tSnaStats(QDC_dyn, snafun = "gtrans", start = start_slice, end = end_slice, time.interval = slice_interval)
#mutuality <- tSnaStats(QDC_dyn, snafun = "mutuality", start = start_slice, end = end_slice, time.interval = slice_interval)
#centralization_degree <- tSnaStats(QDC_dyn, snafun = "centralization", start = start_slice, end = end_slice, time.interval = slice_interval, FUN = "degree", cmode = "freeman")
#centralization_indegree <- tSnaStats(QDC_dyn, snafun = "centralization", start = start_slice, end = end_slice, time.interval = slice_interval, FUN = "degree", cmode = "indegree")
#centralization_outdegree <- tSnaStats(QDC_dyn, snafun = "centralization", start = start_slice, end = end_slice, time.interval = slice_interval, FUN = "degree", cmode = "outdegree")

components_weak <- tSnaStats(QDC_dyn, snafun = "components", start = start_slice, end = end_slice, time.interval = slice_interval, connected = "weak")
components_unilateral <- tSnaStats(QDC_dyn, snafun = "components", start = start_slice, end = end_slice, time.interval = slice_interval, connected = "unilateral")

degree <- tSnaStats(QDC_dyn_onset_62, snafun = "degree", start = start_slice, end = end_slice, time.interval = slice_interval, diag=TRUE)
indegree <- tSnaStats(QDC_dyn_onset_62, snafun = "degree", start = start_slice, end = end_slice, time.interval = slice_interval, cmode="indegree", diag=TRUE)
outdegree <- tSnaStats(QDC_dyn_onset_62, snafun = "degree", start = start_slice, end = end_slice, time.interval = slice_interval, cmode="outdegree", diag=TRUE)

if (produce_centrality_histogram == TRUE) {
  source(paste0(Data_path, "analysis_scripts\\produce_centrality_histogram.R"))
} 

if (produce_statistics_by_slice_or_year == TRUE) {
  
degree_neg <- tSnaStats(QDC_dyn_neg, snafun = "degree", start = start_slice, end = end_slice, time.interval = slice_interval, diag=TRUE)
indegree_neg <- tSnaStats(QDC_dyn_neg, snafun = "degree", start = start_slice, end = end_slice, time.interval = slice_interval, cmode="indegree", diag=TRUE)
outdegree_neg <- tSnaStats(QDC_dyn_neg, snafun = "degree", start = start_slice, end = end_slice, time.interval = slice_interval, cmode="outdegree", diag=TRUE)

eigenvector_undirected <- tSnaStats(QDC_dyn_onset_62_undirected, snafun = "evcent", start = start_slice, end = end_slice, time.interval = slice_interval, maxiter=1e7, use.eigen = F)
eigenvector_inversed <- tSnaStats(QDC_dyn_onset_62_inversed, snafun = "evcent", start = start_slice, end = end_slice, time.interval = slice_interval, maxiter=1e7, use.eigen = F)

undirected_closeness <- tSnaStats(QDC_dyn_onset_62, snafun = "closeness", start = start_slice, end = end_slice, time.interval = slice_interval, cmode = "suminvundir", rescale = FALSE)

# calculated directed closeness, but with ties inversed: Do actors receive ties direct or more distantly?
directed_closeness_inversed <- tSnaStats(QDC_dyn_onset_62_inversed, snafun = "closeness", start = start_slice, end = end_slice, time.interval = slice_interval, cmode = "suminvdir", rescale = FALSE)

# Calculate the proportions of ties (both overall and negative only) sent to Rousseau
## with test: is Rousseau in the column names of object (i.e. were vertex names set correctly?)

check_if_node_is_present <- function(df, node_name) {
  if (!(any(grepl(node_name, colnames(df))))) {
    stop("Vertex names may not have been set correctly. Investigate!")
  }
}

check_if_node_is_present(df = indegree, node_name = "Rousseau")
check_if_node_is_present(df = indegree_neg, node_name = "Rousseau")

Rousseau_indegree <- indegree[, grepl("Rousseau", colnames(indegree))]
net_stats$prop_ties_sent_to_rousseau <- Rousseau_indegree/net_stats$edges 

Rousseau_indegree_neg <- indegree_neg[, grepl("Rousseau", colnames(indegree_neg))]
net_stats_neg$prop_neg_ties_sent_to_rousseau <- Rousseau_indegree_neg/net_stats$edges 


# Largest receivers/senders of ties

largest_receivers <- find_Nth_largest(x = indegree, N = 1)
second_largest_receivers <- find_Nth_largest(x = indegree, N = 2)
third_largest_receivers <- find_Nth_largest(x = indegree, N = 3)

largest_receivers_negative <- find_Nth_largest(x = indegree_neg, N = 1)
second_largest_receivers_negative <- find_Nth_largest(x = indegree_neg, N = 2)
third_largest_receivers_negative <- find_Nth_largest(x = indegree_neg, N = 3)
fourth_largest_receivers_negative <- find_Nth_largest(x = indegree_neg, N = 4)
fifth_largest_receivers_negative <- find_Nth_largest(x = indegree_neg, N = 5)

largest_senders_negative <- find_Nth_largest(x = outdegree_neg, N = 1)
second_largest_senders_negative <- find_Nth_largest(x = outdegree_neg, N = 2)
third_largest_senders_negative <- find_Nth_largest(x = outdegree_neg, N = 3)
fourth_largest_senders_negative <- find_Nth_largest(x = outdegree_neg, N = 4)
fifth_largest_senders_negative <- find_Nth_largest(x = outdegree_neg, N = 5)


for (col in c("transitivity", "largest_receivers", "second_largest_receivers", "third_largest_receivers")){
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

## Create export of statistics

stats_to_export <- c("net_stats", "degree", "indegree", "outdegree", "degree_neg", "indegree_neg", "outdegree_neg", "undirected_closeness", "directed_closeness_inversed", "eigenvector_undirected", "eigenvector_inversed")
# Set row names for exported statistics, calling them "slice"

row_names <- seq(from = start_slice, to = end_slice, by = slice_interval)

stats_list <- list()
for (stat in stats_to_export) {
  stat_df <- as.data.frame(get(stat))
  if (stat %in% c("undirected_closeness", "directed_closeness_inversed", "eigenvector_undirected", "eigenvector_inversed")) {
    stat_df <- sapply(stat_df, function(x) {round(x, digits = 3)})
    stat_df <- as.data.frame(stat_df)
  }
  stats_list[[stat]] <- cbind(row_names, stat_df)
  colnames(stats_list[[stat]])[1] <- slice_or_year
}

# Transpose most dfs to make output more readable

stats_list_transposed <- list()
stats_list_transposed[["net_stats"]] <- stats_list[["net_stats"]]

for (stat in stats_to_export[stats_to_export!="net_stats"]) {
  stat_df <- as.data.frame(get(stat))
  if (stat %in% c("undirected_closeness", "directed_closeness_inversed", "eigenvector_undirected", "eigenvector_inversed")) {
    stat_df <- sapply(stat_df, function(x) {round(x, digits = 3)})
    stat_df <- as.data.frame(stat_df)
  }
  stat_df <- cbind(row_names, stat_df)
  colnames(stat_df)[1] <- slice_or_year
  new_rownames <- colnames(stat_df)
  stats_transposed <- t(stat_df)
  colnames(stats_transposed) <- row_names
  rownames(stats_transposed) <- new_rownames
  stats_transposed <- stats_transposed[-1,]
  stats_transposed <- cbind(rownames(stats_transposed), as.data.frame(stats_transposed))
  colnames(stats_transposed)[1] <- paste0("Actor_", text_or_pers, "_name")
  stats_list_transposed[[stat]] <- stats_transposed
}

export(stats_list_transposed, file = paste0(export_path,"stats\\stats_by_", slice_or_year, "_", date, "_", text_or_pers,"_net_transposed.xlsx"))

}
