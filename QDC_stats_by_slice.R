########################################
##### QDC - Descriptive Statistics #####
########################################

library(tsna)
library(writexl)

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
QDC_text_dyn_onset_62 <- networkDynamic(vertex.spells = QDC_vs_onset_62[,1:5], edge.spells = QDC_es[,1:4], create.TEAs = FALSE)

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



# Calculate statistics

tErgmStats(QDC_text_dyn, formula = "~ edges + density + transitive + ttriple", start = start_slice, end = end_slice, time.interval = slice_interval)


transitivity <- tSnaStats(QDC_text_dyn, snafun = "gtrans", start = start_slice, end = end_slice, time.interval = slice_interval)
mutuality <- tSnaStats(QDC_text_dyn, snafun = "mutuality", start = start_slice, end = end_slice, time.interval = slice_interval)
centralization_degree <- tSnaStats(QDC_text_dyn, snafun = "centralization", start = start_slice, end = end_slice, time.interval = slice_interval, FUN = "degree", cmode = "freeman")
centralization_indegree <- tSnaStats(QDC_text_dyn, snafun = "centralization", start = start_slice, end = end_slice, time.interval = slice_interval, FUN = "degree", cmode = "indegree")
centralization_outdegree <- tSnaStats(QDC_text_dyn, snafun = "centralization", start = start_slice, end = end_slice, time.interval = slice_interval, FUN = "degree", cmode = "outdegree")

components <- tSnaStats(QDC_text_dyn, snafun = "components", start = start_slice, end = end_slice, time.interval = slice_interval, connected = "weak")

degree <- tSnaStats(QDC_text_dyn_onset_62, snafun = "degree", start = start_slice, end = end_slice, time.interval = slice_interval)
indegree <- tSnaStats(QDC_text_dyn_onset_62, snafun = "degree", start = start_slice, end = end_slice, time.interval = slice_interval, cmode="indegree")
outdegree <- tSnaStats(QDC_text_dyn_onset_62, snafun = "degree", start = start_slice, end = end_slice, time.interval = slice_interval, cmode="outdegree")
#directed_closeness <- tSnaStats(QDC_text_dyn_onset_62, snafun = "closeness", start = start_slice, end = end_slice, time.interval = slice_interval, cmode = "suminvdir")
undirected_closeness <- tSnaStats(QDC_text_dyn_onset_62, snafun = "closeness", start = start_slice, end = end_slice, time.interval = slice_interval, cmode = "suminvundir", rescale = TRUE)

# calculated directed closeness, but with ties inversed: Do actors receive ties direct or more distantly?
directed_closeness_inversed <- tSnaStats(QDC_text_dyn_onset_62_inversed, snafun = "closeness", start = start_slice, end = end_slice, time.interval = slice_interval, cmode = "suminvdir", rescale = TRUE)


eigenvector <- tSnaStats(QDC_text_dyn_onset_62_undirected, snafun = "evcent", start = start_slice, end = end_slice, time.interval = slice_interval, gmode = "graph")

## Create export of statistics

stats_to_export <- c("degree", "indegree", "outdegree", "undirected_closeness", "directed_closeness_inversed")
# Set row names for exported statistics, calling them "slice"

row_names <- seq(from = start_slice, to = end_slice, by = slice_interval)

stats_list <- list()
for (stat in stats_to_export) {
  stats_list[[stat]] <- cbind("slice" = row_names, as.data.frame(get(stat)))
}

date <- format(Sys.Date(), "%Y_%m_%d")

write_xlsx(stats_list, path = paste0(export_path,"Actor_stats_by_slice_", date, ".xlsx"))
