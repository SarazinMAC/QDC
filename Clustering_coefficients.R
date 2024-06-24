
# Calculate clustering coefficient at a particular point in time

calculate_clustering_coefficient <- function(dyn_net = QDC_dyn, .start_slice = start_slice, .end_slice = end_slice,
                                             .slice_interval = slice_interval, attr_dyn_df, .text_or_pers = text_or_pers) {
  
  # Set constants
  # Number of slices
  slices <- seq(from = .start_slice, to = .end_slice, by = .slice_interval)
  
  # Actor names
  for (col in colnames(attr_dyn_df)) {
    dyn_net %v% col <- attr_dyn_df[[col]]
  }
  
  if (text_or_pers == "text") {
    actor_name <- dyn_net %v% "Text_Name"
  } else if (text_or_pers == "pers") {
    actor_name <- dyn_net %v% "Pers_Name"
  }
  
  # Create initial df
  CC_stats <- as.data.frame(actor_name)
  
  for (.slice in slices) {
    net_slice <- network.collapse(dyn_net, at = .slice, rule = "any", active.default = FALSE, retain.all.vertices = TRUE)
    net_slice <- asIgraph(net_slice)
    CC <- transitivity(graph = net_slice, type = "localundirected")
    CC_stats <- cbind(CC_stats, .slice = CC)
  }
  colnames(CC_stats)[2:ncol(CC_stats)] <- slices
  return(CC_stats)
}


rio::export(CC_stats, file = paste0(export_path,"Clustering_coefficient_by_", slice_or_year, "_", date, "_", text_or_pers,"_net.xlsx"))
