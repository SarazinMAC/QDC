

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



# Re-create static vertex attributes

if (text_or_pers == "text") {
  QDC_attr_dyn <- create_static_vertex_attr_df(vs_df = QDC_vs, 
                                                    node_attr_df = QDC_text_nodes,
                                                    actor_colname = "Actor_text",
                                                    Text_or_pers_name = "Text_Name")
} else if (text_or_pers == "pers") {
  QDC_attr_dyn <- create_static_vertex_attr_df(vs_df = QDC_vs, 
                                                    node_attr_df = QDC_pers_nodes,
                                                    Text_or_pers_name = "Pers_Name",
                                                    actor_colname = "Actor_pers")
}

# Run function

calculate_clustering_coefficient(attr_dyn_df = QDC_attr_dyn)


