
# Calculate Bonacich alpha Centrality

alphacent_stats <- data.frame(dyn_net %v% "Actor_pers")

for (.slice in as.character(slices)) {
  net_slice <- as_igraph_net_slice(dynamic_net = dyn_net, v_name_attr = "Actor_pers", slice = as.numeric(.slice), retain_vertices = TRUE)
  #net_slice <- simplify(net_slice, remove.multiple = TRUE, remove.loops = TRUE)
  #net_slice <- delete.vertices(net_slice, degree(net_slice)==0)
  E(net_slice)$weight <- edge_attr(net_slice, "edge_weights")
  net_slice <- delete_edges(net_slice, which(which_loop(net_slice)))
  
  alphacent_stats[[.slice]] <- alpha_centrality(net_slice, alpha = 0.4)
} 
colnames(alphacent_stats)[1] <- "actor_name"

rio::export(alphacent_stats, file = paste0(export_path,"alpha_centrality_by_", slice_or_year, "_", date, "_", text_or_pers,"_net.csv"))


