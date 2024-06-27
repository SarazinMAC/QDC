###################################################
##### Produce Communities and Community stats #####
###################################################

# Processing --------

source(paste0(Data_path, "stats_and_communities_helper.R"))

dyn_net <- QDC_dyn

attr_dyn_df <- QDC_vs[,5:ncol(QDC_vs)]

# Actor names
for (col in colnames(attr_dyn_df)) {
  dyn_net %v% col <- attr_dyn_df[[col]]
}

## Set weights and tie colour as dynamic edge attributes

# Fix Qual_col for pre-QdC edges

QDC_es$Qual_col_inc_pre_qdc <- QDC_es$Qual_col
QDC_es$Qual_col_inc_pre_qdc[QDC_es$Qual_col_inc_pre_qdc=="grey90"] <- c("red", "red", "grey61", "chartreuse3", "chartreuse3", 
                                                                        "orange", "grey61", "grey61", "grey15")[QDC_es$Quality[QDC_es$Qual_col_inc_pre_qdc=="grey90"]]

for(row in 1:nrow(QDC_es)){
  edge_id <- get.edgeIDs(dyn_net,v=QDC_es$Actor_code[row],
                         alter=QDC_es$Tie_code[row])
  activate.edge.attribute(dyn_net,'edge_weights',QDC_es$edge_weights[row],
                          onset=QDC_es$onset[row],terminus=QDC_es$terminus[row],e=edge_id)
  activate.edge.attribute(dyn_net,'Qual_col',QDC_es$Qual_col_inc_pre_qdc[row],
                          onset=QDC_es$onset[row],terminus=QDC_es$terminus[row],e=edge_id)
}

# Set number of multiple ties sent/received by vertices as vertex attribute

for(row in 1:nrow(n_multiple_ties_df)){
  activate.vertex.attribute(x = dyn_net, prefix = 'node_edge_weights',
                            value = (n_multiple_ties_df$node_edge_weights[row]),
                            onset=n_multiple_ties_df$onset[row],terminus=n_multiple_ties_df$terminus[row],
                            v=n_multiple_ties_df$Actor_code[row])
}


slices <- seq(from = start_slice, to = end_slice, by = slice_interval)

slices <- c(17634, 17635)

all_community_sizes <- list()

all_community_stats_combined <- list()
all_communities_combined <- list()
all_memberships_combined <- list()

number_of_iterations <- 1000

for (i in 1:number_of_iterations) {
  seed <- set.seed(i)
  all_communities <- list()
  all_memberships <- list()
  all_community_stats <- list()
  for (.slice in slices){
    net_slice <- network.collapse(dyn_net, at = .slice, rule = "any", active.default = FALSE, retain.all.vertices = FALSE)
    net_slice <- asIgraph(net_slice)
    V(net_slice)$name <- vertex_attr(net_slice, "Actor_pers")
    E(net_slice)$weight <- edge_attr(net_slice, "edge_weights")
    # Remove loops because they influence community detection algorithm
    net_slice <- simplify(net_slice, remove.loops = TRUE)
    net_slice <- as.undirected(net_slice, mode = "collapse")
    if (cd_algorithm == "Louvain") {
      communities <- cluster_louvain(net_slice) 
    } else if (cd_algorithm == "Leiden") {
      communities <- cluster_leiden(net_slice, objective_function = "modularity", n_iterations = 20)
    } else {
      stop(paste0(cd_algorithm, " is an invalid community detection algorithm! Valid values are 'Louvain' and 'Leiden'"))
    }
    # Check whether Louvain produces badly connected communities (by running Leiden as iteration after Louvain)
    #  membership <- membership(communities)
    #  communities <- cluster_leiden(net_slice, objective_function = "modularity", n_iterations = 10, initial_membership = membership)
    all_communities[[as.character(.slice)]] <- communities
    all_memberships[[as.character(.slice)]] <- membership(communities)
    community_sizes <- sizes(communities)
    all_community_sizes[[as.character(.slice)]] <- community_sizes
    if (cd_algorithm == "Louvain") {
      community_stats <- c("slice" = .slice,
                           "N_communities" = length(communities),
                           "net_modularity" = modularity(communities),
                           "min_community_size" = min(community_sizes), 
                           "max_community_size" = max(community_sizes),
                           "mean_community_size" = mean(community_sizes),
                           "sd_community_size" = sd(community_sizes)
      )
      all_community_stats[[as.character(.slice)]] <- community_stats
    } else if (cd_algorithm == "Leiden") {
      community_stats <- c("slice" = .slice,
                           "N_communities" = length(communities),
                           "net_modularity" = communities$quality,
                           "min_community_size" = min(community_sizes), 
                           "max_community_size" = max(community_sizes),
                           "mean_community_size" = mean(community_sizes),
                           "sd_community_size" = sd(community_sizes)
      )
      all_community_stats[[as.character(.slice)]] <- community_stats
    }
  }
  all_community_stats <- rbindlist(lapply(all_community_stats, as.data.frame.list))
  
  # Returned outputs
  all_community_stats_combined[[as.character(i)]] <- all_community_stats
  all_communities_combined[[as.character(i)]] <- all_communities
  all_memberships_combined[[as.character(i)]] <- all_memberships
  
  if (i %% 10 == 0) {
    print(paste0("Done with iteration ", i))
  }
}



if (produce_modularity_stats == TRUE) {

  # Combine community statistics into one dataframe
  all_community_stats_combined_df <- abind::abind(all_community_stats_combined, along = 3)
  all_community_stats_combined_df <- as.data.frame(apply(all_community_stats_combined_df, c(1,2), mean))
  
  # Export dataframe
  rio::export(all_community_stats_combined_df, paste0(export_path, "all_community_stats_no_loops.xlsx"), rowNames = FALSE)
  
  # Run produce_modularity_stats.R
  source(paste0(Data_path, "produce_modularity_stats.R"))
}

if (produce_community_visuals == TRUE) {
  
  # Run produce_modularity_stats.R
  source(paste0(Data_path, "produce_community_visuals.R"))
}
