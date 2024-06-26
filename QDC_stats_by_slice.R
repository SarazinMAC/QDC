########################################
##### QDC - Descriptive Statistics #####
########################################

library(igraph)
library(tsna)
library(writexl)
library(tergm)
library(intergraph)
library(dplyr)
library(Cairo)
library(data.table)
library(ggplot2)
library(extrafont)

options(scipen = 999)

## set configurable values

export_path <- Data_path

date <- format(Sys.Date(), "%Y_%m_%d")

# TODO: Make the below work with both the text and person network (note last column "Actor_pers")
#QDC_vs_colnames_to_keep <- c("onset", "terminus", "Actor_code", "Actor_pers")

# Custom functions ----------

# Create object to add on multiple edges

add_multiple_edges_active <- function(netdyn, edge_spells = QDC_es, tail_col = "Actor_code", head_col = "Tie_code") {
  multiple_edges <- edge_spells[,c(tail_col, head_col, "onset", "terminus")]
  multiple_edges <- multiple_edges[duplicated(multiple_edges[c(tail_col, head_col)]),]
  networkDynamic::add.edges.active(netdyn, tail = multiple_edges[[tail_col]], head = multiple_edges[[head_col]], onset = multiple_edges$onset, terminus = multiple_edges$terminus)
  return(netdyn)
}

# Return first three largest receivers of ties in overall network

find_Nth_largest <- function(x, N) {
  # First find column indices of columns that have the Nth largest values in each row
  indices_list <- apply(x, 1, simplify = FALSE, function(y) {
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

# create an igraph network slice

as_igraph_net_slice <- function(dynamic_net, v_name_attr, slice, retain_vertices) {
  net_slice <- network.collapse(dynamic_net, at = slice, rule = "any", active.default = FALSE, retain.all.vertices = retain_vertices)
  net_slice <- asIgraph(net_slice)
  V(net_slice)$name <- vertex_attr(net_slice, v_name_attr)
  return(net_slice)
}


# Processing -------------


# assign correct onsets to pre_QDC edges
QDC_es <- assign_pre_QDC_edge_onsets(edge_spells = QDC_es)

# recreate QDC_dyn object

QDC_vs <- QDC_vs[order(QDC_vs$Actor_code),]

QDC_dyn <- networkDynamic(vertex.spells = QDC_vs[,1:4], edge.spells = QDC_es[,1:4], create.TEAs = TRUE)


# Extract community structure / modularity score of a network at a time point

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


#year <- 1789

#slices <- c(year*10, year*10+3, year*10+6, year*10+8)

slices <- seq(from = start_slice, to = end_slice, by = slice_interval)

slices <- c(17634, 17635)
all_community_sizes <- list()

cd_algorithm <- "Louvain"

all_community_stats_combined <- list()
all_communities_combined <- list()
all_memberships_combined <- list()

number_of_iterations <- 1000

for (i in 1:number_of_iterations) {
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
all_community_stats_combined[[as.character(i)]] <- all_community_stats
all_communities_combined[[as.character(i)]] <- all_communities
all_memberships_combined[[as.character(i)]] <- all_memberships

if (i %% 10 == 0) {
  print(paste0("Done with iteration ", i))
}
}




all_community_stats_combined_df <- abind::abind(all_community_stats_combined, along = 3)
all_community_stats_combined_df <- as.data.frame(apply(all_community_stats_combined_df, c(1,2), mean))

# Export dataframe
rio::export(all_community_stats_combined_df, paste0(export_path, "all_community_stats_no_loops.xlsx"), rowNames = FALSE)







# Combine community structures into one adjacency matrix per slice

slice_to_extract <- "17635"

slice_memberships_combined <- lapply(all_memberships_combined, function(second_order_list) { second_order_list[[slice_to_extract]]})
# Record the number of times that particular membership results have occurred, and export the visual 

# Flatten the list of lists into a single list of strings
membership_strings <- unlist(lapply(slice_memberships_combined, function(x) toString(unlist(x))))

# Count the number of times each unique string appears
membership_counts <- table(membership_strings)
membership_counts
membership_probs <- prop.table(membership_counts)

# Now 'membership_counts' is a table where the names are the unique membership structures (as strings), and the values are the number of times each structure appears

# Extract the results of the membership structure of the network, in order of likelihood

membership_by_count <- names(sort(membership_counts, decreasing = T))

#for each result, identify one element in communities that has this membership structure, and export it

for (membership in membership_by_count) {
  membership_prob <- membership_probs[which(names(membership_probs)==membership)]
  
  community <- which(membership_strings==membership)[1]
  communities_to_plot <- all_communities_combined[[community]][[slice_to_extract]]
  # set consistent colours for communities, based on presence of key actors ("community_leaders")
  # NOTE: This works in for slices 17634 qnd 17635, when Rivard community merges into Parlement de Paris - unsure if it works in other situations
#  community_leaders <- c("D'Alembert", "La Chalotais", "Rivard", "Parlement de Paris", "Rousseau", "Helv�tius", "Pellicier", "Pluche", "Louis XV", "Daragon")
  community_leaders <- c("Louis XV", "La Chalotais", "Rivard", "Parlement de Paris", "Rousseau", "Pluche", "Pellicier", "Helv�tius", "D'Alembert", "Daragon")
#  community_leader_colours_comm <- rainbow(10, alpha = 0.3)[seq_along(community_leaders)]
  community_leader_colours_comm <- pal(10, alpha = 0.5)[seq_along(community_leaders)]
#  community_leader_colours_comm <- c25_comm[seq_along(community_leaders)]
#  community_leader_colours_comm <- terrain.colors(10, alpha = 0.3)[seq_along(community_leaders)]
#  community_leader_colours_comm <-  colorspace::rainbow_hcl(10, alpha = 0.3)[seq_along(community_leaders)]
  
#  community_leader_colours_borders <- rainbow(10, alpha = 1)[seq_along(community_leaders)]
  community_leader_colours_borders <- pal(10, alpha = 1)[seq_along(community_leaders)]
#  community_leader_colours_borders <- c25[seq_along(community_leaders)]
#  community_leader_colours_borders <- terrain.colors(10, alpha = 1)[seq_along(community_leaders)]
#  community_leader_colours_borders <- colorspace::rainbow_hcl(10, alpha = 1)[seq_along(community_leaders)]
  
  names(community_leader_colours_comm) <- community_leaders
  names(community_leader_colours_borders) <- community_leaders
  
  communities_to_plot_members <- membership(communities_to_plot)
  community_leaders_communities <- communities_to_plot_members[community_leaders]
  colours <- rep(categorical_pal(8), 2)[seq_along(community_leaders)]
  colours_node <- rep(NA, length(communities_to_plot_members))
  colours_comm <- rep(NA, length(unique(community_leaders_communities)))
  names(colours_comm) <- names(sort(community_leaders_communities[!duplicated(community_leaders_communities, fromLast = TRUE)]))
  colours_borders <- rep(NA, length(unique(community_leaders_communities)))
  names(colours_borders) <- names(sort(community_leaders_communities[!duplicated(community_leaders_communities, fromLast = TRUE)]))
  
#  colours_borders <- rep(NA, length(community_leaders_communities))
  for (i in seq_along(community_leaders)) {
    colours_node <- ifelse(communities_to_plot_members==community_leaders_communities[i], colours[i], colours_node)
    colours_comm[names(colours_comm)==community_leaders[i]] <- community_leader_colours_comm[i]
    colours_borders[names(colours_borders)==community_leaders[i]] <- community_leader_colours_comm[i]
  }

  # Create title for visualisation
  plot_title <- paste0(cd_algorithm, " community visualisation (", 
                  substr(slice_to_extract, start = 1, stop = 4), 
                  ", slice ", substr(slice_to_extract, start = 5, stop = 5),
                  ") - ", membership_prob*100, "% of ", number_of_iterations, " runs")

  # Plot visual
  slice_to_plot <- network.collapse(dyn_net, at = as.numeric(slice_to_extract), rule = "any", active.default = FALSE, retain.all.vertices = FALSE)
  slice_to_plot <- asIgraph(slice_to_plot)
  V(slice_to_plot)$name <- vertex_attr(slice_to_plot, "Actor_pers")
  E(slice_to_plot)$weight <- edge_attr(slice_to_plot, "edge_weights")
  slice_to_plot <- delete_edges(slice_to_plot, which(which_loop(slice_to_plot)))
  V(slice_to_plot)$node_size <- node_size(slice = slice_to_plot, network_or_igraph = "igraph")*12
#  slice_to_plot <- as.undirected(slice_to_plot, mode = "collapse")
  
  plot(x = communities_to_plot, y = slice_to_plot,
       vertex.label = V(slice_to_plot)$Actor_pers,
       vertex.size = V(slice_to_plot)$node_size,
       edge.arrow.size = 0.125,
       edge.arrow.width = 3,
       edge.lty = c("solid", "dashed")[igraph::crossing(communities_to_plot, slice_to_plot) + 1],
       edge.width = E(slice_to_plot)$weight*2.5,
       edge.color = E(slice_to_plot)$Qual_col,
       col = colours_node,
       # Actual need to set community colours
       mark.col = colours_comm,
       mark.border = colours_borders
       )
  title(plot_title, cex.main = 3)
  vis <- recordPlot()


#  Cairo(file = paste0(export_path, "Communities_", slice_to_extract, "_likelihood_", membership_prob, "_", cd_algorithm, "_no_loops_test_colours.png"), width = 2400, height = 1800, type = "png", bg = "white")
#  tiff(filename = paste0(export_path, "Communities_", slice_to_extract, "_likelihood_", membership_prob, "_", cd_algorithm, "_no_loops_test.tiff"),
#       width = 4800, height = 3600, type = "cairo", bg = "white", family = "Calibri", symbolfamily = "Calibri", res = 200)
  jpeg(filename = paste0(export_path, "Communities_", slice_to_extract, "_likelihood_", membership_prob, "_", cd_algorithm, "_no_loops_test_colours_node_size.jpeg"),
       width = 4800, height = 3600, type = "cairo", bg = "white", family = "Calibri", symbolfamily = "Calibri", res = 200)
  print(vis)
  dev.off()
}



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
  QDC_attr_dyn <- QDC_text_attr[order(QDC_text_attr$Text_Name),]
} else if (text_or_pers == "pers") {
  QDC_attr_dyn <- QDC_pers_attr[order(QDC_pers_attr$Pers_Name),]
}

for (col in colnames(QDC_attr_dyn)) {
  QDC_dyn_onset_62 %v% col <- QDC_attr_dyn[[col]]
}


## Because base_net argument removed, set vertex names manually

if (text_or_pers == "text") {
  vertex_names <- QDC_vs[, c("Actor_code", "Actor_text")]
  vertex_names <- vertex_names[order(vertex_names$Actor_code),]
  vertex_names <- vertex_names$Actor_text
} else if (text_or_pers == "pers") {
  vertex_names <- QDC_vs[, c("Actor_code", "Actor_pers")]
  vertex_names <- vertex_names[order(vertex_names$Actor_code),]
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

export(stats_list_transposed, file = paste0(export_path,"stats_by_", slice_or_year, "_", date, "_", text_or_pers,"_net_transposed.xlsx"))



## Export overall degree/outdegree centrality results

measure <- "outdegree"

degree_data <- get(measure)
degree_data <- degree_data[nrow(degree_data),]

degree_data <- as.data.frame(degree_data)

# Note: no longer needed as node labels will be created elsewhere
# create actor labels to display on the visualisation
#if (text_or_pers=="pers") {
#  if (measure=="degree") {
#    actors_to_label <- c("Rousseau", "Mercure", "Ann�e litt�raire", "Rolland d'Erceville")
#    actor_labels <- c("Rousseau (16.7%)", " Mercure (8.6%)", " Ann�e\nlitt�raire (11.4%)", "Rolland\nd'Erceville (9.6%)")
#    label_y_position <- c(2, 2, 4, 5.5)
#    label_hjust <- c(0.7, 0.5, 0.5, 0.5)
#  } else if (measure=="outdegree") {
#    actors_to_label <- c("Rousseau", "Mercure", "Ann�e litt�raire", "Rolland d'Erceville", "Borrelly", "Rivard")
#    actor_labels <- c("Rousseau (0%)", "Mercure (8.4%)", "Ann�e\nlitt�raire (10.6%)", "Rolland\nd'Erceville (8.9%)", "Borrelly (5.3%)", "Rivard (7.6%)")
#    label_y_position <- c(2.5, 4, 6, 7.5, 2, 2)
#    label_hjust <- c(0.4, 0.5, 0.5, 0.5, 0.5, 0.5)
#  }
#} else if (text_or_pers=="text") {
#  if (measure=="degree") {
#    actors_to_label <- c("Rousseau (1762)", "D'Alembert (1753)", "Borrelly (1768)", "La Chalotais (1763)")
#    actor_labels <- c("Rousseau\n(1762) - 16.6%", "D'Alembert\n(1753) - 6.2%", " Borrelly\n(1768) - 5.5%", "La Chalotais\n(1763) - 5.5%")
#    label_y_position <- c(5, 5, 11, 11)
#    label_hjust <- c(0.7, 0.1, -0.05, 1.05)
#  } else if (measure=="outdegree") {
#    actors_to_label <- c("Rousseau (1762)", "Borrelly (1768)", "Rolland d'Erceville (1783)", "Rolland d'Erceville (1769)")
#    actor_labels <- c("Rousseau\n(1762) - 0%", "Borrelly\n(1768) - 5.2%", "Rolland d'Erceville\n(1783) - 4.9%", "Rolland d'Erceville\n(1769) - 3.6%")
#    label_y_position <- c(5, 5, 12.5, 5)
#    label_hjust <- c(0.4, 0.5, 0.5, 0.5)
#  }
#}
#
#degree_data$labels <- NA
#degree_data$label_y_position <- NA
#
#for (i in seq_along(actors_to_label)) {
#  degree_data[actors_to_label[i],"labels"] <- paste0(actor_labels[i])
#  degree_data[actors_to_label[i],"label_y_position"] <- label_y_position[i]
#  degree_data[actors_to_label[i],"label_hjust"] <- label_hjust[i]
#}
#
#degree_data <- degree_data[order(degree_data$degree_data, decreasing = TRUE),]

if (measure=="degree") {
  
ggplot(data = degree_data,
       aes(x = degree_data)) +
  geom_histogram(binwidth = 1, fill = "#FF776C", col = "black") +
#  geom_label(data = degree_data[!is.na(degree_data$labels),], mapping = aes(x = degree_data, y = label_y_position, label = labels, hjust = label_hjust), size = 15) +
  labs(x = "Degree centrality", y = "Number of nodes") +
  theme(axis.title = element_text(size = 50), axis.text = element_text(size = 50)) +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60)) +
  theme(plot.margin = unit(c(15, 15, 15, 15), units = "pt")) +
  theme(text = element_text(family = "Calibri"))
  
} else if (measure=="outdegree") {
  ggplot(data = degree_data,
         aes(x = degree_data)) +
    geom_histogram(binwidth = 1, fill = "#FF776C", col = "black") +
#    geom_label(data = degree_data[!is.na(degree_data$labels),], mapping = aes(x = degree_data, y = label_y_position , label = labels, hjust = label_hjust), size = 15) +
    labs(x = "Degree centrality", y = "Number of nodes") +
    theme(axis.title = element_text(size = 50), axis.text = element_text(size = 50)) +
    scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60)) +
    theme(plot.margin = unit(c(15, 15, 15, 15), units = "pt")) +
    theme(text = element_text(family = "Calibri"))
}

# Export vis

vis <- recordPlot()
#Cairo(file = paste0(export_path, measure, "_centrality_", text_or_pers, "_network_node_no_labels.png"), width = 2400, height = 1800, type = "png", bg = "white")
jpeg(filename = paste0(export_path, measure, "_centrality_", text_or_pers, "_network_node_no_labels.png"),
     width = 4800, height = 3600, type = "cairo", bg = "white", family = "Calibri", symbolfamily = "Calibri", res = 200)
print(vis)
dev.off()

