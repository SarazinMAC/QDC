########################################
##### QDC - Descriptive Statistics #####
########################################

#intergraph_url <- "https://cran.r-project.org/src/contrib/Archive/intergraph/intergraph_2.0-2.tar.gz"
#install.packages(intergraph_url, repos=NULL, type="source")

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

## Create reference dfs

original_QDC_es <- QDC_es


## set configurable values

export_path <- Data_path

text_or_pers <- "pers"

date <- format(Sys.Date(), "%Y_%m_%d")

# TODO: Make the below work with both the text and person network (note last column "Actor_pers")
#QDC_vs_colnames_to_keep <- c("onset", "terminus", "Actor_code", "Actor_pers")

# Start and end slices (periods), and slice intervals, on which to calculate network/node statistics


start_slice <- 17620
end_slice <- 17899
slice_interval <- 1

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
QDC_es <- assign_pre_QDC_edge_onsets(edge_spells = original_QDC_es)

# Divide onsets and termini by 10
#TODO: Fix this bodge to make it more sustainable

#if (any(QDC_vs$onset > 10000)) {
#  QDC_vs$onset <- trunc(QDC_vs$onset/10); QDC_vs$terminus <- trunc(QDC_vs$terminus/10)
#  QDC_es$onset <- trunc(QDC_es$onset/10); QDC_es$terminus <- trunc(QDC_es$terminus/10)
#}

#QDC_vs <- QDC_vs[!duplicated(QDC_vs[,c("Actor_code")]),]
#QDC_es <- QDC_es[!duplicated(QDC_es[,c("Actor_code", "Tie_code")]),]

# recreate QDC_dyn object

QDC_vs <- QDC_vs[order(QDC_vs$Actor_code),]

QDC_dyn <- networkDynamic(vertex.spells = QDC_vs[,1:4], edge.spells = QDC_es[,1:4], create.TEAs = TRUE)





# static vertex attributes

if (text_or_pers == "text") {
  QDC_text_attr_dyn <- create_static_vertex_attr_df(vs_df = QDC_vs, 
                                                    node_attr_df = QDC_text_nodes,
                                                    actor_colname = "Actor_text",
                                                    Text_or_pers_name = "Text_Name")
} else if (text_or_pers == "pers") {
  QDC_pers_attr_dyn <- create_static_vertex_attr_df(vs_df = QDC_vs, 
                                                    node_attr_df = QDC_pers_nodes,
                                                    Text_or_pers_name = "Pers_Name",
                                                    actor_colname = "Actor_pers")
}


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


# Extract community structure / modularity score of a network at a time point

dyn_net = QDC_dyn

attr_dyn_df <- QDC_vs[,5:ncol(QDC_vs)]

par(mfrow=c(2,2), mar = c(3, 0, 3, 0))

par(mfrow=c(1,1), mar = c(3, 0, 3, 0))

# Actor names
for (col in colnames(attr_dyn_df)) {
  dyn_net %v% col <- attr_dyn_df[[col]]
}

## Set weights as dynamic edge attribute

for(row in 1:nrow(QDC_es)){
  # get the id of the edge from its tail and head
  edge_id <- get.edgeIDs(dyn_net,v=QDC_es$Actor_code[row],
                         alter=QDC_es$Tie_code[row])
  activate.edge.attribute(dyn_net,'edge_weights',QDC_es$edge_weights[row],
                          onset=QDC_es$onset[row],terminus=QDC_es$terminus[row],e=edge_id)
}


#year <- 1789

#slices <- c(year*10, year*10+3, year*10+6, year*10+8)

slices <- seq(from = start_slice, to = end_slice, by = slice_interval)

plot <- FALSE

slices <- c(17634, 17635)
all_community_sizes <- list()

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
  communities <- cluster_louvain(net_slice)
#  communities <- cluster_leiden(net_slice, objective_function = "modularity", n_iterations = 3)
  all_communities[[as.character(.slice)]] <- communities
  all_memberships[[as.character(.slice)]] <- membership(communities)
  if (plot) {
    plot(x = communities, y = net_slice,
         vertex.label = V(net_slice)$Actor_pers)
    title(.slice, cex.main = 3)
    vis <- recordPlot()
  }
  community_sizes <- sizes(communities)
  all_community_sizes[[as.character(.slice)]] <- community_sizes
  community_stats <- c("slice" = .slice,
                       "N_communities" = length(communities),
                       "net_modularity" = modularity(communities),
#                       "net_modularity" = communities$quality,
                       "min_community_size" = min(community_sizes), 
                       "max_community_size" = max(community_sizes),
                       "mean_community_size" = mean(community_sizes),
                       "sd_community_size" = sd(community_sizes)
                       )
  all_community_stats[[as.character(.slice)]] <- community_stats
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

#Export visual

Cairo(file = paste0(export_path, "Communities - ", year, ".png"), width = 2400, height = 1800, type = "png", bg = "white")
print(vis)
dev.off()

# Visualise change in modularity, showing intervention by La Chalotais (1763) and Borrely

all_community_stats_combined_df$year <- trunc(all_community_stats_combined_df$slice/10)
all_community_stats_combined_df$year_for_vis <- as.Date(as.character(all_community_stats_combined_df$year), format = "%Y")

# X-axis: Create a named vector where the names are the 'slice' values and the values are the 'year_for_vis' values
year_labels <- setNames(format(all_community_stats_combined_df$year_for_vis, "%Y"), all_community_stats_combined_df$slice)

# X-axis: Select every element of 'slice' that ends in 0
breaks <- all_community_stats_combined_df$slice[seq(1, length(all_community_stats_combined_df$slice), by = 10)]

# Modify year_labels accordingly
year_labels <- year_labels[as.character(breaks)]

# Create "interventions" df for showing La Chal (1763) and Borrelly (1768) intervention

intervention_slices <- unique(
  QDC_es$onset[QDC_es$`ACTOR-PERSON`=="La Chalotais" & QDC_es$onset>17630 | 
                                      QDC_es$`ACTOR-PERSON`=="Borrelly" & QDC_es$onset<17700]
)

interventions <- all_community_stats_combined_df[all_community_stats_combined_df$slice %in% intervention_slices,]
interventions$slice <- interventions$slice-1
interventions$vline_labels <- c("La Chalotais (1763)", "Borrelly (1768)")

# load fonts

#font_import()
loadfonts(device = "win")

# Create dataframe with just La Chalotais and Borrely
ggplot(data = all_community_stats_combined_df[c(-1, -2),],
       aes(x = slice, y = net_modularity)) +
  geom_line(col = "black") +
  geom_vline(data = interventions, mapping = aes(xintercept = slice), linewidth = 0.4, color = "red", show.legend = FALSE) +
  geom_label(data = interventions, mapping = aes(x = slice, y = 0.66, label = vline_labels, hjust = 0), size = 10) +
  labs(x = "year", y = "Network modularity") +
  scale_x_continuous(name = "year", breaks = breaks, labels = year_labels) +
  theme(axis.title = element_text(size = 50), axis.text = element_text(size = 50)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.margin = unit(c(15, 15, 15, 15), units = "pt")) +
  theme(text = element_text(family = "Calibri"))


# Export vis

vis <- recordPlot()
Cairo(file = paste0(export_path, "Network_modularity_", text_or_pers, "_network_no_loops.png"), width = 2400, height = 1800, type = "png", bg = "white")
print(vis)
dev.off()





# Combine community structures into one adjacency matrix per slice


# Record the number of times that particular membership results have occurred, and export the visual 

# Flatten the list of lists into a single list of strings
membership_strings <- unlist(lapply(all_memberships_combined, function(x) toString(unlist(x))))

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
  
  communities_to_plot <- all_communities_combined[[community]][[1]]
  plot(x = communities_to_plot, y = net_slice,
       vertex.label = V(net_slice)$Actor_pers)
  title(.slice, cex.main = 3)
  vis <- recordPlot()
  
  Cairo(file = paste0(export_path, "Communities_", .slice, "_likelihood_", membership_prob, "_no_loops.png"), width = 2400, height = 1800, type = "png", bg = "white")
  print(vis)
  dev.off()
}


# Calculate Bonacich alpha Centrality

alphacent_stats <- data.frame(dyn_net %v% "Actor_pers")

for (.slice in as.character(slices)) {
  net_slice <- as_igraph_net_slice(dynamic_net = dyn_net, v_name_attr = "Actor_pers", slice = as.numeric(.slice), retain_vertices = TRUE)
  #net_slice <- simplify(net_slice, remove.multiple = TRUE, remove.loops = TRUE)
  #net_slice <- delete.vertices(net_slice, degree(net_slice)==0)
  alphacent_stats[[.slice]] <- alpha_centrality(net_slice, alpha = 0.1)
  } 
colnames(alphacent_stats)[1] <- "actor_name"

rio::export(alphacent_stats, file = paste0(export_path,"alpha_centrality_by_", slice_or_year, "_", date, "_", text_or_pers,"_net.csv"))


# Create dynamic network objects
## Note: not needed with line above

#if (text_or_pers == "text") {
#  QDC_dyn <- QDC_text_dyn
#} else if (text_or_pers == "pers") {
#  QDC_dyn <- QDC_pers_dyn
#}

# Add multiple edges - note, should only be relevant with person network; might even return an error with text net
#QDC_dyn <- add_multiple_edges_active(netdyn = QDC_dyn)

### tSnaStats doesn't seem to work with changing vertex activity - so recreate network with all nodes present in 17620

QDC_vs_onset_62 <- QDC_vs
QDC_vs_onset_62$onset <- start_slice

## Remove the base_net argument below as otherwise the tSnaStats function ignores edge spells 
QDC_dyn_onset_62 <- networkDynamic(vertex.spells = QDC_vs_onset_62[,1:4], edge.spells = QDC_es[,1:4], create.TEAs = TRUE)
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

degree <- tSnaStats(QDC_dyn_onset_62, snafun = "degree", start = start_slice, end = end_slice, time.interval = slice_interval)
indegree <- tSnaStats(QDC_dyn_onset_62, snafun = "degree", start = start_slice, end = end_slice, time.interval = slice_interval, cmode="indegree")
outdegree <- tSnaStats(QDC_dyn_onset_62, snafun = "degree", start = start_slice, end = end_slice, time.interval = slice_interval, cmode="outdegree")
degree_neg <- tSnaStats(QDC_dyn_neg, snafun = "degree", start = start_slice, end = end_slice, time.interval = slice_interval)
indegree_neg <- tSnaStats(QDC_dyn_neg, snafun = "degree", start = start_slice, end = end_slice, time.interval = slice_interval, cmode="indegree")
outdegree_neg <- tSnaStats(QDC_dyn_neg, snafun = "degree", start = start_slice, end = end_slice, time.interval = slice_interval, cmode="outdegree")

eigenvector_undirected <- tSnaStats(QDC_dyn_onset_62_undirected, snafun = "evcent", start = start_slice, end = end_slice, time.interval = slice_interval, maxiter=1e7, use.eigen = F)
eigenvector_inversed <- tSnaStats(QDC_dyn_onset_62_inversed, snafun = "evcent", start = start_slice, end = end_slice, time.interval = slice_interval, maxiter=1e7, use.eigen = F)

#directed_closeness <- tSnaStats(QDC_dyn_onset_62, snafun = "closeness", start = start_slice, end = end_slice, time.interval = slice_interval, cmode = "suminvdir")
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


# Try to calculate local transitivity/clustering coefficient

#extracts <- network.extract(QDC_dyn, at = 17620, rule = "any", active.default = FALSE)
#extracts <- intergraph::asIgraph(extracts)

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

#write_xlsx(stats_list, path = paste0(export_path,"stats_by_", slice_or_year, "_", date, "_", text_or_pers,"_net.xlsx"))

# Try transposing everything to make it more readable

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

write_xlsx(stats_list_transposed, path = paste0(export_path,"stats_by_", slice_or_year, "_", date, "_", text_or_pers,"_net_transposed.xlsx"))



## Export overall degree centrality results

degree_data <- degree[nrow(degree),]

degree_data <- as.data.frame(degree_data)

# create actor labels to display on the visualisation
degree_data$labels <- NA
actors_to_label <- c("Rousseau", "Mercure", "Ann?e litt?raire", "Rolland d'Erceville")
percentages <- c("(16.7%)", "(8.6%)", "(11.4%)", "(9.6%)")

for (i in seq_along(actors_to_label)) {
  degree_data[actors_to_label[i],"labels"] <- paste0(actors_to_label[i], " ", percentages[i])
}

ggplot(data = degree_data,
       aes(x = degree_data)) +
  geom_histogram(binwidth = 1, fill = "#FF776C", col = "black") +
  geom_label(data = degree_data[!is.na(degree_data$labels),], mapping = aes(x = degree_data, y = c(2, 2, 4, 3), label = labels, hjust = c(0.7, 0.5, 0.5, 0.5)), size = 15) +
  labs(x = "Degree centrality", y = "Number of nodes") +
  theme(axis.title = element_text(size = 50), axis.text = element_text(size = 50)) +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60)) +
  theme(plot.margin = unit(c(15, 15, 15, 15), units = "pt"))


# Export vis

vis <- recordPlot()
Cairo(file = paste0(export_path, "Degree_centrality_", text_or_pers, "_network_node_labels.png"), width = 2400, height = 1800, type = "png", bg = "white")
print(vis)
dev.off()

