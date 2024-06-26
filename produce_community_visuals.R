#####################################
##### Produce Community Visuals #####
#####################################



# Export visuals from every result of the community detection algorithm

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
