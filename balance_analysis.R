###############################
##### Analysis of balance #####
###############################

library(signnet)

slices <- seq(from = start_slice, to = end_slice, by = slice_interval)

## Add multiple_edges

dyn_net <- add_multiple_edges_active(netdyn = dyn_net)

## Create sign variable and assign to dyn_net

QDC_es$sign <- (QDC_es$Quality %in% c(1, 2, 6))*-2
QDC_es$sign <- QDC_es$sign + 1
table(QDC_es$sign, QDC_es$Quality) # spot check
  
for(row in 1:nrow(QDC_es)){
  # get the id of the edge from its tail and head
  edge_id <- get.edgeIDs(dyn_net,v=QDC_es$Actor_code[row],
                         alter=QDC_es$Tie_code[row])
  activate.edge.attribute(dyn_net,'sign',QDC_es$sign[row],
                          onset=QDC_es$onset[row],terminus=QDC_es$terminus[row],e=edge_id)
}

balance_scores <- as.data.frame(slices)
balance_scores$frustration_balance <- NA

for (.slice in as.character(slices)) {
net_slice <- as_igraph_net_slice(dynamic_net = dyn_net, v_name_attr = "Actor_pers", slice = as.numeric(.slice), retain_vertices = TRUE)
net_slice <- simplify(net_slice, remove.multiple = FALSE, remove.loops = TRUE)
net_slice <- as.undirected(net_slice, mode = "collapse", edge.attr.comb = "sum")
E(net_slice)$sign <- ifelse(E(net_slice)$sign>0, 1, -1)
balance_scores$frustration_balance[balance_scores$slices==.slice] <- balance_score(net_slice, method = "frustration")
balance_scores$local_balance[balance_scores$slices==.slice] <- balance_score(net_slice, method = "triangles")
}


# Visualise change in balance scores, showing intervention by Leroy

balance_scores$year <- trunc(balance_scores$slice/10)
balance_scores$year_for_vis <- as.Date(as.character(balance_scores$year), format = "%Y")

# X-axis: Create a named vector where the names are the 'slice' values and the values are the 'year_for_vis' values
year_labels <- setNames(format(balance_scores$year_for_vis, "%Y"), balance_scores$slice)

# X-axis: Select every element of 'slice' that ends in 0
breaks <- balance_scores$slice[seq(1, length(balance_scores$slice), by = 10)]

# Modify year_labels accordingly
year_labels <- year_labels[as.character(breaks)]

# Create "interventions" df for showing La Chal (1763) and Borrelly (1768) intervention

intervention_slices <- unique(
  QDC_es$onset[QDC_es$`ACTOR-PERSON`=="Leroy"])

interventions <- balance_scores[balance_scores$slice %in% intervention_slices,]
interventions$slice <- interventions$slice-1
interventions$vline_labels <- c("Leroy (1777)")

# load fonts

#font_import()
loadfonts(device = "win")

# Create dataframe with just La Chalotais and Borrely
ggplot(data = balance_scores[c(-1:-4),],
       aes(x = slices, y = local_balance)) +
  geom_line(col = "black") +
  geom_vline(data = interventions, mapping = aes(xintercept = slice), linewidth = 0.4, color = "red", show.legend = FALSE) +
  geom_label(data = interventions, mapping = aes(x = slice, y = 0.66, label = vline_labels, hjust = 0), size = 10) +
  labs(x = "year", y = "local balance") +
  scale_x_continuous(name = "year", breaks = breaks, labels = year_labels) +
  theme(axis.title = element_text(size = 50), axis.text = element_text(size = 50)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.margin = unit(c(15, 15, 15, 15), units = "pt")) +
  theme(text = element_text(family = "Calibri"))


# Export vis

vis <- recordPlot()
Cairo(file = paste0(export_path, "\\charts\\balance_local_", text_or_pers, "_network_no_loops.png"), width = 2400, height = 1800, type = "png", bg = "white")
print(vis)
dev.off()

