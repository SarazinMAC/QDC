########################################
##### Produce centrality histogram #####
########################################

## Create and Export histogram of degree/outdegree centrality distribution


degree_data <- get(histogram_measure)
degree_data <- degree_data[nrow(degree_data),]

degree_data <- as.data.frame(degree_data)

# Note: no longer needed as node labels will be created separately
# create actor labels to display on the visualisation
#if (text_or_pers=="pers") {
#  if (histogram_measure=="degree") {
#    actors_to_label <- c("Rousseau", "Mercure", "Ann�e litt�raire", "Rolland d'Erceville")
#    actor_labels <- c("Rousseau (16.7%)", " Mercure (8.6%)", " Ann�e\nlitt�raire (11.4%)", "Rolland\nd'Erceville (9.6%)")
#    label_y_position <- c(2, 2, 4, 5.5)
#    label_hjust <- c(0.7, 0.5, 0.5, 0.5)
#  } else if (histogram_measure=="outdegree") {
#    actors_to_label <- c("Rousseau", "Mercure", "Ann�e litt�raire", "Rolland d'Erceville", "Borrelly", "Rivard")
#    actor_labels <- c("Rousseau (0%)", "Mercure (8.4%)", "Ann�e\nlitt�raire (10.6%)", "Rolland\nd'Erceville (8.9%)", "Borrelly (5.3%)", "Rivard (7.6%)")
#    label_y_position <- c(2.5, 4, 6, 7.5, 2, 2)
#    label_hjust <- c(0.4, 0.5, 0.5, 0.5, 0.5, 0.5)
#  }
#} else if (text_or_pers=="text") {
#  if (histogram_measure=="degree") {
#    actors_to_label <- c("Rousseau (1762)", "D'Alembert (1753)", "Borrelly (1768)", "La Chalotais (1763)")
#    actor_labels <- c("Rousseau\n(1762) - 16.6%", "D'Alembert\n(1753) - 6.2%", " Borrelly\n(1768) - 5.5%", "La Chalotais\n(1763) - 5.5%")
#    label_y_position <- c(5, 5, 11, 11)
#    label_hjust <- c(0.7, 0.1, -0.05, 1.05)
#  } else if (histogram_measure=="outdegree") {
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

if (histogram_measure=="degree") {
  
vis <- ggplot(data = degree_data,
         aes(x = degree_data)) +
    geom_histogram(binwidth = 1, fill = "#FF776C", col = "black") +
    #  geom_label(data = degree_data[!is.na(degree_data$labels),], mapping = aes(x = degree_data, y = label_y_position, label = labels, hjust = label_hjust), size = 15) +
    labs(x = "Degree centrality", y = "Number of nodes") +
    theme(axis.title = element_text(size = 50), axis.text = element_text(size = 50)) +
    scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60)) +
    theme(plot.margin = unit(c(15, 15, 15, 15), units = "pt")) +
    theme(text = element_text(family = "Calibri"))
  
} else if (histogram_measure=="outdegree") {
  vis <- ggplot(data = degree_data,
         aes(x = degree_data)) +
    geom_histogram(binwidth = 1, fill = "#FF776C", col = "black") +
    #    geom_label(data = degree_data[!is.na(degree_data$labels),], mapping = aes(x = degree_data, y = label_y_position , label = labels, hjust = label_hjust), size = 15) +
    labs(x = "Outdegree centrality", y = "Number of nodes") +
    theme(axis.title = element_text(size = 50), axis.text = element_text(size = 50)) +
    scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60)) +
    theme(plot.margin = unit(c(15, 15, 15, 15), units = "pt")) +
    theme(text = element_text(family = "Calibri"))
}

# Export vis

#vis <- recordPlot()
#Cairo(file = paste0(export_path, histogram_measure, "_centrality_", text_or_pers, "_network_node_no_labels.png"), width = 2400, height = 1800, type = "png", bg = "white")
jpeg(filename = paste0(export_path, histogram_measure, "_centrality_", text_or_pers, "_network.jpeg"),
     width = 4800, height = 3600, type = "cairo", bg = "white", family = "Calibri", symbolfamily = "Calibri", res = 200)
print(vis)
dev.off()

