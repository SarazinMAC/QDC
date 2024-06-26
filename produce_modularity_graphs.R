####################################
##### Produce Modularity Graph #####
####################################




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

loadfonts(device = "win")

# Create dataframe with just La Chalotais and Borrely
ggplot(data = all_community_stats_combined_df[c(-1, -2),],
       aes(x = slice, y = net_modularity)) +
  geom_line(col = "black") +
  geom_vline(data = interventions, mapping = aes(xintercept = slice), linewidth = 0.4, color = "red", show.legend = FALSE) +
  #  geom_label(data = interventions, mapping = aes(x = slice, y = 0.66, label = vline_labels, hjust = 0), size = 10) +
  labs(x = "year", y = "Network modularity") +
  scale_x_continuous(name = "year", breaks = breaks, labels = year_labels) +
  theme(axis.title = element_text(size = 50), axis.text = element_text(size = 50)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.margin = unit(c(15, 15, 15, 15), units = "pt")) +
  theme(text = element_text(family = "Calibri"))


# Export vis

vis <- recordPlot()
#Cairo(file = paste0(export_path, "Network_modularity_", text_or_pers, "_network_no_loops.png"), width = 2400, height = 1800, type = "png", bg = "white")
jpeg(filename = paste0(export_path, "Network_modularity_", text_or_pers, "_network_", cd_algorithm, "_no_loops_no_labels.png"),
     width = 4800, height = 3600, type = "cairo", bg = "white", family = "Calibri", symbolfamily = "Calibri", res = 200)
print(vis)
dev.off()

