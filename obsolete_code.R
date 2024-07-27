# vector of colours, taken from Stackoverflow (courtesy Kevin Wright) - https://stackoverflow.com/questions/9563711/r-color-palettes-for-many-data-classes
#
#c25 <- c(
#  "dodgerblue2", "#E31A1C", # red
#  "green4",
#  "#6A3D9A", # purple
#  "#FF7F00", # orange
##  "black", 
#  "gold1",
#  "skyblue2", "#FB9A99", # lt pink
#  "palegreen2",
#  "#CAB2D6", # lt purple
#  "#FDBF6F", # lt orange
#  "gray70", "khaki2",
#  "maroon", "orchid1", "deeppink1", "blue1", "steelblue4",
#  "darkturquoise", "green1", "yellow4", "yellow3",
#  "darkorange4", "brown"
#)
#
#c25_comm <- rep(NA, length(c25))
#names(c25_comm) <- c25
#
#for (col in c25) {
#  c25_comm[col] <- rgb(red = col2rgb(col)[1], green = col2rgb(col)[2], blue = col2rgb(col)[3], alpha = 0.3, maxColorValue = 255)
#}
#




#compute.animation(QDC_pers_dyn, slice.par=list(start=1762, end=1770, interval=1, aggregate.dur=1, rule="any"), default.dist = 6, verbose = TRUE)
#compute.animation(QDC_pers_dyn, slice.par=list(start=1761.8, end=1764.8, interval=0.2, aggregate.dur=0.2, rule="any"), chain.direction = "reverse", default.dist = 6, verbose = TRUE) # Note: This producesa kind of error where 'animation.x.active' is apparently malformed (inconsistent across years). This is the case whether I split QDC years or leave the Date variable as it is originally


## Normal way of doing it


compute.animation(QDC_pers_dyn, slice.par=list(start=start, end=end, interval=1, aggregate.dur=1, rule="any"), default.dist = 6, verbose = TRUE)

QDC_pers_anim_final <- compute.animation(QDC_pers_dyn, slice.par=list(start=end, end=end, interval=1, aggregate.dur=1, rule="any"), animation.mode = "kamadakawai", chain.direction = "reverse", default.dist = 6, verbose = TRUE)

QDC_pers_anim <- compute.animation(QDC_pers_dyn, slice.par=list(start=start, end=end, interval=1, aggregate.dur=1, rule="any"), animation.mode = "kamadakawai", seed.coords = matrix(data = c(get.vertex.attribute.active(QDC_pers_anim_final, "animation.x", at = end), get.vertex.attribute.active(QDC_pers_anim_final, "animation.y", at = end)), ncol = 2), chain.direction = "reverse", default.dist = 6, verbose = TRUE)



render.d3movie(QDC_pers_anim, render.par=list(tween.frames=50, show.time = TRUE), displaylabels = FALSE,
               plot.par = list(bg="white",
                               #               render.par=list(tween.frames=100, show.time= FALSE),
                               vertex.border="#ffffff",
                               vertex.col = "Type_col",
                               #               vertex.sides = "Vertex_sides", # only circular nodes wanted for final version
                               main="Querelle des coll?ges, 1762-1789",
                               #              xlab = function(s){paste(trunc((QDC_pers_dyn$gal$slice.par$start+1761)+(QDC_pers_dyn$gal$slice.par$interval*s)/210))}, #This label makes the start year appear at the bottom, truncated of its decimal numbers, when you use the system where each year is split into 210
                               xlab = function(s){paste(trunc((QDC_pers_dyn$gal$slice.par$start+QDC_pers_dyn$gal$slice.par$interval*s)/10))},
                               vertex.cex = function(slice){(10*(sna::degree(slice, cmode = "freeman") + 0.000001)/(sna::degree(slice, cmode = "freeman") + 0.000001)*(log(((sna::degree(slice, cmode = "freeman")+5)/100)+1)))},
                               #               vertex.cex = 0.8,
                               #               vertex.cex = function(slice){ 0.8*degree(slice)/degree(slice) + 0.000001},
                               edge.lwd = 2,
                               vertex.tooltip = function(slice){slice %v% 'vertex.names'}, #(QDC_pers_dyn %v% 'vertex.names')
                               #                               edge.tooltip = function(slice){slice %e% 'Tie_name'},
                               edge.col = "Qual_col", usearrows=TRUE),
               d3.options = list(animationDuration=800, debugFrameInfo=FALSE, durationControl=TRUE, margin=list(x=0,y=10), enterExitAnimationFactor=0.1),
               #               launchBrowser=TRUE, filename="QDC_pers_indegree_slider_fixed_final_test_positions.html",
               launchBrowser=TRUE, filename="QDC_pers_degree_slider_fixed.html",
               #               launchBrowser=TRUE, filename="Final position.html",
               verbose=TRUE)




#### Legacy visualisation codes



start <- 1761.8
end <- 1790
inertia <- 8 # node position calculated by kamada kawai will be 1/inertia of the final node position. I.e. the greater the inertia, the less the position calculated by kamada kawai will contribute to the node's position and the more the node will stay close to its final position--i.e., the less the node will move

## Normal version - without inertia
## Let's make texts appear month by month (i.e. interval of 0.08333333)

compute.animation(QDC_pers_dyn, slice.par=list(start=start, end=end, interval=0.08333333, aggregate.dur=0.08333333, rule="any"), animation.mode = "kamadakawai", chain.direction = "reverse", default.dist = 6, verbose = TRUE) # Note: This producesa kind of error where 'animation.x.active' is apparently malformed (inconsistent across years). This is the case whether I split QDC years or leave the Date variable as it is originally
render.d3movie(QDC_pers_dyn, displaylabels = FALSE, bg="white",
               render.par=list(tween.frames=50, show.time=FALSE),
               vertex.border="#ffffff",
               vertex.col = "Type_col",
               xlab = function(s){paste(trunc(QDC_pers_dyn$gal$slice.par$start+QDC_pers_dyn$gal$slice.par$interval*s))}, #This label makes the start year appear at the bottom, truncated of its decimal numbers
               cex = 2,
               #              plot.par = list(mai=c(0,0,0,0)), # DOESN'T WORK
               #               vertex.cex = function(slice){0.5/(1 + degree(slice)*10) + (10*degree(slice)/(degree(slice) + 0.00001)*(log(((degree(slice)+5)/100)+1)))},
               vertex.cex = function(slice){(10*(log(((degree(slice)+5)/100)+1)))},
               edge.lwd = 1.5,
               vertex.tooltip = function(slice){slice %v% 'vertex.names'},
               edge.col = "Qual_col", usearrows=TRUE, edge.lwd=4,
               output.mode = "HTML",
               d3.options = list(animationDuration=600, enterExitAnimationFactor=0, debugFrameInfo=FALSE, durationControl=TRUE, margin=list(x=1,y=1)),
               launchBrowser=TRUE, filename="QDC_pers_degree_normal.html",
               verbose=TRUE)


## Let's try to create algorith that makes the layout move much less

## Version 1: make nodes stay closer to their final position



compute.animation(QDC_pers_dyn, slice.par=list(start=(end-0.01), end=end, interval=0.08333333, aggregate.dur=0.08333333, rule="any"), animation.mode = "kamadakawai", layout.par = list(x = get.vertex.attribute.active(QDC_pers_dyn, "animation.x", onset = 1789.8, terminus = 1790), y = get.vertex.attribute.active(QDC_pers_dyn, "animation.y", onset = 1789.8, terminus = 1790)), chain.direction = "reverse", default.dist = 6, verbose = TRUE) # Note: This producesa kind of error where 'animation.x.active' is apparently malformed (inconsistent across years). This is the case whether I split QDC years or leave the Date variable as it is originally
x <- get.vertex.attribute.active(QDC_pers_dyn, "animation.x", at=end)
y <- get.vertex.attribute.active(QDC_pers_dyn, "animation.y", at=end)


compute.animation(QDC_pers_dyn, slice.par=list(start=start, end=end, interval=0.08333333, aggregate.dur=0.08333333, rule="any"), animation.mode = "kamadakawai", chain.direction = "reverse", default.dist = 6, verbose = TRUE) # Note: This producesa kind of error where 'animation.x.active' is apparently malformed (inconsistent across years). This is the case whether I split QDC years or leave the Date variable as it is originally
for (i in seq(from = start,to = end, by=0.08333333)) {
  activate.vertex.attribute(QDC_pers_dyn, "animation_x", value = (get.vertex.attribute.active(QDC_pers_dyn, "animation.x", at = i)+(x*(inertia-1)))/inertia, at = i)
}

for (i in seq(from = start,to = end, by=0.08333333)) {
  activate.vertex.attribute(QDC_pers_dyn, "animation_y", value = (get.vertex.attribute.active(QDC_pers_dyn, "animation.y", at = i)+(y*(inertia-1)))/inertia, at = i)
}

## Let's make texts appear month by month (i.e. interval of 0.08333333)
compute.animation(QDC_pers_dyn, slice.par=list(start=start, end=end, interval=0.08333333, aggregate.dur=0.08333333, rule="any"), animation.mode = "useAttribute", layout.par = list(x = "animation_x", y = "animation_y"), chain.direction = "reverse", default.dist = 6, verbose = TRUE)
render.d3movie(QDC_pers_dyn, displaylabels = FALSE, bg="white",
               render.par=list(tween.frames=50, show.time=FALSE),
               vertex.border="#ffffff",
               vertex.col = "Type_col",
               xlab = function(s){paste(trunc(QDC_pers_dyn$gal$slice.par$start+QDC_pers_dyn$gal$slice.par$interval*s))}, #This label makes the start year appear at the bottom, truncated of its decimal numbers
               cex = 2,
               #              plot.par = list(mai=c(0,0,0,0)), # DOESN'T WORK
               #               vertex.cex = function(slice){0.5/(1 + degree(slice)*10) + (10*degree(slice)/(degree(slice) + 0.00001)*(log(((degree(slice)+5)/100)+1)))},
               vertex.cex = function(slice){(10*(log(((degree(slice)+5)/100)+1)))},
               edge.lwd = 1.5,
               vertex.tooltip = function(slice){slice %v% 'vertex.names'},
               edge.col = "Qual_col", usearrows=TRUE, edge.lwd=4,
               output.mode = "HTML",
               d3.options = list(animationDuration=600, enterExitAnimationFactor=0, debugFrameInfo=FALSE, durationControl=TRUE, margin=list(x=1,y=1)),
               launchBrowser=TRUE, filename="QDC_pers_normal.html",
               verbose=TRUE)





#### Version 2: make nodes closer to their next position in the sequence.

## Doesn't do much in practice

QDC_pers_dyn <- networkDynamic(base.net = QDC_pers_net, vertex.spells = QDC_vs[,1:5], edge.spells = QDC_es[,1:4], create.TEAs = TRUE)

compute.animation(QDC_pers_dyn, slice.par=list(start=start, end=end, interval=0.08333333, aggregate.dur=0.08333333, rule="any"), animation.mode = "kamadakawai", chain.direction = "reverse", default.dist = 6, verbose = TRUE) # Note: This producesa kind of error where 'animation.x.active' is apparently malformed (inconsistent across years). This is the case whether I split QDC years or leave the Date variable as it is originally

activate.vertex.attribute(QDC_pers_dyn, "animation_x", value = get.vertex.attribute.active(QDC_pers_dyn, "animation.x", at = end), at = end)
activate.vertex.attribute(QDC_pers_dyn, "animation_y", value = get.vertex.attribute.active(QDC_pers_dyn, "animation.y", at = end), at = end)

for (i in seq(from = end,to = start, by=-0.08333333)) {
  x <- get.vertex.attribute.active(QDC_pers_dyn, "animation.x", onset=(i), terminus=i)
  activate.vertex.attribute(QDC_pers_dyn, "animation_x", value = (get.vertex.attribute.active(QDC_pers_dyn, "animation.x", at = i-0.08333333)+(x*(inertia-1)))/inertia, at = i-0.08333333)
}

for (i in seq(from = end,to = start, by=-0.08333333)) {
  y <- get.vertex.attribute.active(QDC_pers_dyn, "animation.y", onset=(i), terminus=i)
  activate.vertex.attribute(QDC_pers_dyn, "animation_y", value = (get.vertex.attribute.active(QDC_pers_dyn, "animation.y", at = i-0.08333333)+(y*(inertia-1)))/inertia, at = i-0.08333333)
}

## Let's make texts appear month by month (i.e. interval of 0.08333333)
compute.animation(QDC_pers_dyn, slice.par=list(start=start, end=end, interval=0.08333333, aggregate.dur=0.08333333, rule="any"), animation.mode = "useAttribute", layout.par = list(x = "animation_x", y = "animation_y"), chain.direction = "reverse", default.dist = 6, verbose = TRUE)
render.d3movie(QDC_pers_dyn, displaylabels = FALSE, bg="white",
               render.par=list(tween.frames=50, show.time=FALSE),
               vertex.border="#ffffff",
               vertex.col = "Type_col",
               xlab = function(s){paste(trunc(QDC_pers_dyn$gal$slice.par$start+QDC_pers_dyn$gal$slice.par$interval*s))}, #This label makes the start year appear at the bottom, truncated of its decimal numbers
               cex = 2,
               #              plot.par = list(mai=c(0,0,0,0)), # DOESN'T WORK
               vertex.cex = function(slice){0.5/(1 + degree(slice)*10) + (10*degree(slice)/(degree(slice) + 0.00001)*(log(((degree(slice)+5)/100)+1)))},
               edge.lwd = 1.5,
               vertex.tooltip = function(slice){slice %v% 'vertex.names'},
               edge.col = "Qual_col", usearrows=TRUE, edge.lwd=4,
               output.mode = "HTML",
               d3.options = list(animationDuration=800, enterExitAnimationFactor=0, debugFrameInfo=TRUE, durationControl=TRUE, margin=list(x=1,y=1)),
               launchBrowser=TRUE, filename="QDC_pers_vers2.html",
               verbose=TRUE)



#### Version 3: Now let's try the solution where we artificially make every node be there from the beginning and make them only appear when they get an edge


QDC_vs_onset62 <- QDC_vs
QDC_vs_onset62$onset <- 1762

QDC_pers_dyn <- networkDynamic(base.net = QDC_pers_net, vertex.spells = QDC_vs_onset62[,1:5], edge.spells = QDC_es, create.TEAs = TRUE)
set.vertex.attribute(QDC_pers_dyn, "vertex.label.size", ifelse(get.vertex.attribute(QDC_pers_dyn, "vertex.names")=="D'Alembert (1753)"| get.vertex.attribute(QDC_pers_dyn, "vertex.names")=="La Chalotais (1763)"| get.vertex.attribute(QDC_pers_dyn, "vertex.names")=="Rousseau (1762)", 1, 0))
vertex.label.size <- get.vertex.attribute(QDC_pers_dyn, "vertex.label.size")

### Make animation
#compute.animation(QDC_pers_dyn, slice.par=list(start=1762, end=1770, interval=1, aggregate.dur=1, rule="any"), default.dist = 6, verbose = TRUE)
#compute.animation(QDC_pers_dyn, slice.par=list(start=1761.8, end=1764.8, interval=0.2, aggregate.dur=0.2, rule="any"), chain.direction = "reverse", default.dist = 6, verbose = TRUE) # Note: This producesa kind of error where 'animation.x.active' is apparently malformed (inconsistent across years). This is the case whether I split QDC years or leave the Date variable as it is originally
## Let's make perss appear month by month (i.e. interval of 0.08333333)

par(mfrow=c(1,1), mar=c(0.1,0.1,0.1,0.1)) 

compute.animation(QDC_pers_dyn, slice.par=list(start=start, end=end, interval=0.08333333, aggregate.dur=0.08333333, rule="any"), chain.direction = "reverse", default.dist = 5, verbose = TRUE) # Note: This producesa kind of error where 'animation.x.active' is apparently malformed (inconsistent across years). This is the case whether I split QDC years or leave the Date variable as it is originally
render.d3movie(QDC_pers_dyn, displaylabels = FALSE, bg="white",
               render.par=list(tween.frames=50, show.time=FALSE),
               vertex.border="#ffffff",
               vertex.col = "Type_col",
               xlab = function(s){paste(trunc(QDC_pers_dyn$gal$slice.par$start+QDC_pers_dyn$gal$slice.par$interval*s))}, #This label makes the start year appear at the bottom, truncated of its decimal numbers
               #vertex.sides = c(3,4,5),
               #vertex.cex = 0.8,
               #vertex.cex = function(slice){ (0.8*degree(slice)/(degree(slice) + 0.000001))}, #This one makes nodes grow as they appear
               #               vertex.cex = function(slice){ (10*degree(slice)/(degree(slice) + 0.000001)*(log(((degree(slice)+5)/100)+1)))}, #Trying to make node size proportional to node degree, but not with a linear funtion - THIS uses logarithm function. To make all nodes bigger, increase constant at beginning of line; to make Rousseau proportionally bigger, increase denominator of (degree(slice)+5)/100); to make small degree nodes relatively bigger, increase the constant k in (degree(slice)+k)/100)
               vertex.cex = function(slice){ (10*degree(slice)/(degree(slice))*(log(((degree(slice)+5)/100)+1)))}, #without the tiny number - Trying to make node size proportional to node degree, but not with a linear funtion - THIS uses logarithm function. To make all nodes bigger, increase constant at beginning of line; to make Rousseau proportionally bigger, increase denominator of (degree(slice)+5)/100); to make small degree nodes relatively bigger, increase the constant k in (degree(slice)+k)/100)
               #vertex.cex = function(slice){ (0.25*degree(slice)/(degree(slice) + 0.000001)*(degree(slice)*exp(-0.015*degree(slice))))}, #geometric decay function (see https://mathworld.wolfram.com/ExponentialDecay.html) - kind of works for large nodes but the small nodes are just tiny
               #vertex.cex = function(slice){ 0.8*degree(slice)/degree(slice)}, #This one makes nodes appear instantly - also if you add a very small number at the end
               #label.cex = vertex.label.size,
               edge.lwd = 1.5,
               vertex.tooltip = (QDC_pers_dyn %v% 'vertex.names'),
               edge.col = "Qual_col", usearrows=TRUE, edge.lwd=4,
               d3.options = list(animationDuration=600, debugFrameInfo=FALSE, durationControl=TRUE, margin=list(x=1,y=1)),
               launchBrowser=TRUE, filename="QDC_pers_onset62_instant_node_appear.html",
               verbose=TRUE)


# Divide onsets and termini by 10
#TODO: Fix this bodge to make it more sustainable

#if (any(QDC_vs$onset > 10000)) {
#  QDC_vs$onset <- trunc(QDC_vs$onset/10); QDC_vs$terminus <- trunc(QDC_vs$terminus/10)
#  QDC_es$onset <- trunc(QDC_es$onset/10); QDC_es$terminus <- trunc(QDC_es$terminus/10)
#}



### Producing several community plots in one visual


par(mfrow=c(2,2), mar = c(3, 0, 3, 0))

par(mfrow=c(1,1), mar = c(3, 0, 3, 0))


plot <- FALSE


if (plot) {
  plot(x = communities, y = net_slice,
       vertex.label = V(net_slice)$Actor_pers)
  title(.slice, cex.main = 3)
  vis <- recordPlot()
}


#Export visual

Cairo(file = paste0(export_path, "Communities - ", year, ".png"), width = 2400, height = 1800, type = "png", bg = "white")
print(vis)
dev.off()




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





#####################################################
##### Combine communities into adjacency matrix #####
#####################################################

### Note: Run select lines in QDC_stats_by_slice first

# Iterate over each second-order list in all_communities_combined
adj_matrices <- list()
for (i in 1:length(all_memberships_combined)) {
  # For each second-order list, create an adjacency matrix for each third-order list
  adj_matrices[[i]] <- lapply(all_memberships_combined[[i]], function(third_order_list) {
    # Create an adjacency matrix where each element is 1 if the corresponding actors are in the same community, and 0 otherwise
    adj_matrix <- outer(third_order_list, third_order_list, FUN = function(x, y) as.integer(x == y))
    return(adj_matrix)
  })
}
  
# Initialize an empty list to store the final result
final_matrices <- list()

# Get the number of matrices in each second-order list
num_matrices <- length(adj_matrices[[1]])

# For each index
for (i in 1:num_matrices) {
  # Extract the i-th matrix from each second-order list
  matrices_to_sum <- lapply(adj_matrices, function(second_order_list) second_order_list[[i]])
  
  # Sum the matrices
  sum_matrix <- Reduce('+', matrices_to_sum)
  sum_matrix <- sum_matrix/number_of_iterations
  
  # Add the summed matrix to the final result
  final_matrices[[i]] <- sum_matrix
}

# Now 'final_matrices' is a list of matrices, where each matrix is the sum of the matrices with the same index across the second-order lists
#TODO: automate naming of final_matrices

rio::export(list("1764" = final_matrices[[1]], "1765" = final_matrices[[2]]), paste0(export_path, "community_assignment.xlsx"), rowNames = TRUE)
