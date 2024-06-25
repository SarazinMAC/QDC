########################################
##### Produce dynamic text network #####
########################################


##### ____Create Network Dynamic Object for text network #####


# Import static network creation scripts

source(paste0(Data_path, "create_static_text_net.R"))

# Create vertex spell
## First, QDC actors

QDC_vs <- create_vertex_spells(main_df = QDC, node_attr_df = QDC_text_nodes,
                               actor_colname = "ACTOR-TEXT", final_actor_colname = "Actor_text",
                               alter_colname = "TIE-TEXT")

## Create edge spell

# First, main QdC texts

QDC_text_62_89 <- QDC_text[QDC_text$Date>1761 & QDC_text$Date<1790, c("ACTOR-TEXT", "TIE-TEXT", "Quality", "Date", "order")]
# Set edge colours again - note the pre-QdC edge colours will be overwritten but we will add these back on later
QDC_text_62_89$Qual_col <- c("red", "red", "grey61", "chartreuse3", "chartreuse3", "orange", "grey61", "grey61", "gray15")[QDC_text_62_89$Quality]


# Second, pre-QdC texts, with edge dates remaining pre-62
# NOTE: Edges for pre-QdC texts should change colour whenever they are first brought in during the QdC
# Similarly, in network stats, their edge onsets should be set to that value

QDC_pre_62_edges <- extract_pre_qdc_edges(main_df = QDC,
                                          actor_colname = "ACTOR-TEXT", alter_colname = "TIE-TEXT",
                                          order_colname = "order")

# now, merge both

QDC_es <- rbind(QDC_pre_62_edges, QDC_text_62_89)
rm(QDC_text_62_89)

QDC_es <- QDC_es_transforms(es_df = QDC_es, vs_df = QDC_vs,
                            actor_colname = "ACTOR-TEXT", alter_colname = "TIE-TEXT",
                            vs_actor_colname = "Actor_text", order_colname = "order")

# Also transform QDC_pre_62_edges to create colour changes

QDC_pre_62_edges <- QDC_es_transforms(es_df = QDC_pre_62_edges, vs_df = QDC_vs,
                                      actor_colname = "ACTOR-TEXT", alter_colname = "TIE-TEXT",
                                      vs_actor_colname = "Actor_text", order_colname = "order")

## Create network dynamic object

# Version where we make pre-QdC edges and vertices be a different colour before they are "brought in"

QDC_vs_dynamic_vis <- QDC_vs

QDC_es_dynamic_vis <- QDC_es

QDC_es_dynamic_vis$Qual_col <- c("red", "red", "grey61", "chartreuse3", "chartreuse3", "orange", "grey61", "grey61", "gray15")[QDC_es_dynamic_vis$Quality]

if (slice_or_year == "slice") {
  start_slice <- 17620
} else if (slice_or_year == "year") {
  start_slice <- 1762
}

QDC_es_dynamic_vis <- assign_pre_QDC_edge_onsets(edge_spells = QDC_es_dynamic_vis, .start_slice = start_slice)

QDC_es_dynamic_vis <- rbind(QDC_pre_62_edges, QDC_es_dynamic_vis)

QDC_text_dyn <- networkDynamic(vertex.spells = QDC_vs_dynamic_vis[,1:5], edge.spells = QDC_es_dynamic_vis[,c(1:4, 9)], create.TEAs = TRUE)

## Define vertex and edge attributes

# static vertex attributes

QDC_text_attr_dyn <- create_static_vertex_attr_df(vs_df = QDC_vs_dynamic_vis, 
                                                  node_attr_df = QDC_text_nodes,
                                                  actor_colname = "Actor_text",
                                                  Text_or_pers_name = "Text_Name")

for (col in colnames(QDC_text_attr_dyn)) {
  QDC_text_dyn %v% col <- QDC_text_attr_dyn[[col]]
}

# dynamic vertex attribute

QDC_vs_dynamic_vis_attr <- create_dyn_vertex_attr_df(vs_df = QDC_vs_dynamic_vis, es_df = QDC_es,
                                                     attr_df = QDC_text_attr_dyn,
                                                     actor_colname = "Actor_text",
                                                     Text_or_pers_name = "Text_Name")

# loop over vertex data to add the dynamic attributes on the vertices
for(row in 1:nrow(QDC_vs_dynamic_vis_attr)){
  activate.vertex.attribute(x = QDC_text_dyn, prefix = 'vertex_colour',
                            value = QDC_vs_dynamic_vis_attr$vertex_colour[row],
                            onset=QDC_vs_dynamic_vis_attr$onset[row],terminus=QDC_vs_dynamic_vis_attr$terminus[row],
                            v=QDC_vs_dynamic_vis_attr$Actor_code[row])
  
}


# Add Dynamic edge attributes

# loop over edge data to add the dynamic attributes on the edge
for(row in 1:nrow(QDC_es_dynamic_vis)){
  edge_id <- get.edgeIDs(QDC_text_dyn,v=QDC_es_dynamic_vis$Actor_code[row],
                         alter=QDC_es_dynamic_vis$Tie_code[row])
  activate.edge.attribute(QDC_text_dyn,'edge_colour',QDC_es_dynamic_vis$Qual_col[row],
                          onset=QDC_es_dynamic_vis$onset[row],terminus=QDC_es_dynamic_vis$terminus[row],e=edge_id)
  activate.edge.attribute(QDC_text_dyn,'Tie_name',QDC_es_dynamic_vis$Tie_name[row],
                          onset=QDC_es_dynamic_vis$onset[row],terminus=QDC_es_dynamic_vis$terminus[row],e=edge_id)
}

#### CORRIGER (!) les accents etc.
chars <- import(paste0(Data_path, "HTML_codes_French_characters.xlsx"))

## For loop: For every French character in the first column of the dataframe "chars", replace it with the HTML code in the third column of the dataframe
for (char in chars[,1]) {
  QDC_text_dyn %v% "vertex.names" <- gsub(char, chars[which(chars$Character==char),3], QDC_text_dyn %v% "vertex.names") 
  QDC_text_dyn %v% "Text_Name" <- gsub(char, chars[which(chars$Character==char),3], QDC_text_dyn %v% "Text_Name") 
  QDC_text_dyn %v% "Actor_text" <- gsub(char, chars[which(chars$Character==char),3], QDC_text_dyn %v% "Text_Name") 
}; rm(char)


##### Create dynamic visual #####

### Classic solution (nodes are not present from the beginning) but with node size weighted by indegree

start <- 17619
end <- 17640

# Calculate animation

QDC_text_anim <- compute.animation(QDC_text_dyn, slice.par=list(start=start, end=end, interval=1, aggregate.dur=0, rule="any"), animation.mode = "kamadakawai", chain.direction = "reverse", default.dist = 6, verbose = TRUE)

#QDC_text_anim_final <- compute.animation(QDC_text_dyn, slice.par=list(start=end, end=end, interval=1, aggregate.dur=0, rule="any"), animation.mode = "kamadakawai", chain.direction = "reverse", default.dist = 6, verbose = TRUE)

#QDC_text_anim <- compute.animation(QDC_text_dyn, slice.par=list(start=start, end=end, interval=1, aggregate.dur=0, rule="any"), animation.mode = "kamadakawai", seed.coords = matrix(data = c(get.vertex.attribute.active(QDC_text_anim_final, "animation.x", at = end), get.vertex.attribute.active(QDC_text_anim_final, "animation.y", at = end)), ncol = 2), chain.direction = "reverse", default.dist = 6, verbose = TRUE)

QDC_text_anim2 <- QDC_text_dyn

for (i in seq(from = start,to = end, by=1)) {
  activate.vertex.attribute(QDC_text_anim2, "animation.x", at = i, value = (get.vertex.attribute.active(QDC_text_anim, "animation.x", at = i))/3.5)
}

for (i in seq(from = start,to = end, by=1)) {
  activate.vertex.attribute(QDC_text_anim2, "animation.y", at = i, value = (get.vertex.attribute.active(QDC_text_anim, "animation.y", at = i))/3.5)
}

## Recreating the network with new process (as of 2024-02-17)

# Note: year_label function has been moved to top of file


node_size <- function(slice){(10*(sna::degree(slice, cmode = "freeman") + 0.000001)/
                                (sna::degree(slice, cmode = "freeman") + 0.000001)*(log(((
                                  sna::degree(slice, cmode = "freeman")+5)/100)+1)))
}

filename <- "QDC_text_with_pre_QdC_colours_refactored_2024_06_05.html"

render.d3movie(QDC_text_anim2,
               #render.d3movie(QDC_text_anim,
               render.par=list(tween.frames=50, show.time = TRUE),
               displaylabels = FALSE,
               plot.par = list(bg="white", mar=c(0,0,0,0), main=paste0("Querelle des collèges, ", trunc(start/10), "-", trunc(end/10))),
               vertex.tooltip = function(slice) {slice %v% 'Actor_text'},
               edge.tooltip = function(slice){slice %e% 'Tie_name'},
               edge.col = "edge_colour",
               vertex.border="#ffffff",
               vertex.col = "vertex_colour",
               xlab = year_label,
               vertex.cex = node_size,
               usearrows=TRUE,
               d3.options = list(animationDuration=800, debugFrameInfo=TRUE, durationControl=TRUE, margin=list(x=0,y=10), enterExitAnimationFactor=0.1),
               output.mode = 'HTML', launchBrowser=TRUE, filename=filename,
               verbose=TRUE)

