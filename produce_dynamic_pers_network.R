##########################################
##### Produce dynamic person network #####
##########################################


##### ____Create Network Dynamic Object for Person network #####

# Import static network creation scripts

source(paste0(Data_path, "create_static_text_net.R"))
source(paste0(Data_path, "create_static_pers_net.R"))

# Create vertex spell

QDC_vs <- create_vertex_spells(main_df = QDC, node_attr_df = QDC_pers_nodes,
                               actor_colname = "ACTOR-PERSON", final_actor_colname = "Actor_pers",
                               alter_colname = "TIE-PERSON")

QDC_for_pers_dyn <- QDC
QDC_for_pers_dyn$`TIE-TEXT`[
  is.na(QDC_for_pers_dyn$`TIE-TEXT`) & !(is.na(QDC_for_pers_dyn$`TIE-PERSON`))] <- QDC_for_pers_dyn$`TIE-PERSON`[
    is.na(QDC_for_pers_dyn$`TIE-TEXT`) & !(is.na(QDC_for_pers_dyn$`TIE-PERSON`))] 

## Create edge spell
## This is tricky: If I just copy and paste, without meaningful modification, the QDC_text procedure, then all ties of an actor in a given period will just enter at once, at the first time point. This makes no sense.
## Instead, the solution is to construct the edge spell for QDC_text, and then match the actor/tie texts with the corresponding persons.
## PROBLEM with using QDC_text as constructed previously: In rare cases, an actor text refers to persons WITHOUT refering to a text (e.g. Thi?bault - look him up). Those cases should not be included in the 'normal' actor-text database, but SHOULD be included in the actor person database
## SOLUTION: Re-construct QDC_text while inputting tie-persons that dont have a tie-text value in the tie-text column
## NOTE : You can't just artificially insert those special cases because the order of entries in the original QDC database is important.

# First, main QDC texts

QDC_62_89_bis <- QDC_for_pers_dyn[which(QDC_for_pers_dyn$Date>1761),]
QDC_text_for_pers_net <- subset(QDC_62_89_bis, select=c("ACTOR-TEXT", "TIE-TEXT", "Quality", "Date", "order"))
QDC_text_for_pers_net <- QDC_text_for_pers_net[!is.na(QDC_text_for_pers_net$`ACTOR-TEXT`),]
QDC_text_for_pers_net <- QDC_text_for_pers_net[!is.na(QDC_text_for_pers_net$`TIE-TEXT`),]
QDC_text_for_pers_net <- QDC_text_for_pers_net[!is.na(QDC_text_for_pers_net$Quality),]

### negative 'Quality' values screw with the network package. Let's just make all Quality values positive
QDC_text_for_pers_net$Quality <- QDC_text_for_pers_net$Quality + 3
QDC_text_for_pers_net$Quality[QDC_text_for_pers_net$Quality>6] <- QDC_text_for_pers_net$Quality[QDC_text_for_pers_net$Quality>6]-1

### Add 'responses' in the querelle as ties - first Response-text 1, then Response-text 2

QDC_text_resp <- QDC_62_89[, c("ACTOR-TEXT", "Response-TEXT 1", "Date", "order")]
QDC_text_resp <- QDC_text_resp[!is.na(QDC_text_resp$`Response-TEXT 1`),]
QDC_text_resp <- QDC_text_resp[!duplicated(QDC_text_resp[,c("ACTOR-TEXT", "Response-TEXT 1", "Date")]),]

QDC_text_resp_2 <- QDC_62_89[, c("ACTOR-TEXT", "Does it respond to a SECOND catalyst? If so, which? Response-TEXT 2", "Date", "order")]
QDC_text_resp_2 <- QDC_text_resp_2[!is.na(QDC_text_resp_2$`Does it respond to a SECOND catalyst? If so, which? Response-TEXT 2`),]
QDC_text_resp_2 <- QDC_text_resp_2[!duplicated(QDC_text_resp_2[,c("ACTOR-TEXT", "Does it respond to a SECOND catalyst? If so, which? Response-TEXT 2", "Date")]),]

colnames(QDC_text_resp) <- c("ACTOR-TEXT", "TIE-TEXT", "Date", "order")
colnames(QDC_text_resp_2) <- colnames(QDC_text_resp)

QDC_text_resp <- rbind(QDC_text_resp, QDC_text_resp_2)

QDC_text_resp[,"Quality"] <- 9

QDC_text_for_pers_net <- rbind(QDC_text_for_pers_net, QDC_text_resp)
rm(QDC_text_resp_2, QDC_text_resp)

QDC_text_for_pers_net <- QDC_text_for_pers_net[order(QDC_text_for_pers_net$order),]

QDC_text_for_pers_net$Qual_col <- c("red", "red", "grey61", "chartreuse3", "chartreuse3", "orange", "grey61", "grey61", "gray15")[QDC_text_for_pers_net$Quality]

# Second, pre-QdC texts, with edge dates remaining pre-62
# NOTE: Edges for pre-QdC texts should change colour whenever they are first brought in during the QdC
# Similarly, in network stats, their edge onsets should be set to that value

QDC_pre_62_edges <- extract_pre_qdc_edges(main_df = QDC_for_pers_dyn,
                                          actor_colname = "ACTOR-TEXT", alter_colname = "TIE-TEXT",
                                          order_colname = "order")

# Now, merge the various datasets and create dynamic edge attributes


QDC_text_for_pers_net <- rbind(QDC_pre_62_edges, QDC_text_for_pers_net)

QDC_vs_text_dyn <- create_vertex_spells(main_df = QDC, node_attr_df = QDC_text_nodes,
                                        actor_colname = "ACTOR-TEXT", final_actor_colname = "Actor_text",
                                        alter_colname = "TIE-TEXT")


# TODO: check whether empty Tie_code here is an issue later on

QDC_es <- QDC_es_transforms(es_df = QDC_text_for_pers_net, vs_df = QDC_vs_text_dyn,
                            actor_colname = "ACTOR-TEXT", alter_colname = "TIE-TEXT",
                            vs_actor_colname = "Actor_text", order_colname = "order")


# Also transform QDC_pre_62_edges to create colour changes

QDC_pre_62_edges <- QDC_es_transforms(es_df = QDC_pre_62_edges, vs_df = QDC_vs_text_dyn,
                                      actor_colname = "ACTOR-TEXT", alter_colname = "TIE-TEXT",
                                      vs_actor_colname = "Actor_text", order_colname = "order")



## NOW turn actor-texts into person-texts - Note: this could be simplified if I just used a node attribute file that combined texts with persons
## WITHOUT such a node attribute file, have to create a table of all ACTOR-TEXT that each ACTOR-PERSON has and re-input tie-persons that have no tie-text value. 
## To do that, just rbind a file with actor-persons in both the actor-text and actor-person columns

text_to_person_mapper <- unique(QDC[, c("ACTOR-TEXT", "ACTOR-PERSON")])
pers_to_person_mapper <- text_to_person_mapper
pers_to_person_mapper$`ACTOR-TEXT` <- pers_to_person_mapper$`ACTOR-PERSON`
pers_to_person_mapper <- unique(pers_to_person_mapper)
text_to_person_mapper <- rbind(text_to_person_mapper, pers_to_person_mapper)

colnames(text_to_person_mapper) <- c("ACTOR-TEXT", "ACTOR-PERSON")
QDC_es$`TIE-PERSON` <- text_to_person_mapper$`ACTOR-PERSON`[match(unlist(QDC_es$`TIE-TEXT`), text_to_person_mapper$`ACTOR-TEXT`)]
QDC_es$`ACTOR-PERSON` <- text_to_person_mapper$`ACTOR-PERSON`[match(unlist(QDC_es$`ACTOR-TEXT`), text_to_person_mapper$`ACTOR-TEXT`)]
QDC_es[, "Actor_code"] <- QDC_vs$Actor_code[match(unlist(QDC_es$`ACTOR-PERSON`), QDC_vs$Actor_pers)]
QDC_es[, "Tie_code"] <- QDC_vs$Actor_code[match(unlist(QDC_es$`TIE-PERSON`), QDC_vs$Actor_pers)]
QDC_es <- QDC_es[, c("onset","terminus","Actor_code","Tie_code","ACTOR-PERSON","TIE-PERSON","ACTOR-TEXT","TIE-TEXT", "Tie_name", "Quality", "Qual_col", "sent_to_rousseau", "order")]

QDC_pre_62_edges$`TIE-PERSON` <- text_to_person_mapper$`ACTOR-PERSON`[match(unlist(QDC_pre_62_edges$`TIE-TEXT`), text_to_person_mapper$`ACTOR-TEXT`)]
QDC_pre_62_edges$`ACTOR-PERSON` <- text_to_person_mapper$`ACTOR-PERSON`[match(unlist(QDC_pre_62_edges$`ACTOR-TEXT`), text_to_person_mapper$`ACTOR-TEXT`)]
QDC_pre_62_edges[, "Actor_code"] <- QDC_vs$Actor_code[match(unlist(QDC_pre_62_edges$`ACTOR-PERSON`), QDC_vs$Actor_pers)]
QDC_pre_62_edges[, "Tie_code"] <- QDC_vs$Actor_code[match(unlist(QDC_pre_62_edges$`TIE-PERSON`), QDC_vs$Actor_pers)]
QDC_pre_62_edges <- QDC_pre_62_edges[, c("onset","terminus","Actor_code","Tie_code","ACTOR-PERSON","TIE-PERSON","ACTOR-TEXT","TIE-TEXT", "Tie_name", "Quality", "Qual_col", "sent_to_rousseau", "order")]


# Try using the mapper on QDC_vs_text_dyn

QDC_vs_pers_dyn <- QDC_vs_text_dyn
QDC_vs_pers_dyn$Actor_pers <- text_to_person_mapper$`ACTOR-PERSON`[match(unlist(QDC_vs_pers_dyn$Actor_text), text_to_person_mapper$`ACTOR-TEXT`)]
QDC_vs_pers_dyn$Actor_code <- QDC_vs$Actor_code[match(unlist(QDC_vs_pers_dyn$Actor_pers), QDC_vs$Actor_pers)]
QDC_vs_pers_dyn <- QDC_vs_pers_dyn[order(QDC_vs_pers_dyn$onset, QDC_vs_pers_dyn$order_of_entry),]
QDC_vs_pers_dyn <- QDC_vs_pers_dyn[!duplicated(QDC_vs_pers_dyn$Actor_code),]

rm(text_to_person_mapper)


# Need to replace QdC_vs with a text version for now

QDC_vs <- QDC_vs_pers_dyn

#### Create weight attribute for ties

edge_weights_df <- QDC_es
edge_weights_df[,"num"] <- 1:nrow(edge_weights_df)
edge_weights_df <- edge_weights_df[order(edge_weights_df$Actor_code, edge_weights_df$Tie_code, edge_weights_df$onset),]
edge_weights_df[,"ego_alter"] <- paste0(edge_weights_df$Actor_code, "_", edge_weights_df$Tie_code)
edge_weights_df$edge_weights <- 1

for (rownum in 2:nrow(edge_weights_df)) {
  #TODO: Make the following condition work with years, not just slices
  if (edge_weights_df$ego_alter[rownum]==edge_weights_df$ego_alter[rownum-1]) {
    edge_weights_df$edge_weights[rownum] <- edge_weights_df$edge_weights[rownum-1] + 1
  }
}

edge_weights_df <- edge_weights_df[order(edge_weights_df$num),]

## Attach edge weights back onto QDC_es, and set pre-QDC edge weights to 1
QDC_es$edge_weights <- edge_weights_df$edge_weights
QDC_pre_62_edges$edge_weights <- 1


## Create vertex and edge spells for visual

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

QDC_pers_dyn <- networkDynamic(vertex.spells = QDC_vs_dynamic_vis, edge.spells = QDC_es_dynamic_vis[,c("onset", "terminus", "Actor_code", "Tie_code", "Qual_col")], create.TEAs = TRUE)

#vertex and edge attributes

QDC_pers_attr_dyn <- QDC_pers_nodes[order(QDC_pers_attr$Pers_Name),]

for (col in colnames(QDC_pers_attr_dyn)) {
  QDC_pers_dyn %v% col <- QDC_pers_attr_dyn[[col]]
}
QDC_pers_dyn %v% "vertex.names" <- QDC_pers_attr_dyn$Pers_Name

# static vertex attributes

QDC_pers_attr_dyn <- create_static_vertex_attr_df(vs_df = QDC_vs_dynamic_vis, 
                                                  node_attr_df = QDC_pers_nodes,
                                                  Text_or_pers_name = "Pers_Name",
                                                  actor_colname = "Actor_pers")

QDC_pers_attr_dyn$onset <- QDC_vs_dynamic_vis$onset[
  match(unlist(QDC_pers_attr_dyn$Pers_Name), QDC_vs_dynamic_vis$Actor_pers)]

for (col in colnames(QDC_pers_attr_dyn)) {
  QDC_pers_dyn %v% col <- QDC_pers_attr_dyn[[col]]
}

QDC_pers_dyn %v% "onset" <- QDC_vs_dynamic_vis$onset

# dynamic vertex attribute

QDC_vs_dynamic_vis_attr <- create_dyn_vertex_attr_df(vs_df = QDC_vs_dynamic_vis, es_df = QDC_es,
                                                     attr_df = QDC_pers_attr_dyn,
                                                     Text_or_pers_name = "Pers_Name",
                                                     actor_colname = "Actor_pers")


##### replace special characters with HTML codes

chars <- import(paste0(Data_path, "HTML_codes_French_characters.xlsx"))

## For loop: For every French character in the first column of the dataframe "chars", replace it with the HTML code in the third column of the dataframe
for (char in chars[,1]) {
  QDC_vs_dynamic_vis_attr$Actor_pers <- gsub(char, chars[which(chars$Character==char),"R_HTML"], QDC_vs_dynamic_vis_attr$Actor_pers)
}; rm(char)


# loop over vertex data to add the dynamic attributes on the vertices
for(row in 1:nrow(QDC_vs_dynamic_vis_attr)){
  activate.vertex.attribute(x = QDC_pers_dyn, prefix = 'vertex_colour',
                            value = QDC_vs_dynamic_vis_attr$vertex_colour[row],
                            onset=QDC_vs_dynamic_vis_attr$onset[row],terminus=QDC_vs_dynamic_vis_attr$terminus[row],
                            v=QDC_vs_dynamic_vis_attr$Actor_code[row])
  activate.vertex.attribute(x = QDC_pers_dyn, prefix = 'Actor_pers_name',
                            value = QDC_vs_dynamic_vis_attr$Actor_pers[row],
                            onset=QDC_vs_dynamic_vis_attr$onset[row],terminus=QDC_vs_dynamic_vis_attr$terminus[row],
                            v=QDC_vs_dynamic_vis_attr$Actor_code[row])
}

# Need to create vertex attribute to take into account the fact that nodes send or receive multiple ties

# Create initial dataset (keeping only cases where edge weights are greater than 1, and the greatest edge weights in each slice)
n_multiple_ties_df_senders <- QDC_es_dynamic_vis[, c("onset", "terminus", "Actor_code", "Tie_code", "edge_weights", "ACTOR-PERSON", "TIE-PERSON")]
n_multiple_ties_df_receivers <- QDC_es_dynamic_vis[, c("onset", "terminus", "Tie_code", "Actor_code", "edge_weights", "TIE-PERSON", "ACTOR-PERSON")]
colnames(n_multiple_ties_df_receivers) <- c("onset", "terminus", "Actor_code", "Tie_code", "edge_weights", "ACTOR-PERSON", "TIE-PERSON")
n_multiple_ties_df <- rbind(n_multiple_ties_df_senders, n_multiple_ties_df_receivers)
n_multiple_ties_df <- n_multiple_ties_df[n_multiple_ties_df$edge_weights!=1,]
n_multiple_ties_df <- n_multiple_ties_df[!duplicated(n_multiple_ties_df, fromLast = TRUE),]
n_multiple_ties_df <- n_multiple_ties_df[!duplicated(n_multiple_ties_df[,c("onset", "terminus", "Actor_code", "Tie_code", "ACTOR-PERSON", "TIE-PERSON")], fromLast = TRUE),]
n_multiple_ties_df <- n_multiple_ties_df[order(n_multiple_ties_df$Actor_code, n_multiple_ties_df$onset, n_multiple_ties_df$Tie_code),]

# substract 1 from edge weights (we don't want to count the first edge between ego and alter) before splitting df into multiple df, one per actor

n_multiple_ties_df$edge_weights <- n_multiple_ties_df$edge_weights-1
n_multiple_ties_list <- split(n_multiple_ties_df, f = n_multiple_ties_df$Actor_code)

# for each actor, sum the edge weights of each distinct (non-overlapping) edge as these edges come in
for (i in seq_along(n_multiple_ties_list)) {
  list_df <- n_multiple_ties_list[[i]]
  list_df2 <- n_multiple_ties_list[[i]]
  
  if(nrow(list_df)==1) {
    next
  }
  for(rownum in 2:nrow(list_df)) {
    non_overlapping_edges <- list_df[1:rownum-1,]
    non_overlapping_edges <- non_overlapping_edges[!duplicated(non_overlapping_edges[,c("terminus", "Actor_code", "Tie_code", "ACTOR-PERSON", "TIE-PERSON")], fromLast = TRUE),]
    non_overlapping_edges <- non_overlapping_edges[non_overlapping_edges$Tie_code!=list_df$Tie_code[rownum],]
    non_overlapping_edge_weights <- non_overlapping_edges$edge_weights
    list_df2$edge_weights[rownum] <- sum(list_df$edge_weights[rownum], non_overlapping_edge_weights)
  }
  n_multiple_ties_list[[i]] <- list_df2
}

# Clean, format dataset, add initial node_edge_weights of 0 for each node
rm(list_df, list_df2, non_overlapping_edges, non_overlapping_edge_weights)
n_multiple_ties_df <- data.table::rbindlist(n_multiple_ties_list)
n_multiple_ties_df <- n_multiple_ties_df[, c("onset", "terminus", "Actor_code", "edge_weights")]
colnames(n_multiple_ties_df)[colnames(n_multiple_ties_df)=="edge_weights"] <- "node_edge_weights"

n_multiple_ties_df_initial <- QDC_vs_dynamic_vis[, c("onset", "terminus", "Actor_code")]
n_multiple_ties_df_initial$node_edge_weights <- 0
n_multiple_ties_df_initial$onset <- min(QDC_vs_dynamic_vis$onset)

n_multiple_ties_df <- rbind(n_multiple_ties_df_initial, n_multiple_ties_df)

# remove cases where multiple n_multiple_ties values appear in a slice - keep the largest value
n_multiple_ties_df <- n_multiple_ties_df[order(n_multiple_ties_df$Actor_code, n_multiple_ties_df$onset, n_multiple_ties_df$node_edge_weights),]
n_multiple_ties_df <- n_multiple_ties_df[!duplicated(n_multiple_ties_df
                                                     [, c("onset", "terminus", "Actor_code")], fromLast = TRUE),]


# Now add var as vertex attribute
for(row in 1:nrow(n_multiple_ties_df)){
  activate.vertex.attribute(x = QDC_pers_dyn, prefix = 'node_edge_weights',
                            value = (n_multiple_ties_df$node_edge_weights[row]),
                            onset=n_multiple_ties_df$onset[row],terminus=n_multiple_ties_df$terminus[row],
                            v=n_multiple_ties_df$Actor_code[row])
}



# Add Dynamic edge attributes


#### Create dynamic edge attribute for the texts that are involved in a tie

QDC_es_dynamic_vis[,"Actor_tie"] <- paste0(QDC_es_dynamic_vis$`ACTOR-PERSON`, "  &#8594 ", QDC_es_dynamic_vis$`TIE-PERSON`)
QDC_es_dynamic_vis[,"num"] <- 1:nrow(QDC_es_dynamic_vis)
QDC_es_dynamic_vis <- QDC_es_dynamic_vis[order(QDC_es_dynamic_vis$Actor_tie, QDC_es_dynamic_vis$onset),]
QDC_es_dynamic_vis[,"Tie_name_dyn"] <- QDC_es_dynamic_vis$Tie_name

for (rownum in 2:nrow(QDC_es_dynamic_vis)) {
  #TODO: Make the following condition work with years, not slices
  if (QDC_es_dynamic_vis$Actor_tie[rownum]==QDC_es_dynamic_vis$Actor_tie[rownum-1] & QDC_es_dynamic_vis$onset[rownum-1]>17619) {
    QDC_es_dynamic_vis$Tie_name_dyn[rownum] <- paste0(QDC_es_dynamic_vis$Tie_name_dyn[rownum-1], "; ", QDC_es_dynamic_vis$Tie_name[rownum])
  }
}

QDC_es_dynamic_vis <- QDC_es_dynamic_vis[order(QDC_es_dynamic_vis$num),]
QDC_es_dynamic_vis$num=NULL

# loop over edge data to add the dynamic attributes on the edge
for(row in 1:nrow(QDC_es_dynamic_vis)){
  edge_id <- get.edgeIDs(QDC_pers_dyn,v=QDC_es_dynamic_vis$Actor_code[row],
                         alter=QDC_es_dynamic_vis$Tie_code[row])
  activate.edge.attribute(QDC_pers_dyn,'edge_colour',QDC_es_dynamic_vis$Qual_col[row],
                          onset=QDC_es_dynamic_vis$onset[row],terminus=QDC_es_dynamic_vis$terminus[row],e=edge_id)
  activate.edge.attribute(QDC_pers_dyn,'Tie_name_dyn',QDC_es_dynamic_vis$Tie_name_dyn[row],
                          onset=QDC_es_dynamic_vis$onset[row],terminus=QDC_es_dynamic_vis$terminus[row],e=edge_id)
  activate.edge.attribute(QDC_pers_dyn,'edge_weights',QDC_es_dynamic_vis$edge_weights[row],
                          onset=QDC_es_dynamic_vis$onset[row],terminus=QDC_es_dynamic_vis$terminus[row],e=edge_id)
  
}



### Make animation

## Trying to make it less bunched

QDC_pers_anim <- compute.animation(QDC_pers_dyn, slice.par=list(start=start_slice, end=end_slice, interval=slice_interval, aggregate.dur=0, rule="any"), animation.mode = "kamadakawai", chain.direction = "reverse", default.dist = 6, verbose = TRUE)

#QDC_pers_anim_final <- compute.animation(QDC_pers_dyn, slice.par=list(start=end_slice, end=end_slice, interval=slice_interval, aggregate.dur=0, rule="any"), animation.mode = "kamadakawai", chain.direction = "reverse", default.dist = 6, verbose = TRUE)

#QDC_pers_anim <- compute.animation(QDC_pers_dyn, slice.par=list(start=start_slice, end=end_slice, interval=slice_interval, aggregate.dur=0, rule="any"), animation.mode = "kamadakawai", seed.coords = matrix(data = c(get.vertex.attribute.active(QDC_pers_anim_final, "animation.x", at = end_slice), get.vertex.attribute.active(QDC_pers_anim_final, "animation.y", at = end_slice)), ncol = 2), chain.direction = "reverse", default.dist = 6, verbose = TRUE)

QDC_pers_anim2 <- QDC_pers_dyn

for (i in seq(from = start_slice,to = end_slice+1, by=1)) {
  activate.vertex.attribute(QDC_pers_anim2, "animation.x", at = i, value = (get.vertex.attribute.active(QDC_pers_anim, "animation.x", at = i))/3.2)
}

for (i in seq(from = start_slice,to = end_slice+1, by=1)) {
  activate.vertex.attribute(QDC_pers_anim2, "animation.y", at = i, value = (get.vertex.attribute.active(QDC_pers_anim, "animation.y", at = i))/3.2)
}

year_label <- function(s){
  if (QDC_pers_anim2$gal$slice.par$start < 17620 & QDC_pers_anim2$gal$slice.par$start > 10000 & 
      s==1) {
    "Pre-1762"
  } else {
    paste(trunc((QDC_pers_anim2$gal$slice.par$start+
                   QDC_pers_anim2$gal$slice.par$interval*s-1)/10))
  }
}

# Set node size in person net visuals - dynamic and static

node_size <- function(slice, network_or_igraph = "network"){
  if (network_or_igraph=="network") {
    degree_value <- sna::degree(slice, cmode = "freeman") + (slice %v% "node_edge_weights") 
  } else if (network_or_igraph=="igraph") {
    degree_value <- igraph::degree(slice, mode = "all", loops = TRUE) + vertex_attr(slice_to_plot, "node_edge_weights") 
  }
  return(
    (10*(degree_value + 0.000001)/
       (degree_value + 0.000001)*
       (log(((degree_value+5)/100)+1)))
  )
}

filename <- "QDC_pers_with_pre_QDC_ties_corrected_2024_06_13_testing.html"

## Creating ndtv visual without base network specified

render.d3movie(QDC_pers_anim2, render.par=list(tween.frames=50, show.time = TRUE), displaylabels = FALSE,
               plot.par = list(bg="white",
                               vertex.border="#ffffff",
                               vertex.col = "vertex_colour",
                               main="Querelle des collèges, 1762-1789",
                               xlab = year_label,
                               vertex.cex = node_size,
                               edge.lwd = function(slice){(log(slice %e% 'edge_weights')*2)+1},
                               vertex.tooltip = function(slice){paste0(slice %v% 'Actor_pers_name')}, 
                               edge.tooltip = function(slice){slice %e% 'Tie_name_dyn'},
                               edge.col = "edge_colour", usearrows=TRUE),
               d3.options = list(animationDuration=800, debugFrameInfo=FALSE, durationControl=TRUE, margin=list(x=0,y=10), enterExitAnimationFactor=0.1),
               launchBrowser=TRUE, filename=filename,
               verbose=TRUE)




