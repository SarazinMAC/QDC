library(intergraph)
library(rio)
library(sna)
library(statnet.common)
library(network)
library(ndtv)
library(networkDynamic)
library(dplyr)


#detach("package:ndtv", unload=TRUE)
#detach("package:networkDynamic", unload=TRUE)
#library(ndtv)
#library(networkDynamic)

# Configurable values

#Data_path <- "C:\\Users\\sarazinm\\Documents\\Gen\\Gemma\\"
Data_name <- "QDC_2024_06_07.xlsx"
Data_path <- "D:\\Git_Repos\\QDC\\"

# Do you want to use slices (i.e. years * 10) or original years in Dynamic network?

slice_or_year <- "slice"

# import dataset

QDC_file <- import(paste0(Data_path, Data_name))

# Custom functions ------------------

# Assign edge spell values of Pre-QdC actors to first onset at which the actors receive a tie

# Actually two functions, one nested in another, because some pre-QDC actors refer to each other

update_edge_spells <- function(spells, spells_to_update, spells_for_updating) {
  onsets <- data.frame(c(spells_for_updating$Actor_code, spells_for_updating$Tie_code), c(spells_for_updating$onset, spells_for_updating$onset))
  onsets <- onsets[complete.cases(onsets),]
  colnames(onsets) <- c("Actor_code", "onsets")
  onsets_for_spells_to_update <- onsets[onsets$Actor_code %in% spells_to_update$Actor_code,]
  onsets_for_spells_to_update <- onsets_for_spells_to_update[order(onsets_for_spells_to_update$Actor_code, onsets_for_spells_to_update$onsets),]
  onsets_for_spells_to_update <- onsets_for_spells_to_update[!duplicated(onsets_for_spells_to_update$Actor_code),]
  spells <- dplyr::left_join(spells, onsets_for_spells_to_update, by = "Actor_code")
  spells$onset[spells$spells_to_update==1] <- spells$onsets[spells$spells_to_update==1]
  spells$onsets=NULL
  return(spells)
}

assign_pre_QDC_edge_onsets <- function(edge_spells = QDC_es, .start_slice = start_slice) {
  #TODO: Insert while loop at the end to keep running the process until edge spells have stopped updating
  edge_spells$spells_to_update <- (edge_spells$onset<.start_slice)*1
  pre_qdc_spells <- edge_spells[edge_spells$spells_to_update==1,]
  qdc_spells <- edge_spells[edge_spells$spells_to_update==0,]
  edge_spells <- update_edge_spells(spells = edge_spells, spells_to_update = pre_qdc_spells, spells_for_updating = qdc_spells)
  edge_spells2 <- update_edge_spells(spells = edge_spells, spells_to_update = pre_qdc_spells, spells_for_updating = edge_spells)
  edge_spells3 <- update_edge_spells(spells = edge_spells2, spells_to_update = pre_qdc_spells, spells_for_updating = edge_spells2)
  edge_spells4 <- update_edge_spells(spells = edge_spells3, spells_to_update = pre_qdc_spells, spells_for_updating = edge_spells3)
  edge_spells4$spells_to_update=NULL
  return(edge_spells4)
}

# turn years into slices (where years are divided according to the number of actors in each year)
# Actors (note, not ties) will come in at equal intervals within each year

turn_years_into_slices <- function(df, actor_colname, year_colname = "Date",
                                   order_colname = "order", round = TRUE) {
  #TODO: Change round argument to a floor or truncate argument??
  formatted_df <- df[,c(actor_colname, year_colname, order_colname)]
  formatted_df <- formatted_df[!duplicated(formatted_df[,c(actor_colname, year_colname)]),]
  formatted_df <- formatted_df[order(formatted_df[[order_colname]]),]
  formatted_df[[year_colname]] <- as.numeric(as.character(formatted_df[[year_colname]]))
  
  # Extract the number of actors in each year
  n_actors_per_year <- table(formatted_df[[year_colname]])
  
  # Now split year: actors will come in at equal intervals within the year
  # Store slices in formatted_df
  # the last slice of the year = year + 1 - the interval within the year
  min_year <- min(formatted_df[[year_colname]])
  max_year <- max(formatted_df[[year_colname]])
  years <- names(n_actors_per_year) #note: the names of the object are the years in formatted_df
  for (i in (min_year:max_year)) {
    if (i %in% years) {
      year_slices <- seq(from = i, 
                         to = i + (1 - (1/n_actors_per_year[years==i])), 
                         length.out = n_actors_per_year[years==i])
      formatted_df[[year_colname]][formatted_df[[year_colname]]==i] <- year_slices
    } else {
      next
    }
  }
  # If relevant, round to one decimal point and Multiply all years by 10
  if (round == TRUE) {
    formatted_df[[year_colname]] <- round(formatted_df[[year_colname]], 1)*10
  }
  # Attach the new slices onto the original df
  df[[year_colname]] <- formatted_df[[year_colname]][match(
    unlist(df[[actor_colname]]), formatted_df[[actor_colname]])]
  return(df)
}

# Create vertex spells - for dynamic visuals

create_vertex_spells <- function(main_df, node_attr_df,
                                 actor_colname, final_actor_colname, alter_colname,
                                 start_year = 1762, end_year = 1789,
                                 .slice_or_year = slice_or_year,
                                 year_colname = "Date", order_colname = "order") {
  
  QDC_start_end <- main_df[main_df[[year_colname]] >= start_year & main_df[[year_colname]] <= end_year,]
  if (.slice_or_year == "slice")  {
    QDC_start_end <- turn_years_into_slices(df = QDC_start_end, actor_colname = actor_colname, year_colname = year_colname,
                                             order_colname = order_colname, round = TRUE)
    # Or Set onset manually for testing
    #QDC_start_end[[year_colname]] <- QDC_start_end[[year_colname]]*10
  }
  QDC_start_end <- QDC_start_end[,c(actor_colname,alter_colname,year_colname, order_colname)]
  QDC_62_89_inversed <- QDC_start_end[,c(alter_colname,actor_colname,year_colname, order_colname)]
  colnames(QDC_62_89_inversed) <- colnames(QDC_start_end)
  
  # Second, pre-QDC actors (whose vertex onsets (Dates) should now remain pre-1762)
  
  QDC_pre_62_nodes <- main_df[main_df[[year_colname]]<start_year,]
  QDC_pre_62_nodes <- subset(QDC_pre_62_nodes, select=c(actor_colname, alter_colname, year_colname, order_colname))
  QDC_pre_62_nodes <- QDC_pre_62_nodes[!is.na(QDC_pre_62_nodes[[actor_colname]]),]
  
  if (.slice_or_year == "slice") {
    QDC_pre_62_nodes[[year_colname]] <- QDC_pre_62_nodes[[year_colname]]*10
  }
  
  QDC_all <- rbind(QDC_pre_62_nodes, QDC_start_end, QDC_62_89_inversed)
  QDC_vs <- unique(QDC_all[, c(actor_colname, year_colname, order_colname)])
  QDC_vs <- QDC_vs[!is.na(QDC_vs[[actor_colname]]),]
  QDC_vs <- QDC_vs[order(QDC_vs[[year_colname]], QDC_vs[[order_colname]]),]
  QDC_vs <- QDC_vs[!duplicated(QDC_vs[[actor_colname]]),]
  colnames(QDC_vs) <- c(final_actor_colname, "onset","order_of_entry")
  QDC_vs$onset <- as.numeric(as.character(QDC_vs$onset))
  QDC_vs[,"terminus"] <- (max(QDC_vs$onset)+1) # set maximum to year/slice + 1 - in most live uses, this should return 1790/17900 
  QDC_vs[, "Actor_code"] <- 1:nrow(QDC_vs)
  QDC_vs <- QDC_vs[,c("onset","terminus","Actor_code",final_actor_colname, "order_of_entry")]
  QDC_vs[[final_actor_colname]] <- as.character(QDC_vs[[final_actor_colname]])
  #QDC_vs[, "Actor_label"] <- ifelse(QDC_vs[[final_actor_colname]]=="D'Alembert (1753)"| QDC_vs[[final_actor_colname]]=="La Chalotais (1763)"| QDC_vs[[final_actor_colname]]=="Rousseau (1762)", QDC_vs[[final_actor_colname]], "")
  #TODO: Make this condition - and other bits of codess - less convoluted (e.g. by simplifying column names and/or having general text or pers arguments)
  if (final_actor_colname == "Actor_text") {
    QDC_vs[,"Corpus_num"] <- node_attr_df$Corpus_num[match(unlist(QDC_vs[[final_actor_colname]]), node_attr_df$Text_Name)]
  }
  return(QDC_vs)
}

# Custom function for extracting pre QDC edges

extract_pre_qdc_edges <- function(main_df, start_year = 1762,
                                  actor_colname, alter_colname, order_colname = NULL) {
  
  QDC_pre_62_edges <- main_df[main_df$Date<1762,]
  if (!is.null(order_colname)) {
    QDC_pre_62_edges <- subset(QDC_pre_62_edges, select=c(actor_colname, alter_colname, "Quality", "Date", order_colname))
  } else {
    QDC_pre_62_edges <- subset(QDC_pre_62_edges, select=c(actor_colname, alter_colname, "Quality", "Date"))
  }
  QDC_pre_62_edges <- QDC_pre_62_edges[!is.na(QDC_pre_62_edges[[actor_colname]]),]
  QDC_pre_62_edges <- QDC_pre_62_edges[!is.na(QDC_pre_62_edges[[alter_colname]]),]
  
  # negative 'Quality' values screw with the network package. Let's just make all Quality values positive
  QDC_pre_62_edges$Quality <- QDC_pre_62_edges$Quality + 3
  QDC_pre_62_edges$Quality[QDC_pre_62_edges$Quality>6] <- QDC_pre_62_edges$Quality[QDC_pre_62_edges$Quality>6]-1
  
  # Set edge colours here as pre-QdC edges have different colours to QdC edges
  
  pre_qdc_negative <- "grey90"
  pre_qdc_positive <- "grey90"
  pre_qdc_ambivalent <- "grey90"
  pre_qdc_neutral <- "grey90"
  QDC_pre_62_edges$Qual_col <- c(pre_qdc_negative, pre_qdc_negative, pre_qdc_neutral, pre_qdc_positive, pre_qdc_positive, pre_qdc_ambivalent, pre_qdc_neutral, pre_qdc_neutral, pre_qdc_neutral)[QDC_pre_62_edges$Quality]
  return(QDC_pre_62_edges)
}




# Custom function for creating QDC_es DVs and re-ordering/changing colnames of QDC_es

QDC_es_transforms <- function(es_df, vs_df, 
                              actor_colname, alter_colname, vs_actor_colname, order_colname = NULL) {
  
  # Set edge colours again - note the pre-QdC edge colours will be overwritten but we will add these back on later
#  es_df$Qual_col <- c("red", "red", "grey61", "chartreuse3", "chartreuse3", "orange", "grey61", "grey61", "gray15")[es_df$Quality]
  
  # Create DVs
  es_df[,"terminus"] <- unique(vs_df$terminus)
  es_df[, "Actor_code"] <- vs_df$Actor_code[match(unlist(es_df[[actor_colname]]), vs_df[[vs_actor_colname]])]
  es_df[, "Tie_code"] <- vs_df$Actor_code[match(unlist(es_df[[alter_colname]]), vs_df[[vs_actor_colname]])]
  es_df[, "Actor_Corpus_num"] <- vs_df$Corpus_num[match(unlist(es_df$Actor_code), vs_df$Actor_code)]
  es_df[, "Tie_Corpus_num"] <- vs_df$Corpus_num[match(unlist(es_df$Tie_code), vs_df$Actor_code)]
  es_df[, "Tie_name"] <- paste0(es_df$Actor_Corpus_num, " &#8594 ", es_df$Tie_Corpus_num) # Note 07.08.2022: this attribute doesn't have a purpose yet, but it may do one day.
  
  ## Create dynamic edge attribute: Is edge sent to Rousseau?
  
  rousseau_codes <- vs_df$Actor_code[grepl("Rousseau", vs_df[[vs_actor_colname]])] # note that I have given the object a plural name but am only expecting a single Rousseau code
  es_df$sent_to_rousseau <- (es_df$Tie_code %in% rousseau_codes)*1
  rm(rousseau_codes)
  
  # Change column order and Date column name
  if (!is.null(order_colname)) {
    es_df <- es_df[,c("Date","terminus","Actor_code","Tie_code", actor_colname, alter_colname, "Quality", "Tie_name", "Qual_col", "sent_to_rousseau", order_colname)]
  } else {
    es_df <- es_df[,c("Date","terminus","Actor_code","Tie_code", actor_colname, alter_colname, "Quality", "Tie_name", "Qual_col", "sent_to_rousseau")]
  }
  colnames(es_df)[colnames(es_df)=="Date"] <- "onset"
  
  # Update es_df onsets with vs_df onsets
  
  es_df$onset <- vs_df$onset[match(unlist(es_df$Actor_code), vs_df$Actor_code)]
  return(es_df)
}


# Create object to store static vertex attributes for dynamic net

create_static_vertex_attr_df <- function(vs_df, node_attr_df, Text_or_pers_name, actor_colname) {
  # re-creating a temporary order variable in vs_df in case the existing order variable is not in order
  vs_df$order <- 1:nrow(vs_df)
  node_attr_df$node_order_dynamic_vis <- vs_df$order[match(
    unlist(node_attr_df[[Text_or_pers_name]]), vs_df[[actor_colname]])]
  node_attr_df$order_of_entry <- vs_df$order_of_entry[match(
    unlist(node_attr_df[[Text_or_pers_name]]), vs_df[[actor_colname]])]
  
  QDC_static_attr_dyn <- node_attr_df[order(node_attr_df$node_order_dynamic_vis),]
  return(QDC_static_attr_dyn)
}


# Create object to store dynamic vertex attributes

create_dyn_vertex_attr_df <- function(vs_df, es_df, attr_df, Text_or_pers_name, actor_colname,
                                      .slice_or_year = slice_or_year, .start_slice = start_slice) {
  
  # import relevant variables into vs_df
  
  vs_df$Actor_type <- attr_df$Actor_type[match(
    unlist(vs_df[[actor_colname]]), attr_df[[Text_or_pers_name]])]
  
  vs_df$Type_col <- attr_df$Type_col[match(
    unlist(vs_df[[actor_colname]]), attr_df[[Text_or_pers_name]])]
  
  # add pre-QdC colours to vs_df for pre-QdC nodes
  
  if (slice_or_year=="slice") {
    pre_62_vs_df <- vs_df[vs_df$onset<17620,]
  } else if (slice_or_year=="year") {
    pre_62_vs_df <- vs_df[vs_df$onset<1762,]  
  }
  
  # Colour for pre-QdC people: #bcddfb
  # colour for pre-QDC institutions: #f5a4db
  # There are no pre-QdC periodicals, so keep gold for them.
  
  pre_62_vs_df$vertex_colour <- c("#BCDDFB", "#F5A4DB", "gold")[pre_62_vs_df$Actor_type]
  
  # set onset (year/slice) at which pre-QdC nodes achieve their final colour
  
  es_reduced <- es_df[c("onset", "Actor_code", "Tie_code")]
  es_reduced_inversed <- es_df[c("onset", "Tie_code", "Actor_code")]
  es_reduced_inversed <- es_reduced_inversed[es_reduced_inversed$onset>=17620,]
  colnames(es_reduced_inversed) <- colnames(es_reduced)
  es_combined <- rbind(es_reduced, es_reduced_inversed)
  
  onset_of_change <- assign_pre_QDC_edge_onsets(edge_spells = es_combined, .start_slice = .start_slice)
  
  onset_of_change <- onset_of_change[onset_of_change$Actor_code %in% pre_62_vs_df$Actor_code,
                                     c("onset", "Actor_code", "Tie_code")]
  #onset_of_change_inversed <- onset_of_change[, c("onset", "Tie_code", "Actor_code")]
  #colnames(onset_of_change_inversed) <- colnames(onset_of_change)
  
  #onset_of_change <- rbind(onset_of_change, onset_of_change_inversed) 
  
  onset_of_change <- onset_of_change[, c("Actor_code", "onset")]
  onset_of_change <- unique(onset_of_change)
  onset_of_change <- onset_of_change[order(onset_of_change$Actor_code, onset_of_change$onset),]
  onset_of_change <- onset_of_change[!duplicated(onset_of_change$Actor_code),]
  
  vs_df$onset_of_change <- onset_of_change$onset[match(
    unlist(vs_df$Actor_code), onset_of_change$Actor_code)]
  
  vs_df$onset[!is.na(vs_df$onset_of_change)] <- vs_df$onset_of_change[!is.na(vs_df$onset_of_change)]
  vs_df$onset_of_change=NULL
  
  vs_df$vertex_colour <- vs_df$Type_col
  vs_df <- rbind(pre_62_vs_df, vs_df)
  vs_df <- vs_df[!duplicated(
    vs_df[, c("onset", "Actor_code")]),]
  return(vs_df)
}

# TODO: Introduce if function to make code adaptable to text or pers network, and delete pers version below

year_label <- function(s){
  if (QDC_text_anim$gal$slice.par$start < 17620 & QDC_text_anim$gal$slice.par$start > 10000 & 
      s==1) {
    "Pre-1762"
  } else {
  paste(trunc((QDC_text_anim$gal$slice.par$start+
                 QDC_text_anim$gal$slice.par$interval*s-1)/10))
  }
}


# Clean dataset of empty rows

empty_rows <- apply(QDC_file, 1, function(x) {all(is.na(x))}) # records TRUE if the row is full of NAs
QDC <- QDC_file[!empty_rows,] # Only keep rows that return FALSE to the line above
QDC$order <- 1:nrow(QDC)


##### Creating datasets #####

## Restricting dataset to 1762 - 1789
QDC_62_89 <- QDC[which(QDC$Date>1761),]



#### __Text-based Querelle ####

#### ____Node database - for all potential text nodes (actor-texts, tie-texts, response texts) ####

# Two steps: Create a database of all nodal attribute information, then create a list of actors (that both send and receive ties) and populate with nodal attributes
## First create a single attribute record for every entry in Actor-text column (which normally includes all actors in tie text and response text)
QDC_text <- subset(QDC, select=
                     c("ACTOR-TEXT", "TIE-TEXT", "Quality", "Date", 
                       "Individual or collective of authors (1); Authority or institution (2); Periodical (3)", 
                       "Gender", "Response-TEXT 1", "Does it respond to a SECOND catalyst? If so, which? Response-TEXT 2",
                       "Place"))
QDC_text <- QDC_text[!is.na(QDC_text$`ACTOR-TEXT`),]


QDC_text_node_data <- as.data.frame(table(QDC_text$`ACTOR-TEXT`, QDC_text$`Individual or collective of authors (1); Authority or institution (2); Periodical (3)`, QDC_text$Gender, QDC_text$Date)) #This creates a problem when nodes have missing data for any attribute; if they do, then it gives missing values to all attributes
QDC_text_node_data <- QDC_text_node_data[QDC_text_node_data$Freq>0,1:4]
colnames(QDC_text_node_data) <- c("Text_Name", "Actor_type", "Gender", "Date")




## Now create list of actors that you want to use to populate the Node database
QDC_text_nodes <- as.data.frame(unique(append(QDC_text$`ACTOR-TEXT`, c(QDC_text$`TIE-TEXT`, QDC_text$`Response-TEXT 1`, QDC_text$`Does it respond to a SECOND catalyst? If so, which? Response-TEXT 2`))))
QDC_text_nodes <- data.frame(QDC_text_nodes[!is.na(QDC_text_nodes)])
colnames(QDC_text_nodes) <- "Actors"

## Now fill with attributes using matching
QDC_text_nodes$Actor_type <- QDC_text_node_data$Actor_type[match(unlist(QDC_text_nodes$Actors), QDC_text_node_data$Text_Name)]
QDC_text_nodes$Gender <- QDC_text_node_data$Gender[match(unlist(QDC_text_nodes$Actors), QDC_text_node_data$Text_Name)]
QDC_text_nodes$Date <- QDC_text_node_data$Date[match(unlist(QDC_text_nodes$Actors), QDC_text_node_data$Text_Name)]
QDC_text_nodes$Actor_type <- as.numeric(QDC_text_nodes$Actor_type)
QDC_text_nodes$Actor_type[which(is.na(QDC_text_nodes$Actor_type))] <- 99 #missing data coded 99
QDC_text_nodes$Gender <- as.numeric(QDC_text_nodes$Gender)
QDC_text_nodes$Gender[which(is.na(QDC_text_nodes$Gender))] <- 99 #missing data coded 99
QDC_text_nodes$Date <- as.numeric(as.character(QDC_text_nodes$Date))
QDC_text_nodes$Date[which(is.na(QDC_text_nodes$Date))] <- 99 #missing data coded 99
colnames(QDC_text_nodes) <- c("Text_Name", "Actor_type", "Gender", "Date")
QDC_text_nodes$Type_col <- c("#74B8F7", "#E831AE", "gold")[QDC_text_nodes$Actor_type]

QDC_text_nodes$Text_Name <- as.character(QDC_text_nodes$Text_Name)
QDC_text_nodes[,"Corpus_num"] <- as.numeric(row.names(QDC_text_nodes))
QDC_text_nodes <- QDC_text_nodes[order(QDC_text_nodes$Text_Name),]


#### ____Create network object ####

QDC_text <- subset(QDC_62_89, select=c("ACTOR-TEXT", "TIE-TEXT", "Quality", "Date", "order"))
QDC_text <- QDC_text[!is.na(QDC_text$`ACTOR-TEXT`),]
QDC_text <- QDC_text[!is.na(QDC_text$`TIE-TEXT`),]
QDC_text <- QDC_text[!is.na(QDC_text$Quality),]
QDC_text[,"Line_type"] <- "solid"

### negative 'Quality' values mess with the network package. Let's just make all Quality values positive
QDC_text$Quality <- QDC_text$Quality + 3
QDC_text$Quality[QDC_text$Quality>6] <- QDC_text$Quality[QDC_text$Quality>6]-1

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
QDC_text_resp[,"Line_type"] <- "dashed"

QDC_text <- rbind(QDC_text, QDC_text_resp)
rm(QDC_text_resp_2, QDC_text_resp)
QDC_text <- QDC_text[order(QDC_text$order),]

QDC_text_net <- network(QDC_text, matrix.type= "edgelist", loops=F, multiple=F, ignore.eval = F)
#QDC_text_net <- network(QDC_text, matrix.type= "edgelist", loops=F, multiple=F, ignore.eval = F)

### Creating node attr database - note, you need list of vertices to do this.

QDC_text_attr <- as.data.frame(QDC_text_net %v% "vertex.names")
colnames(QDC_text_attr) <- "Text_Name"
## Import Actor type
QDC_text_attr$Text_Name <- as.character(QDC_text_attr$Text_Name)
QDC_text_attr[,"Actor_type"] <- QDC_text_nodes$Actor_type[match(unlist(QDC_text_attr$Text_Name), QDC_text_nodes$Text_Name)]
QDC_text_attr$Type_col <- QDC_text_nodes$Type_col[match(unlist(QDC_text_attr$Text_Name), QDC_text_nodes$Text_Name)]

## Create a vertex attribute for the number of sides of the vertex - if the actor is an authority or institution (i.e. actor type 2), then a pentagon, otherwise a circle
QDC_text_attr[,"Vertex_sides"] <- ifelse(QDC_text_attr$Actor_type==2, 4, 50) # 50 means 50 sides, which for some reason ends up appearing as a circle
## Create label - only for D'Alembert (1753), La Chalotais (1763), and Rousseau (1762)
QDC_text_attr[, "Label"] <- ifelse(QDC_text_attr$Text_Name=="D'Alembert (1753)"|QDC_text_attr$Text_Name=="La Chalotais (1763)"|QDC_text_attr$Text_Name=="Rousseau (1762)", QDC_text_attr$Text_Name, "")
## Create vertex attribute for the corpus number of the text in question.
QDC_text_attr[,"Corpus_num"] <- QDC_text_nodes$Corpus_num[match(unlist(QDC_text_attr$Text_Name), QDC_text_nodes$Text_Name)]

## Check that actors in the node df and network are in the same order
all(QDC_text_net %v% "vertex.names"==QDC_text_attr$Text_Name)

## Create tie name edge variable for network object

QDC_text[, "Actor_Corpus_num"] <- QDC_text_nodes$Corpus_num[match(unlist(QDC_text$`ACTOR-TEXT`), QDC_text_nodes$Text_Name)]
QDC_text[, "Tie_Corpus_num"] <- QDC_text_nodes$Corpus_num[match(unlist(QDC_text$`TIE-TEXT`), QDC_text_nodes$Text_Name)]
QDC_text[, "Tie_name"] <- paste0(QDC_text$Actor_Corpus_num, " &#8594 ", QDC_text$Tie_Corpus_num)
QDC_text$Actor_Corpus_num=NULL; QDC_text$Tie_Corpus_num=NULL


## NOW finally create network object

QDC_text_net <- network(QDC_text, matrix.type= "edgelist", vertex.attr = QDC_text_attr, loops=F, multiple=F, ignore.eval = F)

## Colour according to actor type
QDC_text_net %v% "Type_col" <- c("#74B8F7", "#E831AE", "gold")[QDC_text_net %v% "Actor_type"]

## Colour according to tie quality
QDC_text_net %e% "Qual_col" <- c("red", "red", "grey61", "chartreuse3", "chartreuse3", "orange", "grey61", "grey61", "gray15")[QDC_text_net %e% "Quality"]




#### __Person-based Querelle ####



#### ____Node database - for all potential person nodes (actor-persons, tie-persons, response persons) ####

## First create a single attribute record for every entry in Actor-text column (which normally includes all actors in tie text and response text)
QDC_pers <- subset(QDC, select=c("ACTOR-PERSON", "TIE-PERSON", "Quality", "Date", "Individual or collective of authors (1); Authority or institution (2); Periodical (3)", "Gender", "Response-PERSON 1", "Response-PERSON 2"))
QDC_pers <- QDC_pers[!is.na(QDC_pers$`ACTOR-PERSON`),]

QDC_pers_nodes <- as.data.frame(table(QDC_pers$`ACTOR-PERSON`, QDC_pers$`Individual or collective of authors (1); Authority or institution (2); Periodical (3)`, QDC_pers$Gender, QDC_pers$Date))
QDC_pers_nodes <- QDC_pers_nodes[QDC_pers_nodes$Freq>0,1:4]
colnames(QDC_pers_nodes) <- c("Pers_Name", "Actor_type", "Gender", "Date")



## Now create list of actors that you want to use to populate the Node database
x <- as.data.frame(unique(append(QDC_pers$`ACTOR-PERSON`, c(QDC_pers$`TIE-PERSON`, QDC_pers$`Response-PERSON 1`, QDC_pers$`Response-PERSON 2`))))
x <- data.frame(x[!is.na(x)])
colnames(x) <- "Actors"

## Now fill with attributes using matching
x[,2] <- QDC_pers_nodes$Actor_type[match(unlist(x$Actors), QDC_pers_nodes$Pers_Name)]
x[,3] <- QDC_pers_nodes$Gender[match(unlist(x$Actors), QDC_pers_nodes$Pers_Name)]
x[,4] <- QDC_pers_nodes$Date[match(unlist(x$Actors), QDC_pers_nodes$Pers_Name)]
x$V2 <- as.numeric(x$V2)
x$V2[which(is.na(x$V2))] <- 99 #missing data coded 99
x$V3 <- as.numeric(x$V3)
x$V3[which(is.na(x$V3))] <- 99 #missing data coded 99
x$V4 <- as.numeric(as.character(x$V4))
x$V4[which(is.na(x$V4))] <- 99 #missing data coded 99
QDC_pers_nodes <- x
colnames(QDC_pers_nodes) <- c("Pers_Name", "Actor_type", "Gender", "Date")
QDC_pers_nodes$Pers_Name <- as.character(QDC_pers_nodes$Pers_Name)
QDC_pers_nodes <- QDC_pers_nodes[order(QDC_pers_nodes$Pers_Name),]
QDC_pers_nodes$Type_col <- c("#74B8F7", "#E831AE", "gold")[QDC_pers_nodes$Actor_type]
rm(x)

#### ____Node attributes for network object ####

#QDC_pers_net <- network(QDC_pers, matrix.type= "edgelist", loops=F, multiple=F, ignore.eval = F)


#### ____network object ####

QDC_pers <- subset(QDC_62_89, select=c("ACTOR-PERSON", "TIE-PERSON", "Quality", "Date"))
QDC_pers <- QDC_pers[!is.na(QDC_pers$`ACTOR-PERSON`),]
QDC_pers <- QDC_pers[!is.na(QDC_pers$`TIE-PERSON`),]
QDC_pers <- QDC_pers[!is.na(QDC_pers$Quality),]
QDC_pers[,"Line_type"] <- "solid"
##Delete College de Soreze and La Fleche if they are in there
#QDC_pers <- QDC_pers[-(which(QDC_pers$`ACTOR-PERSON`=="Coll?ge de Sor?ze" | QDC_pers$`TIE-PERSON`=="Coll?ge de Sor?ze")),]
#QDC_pers <- QDC_pers[-(which(QDC_pers$`ACTOR-PERSON`=="Coll?ge de la Fl?che" | QDC_pers$`TIE-PERSON`=="Coll?ge de la Fl?che")),]

### negative 'Quality' values screw with the network package. Let's just make all Quality values positive
QDC_pers$Quality <- QDC_pers$Quality + 3
QDC_pers$Quality[QDC_pers$Quality>6] <- QDC_pers$Quality[QDC_pers$Quality>6]-1

### Add 'responses' in the querelle as ties - first Response-text 1, then Response-text 2 # Note: have to add Actor text in there so the "unique" command works, as La Chalotais sends a response to the Parlement de Bretagne twice but in two different texts (then delete the variable)

QDC_pers_resp <- data.frame(QDC$`ACTOR-PERSON`, QDC$`Response-PERSON 1`, QDC$`ACTOR-TEXT`, QDC$`Response-TEXT 1`,  QDC$Date)
QDC_pers_resp <- QDC_pers_resp[!is.na(QDC_pers_resp$QDC..Response.PERSON.1.),]
QDC_pers_resp <- unique(QDC_pers_resp)
QDC_pers_resp$QDC..ACTOR.TEXT.=NULL; QDC_pers_resp$QDC..Response.TEXT.1.=NULL

QDC_pers_resp_2 <- data.frame(QDC$`ACTOR-PERSON`, QDC$`Response-PERSON 2`, QDC$`ACTOR-TEXT`, QDC$`Does it respond to a SECOND catalyst? If so, which? Response-TEXT 2`, QDC$Date)
QDC_pers_resp_2 <- QDC_pers_resp_2[!is.na(QDC_pers_resp_2$QDC..Response.PERSON.2.),]
QDC_pers_resp_2 <- unique(QDC_pers_resp_2)
QDC_pers_resp_2$QDC..ACTOR.TEXT.=NULL; QDC_pers_resp_2$QDC..Does.it.respond.to.a.SECOND.catalyst..If.so..which..Response.TEXT.2.=NULL

colnames(QDC_pers_resp) <- c("ACTOR-PERSON", "TIE-PERSON", "Date"); colnames(QDC_pers_resp_2) <- colnames(QDC_pers_resp)

QDC_pers_resp <- rbind(QDC_pers_resp, QDC_pers_resp_2)

QDC_pers_resp[,"Quality"] <- 9
QDC_pers_resp[,"Line_type"] <- "dashed"

QDC_pers <- rbind(QDC_pers, QDC_pers_resp)
rm(QDC_pers_resp_2, QDC_pers_resp)


##Note: Loops are allowed; if they aren't, just delete them from the DB - find out which ones they are here to them here: x <- QDC_pers[QDC_pers$`ACTOR-PERSON`==QDC_pers$`TIE-PERSON`,]
##Note: There are also multiple ties. FOR NDTV DYNAMIC VIS (which can't take them into account anyways): Delete
x <- QDC_pers[duplicated(QDC_pers[,c("ACTOR-PERSON", "TIE-PERSON")]),] # There are quite a few, and not only self-ties or ties from journals...
QDC_pers <- QDC_pers[!duplicated(QDC_pers[,c("ACTOR-PERSON", "TIE-PERSON")]),] # This deletes them

QDC_pers_net <- network(QDC_pers, matrix.type= "edgelist", loops=T, multiple=F, ignore.eval = F)

### Creating node attr database - note, you need list of vertices to do this.

QDC_pers_attr <- as.data.frame(QDC_pers_net %v% "vertex.names")
colnames(QDC_pers_attr) <- "Pers_Name"
## Import Actor type
QDC_pers_attr$Pers_Name <- as.character(QDC_pers_attr$Pers_Name)
QDC_pers_attr[,"Actor_type"] <- QDC_pers_nodes$Actor_type[match(unlist(QDC_pers_attr$Pers_Name), QDC_pers_nodes$Pers_Name)]
## Attribute for node colour
QDC_pers_attr$Type_col <- c("#74B8F7", "#E831AE", "gold")[QDC_pers_attr$Actor_type]
## Create a vertex attribute for the number of sides of the vertex - if the actor is an authority or institution (i.e. actor type 2), then a pentagon, otherwise a circle
QDC_pers_attr[,"Vertex_sides"] <- ifelse(QDC_pers_attr$Actor_type==2, 4, 50) # 50 means 50 sides, which for some reason ends up appearing as a circle
## Create label - only for D'Alembert, La Chalotais, and Rousseau
QDC_pers_attr[, "Label"] <- ifelse(QDC_pers_attr$Pers_Name=="D'Alembert"|QDC_pers_attr$Pers_Name=="La Chalotais"|QDC_pers_attr$Pers_Name=="Rousseau", QDC_pers_attr$Pers_Name, "")
## Check that actors in the node df and network are in the same order
all(QDC_pers_net %v% "vertex.names"==QDC_pers_attr$Pers_Name)

## NOW finally create network object

QDC_pers_net <- network(QDC_pers, matrix.type= "edgelist", vertex.attr = QDC_pers_attr, loops=TRUE, multiple=F, ignore.eval = F)

## Colour according to actor type
QDC_pers_net %v% "Type_col" <- c("#74B8F7", "#E831AE", "gold")[QDC_pers_net %v% "Actor_type"]

## Colour according to tie quality
QDC_pers_net %e% "Qual_col" <- c("red", "red", "grey61", "chartreuse3", "chartreuse3", "orange", "grey61", "grey61", "gray15")[QDC_pers_net %e% "Quality"]







##### Visualisations #####


##### ____Network Dynamic Object - Person-based network #####

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
#QDC_62_89_bis <- QDC_62_89
#QDC_62_89_bis$`TIE-TEXT`[is.na(QDC_62_89_bis$`TIE-TEXT`) & !(is.na(QDC_62_89_bis$`TIE-PERSON`))] <- QDC_62_89_bis$`TIE-PERSON`[is.na(QDC_62_89_bis$`TIE-TEXT`) & !(is.na(QDC_62_89_bis$`TIE-PERSON`))] 
QDC_text_for_pers_net <- subset(QDC_62_89_bis, select=c("ACTOR-TEXT", "TIE-TEXT", "Quality", "Date", "order"))
QDC_text_for_pers_net <- QDC_text_for_pers_net[!is.na(QDC_text_for_pers_net$`ACTOR-TEXT`),]
QDC_text_for_pers_net <- QDC_text_for_pers_net[!is.na(QDC_text_for_pers_net$`TIE-TEXT`),]
QDC_text_for_pers_net <- QDC_text_for_pers_net[!is.na(QDC_text_for_pers_net$Quality),]
#QDC_text_for_pers_net[,"Line_type"] <- "solid" #Not needed for dynamic vis

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
#QDC_text_for_pers_net$order=NULL

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

#text_to_person_mapper <- as.data.frame(table(QDC$`ACTOR-TEXT`, QDC$`ACTOR-PERSON`))
#text_to_person_mapper <- text_to_person_mapper[text_to_person_mapper$Freq>0,1:2]
#xx <- data.frame(text_to_person_mapper$Var2, text_to_person_mapper$Var2)
#xx <- unique(xx)
#colnames(xx) <- c("Var1", "Var2")
#text_to_person_mapper <- rbind(text_to_person_mapper, xx)


text_to_person_mapper <- unique(QDC[, c("ACTOR-TEXT", "ACTOR-PERSON")])
pers_to_person_mapper <- text_to_person_mapper
pers_to_person_mapper$`ACTOR-TEXT` <- pers_to_person_mapper$`ACTOR-PERSON`
pers_to_person_mapper <- unique(pers_to_person_mapper)
text_to_person_mapper <- rbind(text_to_person_mapper, pers_to_person_mapper)

## CHECK: Does each actor-text correspond to exactly one actor-person?
#which(duplicated(text_to_person_mapper$Var1))
# ANS: Yes

## CHECK: Are any actors in tie-text, response-text 1 and response-text 2 NOT in actor-text column?
#QDC$`TIE-TEXT`[which(!(QDC$`TIE-TEXT` %in% QDC$`ACTOR-TEXT`))] ; QDC$`Response-TEXT 1`[which(!(QDC$`Response-TEXT 1` %in% QDC$`ACTOR-TEXT`))] ; QDC$`Does it respond to a SECOND catalyst? If so, which? Response-TEXT 2`[which(!(QDC$`Does it respond to a SECOND catalyst? If so, which? Response-TEXT 2` %in% QDC$`ACTOR-TEXT`))] 
## CHECK: Are any actors in tie-person, response-person 1 and response-person 2 NOT in actor-person column?
#QDC$`TIE-PERSON`[which(!(QDC$`TIE-PERSON` %in% QDC$`ACTOR-PERSON`))] ; QDC$`Response-PERSON 1`[which(!(QDC$`Response-PERSON 1` %in% QDC$`ACTOR-PERSON`))] ; QDC$`Response-PERSON 2`[which(!(QDC$`Response-PERSON 2` %in% QDC$`ACTOR-PERSON`))] 
# ANS: all Response/tie texts/persons are in the actor text/response columns
## IF NOT, run the following:
#xx <- as.data.frame(table(QDC$`Response-TEXT 1`, QDC$`Response-PERSON 1`))
#xx <- xx[xx$Freq>0,]
#w <- as.data.frame(table(xx$Var1)) # CHECK: each actor-text corresponds to exactly one actor-person
#xxx <- as.data.frame(table(QDC$`Does it respond to a SECOND catalyst? If so, which? Response-TEXT 2`, QDC$`Response-PERSON 2`))
#xxx <- xxx[xxx$Freq>0,]
#w <- as.data.frame(table(xxx$Var1)) # CHECK: each actor-text corresponds to exactly one actor-person
#xx <- rbind(text_to_person_mapper, xx, xxx)
#rm(xx, xxx)

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
#colnames(QDC_vs_pers_dyn)[colnames(QDC_vs_pers_dyn)=="Actor_text"] <- "Actor_pers"
QDC_vs_pers_dyn$Actor_pers <- text_to_person_mapper$`ACTOR-PERSON`[match(unlist(QDC_vs_pers_dyn$Actor_text), text_to_person_mapper$`ACTOR-TEXT`)]
QDC_vs_pers_dyn$Actor_code <- QDC_vs$Actor_code[match(unlist(QDC_vs_pers_dyn$Actor_pers), QDC_vs$Actor_pers)]
QDC_vs_pers_dyn <- QDC_vs_pers_dyn[order(QDC_vs_pers_dyn$onset, QDC_vs_pers_dyn$order_of_entry),]
QDC_vs_pers_dyn <- QDC_vs_pers_dyn[!duplicated(QDC_vs_pers_dyn$Actor_code),]

rm(text_to_person_mapper)

## The following is just to remove ambiguity with variable names

#colnames(QDC_es)[colnames(QDC_es)=="ACTOR-TEXT"] <- "ACTOR-PERSON"; 
#colnames(QDC_es)[colnames(QDC_es)=="TIE-TEXT"] <- "TIE-PERSON"
#colnames(QDC_pre_62_edges)[colnames(QDC_pre_62_edges)=="ACTOR-TEXT"] <- "ACTOR-PERSON"; 
#colnames(QDC_pre_62_edges)[colnames(QDC_pre_62_edges)=="TIE-TEXT"] <- "TIE-PERSON"

# Need to replace QdC_vs with a text version for now

QDC_vs <- QDC_vs_pers_dyn


## Create network dynamic object
# Don't use base_net - it screws with creation of dynamic net
#QDC_pers_dyn <- networkDynamic(base.net = QDC_pers_net, vertex.spells = QDC_vs[,1:5], edge.spells = QDC_es[,c(1:4, 10)], create.TEAs = TRUE)µ
#number_of_nodes <- length(unique(QDC_vs$Actor_code))

#QDC_pers_dyn <- networkDynamic(base.net = dummy_net, vertex.spells = QDC_vs[,1:5], edge.spells = QDC_es[,c(1:4, 10)], create.TEAs = TRUE, verbose = TRUE)
#QDC_pers_dyn <- networkDynamic(vertex.spells = QDC_vs[,1:4], edge.spells = QDC_es[,c("onset", "terminus", "Actor_code", "Tie_code", "Quality", "Qual_col")], create.TEAs = TRUE, verbose = TRUE)

#### Create weight attribute for ties

edge_weights_df <- QDC_es
edge_weights_df[,"num"] <- 1:nrow(edge_weights_df)
edge_weights_df <- edge_weights_df[order(edge_weights_df$Actor_code, edge_weights_df$Tie_code, edge_weights_df$onset),]
edge_weights_df[,"ego_alter"] <- paste0(edge_weights_df$Actor_code, "_", edge_weights_df$Tie_code)
edge_weights_df$edge_weights <- 1

for (rownum in 2:nrow(edge_weights_df)) {
  #TODO: Make the following condition work with years, not slices
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
  #  pre_62 <- QDC_es[QDC_es$onset<start_slice,]
} else if (slice_or_year == "year") {
  start_slice <- 1762
  #  pre_62 <- QDC_es[QDC_es$onset<start_slice,]
}

QDC_es_dynamic_vis <- assign_pre_QDC_edge_onsets(edge_spells = QDC_es_dynamic_vis, .start_slice = start_slice)

QDC_es_dynamic_vis <- rbind(QDC_pre_62_edges, QDC_es_dynamic_vis)

QDC_pers_dyn <- networkDynamic(vertex.spells = QDC_vs_dynamic_vis, edge.spells = QDC_es_dynamic_vis[,c("onset", "terminus", "Actor_code", "Tie_code", "Qual_col")], create.TEAs = TRUE)

#reconcile.vertex.activity(QDC_pers_dyn, mode = "expand.to.edges", edge.active.default = FALSE)

#vertex and edge attributes

QDC_pers_attr_dyn <- QDC_pers_nodes[order(QDC_pers_attr$Pers_Name),]

for (col in colnames(QDC_pers_attr_dyn)) {
  QDC_pers_dyn %v% col <- QDC_pers_attr_dyn[[col]]
}
QDC_pers_dyn %v% "vertex.names" <- QDC_pers_attr_dyn$Pers_Name

#QDC_pers_dyn %e% "Tie_name_fix" <- QDC_es$Tie_name_fixed
#TODO: Fix the presence of NAs in this attribute

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
#    non_overlapping_edge_weights <- non_overlapping_edges$edge_weights[length(non_overlapping_edges$edge_weights)]
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

start <- 17619
end <- 17640

## Trying to make it less bunched

QDC_pers_anim <- compute.animation(QDC_pers_dyn, slice.par=list(start=start, end=end, interval=1, aggregate.dur=0, rule="any"), animation.mode = "kamadakawai", chain.direction = "reverse", default.dist = 6, verbose = TRUE)

#QDC_pers_anim_final <- compute.animation(QDC_pers_dyn, slice.par=list(start=end, end=end, interval=1, aggregate.dur=1, rule="any"), animation.mode = "kamadakawai", chain.direction = "reverse", default.dist = 6, verbose = TRUE)

#QDC_pers_anim <- compute.animation(QDC_pers_dyn, slice.par=list(start=start, end=end, interval=1, aggregate.dur=1, rule="any"), animation.mode = "kamadakawai", seed.coords = matrix(data = c(get.vertex.attribute.active(QDC_pers_anim_final, "animation.x", at = end), get.vertex.attribute.active(QDC_pers_anim_final, "animation.y", at = end)), ncol = 2), chain.direction = "reverse", default.dist = 6, verbose = TRUE)

QDC_pers_anim2 <- QDC_pers_dyn

for (i in seq(from = start,to = end+1, by=1)) {
  activate.vertex.attribute(QDC_pers_anim2, "animation.x", at = i, value = (get.vertex.attribute.active(QDC_pers_anim, "animation.x", at = i))/3.2)
}

for (i in seq(from = start,to = end+1, by=1)) {
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
                               #               render.par=list(tween.frames=100, show.time= FALSE),
                               vertex.border="#ffffff",
                               vertex.col = "vertex_colour",
                               #               vertex.sides = "Vertex_sides", # only circular nodes wanted for final version
                               main="Querelle des collèges, 1762-1789",
                               #              xlab = function(s){paste(trunc((QDC_pers_dyn$gal$slice.par$start+1761)+(QDC_pers_dyn$gal$slice.par$interval*s)/210))}, #This label makes the start year appear at the bottom, truncated of its decimal numbers, when you use the system where each year is split into 210
#                               xlab = function(s){paste(trunc((QDC_pers_dyn$gal$slice.par$start+QDC_pers_dyn$gal$slice.par$interval*s)/10))},
                               xlab = year_label,
#                               vertex.cex = function(slice){(10*(sna::degree(slice, cmode = "freeman") + 0.000001)/(sna::degree(slice, cmode = "freeman") + 0.000001)*(log(((sna::degree(slice, cmode = "freeman")+5)/100)+1)))},
                                vertex.cex = node_size,
                               #               vertex.cex = 0.8,
                               #               vertex.cex = function(slice){ 0.8*degree(slice)/degree(slice) + 0.000001},
#                               edge.lwd = function(slice){log((slice %e% 'edge_weights')+1)*2},
                               edge.lwd = function(slice){(log(slice %e% 'edge_weights')*2)+1},
                               vertex.tooltip = function(slice){paste0(slice %v% 'Actor_pers_name')}, 
#                               vertex.tooltip = QDC_pers_anim2 %v% 'Pers_Name', 
                               edge.tooltip = function(slice){slice %e% 'Tie_name_dyn'},
                               edge.col = "edge_colour", usearrows=TRUE),
               d3.options = list(animationDuration=800, debugFrameInfo=TRUE, durationControl=TRUE, margin=list(x=0,y=10), enterExitAnimationFactor=0.1),
                               launchBrowser=TRUE, filename=filename,
               verbose=TRUE)






















#### ____Network Dynamic Object - text-based network ####

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
end <- 17899

# Calculate animation
#QDC_text_anim <- compute.animation(QDC_text_dyn, slice.par=list(start=start, end=end, interval=1, aggregate.dur=1, rule="any"), animation.mode = "kamadakawai", chain.direction = "reverse", default.dist = 6, verbose = TRUE)
# Trying to change aggregation_dur
QDC_text_anim <- compute.animation(QDC_text_dyn, slice.par=list(start=start, end=end, interval=1, aggregate.dur=0, rule="any"), animation.mode = "kamadakawai", chain.direction = "reverse", default.dist = 6, verbose = TRUE)

#QDC_text_anim_final <- compute.animation(QDC_text_dyn, slice.par=list(start=end, end=end, interval=1, aggregate.dur=1, rule="any"), animation.mode = "kamadakawai", chain.direction = "reverse", default.dist = 6, verbose = TRUE)

#QDC_text_anim <- compute.animation(QDC_text_dyn, slice.par=list(start=start, end=end, interval=1, aggregate.dur=1, rule="any"), animation.mode = "kamadakawai", seed.coords = matrix(data = c(get.vertex.attribute.active(QDC_text_anim_final, "animation.x", at = end), get.vertex.attribute.active(QDC_text_anim_final, "animation.y", at = end)), ncol = 2), chain.direction = "reverse", default.dist = 6, verbose = TRUE)

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
#               edge.col = function(slice){slice %e% 'sent_to_rousseau'},
               vertex.border="#ffffff",
               vertex.col = "vertex_colour",
               xlab = year_label,
               vertex.cex = node_size,
               usearrows=TRUE,
               d3.options = list(animationDuration=800, debugFrameInfo=TRUE, durationControl=TRUE, margin=list(x=0,y=10), enterExitAnimationFactor=0.1),
               output.mode = 'HTML', launchBrowser=TRUE, filename=filename,
               verbose=TRUE)


