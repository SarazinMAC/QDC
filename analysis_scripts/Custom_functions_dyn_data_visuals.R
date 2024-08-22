#########################################################
##### Custom functions - dynamic datasets & visuals #####
#########################################################

# Assign edge spell values of Pre-QdC actors to first onset at which the actors receive a tie

# Actually two functions, one nested in another, because some pre-QDC actors refer to each other

update_edge_spells <- function(spells, spells_to_update, spells_for_updating) {
  onsets <- data.frame(c(spells_for_updating$Actor_code, spells_for_updating$Tie_code), c(spells_for_updating$onset, spells_for_updating$onset))
  onsets <- onsets[complete.cases(onsets),]
  colnames(onsets) <- c("Actor_code", "onsets")
  onsets_for_spells_to_update <- onsets[onsets$Actor_code %in% spells_to_update$Actor_code,]
  onsets_for_spells_to_update <- onsets_for_spells_to_update[base::order(onsets_for_spells_to_update$Actor_code, onsets_for_spells_to_update$onsets),]
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
  formatted_df <- formatted_df[base::order(formatted_df[[order_colname]]),]
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
  QDC_vs <- QDC_vs[base::order(QDC_vs[[year_colname]], QDC_vs[[order_colname]]),]
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
  
  QDC_static_attr_dyn <- node_attr_df[base::order(node_attr_df$node_order_dynamic_vis),]
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
  onset_of_change <- onset_of_change[base::order(onset_of_change$Actor_code, onset_of_change$onset),]
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
