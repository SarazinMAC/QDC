########################################
##### Stats and Communities Helper #####
########################################

library(igraph)
library(tsna)
library(Cairo)
library(data.table)
library(ggplot2)
library(extrafont)

## Set global options
options(scipen = 999)

## import files creating dynamic networks

if(text_or_pers=="pers") {
  
  # Produce the dynamic person network
  source(paste0(Data_path, "analysis_scripts\\produce_dynamic_pers_network.R"))
  
} else if (text_or_pers=="text") {
  
  # Produce the dynamic text network
  source(paste0(Data_path, "analysis_scripts\\produce_dynamic_text_network.R"))
}


## set configurable values

export_path <- Data_path

date <- format(Sys.Date(), "%Y_%m_%d")

# TODO: Make the below work with both the text and person network (note last column "Actor_pers")
#QDC_vs_colnames_to_keep <- c("onset", "terminus", "Actor_code", "Actor_pers")

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
QDC_es <- assign_pre_QDC_edge_onsets(edge_spells = QDC_es)

# recreate QDC_dyn object

QDC_vs <- QDC_vs[base::order(QDC_vs$Actor_code),]

QDC_dyn <- networkDynamic(vertex.spells = QDC_vs[,1:4], edge.spells = QDC_es[,1:4], create.TEAs = TRUE)
