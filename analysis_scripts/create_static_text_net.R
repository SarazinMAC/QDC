##################################
##### create_static_text_net #####
##################################

#### __Text-based Querelle ####

#### ____Node database - for all potential text nodes (actor-texts, tie-texts, response texts) ####

# Two steps: Create a database of all nodal attribute information, then create a list of actors (that both send and receive ties) and populate with nodal attributes
## First create a single attribute record for every entry in Actor-text column (which normally includes all actors in tie text and response text)
QDC_text <- subset(QDC, select=
                     c("ACTOR-TEXT", "TIE-TEXT", "Quality", "Date", 
                       "Individual or collective of authors (1); Authority or institution (2); Periodical (3)", 
                       "Gender", "Response-TEXT 1", "Does it respond to a SECOND catalyst? If so, which? Response-TEXT 2",
                       "Place"))
QDC_text <- filter_non_na(QDC_text, "ACTOR-TEXT")


QDC_text_node_data <- as.data.frame(table(QDC_text$`ACTOR-TEXT`, QDC_text$`Individual or collective of authors (1); Authority or institution (2); Periodical (3)`, QDC_text$Gender, QDC_text$Date)) #This creates a problem when nodes have missing data for any attribute; if they do, then it gives missing values to all attributes
QDC_text_node_data <- QDC_text_node_data[QDC_text_node_data$Freq>0,1:4]
colnames(QDC_text_node_data) <- c("Text_Name", "Actor_type", "Gender", "Date")




## Now create list of actors that you want to use to populate the Node database
QDC_text_nodes <- as.data.frame(unique(append(QDC_text$`ACTOR-TEXT`, c(QDC_text$`TIE-TEXT`, QDC_text$`Response-TEXT 1`, QDC_text$`Does it respond to a SECOND catalyst? If so, which? Response-TEXT 2`))))
QDC_text_nodes <- data.frame(QDC_text_nodes[!is.na(QDC_text_nodes)])
colnames(QDC_text_nodes) <- "Actors"

## Now fill with attributes using matching
colnames(QDC_text_nodes) <- "Text_Name"
QDC_text_nodes <- assign_node_attributes(QDC_text_nodes, QDC_text_node_data, c("Actor_type", "Gender", "Date"))
QDC_text_nodes <- convert_numeric_and_replace_na(QDC_text_nodes, c("Actor_type", "Gender", "Date"))
QDC_text_nodes$Type_col <- c("#74B8F7", "#E831AE", "gold")[QDC_text_nodes$Actor_type]
QDC_text_nodes$Text_Name <- as.character(QDC_text_nodes$Text_Name)
QDC_text_nodes[,"Corpus_num"] <- as.numeric(row.names(QDC_text_nodes))
QDC_text_nodes <- QDC_text_nodes[base::order(QDC_text_nodes$Text_Name),]


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
QDC_text_resp <- process_all_response_edges(QDC_62_89)

QDC_text <- rbind(QDC_text, QDC_text_resp)
rm(QDC_text_resp_2, QDC_text_resp)
QDC_text <- QDC_text[base::order(QDC_text$order),]

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

#' Filter dataframe by non-NA in a column
#' @param df Dataframe
#' @param colname Column name to check for NA
#' @return Filtered dataframe
filter_non_na <- function(df, colname) {
  df[!is.na(df[[colname]]), ]
}

#' Assign attributes to a node dataframe using matching
#' @param nodes_df Dataframe of nodes
#' @param attr_df Dataframe of attributes
#' @param attr_names Names of attributes to assign
#' @param key_col Name of key column for matching
#' @return Dataframe with attributes assigned
assign_node_attributes <- function(nodes_df, attr_df, attr_names, key_col = "Text_Name") {
  for (attr in attr_names) {
    nodes_df[[attr]] <- attr_df[[attr]][match(unlist(nodes_df[[key_col]]), attr_df[[key_col]])]
  }
  return(nodes_df)
}

#' Convert columns to numeric and replace NA with a code
#' @param df Dataframe
#' @param columns Character vector of column names to convert
#' @param na_code Value to use for NA replacement (default 99)
#' @return Dataframe with specified columns converted and NA replaced
convert_numeric_and_replace_na <- function(df, columns, na_code = 99) {
  for (col in columns) {
    df[[col]] <- as.numeric(df[[col]])
    df[[col]][is.na(df[[col]])] <- na_code
  }
  return(df)
}

#' Remove NA and duplicated rows for response edges
#' @param df Dataframe
#' @param response_col Name of the response column
#' @return Cleaned dataframe with columns renamed to standard edge columns
clean_response_edges <- function(df, response_col) {
  resp <- df[!is.na(df[[response_col]]), ]
  resp <- resp[!duplicated(resp[, c("ACTOR-TEXT", response_col, "Date")]), ]
  colnames(resp) <- c("ACTOR-TEXT", "TIE-TEXT", "Date", "order")
  return(resp)
}

#' Process and combine all response edges for the querelle
#' @param QDC_62_89 Dataframe for 1762-1789
#' @return Combined and formatted response edge dataframe
process_all_response_edges <- function(QDC_62_89) {
  resp1 <- clean_response_edges(QDC_62_89[, c("ACTOR-TEXT", "Response-TEXT 1", "Date", "order")], "Response-TEXT 1")
  resp2 <- clean_response_edges(QDC_62_89[, c("ACTOR-TEXT", "Does it respond to a SECOND catalyst? If so, which? Response-TEXT 2", "Date", "order")], "Does it respond to a SECOND catalyst? If so, which? Response-TEXT 2")
  resp <- rbind(resp1, resp2)
  resp$Quality <- 9
  resp$Line_type <- "dashed"
  return(resp)
}

