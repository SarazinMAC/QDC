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

