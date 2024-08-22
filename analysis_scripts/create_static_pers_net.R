##################################
##### create_static_pers_net #####
##################################



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
QDC_pers_nodes <- QDC_pers_nodes[base::order(QDC_pers_nodes$Pers_Name),]
QDC_pers_nodes$Type_col <- c("#74B8F7", "#E831AE", "gold")[QDC_pers_nodes$Actor_type]
rm(x)

#### ____network object ####

QDC_pers <- subset(QDC_62_89, select=c("ACTOR-PERSON", "TIE-PERSON", "Quality", "Date"))
QDC_pers <- QDC_pers[!is.na(QDC_pers$`ACTOR-PERSON`),]
QDC_pers <- QDC_pers[!is.na(QDC_pers$`TIE-PERSON`),]
QDC_pers <- QDC_pers[!is.na(QDC_pers$Quality),]
QDC_pers[,"Line_type"] <- "solid"

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


