#################################
##### Static visualisations #####
#################################


##### ____Divide network by period - TODO: turn into custom function #####

#### ______62_64 network ####

#QDC_text_62_64 <- QDC_text[QDC_text$Date<1765,] 
#QDC_text_net_62_64 <- network(QDC_text_62_64, matrix.type= "edgelist", loops=F, multiple=F, ignore.eval = F)
#QDC_text_attr_62_64 <- QDC_text_attr[QDC_text_attr$Text_Name %in% (QDC_text_net_62_64 %v% "vertex.names"),]
#QDC_text_net_62_64 <- network(QDC_text_62_64, matrix.type= "edgelist", vertex.attr = QDC_text_attr_62_64, loops=F, multiple=F, ignore.eval = F)

#QDC_text_net_62_64 %v% "Type_col" <- c("#74B8F7", "#E831AE", "gold")[QDC_text_net_62_64 %v% "Actor_type"]
#QDC_text_net_62_64 %e% "Qual_col" <- c("red", "red", "grey61", "chartreuse3", "chartreuse3", "orange", "grey61", "grey61", "gray15")[QDC_text_net_62_64 %e% "Quality"]

#### ______62_69 network ####

#QDC_text_62_69 <- QDC_text[QDC_text$Date<1770,] 
#QDC_text_net_62_69 <- network(QDC_text_62_69, matrix.type= "edgelist", loops=F, multiple=F, ignore.eval = F)
#QDC_text_attr_62_69 <- QDC_text_attr[QDC_text_attr$Text_Name %in% (QDC_text_net_62_69 %v% "vertex.names"),]
#QDC_text_net_62_69 <- network(QDC_text_62_69, matrix.type= "edgelist", vertex.attr = QDC_text_attr_62_69, loops=F, multiple=F, ignore.eval = F)
#
#QDC_text_net_62_69 %v% "Type_col" <- c("#74B8F7", "#E831AE", "gold")[QDC_text_net_62_69 %v% "Actor_type"]
#QDC_text_net_62_69 %e% "Qual_col" <- c("red", "red", "grey61", "chartreuse3", "chartreuse3", "orange", "grey61", "grey61", "gray15")[QDC_text_net_62_69 %e% "Quality"]

#### ______70_79 network ####

#QDC_text_70_79 <- QDC_text[QDC_text$Date>=1770 & QDC_text$Date<1780,] 
#QDC_text_net_70_79 <- network(QDC_text_70_79, matrix.type= "edgelist", loops=F, multiple=F, ignore.eval = F)
#QDC_text_attr_70_79 <- QDC_text_attr[QDC_text_attr$Text_Name %in% (QDC_text_net_70_79 %v% "vertex.names"),]
#QDC_text_net_70_79 <- network(QDC_text_70_79, matrix.type= "edgelist", vertex.attr = QDC_text_attr_70_79, loops=F, multiple=F, ignore.eval = F)#

#QDC_text_net_70_79 %v% "Type_col" <- c("#74B8F7", "#E831AE", "gold")[QDC_text_net_70_79 %v% "Actor_type"]
#QDC_text_net_70_79 %e% "Qual_col" <- c("red", "red", "grey61", "chartreuse3", "chartreuse3", "orange", "grey61", "grey61", "gray15")[QDC_text_net_70_79 %e% "Quality"]#

#### ______80_89 network ####
#
#QDC_text_80_89 <- QDC_text[QDC_text$Date>=1780 & QDC_text$Date<1790,] 
#QDC_text_net_80_89 <- network(QDC_text_80_89, matrix.type= "edgelist", loops=F, multiple=F, ignore.eval = F)
#QDC_text_attr_80_89 <- QDC_text_attr[QDC_text_attr$Text_Name %in% (QDC_text_net_80_89 %v% "vertex.names"),]
#QDC_text_net_80_89 <- network(QDC_text_80_89, matrix.type= "edgelist", vertex.attr = QDC_text_attr_80_89, loops=F, multiple=F, ignore.eval = F)
#
#QDC_text_net_80_89 %v% "Type_col" <- c("#74B8F7", "#E831AE", "gold")[QDC_text_net_80_89 %v% "Actor_type"]
#QDC_text_net_80_89 %e% "Qual_col" <- c("red", "red", "grey61", "chartreuse3", "chartreuse3", "orange", "grey61", "grey61", "gray15")[QDC_text_net_80_89 %e% "Quality"]







#### __Static vis ####



#### ____network package ####

### QDC_text

plot(QDC_text_net, vertex.col = "Type_col", edge.col = "Qual_col")
plot(QDC_text_net, vertex.col = "Type_col", edge.col = "Qual_col", edge.lty= "Line_type")
## With node size
par(mar=c(0,0,0,0))

## Fix layout - fruchterman-Reingold, not KK
lay <- network.layout.fruchtermanreingold(QDC_text_net, layout.par = NULL)

## degree (freeman) centrality
plot(QDC_text_net, coord = lay, vertex.col = "Type_col", vertex.cex = (10*(log(((degree(QDC_text_net)+5)/100)+1))), vertex.lwd = 0.5, edge.col = "Qual_col", arrowhead.cex=0.5, edge.lwd=0.20)
## outdegree centrality
plot(QDC_text_net, coord = lay, vertex.col = "Type_col", vertex.cex = (10*(log(((degree(QDC_text_net, cmode = "outdegree")+5)/100)+1))), vertex.lwd = 0.5, edge.col = "Qual_col", arrowhead.cex=0.5, edge.lwd=0.20)
## indegree centrality
plot(QDC_text_net, coord = lay, vertex.col = "Type_col", vertex.cex = (10*(log(((sna::degree(QDC_text_net, cmode = "indegree")+5)/100)+1))), vertex.lwd = 0.5, edge.col = "Qual_col", arrowhead.cex=0.5, edge.lwd=0.20)

## With tiny labels
## degree (freeman) centrality
plot(QDC_text_net, coord = lay, vertex.col = "Type_col", vertex.cex = (10*(log(((degree(QDC_text_net)+5)/100)+1))), vertex.lwd = 0.5, edge.col = "Qual_col", arrowhead.cex=0.5, edge.lwd=0.20, displaylabels=TRUE, label.cex=0.1, label.pos = 3)

lay <- network.layout.kamadakawai(QDC_text_net, layout.par = NULL)


### QDC_pers

plot(QDC_pers_net, vertex.col = "Type_col", edge.col = "Qual_col", edge.lty= "Line_type")

## With node size
par(mar=c(0,0,0,0))

## Fix layout - kamada-kawai
lay <- network.layout.kamadakawai(QDC_pers_net, layout.par = NULL)

## degree (freeman) centrality
plot(QDC_pers_net, coord = lay,  vertex.col = "Type_col", vertex.cex = (10*(log(((degree(QDC_pers_net)+5)/100)+1))), vertex.lwd = 0.5, edge.col = "Qual_col", arrowhead.cex=0.5, edge.lwd=0.20)
## outdegree centrality
plot(QDC_pers_net, coord = lay, vertex.col = "Type_col", vertex.cex = (10*(log(((degree(QDC_pers_net, cmode = "outdegree")+5)/100)+1))), vertex.lwd = 0.5, edge.col = "Qual_col", arrowhead.cex=0.5, edge.lwd=0.20)
## indegree centrality
plot(QDC_pers_net, coord = lay, vertex.col = "Type_col", vertex.cex = (10*(log(((degree(QDC_pers_net, cmode = "indegree")+5)/100)+1))), vertex.lwd = 0.5, edge.col = "Qual_col", arrowhead.cex=0.5, edge.lwd=0.20)

## Same - but counting loops in sizing of nodes
## degree (freeman) centrality
plot(QDC_pers_net, coord = lay,  vertex.col = "Type_col", vertex.cex = (10*(log(((degree(QDC_pers_net, diag = TRUE)+5)/100)+1))), vertex.lwd = 0.5, edge.col = "Qual_col", arrowhead.cex=0.5, edge.lwd=0.20)
## outdegree centrality
plot(QDC_pers_net, coord = lay, vertex.col = "Type_col", vertex.cex = (10*(log(((degree(QDC_pers_net, diag = TRUE, cmode = "outdegree")+5)/100)+1))), vertex.lwd = 0.5, edge.col = "Qual_col", arrowhead.cex=0.5, edge.lwd=0.20)
## indegree centrality
plot(QDC_pers_net, coord = lay, vertex.col = "Type_col", vertex.cex = (10*(log(((degree(QDC_pers_net, diag = TRUE, cmode = "indegree")+5)/100)+1))), vertex.lwd = 0.5, edge.col = "Qual_col", arrowhead.cex=0.5, edge.lwd=0.20)

## With labels

## degree (freeman) centrality
plot(QDC_pers_net, coord = lay,  vertex.col = "Type_col", vertex.cex = (10*(log(((degree(QDC_pers_net, diag = TRUE)+5)/100)+1))), vertex.lwd = 0.5, edge.col = "Qual_col", arrowhead.cex=0.5, edge.lwd=0.20, displaylabels=TRUE, label.cex=0.1, label.pos = 3)


## Fix layout - Fruchterman-Reingold
lay <- network.layout.fruchtermanreingold(QDC_pers_net, layout.par = NULL)

## degree (freeman) centrality
plot(QDC_pers_net, coord = lay,  vertex.col = "Type_col", vertex.cex = (10*(log(((degree(QDC_pers_net)+5)/100)+1))), vertex.lwd = 0.5, edge.col = "Qual_col", arrowhead.cex=0.5, edge.lwd=0.20)
## outdegree centrality
plot(QDC_pers_net, coord = lay, vertex.col = "Type_col", vertex.cex = (10*(log(((degree(QDC_pers_net, cmode = "outdegree")+5)/100)+1))), vertex.lwd = 0.5, edge.col = "Qual_col", arrowhead.cex=0.5, edge.lwd=0.20)
## indegree centrality
plot(QDC_pers_net, coord = lay, vertex.col = "Type_col", vertex.cex = (10*(log(((degree(QDC_pers_net, cmode = "indegree")+5)/100)+1))), vertex.lwd = 0.5, edge.col = "Qual_col", arrowhead.cex=0.5, edge.lwd=0.20)

#### ____igraph ####

## IF network object already computed

par(mar=c(0,0,0,0))
QDC_text_net_igraph <- asIgraph(QDC_text_net)

plot(QDC_text_net_igraph, vertex.color = V(QDC_text_net_igraph)$Type_col, edge.color = E(QDC_text_net_igraph)$Qual_col, vertex.size=4, edge.curved=0, edge.width=2, edge.lty=E(QDC_text_net_igraph)$Line_type, vertex.label=V(QDC_text_net_igraph)$Label, vertex.label.cex=1, edge.arrow.size=0.3, vertex.label.dist=1, vertex.label.degree=-pi/2, layout=layout_with_fr)
#plot(QDC_text_net_igraph, vertex.color = V(QDC_text_net_igraph)$Type_col, edge.color = E(QDC_text_net_igraph)$Qual_col, vertex.size=4, vertex.label=NULL, vertex.label.dist=0.4, edge.curved=0, edge.width=2, edge.lty=E(QDC_text_net_igraph)$Line_type, vertex.label.cex=0.2, edge.arrow.size=0.3, vertex.label.degree=-pi/2, layout=layout_with_kk)

QDC_text_net_62_64_igraph <- asIgraph(QDC_text_net_62_64)
plot(QDC_text_net_62_64_igraph, vertex.color = V(QDC_text_net_62_64_igraph)$Type_col, edge.color = E(QDC_text_net_62_64_igraph)$Qual_col, vertex.size=4, edge.curved=0, edge.width=2, edge.lty=E(QDC_text_net_62_64_igraph)$Line_type, vertex.label=V(QDC_text_net_62_64_igraph)$Label, vertex.label.cex=1, edge.arrow.size=0.3, vertex.label.dist=1, vertex.label.degree=-pi/2, layout=layout_with_fr)

QDC_text_net_62_69_igraph <- asIgraph(QDC_text_net_62_69)
plot(QDC_text_net_62_69_igraph, vertex.color = V(QDC_text_net_62_69_igraph)$Type_col, edge.color = E(QDC_text_net_62_69_igraph)$Qual_col, vertex.size=4, edge.curved=0, edge.width=2, edge.lty=E(QDC_text_net_62_69_igraph)$Line_type, vertex.label=V(QDC_text_net_62_69_igraph)$Label, vertex.label.cex=1, edge.arrow.size=0.3, vertex.label.dist=1, vertex.label.degree=-pi/2, layout=layout_with_fr)

QDC_text_net_70_79_igraph <- asIgraph(QDC_text_net_70_79)
plot(QDC_text_net_70_79_igraph, vertex.color = V(QDC_text_net_70_79_igraph)$Type_col, edge.color = E(QDC_text_net_70_79_igraph)$Qual_col, vertex.size=4, edge.curved=0, edge.width=2, edge.lty=E(QDC_text_net_70_79_igraph)$Line_type, vertex.label=V(QDC_text_net_70_79_igraph)$Label, vertex.label.cex=1, edge.arrow.size=0.3, vertex.label.dist=1, vertex.label.degree=-pi/2, layout=layout_with_fr)

QDC_text_net_80_89_igraph <- asIgraph(QDC_text_net_80_89)
plot(QDC_text_net_80_89_igraph, vertex.color = V(QDC_text_net_80_89_igraph)$Type_col, edge.color = E(QDC_text_net_80_89_igraph)$Qual_col, vertex.size=4, edge.curved=0, edge.width=2, edge.lty=E(QDC_text_net_80_89_igraph)$Line_type, vertex.label=V(QDC_text_net_80_89_igraph)$Label, vertex.label.cex=1, edge.arrow.size=0.3, vertex.label.dist=1, vertex.label.degree=-pi/2, layout=layout_with_fr)


# Note: previous colour for neutral ties: "#696969"

par(mfrow=c(1,1), mar=c(0.1,0.1,0.1,0.1)) 
E(QDC_text_net)$color <- ifelse(E(QDC_text_net)$Quality=="-1" | E(QDC_text_net)$Quality=="-2", "red", "gray15") 
E(QDC_text_net)$color <- ifelse(E(QDC_text_net)$Quality==" 1" | E(QDC_text_net)$Quality==" 2", "chartreuse3", E(QDC_text_net)$color) 
E(QDC_text_net)$color <- ifelse(E(QDC_text_net)$Quality==" 4", "orange", E(QDC_text_net)$color) 

plot.igraph(QDC_text_net, vertex.size=2, vertex.label.dist=0.4, edge.curved=0.3, vertex.label.cex=0.2, edge.arrow.size=0.1, vertex.label.degree=-pi/2, layout=layout_with_fr)


par(mfrow=c(1,1), mar=c(0.1,0.1,0.1,0.1))
E(QDC_pers2_net)$color <- ifelse(E(QDC_pers2_net)$Quality=="-1" | E(QDC_pers2_net)$Quality=="-2", "red", "gray15") 
E(QDC_pers2_net)$color <- ifelse(E(QDC_pers2_net)$Quality==" 1" | E(QDC_pers2_net)$Quality==" 2", "chartreuse3", E(QDC_pers2_net)$color) 
E(QDC_pers2_net)$color <- ifelse(E(QDC_pers2_net)$Quality==" 4", "orange", E(QDC_pers2_net)$color) 

plot.igraph(QDC_pers2_net, vertex.size=2, vertex.label.dist=0.4, edge.curved=0.3, vertex.label.cex=0.4, edge.arrow.size=0.1, vertex.label.degree=-pi/2, layout=layout_with_fr)


par(mfrow=c(1,1), mar=c(0.1,0.1,0.1,0.1))
E(QDC_pers_net)$color <- ifelse(E(QDC_pers_net)$Quality=="-1" | E(QDC_pers_net)$Quality=="-2", "red", "gray15") 
E(QDC_pers_net)$color <- ifelse(E(QDC_pers_net)$Quality==" 1" | E(QDC_pers_net)$Quality==" 2", "chartreuse3", E(QDC_pers_net)$color) 
E(QDC_pers_net)$color <- ifelse(E(QDC_pers_net)$Quality==" 4", "orange", E(QDC_pers_net)$color) 

plot.igraph(QDC_pers_net, vertex.size=2, vertex.label.dist=0.4, edge.curved=0.3, vertex.label.cex=0.4, edge.arrow.size=0.1, vertex.label.degree=-pi/2, layout=layout_with_fr)


par(mfrow=c(1,1), mar=c(0.1,0.1,0.1,0.1))
E(QDC_pers3_net)$color <- ifelse(E(QDC_pers3_net)$Quality=="-1" | E(QDC_pers3_net)$Quality=="-2", "red", "gray15") 
E(QDC_pers3_net)$color <- ifelse(E(QDC_pers3_net)$Quality==" 1" | E(QDC_pers3_net)$Quality==" 2", "chartreuse3", E(QDC_pers3_net)$color) 
E(QDC_pers3_net)$color <- ifelse(E(QDC_pers3_net)$Quality==" 4", "orange", E(QDC_pers3_net)$color) 


plot.igraph(QDC_pers3_net, vertex.size=2, vertex.label.dist=0.4, edge.curved=0.3, vertex.label.cex=0.4, edge.arrow.size=0.1, vertex.label.degree=-pi/2, layout=layout_with_fr)

### Separate graphs for years 1776-1785

lay <- layout_with_fr(QDC_pers2_net_1785)
lay_1784 <- lay[4:110,]


E(QDC_pers2_net_1785)$color <- ifelse(E(QDC_pers2_net_1785)$Quality=="-1" | E(QDC_pers2_net_1785)$Quality=="-2", "red", ifelse(E(QDC_pers2_net_1785)$Quality==" 1" | E(QDC_pers2_net_1785)$Quality==" 2", "chartreuse3", "#696969"))
plot.igraph(QDC_pers2_net_1785, vertex.size=2, vertex.label.dist=0.4, edge.curved=0.3, vertex.label.cex=0.4, edge.arrow.size=0.1, vertex.label.degree=-pi/2, layout=lay)

E(QDC_pers2_net_1784)$color <- ifelse(E(QDC_pers2_net_1784)$Quality=="-1" | E(QDC_pers2_net_1784)$Quality=="-2", "red", ifelse(E(QDC_pers2_net_1784)$Quality==" 1" | E(QDC_pers2_net_1784)$Quality==" 2", "chartreuse3", "#696969"))
plot.igraph(QDC_pers2_net_1784, vertex.size=2, vertex.label.dist=0.4, edge.curved=0.3, vertex.label.cex=0.4, edge.arrow.size=0.1, vertex.label.degree=-pi/2, layout=lay_1784)

