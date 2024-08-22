###########################
##### Ad-hoc analyses #####
###########################
  
if(text_or_pers=="pers") {
  # Produce the dynamic person network
  source(paste0(Data_path, "analysis_scripts\\produce_dynamic_pers_network.R"))
  
} else if (text_or_pers=="text") {
  
  # Produce the dynamic text network
  source(paste0(Data_path, "analysis_scripts\\produce_dynamic_text_network.R"))
}

# Produce results for Table 1

table_1_results <- table(QDC_es$Quality)
table_1_results <- as.numeric(table_1_results)
names(table_1_results) <- c("An explicit negative reference (-2)",
                                                          "An implicit negative reference (-1)",
                                                          "A neutral reference (0)",
                                                          "An implicit positive reference (1)",
                                                          "An explicit positive reference (2)",
                                                          "An ambivalent reference (3)",
                                                          "A form of republication (4)",
                                                          "A sequel or supplement (5)",
                                                          "A response to a catalyst (6)")

# Produce results about texts published in 1762 and 1763 and their references 
# Note: for text network only

if (text_or_pers == "text") {
  # Temporary fix to assign correct onset to Gerdil (1763)
  
  texts_62_63 <- QDC_es
  texts_62_63$onset[texts_62_63$`ACTOR-TEXT`=="Gerdil (1763)"] <- 17639
  
  texts_62_63 <- texts_62_63[which(texts_62_63$onset>17619 & texts_62_63$onset<17640),]
  
  
  # create quality var with original values
  
  texts_62_63$Quality_original <- texts_62_63$Quality - 3
  
  # display references of texts according to their quality
  
  references_of_texts_62_63 <- table(texts_62_63$`TIE-TEXT`, texts_62_63$Quality_original)
  percent_refs_to_rousseau <- sum(
    references_of_texts_62_63["Rousseau (1762)",]/length(unique(texts_62_63$`ACTOR-TEXT`))
    )
rm(texts_62_63)
}


# display the indegrees and outdegrees of texts referring to Rousseau (1762) and La Chalotais (1763)

ids_referring_to_rousseau <- unique(
  QDC_es$Actor_code[QDC_es$`TIE-TEXT`=="Rousseau (1762)"])
names_referring_to_rousseau <- unique(
  QDC_es$`ACTOR-TEXT`[QDC_es$`TIE-TEXT`=="Rousseau (1762)"])

indegree_texts_referring_to_rousseau <- sna::degree(QDC_text_dyn, nodes = ids_referring_to_rousseau, cmode = "indegree")
names(indegree_texts_referring_to_rousseau) <- names_referring_to_rousseau
outdegree_texts_referring_to_rousseau <- sna::degree(QDC_text_dyn, nodes = ids_referring_to_rousseau, cmode = "outdegree")
names(outdegree_texts_referring_to_rousseau) <- names_referring_to_rousseau

rm(ids_referring_to_rousseau, names_referring_to_rousseau)


ids_referring_to_la_chalotais <- unique(
  QDC_es$Actor_code[QDC_es$`TIE-TEXT`=="La Chalotais (1763)"])
names_referring_to_la_chalotais <- unique(
  QDC_es$`ACTOR-TEXT`[QDC_es$`TIE-TEXT`=="La Chalotais (1763)"])

indegree_texts_referring_to_la_chalotais <- degree(QDC_text_dyn, nodes = ids_referring_to_la_chalotais, cmode = "indegree")
names(indegree_texts_referring_to_la_chalotais) <- names_referring_to_la_chalotais
outdegree_texts_referring_to_la_chalotais <- degree(QDC_text_dyn, nodes = ids_referring_to_la_chalotais, cmode = "outdegree")
names(outdegree_texts_referring_to_la_chalotais) <- names_referring_to_la_chalotais

rm(ids_referring_to_la_chalotais, names_referring_to_la_chalotais)

ids_all_texts_making_references <- unique(QDC_es$Actor_code)

indegree_all_texts_making_references <- degree(QDC_text_dyn, nodes = ids_all_texts_making_references, cmode = "indegree")
indegree_all_texts_making_references <- table(indegree_all_texts_making_references)

rm(ids_all_texts_making_references)



# How many nodes refer to La Chalotais and to the other texts he refers to?

actor <- "La Chalotais"

ties_sent_to_actor_after_17635 <- QDC_es[QDC_es$`TIE-PERSON`== actor & QDC_es$onset>=17635 & QDC_es$`ACTOR-PERSON` != "La Chalotais",]

persons_refer_to_actor <- QDC_es[QDC_es$`ACTOR-PERSON` %in% ties_sent_to_actor_after_17635$`ACTOR-PERSON`,]

persons_refer_to_actor <- persons_refer_to_actor[persons_refer_to_actor$onset>=17635,]

list_pers <- split(persons_refer_to_actor, f = persons_refer_to_actor$`ACTOR-PERSON`)

persons_referring_to_la_chalotais_and_others <- sapply(list_pers, function(x)  {
  "La Chalotais" %in% x[,"TIE-PERSON"]*1 +
    "Rousseau" %in% x[,"TIE-PERSON"]*1 +
    "D'Alembert" %in% x[,"TIE-PERSON"]*1 +
    "De l'éducation publique" %in% x[,"TIE-PERSON"]*1})

n_persons_referring_to_la_chalotais_and_others <- length(persons_referring_to_la_chalotais_and_others)

rm(actor, ties_sent_to_actor_after_17635, persons_refer_to_actor)