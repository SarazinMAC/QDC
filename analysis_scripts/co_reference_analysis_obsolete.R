#############################
##### Ad hoc statistics #####
#############################


# section 4.2: how many nodes refer to La Chalotais and to the other texts he refers to?

actor <- "La Chalotais"

# Texts 

ties_sent_to_actor_after_17635 <- QDC_es[QDC_es$`TIE-PERSON`== actor & QDC_es$onset>=17635 & QDC_es$`ACTOR-PERSON` != "La Chalotais",]

texts_refer_to_actor <- QDC_es[QDC_es$`ACTOR-TEXT` %in% ties_sent_to_actor_after_17635$`ACTOR-TEXT`,]
texts_refer_to_actor <- texts_refer_to_actor[texts_refer_to_actor$onset>=17635,]

length(unique(texts_refer_to_actor$`ACTOR-TEXT`))

#table(texts_refer_to_actor$`TIE-PERSON`)

list_text <- split(texts_refer_to_actor, f = texts_refer_to_actor$`ACTOR-TEXT`)

co_refs_texts <- sapply(list_text, function(x)  {
  "La Chalotais" %in% x[,"TIE-PERSON"]*1 +
    "Rousseau" %in% x[,"TIE-PERSON"]*1 +
    "D'Alembert" %in% x[,"TIE-PERSON"]*1 +
    "De l'éducation publique" %in% x[,"TIE-PERSON"]*1})

table(co_refs_texts)


# Persons

persons_refer_to_actor <- QDC_es[QDC_es$`ACTOR-PERSON` %in% ties_sent_to_actor_after_17635$`ACTOR-PERSON`,]

persons_refer_to_actor <- persons_refer_to_actor[persons_refer_to_actor$onset>=17635,]

length(unique(persons_refer_to_actor$`ACTOR-PERSON`))

#table(persons_refer_to_actor$`TIE-PERSON`)

#length(unique(persons_refer_to_actor$`ACTOR-PERSON`[persons_refer_to_actor$`TIE-PERSON`=="Rousseau"]))
#length(unique(persons_refer_to_actor$`ACTOR-PERSON`[persons_refer_to_actor$`TIE-PERSON`=="D'Alembert"]))
#length(unique(persons_refer_to_actor$`ACTOR-PERSON`[persons_refer_to_actor$`TIE-PERSON`=="De l'éducation publique"]))

list_pers <- split(persons_refer_to_actor, f = persons_refer_to_actor$`ACTOR-PERSON`)

co_refs_persons <- sapply(list_pers, function(x)  {
  "La Chalotais" %in% x[,"TIE-PERSON"]*1 +
    "Rousseau" %in% x[,"TIE-PERSON"]*1 +
    "D'Alembert" %in% x[,"TIE-PERSON"]*1 +
    "De l'éducation publique" %in% x[,"TIE-PERSON"]*1})

table(co_refs_persons)
