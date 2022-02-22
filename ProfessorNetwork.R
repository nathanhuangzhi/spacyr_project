library(spacyr)

# -- need to do these before the next function will work:
# spacy_install()
# spacy_download_langmodel("en_core_web_trf")
spacy_initialize(model = "en_core_web_trf")

library(pdftools)
pdf_data = pdf_text(pdf = "https://www.gsb.stanford.edu/sites/default/files/faculty/cv/cv-athey-susan-carleton-2021-mar.pdf")
parsedtxt <- spacy_parse(pdf_data, lemma = FALSE, entity = TRUE, nounphrase = TRUE)
possible_names = entity_extract(parsedtxt)
person_names = possible_names[possible_names$entity_type=="PERSON",]




# Get rid of inaccurate names
person_names$entity =  gsub(",", "", person_names$entity)
person_names$entity =  gsub(" ", "", person_names$entity)
person_names$entity =  gsub("__", "_", person_names$entity)
pattern = "_"
person_names = person_names[grep(pattern, person_names$entity),]
pattern = "[a-z]"
person_names = person_names[grep(pattern, person_names$entity),]
person_names$entity =  gsub("_\n_", "_", person_names$entity)
person_names = person_names[person_names$entity !="", ]
person_names$entity =  gsub("'", "", person_names$entity)
person_names$entity = gsub("_"," ", person_names$entity)



library(dplyr)
person_names = person_names %>% group_by(doc_id,sentence_id) %>% mutate(group = group_indices())
person_names = person_names %>% group_by(entity) %>% mutate(uid = group_indices())
person_names$uid = person_names$uid - 1

person_names = person_names %>% group_by(group) %>% distinct(entity, .keep_all=TRUE)

Nodes = person_names[,c(3,5)]
Nodes$size = 1
person_names = person_names %>% group_by(entity) %>% mutate(name_count = n())
person_names = person_names %>% group_by(group) %>% mutate(center_value = max(name_count))


group_center = person_names[person_names$name_count == person_names$center_value,]
group_center = group_center %>% group_by(group) %>% distinct(name_count,.keep_all = TRUE)
group_source = person_names[!person_names$uid %in% group_center$uid,]
group_center = group_center[,c(6,5,3)]
names(group_center)[c(1,3)] = c("target","target_name")

group_source = group_source[,c(6,5,8,3)]
names(group_source)[c(1,4)] = c("source","source_name")

Group = merge(group_source,group_center,by = "group")
Group$value = 1
names(Nodes)[1] = "name"




library(networkD3)


simpleNetwork(Group, Source = 'source_name', Target = 'target_name',zoom= TRUE)



