## Biodiversity Observations Miner (outside shiny environment. Working version )
## Gabriel Munoz (fgabriel1891@gmail.com/gabriel.munoz@concordia.ca)
## Aug 2019 


## read vocabularies 
insectTraits <-  read.csv("Ontologies/InsectRelatedTraits.csv", header = T, stringsAsFactors = F)
bioticInteractionsTerms <- read.csv("Ontologies/BasicBioticInteractions.csv", header = T, stringsAsFactors = F)
#######

source("Functions.R")

### Start search

#BatchSearchPlos("ecuador") ## Case Example showed at IBS conference

mined3 <- readRDS("ecuador.RDS") ## Load the RDS file 
textMine <- mined3$value

## Annotate the text (time consuming, can't be parallelized :/)

AnnotText <- lapply(1:length(mined3$value), 
                    function(x) 
                      annotateText(mined3[x,],
                                   udpipe_load_model("Models/english-ewt-ud-2.3-181115.udpipe")))

anotaMine(AnnotText[[1]], 4)
mined3$value <- droplevels(mined3$value)

## Start paralellization from this point onwards
library(snowfall)

sfInit(parallel=TRUE, cpus=100, type="SOCK") 
sfSource("Functions.R")
# Export the annotated text object ot the server
sfExport("AnnotText")
sfExport("mined3")
sfExport("insectTraits")
sfExport("bioticInteractionsTerms")
sfExport("tags")
## Time consuming step, even when paralellized // so far running for 18 hrs 

tags <- snowfall::sfLapply(1:length(AnnotText), function(x) anotaMine(AnnotText[[x]],skipgram = 4))

names(tags) <- mined3$L1

traitTags <- snowfall::sfLapply(1:length(tags), 
                                function(x) 
                                  unique(insectTraits$Traits
                                         [insectTraits$Term %in% tags[[x]]$term1]))

bioticTags <- snowfall::sfLapply(1:length(tags), 
                                function(x) 
                                  unique(bioticInteractionsTerms$InteractionName
                                         [bioticInteractionsTerms$term %in% tags[[x]]$term1]))



sfStop()

### End paralellization 


prunnedTags <- traitTags[which(!sapply(1:length(traitTags), function(x) length(traitTags[[x]])) == 0)]

prunnedTagsBiot <- bioticTags[which(!sapply(1:length(bioticTags), function(x) length(bioticTags[[x]])) == 0)]


names(prunnedTags) <- which(!sapply(1:length(traitTags), function(x) length(traitTags[[x]])) == 0)
names(prunnedTagsBiot) <- which(!sapply(1:length(bioticTags), function(x) length(bioticTags[[x]])) == 0)




pTag <- reshape2::melt(prunnedTags)
pTag <- cbind(insectTraits[match(pTag$value, insectTraits$Traits),], pTag)



pTag2 <- reshape2::melt(prunnedTagsBiot)
pTag2 <- cbind(bioticInteractionsTerms[match(pTag2$value, bioticInteractionsTerms$InteractionName),], pTag2)


head(pTag2)
write.csv(pTag, "pTagInsectTraits.csv", row.names = F)
write.csv(pTag2, "pTagBiotInter.csv", row.names = F)


#############
plot(log(summary(as.factor(mined3$L1))))
plot(log(summary(as.factor(mined3$ordo))))

log(summary(as.factor(mined3$clade)))


## How to recover searchs from cache.. only in emergencies... disregard...
# ## Ecuador 
# 
# sear2 <- c()
# 
# for(i in 1:length(sear)){ 
#   
#   print(paste("Working on file", i))
#   sear2[i] <- ft_get(sear[[i]]$data$id)
#   Sys.sleep(15)
#   cat("\014") 
# }
# 
# 
# sear3 <- unlist(sapply(1:length(sear2), function(z) 
#   sapply(1:length(sear2[[z]]$data$path), 
#          function(k) sear2[[z]]$data$path[[k]]$path)))
# 
# sear3 <- paste0("/Users/Gabriel/Library/Caches/R/fulltext/",list.files("/Users/Gabriel/Library/Caches/R/fulltext/"))
# red <- c()
# 
# for (i in 1:length(sear3)){ 
#   print(paste("File number",i))
#   red[i] <- tryCatch({pub_chunks(sear3[i], c("body"))}, error = function(e){""})
#   cat("\014") 
# }
# 
# 
# # Pullout the body
# body <- lapply(1:length(red), function(x) red[x]$body)
# body <- lapply(body, function(x) paste(x, collapse = ""))
# 
# #body <- sapply(1:length(red), function(x) paste0(red[x][[1]], collapse = ""))
# 
# ###### Corpus onwards
# 
# ### Scientific names per article 
# nam <- lapply(body[-which(body == "")], function(x) taxize::scrapenames(text = x)$data)
# nam <- reshape2::melt(nam)
# names(nam) <- c("verbatim","scientificname","variable","value","doi")
# 
# nam2 <- lapply(1:length(nam), function(x) reshape2::melt(nam[[x]]))
# nam2 <- reshape2::melt(nam2)
# ## How many records:
# 
# summary(nam2$variable)
# 
# ### Get taxonomic info from results 
# 
# ### How many species? 
# sp = unique(nam2$scientificname)
# length(sp)
# 
# 
# body2 <- body[-which(body == "")]
# spp <- lapply(unique(mined3$L1), function(x) wikitaxa::wt_wikispecies(x))
# class <- lapply(1:length(spp), function(x)   spp[[x]]$classification)
# names(class) <- unique(mined3$L1)
# 
# 
# ssa <- sapply(1:length(class), function(z) class[[z]][class[[z]]$rank == "Ordo",])
# ssb <- rbindlist(ssa, fill = T, idcol = "name", use.names = T)
# names(ssb) <- c("spName", "rank", "orderName")
# ssb$spName <- names(class)[(ssb$spName)]
# mined3$ordo <- ssb$orderName[match(mined3$L1, ssb$spName)]
# 
# 
# ssc <- sapply(1:length(class), function(z) class[[z]][class[[z]]$rank == "Subordo",])
# ssc <- rbindlist(ssc, fill = T, idcol = "name", use.names = T)
# names(ssc) <- c("spName", "rank", "orderName")
# ssc$spName <- names(class)[(ssc$spName)]
# mined3$clade <- ssc$orderName[match(mined3$L1, ssc$spName)]
# 
# 

