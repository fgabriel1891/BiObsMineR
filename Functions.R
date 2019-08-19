### Tryouts to programatically find knowledge gaps and global distribution of knowldege 

## Gabriel Muñoz 
# Jun 2019 

# install.packages("fulltext")
# install.packages("pubchunks")
# install.packages("taxize")
# install.packages("wikitaxa")

# load libraries

#library(fulltext)
library(pubchunks)
library(taxize)
library(wikitaxa)
library(data.table)
library(dplyr)
library(purrr)
library(tibble)
library(udpipe)
library(tidytext)

### Search and extract 

### Function to perform a batch search in PLOS using the maximun API limits. if hardSave equals T, the resulting object will also be saved in the directory as a RDS file. 

BatchSearchPlos <- function(searchterm, hardSave = F){
  
  ssa <- ft_search(searchterm, from = "plos")$plos$found
  sear <- c()
  sequ <- seq(ssa, 11)
  for(i in 1:length(sequ)){ 
    
    print(i)
    sear[i] <- ft_search(searchterm, 
                         from = "plos", 
                         plosopts = list("start" = sequ[i]))
    Sys.sleep(15)
    cat("\014") 
  }
  
  sear2 <- c()
  
  for(i in 1:length(sear)){ 
    
    print(i)
    sear2[i] <- ft_get(sear[[i]]$data$id)
    Sys.sleep(15)
    cat("\014") 
  }
  
  
  
  if(hardSave == T ){
    
    # hard save result 
    saveRDS(sear2, paste0(searchterm,".RDS"))
    
  }
  
  return(sear)
  
}


## Function to get the text snipped around scientific names__from BOM__
giveContext <- function(text,terms, up, down) {
  
  gsub <- gsub("\\)", "",terms)
  gsub <- gsub("\\(", "", gsub)
  indx <- unlist(gregexpr(gsub, text, fixed = T))
  cont = sapply(indx, function(x) stringr::str_sub(text, x-up,x+down))
  names(cont) <- names(text) 
  return(cont)}




mineArt <- function(body, database){ 
  mined <- lapply(1:length(database),
                  function(x) 
                    lapply(unique(database[[x]]$verbatim),
                           function(y) 
                             giveContext(body[x], y ,150,150)))
  
  
  for (z in 1:length(mined)){  
    names(mined[[z]]) <- database[[z]]$scientificname[match(unique(database[[z]]$verbatim), 
                                                            database[[z]]$verbatim)]
    
  }
  mined <- lapply(1:length(mined), function(t) reshape2::melt(mined[[t]]))
  mined <- rbindlist(mined, fill = T)
  return(mined)
  
}


## Function to find word coocurrence vectors calculate and normalized probability between pair of words (option to filter with dictionary matches)

getCoOcu = function(x, dictio, filter = F, skipgram = 5){
  
  
  
  stats <- cooccurrence(x = x$lemma, 
                        relevant = x$pos %in% c("NOUN", "VERB","ADJ"), 
                        skipgram = skipgram)
  
  if (filter == T) {
    dictio = read.csv(dictio,
                      header = TRUE, 
                      stringsAsFactors = F)
    
    match1  = unlist(sapply(dictio$dictionary, 
                            function(x)grep(x,stats$term1)))
    match2 = unlist(sapply(dictio$dictionary, 
                           function(x)grep(x,stats$term2)))
    match = unique(c(match1, match2))
    print(match2)
    print(match)
    stats = stats[match,]
  }
  else{stats = stats}
  
  stats[order(stats$cooc,stats$term1, decreasing = T),]
}


## Function to annotate text based on a given udpipe model

annotateText <- function(text, modelpath){
  model = udpipe_load_model(modelpath)
  
  x = udpipe_annotate(model,
                      x = text$value,
                      doc_id = text$L1)
}


###  Wrapper to get word coocurrence vectors fro annotated text

anotaMine <- function(anotatedText, skipgram){ 
  
  
  
  if (nchar(as.character(anotatedText$x)) > 0){ 
    
    coOc <- getCoOcu(as.data.frame(anotatedText), filter = F, skipgram)
    
    coOc <-coOc[!coOc$term1 %in% stopwords::stopwords("en") & !coOc$term2 %in% stopwords::stopwords("en"),]
    
  } 
  
  else{ 
    coOc <-  list("term1" = "", "term2" = "", "cooc" = "")
    
  }
  
  return(coOc)
  
}


### Helper fucntion to subset text strings 

subs = function(x, size, offset){
  nc    = nchar(x)
  first = seq(1, nc-size+1L, by=offset)
  last  = first + size -1L
  substring(x, first, last)
}






