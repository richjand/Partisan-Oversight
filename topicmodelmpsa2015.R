library(stm)
library(readr)
library(stringr)
library(dplyr)

p <- read_csv('C:/Users/richjand/Dropbox/Diss stuff/partisan oversight/Data/osgovreformpresent105-112.csv')
md <- read_csv("C:/Users/richjand/Dropbox/Diss stuff/uniquehearingmetadata.csv")
hearings <- unique(p$hearingid)
hearings <- hearings[!(hearings %in% c('CHRG-108hhrg90751','CHRG-108hhrg90752', 'CHRG-108hhrg90753', 
                                       'CHRG-109hhrg22684','CHRG-109hhrg24134','CHRG-112hhrg75521',
                                       'CHRG-112hhrg75522','CHRG-112hhrg75523','CHRG-112hhrg76367',
                                       'CHRG-112hhrg77683'))]
hearings = hearings[!(grepl("CHRG-104",hearings))]
isup <- function(x){
  x==toupper(x)
}

text <- ''

for (i in 1:length(hearings)){ ##Load raw hearing texts from external drive
  print(i)
  
  t <- read_lines(paste("E:/Hearings/GPO/",hearings[i],'.htm', sep = ''))
  begin <- grep("^(    |     )(Mr|Mrs|Ms|Dr)\\. ([A-Za-z]*|[A-Za-z]*-[A-Za-z]*|[A-Za-z]* [A-Za-z]*|[A-Za-z]*[[:punct:]]*[A-Za-z]*)\\.", t)[1] ##Begins with first time a speaker is recognized##
  if ((length(t)- begin) > 100){
  sub <- t[begin:(begin+100)]} else {sub <- t} ##Take first 100 words from hearing##
  
  ##CLEAN TEXT##
  notitles = str_replace_all(sub, pattern = "^(    |     )(Mr|Mrs|Ms|Dr)\\. ([A-Za-z]*|[A-Za-z]*-[A-Za-z]*|[A-Za-z]* [A-Za-z]*|[A-Za-z]*[[:punct:]]*[A-Za-z]*)\\.", '')
  nocaps = notitles[isup(notitles)==F]
  noboiler = str_replace_all(nocaps, "Thank you|thanks|thank you|thank|Statement|statement|will|order|hearing|Hearing|Chairman|chairman|member|Committee|committee|Subcommittee|subcommittee",'')
  text[i] = str_c(noboiler, collapse = '')
}

data <- left_join(x = data.frame(text,hearingid = hearings), y = md)
data$divided = ifelse(data$congress %in% c(104, 105, 106, 110, 112, 113),1,0)
data$republican <- ifelse(data$congress %in% c(104,105,106,107,108,109,112, 113),1,0)

##Data processed, ready to analyze##
processed <- textProcessor(data$text, metadata=data)
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
docs <- out$documents
vocab <- out$vocab
meta  <-out$meta

textfit30r <- stm(out$documents,out$vocab,K=30,
                 prevalence =~ divided+ s(congress)+ republican,
                 data=out$meta,seed=5926696)


plot.STM(textfit30r,type="labels")
meta$divided<-as.factor(meta$divided)
prep <- estimateEffect(1:30 ~ divided + congress + republican,textfit30r,
                       meta=meta, uncertainty = "Global")

labelTopics(textfit30r, topics = c(3,6,8,11,12,14,22))

pdf("C:/Users/richjand/Dropbox/Diss stuff/partisan oversight/stm_effects.pdf")
plot.estimateEffect(prep, 'divided', model = 'textfit30r', method = 'difference',
                    cov.value1=1,cov.value2=0, verbose.labels = F, topics = c(3,6,8,11,12,14,22),
                    labeltype = "custom", 
                    xlab = 'Effect of Divided Government',
                    custom.labels = c('Investigations',
                                      'Government Management',
                                      'Law & Crime',
                                      'Defense',
                                      'Homeland Security',
                                      'Implementation',
                                      'Stimulus'), 
                    xlim = c(-.1,.1))
dev.off()

pdf("C:/Users/richjand/Dropbox/Diss stuff/partisan oversight/stm_topics.pdf")
plot.STM(textfit30r,type="summary")
dev.off()