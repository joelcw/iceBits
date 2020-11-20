#original: Friday 13th Nov 2020
#16 Nov 2020

library(tidyr)
library(tidyselect)
library(tidyverse)

#source(file="~/constantentropy/dormUido.R")
library(devtools)
source_url('https://raw.githubusercontent.com/joelcw/constantentropy/09543002e1eb28b4c9a1a83e4b08b67bcc258e79/dormUido.R')

foo <- read.delim(file="~/iceBits/ovCodingTreeAndClauseFreq.tsv",header = F,sep="\t")

#drop empty columns and name good columns
foo <- foo[,1:14]
colnames(foo) <- c("OV","ObjType","SbjType","Clause","NodeWords","NodeWords2","NodeString","TextId","Year","Genre", "TreeId","sentString","ClauseFreq","SentFreq")

#strip off extended labels from clause labels
foo$Clause <- str_extract(foo$Clause,"IP-[A-Z]{3}")

#strip response times of brackets, convert them to list of numbers by splitting
foo$ClauseFreq <- as.character(foo$ClauseFreq)
foo$SentFreq <- as.character(foo$SentFreq)
foo$ClauseFreq <- str_remove_all(foo$ClauseFreq, "[ \\[\\]]")
foo$SentFreq <- str_remove_all(foo$SentFreq, "[ \\[\\]]")
#Because R string functions are as unintuitive as humanly possible, in order to get this to actually return an indexable list of substrings
#you need to use simplify=T below, which returns a matrix, and then convert that into a vector or a list (list in this case because lists can be single
# elements in data frames but vectors can't for some reason), then make sure the vector/list is numeric, 
# and then you need to loop over every item so other dumb things don't happen...geez:
i = 1
for (i in 1:nrow(foo))
{
  foo$ClauseFreq[i] <- list(as.numeric(str_split(foo$ClauseFreq[i],",",simplify=T)))
  foo$SentFreq[i] <- list(as.numeric(str_split(foo$SentFreq[i],",",simplify=T)))
  
  #add 1 to all the frequencies to get rid of 0s, which won't be of any use to anyone
  foo$ClauseFreq[i][[1]] <- foo$ClauseFreq[i][[1]] + 1
  foo$SentFreq[i][[1]] <- foo$SentFreq[i][[1]] + 1
  
}

#Now, because each element has been coerced into a list, they need to be indexed in a really dumb way, e.g. fooNew$responseTimestamps[i][[1]]

#Convert to probability, and to bits. Then make columns for dorm, and dorm-uido:

foo$ClauseDorm <- 0
foo$ClauseDormUido <- 0
foo$SentDorm <- 0
foo$SentDormUido <- 0

i = 1
while (i <= nrow(foo))
  {
  
  foo$ClauseProb[i][[1]] <- foo$ClauseFreq[i][[1]]*(1/25000000)
  foo$ClauseInfo[i][[1]] <- log2(foo$ClauseProb[i][[1]])
  foo$ClauseDorm[i] <- dorm(foo$ClauseInfo[i][[1]], correct=TRUE)
  foo$ClauseDormUido[i] <- foo$ClauseDorm[i] - dorm(uido(foo$ClauseInfo[i][[1]]))
  
  foo$SentProb[i][[1]] <- (foo$SentFreq[i][[1]])*(1/25000000)
  foo$SentInfo[i][[1]] <- log2(foo$SentProb[i][[1]])
  foo$SentDorm[i] <- dorm(foo$SentInfo[i][[1]], correct=TRUE)
  foo$SentDormUido[i] <- foo$SentDorm[i] - dorm(uido(foo$SentInfo[i][[1]]))
  
  i = i+1
  
  }
