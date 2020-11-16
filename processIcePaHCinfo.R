#original: Friday 13th Nov 2020
#16 Nov 2020

library(tidyr)
library(tidyselect)
library(tidyverse)
library(stringdist)
#source(file="~/constantentropy/dormUido.R")
library(devtools)
source_url('https://raw.githubusercontent.com/joelcw/constantentropy/09543002e1eb28b4c9a1a83e4b08b67bcc258e79/dormUido.R')

foo <- read.delim(file="~/iceBits/ovCodingTreeAndClauseFreq.tsv",header = F,sep="\t")
colnames(foo) <- c("clauseFreq","sentFreq")

#strip response times of brackets, convert them to list of numbers by splitting
foo$clauseInfo <- str_remove_all(foo$clauseFreq, "[ \\[\\]]")
#Because R string functions are as unintuitive as humanly possible, in order to get this to actually return an indexable list of substrings
#you need to use simplify=T below, which returns a matrix, and then convert that into a vector or a list (list in this case because lists can be single
# elements in data frames but vectors can't for some reason), then make sure the vector/list is numeric, 
# and then you need to loop over every item so other dumb things don't happen...geez:
i = 1
for (i in 1:nrow(foo))
{
  foo$clauseFreq[i] <- list(as.numeric(str_split(foo$clauseFreq[i],",",simplify=T)))
  foo$sentFreq[i] <- list(as.numeric(str_split(foo$sentFreq[i],",",simplify=T)))
  
  #add 1 to all the frequencies to get rid of 0s, which won't be of any use to anyone
  foo$clauseFreq[i][[1]] <- foo$clauseFreq[i][[1]] + 1
  foo$sentFreq[i][[1]] <- foo$sentFreq[i][[1]] + 1
  
}

#Now, because each element has been coerced into a list, they need to be indexed in a really dumb way, e.g. fooNew$responseTimestamps[i][[1]]

#Convert to probability, and to bits. Then make columns for dorm, and dorm-uido:

foo$clauseDorm <- 0
foo$clauseDormUido <- 0
foo$sentDorm <- 0
foo$sentDormUido <- 0

i = 1
for (i in 1:nrow(foo))
  {
  
  foo$clauseInfo[i][[1]] <- log2(foo$clauseInfo[i][[1]]/25000000)
  foo$clauseDorm[i] <- dorm(foo$clauseInfo[i][[1]])
  foo$clauseDormUido[i] <- foo$clauseDorm[i] - dorm(uido(foo$clauseInfo[i][[1]]))
  
  }
