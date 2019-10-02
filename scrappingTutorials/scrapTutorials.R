
### --- load packages
library(plyr); 
library(stringr);
library(tm); 
library(wordcloud)

### --- load data

library(readxl)
UseRtutorials <- read_excel("UseRtutorials.xlsx")
table(UseRtutorials$Year)

## ----regexClean----------------------------------------------------------

colnames(UseRtutorials)

head(UseRtutorials$Title)
head(UseRtutorials$Description)

titles <- UseRtutorials$Title
# titles <- gsub("@\\w+", "", titles) # Replace @UserName
# titles <- gsub("[[:punct:]]", "", titles) # Remove punctuation
titles <- gsub("[ |\t]{2,}", "", titles) # Remove tabs
# titles<-  gsub( '[^A-z0-9_]', ' ', tweets.text) # remove codes that are neither characters nor digits
# titles <- tolower(titles) # Set characters to lowercase
titles <- gsub("rt", "", titles) # Replace blank space
titles <- gsub("^ ", "", titles) # Remove blank spaces at the beginning
titles <- gsub(" $", "", titles) # Remove blank spaces at the end
head(titles)

##--- prepare groups of years
years <- 2010:2019
groupsOfYears <- list()
groupsOfTitles <- list()
windowSize <- 5
numYearsInGroup <- length(years)- windowSize
pdf("wordClouds.pdf")
for (i in 1:windowSize){
  groupsOfYears <- years[i]:years[i+numYearsInGroup]
  groupsOfTitles<- titles[UseRtutorials$Year %in% groupsOfYears]
  titles.text.corpus <- Corpus(VectorSource(groupsOfTitles)) #create corpus
  titles.text.corpus <- tm_map(titles.text.corpus, function(x) removeWords(x, stopwords())) # remove stopwords
  dtm <- TermDocumentMatrix( titles.text.corpus)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  cat("Years analyzed: ", groupsOfYears,"\n")
  wordcloud (words = d$word, freq = d$freq, # titles.text.corpus, 
            min.freq = 2, scale=c(7,0.5),
            colors=brewer.pal(8, "Dark2"),random.color= TRUE, random.order = FALSE, 
            max.words = 200)
}
dev.off()


