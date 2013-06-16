##########################
# Alex Roussos, 16 June 2013 for GADS4
# Creates a plot of how 'spammy' words are from text messages vs how often they appear.
# For readability, only a random sampling (plus the top 10 words) are shown.
# Based on code by Aaron Schumacher (see spamplot-failed-attempt.r for initial approach)
#
# Data from http://www.dt.fee.unicamp.br/~tiago/smsspamcollection/
##########################

##########################
# Processing
##########################

# load data
sms <- read.table('SMSSpamCollection', sep='\t', as.is=T, quote="")
names(sms) <- c("class", "text")

# load library
library(tm)

process <- function(charvec, minOccurs) {
  corpus <- Corpus(VectorSource(charvec))
  corpus <- tm_map(corpus, tolower)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  # TODO experiment with not using stopwords -- could be interesting to see if spam uses them 
  #    at a different frequency (bc maybe uses more shorthand?)
  corpus <- tm_map(corpus, removeWords, stopwords('english'))
  
  # stemming is annoying; omitting for now
  counts <- rowSums(as.matrix(TermDocumentMatrix(corpus)))
  counts <- subset(counts, counts > minOccurs) # only words that occur enough times to be significant
  freqs <- counts/sum(counts)
  return(data.frame(words=names(freqs), counts, percs=unname(freqs)))
}

ham <- process(subset(sms, class=='ham', select='text', drop=TRUE), 0)
spam <- process(subset(sms, class=='spam', select='text', drop=TRUE), 5)

all <- merge(ham, spam, by="words")
all$spamminess <- all$percs.y / all$percs.x
all$totalCounts <- all$counts.x + all$counts.y
all <- all[order(-all$spamminess),]

##########################
# Visualization
##########################

# Show the top spam words plus a random subset to make plot interesting
top <- all[1:10,]
sample <- all[sample(10:nrow(all), 10),]
sample <- rbind(top, sample)

# Plot with directlabel to avoid overlapping labels
library(ggplot2)
library(lattice)
library(directlabels)
labels <- as.character(sample$words)
png(filename="hw1-ggplot.png", width=600, height=480)
q<-qplot(sample$totalCounts, sample$spamminess) + geom_point(aes(colour=labels))
q <- direct.label(q)
q <- q + ylab("Spamminess")
q <- q + xlab("# Occurrences in All Messages")
q <- q + labs(title = "Spammy Words and their Frequencies")
q
dev.off()

# Make a wordcloud for fun
library(wordcloud)
png(filename="hw1-wordcloud.png", width=600, height=480)
wordcloud(all$words, all$spamminess, min.freq=10) # colors=all$totalCounts
dev.off()
