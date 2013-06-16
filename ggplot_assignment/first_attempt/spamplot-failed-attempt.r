# Alex Roussos1 16 June 2013, for GADS4
# Data from http://www.dt.fee.unicamp.br/~tiago/smsspamcollection/
# Not fully implemented, see 'TODO's

rawData <- read.table("smsspamcollection//SMSSpamCollection", sep="\t", as.is=T, quote="")

# Maps contain a word and its count in spam or ham SMSs
spamMap <- new.env(hash=T, parent=emptyenv())
hamMap <- new.env(hash=T, parent=emptyenv())

# Main function
processData <- function() {
  # Populate the word counts of Ham and Spam
  apply(rawData, 1, processRow)
  
  # Spam and Ham maps now populated, get into a data frame for sorting
  spamCounts <- vector()
  for (key in ls(spamMap)) {
    append(spamCounts, spamMap[[key]])
  }
  # Build a frame with parallel vectors of word and count
  spamFrame = data.frame(Word=ls(spamMap), Counts=spamCounts)
  
  # TODO sort, then compute relative frequencies and plot
}

# Takes in a row (spam/ham and message) and adds the words to the correct map
processRow <- function(row) {
  # Split the sms into a vector of words
  type <- row[1]
  sms <- as.vector(as.matrix(row[2]))
  words <- unlist(strsplit(sms, " "))
  
  # Select correct map to add to (spam/ham)
  if (type == "spam") {
    map <- spamMap
  } else {
    map <- hamMap
  }
  
  # Add all the words in this SMS to the correct map 
  # TODO try: sapply(words, addWord, map=map)
  for (word in words) {
    addWord(word, map)
  }
}

# Add a word to the specified map (spam/ham) and increment count
addWord <- function(word, map) {
  if (!is.null(word) && length(word) > 0) {
    word <- tolower(word)
    if (is.null(map[[word]])) {
      map[[word]] <- 1
    } else {
      map[[word]] <- map[[word]] + 1
    }
  }
}
