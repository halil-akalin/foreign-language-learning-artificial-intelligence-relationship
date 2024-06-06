# Data processing phase
#Accessing the data set
unorganizedData <- readLines("D:/Code/R/socialMediaWebAnalysis/dataExtractionPhase/comments_output.txt")
#<----------------------------------------------------------------------------------------------------->

# We load the "tm" package to edit the text.
library(tm)

#Cleaning of spaces
cleaned_text <- gsub("\\s+", " ", merged_text)
#Converting uppercase letters to lowercase letters
cleaned_text <- tolower(cleaned_text)
#Numbers are translated into words
cleaned_text <- gsub("\\b\\d+\\b", "number", cleaned_text)
#Special characters are cleared
cleaned_text <- gsub("[^[:alpha:] ]", "", cleaned_text)
#Saving the cleaned text to a new txt file
writeLines(cleanedText, "cleanedDataSet.txt")
#<----------------------------------------------------------------------------------------------------->

setwd("D:/Code/R/socialMediaWebAnalysis/dataEditingPhase")
file_path <- "cleanedDataSet.txt"
text_content <- readLines(file_path)
#Tokenized process
library(tokenizers)
#Function written to tokenize text
tokenized_text <- function(text) {
  tokens <- unlist(tokenize_words(text))
  return(tokens)
}
tokens <- lapply(text_content, tokenized_text)
print(tokens)
#<----------------------------------------------------------------------------------------------------->

#Stop words cleaning process
library(tidytext)
#Create a data frame
data_reddit <- data.frame(tokens = unlist(tokens))
#Stop words list
custom_stop_words <- c("i", "me", "my", "myself", "we", "our", "ours", "ourselves", "you", "your", "yours", "yourself", "yourselves",
                       "he", "him", "his", "himself", "she", "her", "hers", "herself", "it", "its", "itself", "they", "them",
                       "their", "theirs", "themselves", "what", "which", "who", "whom", "this", "that", "these", "those", "am",
                       "is", "are", "was", "were", "be", "been", "being", "have", "has", "had", "having", "do", "does", "did",
                       "doing", "a", "an", "the", "and", "but", "if", "or", "because", "as", "until", "while", "of", "at",
                       "by", "for", "with", "about", "against", "between", "into", "through", "during", "before", "after",
                       "above", "below", "to", "from", "up", "down", "in", "out", "on", "off", "over", "under", "again",
                       "further", "then", "once", "here", "there", "when", "where", "why", "how", "all", "any", "both",
                       "each", "few", "more", "most", "other", "some", "such", "no", "nor", "not", "only", "own", "same",
                       "so", "than", "too", "very", "s", "t", "can", "will", "just", "don", "should", "now", "d", "ll", "m",
                       "o", "re", "ve", "y", "ain", "aren", "couldn", "didn", "doesn", "hadn", "hasn", "haven", "isn", "ma",
                       "mightn", "mustn", "needn", "shan", "shouldn", "wasn", "weren", "won", "wouldn", "im", "get", "thot", "us")
#Clearing phase of specified stop words in the data frame
cleaned_tokens <- data_reddit %>%
  filter(!tokens %in% custom_stop_words)
print(cleaned_tokens)
#<----------------------------------------------------------------------------------------------------->

#Conjunction cleaning phase
library(tidytext)
#Conjunction list
conjunctions <- c("and", "or", "but", "nor", "so", "for", "yet", "because", "eventually", "therefore", "finally", 
                  "since", "unless", "if", "after", "while", "then", "during", "until", "hence", "as", "only", "oh", 
                  "wherever", "however", "before", "when", "whenever", "though", "although", "nonetheless", "moreover", "besides", "also", "be")
#Clearing phase of specified conjunction in the data frame
cleaned_tokens <- cleaned_tokens %>%
  filter(!tokens %in% conjunctions)
print(cleaned_tokens)

#Printing data cleared of stop words and conjunctions to txt file
cleaned_data <- cleaned_tokens
output_file <- "D:/Code/R/socialMediaWebAnalysis/dataEditingPhase/cleanedStopWordsAndConjunctions.txt" 
writeLines(cleaned_data$tokens, output_file)
#<----------------------------------------------------------------------------------------------------->