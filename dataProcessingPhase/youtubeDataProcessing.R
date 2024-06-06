setwd("D:/Code/R/socialMediaWebAnalysis/dataProcessingPhase")
getwd()

file_path <- "cleanedStopWordsAndConjunctions.txt"
text_content <- readLines(file_path)
#<------------------------------------------------------------------------------------------------------>

# Tokenized process
library(tokenizers)
# Function written to tokenize text
tokenized_text <- function(text) {
  tokens <- unlist(tokenize_words(text))
  return(tokens)
}
tokens <- lapply(text_content, tokenized_text)
print(tokens)
# The process of combining tokens
all_tokens <- unlist(tokens)

# Word frequency operation
word_freq <- table(all_tokens)
# Sorting word frequency from largest to smallest
sorted_word_freq <- sort(word_freq, decreasing = TRUE)
head(sorted_word_freq, 10)
#<------------------------------------------------------------------------------------------------------>

# Data visualization phase
library(wordcloud)
# Merge tokens
all_tokens <- unlist(tokens)
# Calculate word frequency
word_freq <- table(all_tokens)
# Sorting word frequency from largest to smallest
sorted_word_freq <- sort(word_freq, decreasing = TRUE)
# Creating a word cloud
wordcloud(words = names(sorted_word_freq), freq = sorted_word_freq, scale=c(3,0.5), min.freq = 15, colors=brewer.pal(8, "Dark2"))
#<------------------------------------------------------------------------------------------------------>

# Drawing phase of the bar chart of tokenized data
library(ggplot2)
print(sorted_word_freq)
# Creating a data frame with the 20 most used words in the dataset
top_words <- head(sorted_word_freq, 20)
df <- data.frame(word = names(top_words), Freq = as.numeric(top_words))
print(df)

# Plot a bar chart
ggplot(df, aes(x = Freq, y = reorder(word, Freq))) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  ggtitle("Frequencies of Most Used Words") +
  ylab("Kelime") +
  xlab("Frekans") +
  theme(axis.text.y = element_text(hjust = 1))
#<------------------------------------------------------------------------------------------------------>

library(tidytext)
library(dplyr)

# Steps to conduct sentiment analysis using the NRC sentiment dictionary
nrc <- get_sentiments("nrc")

# Creating a data frame for sentiment analysis
data_tidy_nrc <- data.frame(word = all_tokens) %>%
  inner_join(nrc, by = "word")

# Collecting sentiment scores
sentiment_scores_nrc <- data_tidy_nrc %>%
  count(sentiment, sort = TRUE)

# Show results
print(sentiment_scores_nrc)
#<------------------------------------------------------------------------------------------------------>
# Data visualization based on sentiment analysis
library(ggplot2)

# Calculating the sum of sentiment scores
total_n <- sum(sentiment_scores_nrc$n)

# Calculating percentage values
sentiment_scores_nrc <- sentiment_scores_nrc %>%
  mutate(percent = n / total_n * 100)

# Step to draw pie chart and add percentage values
ggplot(sentiment_scores_nrc, aes(x = "", y = n, fill = sentiment)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  scale_fill_brewer(palette = "Set3") +
  ggtitle("Emotion Distribution According to NRC Emotion Dictionary") +
  geom_text(aes(label = sprintf("%.1f%%", percent)), position = position_stack(vjust = 0.5))

#<------------------------------------------------------------------------------------------------------>

# Sentiment analysis using NRC sentiment dictionary
library(dplyr)
library(ggplot2)
library(tidytext)

# Let's choose the first two most repeated words for each emotion category
top_nrc_words <- data_tidy_nrc %>%
  count(word, sentiment) %>%
  group_by(sentiment) %>%
  top_n(1, n) %>%
  ungroup()

# and here is the bar graph drawing step
ggplot(top_nrc_words, aes(x = word, y = n, fill = sentiment)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3") +
  ggtitle("The first two words that occur most frequently for each emotion category") +
  xlab("Kelime") +
  ylab("Frekans")
#<------------------------------------------------------------------------------------------------------>

# First, we define our emotion and emoji matches as a list
library(tidyr)
sentiment_emoji <- list(
  "positive" = c("????", "????", "????"),
  "negative" = c("????", "????", "????"),
  "neutral" = c("????", "????"),
  "anger" = c("????", "????"),
  "anticipation" = c("????", "????"),
  "disgust" = c("????", "????"),
  "fear" = c("????", "????"),
  "joy" = c("????", "????"),
  "sadness" = c("????", "????"),
  "surprise" = c("????", "????"),
  "trust" = c("????", "????")
)

# Creating a data frame for sentiment analysis
data_tidy_nrc <- data.frame(word = all_tokens) %>%
  inner_join(nrc, by = "word")

# Assign the sentiment category for each word
data_tidy_nrc <- data_tidy_nrc %>%
  group_by(word) %>%
  summarize(sentiment = toString(unique(sentiment)))

# Choose a random emoji for each category
assign_emoji <- function(sentiments) {
  sentiments <- unlist(strsplit(sentiments, ", "))
  emojis <- unlist(lapply(sentiments, function(s) sample(sentiment_emoji[[s]], size = 1)))
  return(toString(emojis))
}

# Assign a random emoji to each word
data_tidy_nrc$emojis <- sapply(data_tidy_nrc$sentiment, assign_emoji)

# Convert the emoji column to a list and apply the unnest_tokens function
data_tidy_nrc$emojis <- strsplit(data_tidy_nrc$emojis, ", ")
emoji_freq <- data_tidy_nrc %>%
  unnest(emojis) %>%
  group_by(emojis) %>%
  summarise(Frequency = n()) %>%
  ungroup()

# Emojis are mapped to the emotion dictionary. Step to show the resulting weight distribution on a pie chart
library(ggplot2)

# Calculate the sum of emoji frequencies
total_freq <- sum(emoji_freq$Frequency)

# Calculating percentage values
emoji_freq <- emoji_freq %>%
  mutate(percent = Frequency / total_freq * 100)

# Step to draw pie chart and add percentage values
ggplot(emoji_freq, aes(x = "", y = Frequency, fill = emojis)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  theme(legend.position = "right") +
  ggtitle("Pie Chart According to Emoji Frequencies") +
  geom_label(aes(label = sprintf("%.1f%%", percent)), position = position_stack(vjust = 0.5), size = 3 * 0.9)
#<------------------------------------------------------------------------------------------------------>

library(ggplot2)
library(dplyr)

# Select the 20 most repeated emojis
top_20_emojis <- emoji_freq %>%
  arrange(desc(Frequency)) %>%
  head(20)

# Creating a horizontal bar chart of the 20 emojis with the highest frequency count
ggplot(top_20_emojis, aes(x = Frequency, y = reorder(emojis, Frequency))) +
  geom_bar(stat = "identity", fill = "#FD6467") +
  ggtitle("Frequencies of the 20 Most Used Emojis") +
  ylab("Emoji") +
  xlab("Frekans") +
  theme(axis.text.y = element_text(hjust = 1))
#<------------------------------------------------------------------------------------------------------>


# Draw emoji cloud
library(wordcloud)

# Creating emoji cloud using emoji frequencies
wordcloud(words = emoji_freq$emojis, freq = emoji_freq$Frequency, scale=c(3,0.5), min.freq = 15, colors=brewer.pal(8, "Set2"))
#<------------------------------------------------------------------------------------------------------>