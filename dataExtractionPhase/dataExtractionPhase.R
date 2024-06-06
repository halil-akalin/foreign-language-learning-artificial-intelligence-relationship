# Data extraction stage
# Necessary libraries must be loaded
install.packages("httr")
install.packages("jsonlite")

library(httr)
library(jsonlite)
#<---------------------------------------------------------------------------------------------->
# Specify the path to the file containing the API Key
source("D:/Code/R/socialMediaWebAnalysis/apiKey.R")
#<---------------------------------------------------------------------------------------------->

# Specify the IDs of the videos
video_ids <- c("CYwG8LmjmI8", "5Jwtv1rRa-A", "SupKb2CSrlE", "nGMYhO9OD9w&t=1s")
# Assign all videos to a vector
all_comments <- c()
# YouTube Data v3 API can pull up to 50 comments. Every 50 comments are defined as 1 page. 
# Write a loop to assign all pages to a data frame
next_page_token <- ""

for (video_id in video_ids) {
  next_page_token <- ""
  video_comments <- c()
  
  while (!is.null(next_page_token)) {
    url <- paste0("https://www.googleapis.com/youtube/v3/commentThreads",
                  "?part=snippet",
                  "&videoId=", video_id,
                  "&maxResults=100", # Sayfa basina en fazla yorum sayisi
                  "&pageToken=", next_page_token,
                  "&key=", api_key)
    
    response <- httr::GET(url)
    comments <- httr::content(response, "parsed")
    
    # Receiving comments
    comments_data <- comments$items
    comments_content <- lapply(comments_data, function(x) x$snippet$topLevelComment$snippet$textDisplay)
    
    # Merge comments
    video_comments <- c(video_comments, comments_content)
    
    # Update pageToken for next page
    next_page_token <- comments$nextPageToken
  }
  
  # Let's add all comments to the all_comments list
  all_comments <- c(all_comments, list(video_comments))
}
#<---------------------------------------------------------------------------------------------->

# Combine all comments to create a character vector
all_comments_combined <- unlist(all_comments)

# Write character vector to file
writeLines(all_comments_combined, "comments_output.txt")
#<---------------------------------------------------------------------------------------------->

# Source
#https://www.youtube.com/watch?v=nGMYhO9OD9w&t=1s
#https://www.youtube.com/watch?v=CYwG8LmjmI8
#https://www.youtube.com/watch?v=SupKb2CSrlE
#https://www.youtube.com/watch?v=5Jwtv1rRa-A


