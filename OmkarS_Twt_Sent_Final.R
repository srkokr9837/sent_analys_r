# •	Geocoded Tweets Data Joined with Author Bing Liu Words Data
#   o	His data contains a list of words used in all his books and categorized them based on sentiment (i.e positive or negative etc.).
#   o	I joined this data with Geocoded Tweets data.
#   o	Using this joined data, I filtered words under each sentiment category and counted their avg. frequency.
#   o	Then I filtered tweets’ words based on sentiment category AND states where the Tweets were made: for this dataset, I computed the frequency for positive sentiment words in New Hampshire and Nevada.
#   o	Then, I plotted the ratio of positive to negative words in the joined dataset.
#   o	I found that Missouri had the highest positive/negative word ratio and Louisiana had the lowest.
#   o	I calculated that Missouri’s positive/negative ratio was 33% higher than Louisiana’s.

# •	Geocoded Tweets Data Joined with Author Donna Loughran Words Data
#   o	Did the same analysis as I did for Bing Liu Except with more sentiment categories:
#     ♣	Negative
#     ♣	Positive
#     ♣	Constraining
#     ♣	Litigious
#     ♣	Superfluous
#     ♣	Uncertainty
#   o	The states I focused on were negative sentiments in New Jersey and Arizona
#   o	When I plotted the states’ positive/negative ratio, I found that Maine had the highest ratio and Louisiana had the lowest again.
#   o	Missouri Tweets had the 14th highest when joined with Liu’s data, illustrating there is some difference in author’s sentiment choice for words used.
#   o	But perhaps, there are more categories I included with Loughran’s data, meaning that a smaller sample size of tweet words belonged to the positive and negative categories.
#   o	The Bing Liu data had more words, meaning it was a larger sample size So the results from his data set joined with Twitter data is more likely to be reliable.
#   o	There is consistency though in how Louisiana had the lowest ratio again.

# Packages used: rio, broom, tidytext, tidyr, dplyr, ggplot2

install.packages("rio")
library("rio")
geocoded_tweets=import("/Users/omkarsreekanth/Documents/FT-20X/Twt_Sent_An/gcod_twts.rda")


# Install packages
install.packages("broom")
library("dplyr")

install.packages("tidytext")
library("tidytext")

# With inner join, implement sentiment analysis using AUTHOR BING LIU
bing <- get_sentiments("bing")

joined_tbl_bing = geocoded_tweets %>%
  
  inner_join(bing)

# NEGATIVE SENTIMENTS COUNTS WHEN INNER JOINING TWEETS WITH 'BING':

neg_words_bing = joined_tbl_bing %>%
  # Filter to only choose the words associated with sadness
  filter(sentiment =="negative") %>%
  # Group by word
  group_by(word) %>%
  # Use the summarize verb to find the mean frequency
  summarize(freq = mean(freq)) %>%
  # Arrange to sort in order of descending frequency
  arrange(desc(freq))

# POSITIVE SENTIMENTS COUNTS WHEN INNER JOINING TWEETS WITH 'BING':
pos_words_bing <- joined_tbl_bing %>%
  # Filter to choose only words associated with joy
  filter(sentiment == "positive") %>%
  # Group by each word
  group_by(word) %>%
  # Use the summarize verb to find the mean frequency
  summarize(freq = mean(freq)) %>%
  # Arrange to sort in order of descending frequency
  arrange(desc(freq))


pos_twts_nh_bing = joined_tbl_bing %>%
  # Find only the words for the state of New Hampshire and associated with positive
  filter(state == "new hampshire",
         sentiment == "positive") %>%
  # Arrange to sort in order of descending frequency
  arrange(desc(freq))

pos_twts_nv_bing = joined_tbl_bing %>%
  # Find only the words for the state of Nevada and associated with positive
  filter(state == "nevada",
         sentiment == "positive") %>%
  # Arrange to sort in order of descending frequency
  arrange(desc(freq))


install.packages("tidyr")
library("tidyr")

install.packages("ggplot2")
library("ggplot2")

# Plotted positive vs negative ratio of words based on joined table
pos_neg_ratios_bing = joined_tbl_bing %>% 
  # Group by two columns: state and sentiment
  group_by(state,sentiment) %>%
  # Use summarize to calculate the mean frequency for these groups
  summarize(freq = mean(freq)) %>%
  spread(sentiment, freq) %>%
  ungroup() %>%
  # Calculate the ratio of positive to negative words
  mutate(ratio = positive / negative,
         state = reorder(state, ratio)) %>%
  # Use aes() to put state on the x-axis and ratio on the y-axis
  ggplot(aes(state,ratio)) +
  # Make a plot with points using geom_point() 
  geom_point() + coord_flip()

plot(pos_neg_ratios_bing)


# WHEN INNER JOINING TWEETS WITH AUTHOR DONNA LOUGHRAN'S WORDS:

lou <- get_sentiments("loughran")

joined_tbl_lou = geocoded_tweets %>%
  # With inner join, implement sentiment analysis using `lou`
  inner_join(lou)

# NEGATIVE SENTIMENTS COUNTS WHEN INNER JOINING TWEETS WITH 'lou':

neg_words_lou = joined_tbl_lou %>%
  # Filter to only choose the words associated with negative
  filter(sentiment =="negative") %>%
  # Group by word
  group_by(word) %>%
  # Use the summarize verb to find the mean frequency
  summarize(freq = mean(freq)) %>%
  # Arrange to sort in order of descending frequency
  arrange(desc(freq))

# POSITIVE SENTIMENTS COUNTS WHEN INNER JOINING TWEETS WITH 'lou':
pos_words_lou <- joined_tbl_lou %>%
  # Filter to choose only words associated with positive
  filter(sentiment == "positive") %>%
  # Group by each word
  group_by(word) %>%
  # Use the summarize verb to find the mean frequency
  summarize(freq = mean(freq)) %>%
  # Arrange to sort in order of descending frequency
  arrange(desc(freq))

# constraining SENTIMENTS COUNTS WHEN INNER JOINING TWEETS WITH 'lou':
const_words_lou <- joined_tbl_lou %>%
  # Filter to choose only words associated with constraining
  filter(sentiment == "constraining") %>%
  # Group by each word
  group_by(word) %>%
  # Use the summarize verb to find the mean frequency
  summarize(freq = mean(freq)) %>%
  # Arrange to sort in order of descending frequency
  arrange(desc(freq))

# litigious SENTIMENTS COUNTS WHEN INNER JOINING TWEETS WITH 'lou':
antic_words_lou <- joined_tbl_lou %>%
  # Filter to choose only words associated with litigious
  filter(sentiment == "litigious") %>%
  # Group by each word
  group_by(word) %>%
  # Use the summarize verb to find the mean frequency
  summarize(freq = mean(freq)) %>%
  # Arrange to sort in order of descending frequency
  arrange(desc(freq))

# superfluous SENTIMENTS COUNTS WHEN INNER JOINING TWEETS WITH 'lou':
disg_words_lou <- joined_tbl_lou %>%
  # Filter to choose only words associated with superfluous
  filter(sentiment == "superfluous") %>%
  # Group by each word
  group_by(word) %>%
  # Use the summarize verb to find the mean frequency
  summarize(freq = mean(freq)) %>%
  # Arrange to sort in order of descending frequency
  arrange(desc(freq))

# uncertainty SENTIMENTS COUNTS WHEN INNER JOINING TWEETS WITH 'lou':
fear_words_lou <- joined_tbl_lou %>%
  # Filter to choose only words associated with uncertainty 
  filter(sentiment == "uncertainty") %>%
  # Group by each word
  group_by(word) %>%
  # Use the summarize verb to find the mean frequency
  summarize(freq = mean(freq)) %>%
  # Arrange to sort in order of descending frequency
  arrange(desc(freq))


neg_twts_nj_lou = joined_tbl_lou %>%
  # Find only the words for the state of New Jersey and associated with negative
  filter(state == "new jersey",
         sentiment == "negative") %>%
  # Arrange to sort in order of descending frequency
  arrange(desc(freq))

neg_twts_az_lou = joined_tbl_lou %>%
  # Find only the words for the state of Arizona and associated with negative
  filter(state == "arizona",
         sentiment == "negative") %>%
  # Arrange to sort in order of descending frequency
  arrange(desc(freq))



# tweets_bing has been pre-defined

library("tidyr")


library("ggplot2")


pos_neg_ratios_lou = joined_tbl_lou %>% 
  # Group by two columns: state and sentiment
  group_by(state,sentiment) %>%
  # Use summarize to calculate the mean frequency for these groups
  summarize(freq = mean(freq)) %>%
  spread(sentiment, freq) %>%
  ungroup() %>%
  # Calculate the ratio of positive to negative words
  mutate(ratio = positive / negative,
         state = reorder(state, ratio)) %>%
  # Use aes() to put state on the x-axis and ratio on the y-axis
  ggplot(aes(state,ratio)) +
  # Make a plot with points using geom_point() 
  geom_point() + coord_flip()

plot(pos_neg_ratios_lou)

# Sources Used: Datacamp, Quartz, https://www.tidytextmining.com/sentiment.html


