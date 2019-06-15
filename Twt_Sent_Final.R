#I performed sentiment analysis on Geocoded Twitter Data. 
#The data consisted of about 500,000 geocoded tweets, and the data I used contained these variables: 
#a word, the state where the word(s) was used, and the frequency of the word appearing.  
#-Plotted frequencies of joy and sadness words using dplyr and ggplot2 packages 
#-Found the top 2 joy words are: love and good; top 2 sadness words are: hate and bad 
#-Calculated the ratio of positive to negative words in each state 
#-Found Missouri had the highest positive/negative word ratio and Louisiana had the lowest 
#-Calculated that Missouri’s positive/negative ratio was 33% higher than Louisiana’s  
# Packages used: rio, broom, tidytext, tidyr, ggplot2

install.packages("rio")
library("rio")
geocoded_tweets=import("gcod_twts.rda")


# Install packages
install.packages("broom")
library("dplyr")

install.packages("tidytext")
library("tidytext")


bing <- get_sentiments("bing")

joined_tbl_bing = geocoded_tweets %>%
  # With inner join, implement sentiment analysis using `bing`
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


# WHEN INNER JOINING TWEETS WITH NRC:

nrc <- get_sentiments("nrc")

joined_tbl_nrc = geocoded_tweets %>%
  # With inner join, implement sentiment analysis using `nrc`
  inner_join(nrc)

# NEGATIVE SENTIMENTS COUNTS WHEN INNER JOINING TWEETS WITH 'NRC':

neg_words_nrc = joined_tbl_nrc %>%
  # Filter to only choose the words associated with negative
  filter(sentiment =="negative") %>%
  # Group by word
  group_by(word) %>%
  # Use the summarize verb to find the mean frequency
  summarize(freq = mean(freq)) %>%
  # Arrange to sort in order of descending frequency
  arrange(desc(freq))

# POSITIVE SENTIMENTS COUNTS WHEN INNER JOINING TWEETS WITH 'NRC':
pos_words_nrc <- joined_tbl_nrc %>%
  # Filter to choose only words associated with positive
  filter(sentiment == "positive") %>%
  # Group by each word
  group_by(word) %>%
  # Use the summarize verb to find the mean frequency
  summarize(freq = mean(freq)) %>%
  # Arrange to sort in order of descending frequency
  arrange(desc(freq))

# ANGER SENTIMENTS COUNTS WHEN INNER JOINING TWEETS WITH 'NRC':
anger_words_nrc <- joined_tbl_nrc %>%
  # Filter to choose only words associated with anger
  filter(sentiment == "anger") %>%
  # Group by each word
  group_by(word) %>%
  # Use the summarize verb to find the mean frequency
  summarize(freq = mean(freq)) %>%
  # Arrange to sort in order of descending frequency
  arrange(desc(freq))

# ANTICIPATION SENTIMENTS COUNTS WHEN INNER JOINING TWEETS WITH 'NRC':
antic_words_nrc <- joined_tbl_nrc %>%
  # Filter to choose only words associated with anticipation
  filter(sentiment == "anticipation") %>%
  # Group by each word
  group_by(word) %>%
  # Use the summarize verb to find the mean frequency
  summarize(freq = mean(freq)) %>%
  # Arrange to sort in order of descending frequency
  arrange(desc(freq))

# Disgust SENTIMENTS COUNTS WHEN INNER JOINING TWEETS WITH 'NRC':
disg_words_nrc <- joined_tbl_nrc %>%
  # Filter to choose only words associated with disgust
  filter(sentiment == "disgust") %>%
  # Group by each word
  group_by(word) %>%
  # Use the summarize verb to find the mean frequency
  summarize(freq = mean(freq)) %>%
  # Arrange to sort in order of descending frequency
  arrange(desc(freq))

# fear SENTIMENTS COUNTS WHEN INNER JOINING TWEETS WITH 'NRC':
fear_words_nrc <- joined_tbl_nrc %>%
  # Filter to choose only words associated with fear
  filter(sentiment == "fear") %>%
  # Group by each word
  group_by(word) %>%
  # Use the summarize verb to find the mean frequency
  summarize(freq = mean(freq)) %>%
  # Arrange to sort in order of descending frequency
  arrange(desc(freq))

# JOY SENTIMENTS COUNTS WHEN INNER JOINING TWEETS WITH 'NRC':
joy_words_nrc <- joined_tbl_nrc %>%
  # Filter to choose only words associated with joy
  filter(sentiment == "joy") %>%
  # Group by each word
  group_by(word) %>%
  # Use the summarize verb to find the mean frequency
  summarize(freq = mean(freq)) %>%
  # Arrange to sort in order of descending frequency
  arrange(desc(freq))

# Sadness SENTIMENTS COUNTS WHEN INNER JOINING TWEETS WITH 'NRC':
sadness_words_nrc <- joined_tbl_nrc %>%
  # Filter to choose only words associated with sadness
  filter(sentiment == "sadness") %>%
  # Group by each word
  group_by(word) %>%
  # Use the summarize verb to find the mean frequency
  summarize(freq = mean(freq)) %>%
  # Arrange to sort in order of descending frequency
  arrange(desc(freq))

# SURPRISE SENTIMENTS COUNTS WHEN INNER JOINING TWEETS WITH 'NRC':
surp_words_nrc <- joined_tbl_nrc %>%
  # Filter to choose only words associated with surprise
  filter(sentiment == "surprise") %>%
  # Group by each word
  group_by(word) %>%
  # Use the summarize verb to find the mean frequency
  summarize(freq = mean(freq)) %>%
  # Arrange to sort in order of descending frequency
  arrange(desc(freq))

# Trust SENTIMENTS COUNTS WHEN INNER JOINING TWEETS WITH 'NRC':
trust_words_nrc <- joined_tbl_nrc %>%
  # Filter to choose only words associated with trust
  filter(sentiment == "trust") %>%
  # Group by each word
  group_by(word) %>%
  # Use the summarize verb to find the mean frequency
  summarize(freq = mean(freq)) %>%
  # Arrange to sort in order of descending frequency
  arrange(desc(freq))

pos_twts_vt_nrc = joined_tbl_nrc %>%
  # Find only the words for the state of Vermont and associated with positive
  filter(state == "vermont",
         sentiment == "positive") %>%
  # Arrange to sort in order of descending frequency
  arrange(desc(freq))

pos_twts_or_nrc = joined_tbl_nrc %>%
  # Find only the words for the state of Oregon and associated with positive
  filter(state == "oregon",
         sentiment == "positive") %>%
  # Arrange to sort in order of descending frequency
  arrange(desc(freq))



# tweets_bing has been pre-defined

library("tidyr")


library("ggplot2")


pos_neg_ratios_nrc = joined_tbl_nrc %>% 
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

plot(pos_neg_ratios_nrc)


# WHEN INNER JOINING TWEETS WITH LOUGHRAN:

loughran <- get_sentiments("loughran")

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

neg_twts_az_lou = joined_tbl_nrc %>%
  # Find only the words for the state of Arizona and associated with negative
  filter(state == "arizona",
         sentiment == "negative") %>%
  # Arrange to sort in order of descending frequency
  arrange(desc(freq))



# tweets_bing has been pre-defined

library("tidyr")


library("ggplot2")


pos_neg_ratios_nrc = joined_tbl_nrc %>% 
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

plot(pos_neg_ratios_nrc)

# Sources Used: Datacamp, Quartz, https://www.tidytextmining.com/sentiment.html


