library(twitteR)
library(dplyr)
library(purrr)
library(lubridate)
library(stringr)
library(tidytext)

nrc <- sentiments %>%
  filter(lexicon == "nrc") %>% 
  dplyr::select(word, sentiment)

setup_twitter_oauth(consumer_key = "#################################", 
                    consumer_secret = "##############################",
                    access_token = "#################################",
                    access_secret = "################################")

# dataset 'bg' includes only recent mentions from Jan. 3 between 9am and 6pm EST

bg <- strip_retweets(searchTwitter("@RepGoodlatte", n=5000, since = "2017-01-03", until = "2017-01-04"), strip_manual = T)

bg <- tbl_df(map_df(bg, as.data.frame))

# dataset 'bg2' includes mentions from Jan. 2 between 9am and 6pm EST

bg2 <- strip_retweets(searchTwitter("@RepGoodlatte", n=5000, since = "2017-01-01", until = "2017-01-03"), strip_manual = T)

bg2 <- tbl_df(map_df(bg2, as.data.frame))

reg <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"

# I have to change the timezones because I'm a dumb American
bg$created <- as.POSIXct(bg$created, tz="GMT")
bg$created <- format(bg$created, tz="EST", usetz = T)

bg2$created <- as.POSIXct(bg2$created, tz="GMT")
bg2$created <- format(bg2$created, tz="EST", usetz = T)


bg2 <- subset(bg2, created >= "2017-01-02 09:00:00")

## NOTE: Twitter's API releases data in GMT, making my scrape of US-based data a little difficult. To combat these difficulties, I limited my dataset to mentions that occurred between 9am and 6pm on either day. If you have a better suggestion for this kind of analysis, or have found a way to pull Twitter data based in wider time periods, contact me please!

bg$created2 <- as.character(bg$created)
bg2$created2 <- as.character(bg2$created)

library(ggplot2)
library(scales)

bg$created2 <- gsub('^(\\d{4}-\\d{2}-\\d{2}\\s{1}\\d{2}):\\d{2}:\\d{2}\\s{1}EST$', '\\1', bg$created2)
bg2$created2 <- gsub('^(\\d{4}-\\d{2}-\\d{2}\\s{1}\\d{2}):\\d{2}:\\d{2}\\s{1}EST$', '\\1', bg2$created2)

# I created these data sets in order to tally the number of tweets per hour.

date <- bg %>%
  group_by(created2) %>%
  count()


date2 <- bg2 %>%
  group_by(created2) %>%
  count()

# Graphs showing the number of mentions that occur each hour

ggplot(date, aes(created2, n, group = 1)) +
  geom_line() +
  labs(x = "Day",
       y = "Number of Tweets", 
       color = "") +
  ggtitle("Tweets To Rep. Goodlatte, 1/3/17, By Hour") +
  scale_x_discrete(labels = c("9am", "10am", "11am", "12pm", "1pm", "2pm", "3pm", "4pm", "5pm", "6pm")) +
  annotate("text", x = 4, y = 470, label = "Peak: 470 mentions at 10am") +
  geom_line(aes(y = mean(n))) +
  geom_text(aes(x = 7, y = mean(n)+10, label = paste0("Average mentions = ", round(mean(n)))))


ggplot(date2, aes(created2, n, group = 1)) +
  geom_line() +
  labs(x = "Day",
       y = "Number of Tweets", 
       color = "") +
  ggtitle("Tweets To Rep. Goodlatte, 1/2/17, By Hour") +
  scale_x_discrete(labels = c("9am", "10am", "12pm", "2pm", "3pm", "4pm", "5pm", "6pm")) +
  annotate("text", x = 4.8, y = 14, label = "Peak: 14 mentions at 4pm") +
  geom_line(aes(y = mean(n))) +
  geom_text(aes(x = 4, y = mean(n)+.5, label = paste0("Average mentions = ", round(mean(n)))))


# number of unique people who had tweeted at each
length(unique(bg$screenName)) #1523
length(unique(bg2$screenName)) #25

# number of screennames who appeared in one database and not the other (so 5 tweeted both on 1/2 and 1/3)
discrep1 <- setdiff(bg$screenName, bg2$screenName) #1518
discrep2 <- setdiff(bg2$screenName, bg$screenName) #20

# filtering the individual words out of the tweet text

bg1a <- bg %>%
  filter(!str_detect(text, '^"')) %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = reg) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))

bg1a <- mutate(bg1a, ID = rownames(bg1a))

bg2a <- bg2 %>%
  filter(!str_detect(text, '^"')) %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = reg) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))

bg2a <- mutate(bg2a, ID = rownames(bg2a))

# number of unique words per data set
length(unique(bg1a$word)) #4124
length(unique(bg2a$word)) #180

# creating a tally of words per dataset
bg1b <- bg1a %>%
  group_by(word) %>%
  summarize(words = length(unique(ID)))

bg2b <- bg2a %>%
  group_by(word) %>%
  summarize(words = length(unique(ID)))

bg1c <- inner_join(bg1b, nrc, by = "word")

bg2c <- inner_join(bg2b, nrc, by = "word")

library(htmlTable)

t1 <- subset(bg1b, words >= 50)
t2 <- subset(bg2b, words >= 4)

write.csv(t1, file = "t1.csv")
write.csv(t2, file = "t2.csv")

# NOTE: I could NOT get these dataframes to appear as I wanted them to, so I exported them to excel then reimported them to coerce. htmlTables is my kryptonite. sloppy, but it is what it is.

t1 <- read.csv("t1.csv")
t2 <- read.csv("t2.csv")

htmlTable(t1, header = c("Word", "Occurrences"))
htmlTable(t2, header = c("Word", "Occurrences"))

# words removed from post-amendment dataset (because their interpretation in the sentiment analysis skews the result)
# ethics, committee, congress, trump, secret, sir, ethical
bg1d <- subset(bg1c, word != "ethics" & word != "committee" & word != "congress" & word != "trump" & word != "secret" & word != "sir" & word != "ethical")

# and of course, the graph showing the frequency of sentiments detected on each day

bg1d %>% 
  group_by(sentiment) %>%
  summarize(total_words = sum(words)) %>%
  mutate(general = ifelse(sentiment == "anticipation" | sentiment == "surprise", "neutral", ifelse(sentiment == "joy" | sentiment == "positive" | sentiment == "trust", "positive", "negative"))) %>%
  ggplot(aes(sentiment, total_words, fill = general)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = total_words, vjust = -.2)) +
  ggtitle("Sentiments found in @RepGoodlatte Mentions, 1/3/17") +
  scale_fill_manual(values = c("#990000", "#b300b3", "#000099"))

# words removed from pre-amendment dataset (because their interpretation in the sentiment analysis skews the result)
# ethics, watchdog
bg2d <- subset(bg2c, word != "ethics" & word != "watchdog")

bg2d %>% 
  group_by(sentiment) %>%
  summarize(total_words = sum(words)) %>%
  mutate(general = ifelse(sentiment == "anticipation" | sentiment == "surprise", "neutral", ifelse(sentiment == "joy" | sentiment == "positive" | sentiment == "trust", "positive", "negative"))) %>%
  ggplot(aes(sentiment, total_words, fill = general)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = total_words, vjust = -.2)) +
  ggtitle("Sentiments found in @RepGoodlatte Mentions, 1/2/17") +
  scale_fill_manual(values = c("#990000", "#b300b3", "#000099"))

#######

# for fun, I decided to split the Jan. 3 dataset at 12pm. House GOP withdrew the amendment just after 11am, but I left it til 12am to leave a little room for reading

alt1 <- subset(bg1a, created >= "2017-01-03 12:00:00 EST")

alt1b <- alt1 %>%
  group_by(word) %>%
  summarize(words = length(unique(ID)))

alt1c <- inner_join(alt1b, nrc, by = "word")

alt1d <- subset(alt1c, word != "ethics" & word != "committee" & word != "congress" & word != "trump" & word != "secret" & word != "sir" & word != "ethical")

a1 <- alt1d %>% 
  group_by(sentiment) %>%
  summarize(total_words = sum(words)) %>%
  mutate(general = ifelse(sentiment == "anticipation" | sentiment == "surprise", "neutral", ifelse(sentiment == "joy" | sentiment == "positive" | sentiment == "trust", "positive", "negative"))) %>%
  ggplot(aes(sentiment, total_words, fill = general)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = total_words, vjust = -.2)) +
  ggtitle("Sentiments found in @RepGoodlatte Mentions, after 12pm on 1/3/17") +
  scale_fill_manual(values = c("#990000", "#b300b3", "#000099")) +
  ylim(c(0,1000)) +
  theme(legend.position = "none")


alt2 <- subset(bg1a, created <= "2017-01-03 12:00:00 EST")

alt2b <- alt2 %>%
  group_by(word) %>%
  summarize(words = length(unique(ID)))

alt2c <- inner_join(alt2b, nrc, by = "word")

alt2d <- subset(alt2c, word != "ethics" & word != "committee" & word != "congress" & word != "trump" & word != "secret" & word != "sir" & word != "ethical")

a2 <- alt2d %>% 
  group_by(sentiment) %>%
  summarize(total_words = sum(words)) %>%
  mutate(general = ifelse(sentiment == "anticipation" | sentiment == "surprise", "neutral", ifelse(sentiment == "joy" | sentiment == "positive" | sentiment == "trust", "positive", "negative"))) %>%
  ggplot(aes(sentiment, total_words, fill = general)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = total_words, vjust = -.2)) +
  ggtitle("Sentiments found in @RepGoodlatte Mentions, before 12pm on 1/3/17") +
  scale_fill_manual(values = c("#990000", "#b300b3", "#000099")) +
  ylim(c(0,1000)) +
  theme(legend.position = "none")

library(gridExtra)

grid.arrange(a2, a1, ncol = 2)
