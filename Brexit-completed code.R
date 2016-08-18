###Getting the data
library("twitteR")
#Set up authentication
consumer_key = 'QJECcZsXyTNMnKztqA4Jbrl4i'
consumer_secret = '94nfaWzrs7KeyNqMbhZZvl3XKKf09YBqxGGT8OndLwF61eLSyj'
access_token = '428225393-dn7FbH58JZYQQ7wBxm0hRjzcUs2OjP04OycKgQod'
access_secret = 'FqAIdQdMqDdxZJqLW7e8OWSEFs1COq6gDEZVra7aUTbfZ'

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

#Scrape the data, 10000 tweets, language = English
brexit = searchTwitter("#Brexit", n = 50000, since = "2016-06-23", until = "2016-06-24", lang = "en")
brexit = twListToDF(brexit)
saveRDS(brexit, "brexit.RDS")
brexit = readRDS("brexit.RDS")

### Cleaning the data
brexit$text = iconv(brexit$text, "latin1", "ASCII", sub="") #strip emotiocons
brexit$text = gsub(' http[^[:blank:]]+', '', brexit$text) #strip http
brexit$text = gsub('(\\.|!|\\?)\\s+|(\\++)', ' ', brexit$text) #strip sentence enders

### Calculate polarity
library(qdap)

#Using function polarity
brexit$everything = polarity(brexit$text)
#Extracting polarity score
brexit$pol = brexit$everything$all$polarity

#Top negative tweets
brexit$text[head(order(brexit$pol), n = 10)]

#Top positive tweets
brexit$text[head(order(-brexit$pol), n = 10)]

#Assign positive, negative and neutral status to the polarity score
brexit$polarity = ifelse(brexit$pol < 0, "negative", ifelse(brexit$pol > 0, "positive", "neutral"))

#Plot
library(ggplot2)
library(scales)
ggplot(brexit, aes(x = as.factor(polarity))) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  scale_y_continuous(labels = percent) +
  labs(title = "The division in emotional polarity", y = "Percent", x = "Polarity")

#Top retweeted tweets
brexit$text[head(order(-brexit$retweetCount), n = 10)]
brexit$pol[head(order(-brexit$retweetCount), n = 10)]

#Calculate mean and median per group
tapply(brexit$retweetCount, brexit$polarity, mean)

tapply(brexit$retweetCount, brexit$polarity, median)

#Plot
ggplot(brexit, aes(x = pol, y = retweetCount)) +
  geom_point(position = 'jitter') + #avoid overplotting
  geom_smooth() + #confident interval
  ggtitle("Polarity vs Retweet") + 
  xlab('Polarity') +
  ylab("Number of Retweets")

#Fav vs retweet
fav.vs.retweet = ggplot(brexit, aes(x = favoriteCount, y = retweetCount))
fav.vs.retweet + geom_point(position = "jitter") + geom_smooth() + ggtitle("Favorite vs Retweet") + xlab("Number of favorites") + ylab("Number of retweets")
