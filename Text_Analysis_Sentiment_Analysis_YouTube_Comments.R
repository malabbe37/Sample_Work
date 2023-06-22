# Name: Michael Labbe
# Date: 01/25/23
# Goal of Program: Datetime Handling, One-Hot Encoding, etc.
# Data Files Used: Youtube Comments


library(readr)
library(tidyverse)
library(stringr)
library(ggplot2)
library(ggmap)
library(lubridate)
library(dplyr)
library(tidygeocoder)
library(tidytext)
library(gutenbergr)
library(textdata)

#1:

youtube <- read_csv("youtube_comments.csv")

#Convert youtube to dataframe and pipe words into word variable
tidy_youtube <- youtube %>% unnest_tokens(word, content) 

glimpse(tidy_youtube)


#2.

#Term count by video
video_words <- tidy_youtube %>% count(video,word,sort=T)

#Total words by video
video_totals <- video_words%>%group_by(video)%>%summarize(total=sum(n))
video_words <- left_join(video_words,video_totals)
video_words <- video_words%>%mutate(tf=n/total)

#Multiply TF by IDF to measure importance for word among many documents.
video_tfidf <- video_words%>%bind_tf_idf(word,video,n)
glimpse(video_tfidf)


#3. 

#Top words by video graph
video_tfidf%>%group_by(video)%>%slice_max(tf_idf,n=10)%>%
  ggplot(aes(x=tf_idf, y=reorder(word,tf_idf)))+
  geom_col(show.legend = F)+
  facet_wrap(~video,ncol=2,scales="free")

#4.

#Term count by class
class_words <- tidy_youtube %>% count(class,word,sort=T)

#Total words by video
class_totals <- class_words%>%group_by(class)%>%summarize(total=sum(n))
class_words <- left_join(class_words,class_totals)
class_words <- class_words%>%mutate(tf=n/total)

#Multiply TF by IDF to measure importance for word among many documents.
class_tfidf <- class_words%>%bind_tf_idf(word,class,n)
glimpse(class_tfidf)

#5. 

#Top words by class graph
class_tfidf%>%group_by(class)%>%slice_max(tf_idf,n=10)%>%
  ggplot(aes(x=tf_idf, y=reorder(word,tf_idf)))+
  geom_col(show.legend = F)+
  facet_wrap(~class,ncol=2,scales="free")


#Part B:


#1.

my_books <- gutenberg_download(c(14220,14304,14837,139,244,2097),meta_fields = c("title","author"))

glimpse(my_books)

#2.

#Unnest the text
tidy_books <- my_books %>% unnest_tokens(word, text) 

glimpse(tidy_books)

#Remove stop words
tidy_books <- tidy_books %>% anti_join(stop_words)
tidy_books

#3. 

#Bring in the "Bing" Lexicon
my_bing <- get_sentiments("bing")
View(my_bing)

#Join the tidy data with the sentiments
feel_books <- tidy_books%>%inner_join(my_bing)
glimpse(feel_books)

#Visualize the sentiment by book
feel_books %>% ggplot(aes(x=author, fill=sentiment))+geom_bar()
#Display as percentage for better overall sentiment
feel_books %>% ggplot(aes(x=author, fill=sentiment))+geom_bar(position = position_fill())

#4.

#Mutate the data set to display the word length
feel_books <- feel_books%>%mutate(word_length=str_length(word))

glimpse(feel_books)

#5.

#Produce summary statistics for mean, median and maximum word values
books_words <- feel_books%>%group_by(author)%>%summarise(mean_word=mean(word_length), median_word=median(word_length), max(word_length))
head(books_words)

#6.

#Conduct tokenization without removing stop words
tidy_books <- my_books %>% unnest_tokens(word, text) 

#Mutate the data set to display the word length
tidy_books <- tidy_books%>%mutate(word_length=str_length(word))

#Produce summary statistics for mean, median and maximum word values
book_words <- tidy_books%>%group_by(author)%>%summarise(mean_word=mean(word_length), median_word=median(word_length), max(word_length))
head(book_words)

#7. 

#Produce separate histograms to show distribution of length of words for two authors
tidy_books[tidy_books$author=="Doyle, Arthur Conan",] %>% ggplot(aes(word_length))+geom_histogram()+ggtitle("Doyle")
tidy_books[tidy_books$author=="Potter, Beatrix",] %>% ggplot(aes(word_length))+geom_histogram(bins = 25)+ggtitle("Potter")

#8.

#Most common word in Potter's books of length 12
tidy_books %>% filter(word_length == 12) %>% select(word) %>% count(word,sort=T)

#9.

#Convert youtube to dataframe and pipe sentences into sentences variable
tidy_books <- my_books %>% unnest_tokens(sentences,text,token="sentences") 

#Mutate the data set to display the sentence length
tidy_books <- tidy_books%>%mutate(sentence_length=str_length(sentences))

#Produce summary statistics for mean, median and maximum sentence values
book_sentences <- tidy_books%>%group_by(author)%>%summarise(mean_sentence=mean(sentence_length), median_word=median(sentence_length), max(sentence_length))
head(book_sentences)

