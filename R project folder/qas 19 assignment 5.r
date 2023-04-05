library(tm)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)
library(wordcloud2)
library(ggplot2)
library(dplyr)
library(readxl)

emails = read_xlsx('C:/Users/Admin/Downloads/Email Data.xlsx')

ham <- emails[which(emails$type == 'ham'),]



#create a corpus or set of all text in the file
emails_corpus_ham = VCorpus(VectorSource(ham$email))

emails_corpus_ham

as.character(emails_corpus_ham[[10]]) #reads that entry in it

#remove numbers from comments

emails_cleanfile_ham = tm_map(emails_corpus_ham, removeNumbers)

#make comments lower case letters

emails_cleanfile_ham = tm_map(emails_cleanfile_ham,
                          content_transformer(tolower))

as.character(emails_cleanfile_ham[[1]]) #check line 1

#remove auxiliary words from comments

emails_cleanfile_ham = tm_map(emails_cleanfile_ham, 
                          removeWords, stopwords())
as.character(emails_cleanfile_ham[[1]])


#remove punctuation 

emails_cleanfile_ham = tm_map(emails_cleanfile_ham,removePunctuation)
as.character(emails_cleanfile_ham[[1]])

#remove any extra white space

emails_cleanfile_ham = tm_map(emails_cleanfile_ham, stripWhitespace)
as.character(emails_cleanfile_ham[[1]])

#create a matrix of words
dtm_emails_ham = TermDocumentMatrix(emails_cleanfile_ham)
dtm_emails_ham
matrix_emails_ham = as.matrix(dtm_emails_ham)
matrix_emails_ham

#row sums, calculate total number of times
#         a word used across all comments
words_emails_ham = sort(rowSums(matrix_emails_ham),decreasing = TRUE)
words_emails_ham

#create a dataframe of words
df_ham = data.frame(word = names(words_emails_ham), freq = words_emails_ham)
df_ham

top200_words_ham = df_ham %>% filter(freq > 200) #new df that shows freq > 200
top200_words_ham

wordcloud2(data = top200_words_ham, size = 0.5, shape = 'rectangle' )


ggplot(top200_words_ham, aes(x = word, y = freq, color = word)) +
  geom_bar(stat = 'identity') +
  xlab('Top Words') +
  ylab ('Frequency of Words') +
  ggtitle('Top words Used in Ham Emails')





#question 2

spam <- emails[which(emails$type == 'spam'),]

#create a corpus or set of all text in the file
emails_corpus_spam = VCorpus(VectorSource(spam$email))

emails_corpus_spam

as.character(emails_corpus_spam[[10]]) #reads that entry in it

#remove numbers from comments

emails_cleanfile_spam = tm_map(emails_corpus_spam, removeNumbers)

#make comments lower case letters

emails_cleanfile_spam = tm_map(emails_cleanfile_spam,
                              content_transformer(tolower))

as.character(emails_cleanfile_spam[[1]]) #check line 1

#remove auxiliary words from comments

emails_cleanfile_spam = tm_map(emails_cleanfile_spam, 
                              removeWords, stopwords())
as.character(emails_cleanfile_spam[[1]])


#remove punctuation 

emails_cleanfile_spam = tm_map(emails_cleanfile_spam,removePunctuation)
as.character(emails_cleanfile_spam[[1]])

#remove any extra white space

emails_cleanfile_spam = tm_map(emails_cleanfile_spam, stripWhitespace)
as.character(emails_cleanfile_spam[[1]])

#create a matrix of words
dtm_emails_spam = TermDocumentMatrix(emails_cleanfile_spam)
dtm_emails_spam
matrix_emails_spam = as.matrix(dtm_emails_spam)
matrix_emails_spam

#row sums, calculate total number of times
#         a word used across all comments
words_emails_spam = sort(rowSums(matrix_emails_spam),decreasing = TRUE)
words_emails_spam

#create a dataframe of words
df_spam = data.frame(word = names(words_emails_spam), freq = words_emails_spam)
df_spam

top100_words_spam = df_spam %>% filter(freq > 100) #new df that shows freq > 200
top100_words_spam

wordcloud2(data = top100_words_spam, size = 0.5, shape = 'rectangle' )


ggplot(top100_words_spam, aes(x = word, y = freq)) +
  geom_bar(stat = 'identity', fill = 'pink') +
  xlab('Top Words') +
  ylab ('Frequency of Words') +
  ggtitle('Top words Used in Spam Emails')






#wordcloud
library(arules)
library(arulesViz)
library(dplyr)



#question 3






#get in market basket format
purchases = read.transactions('C:/Users/Admin/Downloads/purchases_data.csv',
                       header = TRUE, format = 'basket', sep = ',')

itemLabels(purchases)

inspect(purchases[1:5])

#creates plots (bar charts to look at frequency)
#absolute
itemFrequencyPlot(purchases,topN = 5, type = 'absolute',
                  col=brewer.pal(5,'Pastel2'),
                  main = 'Plot of absolute Frequency')

#relative
itemFrequencyPlot(purchases,topN = 5, type = 'relative', 
                  col = brewer.pal(5, 'GnBu'),
                  main = 'Plot of Relative Frequency' )






#question 4


pr = apriori(purchases)

#setting up rules

pr1 = apriori(purchases, parameter = list(supp = .001,
                                      conf = .01, maxlen = 12))
#explain what these rules do and which one makes more sense
pr2 = apriori(purchases, parameter = list(supp = .1,
                                          conf = .001, maxlen = 12))

#explain why pr1 makes more sense and why





#question 5

inspect(sort(pr1, by = 'lift')[1:10])
top_lift_pr1 = head(pr1, n = 10, by = 'lift')

inspect(sort(pr1, by = 'support')[1:5])
top_support_pr1 = head(top_lift_pr1, n = 5, by = 'support')

inspect(sort(pr1, by = 'confidence')[1:5])
top_confidence_pr1 = head(top_lift_pr1, n = 5, by = 'confidence')

df_support_pr1 = DATAFRAME(top_support_pr1)
df_confidence_pr1 = DATAFRAME(top_confidence_pr1)

#Explain which of these 2 tables is more reliable and why?


