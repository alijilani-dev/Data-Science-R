######################################################################
#                                                                    #
#                         Ali Akbar Jilani                           #
#               Department of Science and Computing,                 #
#               Waterford Institute of Technology,                   #
#                       Waterford, Ireland.                          #
#                   E-mail: 20078735@mail.wit.ie                     #
#                                                                    #
######################################################################
# Final Dissertation: MSc. in Computing, Enterprise Software Systems #
######################################################################
#                                                                    #
# References:                                                        #
#                                                                    #
# [1] Jeffrey Breen on Twitter text mining                           #
# https://datamatters.blog/2011/07/04/twitter-text-mining-r-slides/  #
#                                                                    #
# [2] Bing Liu, Minqing Hu and Junsheng Cheng. "Opinion Observer:    #
# Analyzing and Comparing Opinions on the Web. "Proceedings of the   #
# 14th International World Wide Web conference (WWW-2005), May 10-14,#
# 2005, Chiba, Japan.                                                #
#                                                                    #
######################################################################

# Install these needed R packages (only need to do this once)
######################################################################
remove.packages("twitteR")
remove.packages("ROAuth")
remove.packages("base64enc")
remove.packages("tm")
remove.packages('SnowballC')
remove.packages("XML")
remove.packages("ggplot2")
remove.packages("plyr")
remove.packages("stringr")
remove.packages("stringr")
remove.packages("stringi")
remove.packages("RTextTools")
remove.packages("ggpubr")


install.packages('twitteR')
install.packages('ROAuth')
install.packages('base64enc')
install.packages('tm') #, dependencies=T)
install.packages('SnowballC')
install.packages('corpus')
install.packages('XML')
install.packages('ggplot2')
install.packages('plyr')
install.packages('stringr')
install.packages('stringi')
install.packages('RTextTools')
install.packages('ggpubr')
install.packages('caret')
######################################################################


#                                                                    #
# Load the packages (need to do this each time R is opened)
######################################################################
library(twitteR)
library(ROAuth)
library(base64enc)
library(tm)
library(SnowballC)
library(corpus)
library(XML)
library(ggplot2)
library(plyr)
library(stringr)
library(stringi)
library(RTextTools)
library(ggpubr)
library(caret)
######################################################################


# Step 1. Authenticate your twitter application using oAuth2.0 protocol.
######################################################################

consumer_key <- "private information"
consumer_secret <- "private information"
access_token <- "private information"
access_secret <- "private information"

download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

cred <- OAuthFactory$new(consumerKey=consumer_key,
                         consumerSecret=consumer_secret,
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL='https://api.twitter.com/oauth/authorize')

cred$handshake(cainfo="cacert.pem")

######################################################################


# Setting working directory.
######################################################################

set.seed(23082018)
wd <- "d:/SVM-R/Code/";
wd_data <- "d:/SVM-R/Code/data/"
wd_cleaning <- "d:/SVM-R/Code/data/cleaning/"
setwd(wd)

######################################################################


# Step 2. Create data frame of keywords and dictionary of Amazon star ratings.
######################################################################
# Create a global dictionary of keyword, sentiment values
# Link 1: https://cran.r-project.org/web/packages/dataMeta/vignettes/dataMeta_Vignette.html
# Link 2: https://stackoverflow.com/questions/7804816/how-to-create-a-dictionary-hash-table-by-iterating-through-a-column/7805180#7805180

keywords = data.frame(
keywords = c("Conan Exiles", "Dead Head", "Viking", "Street Fighter", "Hybrid", "WWE", "Guilty Party", "Guilty Gear", "Civilization", "NBA",
             "Miles Edgeworth", "The King of Fighters", "Blue Dragon", "Back to the future", "Golden Sun", "House of the dead", "Cuphead", "The Last Blade", "Marvel vs. Capcom",
             "Dead Rising", "Devil May Cry", "The walking dead", "Batman", "Folklore", "Mortal Kombat", "Stuntman", "Warhammer", "Dead Island", "Nier", "The Club", "Bayonetta",
             "For Honor", "Freedom Planet", "Proving Ground", "Lego Batman", "Lego Marvel", "Tomb Raider", "Final Fantasy", "Sonic Mania", "Skullgirls", "Jet Set Radio",
             "Game of thrones", "Sonic Generations", "Dragon Ball Fighterz", "Garou", "Killer Instinct", "Ghost Trick", "World Ends With You", "Resistance", "Jeanne d'Arc"))

Star_Ratings = data.frame(
keywords = c("Conan Exiles", "Dead Head", "Viking", "Street Fighter", "Hybrid", "WWE", "Guilty Party", "Guilty Gear", "Civilization", "NBA",
             "Miles Edgeworth", "The King of Fighters", "Blue Dragon", "Back to the future", "Golden Sun", "House of the dead", "Cuphead", "The Last Blade", "Marvel vs. Capcom",
             "Dead Rising", "Devil May Cry", "The walking dead", "Batman", "Folklore", "Mortal Kombat", "Stuntman", "Warhammer", "Dead Island", "Nier", "The Club", "Bayonetta",
             "For Honor", "Freedom Planet", "Proving Ground", "Lego Batman", "Lego Marvel", "Tomb Raider", "Final Fantasy", "Sonic Mania", "Skullgirls", "Jet Set Radio",
             "Game of thrones", "Sonic Generations", "Dragon Ball Fighterz", "Garou", "Killer Instinct", "Ghost Trick", "World Ends With You", "Resistance", "Jeanne d'Arc"),
ratings = c( 3.4, 3.9, 3.7, 3.1, 4.4, 5.0, 4.1, 4.1, 3.4, 3.9,
             4.6, 4.5, 4.2, 3.4, 4.2, 2.6, 4.4, 4.1, 3.4,
             4.0, 4.0, 3.5, 3.7, 3.9, 3.8, 3.7, 3.7, 3.3, 3.8, 2.8, 4.0,
             3.5, 3.6, 3.7, 4.4, 4.6, 2.7, 3.6, 3.9, 4.8, 3.8,
             4.5, 3.2, 4.2, 4.4, 4.1, 4.6, 4.6, 4.8, 5.0)
)
######################################################################


# Step 3. Download "Computer Games" tweets using Twitter APIs from twitteR package.
######################################################################

Prod1_tweets = searchTwitter(searchString=as.character(keywords[1,1]), n=500, lang="en")
Prod2_tweets = searchTwitter(searchString=as.character(keywords[2,1]), n=500, lang="en")
Prod3_tweets = searchTwitter(searchString=as.character(keywords[3,1]), n=500, lang="en")
Prod4_tweets = searchTwitter(searchString=as.character(keywords[4,1]), n=500, lang="en")
Prod5_tweets = searchTwitter(searchString=as.character(keywords[5,1]), n=500, lang="en")
Prod6_tweets = searchTwitter(searchString=as.character(keywords[6,1]), n=500, lang="en")
Prod7_tweets = searchTwitter(searchString=as.character(keywords[7,1]), n=500, lang="en")
Prod8_tweets = searchTwitter(searchString=as.character(keywords[8,1]), n=500, lang="en")
Prod9_tweets = searchTwitter(searchString=as.character(keywords[9,1]), n=500, lang="en")
Prod10_tweets = searchTwitter(searchString=as.character(keywords[10,1]), n=500, lang="en")
Prod11_tweets = searchTwitter(searchString=as.character(keywords[11,1]), n=500, lang="en")
Prod12_tweets = searchTwitter(searchString=as.character(keywords[12,1]), n=500, lang="en")
Prod13_tweets = searchTwitter(searchString=as.character(keywords[13,1]), n=500, lang="en")
Prod14_tweets = searchTwitter(searchString=as.character(keywords[14,1]), n=500, lang="en")
Prod15_tweets = searchTwitter(searchString=as.character(keywords[15,1]), n=500, lang="en")
Prod16_tweets = searchTwitter(searchString=as.character(keywords[16,1]), n=500, lang="en")
Prod17_tweets = searchTwitter(searchString=as.character(keywords[17,1]), n=500, lang="en")
Prod18_tweets = searchTwitter(searchString=as.character(keywords[18,1]), n=500, lang="en")
Prod19_tweets = searchTwitter(searchString=as.character(keywords[19,1]), n=500, lang="en")
Prod20_tweets = searchTwitter(searchString=as.character(keywords[20,1]), n=500, lang="en")
Prod21_tweets = searchTwitter(searchString=as.character(keywords[21,1]), n=500, lang="en")
Prod22_tweets = searchTwitter(searchString=as.character(keywords[22,1]), n=500, lang="en")
Prod23_tweets = searchTwitter(searchString=as.character(keywords[23,1]), n=500, lang="en")
Prod24_tweets = searchTwitter(searchString=as.character(keywords[24,1]), n=500, lang="en")
Prod25_tweets = searchTwitter(searchString=as.character(keywords[25,1]), n=500, lang="en")
Prod26_tweets = searchTwitter(searchString=as.character(keywords[26,1]), n=500, lang="en")
Prod27_tweets = searchTwitter(searchString=as.character(keywords[27,1]), n=500, lang="en")
Prod28_tweets = searchTwitter(searchString=as.character(keywords[28,1]), n=500, lang="en")
Prod29_tweets = searchTwitter(searchString=as.character(keywords[29,1]), n=500, lang="en")
Prod30_tweets = searchTwitter(searchString=as.character(keywords[30,1]), n=500, lang="en")
Prod31_tweets = searchTwitter(searchString=as.character(keywords[31,1]), n=500, lang="en")
Prod32_tweets = searchTwitter(searchString=as.character(keywords[32,1]), n=500, lang="en")
Prod33_tweets = searchTwitter(searchString=as.character(keywords[33,1]), n=500, lang="en")
Prod34_tweets = searchTwitter(searchString=as.character(keywords[34,1]), n=500, lang="en")
Prod35_tweets = searchTwitter(searchString=as.character(keywords[35,1]), n=500, lang="en")
Prod36_tweets = searchTwitter(searchString=as.character(keywords[36,1]), n=500, lang="en")
Prod37_tweets = searchTwitter(searchString=as.character(keywords[37,1]), n=500, lang="en")
Prod38_tweets = searchTwitter(searchString=as.character(keywords[38,1]), n=500, lang="en")
Prod39_tweets = searchTwitter(searchString=as.character(keywords[39,1]), n=500, lang="en")
Prod40_tweets = searchTwitter(searchString=as.character(keywords[40,1]), n=500, lang="en")
Prod41_tweets = searchTwitter(searchString=as.character(keywords[41,1]), n=500, lang="en")
Prod42_tweets = searchTwitter(searchString=as.character(keywords[42,1]), n=500, lang="en")
Prod43_tweets = searchTwitter(searchString=as.character(keywords[43,1]), n=500, lang="en")
Prod44_tweets = searchTwitter(searchString=as.character(keywords[44,1]), n=500, lang="en")
Prod45_tweets = searchTwitter(searchString=as.character(keywords[45,1]), n=500, lang="en")
Prod46_tweets = searchTwitter(searchString=as.character(keywords[46,1]), n=500, lang="en")
Prod47_tweets = searchTwitter(searchString=as.character(keywords[47,1]), n=500, lang="en")
Prod48_tweets = searchTwitter(searchString=as.character(keywords[48,1]), n=500, lang="en")
Prod49_tweets = searchTwitter(searchString=as.character(keywords[49,1]), n=500, lang="en")
Prod50_tweets = searchTwitter(searchString=as.character(keywords[50,1]), n=500, lang="en")
######################################################################


# Function Definitions
######################################################################
# cleans up tweets with R's regex-driven global substitute, gsub():
tweets.clean = function(tweets, .keyword='none')
{
  setwd(wd_cleaning)
  tweets.df <- twListToDF(tweets)
  # Step a: Create a tm corpus to start cleaning the tweets with TM (text mining) package. 
  # http://www.rdatamining.com/docs/twitter-analysis-with-r
  mycorpus <- Corpus(VectorSource(tweets.df$text))
  save(mycorpus, file = paste( .keyword, "_Sa.RData"))
  # use ls() in R GUI to get list of available items and type item name + Enter to open it. 
  
  # Step b. Eliminating stop words
  mycorpus <- tm_map(mycorpus, removeWords, stopwords())
  save(mycorpus, file = paste( .keyword, "_Sb.RData"))

  # Step c. Eliminating custom stopwords 
  # http://www.rdatamining.com/docs/twitter-analysis-with-r (Page 10).
  remove_cStopwords <- c(setdiff(stopwords('english'), c("r", "big")), "use", "see", "used", "via", "amp", "red", "hat", "redhat")
  mycorpus <- tm_map(mycorpus, removeWords, remove_cStopwords)
  save(mycorpus, file = paste( .keyword, "_Sc.RData"))
  
  # Step d. Eliminating RT and via symbols
  remove_RTlabels <- function(x) gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", x)
  mycorpus <- tm_map(mycorpus, content_transformer(remove_RTlabels))
  save(mycorpus, file = paste( .keyword, "_Sd.RData"))
  
  
  # Step e. Eliminate twitter handles, like @washingtonpost
  remove_handles <- function(x) gsub("((?:\\b\\W*@\\w+)+)", " ", x)
  mycorpus <- tm_map(mycorpus, content_transformer(remove_handles))
  save(mycorpus, file = paste( .keyword, "_Se.RData"))
  
  # Step f. Eliminate punctuations, without affecting #Hashtags.
  # https://stackoverflow.com/questions/27951377/tm-custom-removepunctuation-except-hashtag
  removeMostPunctuation <- function(x, preserve_intra_word_dashes = FALSE) 
  {
    rmpunct <- function(x) {
      x <- gsub("#", "\002", x)
      x <- gsub("[[:punct:]]+", "", x)
      gsub("\002", "#", x, fixed = TRUE)
    }
    if (preserve_intra_word_dashes) { 
      x <- gsub("(\\w)-(\\w)", "\\1\001\\2", x)
      x <- rmpunct(x)
      gsub("\001", "-", x, fixed = TRUE)
    } else {
      rmpunct(x)
    }
  }
  mycorpus <- tm_map(mycorpus, content_transformer(removeMostPunctuation), preserve_intra_word_dashes = TRUE)
  save(mycorpus, file = paste( .keyword, "_Sf.RData"))

  # Step g. Eliminate all "#Hashtags"
  remove_hastags <- function(x) gsub(" #\\S*", " ", x)
  mycorpus <- tm_map(mycorpus, content_transformer(remove_hastags))
  save(mycorpus, file = paste( .keyword, "_Sg.RData"))
  
  # Step h. Eliminate punctuations using the standard function
  #mycorpus <- tm_map(mycorpus, removePunctuation)
  #save(mycorpus, file = paste( .keyword, "_Sh.RData"))
  
  # Step i. Eliminate tiny URLs
  remove_turl <- function(x) gsub("[A-Za-z]{1,5}[.][A-Za-z]{2,3}/[A-Za-z0-9]+\\b", "", x)
  mycorpus <- tm_map(mycorpus, content_transformer(remove_turl))
  save(mycorpus, file = paste( .keyword, "_Si.RData"))
  
  # Step j. Eliminate normal URLs https://stackoverflow.com/questions/25352448/remove-urls-from-string-in-r
  remove_url <- function(x) gsub("(f|ht)tp\\S+\\s*", "", x) 
  mycorpus <- tm_map(mycorpus, content_transformer(remove_url))
  save(mycorpus, file = paste( .keyword, "_Sj.RData"))
  
  # Step k. Eliminate anything other than english letters and space.Remove all emoticons
  # https://stackoverflow.com/questions/44893354/remove-emoticons-in-r-using-tm-package : Get rid of all non-ASCII characters.
  removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
  mycorpus <- tm_map(mycorpus, content_transformer(removeNumPunct))
  save(mycorpus, file = paste( .keyword, "_Sk.RData"))
  
  # Step l. using the standard function to convert text to lower case.
  mycorpus <- tm_map(mycorpus, content_transformer(tolower))
  save(mycorpus, file = paste( .keyword, "_Sl.RData"))
  
  # Step m. Eliminate extra whitespace using standard function.
  mycorpus <- tm_map(mycorpus, stripWhitespace)
  save(mycorpus, file = paste( .keyword, "_Sm.RData"))
  
  # Step n. Eliminate stem words using standard function.
  mycorpus <- tm_map(mycorpus, stemDocument)
  save(mycorpus, file = paste( .keyword, "_Sn.RData"))
  
  # Step o. Need to investigate..
  #mycorpus<- tm_map(mycorpus, PlainTextDocument)
  #save(mycorpus, file = paste( .keyword, "_So.RData"))
  
  # convert the corpus to text vector
  text <- as_corpus_text(mycorpus[]$content)
  
  # https://stackoverflow.com/questions/7201341/how-can-two-strings-be-concatenated
  x1 <- c("tech_corpus", .keyword)
  x2 <- paste(x1, collapse = "_")
  x2 <- c(x2, "csv")
  x3 <- paste(x2, collapse = ".")

  path <- c(wd, "data/")
  wd_data <- paste(path, collapse = "/")
  setwd(wd_data)
  write.csv(text, x3)
  setwd(wd)
  
  return(text);
}

# import positive and negative words (Add credits to developers).
pos = readLines("d:/SVM-R/Code/positive-words.txt")
neg = readLines("d:/SVM-R/Code/negative-words.txt")

# Calculates sentiment score against tweets passed to return data frame.
score.sentiment = function(sentences, pos.words, neg.words, .keyword='none', .progress='none')
{
  #' score.sentiment() implements a very simple algorithm to estimate
  #' sentiment, assigning a integer score by subtracting the number 
  #' of occurrences of negative words from that of positive words.
  #' 
  #' @param sentences vector of text to score
  #' @param pos.words vector of words of postive sentiment
  #' @param neg.words vector of words of negative sentiment
  #' @param .keyword passed to archive data to a .csv file.
  #' @param .progress passed to <code>laply()</code> to control of progress bar.
  #' @returnType data.frame
  #' @return data.frame of text and corresponding sentiment scores
  #' @author Jefrey Breen <jbreen@cambridge.aero>
  #' https://github.com/jeffreybreen/twitter-sentiment-analysis-tutorial-201107/blob/master/R/sentiment.R
  #' 
  # create simple array of scores with laply
  scores = laply(sentences,
                 function(sentence, pos.words, neg.words)
                 {
                   
                   # define error handling function when trying tolower
                   tryTolower = function(x)
                   {
                     # create missing value
                     y = NA
                     # tryCatch error
                     try_error = tryCatch(tolower(x), error=function(e) e)
                     # if not an error
                     if (!inherits(try_error, "error"))
                       y = tolower(x)
                     # result
                     return(y)
                   }
                   # use tryTolower with sapply 
                   sentence = sapply(sentence, tryTolower)
                   
                   # split sentence into words with str_split (stringr package)
                   word.list = str_split(sentence, "\\s+")
                   words = unlist(word.list)
                   
                   # compare words to the dictionaries of positive & negative terms
                   pos.matches = match(words, pos.words)
                   neg.matches = match(words, neg.words)
                   
                   # get the position of the matched term or NA
                   # we just want a TRUE/FALSE
                   pos.matches = !is.na(pos.matches)
                   neg.matches = !is.na(neg.matches)
                   
                   # final score
                   #score = sum(pos.matches) - sum(neg.matches)
                   total_sentiment_words = sum(pos.matches) + sum(neg.matches)
                   if (total_sentiment_words == 0) {
                     score = NA
                   }
                   else {
                     score = sum(pos.matches) / total_sentiment_words
                   }
                   return(score)
                 }, pos.words, neg.words, .progress=.progress )
  
  # data frame with scores for each sentence
  scores.df = data.frame(text=sentences, score=scores)
  
  # remove NA rows in scores.df
  scores.df = scores.df[!is.na(scores.df$score), ]
  
  # https://stackoverflow.com/questions/7201341/how-can-two-strings-be-concatenated
  x1 <- c("tech_scores", .keyword)
  x2 <- paste(x1, collapse = "_")
  x2 <- c(x2, "csv")
  x3 <- paste(x2, collapse = ".")
  #wd <- getwd()
  path <- c(wd, "data/")
  wd_data <- paste(path, collapse = "/")
  setwd(wd_data)
  write.csv(scores.df, x3)
  setwd(wd)
  
  return(scores.df)
}

# Calculates global score against passed keyword to return global score.
score.global = function(scores, .keyword = 'none')
{
  # Calculate global score
  globalscore =  mean(scores$score, na.rm = TRUE)
  
  return (globalscore)
}

# Calculates sentiment score against tweets passed to return data frame.
label_data = function(df) 
{
  df = df[df$score != 0, , drop=FALSE]
  df$score = sapply(df$score, FUN = function(x) ifelse(x>0.5, 'POS', 'NEG'))
  df
}
######################################################################


# Step 4. Clean the Tweets.
######################################################################
options(warn=-1)
Prod1_text = tweets.clean(Prod1_tweets, .keyword=keywords[1,1])
Prod2_text = tweets.clean(Prod2_tweets, .keyword=keywords[2,1])
Prod3_text = tweets.clean(Prod3_tweets, .keyword=keywords[3,1])
Prod4_text = tweets.clean(Prod4_tweets, .keyword=keywords[4,1])
Prod5_text = tweets.clean(Prod5_tweets, .keyword=keywords[5,1])
Prod6_text = tweets.clean(Prod6_tweets, .keyword=keywords[6,1])
Prod7_text = tweets.clean(Prod7_tweets, .keyword=keywords[7,1])
Prod8_text = tweets.clean(Prod8_tweets, .keyword=keywords[8,1])
Prod9_text = tweets.clean(Prod9_tweets, .keyword=keywords[9,1])
Prod10_text = tweets.clean(Prod10_tweets, .keyword=keywords[10,1])
Prod11_text = tweets.clean(Prod11_tweets, .keyword=keywords[11,1])
Prod12_text = tweets.clean(Prod12_tweets, .keyword=keywords[12,1])
Prod13_text = tweets.clean(Prod13_tweets, .keyword=keywords[13,1])
Prod14_text = tweets.clean(Prod14_tweets, .keyword=keywords[14,1])
Prod15_text = tweets.clean(Prod15_tweets, .keyword=keywords[15,1])
Prod16_text = tweets.clean(Prod16_tweets, .keyword=keywords[16,1])
Prod17_text = tweets.clean(Prod17_tweets, .keyword=keywords[17,1])
Prod18_text = tweets.clean(Prod18_tweets, .keyword=keywords[18,1])
Prod19_text = tweets.clean(Prod19_tweets, .keyword=keywords[19,1])
Prod20_text = tweets.clean(Prod20_tweets, .keyword=keywords[20,1])
Prod21_text = tweets.clean(Prod21_tweets, .keyword=keywords[21,1])
Prod22_text = tweets.clean(Prod22_tweets, .keyword=keywords[22,1])
Prod23_text = tweets.clean(Prod23_tweets, .keyword=keywords[23,1])
Prod24_text = tweets.clean(Prod24_tweets, .keyword=keywords[24,1])
Prod25_text = tweets.clean(Prod25_tweets, .keyword=keywords[25,1])
Prod26_text = tweets.clean(Prod26_tweets, .keyword=keywords[26,1])
Prod27_text = tweets.clean(Prod27_tweets, .keyword=keywords[27,1])
Prod28_text = tweets.clean(Prod28_tweets, .keyword=keywords[28,1])
Prod29_text = tweets.clean(Prod29_tweets, .keyword=keywords[29,1])
Prod30_text = tweets.clean(Prod30_tweets, .keyword=keywords[30,1])
Prod31_text = tweets.clean(Prod31_tweets, .keyword=keywords[31,1])
Prod32_text = tweets.clean(Prod32_tweets, .keyword=keywords[32,1])
Prod33_text = tweets.clean(Prod33_tweets, .keyword=keywords[33,1])
Prod34_text = tweets.clean(Prod34_tweets, .keyword=keywords[34,1])
Prod35_text = tweets.clean(Prod35_tweets, .keyword=keywords[35,1])
Prod36_text = tweets.clean(Prod36_tweets, .keyword=keywords[36,1])
Prod37_text = tweets.clean(Prod37_tweets, .keyword=keywords[37,1])
Prod38_text = tweets.clean(Prod38_tweets, .keyword=keywords[38,1])
Prod39_text = tweets.clean(Prod39_tweets, .keyword=keywords[39,1])
Prod40_text = tweets.clean(Prod40_tweets, .keyword=keywords[40,1])
Prod41_text = tweets.clean(Prod41_tweets, .keyword=keywords[41,1])
Prod42_text = tweets.clean(Prod42_tweets, .keyword=keywords[42,1])
Prod43_text = tweets.clean(Prod43_tweets, .keyword=keywords[43,1])
Prod44_text = tweets.clean(Prod44_tweets, .keyword=keywords[44,1])
Prod45_text = tweets.clean(Prod45_tweets, .keyword=keywords[45,1])
Prod46_text = tweets.clean(Prod46_tweets, .keyword=keywords[46,1])
Prod47_text = tweets.clean(Prod47_tweets, .keyword=keywords[47,1])
Prod48_text = tweets.clean(Prod48_tweets, .keyword=keywords[48,1])
Prod49_text = tweets.clean(Prod49_tweets, .keyword=keywords[49,1])
Prod50_text = tweets.clean(Prod50_tweets, .keyword=keywords[50,1])
options(warn=0)
######################################################################


# Step 5. Calculate Sentiment Scores
######################################################################

Prod1_scores   = score.sentiment( Prod1_text,  pos, neg, .keyword=keywords[1,1],  .progress='text')
Prod2_scores   = score.sentiment( Prod2_text,  pos, neg, .keyword=keywords[2,1],  .progress='text')
Prod3_scores   = score.sentiment( Prod3_text,  pos, neg, .keyword=keywords[3,1],  .progress='text')
Prod4_scores   = score.sentiment( Prod4_text,  pos, neg, .keyword=keywords[4,1],  .progress='text')
Prod5_scores   = score.sentiment( Prod5_text,  pos, neg, .keyword=keywords[5,1],  .progress='text')
Prod6_scores   = score.sentiment( Prod6_text,  pos, neg, .keyword=keywords[6,1],  .progress='text')
Prod7_scores   = score.sentiment( Prod7_text,  pos, neg, .keyword=keywords[7,1],  .progress='text')
Prod8_scores   = score.sentiment( Prod8_text,  pos, neg, .keyword=keywords[8,1],  .progress='text')
Prod9_scores   = score.sentiment( Prod9_text,  pos, neg, .keyword=keywords[9,1],  .progress='text')
Prod10_scores  = score.sentiment( Prod10_text, pos, neg, .keyword=keywords[10,1], .progress='text')
Prod11_scores  = score.sentiment( Prod11_text, pos, neg, .keyword=keywords[11,1], .progress='text')
Prod12_scores  = score.sentiment( Prod12_text, pos, neg, .keyword=keywords[12,1], .progress='text')
Prod13_scores  = score.sentiment( Prod13_text, pos, neg, .keyword=keywords[13,1], .progress='text')
Prod14_scores  = score.sentiment( Prod14_text, pos, neg, .keyword=keywords[14,1], .progress='text')
Prod15_scores  = score.sentiment( Prod15_text, pos, neg, .keyword=keywords[15,1], .progress='text')
Prod16_scores  = score.sentiment( Prod16_text, pos, neg, .keyword=keywords[16,1], .progress='text')
Prod17_scores  = score.sentiment( Prod17_text, pos, neg, .keyword=keywords[17,1], .progress='text')
Prod18_scores  = score.sentiment( Prod18_text, pos, neg, .keyword=keywords[18,1], .progress='text')
Prod19_scores  = score.sentiment( Prod19_text, pos, neg, .keyword=keywords[19,1], .progress='text')
Prod20_scores  = score.sentiment( Prod20_text, pos, neg, .keyword=keywords[20,1], .progress='text')
Prod21_scores  = score.sentiment( Prod21_text, pos, neg, .keyword=keywords[21,1], .progress='text')
Prod22_scores  = score.sentiment( Prod22_text, pos, neg, .keyword=keywords[22,1], .progress='text')
Prod23_scores  = score.sentiment( Prod23_text, pos, neg, .keyword=keywords[23,1], .progress='text')
Prod24_scores  = score.sentiment( Prod24_text, pos, neg, .keyword=keywords[24,1], .progress='text')
Prod25_scores  = score.sentiment( Prod25_text, pos, neg, .keyword=keywords[25,1], .progress='text')
Prod26_scores  = score.sentiment( Prod26_text, pos, neg, .keyword=keywords[26,1], .progress='text')
Prod27_scores  = score.sentiment( Prod27_text, pos, neg, .keyword=keywords[27,1], .progress='text')
Prod28_scores  = score.sentiment( Prod28_text, pos, neg, .keyword=keywords[28,1], .progress='text')
Prod29_scores  = score.sentiment( Prod29_text, pos, neg, .keyword=keywords[29,1], .progress='text')
Prod30_scores  = score.sentiment( Prod30_text, pos, neg, .keyword=keywords[30,1], .progress='text')
Prod31_scores  = score.sentiment( Prod31_text, pos, neg, .keyword=keywords[31,1], .progress='text')
Prod32_scores  = score.sentiment( Prod32_text, pos, neg, .keyword=keywords[32,1], .progress='text')
Prod33_scores  = score.sentiment( Prod33_text, pos, neg, .keyword=keywords[33,1], .progress='text')
Prod34_scores  = score.sentiment( Prod34_text, pos, neg, .keyword=keywords[34,1], .progress='text')
Prod35_scores  = score.sentiment( Prod35_text, pos, neg, .keyword=keywords[35,1], .progress='text')
Prod36_scores  = score.sentiment( Prod36_text, pos, neg, .keyword=keywords[36,1], .progress='text')
Prod37_scores  = score.sentiment( Prod37_text, pos, neg, .keyword=keywords[37,1], .progress='text')
Prod38_scores  = score.sentiment( Prod38_text, pos, neg, .keyword=keywords[38,1], .progress='text')
Prod39_scores  = score.sentiment( Prod39_text, pos, neg, .keyword=keywords[39,1], .progress='text')
Prod40_scores  = score.sentiment( Prod40_text, pos, neg, .keyword=keywords[40,1], .progress='text')
Prod41_scores  = score.sentiment( Prod41_text, pos, neg, .keyword=keywords[41,1], .progress='text')
Prod42_scores  = score.sentiment( Prod42_text, pos, neg, .keyword=keywords[42,1], .progress='text')
Prod43_scores  = score.sentiment( Prod43_text, pos, neg, .keyword=keywords[43,1], .progress='text')
Prod44_scores  = score.sentiment( Prod44_text, pos, neg, .keyword=keywords[44,1], .progress='text')
Prod45_scores  = score.sentiment( Prod45_text, pos, neg, .keyword=keywords[45,1], .progress='text')
Prod46_scores  = score.sentiment( Prod46_text, pos, neg, .keyword=keywords[46,1], .progress='text')
Prod47_scores  = score.sentiment( Prod47_text, pos, neg, .keyword=keywords[47,1], .progress='text')
Prod48_scores  = score.sentiment( Prod48_text, pos, neg, .keyword=keywords[48,1], .progress='text')
Prod49_scores  = score.sentiment( Prod49_text, pos, neg, .keyword=keywords[49,1], .progress='text')
Prod50_scores  = score.sentiment( Prod50_text, pos, neg, .keyword=keywords[50,1], .progress='text')
######################################################################


# Step 6. Save the Sentiment Score into env variable.
######################################################################
global.scores <- new.env(hash=TRUE)                  # Allocate new memory.
global.scores[["Conan Exiles"]]     <- score.global(Prod1_scores, .keyword = keywords[1,1])      # Set an item value.
global.scores[["Dead Head"]]        <- score.global(Prod2_scores, .keyword = keywords[2,1])      # Set an item value.
global.scores[["Viking"]]           <- score.global(Prod3_scores, .keyword = keywords[3,1])      # Set an item value.
global.scores[["Street Fighter"]]   <- score.global(Prod4_scores, .keyword = keywords[4,1])      # Set an item value.
global.scores[["Hybrid"]]           <- score.global(Prod5_scores, .keyword = keywords[5,1])      # Set an item value.
global.scores[["WWE"]]              <- score.global(Prod6_scores, .keyword = keywords[6,1])      # Set an item value.
global.scores[["Guilty Party"]]     <- score.global(Prod7_scores, .keyword = keywords[7,1])      # Set an item value.
global.scores[["Guilty Gear"]]      <- score.global(Prod8_scores, .keyword = keywords[8,1])      # Set an item value.
global.scores[["Civilization"]]     <- score.global(Prod9_scores, .keyword = keywords[9,1])      # Set an item value.
global.scores[["NBA"]]              <- score.global(Prod10_scores, .keyword = keywords[10,1])      # Set an item value.
global.scores[["Miles Edgeworth"]]  <- score.global(Prod11_scores, .keyword = keywords[11,1])      # Set an item value.
global.scores[["The King of Fighters"]]<- score.global(Prod12_scores, .keyword = keywords[12,1])      # Set an item value.
global.scores[["Blue Dragon"]]      <- score.global(Prod13_scores, .keyword = keywords[13,1])      # Set an item value.
global.scores[["Back to the future"]]<- score.global(Prod14_scores, .keyword = keywords[14,1])      # Set an item value.
global.scores[["Golden Sun"]]       <- score.global(Prod15_scores, .keyword = keywords[15,1])      # Set an item value.
global.scores[["House of the dead"]]<- score.global(Prod16_scores, .keyword = keywords[16,1])      # Set an item value.
global.scores[["Cuphead"]]          <- score.global(Prod17_scores, .keyword = keywords[17,1])      # Set an item value.
global.scores[["The Last Blade"]]   <- score.global(Prod18_scores, .keyword = keywords[18,1])      # Set an item value.
global.scores[["Marvel vs. Capcom"]]<- score.global(Prod19_scores, .keyword = keywords[19,1])      # Set an item value.
global.scores[["Dead Rising"]]      <- score.global(Prod20_scores, .keyword = keywords[20,1])      # Set an item value.
global.scores[["Devil May Cry"]]    <- score.global(Prod21_scores, .keyword = keywords[21,1])      # Set an item value.
global.scores[["The walking dead"]] <- score.global(Prod22_scores, .keyword = keywords[22,1])      # Set an item value.
global.scores[["Batman"]]           <- score.global(Prod23_scores, .keyword = keywords[23,1])      # Set an item value.
global.scores[["Folklore"]]         <- score.global(Prod24_scores, .keyword = keywords[24,1])      # Set an item value.
global.scores[["Mortal Kombat"]]    <- score.global(Prod25_scores, .keyword = keywords[25,1])      # Set an item value.
global.scores[["Stuntman"]]         <- score.global(Prod26_scores, .keyword = keywords[26,1])      # Set an item value.
global.scores[["Warhammer"]]        <- score.global(Prod27_scores, .keyword = keywords[27,1])      # Set an item value.
global.scores[["Dead Island"]]      <- score.global(Prod28_scores, .keyword = keywords[28,1])      # Set an item value.
global.scores[["Nier"]]             <- score.global(Prod29_scores, .keyword = keywords[29,1])      # Set an item value.
global.scores[["The Club"]]         <- score.global(Prod30_scores, .keyword = keywords[30,1])      # Set an item value.
global.scores[["Bayonetta"]]        <- score.global(Prod31_scores, .keyword = keywords[31,1])      # Set an item value.
global.scores[["For Honor"]]        <- score.global(Prod32_scores, .keyword = keywords[32,1])      # Set an item value.
global.scores[["Freedom Planet"]]   <- score.global(Prod33_scores, .keyword = keywords[33,1])      # Set an item value.
global.scores[["Proving Ground"]]   <- score.global(Prod34_scores, .keyword = keywords[34,1])      # Set an item value.
global.scores[["Lego Batman"]]      <- score.global(Prod35_scores, .keyword = keywords[35,1])      # Set an item value.
global.scores[["Lego Marvel"]]      <- score.global(Prod36_scores, .keyword = keywords[36,1])      # Set an item value.
global.scores[["Tomb Raider"]]      <- score.global(Prod37_scores, .keyword = keywords[37,1])      # Set an item value.
global.scores[["Final Fantasy"]]    <- score.global(Prod38_scores, .keyword = keywords[38,1])      # Set an item value.
global.scores[["Sonic Mania"]]      <- score.global(Prod39_scores, .keyword = keywords[39,1])      # Set an item value.
global.scores[["Skullgirls"]]       <- score.global(Prod40_scores, .keyword = keywords[40,1])      # Set an item value.
global.scores[["Jet Set Radio"]]    <- score.global(Prod41_scores, .keyword = keywords[41,1])      # Set an item value.
global.scores[["Game of thrones"]]  <- score.global(Prod42_scores, .keyword = keywords[42,1])      # Set an item value.
global.scores[["Sonic Generations"]]<- score.global(Prod43_scores, .keyword = keywords[43,1])      # Set an item value.
global.scores[["Dragon Ball Fighterz"]] <- score.global(Prod44_scores, .keyword = keywords[44,1])      # Set an item value.
global.scores[["Garou"]]            <- score.global(Prod45_scores, .keyword = keywords[45,1])      # Set an item value.
global.scores[["Killer Instinct"]]  <- score.global(Prod46_scores, .keyword = keywords[46,1])      # Set an item value.
global.scores[["Ghost Trick"]]      <- score.global(Prod47_scores, .keyword = keywords[47,1])      # Set an item value.
global.scores[["World Ends With You"]] <- score.global(Prod48_scores, .keyword = keywords[48,1])      # Set an item value.
global.scores[["Resistance"]]       <- score.global(Prod49_scores, .keyword = keywords[49,1])      # Set an item value.
global.scores[["Jeanne d'Arc"]]     <- score.global(Prod50_scores, .keyword = keywords[50,1])      # Set an item value.
######################################################################


# Step 7. Create dataframes out of sentiment text
######################################################################

Prod1_df  <- data.frame(Prod1_scores$text)      #Conan Exiles')
Prod2_df  <- data.frame(Prod2_scores$text)      #Dead Head')
Prod3_df  <- data.frame(Prod3_scores$text)      #Viking')
Prod4_df  <- data.frame(Prod4_scores$text)      #Street Fighter')
Prod5_df  <- data.frame(Prod5_scores$text)      #Hybrid')
Prod6_df  <- data.frame(Prod6_scores$text)      #WWE')
Prod7_df  <- data.frame(Prod7_scores$text)      #Guilty Party')
Prod8_df  <- data.frame(Prod8_scores$text)      #Guilty Gear')
Prod9_df  <- data.frame(Prod9_scores$text)      #Civilization')
Prod10_df <- data.frame(Prod10_scores$text)     #NBA')
Prod11_df <- data.frame(Prod11_scores$text)     #Miles Edgeworth')
Prod12_df <- data.frame(Prod12_scores$text)     #The King of Fighters')
Prod13_df <- data.frame(Prod13_scores$text)     #Blue Dragon')
Prod14_df <- data.frame(Prod14_scores$text)     #Back to the future')
Prod15_df <- data.frame(Prod15_scores$text)     #Golden Sun')
Prod16_df <- data.frame(Prod16_scores$text)     #House of the dead')
Prod17_df <- data.frame(Prod17_scores$text)     #Cuphead')
Prod18_df <- data.frame(Prod18_scores$text)     #The Last Blade')
Prod19_df <- data.frame(Prod19_scores$text)     #Marvel vs. Capcom')
Prod20_df <- data.frame(Prod20_scores$text)     #Dead Rising')
Prod21_df <- data.frame(Prod21_scores$text)     #Devil May Cry')
Prod22_df <- data.frame(Prod22_scores$text)     #The walking dead')
Prod23_df <- data.frame(Prod23_scores$text)     #Batman')
Prod24_df <- data.frame(Prod24_scores$text)     #Folklore')
Prod25_df <- data.frame(Prod25_scores$text)     #Mortal Kombat')
Prod26_df <- data.frame(Prod26_scores$text)     #Stuntman')
Prod27_df <- data.frame(Prod27_scores$text)     #Warhammer')
Prod28_df <- data.frame(Prod28_scores$text)     #Dead Island')
Prod29_df <- data.frame(Prod29_scores$text)     #Nier')
Prod30_df <- data.frame(Prod30_scores$text)     #The Club')
Prod31_df <- data.frame(Prod31_scores$text)     #Bayonetta')
Prod32_df <- data.frame(Prod32_scores$text)     #For Honor')
Prod33_df <- data.frame(Prod33_scores$text)     #Freedom Planet')
Prod34_df <- data.frame(Prod34_scores$text)     #Proving Ground')
Prod35_df <- data.frame(Prod35_scores$text)     #Lego Batman')
Prod36_df <- data.frame(Prod36_scores$text)     #Lego Marvel')
Prod37_df <- data.frame(Prod37_scores$text)     #Tomb Raider')
Prod38_df <- data.frame(Prod38_scores$text)     #Final Fantasy')
Prod39_df <- data.frame(Prod39_scores$text)     #Sonic Mania')
Prod40_df <- data.frame(Prod40_scores$text)     #Skullgirls')
Prod41_df <- data.frame(Prod41_scores$text)     #Jet Set Radio')
Prod42_df <- data.frame(Prod42_scores$text)     #Game of thrones')
Prod43_df <- data.frame(Prod43_scores$text)     #Sonic Generations')
Prod44_df <- data.frame(Prod44_scores$text)     #Dragon Ball Fighterz')
Prod45_df <- data.frame(Prod45_scores$text)     #Garou')
Prod46_df <- data.frame(Prod46_scores$text)     #Killer Instinct')
Prod47_df <- data.frame(Prod47_scores$text)     #Ghost Trick')
Prod48_df <- data.frame(Prod48_scores$text)     #World Ends With You')
Prod49_df <- data.frame(Prod49_scores$text)     #Resistance')
Prod50_df <- data.frame(Prod50_scores$text)     #Jeanne d'Arc")
######################################################################


# Step 8. Rename "" Column in Prodn_df to text.
######################################################################
names(Prod1_df)[1]<-"text"
names(Prod2_df)[1]<-"text"
names(Prod3_df)[1]<-"text"
names(Prod4_df)[1]<-"text"
names(Prod5_df)[1]<-"text"
names(Prod6_df)[1]<-"text"
names(Prod7_df)[1]<-"text"
names(Prod8_df)[1]<-"text"
names(Prod9_df)[1]<-"text"
names(Prod10_df)[1]<-"text"

names(Prod11_df)[1]<-"text"
names(Prod12_df)[1]<-"text"
names(Prod13_df)[1]<-"text"
names(Prod14_df)[1]<-"text"
names(Prod15_df)[1]<-"text"
names(Prod16_df)[1]<-"text"
names(Prod17_df)[1]<-"text"
names(Prod18_df)[1]<-"text"
names(Prod19_df)[1]<-"text"
names(Prod20_df)[1]<-"text"

names(Prod21_df)[1]<-"text"
names(Prod22_df)[1]<-"text"
names(Prod23_df)[1]<-"text"
names(Prod24_df)[1]<-"text"
names(Prod25_df)[1]<-"text"
names(Prod26_df)[1]<-"text"
names(Prod27_df)[1]<-"text"
names(Prod28_df)[1]<-"text"
names(Prod29_df)[1]<-"text"
names(Prod30_df)[1]<-"text"

names(Prod31_df)[1]<-"text"
names(Prod32_df)[1]<-"text"
names(Prod33_df)[1]<-"text"
names(Prod34_df)[1]<-"text"
names(Prod35_df)[1]<-"text"
names(Prod36_df)[1]<-"text"
names(Prod37_df)[1]<-"text"
names(Prod38_df)[1]<-"text"
names(Prod39_df)[1]<-"text"
names(Prod40_df)[1]<-"text"

names(Prod41_df)[1]<-"text"
names(Prod42_df)[1]<-"text"
names(Prod43_df)[1]<-"text"
names(Prod44_df)[1]<-"text"
names(Prod45_df)[1]<-"text"
names(Prod46_df)[1]<-"text"
names(Prod47_df)[1]<-"text"
names(Prod48_df)[1]<-"text"
names(Prod49_df)[1]<-"text"
names(Prod50_df)[1]<-"text"
######################################################################


# Step 9. Concatenate all the 500 rows of text into single string
######################################################################

# How R handles missing values: 
# https://stats.idre.ucla.edu/r/faq/how-does-r-handle-missing-values/

Prod1.data  <- paste(Prod1_df[1:nrow(Prod1_df),],   collapse = " ")     #Conan Exiles')
Prod2.data  <- paste(Prod2_df[1:nrow(Prod2_df),],   collapse = " ")     #Dead Head')
Prod3.data  <- paste(Prod3_df[1:nrow(Prod3_df),],   collapse = " ")     #Viking')
Prod4.data  <- paste(Prod4_df[1:nrow(Prod4_df),],   collapse = " ")     #Street Fighter')
Prod5.data  <- paste(Prod5_df[1:nrow(Prod5_df),],   collapse = " ")     #Hybrid')
Prod6.data  <- paste(Prod6_df[1:nrow(Prod6_df),],   collapse = " ")     #WWE')
Prod7.data  <- paste(Prod7_df[1:nrow(Prod7_df),],   collapse = " ")     #Guilty Party')
Prod8.data  <- paste(Prod8_df[1:nrow(Prod8_df),],   collapse = " ")     #Guilty Gear')
Prod9.data  <- paste(Prod9_df[1:nrow(Prod9_df),],   collapse = " ")     #Civilization')
Prod10.data <- paste(Prod10_df[1:nrow(Prod10_df),], collapse = " ")     #NBA')
Prod11.data <- paste(Prod11_df[1:nrow(Prod11_df),], collapse = " ")     #Miles Edgeworth')
Prod12.data <- paste(Prod12_df[1:nrow(Prod12_df),], collapse = " ")     #The King of Fighters')
Prod13.data <- paste(Prod13_df[1:nrow(Prod13_df),], collapse = " ")     #Blue Dragon')
Prod14.data <- paste(Prod14_df[1:nrow(Prod14_df),], collapse = " ")     #Back to the future')
Prod15.data <- paste(Prod15_df[1:nrow(Prod15_df),], collapse = " ")     #Golden Sun')
Prod16.data <- paste(Prod16_df[1:nrow(Prod16_df),], collapse = " ")     #House of the dead')
Prod17.data <- paste(Prod17_df[1:nrow(Prod17_df),], collapse = " ")     #Cuphead')
Prod18.data <- paste(Prod18_df[1:nrow(Prod18_df),], collapse = " ")     #The Last Blade')
Prod19.data <- paste(Prod19_df[1:nrow(Prod19_df),], collapse = " ")     #Marvel vs. Capcom')
Prod20.data <- paste(Prod20_df[1:nrow(Prod20_df),], collapse = " ")     #Dead Rising')
Prod21.data <- paste(Prod21_df[1:nrow(Prod21_df),], collapse = " ")     #Devil May Cry')
Prod22.data <- paste(Prod22_df[1:nrow(Prod22_df),], collapse = " ")     #The walking dead')
Prod23.data <- paste(Prod23_df[1:nrow(Prod23_df),], collapse = " ")     #Batman')
Prod24.data <- paste(Prod24_df[1:nrow(Prod24_df),], collapse = " ")     #Folklore')
Prod25.data <- paste(Prod25_df[1:nrow(Prod25_df),], collapse = " ")     #Mortal Kombat')
Prod26.data <- paste(Prod26_df[1:nrow(Prod26_df),], collapse = " ")     #Stuntman')
Prod27.data <- paste(Prod27_df[1:nrow(Prod27_df),], collapse = " ")     #Warhammer')
Prod28.data <- paste(Prod28_df[1:nrow(Prod28_df),], collapse = " ")     #Dead Island')
Prod29.data <- paste(Prod29_df[1:nrow(Prod29_df),], collapse = " ")     #Nier')
Prod30.data <- paste(Prod30_df[1:nrow(Prod30_df),], collapse = " ")     #The Club')
Prod31.data <- paste(Prod31_df[1:nrow(Prod31_df),], collapse = " ")     #Bayonetta')
Prod32.data <- paste(Prod32_df[1:nrow(Prod32_df),], collapse = " ")     #For Honor')
Prod33.data <- paste(Prod33_df[1:nrow(Prod33_df),], collapse = " ")     #Freedom Planet')
Prod34.data <- paste(Prod34_df[1:nrow(Prod34_df),], collapse = " ")     #Proving Ground')
Prod35.data <- paste(Prod35_df[1:nrow(Prod35_df),], collapse = " ")     #Lego Batman')
Prod36.data <- paste(Prod36_df[1:nrow(Prod36_df),], collapse = " ")     #Lego Marvel')
Prod37.data <- paste(Prod37_df[1:nrow(Prod37_df),], collapse = " ")     #Tomb Raider')
Prod38.data <- paste(Prod38_df[1:nrow(Prod38_df),], collapse = " ")     #Final Fantasy')
Prod39.data <- paste(Prod39_df[1:nrow(Prod39_df),], collapse = " ")     #Sonic Mania')
Prod40.data <- paste(Prod40_df[1:nrow(Prod40_df),], collapse = " ")     #Skullgirls')
Prod41.data <- paste(Prod41_df[1:nrow(Prod41_df),], collapse = " ")     #Jet Set Radio')
Prod42.data <- paste(Prod42_df[1:nrow(Prod42_df),], collapse = " ")     #Game of thrones')
Prod43.data <- paste(Prod43_df[1:nrow(Prod43_df),], collapse = " ")     #Sonic Generations')
Prod44.data <- paste(Prod44_df[1:nrow(Prod44_df),], collapse = " ")     #Dragon Ball Fighterz')
Prod45.data <- paste(Prod45_df[1:nrow(Prod45_df),], collapse = " ")     #Garou')
Prod46.data <- paste(Prod46_df[1:nrow(Prod46_df),], collapse = " ")     #Killer Instinct')
Prod47.data <- paste(Prod47_df[1:nrow(Prod47_df),], collapse = " ")     #Ghost Trick')
Prod48.data <- paste(Prod48_df[1:nrow(Prod48_df),], collapse = " ")     #World Ends With You')
Prod49.data <- paste(Prod49_df[1:nrow(Prod49_df),], collapse = " ")     #Resistance')
Prod50.data <- paste(Prod50_df[1:nrow(Prod50_df),], collapse = " ")     #Jeanne d'Arc")
######################################################################


# Step 10. Combine data from all the strings into a universal dataframe and prediction data.
######################################################################

universal.data = data.frame(text = c(
  Prod1.data,Prod2.data,Prod3.data,Prod4.data,Prod5.data,Prod6.data,Prod7.data,Prod8.data,Prod9.data,Prod10.data,
  Prod11.data,Prod12.data,Prod13.data,Prod14.data,Prod15.data,Prod16.data,Prod17.data,Prod18.data,Prod19.data,Prod20.data,
  Prod21.data,Prod22.data,Prod23.data,Prod24.data,Prod25.data,Prod26.data,Prod27.data,Prod28.data,Prod29.data,Prod30.data,
  Prod31.data,Prod32.data,Prod33.data,Prod34.data,Prod35.data,Prod36.data,Prod37.data,Prod38.data,Prod39.data,Prod40.data,
  Prod41.data,Prod42.data,Prod43.data,Prod44.data,Prod45.data,Prod46.data,Prod47.data,Prod48.data,Prod49.data,Prod50.data), 
  score = 
    c(get("Conan Exiles", envir = global.scores),
      get("Dead Head", envir = global.scores),
      get("Viking", envir = global.scores),
      get("Street Fighter", envir = global.scores),
      get("Hybrid", envir = global.scores),
      get("WWE", envir = global.scores),
      get("Guilty Party", envir = global.scores),
      get("Guilty Gear", envir = global.scores),
      get("Civilization", envir = global.scores),
      get("NBA", envir = global.scores),
      get("Miles Edgeworth", envir = global.scores),
      get("The King of Fighters", envir = global.scores),
      get("Blue Dragon", envir = global.scores),
      get("Back to the future", envir = global.scores),
      get("Golden Sun", envir = global.scores),
      get("House of the dead", envir = global.scores),
      get("Cuphead", envir = global.scores),
      get("The Last Blade", envir = global.scores),
      get("Marvel vs. Capcom", envir = global.scores),
      get("Dead Rising", envir = global.scores),
      get("Devil May Cry", envir = global.scores),
      get("The walking dead", envir = global.scores),
      get("Batman", envir = global.scores),
      get("Folklore", envir = global.scores),
      get("Mortal Kombat", envir = global.scores),
      get("Stuntman", envir = global.scores),
      get("Warhammer", envir = global.scores),
      get("Dead Island", envir = global.scores),
      get("Nier", envir = global.scores),
      get("The Club", envir = global.scores),
      get("Bayonetta", envir = global.scores),
      get("For Honor", envir = global.scores),
      get("Freedom Planet", envir = global.scores),
      get("Proving Ground", envir = global.scores),
      get("Lego Batman", envir = global.scores),
      get("Lego Marvel", envir = global.scores),
      get("Tomb Raider", envir = global.scores),
      get("Final Fantasy", envir = global.scores),
      get("Sonic Mania", envir = global.scores),
      get("Skullgirls", envir = global.scores),
      get("Jet Set Radio", envir = global.scores),
      get("Game of thrones", envir = global.scores),
      get("Sonic Generations", envir = global.scores),
      get("Dragon Ball Fighterz", envir = global.scores),
      get("Garou", envir = global.scores),
      get("Killer Instinct", envir = global.scores),
      get("Ghost Trick", envir = global.scores),
      get("World Ends With You", envir = global.scores),
      get("Resistance", envir = global.scores),
      get("Jeanne d'Arc", envir = global.scores)
))

######################################################################


# Step 11. H1: Investigate Correlation.
######################################################################

# http://www.sthda.com/english/wiki/correlation-test-between-two-variables-in-r
displaydata = data.frame(game = as.character(Star_Ratings[,1]), mean_scores =  as.matrix(universal.data$score), reviews = as.matrix(Star_Ratings[,2])) # for visualization only.

# Visualization Data
cordata = data.frame(x =  displaydata$mean_scores, y = displaydata$reviews) 

# Plot Generation
ggscatter(cordata, x = "x", y = "y", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "points", ylab = "out of 5 stars")

# Correlation Test
x <- displaydata[["mean_scores"]]
y <- displaydata[["reviews"]]
result <- cor.test(x,y,method = "pearson")

# Test Result
result

######################################################################


# Step 12. H2: Perform Machine Learning
###################################################################### 

train_data = label_data(universal.data)

#From the training dataset select 70% of the tweets for training and 30% to validate the results
samp_id = sample(1:nrow(train_data),              # examine/ inspect the data 
                 round(nrow(train_data)*.70),     # 70% records will be used for training
                 replace = F)                     # sampling without replacement.

train = train_data[samp_id,]                      # 70% of training data set, examine struc of samp_id obj
test = train_data[-samp_id,]                      # remaining 30% of training data set

train.data = rbind(train,test)                    # join the data sets

#Process the text data and create DTM (Document Term Matrix)
train.data$text = tolower(train.data$text)        # Convert to lower case

text = train.data$text                            # omit last line of code.
text = removePunctuation(text)                    # remove punctuation marks
text = removeNumbers(text)                        # remove numbers
text = stripWhitespace(text)                      # remove blank space
text = removeWords(text,stopwords("english")) 

cor = Corpus(VectorSource(text))                  # Create text corpus
dtm = DocumentTermMatrix(cor,                     # Create DTM
                         control = list(weighting =             
                                          function(x)
                                            weightTfIdf(x, normalize = F))) # IDF weighing
## colnames(dtm) # shows vocabulary.

train_dtm = dtm[samp_id, ] 
test_dtm = dtm[-samp_id, ] 
train_y = train.data$score[samp_id]
test_y = train.data$score[-samp_id]

container <- create_container(dtm,                # creates a 'container' obj for training, classifying, and analyzing docs
                              t(train_y),         # transpose function - labels or the Y variable / outcome we want to train on
                              trainSize = 1:nrow(train_dtm) ,
                              # testSize = NULL,  # NO NEED TO SET test data, because we already split the data into train and test sets
                              virgin = FALSE)
svm_model <- train_models(container,              # ?train_models; makes a model object using the specified algorithms.
                          algorithms=c("SVM"))    #"MAXENT","SVM","GLMNET","SLDA","TREE","BAGGING","BOOSTING","RF"

# train accuracy and confusion matrix
train_pred = predict(svm_model, type="response")$SVM
caret::confusionMatrix(train_pred, as.factor(train_y))

# test accuracy and confusion matrix
test_pred = predict(svm_model, newdata = test_dtm)$SVM
caret::confusionMatrix(test_pred, as.factor(test_y))

###################################################################### 
