# -- SMS Data --

sms <- read.csv(file.choose(),stringsAsFactors = F) # Importing Data
View(sms) # Display SMSs
class(sms) # Class of SMS

sms$type <- factor(sms$type) # Converting SMS "Type" into factor
table(sms$type) # Total count of Spam and Ham

library(tm)
sms_corpus <- Corpus(VectorSource(sms$text)) # Creating a Corpus Data of SMS

stop_words <- readLines(file.choose()) # Importing Stop Words

# Cleaning data
corpus_clean <- tm_map(sms_corpus, tolower) # Convert all characters into Lower cases
corpus_clean <- tm_map(corpus_clean, removeNumbers) # Removing all Numbers
corpus_clean <- tm_map(corpus_clean, removeWords,stop_words) # Removing Stop Words
corpus_clean <- tm_map(corpus_clean, removePunctuation) # Removing Punctuations
corpus_clean <- tm_map(corpus_clean, stripWhitespace) # Removing Extra White-Spaces

class(corpus_clean) # Class of Corpus data

sms_dtm <- DocumentTermMatrix(corpus_clean) # Creating a Document Term Matrix
class(sms_dtm) # Class of DTM of SMS


sms_raw_train <- sms[1:4169,] # Raw Train data
sms_raw_test  <- sms[4170:5559,] # Raw Test data

sms_corpus_train <- corpus_clean[1:4169] # Corpus Train
sms_corpus_test  <- corpus_clean[4170:5559] # Corpus Test

table(sms_raw_train$type) # Total count of Spam and Ham in Train data
table(sms_raw_test$type) # Total count of Spam and Ham in Test data

sms_dict <- findFreqTerms(sms_dtm, 5) # Finding Frequent words with count more than 5
View(sms_dict)

sms_train <- DocumentTermMatrix(sms_corpus_train, list(dictionary = sms_dict)) # Creating Training Dataset
sms_test  <- DocumentTermMatrix(sms_corpus_test, list(dictionary = sms_dict)) # Creating Testing Dataset

inspect(sms_corpus_train[1:100]) # Display
list(sms_dict[1:100])

# convert counts to a factor with value "Yes" and "No"
convert_counts <- function(x) {
  x <- ifelse(x > 0, 1, 0)
  x <- factor(x, levels = c(0, 1), labels = c("No", "Yes"))
}

sms_train <- apply(sms_train, MARGIN = 2, convert_counts) # Applying counts function to Train data
sms_test  <- as.data.frame(apply(sms_test, MARGIN = 2, convert_counts)) # Converting Test data to factor
View(sms_train)
View(sms_test)


library(e1071)
sms_classifier <- naiveBayes(sms_train, sms_raw_train$type) # Naive Bayes Model building
sms_classifier


sms_test_pred <- predict(sms_classifier, sms_test) # Prediction

library(gmodels) # Crosstable between Actual and Predicted Data
CrossTable(sms_test_pred, sms_raw_test$type, prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE, dnn = c('predicted', 'actual'))

mean(sms_test_pred == sms_raw_test$type) # Accuracy of the Model = 97.4%
