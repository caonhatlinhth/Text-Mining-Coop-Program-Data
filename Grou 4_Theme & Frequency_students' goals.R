
# load library

library(corpus)
library(tm)
library(dplyr)
library(RWeka)
library(readxl)
import_data <- read_excel("C:/Users/Lenovo/OneDrive - Drexel University/mis group 4 (project)/student_dataset.xlsx" ,  sheet = "Goals student")

#Dataset column to read the text
text_data <- import_data$`Please submit a 400-word reflective analysis on how one aspect of this co-op experience relates to a personal, academic, or professional goal that you are pursuing at Drexel. Be specific about both your goal and how one aspect of the co-op relates to this`

#tm.VCorpus package basically transforms a collection of texts or metadata into a standardize object.
all_data <- VCorpus(VectorSource(text_data))


# Make tokenizer function 
bigramTkn <- function(x) {
  NGramTokenizer(x, Weka_control(min = 2, max = 2))
}


#Tokenizing and cleaning uninformative words

clean_corpus <- function(corpus){
  corpus   %>%
    tm_map(removePunctuation)  %>%            #Remove punctuation
    tm_map(stripWhitespace)  %>%              #Remove white space
    tm_map(removeNumbers)  %>%                #Remove numbers
    tm_map(stemDocument)  %>%                 #Reducing the same meaning of words
    tm_map( content_transformer(function(x) gsub(x, pattern = "relat", replacement = "relate"))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "busi", replacement = "business"))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "pursu", replacement = "pursue"))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "compani", replacement = "company"))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "servic", replacement = "service"))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "futur", replacement = "future" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "emerg", replacement = "emergency" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "bodi", replacement = "body" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "absolut", replacement = "absolute" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "everi", replacement = "every" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "estat", replacement = "estate" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "environ", replacement = "environment" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "realli", replacement = "really" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "arriv", replacement = "arrive" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "experi", replacement = "experience" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "achiev", replacement = "achieve" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "communic", replacement = "communication" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "communicateate", replacement = "communication" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "person", replacement = "personal" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "profession", replacement = "professional" ))) %>%  #Remove a custom Stemming
    tm_map(content_transformer(tolower))  %>% #Change the case to lower case
    tm_map(removeWords, stopwords("en"))  %>% #Remove common English word (stop words)
    tm_map(removeWords,c(stopwords("english"),"coop","the", "and", "i", "me", "you", 
                         "your", "are", "was", "us", "a", "they", "their", "theirs", 
                         "he", "him", "his", "na","co-op","also","abl","will","get","well",
                         "she", "her", "hers","etc","ie", "eg")) #Remove a custom vector of words to adjust for things like e.g., i.e., etc.
  
}


corpus_all <- clean_corpus(all_data)

# converting document term matrix unstructure to structure format
dtm <- DocumentTermMatrix(corpus_all, control = list(tokenize = bigramTkn))
# sum over columns and get total count for each terms
freq_terms <- colSums(as.matrix(dtm))
# total number of terms
length(freq_terms)
sort_terms <- order(freq_terms, decreasing = TRUE)



# Frequency term statistic table functions
frequency_term <- function(c_in){
  doc_term_mat <- TermDocumentMatrix(c_in, control = list(tokenize = bigramTkn) )
  freq_terms <- findFreqTerms(doc_term_mat)[1:max(doc_term_mat$nrow)]
  terms_grouped <- doc_term_mat[freq_terms,]  %>%
    as.matrix()  %>%
    rowSums()  %>%
    data.frame(Term=freq_terms, Frequency = .)  %>%
    arrange(desc(Frequency))  %>%
    mutate(pct_corp=Frequency/nrow(.)) %>%
    mutate(culm_sum=cumsum(pct_corp))
  return(data.frame(terms_grouped))
}

# adding to document term matrix to data frame output
frequency_terms <- data.frame(frequency_term(corpus_all))
head(frequency_terms, 25)
