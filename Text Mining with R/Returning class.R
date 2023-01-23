
# load library

library(corpus)
library(tm)
library(dplyr)
library(RWeka)
library(readxl)
import_data <- read_excel("Full_Co-op_dataset.xlsx" ,  sheet = "Skills student")

#Dataset column to read the text
text_data <- import_data$`In returning to classes, what do you want to learn/focus on?`

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
    tm_map( content_transformer(function(x) gsub(x, pattern = "insur", replacement = "insurance"))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "compani", replacement = "company"))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "servic", replacement = "service"))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "roadsid", replacement = "roadside" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "emerg", replacement = "emergency" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "bodi", replacement = "body" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "absolut", replacement = "absolute" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "everi", replacement = "every" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "estat", replacement = "estate" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "environ", replacement = "environment" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "dure", replacement = "difficult" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "arriv", replacement = "arrive" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "experi", replacement = "experience" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "achiev", replacement = "achieve" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "communic", replacement = "communication" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "communicateate", replacement = "communication" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "person", replacement = "personal" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "profession", replacement = "professional" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "manag", replacement = "management" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "continu", replacement = "continue" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "financ", replacement = "finance" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "knowledg", replacement = "knowledge" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "suppli", replacement = "supply" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "realli", replacement = "really" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "analyt", replacement = "analytics" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "improv", replacement = "improve" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "analysi", replacement = "analysis" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "busi", replacement = "business" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "cours", replacement = "course" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "futur", replacement = "future" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "studi", replacement = "study" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "differ", replacement = "different" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "prepar", replacement = "prepare" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "rais", replacement = "raise" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "veri", replacement = "very" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "solv", replacement = "solving" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "inform", replacement = "information" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "analyz", replacement = "analyze" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "financei", replacement = "financial" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "industri", replacement = "industry" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "comput", replacement = "computer" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "scienc", replacement = "science" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "privat", replacement = "private" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "equiti", replacement = "equity" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "appli", replacement = "apply" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "complet", replacement = "complete" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "degre", replacement = "degree" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "consum", replacement = "consumer" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "graduat", replacement = "graduate" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "languag", replacement = "language" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "onli", replacement = "only" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "manag", replacement = "manage" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "informat", replacement = "information" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "atten", replacement = "attention" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "financ", replacement = "finance" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "strategi", replacement = "strategy" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "digit", replacement = "digital" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "suppli", replacement = "supply" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "analyt", replacement = "analysis" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "busi", replacement = "business" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "financee", replacement = "finance" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "analyticsics", replacement = "analysis" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "manageement", replacement = "management" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "businessnessness", replacement = "business" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "financeial", replacement = "financial" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "analysisics", replacement = "analysis" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "believ", replacement = "believe" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "becaus", replacement = "because" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "becom", replacement = "become" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "attitud", replacement = "attitude" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "posit", replacement = "positive" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "attentiont", replacement = "attention" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "demonstr", replacement = "demonstration" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "initi", replacement = "initiative" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "independ", replacement = "independence" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "pleasur", replacement = "pleasure" ))) %>%  #Remove a custom Stemming
    tm_map( content_transformer(function(x) gsub(x, pattern = "qualiti", replacement = "quality" ))) %>%  #Remove a custom Stemming
    

    tm_map(content_transformer(tolower))  %>% #Change the case to lower case
    tm_map(removeWords, stopwords("en"))  %>% #Remove common English word (stop words)
    tm_map(removeWords,c(stopwords("english"),"coop","the", "and", "i", "me", "you", 
                         "your", "are", "was", "us", "a", "they", "their", "theirs", 
                         "he", "him", "his", "na","co-op","also","abl","will","get","well",
                         "she", "her", "hers","etc","ie", "eg", "want", "focus", "return", 
                         "class", "lot","like", "course", "can", "new","want", "continue", 
                         "expand", "knowledge", "learn", "really", "make", "sure", "major", 
                         "id","real", "world", "technical", "look", "forward", "better", "understand", 
                         "career path", "go", "back", "future", "senior", "year", "come", "co op", "gpa", 
                         "prepare", "next", "very", "intern", "technic", "im", "excit", "only", "taken", 
                         "busi", "analyt", "suppli", "because", "become", "improve", "upon", "need", "ani")) #Remove a custom vector of words to adjust for things like e.g., i.e., etc.
  
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
head(frequency_terms, 80)
