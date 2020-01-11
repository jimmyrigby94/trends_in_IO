# Function to convert user search into regular expression
prep_unigram<-function(unigram){
   str_replace_all(unigram, pattern = ", ", "|") %>% 
    tolower()
}
