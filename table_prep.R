#Preps the table
table_prep<-function(data, unigram, threshhold, since){
  
  # Prepping Unigram for search
  unigram1<-str_replace_all(unigram, pattern = ", ", "|")%>%
    tolower()
  
  # Counting number of matches to each term
  count<-tibble(`Matching Terms` = str_count(string = data$Abstract, pattern = unigram1))
  
  # Imposing Filters, arranging by relevance, and calculating relative citation rates
  temp<-cbind(data,count)%>%
    filter(Year>since,
           `Matching Terms`>=threshhold)%>%
    arrange(desc(`Matching Terms`))%>%
    select(Authors:Issue,  EID, `Matching Terms`, `Cited by`:Link)%>%
    group_by(Year)%>%
    mutate(`Relative Citation Rate` = `Cited by`-mean(`Cited by`, na.rm = TRUE)/sd(`Cited by`, na.rm = TRUE),
           `log(Relative Citation Rate)` = log10(`Relative Citation Rate`))
  
  return(temp)
}