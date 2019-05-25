download_prep<-function(data, unigram, threshhold, since){
  
  unigram1<-str_replace_all(unigram, pattern = ", ", "|")%>%
    tolower()
  
  
  count<-tibble(`Relevance Score` = str_count(string = data$Abstract, pattern = unigram1))
  
  temp<-cbind(data,count)%>%
    filter(Year > since,
           `Relevance Score`>=threshhold)%>%
    arrange(desc(`Relevance Score`))%>%
    select(Authors:Issue, Abstract, EID, `Relevance Score`, `Cited by`:Link)
  
  return(temp)
}