search_abstract<-function(data, unigram, threshhold, date){
  data<-data %>% 
    filter(Year>=date[1], Year<=date[2])
  
  data%>%
    mutate(`Matching Terms` = str_count(string = data$Abstract, pattern = unigram),
           Present = if_else(`Matching Terms` >= threshhold, "Present", "Absent"),
           Present = factor(Present, levels = c("Absent", "Present")))
}
