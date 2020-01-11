tidy_trend_plot <- function(data, prop, byjourn, date, journals) {
  
  possible_observations<-expand_grid(Year = min(data$Year):date[[2]], 
                                     Journal = unique(data$`Source title`),
                                     Present = unique(data$Present))
  
  
  
  data<-data%>%
          group_by(Year, `Source title`, Present)%>%
          summarise(n = n())%>%
          ungroup()%>%
          rename(Journal = `Source title`)
  
  
  data<-left_join(possible_observations, data)%>%
    mutate(n = if_else(is.na(n), as.double(0), as.double(n)))
    

  # filters database based on user-specified threshhold parameter
  # counts number of articles for each year and journal
  # formats/renames key variables
  if (prop == TRUE) {

  if (byjourn == TRUE) {

    # generates plot by individual journal
  p<- data %>%
        group_by(Year, Journal)%>%
        mutate(Proportion = n/sum(n))%>%
        filter(Present == "Present")%>%
        ggplot(aes(x = Year, y = Proportion)) +
               geom_line(aes(color = Journal)) +
               geom_point(aes(color = Journal))+
      theme(panel.background = element_blank(),
            panel.grid = element_blank(),
            axis.line = element_line())+
      guides(color = guide_legend(title = NULL))
  
  ggplotly(p, tooltip = c("x", "y", "colour"))
  
  } else {
    
    p<- data %>%
      group_by(Year, Present)%>%
      summarise(n = sum(n))%>%
      ungroup()%>%
      group_by(Year)%>%
      mutate(Proportion = n/sum(n))%>%
      filter(Present == "Present")%>%
      ggplot(aes(x = Year, y = Proportion)) +
      geom_line() +
      geom_point()+
      theme(panel.background = element_blank(),
      panel.grid = element_blank(),
      axis.line = element_line())+
      guides(color = guide_legend(title = NULL))
    
    ggplotly(p)
             
             
    }
  } else {

    # conditional logic that plots by individual journal or aggregate of all journals
    if (byjourn == TRUE) {
      
      p<-data%>%
          rename(Articles = n)%>%
          filter(Present == "Present")%>%
          ggplot(aes(x = Year, y = Articles)) +
          geom_line(aes(color = Journal)) +
          geom_point(aes(color = Journal)) +
                     theme(panel.background = element_blank(),
                     panel.grid = element_blank(),
                     axis.line = element_line()) +
                     guides(color = guide_legend(title = NULL))
      
      ggplotly(p, tooltip = c("x", "y", "colour"))

    } else {
      
     p<-data%>%
          filter(Present == "Present")%>%
          group_by(Year)%>%
          summarise(Articles = sum(n))%>%
          ggplot(aes(x = Year, y = Articles))+ 
          geom_line() + 
          geom_point()+
          theme(panel.background = element_blank(),
                panel.grid = element_blank(),
                axis.line = element_line())
    }
  }
}