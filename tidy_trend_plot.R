tidy_trend_plot<-function(data, date, group, unigram, threshhold, prop, byjourn, since){
  #tidy eval quazi-quotation
  date1 <- enquo(date)
  group1 <- enquo(group)
  
  
  #replaces comma seporators with boolean operator and converts to lowercase (database has already been converted)
  unigram1 <- str_replace_all(unigram, pattern = ", ", "|") %>%
    tolower()
  
  #counts the frequency of term matches per article
  count <- str_count(string = data$Abstract, pattern = unigram1)
  
  #filters the dataset based on a threshhold parameter, counts the number of articles for each year in each journal, and formats/renames key variables
  if (prop == TRUE) {
  
    # Generates proportions of articles that match articles and exceed threshhold
    # Uses left_join() to merge together two data 
    #lhs has total number of articles published in journal/year
    #RHS has number of articles that match the threshhold
    
    temp <- left_join(
      data %>%
        count(!!date1,!!group1) %>%
        rename(
          Journal = !!group1,
          Year = !!date1,
          Published = n
        ),
      data[count >= threshhold, ] %>%
        count(!!date1,!!group1) %>%
        rename(
          Journal = !!group1,
          Year = !!date1,
          Articles = n
        )
    ) %>%
      mutate(Proportion = if_else(is.na(Articles), 0, Articles / Published))
    
    if (byjourn == TRUE) {
    
      # Generates plot by grouping journal
      ggplotly(ggplot(temp%>%filter(Year>since), aes(x = Year, y = Proportion)) +
                 geom_line(aes(group = Journal, color = Journal)) +
                 geom_point(aes(color = Journal, group = Journal)))
    }
    else{
      # If not by journal counts are collapsed across journal and proportion is recalculated
      ggplotly(
        temp %>%
          filter(Year>since)%>%
          ungroup %>%
          group_by(Year) %>%
          summarise(
            Articles = sum(Articles, na.rm = TRUE),
            Published = sum(Published, na.rm = TRUE)
          ) %>%
          mutate(Proportion = Articles / Published) %>%
          ggplot(aes(x = Year, y = Proportion)) +
          geom_line() +
          geom_point()
      )
    }
  }
  else{
    
    # If proprotion == FALSE, simply counts the number of articles that exceed threshhold
    
    art_year<-data%>%
      count(`Source title`, Year)
    
    temp <-
      left_join(
        art_year %>% select(Journal = `Source title`, Year),
        data[count >= threshhold, ] %>%
          count(!!date1,!!group1) %>%
          rename(
            Journal = !!group1,
            Year = !!date1,
            Articles = n
          )
      )
    
    # If there are no matches for a given year, 0 is imputed
    temp$Articles[is.na(temp$Articles)] <- 0
    
    # uses conditional logic to either plot by journal or show global trends as defined by the user
    if (byjourn == TRUE) {
      ggplotly(ggplot(temp%>%filter(Year>since), aes(x = Year, y = Articles)) +
                 geom_line(aes(group = Journal, color = Journal)) +
                 geom_point(aes(color = Journal, group = Journal)))
    }
    else{
      ggplotly(
        temp %>%
          filter(Year>since)%>%
          ungroup %>%
          group_by(Year) %>%
          summarise(Articles = sum(Articles)) %>%
          ggplot(aes(x = Year, y = Articles)) +
          geom_line() +
          geom_point()
      )
    }
  }
}
