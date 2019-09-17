tidy_trend_plot <- function(data, date, group, unigram, threshhold, prop, byjourn, since) {

  #tidy eval quasi-quotation
  date1 <- enquo(date)
  group1 <- enquo(group)

  # replaces comma seporators with boolean operator and converts to lowercase (database has already been converted)
  unigram1 <- str_replace_all(unigram, pattern = ", ", "|") %>% tolower()

  # frequency counts for term/phrase matches
  count <- str_count(string = data$Abstract, pattern = unigram1)

  #filters the dataset based on user's threshhold parameter, counts the number of articles for each year & journal, & formats/renames key variables
  if (prop == TRUE) {

    # generates proportions of articles that meet user specified search terms/phrases & threshhold parameter
      # uses left_join() to merge data 
        # left side => total number of articles published in journal/year
        # right side => number of articles that meet user specified threshhold parameter
    temp <- left_join(data %>% count(!!date1,!!group1) %>%
                        rename(Journal = !!group1, Year = !!date1, Published = n),
                      data[count >= threshhold, ] %>%
                        count(!!date1, !!group1) %>%
                        rename(Journal = !!group1, Year = !!date1, Articles = n)) %>%
                        mutate(Proportion = if_else(is.na(Articles), 0, Articles / Published))

  if (byjourn == TRUE) {

    # generates plot by journal
    ggplotly(ggplot(temp %>% filter(Year > since),
                    aes(x = Year, y = Proportion)) +
                    geom_line(aes(color = Journal)) +
                    geom_point(aes(color = Journal))+
      theme(panel.background = element_blank(),
            panel.grid = element_blank(),
            axis.line = element_line())+
      guides(color = guide_legend(title = NULL)),
      tooltip = c("x", "y", "colour"))
  } else {

    # frequency counts are collapsed across journal and proportion is recalculated
      ggplotly(temp %>% filter(Year > since) %>% ungroup %>% group_by(Year) %>%
               summarise(Articles = sum(Articles, na.rm = TRUE),
               Published = sum(Published, na.rm = TRUE)) %>%
               mutate(Proportion = Articles / Published) %>%
               ggplot(aes(x = Year, y = Proportion)) +
                      geom_line() +
                      geom_point()+
                 theme(panel.background = element_blank(),
                       panel.grid = element_blank(),
                       axis.line = element_line())+
                 guides(color = guide_legend(title = NULL)))
    }
  } else {

      # if proportion == FALSE, provides frequency counts for number of articles that exceed user specified threshhold parameter
      art_year <- data %>% count(`Source title`, Year)

      temp <- left_join(art_year %>% select(Journal = `Source title`, Year),
                        data[count >= threshhold, ] %>%
                          count(!!date1, !!group1) %>%
                          rename(Journal = !!group1,
                                 Year = !!date1,
                                 Articles = n))

    # if there are no matches for a given year, 0 is imputed
    temp$Articles[is.na(temp$Articles)] <- 0

    # uses conditional logic to plot by journal or aggregation of all journals
    if (byjourn == TRUE) {
          ggplotly(ggplot(temp %>% filter(Year > since),
                          aes(x = Year, y = Articles)) +
                          geom_line(aes(color = Journal)) +
                          geom_point(aes(color = Journal))+
                     theme(panel.background = element_blank(),
                           panel.grid = element_blank(),
                           axis.line = element_line())+
                     guides(color = guide_legend(title = NULL)),
                   tooltip = c("x", "y", "colour"))

    } else {

      ggplotly(temp %>% filter(Year > since) %>% ungroup %>%
               group_by(Year) %>% summarise(Articles = sum(Articles)) %>%
               ggplot(aes(x = Year, y = Articles))+ 
                 geom_line() + 
                 geom_point()+
                 theme(panel.background = element_blank(),
                       panel.grid = element_blank(),
                       axis.line = element_line()))
    }
  }
}