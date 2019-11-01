tidy_trend_plot <- function(data, date, group, unigram, threshhold, prop, byjourn, upper_year, lower_year) {

  # tidy eval quasi-quotation
  date1 <- enquo(date)
  group1 <- enquo(group)

  # replaces comma separators with boolean operators
  # converts to lowercase (database already converted)
  unigram1 <- str_replace_all(unigram, pattern = ", ", "|") %>% tolower()

  # provides frequency counts for user-specified search term(s)/phrase(s) matches
  count <- str_count(string = data$Abstract, pattern = unigram1)

  # filters database based on user-specified threshhold parameter
  # counts number of articles for each year and journal
  # formats/renames key variables
  if (prop == TRUE) {

    # generates proportion of articles meeting user-specified search term(s)/phrase(s) and threshhold parameter
      # uses left_join() to merge data 
        # left side == total number of articles published in journal/year
        # right side == number of articles meeting user-specified threshhold parameter
    temp <- left_join(data %>% count(!!date1,!!group1) %>%
                        rename(Journal = !!group1, Year = !!date1, Published = n),
                      data[count >= threshhold, ] %>%
                        count(!!date1, !!group1) %>%
                        rename(Journal = !!group1, Year = !!date1, Articles = n)) %>%
                        mutate(Proportion = if_else(is.na(Articles), 0, Articles / Published))

  if (byjourn == TRUE) {

    # generates plot by individual journal
    ggplotly(ggplot(temp %>% filter(Year > lower_year, Year < upper_year),
                    aes(x = Year, y = Proportion)) +
                    geom_line(aes(color = Journal)) +
                    geom_point(aes(color = Journal))+
      theme(panel.background = element_blank(),
            panel.grid = element_blank(),
            axis.line = element_line())+
      guides(color = guide_legend(title = NULL)),
      tooltip = c("x", "y", "colour"))
  } else {

    # frequency counts collapsed across journal, and proportion is recalculated
      ggplotly(temp %>% filter(Year > lower_year, Year < upper_year) %>% ungroup %>% group_by(Year) %>%
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

      # if proportion == FALSE, provides frequency counts for number of articles exceeding user-specified threshhold parameter
      art_year <- data %>% count(`Source title`, Year)

      temp <- left_join(art_year %>% select(Journal = `Source title`, Year),
                        data[count >= threshhold, ] %>%
                          count(!!date1, !!group1) %>%
                          rename(Journal = !!group1,
                                 Year = !!date1,
                                 Articles = n))

    # if no matches for a given year, imputes "0"
    temp$Articles[is.na(temp$Articles)] <- 0

    # conditional logic that plots by individual journal or aggregate of all journals
    if (byjourn == TRUE) {
          ggplotly(ggplot(temp %>% filter(Year > lower_year, Year < upper_year),
                          aes(x = Year, y = Articles)) +
                          geom_line(aes(color = Journal)) +
                          geom_point(aes(color = Journal)) +
                     theme(panel.background = element_blank(),
                           panel.grid = element_blank(),
                           axis.line = element_line()) +
                     guides(color = guide_legend(title = NULL)),
                   tooltip = c("x", "y", "colour"))

    } else {

      ggplotly(temp %>% filter(Year > lower_year, Year < upper_year) %>% ungroup %>%
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