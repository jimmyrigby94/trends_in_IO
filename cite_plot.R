# citation rates analyses
cite_plot <- function(data, date, group, unigram, threshhold) {

  # tidy eval
  date1 <- enquo(date)
  group1 <- enquo(group)
  
  # replaces comma separators with boolean operators, and converts to lowercase (database already converted)
  unigram1 <- str_replace_all(unigram, pattern = ", ", "|") %>% tolower()
  
  # frequency counts for term/phrase matches per article
  plot_dat <- data %>% mutate(word_freq = str_count(string = data$Abstract, pattern = unigram1),
                           Present = if_else(word_freq >= threshhold, "Present", "Absent"),
                           Present = factor(Present, levels = c("Present", "Absent"))) %>%
    group_by(Year, Present)%>%
    summarise(`Citation Rate` = round(mean(`Cited by`, na.rm = TRUE),2))
    
  
  ggplotly(ggplot(plot_dat, aes(x = Year, y = `Citation Rate`, color = Present)) +
             geom_line() + 
             geom_point() +
             scale_colour_manual(values = c("black", "red")) +
             theme(panel.background = element_blank(),
                   panel.grid = element_blank(),
                   axis.line = element_line()) +
             guides(color = guide_legend(title = NULL)),
           tooltip = c("x", "y"))
  
}