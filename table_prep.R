# preps the table
table_prep <- function(data, unigram, threshhold, upper_year, lower_year) {

  # preps unigram for search
  unigram1 <- str_replace_all(unigram, pattern = ", ", "|") %>% tolower()

  # preps frequency counts for user-specified search term(s)/phrase(s) matches
  count <- tibble(`Matching Terms` = str_count(string = data$Abstract, pattern = unigram1))

  # imposes filters, arranges by relevance, and calculates relative citation rates
  temp <- cbind(data, count) %>%
          filter(Year > lower_year, Year < upper_year, `Matching Terms` >= threshhold) %>%
          arrange(desc(`Matching Terms`)) %>%
          select(Authors:Issue, EID, `Matching Terms`, `Cited by`:Link) %>%
          group_by(Year) %>%
          mutate(`Relative Citation Rate` = `Cited by` - mean(`Cited by`, na.rm = TRUE) / sd(`Cited by`, na.rm = TRUE),
                 `log(Relative Citation Rate)` = log10(`Relative Citation Rate`))%>%
    as.data.frame(.)
  return(temp)
}