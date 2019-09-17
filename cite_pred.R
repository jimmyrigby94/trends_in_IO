# citation rates analyses
cite_pred <- function(data, date, group, unigram, threshhold) {

  # helper function
  cited_by <- function(data) {
                glm(`Cited by` ~ present, data = data, family = "poisson")
              }

  # tidy eval
  date1 <- enquo(date)
  group1 <- enquo(group)

  # replaces comma seporators with boolean operator and converts to lowercase (database has already been converted)
  unigram1 <- str_replace_all(unigram, pattern = ", ", "|") %>% tolower()

  # frequency counts for term/phrase matches per article
  count <- data %>% mutate(word_freq = str_count(string = data$Abstract, pattern = unigram1),
                           present = if_else(word_freq >= threshhold, 1, 0),
                           decade = cut(Year,
                           breaks = c(-Inf, 1960, 1970, 1980, 1990, 2000, 2010, 2020),
                           right = TRUE,
                           labels = c("Pre-1960s", "1960s", "1970s", "1980s", "1990s", "2000s", "2010s"))) %>%
           group_by(decade) %>% nest() %>%
           mutate(Model = map(data, cited_by), tidy_lm = map(Model, tidy))

  temp <- count %>% unnest(tidy_lm) %>%
          mutate(term = if_else(term == "(Intercept)", "Terms Not Present", "Terms Present")) %>%
          mutate_if(.predicate = is.numeric, funs(round), digits = 2)

  temp2 <- left_join(temp %>% filter(term != "Terms Not Present") %>% select(-term),
                     temp %>% filter(term == "Terms Not Present") %>%
                       select(decade, estimate) %>% rename(Mean = estimate)) %>%
                       mutate(`Estimate With` = exp(Mean + estimate), `Estimate Without` = exp(Mean)) %>%
                       select(Decade = decade, `Estimate Without`, `Estimate With`, Effect = estimate,
                              `Standard Error` = std.error, Statistic = statistic, `P Value` = p.value) %>%
                       arrange(Decade)
}