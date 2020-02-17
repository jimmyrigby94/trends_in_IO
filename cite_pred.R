# citation rates analyses
cite_pred <- function(data) {

  # frequency counts for term/phrase matches per article
  count <- data %>% mutate(decade = cut(Year,
                                        breaks = c(-Inf, 1960, 1970, 1980, 1990, 2000, 2010, 2020),
                                        right = TRUE,
                                        labels = c("Pre-1960s", "1960s", "1970s", "1980s", "1990s", "2000s", "2010s"))) %>%
           group_by(decade) %>% 
           nest() %>%
           mutate(Model = map(data, cited_by_models), 
                  tidy_lm = map(Model, tidy))

  temp <- count %>% 
          unnest(tidy_lm) %>%
          ungroup() %>%
          mutate(term = if_else(term == "(Intercept)", "Terms Not Present", "Terms Present")) %>%
          mutate_if(.predicate = is.numeric, funs(round), digits = 2)
  


  temp2 <- left_join(temp %>% filter(term == "Terms Not Present") %>%
                       select(decade, estimate) %>% rename(Mean = estimate),
                     temp %>% filter(term != "Terms Not Present") %>% select(-term)) %>%
                       mutate(`Estimate With` = round(exp(Mean + estimate),2), `Estimate Without` = round(exp(Mean),2)) %>%
                       select(Decade = decade, `Estimate Without`, `Estimate With`, Effect = estimate,
                              `Standard Error` = std.error, `t` = statistic, `p` = p.value) %>%
                       arrange(Decade)%>%
    mutate(`Estimate With` = if_else(is.na(`Estimate With`), "Not Enough Information", as.character(`Estimate With`)))%>%
    as.data.frame(.)
  
  temp2
}
