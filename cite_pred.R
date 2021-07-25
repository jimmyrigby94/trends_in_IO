# citation rates analyses
cite_pred <- function(data) {

  # frequency counts for term/phrase matches per article
  count <- data %>% mutate(decade = cut(Year,
                                        breaks = c(-Inf, 1960, 1970, 1980, 1990, 2000, 2010, 2020, Inf),
                                        right = TRUE,
                                        labels = c("Pre-1960s", "1960s", "1970s", "1980s", "1990s", "2000s", "2010s", '2020s'))) %>%
           group_by(decade) %>% 
           nest() %>%
           mutate(Model = map(data, cited_by_models), 
                  tidy_lm = map(Model, tidy))

  temp <- count %>% 
          unnest(tidy_lm) %>%
          ungroup()%>%
          mutate(term = if_else(term == "(Intercept)", "Terms Not Present", "Terms Present"))
  


  temp2 <- left_join(temp %>% filter(term == "Terms Not Present") %>%
                       select(decade, estimate) %>% rename(Mean = estimate),
                     temp %>% filter(term != "Terms Not Present") %>% select(-term)) %>%
                       mutate(`Estimate With` = exp(Mean + estimate), `Estimate Without` = exp(Mean)) %>%
                       select(Decade = decade, `Estimate Without`, `Estimate With`, Effect = estimate,
                              `Standard Error` = std.error, `t` = statistic, `p` = p.value) %>%
                       arrange(Decade)%>%
    mutate_if(.predicate = is.numeric, list(~format(round(., digits = 2), nsmall = 2)))%>%
    mutate(`Estimate With` = if_else(`Estimate With`=="NA", "Not Enough Information", `Estimate With`))%>%
    mutate_at(.vars = vars(Effect:p), ~if_else(. == "NA", "-", .))%>%
    as.data.frame(.)
  
  
  temp2
}
