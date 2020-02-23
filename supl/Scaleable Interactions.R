# Potentially a more appropriate solution for non-linear interactions (not scalable)
library(palmtree)

test <-
  search_abstract(
    data = master,
    unigram = "personality",
    threshhold = 1,
    date = c(1950, 2020)
  ) %>%
  filter(!is.na(Present),!is.na(`Cited by`),!is.na(Year))

m1 <-
  palmtree(`Cited by` ~  Present |
             0 | Year, data  = test, family = "poisson")

plot(m1)

new_data <-
  expand_grid(Present = as.factor(c("Absent", "Present")), Year = 1960:2020)

new_data$predict <- predict(m1, newdata = new_data)

vcov(m1$palm)

plotly::ggplotly(
  new_data %>%
    group_by(predict, Present) %>%
    summarise(From  = min(Year), To = max(Year)) %>%
    arrange(From) %>%
    ggplot(aes(
      x = From, y = predict, color = Present
    )) +
    geom_step()
)
