# helper function
cited_by_models <- function(data) {
  glm(`Cited by` ~ Present, data = data, family = "poisson")
}
