# helper function
cited_by_models <- function(data) {
  if(length(levels(data$Present))>1){
  glm(`Cited by` ~ Present, data = data, family = "poisson")
  }else{
  glm(`Cited by` ~ 1, data = data, family = "poisson")
  }
}
