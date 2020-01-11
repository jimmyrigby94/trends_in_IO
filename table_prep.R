# preps the table
table_prep <- function(data) {

  data%>%
    select(Authors:Issue, EID, `Matching Terms`, `Cited by`:Link)

}
