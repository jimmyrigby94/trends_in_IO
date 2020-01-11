# citation rates analyses
cite_plot <- function(data) {

  # frequency counts for term/phrase matches per article
  data <- data%>%
    group_by(Year, Present)%>%
    summarise(`Citation Rate` = round(mean(`Cited by`, na.rm = TRUE),2))
    
  
  ggplotly(ggplot(data, aes(x = Year, y = `Citation Rate`, color = Present)) +
             geom_line() + 
             geom_point() +
             scale_colour_manual(values = c("black", "red")) +
             theme(panel.background = element_blank(),
                   panel.grid = element_blank(),
                   axis.line = element_line()) +
             guides(color = guide_legend(title = NULL)),
           tooltip = c("x", "y"))
  
}
