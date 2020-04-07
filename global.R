# helper
library(shiny)
library(tidyverse)
library(tidytext)
library(plotly)
library(knitr)
library(shinydashboard)
library(rintrojs)
library(shinycssloaders)

# import dependencies
source("tidy_trend_plot.R")
source("cite_pred.R")
source("download_prep.R")
source("table_prep.R")
source("cite_plot.R")
source("prep_unigram.R")
source("search_abstract.R")
source("cited_by_models.R")

init_selected<-c(
  "Journal of Applied Psychology",
  "Personnel Psychology",
  "Academy of Management Journal",
  "Journal of Management",
  "Journal of Occupational and Organizational Psychology",
  "International Journal of Selection and Assessment",
  "Organizational Behavior and Human Decision Processes",
  "Journal of Vocational Behavior",
  "Academy of Management Review",
  "Psychological Bulletin",
  "Human Performance",
  "American Psychologist",
  "Journal of Business and Psychology",
  "Leadership Quarterly",
  "Journal of Applied Social Psychology",
  "Journal of Occupational Health Psychology",
  "Applied Psychology"
)
# defines journal names for the named vector in order to reduce repeating code
j_names <- c(
  "Academy of Management Executive" = "Academy of Management Executive",
  "Academy of Management Journal" = "Academy of Management Journal",
  "Academy of Management Perspectives" = "Academy of Management Perspectives",
  "Academy of Management Review" = "Academy of Management Review",
  "Administrative Science Quarterly" = "Administrative Science Quarterly",
  "American Psychologist" = "American Psychologist",
  "Annual Review of Psychology" = "Annual Review of Psychology",
  "Applied Psychological Measurement" = "Applied Psychological Measurement",
  "Applied Psychology" = "Applied Psychology",
  "Assessment" = "Assessment",
  "Basic and Applied Social Psychology" = "Basic and Applied Social Psychology",
  "Behavior Research Methods & Instrumentation" = "Behavior Research Methods & Instrumentation",
  "Behavior Research Methods" = "Behavior Research Methods",
  "Behavior Research Methods, Instruments, & Computers" = "Behavior Research Methods, Instruments, & Computers",
  "Computers in Human Behavior" = "Computers in Human Behavior",
  "Current Directions in Psychological Science" = "Current Directions in Psychological Science",
  "Educational and Psychological Measurement" = "Educational and Psychological Measurement",
  "European Journal of Psychological Assessment" = "European Journal of Psychological Assessment",
  "European Journal of Work and Organizational Psychology" = "European Journal of Work and Organizational Psychology",
  "Foundations and Trends in Human-Computer Interaction" = "Foundations and Trends in Human-Computer Interaction",
  "Group & Organization Management" = "Group & Organization Management",
  "Group Dynamics" = "Group Dynamics",
  "Handbook of Employee Selection" = "Handbook of Employee Selection",
  "Handbook of Employee Selection, Second Edition" = "Handbook of Employee Selection, Second Edition",
  "Harvard Business Review" = "Harvard Business Review",
  "Historical Perspectives in Industrial and Organizational Psychology" = "Historical Perspectives in Industrial and Organizational Psychology",
  "Human Factors" = "Human Factors",
  "Human Performance" = "Human Performance",
  "Human Relations" = "Human Relations",
  "Human Resource Management Journal" = "Human Resource Management Journal",
  "Human Resource Management Review" = "Human Resource Management Review",
  "Human Resource Management" = "Human Resource Management",
  "Human-Computer Interaction" = "Human-Computer Interaction",
  "Industrial and Organizational Psychology" = "Industrial and Organizational Psychology",
  "International Journal of Human Resource Management" = "International Journal of Human Resource Management",
  "International Journal of Human-Computer Interaction" = "International Journal of Human-Computer Interaction",
  "International Journal of Human-Computer Studies" = "International Journal of Human-Computer Studies",
  "International Journal of Selection and Assessment" = "International Journal of Selection and Assessment",
  "International Journal of Stress Management" = "International Journal of Stress Management",
  "International Journal of Training and Development" = "International Journal of Training and Development",
  "International Review of Industrial and Organizational Psychology" = "International Review of Industrial and Organizational Psychology",
  "Journal of Applied Psychology" = "Journal of Applied Psychology",
  "Journal of Applied Social Psychology" = "Journal of Applied Social Psychology",
  "Journal of Behavioral Decision Making" = "Journal of Behavioral Decision Making",
  "Journal of Business and Psychology" = "Journal of Business and Psychology",
  "Journal of Educational Measurement" = "Journal of Educational Measurement",
  "Journal of Experimental Psychology: General" = "Journal of Experimental Psychology General",
  "Journal of Individual Differences" = "Journal of Individual Differences",
  "Journal of Leadership and Organizational Studies" = "Journal of Leadership and Organizational Studies",
  "Journal of Management Studies" = "Journal of Management Studies",
  "Journal of Management" = "Journal of Management",
  "Journal of Managerial Psychology" = "Journal of Managerial Psychology",
  "Journal of Occupational and Organizational Psychology" = "Journal of Occupational and Organizational Psychology",
  "Journal of Occupational Health Psychology" = "Journal of Occupational Health Psychology",
  "Journal of Organizational Behavior Management" = "Journal of Organizational Behavior Management",
  "Journal of Organizational Behavior" = "Journal of Organizational Behavior",
  "Journal of Personality and Social Psychology" = "Journal of Personality and Social Psychology",
  "Journal of Personality" = "Journal of Personality",
  "Journal of Research in Personality" = "Journal of Research in Personality",
  "Journal of Vocational Behavior" = "Journal of Vocational Behavior",
  "Judgment and Decision Making" = "Judgment and Decision Making",
  "Leadership Quarterly" = "Leadership Quarterly",
  "Military Psychology" = "Military Psychology",
  "Organization Science" = "Organization Science",
  "Organizational Behavior and Human Decision Processes" = "Organizational Behavior and Human Decision Processes",
  "Organizational Dynamics" = "Organizational Dynamics",
  "Organizational Psychology Review" = "Organizational Psychology Review",
  "Organizational Research Methods" = "Organizational Research Methods",
  "Personality and Individual Differences" = "Personality and Individual Differences",
  "Personnel Psychology" = "Personnel Psychology",
  "Personnel Review" = "Personnel Review",
  "Perspectives on Psychological Science" = "Perspectives on Psychological Science",
  "Psychological Bulletin" = "Psychological Bulletin",
  "Psychological Methods" = "Psychological Methods",
  "Psychological Review" = "Psychological Review",
  "Psychological Science" = "Psychological Science",
  "Psychometrika" = "Psychometrika",
  "Public Personnel Management" = "Public Personnel Management",
  "Research in Organizational Behavior" = "Research in Organizational Behavior",
  "Research in Personnel and Human Resources Management" = "Research in Personnel and Human Resources Management",
  "Small Group Research" = "Small Group Research",
  "Strategic Management Journal" = "Strategic Management Journal",
  "The International Journal of Human Resource Management" = "The International Journal of Human Resource Management",
  "The Personnel Administrator" = "The Personnel Administrator",
  "Work and Stress" = "Work and Stress"
)

# Reading in Initial Data Files
master<-map_dfr(init_selected, function(x){
  read_rds(paste0("data/", x, ".rds"))
}
)
