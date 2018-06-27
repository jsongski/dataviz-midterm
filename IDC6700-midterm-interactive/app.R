#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(haven)
library(shiny)
library(ggplot2)
library(scales)
library(dplyr)

genmemData <- read_sav("gen-mem-file1-Merged1985-2010_Data_112211.sav")
genmemData <- as_factor(genmemData)

grpwwii <- with(genmemData, table(survey, cohort17, wwii))
grpwwii <- as.data.frame(grpwwii)
groupedGens <- grpwwii

grpviet <- with(genmemData, table(survey, cohort17, vietnam))
grpviet <- as.data.frame(grpviet)
grp911 <- with(genmemData, table(survey, cohort17, sept11))
grp911 <- as.data.frame(grp911)

mentioned <- subset(grpwwii, wwii=="Mentioned", select=c(survey, cohort17, Freq))
notmentioned <- subset(grpwwii, wwii=="Not mentioned", select=c(survey, cohort17, Freq))
groupedGens <- mentioned
groupedGens <- cbind(groupedGens, totalwwii=mentioned$Freq + notmentioned$Freq)
groupedGens <- cbind(groupedGens, percwwii=mentioned$Freq / (mentioned$Freq + notmentioned$Freq))

mentioned <- subset(grpviet, vietnam=="Mentioned", select=c(survey, cohort17, Freq))
notmentioned <- subset(grpviet, vietnam=="Not mentioned", select=c(survey, cohort17, Freq))
groupedGens <- cbind(groupedGens, freqviet=mentioned$Freq)
groupedGens <- cbind(groupedGens, totalviet=mentioned$Freq + notmentioned$Freq)
groupedGens <- cbind(groupedGens, percviet=mentioned$Freq / (mentioned$Freq + notmentioned$Freq))

mentioned <- subset(grp911, sept11=="Mentioned", select=c(survey, cohort17, Freq))
notmentioned <- subset(grp911, sept11=="Not mentioned", select=c(survey, cohort17, Freq))
groupedGens <- cbind(groupedGens, freq911=mentioned$Freq)
groupedGens <- cbind(groupedGens, total911=mentioned$Freq + notmentioned$Freq)
groupedGens <- cbind(groupedGens, perc911=mentioned$Freq / (mentioned$Freq + notmentioned$Freq))

colnames(groupedGens)[3] <- "freqwwii"
groupedAll <- groupedGens

weightedGroups <- subset(groupedAll, totalwwii > 0 | totalviet > 0 | total911 > 0, select=c(cohort17, survey, freqwwii, totalwwii, freqviet, totalviet))
weightedGroups <- subset(weightedGroups, cohort17 != "1900-10" & cohort17 != "1986-91")
weightedGroups <- aggregate(. ~ cohort17, weightedGroups, sum) # To get weight avg referred to in Appendix C
weightedGroups <- cbind(weightedGroups, allpercwwii=weightedGroups$freqwwii / weightedGroups$totalwwii)
weightedGroups <- cbind(weightedGroups, allpercviet=weightedGroups$freqviet / weightedGroups$totalviet)

groupedGens <- subset(groupedGens, totalwwii >= 40 | totalviet >= 40 | total911 >= 40)

# Fig 5 Interactive
ui <- fluidPage(titlePanel("Percentage of Cohorts that Mentioned Vietnam War (with regression line)"), 
                selectInput(inputId="survey", label="Select individual survey from the menu below:", choices=unique(groupedGens$survey)),
                plotOutput("plot", width="100%", height="500px"))

server <- function(input, output) {
  groupedGensIntrct <- select(groupedGens, -survey)
  
  output$plot <- renderPlot({
    ggplot() +
      geom_point(data=groupedGensIntrct, aes(x=cohort17, y=percviet), shape=19, size=2, color="grey", alpha=0.5) +
      geom_point(data=groupedGens[groupedGens$survey==input$survey,], aes(x=cohort17, y=percviet, color=survey)) +
      stat_smooth(data=groupedGensIntrct, aes(x=cohort17, y=percviet, weight=totalviet), color="grey", group=survey, span=0.55, se=FALSE, size=0.75, linetype="dashed") +
      stat_smooth(data=groupedGens[groupedGens$survey==input$survey,], aes(x=cohort17, y=percviet, color=survey, group=survey, weight=totalviet), span=0.55, se=FALSE, size=0.75, linetype="dashed") +
      geom_point(data=weightedGroups, aes(x=weightedGroups$cohort17, y=weightedGroups$allpercviet), shape=19, size=4, color="black") +
      stat_smooth(data=weightedGroups, aes(x=weightedGroups$cohort17, y=weightedGroups$allpercviet, group=1, linetype="Weighted Average - All Surveys", weight=weightedGroups$totalviet), color="black", span=0.55, se=FALSE, size=1.75) +
      geom_vline(weightedGroups, xintercept=which.max(weightedGroups$allpercviet) + 1, linetype="dashed") +
      scale_y_continuous(labels=percent) +
      labs(x="Birth Cohort", y="Percentage of Respondents who Mentioned Event", color="Survey", shape="Survey", linetype="") +
      annotate("text", x=8.9, y=0.025, hjust=1, label="1946-50 Cohort Apprx Age at:") +
      annotate("text", x=8.8, y=0.01, hjust=1, label="Start of war, 1965 = 17") +
      annotate("text", x=8.8, y=0, hjust=1, label="Crisis in 1968 = 20") +
      annotate("text", x=8.8, y=-0.01, hjust=1, label="End of war, 1973 = 25") +
      annotate("segment", 
               x=6.25,
               xend=8.9, 
               y=.02, 
               yend=.02, 
               color="black", size=0.5) +
      theme(text=element_text(family="Times", size=18), axis.text.x=element_text(angle=45, hjust=1))
  })
}

shinyApp(ui, server)
