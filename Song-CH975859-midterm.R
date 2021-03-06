library(haven)
library(labelled)
library(plotly)
library(MASS)
library(ggplot2)
library(scales)
library(plyr)
library(reshape2)
library(dplyr)
library(directlabels)
library(ggrepel)
library(shiny)

genmemData <- read_sav("./gen-mem-file1-Merged1985-2010_Data_112211.sav")
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

groupedPost911 <- subset(groupedGens, survey=="SRC 2001-02/POST" | survey=="NCS 2001-02/POST" | survey=="SRC 2009-10", select=c(survey, cohort17, freq911, total911, perc911))

oldestCohorts <- c(1900, seq(1911, 1986, 5))
youngestCohorts <- c(seq(1910, 1985, 5), 1991)

oldestEndwar <- 1945 - oldestCohorts
youngestStartwar <- 1941 - youngestCohorts
warAgeRange <- data.frame(Youngest=youngestStartwar, Oldest=oldestEndwar)
warAgeRange <- subset(warAgeRange, Youngest >= 0 | Oldest >= 0)
warAgeRange$Youngest[8] <- 0


# Figure 2: WWII Mentioned
ggplot() +
  geom_point(data=groupedGens, aes(x=groupedGens$cohort17, y=groupedGens$percwwii, group=groupedGens$survey, color=groupedGens$survey), shape=19, size=1.5) +
  stat_smooth(data=groupedGens, aes(x=groupedGens$cohort17, y=groupedGens$percwwii, group=groupedGens$survey, color=groupedGens$survey, weight=groupedGens$totalwwii), se=FALSE, span=0.55, size=0.5, linetype="dashed") +
  geom_point(data=weightedGroups, aes(x=weightedGroups$cohort17, y=weightedGroups$allpercwwii), shape=17, size=4, color="black") +
  stat_smooth(data=weightedGroups, aes(x=weightedGroups$cohort17, y=weightedGroups$allpercwwii, group=1, weight=weightedGroups$totalwwii, linetype="Weighted Average"), color="black", span=0.55, se=FALSE, size=2) +
  annotate("text", x=0, y=0, label="Age during war:", hjust=0.75, vjust=1, size=4) +
  annotate("text",
           x=c(seq(1, 8, 1)), 
           y=0, 
           hjust=0.5,
           vjust=1,
           size=5,
           label=c(paste(as.character(warAgeRange$Youngest), as.character(warAgeRange$Oldest), sep="-", collapse=NULL))
           ) +
  geom_dl(data=groupedGens, aes(x=groupedGens$cohort17, y=groupedGens$percwwii, group=groupedGens$survey, color=groupedGens$survey, label=survey), method=list("last.points", cex=0.8, vjust=0.5, hjust=-0.09)) +
  geom_dl(data=weightedGroups, aes(x=weightedGroups$cohort17, y=weightedGroups$allpercwwii, label="Weighted Average"), method=list("first.points", cex=1.25, vjust=1.5, hjust=1)) +
  scale_x_discrete(expand=c(0.07, 0)) +
  scale_y_continuous(labels=percent) +
  labs(x="Birth Cohort", y="Percentage of Cohort that Mentioned Event", color="Survey", linetype="", title="Percentage of Cohorts that Mentioned World War II (with regression lines)") +
  theme(text=element_text(family="Times", size=18), axis.text.x=element_text(angle=45, hjust=1), legend.position="")


# Figure 3 Mod: 9/11
# Purpose/argument: Address one confound (recency effect) by showing evidence of critical period regardless of time.
# Limitations: Recycled same plot format; lazy. Superfluous x-ticks. Still used weighted average even though the whole point is to show DIFFERENCES across survey, so aggregating them is unnecessary.
oldest911 <- c(seq(1926, 1986, 5))
youngest911 <- c(seq(1930, 1985, 5), 1991)

oldest911 <- 2001 - oldest911
youngest911 <- 2001 - youngest911
sept11AgeRange <- data.frame(Youngest=youngest911, Oldest=oldest911)

ageLabels <- c(paste(as.character(sept11AgeRange$Youngest), as.character(sept11AgeRange$Oldest), sep="-", collapse=NULL))

ggplot(groupedPost911, aes(x=groupedPost911$cohort17, y=groupedPost911$perc911, group=groupedPost911$survey, color=groupedPost911$survey)) +
  geom_point(shape=19, size=3) +
  stat_smooth(se=FALSE, span=0.7, size=0.5, linetype="dashed") +
  geom_dl(aes(label=survey), method=list("last.points", cex=0.9, vjust=0.3, hjust=-0.1)) +
  scale_x_discrete(expand=c(0.11, 0), labels=ageLabels) +
  scale_y_continuous(labels=percent) +
  labs(x="Age during Event", y="Percentage of Cohort that Mentioned Event", color="Survey", shape="Survey", title="Percentage of Cohorts that Mentioned 9/11") +
  theme(text=element_text(family="Times", size=18), axis.text.x=element_text(angle=45, hjust=1), legend.position="none")

ggplot(groupedPost911, aes(x=groupedPost911$survey, y=groupedPost911$perc911, group=groupedPost911$cohort17, color=groupedPost911$cohort17)) +
  geom_bar(aes(fill=groupedPost911$cohort17), stat="identity", position="dodge", width=0.7) +
  scale_x_discrete(limits=c("NCS 2001-02/POST", "SRC 2001-02/POST", "SRC 2009-10"), labels=c("Sept - Nov 2001", "Nov 2001 - Jan 2002", "Aug 2009 - Mar 2010")) +
  scale_y_continuous(labels=percent) +
  scale_fill_discrete(name="Age during Event", labels=ageLabels) +
  scale_color_discrete(name="Age during Event", labels=ageLabels) +
  labs(x="Time Period Surveyed", y="Percentage of Cohort that Mentioned Event", color="Birth Cohort", shape="Birth Cohort", title="Percentage of Cohorts that Mentioned 9/11 (2001) at Different Points in Time") +
  guides(fill=guide_legend(nrow = 1, 
                           direction = "horizontal",
                           title.position = "top",
                           label.position = "bottom",
                           label.hjust = 0.5,
                           label.vjust = -1)) +
  theme(text=element_text(family="Times", size=18), legend.position="bottom", legend.key.width=unit(2.5, "cm"), legend.box.spacing=unit(1, "cm"))


# Figure 5 Mod: Vietnam.
# Purpose/argument: Strongest evidence of critical period.
# Limitations: Explains age at event in text, but when viewing the graph, this is not made explicit, despite this information being critical to the argument!
# Fix: Add annotation/v-line. Point out ages at critical events (start of war, 1968 crisis, end of war) for modal cohort (46-50).
# Fig 5 Mod 1:
ggplot() +
  geom_point(data=groupedGens, aes(x=groupedGens$cohort17, y=groupedGens$percviet, group=groupedGens$survey, color=groupedGens$survey), shape=19, size=1) +
  stat_smooth(data=groupedGens, aes(x=groupedGens$cohort17, y=groupedGens$percviet, group=groupedGens$survey, color=groupedGens$survey, weight=groupedGens$totalviet), span=0.55, se=FALSE, size=0.5, linetype="dashed") +
  geom_point(data=weightedGroups, aes(x=weightedGroups$cohort17, y=weightedGroups$allpercviet), shape=17, size=4, color="black") +
  stat_smooth(data=weightedGroups, aes(x=weightedGroups$cohort17, y=weightedGroups$allpercviet, group=1, weight=weightedGroups$totalviet, linetype="Weighted Average"), color="black", span=0.55, se=FALSE, size=2) +
  geom_vline(weightedGroups, xintercept=which.max(weightedGroups$allpercviet) + 1, linetype="dashed") +
  scale_y_continuous(labels=percent) +
  labs(x="Birth Cohort", y="Percentage of Cohort that Mentioned Event", color="Survey", shape="Survey", linetype="", title="Percentage of Cohorts that Mentioned Vietnam War (with regression lines)") +
  annotate("text", x=8.9, y=0.015, hjust=1, label="1946-50 Cohort Apprx Age at:") +
  annotate("text", x=8.8, y=0, hjust=1, label="Start of war, 1965 = 17") +
  annotate("text", x=8.8, y=-.01, hjust=1, label="Crisis in 1968 = 20") +
  annotate("text", x=8.8, y=-0.02, hjust=1, label="End of war, 1973 = 25") +
  annotate("segment", 
           x=6.1,
           xend=8.9, 
           y=.01, 
           yend=.01, 
           color="black", size=0.5) +
  theme(text=element_text(family="Times", size=18), axis.text.x=element_text(angle=45, hjust=1), legend.justification=c(1, 1), legend.position=c(1, 1))

# Fig 5 Mod 2: Weighted Average only
ggplot(weightedGroups, aes(x=weightedGroups$cohort17, y=weightedGroups$allpercviet)) +
  geom_point(shape=19, size=4) +
  stat_smooth(aes(group=1,  weight=weightedGroups$totalviet), se=FALSE, color="black", size=1.5) +
  scale_y_continuous(labels=percent) +
  geom_vline(weightedGroups, xintercept=which.max(weightedGroups$allpercviet), linetype="dashed") +
  annotate("text", x=7.9, y=0.02, hjust=1, label="1946-50 Cohort Apprx Age at:") +
  annotate("text", x=7.8, y=0.01, hjust=1, label="Start of war, 1965 = 17") +
  annotate("text", x=7.8, y=0.005, hjust=1, label="Crisis in 1968 = 20") +
  annotate("text", x=7.8, y=0, hjust=1, label="End of war, 1973 = 25") +
  annotate("segment", 
           x=5.5,
           xend=7.9, 
           y=.015, 
           yend=.015, 
           color="black", size=0.5) +
  labs(x="Birth Cohort", y="Percentage of Respondents who Mentioned Event", color="Survey", shape="Survey", title="Percentage of Cohorts that Mentioned Vietnam War (weighted avg of all surveys w/ regression line)") +
  theme(text=element_text(family="Times", size=18), axis.text.x=element_text(angle=45, hjust=1))


# Fig 5 Mod 3: Interactive
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
