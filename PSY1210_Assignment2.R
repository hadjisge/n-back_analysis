library(tidyverse)
library(dplyr)

# read in data ---------------------------------------------------------------------------------------
raw_data <- read_csv("22-4.csv")


#using piping, select relevant columns to analyze-----------------------------------------------------
branch_data <- raw_data %>%
  select(Subject, Session, ExternalStimulus, Task, DisplayStim.ACC, DisplayStim.RT, Phase, PhaseSwitch) %>%
  filter(Task == "branch") %>%
  mutate(Trial = 1:length(Subject)) #add column to count the number of trials in the task for plots

#make a plot to visualize reaction time throughout the session--------------------------------------------

#BRANCH
branch_rt_overtime <- ggplot(branch_data, aes(x = Trial, y = DisplayStim.RT)) + geom_point()
branch_rt_overtime
ggsave('branch_rt_overtime.pdf',units='in',width=4,height=5)

#recode PhaseSwitch column so that values are in words rather than numbers and NA for ease of bar graph x axis labels later on

branch_data <- mutate(branch_data, PhaseSwitch = recode(branch_data$PhaseSwitch, "1" = "primary", "2" = "secondary", "3" = "return"), PhaseSwitch = replace_na(PhaseSwitch, "average")) 

  
#get the average ratings across rating types for this session

ratings <- raw_data %>% #new tibble for ratings
  select(ExternalStimulus, Intensity, Salience, Unpleasantness) %>% #select the rating scales
  filter(!is.na(Intensity)) %>% #filter values that are not NA. This removes NA from the Salience and Unpleasantness variables
  mutate(Intensity_avg = mean(Intensity), Salience_avg = mean(Salience), Unpleasantness_avg = mean(Unpleasantness)) #calculate the mean for each rating

  
#TASK ANALYSIS BRANCH--------------------------------------------------------------------------------
#I will analyze reaction times of correct trial types only. A value in the PhaseSwitch column corresponds to the first trial in each phase of the task i.e. the first trial of the new phase after it has switched.
#The first trial in PhaseSwitch #1 is called the "primary" trial

#primary
branch_rt_primary <- branch_data %>% #new tibble for this trial type
  filter(DisplayStim.ACC == 1, PhaseSwitch == "primary") %>% #filter for correct trials (accuracy = 1) and for the first phase switch or "primary" trial type
  group_by(Subject, PhaseSwitch, ExternalStimulus, Task) %>% #group by the variables of interest
  mutate(meanRT = mean(DisplayStim.RT), sd = sd(DisplayStim.RT), RT_Outliers = abs(DisplayStim.RT-meanRT)>2*sd) %>% #find the mean reaction time and standard deviation of reaction times to identify standard deviations greater than |2| as outliers "TRUE" or "FALSE"
  filter(RT_Outliers == "FALSE") %>% #remove outliers by filtering out "TRUE" values
  summarize(meanRT_clean = mean(DisplayStim.RT), sem = (sd(DisplayStim.RT))/sqrt(length(Subject))) #create new columns with the clean mean reaction time and standard deviation

#mutate(branch_rt_primary, sem = sd/sqrt(length(Subject)))
#rt_bar_graph <- ggplot(summary_branch, aes(x = PhaseSwitch, y = meanRT_clean)) + geom="bar"

#doing the same as above for the remaining trial types
#The first trial in PhaseSwitch #2 is called the "secondary" trial
branch_rt_secondary <- branch_data %>% #new tibble for this trial type
  filter(DisplayStim.ACC == 1, PhaseSwitch == "secondary") %>% #filter for correct trials and for the second phase switch or "secondary" trial type
  group_by(Subject, PhaseSwitch, ExternalStimulus, Task) %>%
  mutate(meanRT = mean(DisplayStim.RT), sd = sd(DisplayStim.RT), RT_Outliers = abs(DisplayStim.RT-meanRT)>2*sd) %>%
  filter(RT_Outliers == "FALSE") %>%
  summarize(meanRT_clean = mean(DisplayStim.RT), sem = (sd(DisplayStim.RT))/sqrt(length(Subject)))

#The first trial in PhaseSwitch #3 is called the "return" trial
branch_rt_return <- branch_data %>% #new tibble for this trial type
  filter(DisplayStim.ACC == 1, PhaseSwitch == "return") %>% #filter for correct trials and for the third phase switch or "return" trial type
  group_by(Subject, PhaseSwitch, ExternalStimulus, Task) %>%
  mutate(meanRT = mean(DisplayStim.RT), sd = sd(DisplayStim.RT), RT_Outliers = abs(DisplayStim.RT-meanRT)>2*sd) %>%
  filter(RT_Outliers == "FALSE") %>%
  summarize(meanRT_clean = mean(DisplayStim.RT), sem = (sd(DisplayStim.RT))/sqrt(length(Subject)))

#The NA trials in PhaseSwitch represent the remaining trials within each phase (primary, secondary, return) combined. These are called the "average" trials
branch_rt_average <- branch_data %>% #new tibble for this trial type
  filter(DisplayStim.ACC == 1, PhaseSwitch == "average") %>% #we still want correct trials, but now we want the remaining trials of each phase, not the first value for each phase, so we select the rows in which PhaseSwitch = NA
  group_by(Subject, PhaseSwitch, ExternalStimulus, Task) %>%
  mutate(meanRT = mean(DisplayStim.RT), sd = sd(DisplayStim.RT), RT_Outliers = abs(DisplayStim.RT-meanRT)>2*sd) %>%
  filter(RT_Outliers == "FALSE") %>%
  summarize(meanRT_clean = mean(DisplayStim.RT), sem = (sd(DisplayStim.RT))/sqrt(length(Subject)))


#Analyzing error rate
#error rate for primary trials
branch_er_primary <- branch_data %>% #need a new tibble where we can view the errors for this trial type
  filter(PhaseSwitch == "primary") %>% #look at only primary trials
  group_by(Subject, PhaseSwitch, ExternalStimulus, Task) %>% #group by these variables
  summarize(ErrorRate = 100 - mean(DisplayStim.ACC==1)*100) #calculate error rate by subtracting the mean accuracy from 100

#doing the same as above for the remaining trial types
#error rate for secondary trials
branch_er_secondary <- branch_data %>%
  filter(PhaseSwitch == "secondary") %>%
  group_by(Subject, PhaseSwitch, ExternalStimulus, Task) %>%
  summarize(ErrorRate = 100 - mean(DisplayStim.ACC==1)*100)

#error rate for return trials
branch_er_return <- branch_data %>%
  filter(PhaseSwitch == "return") %>%
  group_by(Subject, PhaseSwitch, ExternalStimulus, Task) %>%
  summarize(ErrorRate = 100 - mean(DisplayStim.ACC==1)*100)

#error rate for average trials
branch_er_average <- branch_data %>%
  filter(PhaseSwitch == "average") %>% #want to get all average trials by selecting for "NA" values in the PhaseSwitch column
  group_by(Subject, PhaseSwitch, ExternalStimulus, Task) %>%
  summarize(ErrorRate = 100 - mean(DisplayStim.ACC==1)*100)

#Create a summary of task performance for reaction times and error rates
summary_er_branch <- bind_rows(branch_er_primary, branch_er_secondary, branch_er_return, branch_er_average)
summary_rt_branch <- bind_rows(branch_rt_primary, branch_rt_secondary, branch_rt_return, branch_rt_average)


#join the two summary tibbles to generate one general summary for the switch task
summary_branch <- left_join(summary_er_branch, summary_rt_branch)

#will make PhaseSwitch values factors to order them as they've been listed in the bar graphs 

summary_branch$PhaseSwitch <- factor(summary_branch$PhaseSwitch, levels = c('primary', 'secondary', 'return', 'average'), ordered = TRUE)

#make bar graphs to show summaries of task performance
rt_bar_graph <- ggplot(summary_branch, aes(x = PhaseSwitch, y = meanRT_clean)) + 
  geom_bar(stat='identity', aes(colour = PhaseSwitch, fill = PhaseSwitch), show.legend = TRUE, position = position_dodge(width = 0.7)) + 
  geom_errorbar(aes(ymin = meanRT_clean-sem, ymax = meanRT_clean+sem), width = 0.7) +
  labs(title="Mean RT Branch", x="Phase Switch", y = "Reaction Time (ms)")
rt_bar_graph

ggsave('summary_rt_branch.pdf',units='in',width=4,height=5)

er_bar_graph <- ggplot(summary_branch, aes(x = PhaseSwitch, y = ErrorRate)) + 
  geom_bar(stat='identity', aes(colour = PhaseSwitch, fill = PhaseSwitch), show.legend = TRUE, position = position_dodge(width = 0.7)) + 
  labs(title="Mean ER Branch", x="Phase Switch", y = "Error Rate (%)")
er_bar_graph

ggsave('summary_er_branch.pdf',units='in',width=4,height=5)

#save summary task performance and ratings as separate csv files-------------------------------
write.csv(summary_branch, "summary_branch.csv")
write.csv(ratings, "summary_ratings.csv")

#bar graph