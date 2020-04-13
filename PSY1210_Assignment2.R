#Project 2 - Branch Task Performance Analysis
#Author: Georgia Hadjis


#import libraries ------------------------------------------------------------------------------------
library(tidyverse)
library(dplyr)

# read in data ---------------------------------------------------------------------------------------
raw_data <- read_csv("22-4.csv") #in this dataset, the condition is pain and it is session 4. Some of the trials say Session = 5 because the session was interrupted and restarted at that point in a new data file, and they were later concatenated


#using piping, select relevant columns to analyze-----------------------------------------------------
branch_data <- raw_data %>%
  select(Subject, Session, ExternalStimulus, Task, DisplayStim.ACC, DisplayStim.RT, Phase, PhaseSwitch) %>% #columns to analyze
  filter(Task == "branch") %>% #task to analyze
  mutate(Trial = 1:length(Subject)) #add column to count the number of trials in the task for plotting purposes

#make a plot to visualize task performance throughout the session-------------------------------------

#BRANCH Reaction Times
branch_rt_overtime <- ggplot(branch_data, aes(x = Trial, y = DisplayStim.RT)) + geom_point() + geom_smooth(method = "lm")+ #add regression line 
  labs(title="Branch RT over time", x="Trial", y = "Reaction time (ms)")
branch_rt_overtime

ggsave('figures/branch_rt_overtime.pdf',units='in',width=4,height=5)

#BRANCH Accuracy
branch_acc_overtime <- ggplot(branch_data, aes(x = Trial, y = DisplayStim.ACC)) + geom_point() + geom_smooth(method = "lm")+ #add regression line
  labs(title="Branch Accuracy over time", x="Trial", y = "Accuracy") # Accuracy = 1 is correct, Accuracy = 0 is incorrect
branch_acc_overtime

ggsave('figures/branch_acc_overtime.pdf',units='in',width=4,height=5)

#summarize the average ratings across rating types for this session----------------------------------

ratings <- raw_data %>% #new tibble for ratings
  select(ExternalStimulus, Intensity, Salience, Unpleasantness) %>% #select the rating scales
  filter(!is.na(Intensity)) %>% #filter values that are not NA. This removes NA from the Salience and Unpleasantness variables as well
  mutate(Intensity_avg = mean(Intensity), Salience_avg = mean(Salience), Unpleasantness_avg = mean(Unpleasantness)) #calculate the mean rating for each rating type

  
#TASK ANALYSIS BRANCH--------------------------------------------------------------------------------
#I will analyze reaction times of correct trials only. A value in the PhaseSwitch column corresponds to the first trial in each phase of the task i.e. the first trial of the new phase after it has switched.
#The first trial in PhaseSwitch #1 is called the "primary" trial, the first trial in PhaseSwitch #2 is called the "secondary" trial, the first trial in PhaseSwitch #3 is called the "return" trial.
#The NA trials in PhaseSwitch represent the remaining trials within each phase (primary, secondary, return) combined. These are called the "average" trials.
#I will first recode the PhaseSwitch column so that values refer to the trials in words rather than numbers and "NA" for ease of bar graph x axis labels later on

branch_data <- mutate(branch_data, PhaseSwitch = recode(branch_data$PhaseSwitch, "1" = "primary", "2" = "secondary", "3" = "return"), PhaseSwitch = replace_na(PhaseSwitch, "average")) #replace the NA values with "average"

#Analyzing reaction time for each phase type:
#primary
branch_rt_primary <- branch_data %>% #new tibble for this phase type
  filter(DisplayStim.ACC == 1, PhaseSwitch == "primary") %>% #filter for correct trials (accuracy = 1) and for the first phase switch or "primary" phase type
  group_by(Subject, PhaseSwitch, ExternalStimulus, Task) %>% #group by the variables of interest
  mutate(meanRT = mean(DisplayStim.RT), sd = sd(DisplayStim.RT), RT_Outliers = abs(DisplayStim.RT-meanRT)>2*sd) %>% #find the mean reaction time and standard deviation of reaction times to identify standard deviations greater than |2| as outliers "TRUE" or "FALSE"
  filter(RT_Outliers == "FALSE") %>% #remove outliers by filtering out "TRUE" values
  summarize(meanRT_clean = mean(DisplayStim.RT), sem = (sd(DisplayStim.RT)/sqrt(length(Subject)))) #create summary by calculating clean mean reaction time and standard error of the mean using the clean standard deviation

#repeat the same steps for primary trials for remaining trials:

#secondary
branch_rt_secondary <- branch_data %>% #new tibble for this phase type
  filter(DisplayStim.ACC == 1, PhaseSwitch == "secondary") %>% #filter for correct trials and for the second phase switch or "secondary" phase type
  group_by(Subject, PhaseSwitch, ExternalStimulus, Task) %>%
  mutate(meanRT = mean(DisplayStim.RT), sd = sd(DisplayStim.RT), RT_Outliers = abs(DisplayStim.RT-meanRT)>2*sd) %>%
  filter(RT_Outliers == "FALSE") %>%
  summarize(meanRT_clean = mean(DisplayStim.RT), sem = (sd(DisplayStim.RT)/sqrt(length(Subject))))

#return
branch_rt_return <- branch_data %>% #new tibble for this phase type
  filter(DisplayStim.ACC == 1, PhaseSwitch == "return") %>% #filter for correct trials and for the third phase switch or "return" phase type
  group_by(Subject, PhaseSwitch, ExternalStimulus, Task) %>%
  mutate(meanRT = mean(DisplayStim.RT), sd = sd(DisplayStim.RT), RT_Outliers = abs(DisplayStim.RT-meanRT)>2*sd) %>%
  filter(RT_Outliers == "FALSE") %>%
  summarize(meanRT_clean = mean(DisplayStim.RT), sem = (sd(DisplayStim.RT)/sqrt(length(Subject))))

#average
branch_rt_average <- branch_data %>% #new tibble for this phase type
  filter(DisplayStim.ACC == 1, PhaseSwitch == "average") %>% #we still want correct trials, but now we want the remaining trials of each phase, not the first value for each phase, so we select the rows in which PhaseSwitch = "average"
  group_by(Subject, PhaseSwitch, ExternalStimulus, Task) %>%
  mutate(meanRT = mean(DisplayStim.RT), sd = sd(DisplayStim.RT), RT_Outliers = abs(DisplayStim.RT-meanRT)>2*sd) %>%
  filter(RT_Outliers == "FALSE") %>%
  summarize(meanRT_clean = mean(DisplayStim.RT), sem = (sd(DisplayStim.RT)/sqrt(length(Subject))))


#Analyzing error rate for each phase type
#error rate for primary trials
branch_er_primary <- branch_data %>% #need a new tibble where we can view the errors for this phase type
  filter(PhaseSwitch == "primary") %>% #look at only primary trials
  group_by(Subject, PhaseSwitch, ExternalStimulus, Task) %>% #group by these variables
  summarize(ErrorRate = 100 - mean(DisplayStim.ACC==1)*100) #calculate error rate by subtracting the mean accuracy from 100

#doing the same as above for the remaining phase types:
#error rate for secondary trials
branch_er_secondary <- branch_data %>%
  filter(PhaseSwitch == "secondary") %>% #look at only secondary trials
  group_by(Subject, PhaseSwitch, ExternalStimulus, Task) %>%
  summarize(ErrorRate = 100 - mean(DisplayStim.ACC==1)*100)

#error rate for return trials
branch_er_return <- branch_data %>%
  filter(PhaseSwitch == "return") %>% #look at only return trials
  group_by(Subject, PhaseSwitch, ExternalStimulus, Task) %>%
  summarize(ErrorRate = 100 - mean(DisplayStim.ACC==1)*100)

#error rate for average trials
branch_er_average <- branch_data %>%
  filter(PhaseSwitch == "average") %>% #look only at average trials
  group_by(Subject, PhaseSwitch, ExternalStimulus, Task) %>%
  summarize(ErrorRate = 100 - mean(DisplayStim.ACC==1)*100)

#Create a summary of task performance for reaction times and error rates-----------------------------
summary_er_branch <- bind_rows(branch_er_primary, branch_er_secondary, branch_er_return, branch_er_average)
summary_rt_branch <- bind_rows(branch_rt_primary, branch_rt_secondary, branch_rt_return, branch_rt_average)


#join the two summary tibbles to generate one general summary for the branch task
summary_branch <- left_join(summary_er_branch, summary_rt_branch)


#make bar graphs to show summaries of task performance-----------------------------------------------

#will make PhaseSwitch values factors to organize them in a desired order for the bar graph
summary_branch$PhaseSwitch <- factor(summary_branch$PhaseSwitch, levels = c('primary', 'secondary', 'return', 'average'), ordered = TRUE)

#Reaction Time
rt_bar_graph <- ggplot(summary_branch, aes(x = PhaseSwitch, y = meanRT_clean)) + 
  geom_bar(stat='identity', aes(colour = PhaseSwitch, fill = PhaseSwitch), show.legend = TRUE, position = position_dodge(width = 0.7)) + #have already calculated mean RT and sem, so stat = identity
  geom_errorbar(aes(ymin = meanRT_clean-sem, ymax = meanRT_clean+sem), width = 0.7) + #sem calculated earlier for each phase
  labs(title="Mean RT Branch", x="Phase Switch", y = "Reaction Time (ms)")
rt_bar_graph

ggsave('figures/summary_rt_branch.pdf',units='in',width=4,height=5)

#Error Rate
er_bar_graph <- ggplot(summary_branch, aes(x = PhaseSwitch, y = ErrorRate)) + 
  geom_bar(stat='identity', aes(colour = PhaseSwitch, fill = PhaseSwitch), show.legend = TRUE, position = position_dodge(width = 0.7)) + #no error bars for error rate because standard error is not important for analyses. the bar graph is meant to show the average error rates as a summary
  labs(title="Mean ER Branch", x="Phase Switch", y = "Error Rate (%)")
er_bar_graph

ggsave('figures/summary_er_branch.pdf',units='in',width=4,height=5)

#save summary task performance and ratings as separate csv files-------------------------------------
write.csv(summary_branch, "summary_data/summary_branch.csv")
write.csv(ratings, "summary_data/summary_ratings.csv")
