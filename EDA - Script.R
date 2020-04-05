library(tidyverse)
library(scales)
library(gridExtra)
library(knitr)
library(dplyr)
library(ggExtra)

# set up plotting theme
theme_akeem <- function(legend_pos="top", base_size=11, font=NA){
  
  # come up with some default text details
  txt <- element_text(size = base_size+3, colour = "black", face = "plain")
  bold_txt <- element_text(size = base_size+3, colour = "black", face = "bold")
  
  # use the theme_minimal() theme as a baseline
  theme_minimal(base_size = base_size, base_family = font)+
    theme(text = txt,
          # axis title and text
          axis.title.x = element_text(size = 15, hjust = 1),
          axis.title.y = element_text(size = 15),
          # gridlines on plot
          panel.grid.major = element_line(linetype = 2),
          panel.grid.minor = element_line(linetype = 2),
          # title and subtitle text
          plot.title = element_text(size = 18, colour = "black", face = "bold"),
          plot.subtitle = element_text(size = 16, colour = "black"),
          
          ###### clean up!
          legend.key = element_blank(),
          # the strip.* arguments are for faceted plots
          strip.background = element_blank(),
          strip.text = element_text(face = "bold", size = 13, colour = "grey35")) +
    
    #----- AXIS -----#
    theme(
      #### remove Tick marks
      axis.ticks=element_blank(),
      
      ### legend depends on argument in function and no title
      legend.position = legend_pos,
      legend.title = element_blank(),
      legend.background = element_rect(fill = NULL, size = 0.5,linetype = 2)
      
      
    )
}


plot_cols <- c("#498972", "#3E8193", "#BC6E2E", "#A09D3C", "#E06E77", "#7589BC", "#A57BAF", "#4D4D4D") 


#Load the data

reg_season_stats <- as.tibble(read.csv("march-madness-analytics-2020/2020DataFiles/2020-Womens-Data/WDataFiles_Stage1/WRegularSeasonDetailedResults.csv",
                                       stringsAsFactors = F))
glimpse(reg_season_stats) #quick view

tourney_stats <- as.tibble(read.csv("march-madness-analytics-2020/2020DataFiles/2020-Womens-Data/WDataFiles_Stage1/WNCAATourneyDetailedResults.csv",
                          stringsAsFactors = F))
teams <- as.tibble(read.csv("march-madness-analytics-2020/2020DataFiles/2020-Womens-Data/WDataFiles_Stage1/WTeams.csv", stringsAsFactors = F))

tourney_stats_compact <- as.tibble(read.csv("march-madness-analytics-2020/2020DataFiles/2020-Womens-Data/WDataFiles_Stage1/WNCAATourneyCompactResults.csv",
                                            stringsAsFactors = F))
tourney_seeds <- as.tibble(read.csv("march-madness-analytics-2020/2020DataFiles/2020-Womens-Data/WDataFiles_Stage1/WNCAATourneySeeds.csv",
                          stringsAsFactors = F))
team_conferences <- as.tibble(read.csv("march-madness-analytics-2020/2020DataFiles/2020-Womens-Data/WDataFiles_Stage1/WTeamConferences.csv",
                                       stringsAsFactors = F))
conferences <- as.tibble(read.csv("march-madness-analytics-2020/2020DataFiles/2020-Womens-Data/WDataFiles_Stage1/Conferences.csv",
                                  stringsAsFactors = F))
players <- read.csv("march-madness-analytics-2020/2020DataFiles/2020-Womens-Data/WPlayers.csv",
                     stringsAsFactors = F, na.strings = c("", "NA")) %>% 
  filter(!is.na(LastName)) %>%
  as.tibble()
  
glimpse(players)


# Play by Play Analysis

play_by_play <- data.frame()

# loop through each seasons PlayByPlay folders and read in in the play by play files
for(each in list.files("march-madness-analytics-2020/2020DataFiles/2020-Womens-Data/")[str_detect(list.files("march-madness-analytics-2020/2020DataFiles/2020-Womens-Data/"), "WEvents")]) {
  df <- read_csv(paste0("march-madness-analytics-2020/2020DataFiles/2020-Womens-Data/", each)) #detect similar events
  
  
  # Grouped shooting variables ----------------------------------------------
  # there are some shooting variables that can probably be condensed - tip ins and dunks
  paint_attempts_made <- c("made2_dunk", "made2_lay", "made2_tip") 
  paint_attempts_missed <- c("miss2_dunk", "miss2_lay", "miss2_tip") 
  paint_attempts <- c(paint_attempts_made, paint_attempts_missed)
  
  # create variables for field goals made (FGM), and also field goals attempted (FGA)(which includes the sum of FGs made and FGs missed)
  FGM <- c("made2_dunk", "made2_jump", "made2_lay",  "made2_tip",  "made3_jump")
  FGA <- c(FGM, "miss2_dunk", "miss2_jump" ,"miss2_lay",  "miss2_tip",  "miss3_jump")
  # variable for three-pointers
  ThreePointer <- c("made3_jump", "miss3_jump")
  #  Two point jumper
  TwoPointJump <- c("miss2_jump", "made2_jump")
  # Free Throws
  FT <- c("miss1_free", "made1_free")
  # all shots
  AllShots <- c(FGA, FT)
  
  
  # Feature Engineering -----------------------------------------------------
  # paste the two even variables together for FGs as this is the format for last years comp data
  df <- df %>%
    mutate_if(is.factor, as.character) %>% 
    mutate(EventType = ifelse(str_detect(EventType, "miss") | str_detect(EventType, "made") | str_detect(EventType, "reb"), paste0(EventType, "_", EventSubType), EventType))
  
  # change the unknown for 3s to "jump" and for FTs "free"
  df <- df %>% 
    mutate(EventType = ifelse(str_detect(EventType, "3"), str_replace(EventType, "_unk", "_jump"), EventType),
           EventType = ifelse(str_detect(EventType, "1"), str_replace(EventType, "_unk", "_free"), EventType))
  
  
  df <- df %>% 
    # create a variable in the df for whether the attempts was made or missed
    mutate(shot_outcome = ifelse(grepl("made", EventType), "Made", ifelse(grepl("miss", EventType), "Missed", NA))) %>%
    # identify if the action was a field goal, then group it into the attempt types set earlier
    mutate(FGVariable = ifelse(EventType %in% FGA, "Yes", "No"),
           AttemptType = ifelse(EventType %in% paint_attempts, "PaintPoints", 
                                ifelse(EventType %in% ThreePointer, "ThreePointJumper", 
                                       ifelse(EventType %in% TwoPointJump, "TwoPointJumper", 
                                              ifelse(EventType %in% FT, "FreeThrow", "NoAttempt")))))
  
  
  # Rework DF so only shots are included and whatever lead to the shot --------
  df <- df %>% 
    mutate(GameID = paste(Season, DayNum, WTeamID, LTeamID, sep = "_")) %>% 
    group_by(GameID, ElapsedSeconds) %>% 
    mutate(EventType2 = lead(EventType),
           EventPlayerID2 = lead(EventPlayerID)) %>% ungroup()
  
  
  df <- df %>% 
    mutate(FGVariableAny = ifelse(EventType %in% FGA | EventType2 %in% FGA, "Yes", "No")) %>% 
    filter(FGVariableAny == "Yes") 
  
  
  # create a variable for if the shot was made, but then the second event was also a made shot
  df <- df %>% 
    mutate(Alert = ifelse(EventType %in% FGM & EventType2 %in% FGM, "Alert", "OK")) %>% 
    # only keep "OK" observations
    filter(Alert == "OK") 
  # replace NAs with something
  df$EventType2[is.na(df$EventType2)] <- "no_second_event"
  
  
  # create a variable for if there was an assist on the FGM:
  df <- df %>% 
    mutate(AssistedFGM = ifelse(EventType %in% FGM & EventType2 == "assist", "Assisted", 
                                ifelse(EventType %in% FGM & EventType2 != "assist", "Solo", 
                                       ifelse(EventType %in% FGM & EventType2 == "no_second_event", "Solo", "None"))))
  
  # # because the FGA could be either in `EventType` (more likely) or `EventType2` (less likely), need
  # # one variable to indicate the shot type
  # df <- df %>% \
  #   mutate(fg_type = ifelse(EventType %in% FGA, EventType, ifelse(EventType2 %in% FGA, EventType2, "Unknown")))
  
  # create final output
  df <- df %>% ungroup()
  play_by_play <- bind_rows(play_by_play, df)
  
  rm(df);gc() #remove garbage content
}

#saveRDS(play_by_play, "play_by_play15_19.rds")
# play_by_play <- readRDS("play_by_play2015_19.rds")


play_by_play %>%
  filter(Season == 2019) %>%
  as.tibble()    #2019 data

library(DT)
#PlaybyPlay seasons range from 2015 to 2019
summary(play_by_play$Season)

#Sum of field goals made and missed from 2015 to 2019
play_by_play %>%
  group_by(shot_outcome) %>% 
  summarise(number_of_shots = n()) %>%
  arrange(desc(number_of_shots)) 


# Sum of field goals made (FGM) in 2019
filter(play_by_play, play_by_play$Season == 2019) %>%
  group_by(shot_outcome) %>% 
  summarise(number_of_shots = n()) %>%
  arrange(desc(number_of_shots)) 

## Morey Ball in the NCAA

play_by_play %>%
  group_by(AttemptType) %>% 
  summarise(number_of_shots = n()) %>%
  arrange(desc(number_of_shots)) %>%
  DT::datatable()         # Shots in descending order


# Scoring patterns
play_by_play %>% 
  group_by(AttemptType) %>% 
  summarise(number_of_shots = n()) %>%
  filter(AttemptType != "NoAttempt") %>% 
  ggplot(aes(x= AttemptType, y= number_of_shots)) +
  geom_col(fill = plot_cols[1], colour = "grey") +
  geom_text(aes(label = comma(number_of_shots)), y=90000, colour = "black", size = 6) +
  scale_y_continuous(labels = c("0", "300,000", "600,000", "900,000", "1,200,000\nattempts")) +
  coord_flip() +
  ggtitle("Scoring Patterns in The Female NCAA", subtitle = ">Three Pointers are closing up on the Two Pointers<") +
  theme_akeem() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))  



## Scoring pattern trend over the years
p_tab <- play_by_play %>% 
  filter(AttemptType != "FreeThrow") %>% 
  group_by(AttemptType, Season) %>% 
  summarise(n_shots = n()) %>% 
  filter(AttemptType != "NoAttempt") %>%
  filter(Season == 2019) %>% ungroup()


play_by_play %>% 
  filter(AttemptType != "FreeThrow") %>% 
  group_by(AttemptType, Season) %>% 
  summarise(n_shots = n()) %>% 
  filter(AttemptType != "NoAttempt") %>% 
  ggplot(aes(x= Season, y= n_shots, colour = AttemptType, group = AttemptType)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_text(data = p_tab, aes(label = AttemptType), hjust= 0, size=6) +
  scale_color_manual(values = plot_cols) +
  scale_x_continuous(labels = c(2015:2020), breaks = c(2015:2020), limits = c(2015, 2022)) +
  scale_y_continuous(labels = c("125,000", "150,000", "175,000", "200,000", "225,000", "250,000 attempts"), breaks = c(seq(from=125000, to= 250000, by= 25000)), limits = c(125000, 250000)) +
  ggtitle("3_points Jumper Popularity Rises While 2_Points Jumper Diminishes") +
  theme_akeem(legend_pos = "none") +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank())

# Three points jumper rose over the years
#install.packages("DT") - Javascript in R

play_by_play %>% 
  filter(FGVariable == "Yes") %>%
  group_by(AttemptType, shot_outcome) %>% 
  summarise(n_attempts = n()) %>% 
  mutate(shot_success = percent(n_attempts / sum(n_attempts))) %>% 
  filter(shot_outcome == "Made") %>% select(-shot_outcome) %>% 
  DT::datatable()

#Graphical illustration of shot_success_percentage by attempt type

play_by_play %>% 
  filter(FGVariable == "Yes") %>%
  group_by(AttemptType, shot_outcome) %>% 
  summarise(n_attempts = n()) %>% 
  mutate(shot_success = percent(n_attempts / sum(n_attempts))) %>% 
  filter(shot_outcome == "Made") %>% 
  ggplot(aes(x = AttemptType , y = shot_success))+
  geom_bar(stat = "identity")+  #identity was the winning strategy, lol
  coord_flip() +
  theme_akeem(legend_pos = "none") +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank())





play_by_play %>% 
  filter(FGVariable == "Yes") %>%
  group_by(AttemptType, shot_outcome) %>% 
  summarise(n_attempts = n()) %>% 
  mutate(shot_failure = percent(n_attempts / sum(n_attempts))) %>% 
  filter(shot_outcome == "Missed") %>% select(-shot_outcome) %>% #shot outcome is redundant in the dataset
  DT::datatable()

#Graphical illustration of shot_failure_percentage by attempt type

play_by_play %>% 
  filter(FGVariable == "Yes") %>%
  group_by(AttemptType, shot_outcome) %>% 
  summarise(n_attempts = n()) %>% 
  mutate(shot_failure = percent(n_attempts / sum(n_attempts))) %>% 
  filter(shot_outcome == "Missed") %>% 
  ggplot(aes(x = AttemptType , y = shot_failure))+
  geom_bar(stat = "identity")+  #identity was the winning strategy, lol
  coord_flip()


# Overall, the FG % was 39.56 on 3,078,936 attempts...
# Suprisingly, tries on threes were only 1.3% less successful than 2-point jumpers, yet the potential payoff is 50% more points 
# for taking the three. That’s what the stat heads keep banging on about as the reason for the shift to threes and shots at the rim.
  


## Expected points
## Expected points
glimpse(play_by_play)

shot_values <- play_by_play %>% 
  filter(FGVariable == "Yes") %>% 
  mutate(EventType = str_remove(EventType, "made"), EventType = str_remove(EventType, "miss")) %>% 
  group_by(EventType, shot_outcome) %>% 
  summarise(n_attempts = n()) %>% 
  mutate(shot_success = n_attempts / sum(n_attempts),
         n_attempts = sum(n_attempts)) %>% 
  filter(shot_outcome == "Made") %>% select(-shot_outcome) 

shot_values %>% 
  mutate(n_attempts = comma(n_attempts),
         shot_success = percent(shot_success)) %>% 
  rename('No_of_Attempts' = n_attempts)%>%
  DT::datatable()

# The data suggested that the ladies sparingly made an attempt at a 2-pointer dunk. The table suggested that a 2_tip is by far the
# most successful strategy. It’s pretty obvious to see why basketball analytics junkies are screaming from the rooftops for players 
# to stop shooting jumpers from inside the three point line - these shot types have an expected 0.67 (.334 x 2) points per attempt… 
# considerably less than the expected 0.95 (.315 x 3) points for attempted shots outside the three-points line.

shot_values %>% 
  mutate(Point = as.numeric(str_extract(EventType, "[[:digit:]]"))) %>% 
  mutate(ExpectedPoints = shot_success * Point) %>% 
  ggplot(aes(x=reorder(EventType, ExpectedPoints), y= ExpectedPoints)) +
  geom_col(fill = plot_cols[1], colour = "grey") +
  geom_text(aes(label = round(ExpectedPoints, 2)), vjust =  1.2, colour = "white", size=6) +
  ggtitle("TWO POINT JUMPERS THE LEAST EFFICIENT SHOT TYPE", subtitle = "Tips yielded a massive 1.23 points per attempt,\nwhile 2-point jumpers yielded approx. half")+
  theme_akeem() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank())


## Assists Vs Solo Attempts

play_by_play %>% 
  filter(AssistedFGM != "None", shot_outcome == "Made") %>% 
  group_by(EventType, AssistedFGM) %>% 
  summarise(n = n()) %>% 
  mutate(perc = n / sum(n)) %>% 
  filter(AssistedFGM == "Solo") %>% 
  ggplot(aes(x=EventType, y= perc, fill = AssistedFGM)) +
  geom_hline(yintercept = mean(play_by_play$AssistedFGM[play_by_play$AssistedFGM != "None"] == "Solo"), linetype = 2) +
  geom_col(fill = plot_cols[2], colour = "grey") +
  geom_text(aes(label = ifelse(AssistedFGM == "Solo", percent(perc), "")), hjust = 1.2, colour = "white", size=6) +
  scale_y_continuous(labels = c("0%", "25%", "50%", "75%", "100% solo")) +
  annotate("text", x=5, y= 0.56, label = paste0(percent(mean(play_by_play$AssistedFGM[play_by_play$AssistedFGM != "None"] == "Solo")), " solo\nattempts overall"), size = 6) +
  coord_flip() +
  ggtitle("HERO BALL MORE FREQUENT FOR SOME SHOTS", subtitle = "Solo 2pt jump shots are much more frequent that 3pt jumpers") +
  theme_akeem(legend_pos = "none") +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())


# Complete code chunk in the RMD file!

