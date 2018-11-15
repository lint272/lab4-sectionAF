# Exercise 7: DPLYR practice with NBA data
#install.packages("dplyr")
library(dplyr)

# Read in the NBA team data of the 2016-2017 season from the data directory  
# into a variable called `team.data` using `read.csv`
team.data <- read.csv("data/teams.csv", stringsAsFactors = FALSE)


# The data.frame team.data should now be accessible to you. 
# View it, and get some basic information about the number of rows/columns. 
# Note the "X" preceding some of the column titles as well as the "*" 
# following the names of teams that made it to the playoffs that year.
View(team.data)


# Add a column that gives the turnovers to steals ratio (TOV / STL) for each team
team.data <- mutate(team.data, TOV_STL = TOV / STL)

# Sort the teams from lowest turnover/steal ratio to highest
sorted_team <- arrange(team.data, TOV_STL)

#Find the average BLK and STL for teams having a TOV greater than the average TOV of all teams
greater <- filter(team.data, TOV > mean(TOV))
avg1 <- summarise(greater, mean(BLK), mean(STL))
# avg2 <-  summarise(filter(team.data, TOV > mean(TOV)), mean(BLK), mean(STL))
avg2 <- summarise(
  filter(
    team.data, TOV > mean(TOV)
  ),
  mean(BLK),
  mean(STL)
)

# Get the team that had the highest Total Rebounds (TRB) only with the columns 
# Team and TRB  *using one line of code*
# select(filter(team.data, TRB == max(TRB)), Team, TRB)

team.data %>% 
  filter(TRB == max(TRB)) %>% 
  select(Team, TRB) #using pipe PREFERED WAYYYY

# Print only the name of the team that had the highest total rebounds
# (that also happens to be the greatest team of all time)
# select(select(filter(team.data, TRB == max(TRB)), Team, TRB), Team)
# dplyr way, use "pull" to get a column
filter(team.data, TRB == max(TRB)) %>%
  pull(Team)

# dplyr with base R way, use `$` to get a column
filter(team.data, TRB == max(TRB)) %>%
  .$Team


## Let's change gears!

# Read in the Pokemon data from the data directory  
# into a variable called `pokemon` using `read.csv`. Remember to not read strings in as factors.
pokemon <- read.csv("data/Pokemon.csv")
# First, View() the data set to see what info you have to work with 
View(pokemon)

# Find all the Pokemon that are "Water" or "Ghost" Type 1 and have a speed higher than 50
pokemon %>% 
filter((Type.1 == "Water" | Type.1 == "Ghost") & Speed > 50) %>% 
View()

# Find the average HP, median HP, min HP and max HP for each of the type of generations
group_by(pokemon, Generation) %>% 
  summarise(avg = mean(HP), median = median(HP), min = min(HP), max = max(HP)) %>% 
  View()

#FIND THE NUMBER OF POKEMONS THAT ARE LEGENDARY AND THE NUMBER OF POKEMONS THAT ARE NOT LEGENDARY 
#THAT HAVE A HIGHER ATTACK THAN DEFENSE VALUE
#NOTE: IT SHOULD BE DONE IN ONE RUN OF DPLYR
#HINT : Look into count() function by dplyr
pokemon%>%
  mutate(higher = Attack > Defense) %>% 
  filter(higher == TRUE) %>% 
  View()

#Find the generation that has the most number of pokemons with Type.1 as "Fire"
filter(pokemon, Type.1 == "Fire") %>% 
  count(Generation) %>% 
  filter(n == max(n)) %>% 
  select(Generation)

# WHICH Type 1 pokemon has the overall within group max value of SUM of HP, Attack, Defense, Sp..Att, Sp.Def, Speed
#HINT : LOOK AT THE DATASET! - THERE IS A SUPER EASY WAY TO DO THIS.
pokemon %>% 
  group_by(Type.1) %>% 
  summarise(sum = sum(Total)) %>% 
  filter(sum == max(sum)) %>% 
  select(Type.1)
