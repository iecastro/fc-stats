library(tidyverse)
source("R/00-setup.R")

meta_df <- FreeCompetitions() 

meta_df %>% glimpse()


liga21matches <- meta_df %>% 
  filter(competition_id == 11 &
           season_id == 90) %>% 
  FreeMatches()

# barca <- matches %>% 
#   filter(
#     (home_team.home_team_name == "Barcelona" & home_team.home_team_gender == "male") |
#       (away_team.away_team_name ==  "Barcelona" & away_team.away_team_gender == "male")
#   )
# 
# barca21 <- barca %>% filter(season.season_id == 90)
# 
# barca %>% distinct(season.season_name, season.season_id)


barca_events <- map_df(liga21matches$match_id, function(x){
  get.matchFree(liga21matches %>% filter(match_id == x)) %>% 
    as_tibble()
}
)
  
pedri <- barca_events %>% filter(player.name == "Pedro González López") %>% mutate(name_short = "Pedri")  
fati <- barca_events %>% filter(player.name == "Anssumane Fati") %>% mutate(name_short = "Fati")     
dest <- barca_events %>% filter(player.name == "Sergino Dest") %>% mutate(name_short = "Dest")  
messi <-  barca_events %>% filter(player.name == "Lionel Andrés Messi Cuccittini")  %>% mutate(name_short = "Messi")  

  