library(tidyverse)
library(ggsoccer)
library(patchwork)

source("R/00-setup.R")

meta_df <- FreeCompetitions() 

liga21matches <- meta_df %>% 
  filter(competition_id == 11 & #comp liga
           season_id == 90) %>% #season 20/21
  FreeMatches()

barca_events <- map_df(liga21matches$match_id, function(x){
  get.matchFree(liga21matches %>% filter(match_id == x)) %>% 
    as_tibble()
}
)

## pressure events, select players
press <- barca_events %>% 
  filter(type.name == "Pressure" &
           player.name %in% 
           c("Pedro González López",
             "Sergino Dest",
             "Lionel Andrés Messi Cuccittini")) %>% 
  mutate(name_short =
           case_when(player.name == "Pedro González López" ~ "Pedri",
                     player.name == "Lionel Andrés Messi Cuccittini" ~ "Messi",
                     player.name == "Sergino Dest" ~ "Dest")) %>% 
  cleanlocations()


## location plots

pr1 <- ggplot() +
  annotate_pitch(dimensions = pitch_statsbomb) +
  geom_point(data = press, #%>% 
             #filter(counterpress == TRUE), #variable removed from open data set
             aes(location.x, 80-location.y),
             alpha = .4, color = "purple", size = .7) +
  #scale_y_reverse() + # not used due to 80-y scaling
  facet_wrap(~name_short) +
  theme_pitch() +
  theme(legend.position = "top") +
  direction_label(y_label = -3.2, text_size = 1.5,
                  x_label = 60,
                  colour = "black")

pr2 <- ggplot() +
  annotate_pitch(dimensions = pitch_statsbomb) +
  geom_density2d_filled(data = press, 
                        aes(location.x, 80-location.y),
                        alpha = .7,
                        adjust = 4/5) + #bandwidth adj
  facet_wrap(~name_short) +
  theme_pitch() +
  labs(caption ="Data source: StatsBomb Open Data") +
  theme(legend.position = "none",
        plot.subtitle=element_text(hjust=0.5)) +
  scale_fill_viridis_d(option = "A", direction = -1) 

pr1 / pr2 +
  plot_annotation(title = "Barcelona, LaLiga 2020/21 ",
                  subtitle = "Data represent the pitch location of pressure events on\nopposing players recieving, carrying, or releasing the ball.")


ggsave("plots/pressure-events.png", plot = last_plot())
