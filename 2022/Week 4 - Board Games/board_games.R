library(tidyverse)
library(viridis)
library(firatheme)
library(hrbrthemes)
library(showtext)
library(camcorder)

gg_record(dir = "temp", device = "png", width = 10, height = 10, units = "in", dpi = 320)

showtext_auto()

font_add_google(family = "patua-one", "Patua One")
font_add_google(family = "montserrat", "Montserrat")
font_add_google(family = "roboto", "Roboto")

tuesdata <- tidytuesdayR::tt_load("2022-01-25")



ratings <- tuesdata$ratings

details <- tuesdata$details


full_games <- ratings %>% 
        left_join(details, by = "id") %>% 
        select(id, name, yearpublished, average, minplayers,maxplayers,minage,playingtime) %>% 
        filter(yearpublished >= 1980) %>%
        mutate(play_time = playingtime/60) %>%
        filter(yearpublished  < 2021) %>%
        #filter(play_time <= 10) %>%
        filter(minage > 2)


arrows <- tibble(x1 = c(2000),
                 y1 = c(23),
                 x2 = c(2007),
                 y2 = c(25))


c <- full_games %>% 
        group_by(minage) %>%
        ggplot(aes(x = yearpublished, y = minage, fill = average))+
        geom_tile(color = "white", size = 0.5)+
        #coord_equal()+
        scale_fill_viridis(option = "rocket",breaks = c(0,3,4.5,6,7.5,9))+
        theme_fira()+
        scale_x_continuous(breaks = seq(1950,2021,5), expand = c(0,0))+
        scale_y_continuous(breaks = seq(3,25,2), expand = c(0,0))+
        labs(y = "Minimum Recommended Age",
             fill = "Average",
             x = "Year",
             title = "Average Rating for Board Games by Minimum Age",
             subtitle = "For each year since 1980 to 2020",
             caption = "Source: BoardGameGeek | Chart: Miguel HG (@mike_dvz)")+
        theme(plot.background = element_rect(fill = "#282828", color = NA),
              axis.text = element_text(color = "white", family = "montserrat", size = 11),
              legend.text = element_text(color = "white", family = "montserrat",face = "bold"),
              axis.title.y = element_text(color = "white", family = "montserrat", size = 13, face = "bold"),
              legend.title = element_text(color = "white", family = "montserrat", size = 11, face = "bold"),
              axis.title.x = element_text(color = "white", family = "montserrat", size = 13, face = "bold"),
              plot.title = element_text(color = "white", family = "roboto", size = 24, face = "bold"),
              plot.subtitle = element_text(color = "white", family = "roboto", size = 17),
              plot.caption = element_text(color = "white", family = "roboto", size = 11),
              legend.position = "bottom")+
        theme(legend.key.size = unit(1.1, 'cm'))+
        annotate("text", x = 1996, y = 23, family = "roboto", size = 3.7, color = "white",
                 label = "Yes, there is a game with
        25 years old minimun age")+
        geom_curve(
                data = arrows, aes(x = x1, y = y1,xend = x2, yend = y2),
                arrow = arrow(length = unit(0.07, "inch")), size = 0.9,
                color = "white", curvature = -0.2,
                inherit.aes = FALSE)

c

        
ggsave("bord_games.png", plot = last_plot(),
       width =1080/72, height = 1080/72, dpi = 72)




