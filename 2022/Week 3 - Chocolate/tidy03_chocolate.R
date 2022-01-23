library(tidyverse)
library(scales)
library(ghibli)
library(hrbrthemes)
library(showtext)
library(firatheme)


showtext_auto()


chocolate <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-18/chocolate.csv')


target_latin <- c("Mexico","Ecuador","Peru","Nicaragua","Venezuela","Colombia","Costa Rica","Argentina",
        "Guatemala","Bolivia","Honduras","Chile","El Salvador")


choc_latin <- chocolate %>% 
        filter(country_of_bean_origin %in% target_latin) %>%
        select(bean_origin = country_of_bean_origin, cocoa_percent, rating, review_date) %>%
        mutate(cocoa_percent = parse_number(cocoa_percent)) %>% 
        filter(bean_origin != "Blend") %>% 
        add_count(bean_origin) %>% 
        filter(n >= 50) %>% 
        count(bean_origin, rating, cocoa_percent)


choc_sorted <- 
        choc_latin %>%
        mutate(bean_origin = fct_reorder(bean_origin,-rating))


choc_sorted$rating <- as.numeric(choc_sorted$rating)


font_add_google(family = "patua-one", "Patua One")
font_add_google(family = "montserrat", "Montserrat")


colors <- c("#49270c", "#502b0d", "#582f0e", "#623714", "#6c3f19", "#71431c", "#76471f", "#7f4f24", "#936639")


arrows <- tibble(x1 = "Mexico",
                 y1 = 0.85,
                 x2 = "Ecuador",
                 y2 = 0.76)

c <- choc_sorted %>%
        group_by(bean_origin) %>%
        ggplot(aes(x = bean_origin, y = cocoa_percent/100, color = bean_origin))+
        stat_summary(fun = mean, geom = "point", size = 7)+
        geom_jitter(size = 2, alpha = 0.4, width = 0.2)+
        theme_fira()+
        scale_color_manual(values = colors)+
        coord_flip()+
        scale_y_continuous(labels = percent)+
        labs(title = "How much cocoa have the chocolates produced in Latin America?",
             subtitle = "Countries with more than 50 ratings in Flavors of Cacao",
             y = "Cocoa Percent",
             x = "",
             caption = "Source: FlavorsOfCacao.com | Visualization: Miguel HG (@mike_dvz) | Tidytuesday Week 03")+
        theme(plot.title = element_text(family = "patua-one", face = "bold", size = 24, color ="#49270c"),
              plot.subtitle = element_text(family = "patua-one",size= 15, color = "#49270c"),
              plot.background = element_rect(fill = "#EDE2D1"),
              plot.caption = element_text(family = "patua-one",size= 12, color = "#49270c"),
              axis.text = element_text(family = "montserrat", face = "bold", color = "#49270c", size = 15),
              axis.title = element_text(size = 16, hjust = 0.5, face = "bold", family = "patua-one",color = "#49270c"))+
        annotate("text", x = "Mexico", y = 0.86, family = "montserrat", size = 6, color = "#49270c",
                label = "Country Average", face = "bold")+
        guides(color = FALSE)
        


c_arrows <- 
        c +
        geom_curve(
                data = arrows, aes(x = x1, y = y1,xend = x2, yend = y2),
                arrow = arrow(length = unit(0.08, "inch")), size = 0.9,
                color = "#49270c", curvature = -0.3)


c_arrows
          


ggsave("choc.jpg",plot = last_plot(), height = 40, width = 40, units = "cm", dpi = 75)      
