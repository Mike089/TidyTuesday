library(tidyverse)
library(scales)
library(hrbrthemes)
library(showtext)
library(firatheme)
library(ggthemes)
library(camcorder)

gg_record(dir = "temp", device = "png", width = 13, height = 10, units = "in", dpi = 320)



showtext_auto()

font_add_google(family = "patua-one", "Patua One")
font_add_google(family = "montserrat", "Montserrat")


colors <- c("#49270c", "#502b0d", "#582f0e", "#623714", "#6c3f19", "#71431c", "#76471f", "#7f4f24", "#936639")



chocolate <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-18/chocolate.csv')


target_latin <- c("Mexico","Ecuador","Peru","Nicaragua","Venezuela","Colombia","Costa Rica","Argentina",
                  "Guatemala","Bolivia","Honduras","Chile","El Salvador")


chocolate %>%
        glimpse()


latin_choc <- chocolate %>%
        filter(company_location %in% target_latin) %>%
        select(company_location, bean_origin = country_of_bean_origin, rating, cocoa_percent) %>%
        mutate(cocoa_percent = parse_number(cocoa_percent)) %>%
        add_count(company_location)%>%
        filter(n >= 10)


choc_sorted <- 
        latin_choc %>%
        mutate(bean_origin = fct_reorder(bean_origin,-rating))


choc_sorted$rating <- as.numeric(choc_sorted$rating)


arrows <- tibble(x1 = c("Mexico","Nicaragua"),
                 y1 = c(0.85,0.55),
                 x2 = c("Guatemala","Nicaragua"),
                 y2 = c(0.716,0.716))

world_avg <-
        chocolate %>%
        mutate(cocoa_percent = parse_number(cocoa_percent)) %>%
        summarize(avg = mean(as.numeric(cocoa_percent), na.rm = TRUE)) %>%
        pull(avg)



c <- choc_sorted %>%
        group_by(bean_origin) %>%
        ggplot(aes(x = company_location, y = cocoa_percent/100, color = company_location))+
        geom_hline(aes(yintercept = world_avg/100), color = "grey60", size = 0.9, linetype = "dotted")+
        stat_summary(fun = mean, geom = "point", size = 7)+
        geom_jitter(size = 2, alpha = 0.4, width = 0.2)+
        theme_gdocs()+
        scale_color_manual(values = colors)+
        coord_flip()+
        scale_y_continuous(labels = percent, breaks = seq(0.40,1,.15))+
        labs(title = "How much cocoa have the chocolates produced in Latin America?",
             subtitle = "Each dot represents one company, showing countries with 10 or more ratings in Flavors Of Cacao",
             y = "Cocoa Percent",
             x = "",
             caption = "Source: FlavorsOfCacao.com | Visualization: Miguel HG (@mike_dvz) | TidyTuesday Week 03")+
        theme(plot.title = element_text(family = "patua-one", face = "bold", size = 26, color ="#49270c"),
              plot.subtitle = element_text(family = "patua-one",size= 13, color = "#49270c"),
              plot.background = element_rect(fill = "#EDE2D1"),
              plot.caption = element_text(family = "patua-one",size= 12, color = "#49270c"),
              axis.text = element_text(family = "montserrat", face = "bold", color = "#49270c", size = 12),
              axis.title = element_text(size = 20, hjust = 0.5, face = "bold", family = "patua-one",color = "#49270c"))+
        annotate("text", x = "Nicaragua", y = 0.5022, family = "montserrat", face = "bold", size = 4, color = "#49270c",
                 label = "Country Average")+
        annotate(
                "text", x = "Mexico", y = 0.90, family = "montserrat", size = 4, color = "#49270c", lineheight = .9,
                label = glue::glue("Worldwide average:\n{round(world_avg, 1)} % of cocoa"))+
        guides(color = FALSE)        


c_arrows <- 
        c +
        geom_curve(
                data = arrows, aes(x = x1, y = y1,xend = x2, yend = y2),
                arrow = arrow(length = unit(0.08, "inch")), size = 0.9,
                color = "#49270c", curvature = -0.2)
c_arrows
        