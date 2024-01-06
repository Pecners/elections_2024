library(tidyverse)
library(sf)
library(MetBrewer)
library(colorspace)
library(showtext)
library(ggtext)
library(glue)

mke_alds <- st_read("data/alderboundaries2024/AlderBoundaries2024.shp")

unopposed <- c(1, 2, 9, 12:14)

uno_alds <- mke_alds |> 
  filter(DISTRICT %in% unopposed)


c_pal <- met.brewer("VanGogh3")
swatchplot(c_pal)

font_add_google("Vollkorn", "v")
showtext_auto()
showtext_opts(dpi = "400")

mke_alds |> 
  mutate(opposed = ifelse(!DISTRICT %in% unopposed, TRUE, FALSE),
         DISTRICT = ifelse(DISTRICT == 9, "9*", DISTRICT),
         vjust = case_when(
           DISTRICT == 5 ~ 0,
           DISTRICT == 10 ~ 1,
           DISTRICT == 15 ~ -.5,
           DISTRICT == 6 ~ 0,
           DISTRICT == 4 ~ 1,
           DISTRICT == 11 ~ 1
         )) |> 
  ggplot(aes(fill = opposed,
             label = DISTRICT,
             vjust = vjust)) +
  geom_sf(color = "white") +
  geom_sf_text(aes(color = ifelse(opposed, alpha("white", .75), "white"),
                   fontface = ifelse(opposed, "plain", "bold"))) +
  scale_fill_manual(values = c(c_pal[4], alpha(c_pal[2], .5))) + 
  scale_color_identity() +
  theme_void() +
  theme(legend.position = "none",
        text = element_text(family = "v"),
        plot.subtitle = element_textbox(width = unit(5, "in"),
                                        hjust = .8,
                                        color = "grey30", lineheight = 1.5),
        plot.title = element_textbox(face = "bold",
                                     width = unit(5, "in"),
                                     hjust = .8,
                                     margin = margin(t = 10, b = 10)),
        plot.caption = element_textbox(width = unit(5, "in"), 
                                       hjust = .8,
                                       color = "grey60", lineheight = 1.75,
                                       size = 6,
                                       margin = margin(b = 10))) +
  labs(title = "Running Unopposed",
       subtitle = glue("Voters in ",
                       "<span style='color:{c_pal[4]}'>**six aldermanic districts**</span> ",
                       "on the far north and south sides of the city ",
                       "will only have one option for alderperson when they vote this spring."),
       caption = glue("*Two individuals submitted signatures to appear on the ballot ",
                      "but as of 8 pm on Jan. 3, one was not recommended for placement ",
                      "on the ballot. ",
                      "Data from City of Milwaukee Election Commission, ",
                      "graphic by Spencer Schien (@MrPecners)."))


ggsave("plots/main.png", bg = "white", width = 7, h = 9)

