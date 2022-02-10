# Loading in the packages
library(tidyverse)
library(scales)
library(sysfonts)
library(showtext)
library(ggimage)

#  Reading in the data
data <- read.csv('https://raw.githubusercontent.com/ajstarks/dubois-data-portraits/master/challenge/2022/challenge01/tam.csv')

# Adding fonts
font_add_google("Rubik")
font_add_google("Almendra Display")
showtext_auto()

# Creating an Illinois title dataframe
illinois_pilots <- data %>%
  filter(State == "IL") %>%
  count(Pilot.type, sort = TRUE) %>%
  mutate(category = "Illinois",
         percent = (round(n / sum(n), 2)),
         cols = as.factor("white"))

# Creating a National title dataframe
national_pilots <- data %>%
  count(Pilot.type, sort = TRUE) %>%
  mutate(category = "US",
         percent = (round(n / sum(n), 2)), 
         cols = as.factor("black"))

# Binding together
pilots <- bind_rows(illinois_pilots, national_pilots)

# Creating the visual
g1 <- pilots %>%
  ggplot(aes(x = reorder(str_to_upper(Pilot.type), percent), y = percent, fill = str_to_upper(category))) + 
  geom_col(position = "dodge", width = .35, color = "black", size = .1) + 
  geom_text(aes(label = percent(percent), color = category), position = position_dodge(.35), 
            hjust = -.5, family = "Rubij", size = 3) +
  scale_fill_manual(values = c("#FFD700",
                               "#000000")) + 
  scale_color_manual(values = c("black", "black"), guide = "none") +
  scale_y_continuous(labels = percent, limits = c(-.041, .75)) +
  labs(title = "PILOT TITLES FOR ILLINOISANS VS THE NATIONAL AVERAGE", x = "", y = "") + 
  coord_flip() + 
  annotate(geom = "text", label = "{", x = 1.07, y = -.01, size = 15, family = "Almendra Display", color = "#474747") +
  annotate(geom = "text", label = "{", x = 2.07, y = -.01, size = 15, family = "Almendra Display", color = "#474747") +
  annotate(geom = "text", label = "{", x = 3.07, y = -.01, size = 15, family = "Almendra Display", color = "#474747") +
  annotate(geom = "text", label = "{", x = 4.07, y = -.01, size = 15, family = "Almendra Display", color = "#474747") +
  annotate(geom = "text", label = "{", x = 5.07, y = -.01, size = 15, family = "Almendra Display", color = "#474747") + 
  theme_minimal() +
  theme(plot.title = element_text(family = "Rubik", "bold", hjust = .3, size = 18, margin = margin(15,0,0,0)),
        plot.title.position = "panel",
        legend.position = "top", 
        legend.justification = "center",
        legend.title = element_blank(),
        panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(family = "Rubik", size = 11., hjust = .5))

ggbackground(g1, background = 'Paper Background.jpeg')


