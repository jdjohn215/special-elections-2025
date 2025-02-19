rm(list = ls())

library(tidyverse)
library(sf)
library(patchwork)

# This script compares the outcome of 2025 legislative special elections with
#   the outcome of the 2024 presidential election in that district

################################################################################
# spreadsheet of special election results
specials.orig <- read_csv("specials.csv", col_types = "cccccnnn") |>
  mutate(district = str_pad(district, width = 3, side = "left", pad = "0"),
         date = as.Date(date, format = "%m/%d/%y"))

################################################################################
# 2024 presidential election results in each state legislative district
district.votes.pres2024 <- read_csv("precincts-to-districts/district-votes-pres2024.csv")

################################################################################
# merge them together, calculate the margins and turnout change
specials.comparison <- specials.orig |>
  left_join(district.votes.pres2024) |>
  mutate(special_margin = (dem/total - rep/total)*100,
         pres_margin = (harris/total_2024 - trump/total_2024)*100,
         turnout_change = (total - total_2024)/total_2024*100,
         margin_change = special_margin - pres_margin)

################################################################################
# save output
write_csv(specials.comparison, "specials-comparison.csv")

################################################################################
# create graphics
#   remove races that haven't been recorded yet
specials.valid <- specials.comparison |>
  filter(!is.na(total))

margin.shift.over.time <- specials.valid |>
  ggplot(aes(date, margin_change)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(aes(shape = house)) +
  ggrepel::geom_text_repel(aes(label = label), min.segment.length = 0, seed = 3,
                           size = 2) +
  scale_x_date(limits = c(as.Date("2025-01-01"), as.Date("2025-11-01")),
               date_breaks = "1 month", date_labels = "%b", name = "2025") +
  scale_y_continuous(limits = c(-25,25),
                     breaks = seq(-25,25,5),
                     labels = function(x){case_when(
                       x < 0 ~ paste0("+", abs(x), "R"),
                       x == 0 ~ "no\nchange",
                       x > 0 ~ paste0("+",x,"D")
                     )},
                     name = "special margin minus 2024 presidential margin") +
  theme_bw() +
  theme(plot.title.position = "plot",
        plot.title = element_text(face = "bold", size = 16),
        plot.background = element_rect(fill = "linen"),
        panel.background = element_rect(fill = "aliceblue"),
        legend.background = element_rect(fill = "aliceblue"))


special.vs.pres <- specials.valid |>
  ggplot(aes(pres_margin, special_margin)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_point(aes(shape = house)) +
  ggrepel::geom_text_repel(aes(label = label), min.segment.length = 0, seed = 3,
                           size = 2) +
  scale_x_continuous(limits = c(-70,70),
                     breaks = seq(-70,70,15),
                     labels = function(x){case_when(
                       x < 0 ~ paste0("+", abs(x), "R"),
                       x == 0 ~ "tie",
                       x > 0 ~ paste0("+",x,"D")
                     )},
                     name = "presidential margin (Harris % minus Trump %)") +
  scale_y_continuous(limits = c(-70,70),
                     breaks = seq(-70,70,15),
                     labels = function(x){case_when(
                       x < 0 ~ paste0("+", abs(x), "R"),
                       x == 0 ~ "tie",
                       x > 0 ~ paste0("+",x,"D")
                     )},
                     name = "special margin (Dem % minus Rep %)") +
  labs(title = "special margin vs. presidential margin") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", size = 12),
        legend.position = "none",
        plot.background = element_rect(fill = "linen"),
        panel.background = element_rect(fill = "aliceblue"))

margin.shift.vs.turnout <- specials.valid |>
  ggplot(aes(turnout_change, margin_change)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(aes(shape = house)) +
  ggrepel::geom_text_repel(aes(label = label), min.segment.length = 0, seed = 3,
                           size = 2) +
  scale_x_continuous(limits = c(-90,-40),
                     breaks = seq(-90,-40,10),
                     labels = scales::percent_format(scale = 1),
                     name = "change in turnout relative to Nov. 2024") +
  scale_y_continuous(limits = c(-30,30),
                     breaks = seq(-30,30,10),
                     labels = function(x){case_when(
                       x < 0 ~ paste0("+", abs(x), "R"),
                       x == 0 ~ "no\nchange",
                       x > 0 ~ paste0("+",x,"D")
                     )},
                     name = "special margin minus 2024 presidential margin") +
  labs(title = "margin shift vs. turnout shift") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", size = 12),
        legend.position = "none",
        plot.background = element_rect(fill = "linen"),
        panel.background = element_rect(fill = "aliceblue"))

subtitle.text <- paste("Across", nrow(specials.valid), "special elections,",
                       "the average shift has been", 
                       round(mean(specials.valid$margin_change),1),
                       "points toward the Democrats, relative to the 2024 presidential election.",
                       "Turnout has averaged", paste0(round(abs(mean(specials.valid$turnout_change)),1),"%"),
                       "lower.")

margin.shift.over.time / (special.vs.pres + margin.shift.vs.turnout) +
  plot_layout(guides = "collect") +
  plot_annotation(title = "Legislative Special Elections in 2025",
                  subtitle = str_wrap(subtitle.text, 134),
                  caption = "Calculations by John D. Johnson (github.com/jdjohn215/special-elections-2025).  Annotations show the state abbreviation, upper or lower house, and district number.",
                  theme = theme(plot.title = element_text(face = "bold", size = 16),
                                plot.background = element_rect(fill = "linen")))
ggsave("graphics/specials-comparison.png", width = 10, height = 8)
