library(tidyverse)

data <- read_csv("C:/Users/Kay93/Downloads/GLB.Ts+dSST.csv", skip = 1, na = "***") %>%
  select(Year = Year, month.abb) %>%
  pivot_longer(-Year, names_to = "Months", values_to = "Values") %>%
  drop_na() %>%
  mutate(Months = factor(Months, levels = month.abb))

last_Dec <- data %>%
  filter(Months == "Dec") %>%
  mutate(Year = Year + 1,
         Months = "Last_Dec")

last_Jan <- data %>%
  filter(Months == "Jan") %>%
  mutate(Year = Year - 1,
         Months = "Next_Jan")

processed_data <- bind_rows(last_Dec, data, last_Jan) %>%
  mutate(Months = factor(Months, levels = c("Last_Dec", month.abb, "Next_Jan")),
         month_number = as.numeric(Months)- 1,
         this_year = Year == 2022)

annotation <- processed_data %>% 
  slice_max(Year) %>%
  slice_max(month_number)

processed_data %>%
  ggplot(aes(x=month_number, y=Values, 
             group=Year, color=Year, size = this_year)) +
  geom_hline(yintercept = 0, color = "white") +
  geom_line() +
  geom_text(data = annotation,
            aes(x=month_number, y=Values, label=Year, color=Year),
            inherit.aes = FALSE, 
            hjust = 0,nudge_x = 0.15, fontface = "bold") +
  scale_x_continuous(breaks = 1:12,
                     labels = month.abb,
                     sec.axis = dup_axis(name = NULL, labels = NULL)) +
  scale_y_continuous(breaks = seq(-2,2,0.2),
                     sec.axis = dup_axis(name = NULL, labels = NULL)) +
  scale_size_manual(breaks = c(FALSE, TRUE),
                    values = c(0.25, 1), guide = "none") +
  scale_color_viridis_c(breaks = seq(1880, 2020, 20),
                        guide = guide_colorbar(frame.colour = "white")) + 
  coord_cartesian(xlim=c(1,12)) +
  labs(x = NULL,
       y = "Temperature change since pre-industrial time [\u00B0C]",
       title = "Global temperature change since 1880 by month") +
  theme(
    panel.background = element_rect(fill = "black", color ="white", size = 1),
    plot.background = element_rect(fill= "#444444"),
    panel.grid = element_blank(),
    axis.text = element_text(color = "white"),
    axis.ticks = element_line(color = "white"),
    axis.ticks.length = unit(-5, "pt"),
    axis.title = element_text(color = "white"),
    plot.title = element_text(color = "white", hjust = 0.5),
    legend.title = element_blank(),
    legend.background = element_rect(fill = NA),
    legend.text = element_text(color = "white"),
    legend.key.height = unit(55, "pt")
)

