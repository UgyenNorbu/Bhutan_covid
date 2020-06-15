library(rvest)
library(tidyverse)
library(janitor)
library(scales)
library(ggrepel)


wiki_pg <- "https://en.wikipedia.org/wiki/COVID-19_pandemic_in_Bhutan#cite_note-MoH-23May-43"

covid <- read_html(wiki_pg)

covid_table <- covid %>% 
    html_table(fill = TRUE)

covid_Bhutan <- covid_table[[3]]
covid_Bhutan <- covid_Bhutan %>% 
    clean_names() %>% 
    select(-c(caseno, ref)) %>% 
    mutate(date = as.Date(date, format("%B %d")))

write_csv(covid_Bhutan, "covid_Bhutan.csv")

covid_grouped <- covid_Bhutan %>%
    group_by(date) %>% 
    summarise(new_cases = n()) 

covid_grouped$total_cases <- cumsum(covid_grouped$new_cases)

covid_grouped_long <- pivot_longer(covid_grouped, -date, 
             names_to = "case_type", values_to = "number")

covid_summary <- covid_Bhutan %>% 
    group_by(status) %>% 
    count()

covid_active_case <- covid_summary %>% 
    filter(status == "Admitted") %>% 
    select(n) %>% 
    pull()

covid_recoverd_case <- covid_summary %>% 
    filter(status == "Recovered") %>% 
    select(n) %>% 
    pull()

my_subtitle <- paste(paste("Active cases =", covid_active_case, sep = " "), 
                     paste("Recoverd =", covid_recoverd_case, sep = " "),
                     paste("Total cases =", sum(covid_grouped$new_cases)),
                     sep = "    ")

covid_grouped_long %>% 
    mutate(label = ifelse(date == max(date), covid_grouped_long$number, NA_character_)) %>% 
    ggplot(aes(x = date, y = number, group = case_type)) +
    geom_line(alpha = 0.2, size = 1.5) +
    geom_point(aes(colour = case_type), size = 3) +
    scale_x_date(date_breaks = "1 week", date_labels = "%d-%B") +
    labs(x = "Date",
         y = "Number of patients",
         title = "Covid-19 cases in Bhutan",
         subtitle = my_subtitle,
         caption = "Data source: https://en.wikipedia.org/wiki/COVID-19_pandemic_in_Bhutan#cite_note-MoH-23May-43",
         colour = "Case type") +
    scale_color_discrete(labels = c("New cases", "Total cases")) +
    geom_label_repel(aes(label = label),nudge_x = 1.5, segment.size = 0.0, 
                    label.size = 0.3) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 15),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          legend.position = c(0.8, 0.9))
ggsave("covid_19_cases_in_Bhutan.jpg", width = 25, height = 15, units = "cm")
