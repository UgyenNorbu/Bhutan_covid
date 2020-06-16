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
colnames(covid_Bhutan) <-c("Date", "total_cases", "new_case", "Admitted_iso", "Admitted_di_iso", "Recvrd", "Deaths", "Quar_in", "Quar_disch", "Tested_tot","Tested_PCR", "Ref")

covid_Bhutan <- covid_Bhutan[-1, ]

covid_Bhutan <- covid_Bhutan %>% 
    clean_names() %>% 
    mutate(date = as.Date(date, format("%B %d"))) %>% 
    mutate(total_cases =parse_number(total_cases)) %>% 
    mutate(new_case = parse_number(new_case)) %>% 
    mutate(admitted_iso = parse_number(admitted_iso)) %>% 
    mutate(admitted_di_iso = parse_number(admitted_di_iso)) %>% 
    mutate(recvrd = parse_number(recvrd)) %>% 
    mutate(deaths = parse_number(deaths)) %>% 
    mutate(quar_in = parse_number(quar_in)) %>% 
    mutate(quar_disch = parse_number(quar_disch)) %>% 
    mutate(tested_tot = parse_number(tested_tot)) %>% 
    mutate(tested_pcr = parse_number(tested_pcr)) %>% 
    select(-ref)
    
write_csv(covid_Bhutan, "output_data/covid_Bhutan.csv")

my_subtitle <- paste(paste("New case(s) =", 
                           covid_Bhutan$new_case[nrow(covid_Bhutan)]), 
                     paste("Recoverd =", 
                           covid_Bhutan$recvrd[nrow(covid_Bhutan)]),
                     paste("Total cases =", 
                           covid_Bhutan$total_cases[nrow(covid_Bhutan)]),
                     sep = "    ")

covid_Bhutan %>% 
    ggplot(aes(x = date)) +
    geom_line(aes(y = total_cases, color = "Total cases")) +
    geom_line(aes(y = new_case, color = "New case")) +
    geom_line(aes(y = recvrd, color = "Recovered")) +
    scale_x_date(date_breaks = "1 week", date_labels = "%d-%B") +
    labs(x = "Date",
         y = "Number of patients",
         title = "Covid-19 cases in Bhutan",
         subtitle = my_subtitle,
         caption = "Data source: https://en.wikipedia.org/wiki/COVID-19_pandemic_in_Bhutan#cite_note-MoH-23May-43",
         colour = "Case type") +
    theme_light() +
    theme(axis.text.x = element_text(angle = 15),
          plot.title = element_text(face = "bold", hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          legend.position = c(0.8, 0.9))

ggsave("output_viz/Covid_19_cases_in_Bhutan.jpg", width = 25, height = 15, units = "cm")
