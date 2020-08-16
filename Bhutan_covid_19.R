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

as.Date(covid_Bhutan$date, format("%B %d"))

as.Date("March 6", format("%B %d"))

covid_Bhutan %>% 
    mutate(Date = as.Date(Date, format("%B %d")))


covid_Bhutan <- covid_Bhutan %>% 
    mutate(Date = as.Date(Date, format("%B %d"))) %>% 
    mutate(total_cases =parse_number(total_cases)) %>% 
    mutate(new_case = parse_number(new_case)) %>% 
    mutate(Admitted_iso = parse_number(Admitted_iso)) %>% 
    mutate(Admitted_di_iso = parse_number(Admitted_di_iso)) %>% 
    mutate(Recvrd = parse_number(Recvrd)) %>% 
    mutate(Deaths = parse_number(Deaths)) %>% 
    mutate(Quar_in = parse_number(Quar_in)) %>% 
    mutate(Quar_disch = parse_number(Quar_disch)) %>% 
    mutate(Tested_tot = parse_number(Tested_tot)) %>% 
    mutate(Tested_PCR = parse_number(Tested_PCR)) %>% 
    select(-Ref)
    
write_csv(covid_Bhutan, "output_data/covid_Bhutan.csv")

my_subtitle <- paste(paste("New case(s) =", 
                           covid_Bhutan$new_case[nrow(covid_Bhutan)]), 
                     paste("Recoverd =", 
                           covid_Bhutan$recvrd[nrow(covid_Bhutan)]),
                     paste("Total cases =", 
                           covid_Bhutan$total_cases[nrow(covid_Bhutan)]),
                     sep = "    ")

covid_Bhutan %>% 
    ggplot(aes(x = Date)) +
    geom_line(aes(y = total_cases, color = "Total cases")) +
    geom_line(aes(y = new_case, color = "New case")) +
    geom_line(aes(y = Recvrd, color = "Recovered")) +
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
          legend.position = "top")

ggsave("output_viz/Covid_19_cases_in_Bhutan.jpg", width = 25, height = 15, units = "cm")
