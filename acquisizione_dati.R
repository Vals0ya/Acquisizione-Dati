
# Scarico librerie e importo dataset --------------------------------------
require(tidyverse)
require(magrittr)
library(lubridate)
library(dplyr)
library(tidyr)
require(ggplot2)
require(gridExtra)
data <- read_csv("Dati/Palestine_Body_Count.csv")


# cambio nome variabili ---------------------------------------------------

data <- data %>% 
  rename("Date_of_death" = "Date of death",
         "Event_location"="Event location",
         "Date_of_event" = 'Date of event', 
         "Event_location_region" = 'Event location - Region', 
         "Total" = "Name",
         "Type_of_injury" ="Type of injury")

# Converto gli oggetti Deathdate e Dateofevent in oggetti Date ------------

data <- data %>% mutate(Date_of_death = as.Date(Date_of_death, format = "%m/%d/%Y")) 
class(data$Date_of_death)
data <- data %>% 
  mutate(Date_of_event = as.Date(Date_of_event, format = "%m/%d/%Y")) 
class(data$Date_of_event)
#con lubridate creo altre colone specifiche per i paramentei delle date, rivonosciuti gia come formato DATE
data <- data %>%
  mutate( 
    Death_year = lubridate::year(Date_of_death), 
    Death_month = lubridate::month(Date_of_death), 
    Death_day = lubridate::day(Date_of_death), 
    Event_year = lubridate::year(Date_of_event), 
    Event_month = lubridate::month(Date_of_event), 
    Event_day = lubridate::day(Date_of_event)) 


# grafico iniziale morti per cittadinanza ---------------------------------

Morticitta <-ggplot(data%>% filter(Citizenship != "Jordanian" & Citizenship != "American"), aes(x = Citizenship, fill = Citizenship)) +
  geom_bar() +
  labs(title = "Numeri di morti per cittadinanza",
       x = "Nazionalità", y = "Numero di persone") +
  scale_fill_manual(values = c("#E69F00","#56B4E9" )) # Colori personalizzati per le barre

# Conteggio Palestinesi per ogni Reason -----------------------------------

data %>% distinct(Reason) 
# Selezione delle osservazioni con Citizenship = Palestinian
count_citt<-data %>% select(Death_year,Citizenship,Reason)%>% 
  filter(Citizenship == "Palestinian") %>% group_by(Reason) %>% summarize(count = n())


# Creazione del grafico orizzontale Ragioni dim morte dei palestinesi
#grafico ultimato 
PlotReasPal <- ggplot(count_citt, aes(x=count, y=Reason)) +
  geom_col(fill = "#00B140", color = "black", size = 0.5) +
  labs(title = "Conteggio delle ragioni di morte dei Cittadini Palestinesi",
       x = "",
       y = "") +
  scale_x_continuous(limits = c(0, max(count_citt$count)), breaks = seq(0, 10000, 1000)) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 8),
        axis.line = element_line(colour = "black"),
        panel.grid.major.x = element_line(colour = "gray90"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.border = element_blank(),
        legend.position = "none") +
  theme_light() +
  theme(plot.margin = margin(5, 10, 5, 5, "pt"))

# Tipo di danno per ogni palestinese ucciso da Reason =esercito israeliano --------

# filtro i dati per ottenere solo le osservazioni di interesse
filtered_data <- data %>%
  filter(Citizenship == "Palestinian", Reason == "Palestinians killed by Israeli security forces") %>% 
  count(Type_of_injury)# creo un nuovo dataframe con il conteggio delle occorrenze di ogni valore di Type_of_injury

# creo il grafico ultimato
Dannosecforc <- ggplot(filtered_data, aes(x = Type_of_injury, y = n)) +
  geom_col(fill = "#00B140") +  # colore delle colonne
  labs(x = "Type of Injury", y = "Count", title = "Tipologia di danno inflitto ai Palestinesi dalle Security Forces Israeliane") + # label dei titoli dell'asse x, dell'asse y e del grafico
  scale_y_continuous(breaks = seq(0, max(filtered_data$n), 1000)) +
  theme_minimal() +  # tema minimalista
  theme(plot.title = element_text(size = 20, hjust = 0.5),  # stile del titolo del grafico
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),  # stile delle etichette dell'asse x
        axis.text.y = element_text(size = 10),  # stile delle etichette dell'asse y
        axis.title.x = element_text(size = 12),  # stile del titolo dell'asse x
        axis.title.y = element_text(size = 12),  # stile del titolo dell'asse y
        panel.grid.major = element_blank(),  # rimuove le linee della griglia maggiori
        panel.grid.minor = element_blank())+ # rimuove le linee della griglia minori
  theme_light()+  geom_text(aes(label = n), vjust = -0.5, size = 3.5) #metto il conteggio diogni barra in cima


# Conteggio totale israeliani per ogni Reason -----------------------------

count_citt_isr <-data %>% select(Citizenship,Reason) %>% filter(Citizenship == "Israeli") %>% 
  group_by(Reason) %>% summarize(count = n())
distinct(count_citt_isr) %>% filter(Reason != "Palestinians killed by Israeli security forces")


# Creazione del grafico orizzontale ragioni di morte israeliani
  Reasonisrael <- ggplot(count_citt_isr%>% filter(Reason != "Palestinians killed by Israeli security forces"), aes(x=count, y=Reason)) +
  geom_col(fill = "#0B72B8", color = "black", size = 0.5) +
  labs(title = "Conteggio delle ragioni di morte dei Cittadini Israeliani",
       x = "",
       y = "") +
  scale_x_continuous(limits = c(0, max(count_citt_isr$count)), breaks = seq(0, 1500, 100)) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 8),
        axis.line = element_line(colour = "black"),
        panel.grid.major.x = element_line(colour = "gray90"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.border = element_blank(),
        legend.position = "none") +
  theme_light() +
  theme(plot.margin = margin(5, 10, 5, 5, "pt"))
  
  
  

# Conteggio totale dei Type of injury per le due Reason principali ---------
  #Seleziona solo le osservazioni di interesse
   Inju_isr <- data %>%
    filter(Citizenship == "Israeli" & Reason == "Israeli civilians killed by Palestinians") %>%
    group_by(Type_of_injury) %>% 
    summarize(count_isr = n())
  
  Inju_isr1 <- data %>%
    filter(Citizenship == "Israeli" & Reason == "Israeli security forces killed by Palestinians") %>% 
    group_by(Type_of_injury) %>% 
    summarize(count_isr1 = n())
  
  # Unisco le due tabelle
  
  Inju_joined <- full_join(Inju_isr, Inju_isr1, by = "Type_of_injury") %>%
    replace_na(list(count_isr = 0, count_isr1 = 0))
  #grafico confronto type of injury per le due reason principali
  plotinjuryconfronto <-  ggplot(Inju_joined, aes(x = Type_of_injury)) +
    geom_col(aes(y = count_isr, fill = "count_isr"), position = position_dodge(width = 0.9), alpha = 0.9) +
    geom_col(aes(y = count_isr1, fill = "count_isr1"), position = position_dodge(width = 0.9), alpha = 0.9) +
    labs(x = "", y = "", fill = "", title = "Confronto dei danni inflitti secondo le due ragioni di morte prevalenti", title.size = 20) +
    scale_y_continuous(limits = c(0, max(Inju_joined$count_isr, Inju_joined$count_isr1) + 50),
                       breaks = seq(0, max(Inju_joined$count_isr, Inju_joined$count_isr1) + 50, 50)) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1,size = 13),
          axis.text.y = element_text(size = 10),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          panel.grid.major = element_line(size = 0.5),
          panel.grid.minor = element_blank(),
          legend.position = "bottom",
          legend.text = element_text(size = 15),
          legend.title = element_text(size = 14),
          legend.key.size = unit(1.5, "lines")) +
    scale_fill_manual(values = c("#0B72B8", "#F8766D"),
                      labels = c("Israeli civilians killed by Palestinians", "Israeli security forces killed by Palestinians"))
  
  
  

# Torno al conteggio totale ed analizzo l'anno 2014 -----------------------

  # Calcolo il numero di morti per ogni anno
  df_deaths <- data  %>%
    group_by(Event_year) %>%
    summarize(TotalDeaths = n())
  
  
  # Creo il grafico a barre
  mortiannuali <-ggplot(df_deaths, aes(x = Event_year, y = TotalDeaths)) +
    geom_bar(stat = "identity", fill = "#56B4E9") +
    labs(title = "Numero di morti annuali",
         x = "Anno", y = "Numero di morti")
  
  
  
  #filtro per anno 2014
  d.2014<- data %>% filter(Event_year==2014)
  
  
  # seleziona le prime 10 città con il maggior numero di morti
  df_top10 <- d.2014 %>% 
    group_by(Event_location) %>% 
    summarize(Total = n()) %>% 
    top_n(10)
  
  # creo una scala di blu
  blues <- c("#EFF3FF", "#BDD7E7", "#6BAED6", "#3182BD", "#08519C","#EFF3FF", "#BDD7E7", "#6BAED6", "#3182BD", "#08519C")
  
  # creo il grafico a barre del 2014 filtrato per la top 10 Event location
  Loc_total <- ggplot(df_top10, aes(x = Event_location, y = Total, fill = Event_location)) + 
    geom_bar(stat = "identity", fill = "#1f77b4") + 
    scale_fill_gradient(low = blues[1], high = blues[length(blues)], guide = "none") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
    ggtitle("")
  
  
  #grafico delle Injury dell anno 2014
  injury_2014 <- ggplot(data %>% filter(Event_year == 2014 & Citizenship != "Jordanian"), aes(x = Citizenship, fill = Type_of_injury)) +
    geom_bar(position = "dodge", width = 0.8, stat = "count") +
    theme_bw() +
    labs(title = "Tipo di lesione per palestinesi e israeliani",
         fill = "Tipo di lesione") +
    guides(fill = guide_legend(title = "Tipo di lesione")) +
    scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#D55E00", "#CC79A7")) +
    facet_wrap(~Citizenship, ncol = 2, scales = "free_x") +
    theme(strip.background = element_blank(),
          panel.spacing.x = unit(2, "cm"))
  

# Confronto delle Reason tra il 2014 e gli altri anni ---------------------
#raggruppo le Reason del 2014
  df_2014_counts <- d.2014 %>% 
    group_by(Reason) %>% 
    summarise(count = n())  
  
  #raggruppamento altri anni
  # Raggruppamento per Reason e conteggio negli altri anni
  df_other_counts <- data %>% 
    filter(year(Date_of_event) != 2014) %>% 
    group_by(Reason) %>% 
    summarise(count = n())
  
  
  # Calcolo delle percentuali per il 2014
  df_2014_counts <- df_2014_counts %>%
    mutate(percent = count/sum(count)*100)
  
  # Calcolo delle percentuali negli altri anni
  df_other_counts <- df_other_counts %>%
    mutate(percent = count/sum(count)*100)
  
  # Combinazione dei dati in un unico data frame
  df_combined <- bind_rows(
    mutate(df_2014_counts, year = "2014"),
    mutate(df_other_counts, year = "Other"))
  
  # Grafico a barre
  reasoncomparison <- ggplot(df_combined, aes(x = year, y = percent, fill = fct_reorder(Reason, percent))) +
    geom_bar(stat = "identity") +
    labs(title = "Morti per Reason dal 2014 e negli altri anni",
         x = "Anno", y = "Percentuale", fill = "Reason") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
    scale_fill_viridis_d(option = "plasma", direction = -1) +
    guides(fill = guide_legend(reverse = TRUE))
  
  
  