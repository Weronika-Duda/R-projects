# Weronika Duda

# Ładowanie pakietów
library(rvest)
library(httr)
library(dplyr)
library(ggplot2)
library(readr)
library(tidyverse)
library(RColorBrewer)
library(plotly)

# Definicja adresu URL i pobranie strony internetowej
url <- "https://pl.wikipedia.org/wiki/Europejski_Złoty_But"
resp <- GET(url)
page <- read_html(url)

# Pobranie tabeli z węzłów HTML
tab <- page %>% 
  html_nodes(css = "table.wikitable")
print(tab)

# Przetworzenie pobranej tabeli na ramkę danych
tabela <- tab[[1]] %>% 
  html_table(fill=T)

tabela <- as.data.frame(tabela)

# Wyświetlenie pierwszych wierszy
head(tabela)

# Usunięcie tekstu przed i włącznie z wykrzyknikiem w kolumnie 'Kraj'
tabela$Kraj <- sub(".*!\\s*", "", tabela$Kraj)

# Konwersja danych do odpowiednich typów
tabela$Kraj <- as.factor(tabela$Kraj)
tabela$Klub <- as.factor(tabela$Klub)
tabela$Liga <- as.factor(tabela$Liga)

# Przekształcenie kolumny 'Sezon' zawierającej daty w formatach '1967/68' na rok początkowy
tabela$RokPoczatkowy <- as.numeric(sub("/.*", "", tabela$Sezon))

# Wyświetlenie struktury ramki danych 'tabela'
str(tabela)

# Wybór danych do analizy
tabela <- tabela %>%
  select(Kraj, Gole, Klub, Liga, RokPoczatkowy)

# Wstępna analiza statystyczna

# 1 Analiza średniej liczby goli dla każdego kraju
tabela %>%
  group_by(Kraj) %>%
  summarise(średnia_goli = mean(Gole),
            suma_goli = sum(Gole)) %>%
  arrange(desc(suma_goli))

# 2 Podsumowanie statystyk dla zmiennej "Gole"
summary(tabela$Gole)

# Wizualizacje

# Analizuję zmienność oraz dominujące trendy w liczbie zdobytych goli w piłce nożnej w różnych krajach, klubach, ligach oraz w poszczególnych latach.

# 1.

#Wykres słupkowy liczby goli dla każdego kraju
tabela %>%
  group_by(Kraj) %>%
  summarise(suma_goli = sum(Gole)) %>%
  arrange(desc(suma_goli)) %>%
  ggplot() +
  geom_col(aes(y = reorder(Kraj, suma_goli), x = suma_goli), fill = "lightgreen") +
  theme_bw() +
  labs(title = "Liczba goli dla każdego kraju", x = "Liczba goli", y = "Kraj")

# 2.

# Wykres słupkowy liczby goli dla każdego klubu
tabela %>%
  group_by(Klub) %>%
  summarise(suma_goli = sum(Gole)) %>%
  arrange(desc(suma_goli)) %>%
  ggplot() +
  geom_col(aes(x = reorder(Klub, suma_goli), y = suma_goli), fill = "lightblue") +
  theme_bw() +
  labs(title = "Liczba goli dla każdego klubu", x = "Liczba goli", y = "Klub") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# 3. 

#Przygotowanie danych
dane_rok <- tabela %>%
  group_by(RokPoczatkowy) %>%
  summarise(Suma_Goli = sum(Gole))

# Wykres liniowy zmiany liczby goli w poszczególnych latach
ggplot(dane_rok, aes(x = RokPoczatkowy, y = Suma_Goli)) +
  geom_line(size = 1) + 
  theme_bw() +
  labs(title = "Zmiana liczby goli w poszczególnych latach", x = "Rok", y = "Suma goli") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_x_continuous(breaks = seq(min(tabela$RokPoczatkowy), max(tabela$RokPoczatkowy), by = 2))

# 4.

# Sumowanie liczby goli dla każdego klubu
suma_goli_kluby <- tabela %>%
  group_by(Klub) %>%
  summarise(Laczna_Liczba_Goli = sum(Gole)) %>%
  arrange(desc(Laczna_Liczba_Goli))  

# Wybór pięciu klubów z największą liczbą goli
top5_kluby <- suma_goli_kluby %>%
  top_n(5, Laczna_Liczba_Goli)

# Wygenerowanie wykresu punktowego dla danych dotyczących 5 klubów z największą liczbą goli
wykres4 <- ggplot(tabela[tabela$Klub %in% top5_kluby$Klub, ], aes(x = RokPoczatkowy, y = Gole, color = Klub)) +
  geom_point(size = 5) +
  theme_bw() +
  scale_color_brewer(palette = "Dark2") +
  labs(title = "Tendencja liczby goli w latach dla 5 klubów z największą liczbą goli", x = "Rok", y = "Liczba goli") +
  scale_x_continuous(breaks = seq(min(tabela$RokPoczatkowy), max(tabela$RokPoczatkowy), by = 2))

# Zamiana na wykres interaktywny
interaktywny_wykres4 <- ggplotly(wykres4)

interaktywny_wykres4

# 5. 
# Obliczenie łącznej liczby goli dla każdej ligi
suma_goli_liga <- tabela %>%
  group_by(Liga) %>%
  summarise(Laczna_Liczba_Goli_Liga = sum(Gole)) %>%
  arrange(desc(Laczna_Liczba_Goli_Liga)) 

# Wybór pięciu lig z największą liczbą goli
top5_liga <- suma_goli_liga %>%
  top_n(5, Laczna_Liczba_Goli_Liga)

# Wygenerowanie wykresu punktowego dla danych dotyczących 5 lig z największą liczbą goli
wykres5 <- ggplot(filter(tabela, Liga %in% top5_liga$Liga), aes(x = RokPoczatkowy, y = Gole, color = Liga)) +
  geom_point(size = 5) +  
  theme_bw() +
  labs(title = "Zmiana liczby goli w latach dla 5 lig z największą liczbą goli", x = "Rok", y = "Liczba goli") +
  scale_color_brewer(palette = "Set2") +
  scale_x_continuous(breaks = seq(min(tabela$RokPoczatkowy), max(tabela$RokPoczatkowy), by = 2))

# Zamiana na wykres interaktywny
interaktywny_wykres5 <- ggplotly(wykres5)

interaktywny_wykres5
