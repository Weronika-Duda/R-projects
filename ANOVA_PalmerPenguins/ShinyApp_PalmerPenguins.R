# Weronika Duda i Amelia Madej 

# Instalacja i załadowanie pakietów

library(shiny)
library(palmerpenguins)
library(ggplot2)
library(dplyr)
library(plotly)
library(bslib)
library(moments)
library(shinythemes)
library(thematic)
bootswatch_themes()

# Załadowanie danych
data <- penguins 
kolor_gatunkow <- c("Adelie" = "#ff6d00", "Chinstrap" = "#c45bca", "Gentoo" = "#067376")

# Wybór zmiennych numerycznych (bez roku)
zmienne_numeryczne <- names(penguins)[sapply(penguins, is.numeric)]
zmienne_numeryczne <- setdiff(zmienne_numeryczne, "year")

# Funkcja do generowania statystyk opisowych
generate_summary_stats <- function(selected_column) {
  mean_val <- mean(selected_column, na.rm = TRUE)
  sd_val <- sd(selected_column, na.rm = TRUE)
  median_val <- median(selected_column, na.rm = TRUE)
  min_val <- min(selected_column, na.rm = TRUE)
  max_val <- max(selected_column, na.rm = TRUE)
  
  stats_data <- data.frame(
    Statystyka = c("Średnia", "Odchylenie standardowe", "Mediana", "Minimum", "Maksimum"),
    Wartość = c(mean_val, sd_val, median_val, min_val, max_val)
  )
  
  return(stats_data)
}

# Definicja interfejsu użytkownika
ui <- navbarPage(
  theme = bs_theme(
    bootswatch = "minty"
  ),
  titlePanel("Test ANOVA", windowTitle = "Test ANOVA"),
  tabPanel("Wybór zmiennej zależnej",
           helpText("Wybierz zmienną, aby wyświetlić wykresy skrzypcowe dla tej zmiennej, 
                    a w kolejnych etapach zbadać dla niej zalożenia testu ANOVA i go wykonać:"),
           selectInput("zmienna_zalezna", "Zmienna numeryczna:", choices = zmienne_numeryczne),
           actionButton("pokaz_main_plot", "Pokaż wykresy skrzypcowe"),
           plotOutput("main_plot")
  ),
  navbarMenu("Poznanie zmiennych",
             tabPanel("Statystyki opisowe",
                      sidebarLayout(
                        sidebarPanel(
                          helpText("Wybierz zmienną, aby wyświetlić statystyki opisowe dla tej zmiennej."),
                          selectInput("zmienna_zalezna_testy", "Zmienna numeryczna:", choices = zmienne_numeryczne),
                        ),
                        mainPanel(
                          tableOutput("summary_table_stats")
                        )
                      )
             ),
             tabPanel("Rozkłady",
                      sidebarLayout(
                        sidebarPanel(
                          helpText("Wybierz zmienną, aby narysować interaktywny wykres pudełkowy z wąsem i histogram dla tej zmiennej."),
                          selectInput("zmienna_zalezna_plot", "Zmienna zależna:", choices = zmienne_numeryczne),
                          actionButton("pokaz_boxploty", "Pokaż Boxploty"),
                          actionButton("pokaz_histogramy", "Pokaż Histogramy"),
                          sliderInput("liczba_kubelkow", "Liczba kubełków:",
                                      min = 1, max = 30, value = 15)
                        ),
                        mainPanel(
                          plotlyOutput("wykres_boxplot"),
                          plotlyOutput("wykres_histogram")
                        )
                      )
             )
  ),
  tabPanel("Sprawdzanie założeń",
           sidebarLayout(
             sidebarPanel(
               helpText("Dla poprzednio wybranej zmiennej sprawdź założenie dla jednoczynnkikowej analizy wariancji."),
               checkboxGroupInput("testy", "Wybierz testy:",
                                  choices = c("Shapiro-Wilk", "Bartlett", "Kruskal-Wallis", "TukeyHSD"),
                                  selected = c())
             ),
             mainPanel(
               verbatimTextOutput("shapiro_test"),
               verbatimTextOutput("bartlett_test"),
               verbatimTextOutput("kruskal_test"),
               verbatimTextOutput("tukey_test")
             )
           )
  ),
  tabPanel("Obliczanie ANOVA",
           helpText("Dla poprzednio wybranej zmiennej zależnej jest wykonywany test ANOVA"),
           verbatimTextOutput("podsumowanie_anova")
  )
)


# Backend
server <- function(input, output) {
  
  observeEvent(input$pokaz_main_plot, {
    output$main_plot <- renderPlot({
      req(input$zmienna_zalezna)
      
      penguins %>%
        ggplot(aes(x = species, y = !!sym(input$zmienna_zalezna), color = species, fill = species, alpha = 0.4)) +
        geom_violin(trim = TRUE) +
        geom_boxplot(width = 0.1, position = position_dodge(0.75), alpha = 0.8, color = "black") +
        labs(title = paste("Wykres skrzypcowy i pudełkowy dla", input$zmienna_zalezna, "względem gatunku pingwina"),
             x = "Gatunek",
             y = input$zmienna_zalezna) +
        theme_light() +
        theme(plot.title = element_text(size = 13, face = "bold"))  +
        scale_color_manual(values = kolor_gatunkow) +
        scale_fill_manual(values = kolor_gatunkow)
      
      
    })
  })
  

  output$summary_table_stats <- renderTable({
    req(input$zmienna_zalezna_testy)
    
    selected_column <- penguins[[input$zmienna_zalezna_testy]]
    generate_summary_stats(selected_column)
    
  })
  observeEvent(input$zmienna_zalezna, {
    output$shapiro_test <- renderPrint({
      if ("Shapiro-Wilk" %in% input$testy) {
        model <- aov(penguins[[input$zmienna_zalezna_testy]] ~ species, data = penguins)
        shapiro_results <- shapiro.test(model$residuals)
        paste("Shapiro-Wilk Test:", "p-value =", format.pval(shapiro_results$p.value))
      }
    })
    
    output$bartlett_test <- renderPrint({
      if ("Bartlett" %in% input$testy) {
        bartlett_results <- bartlett.test(penguins[[input$zmienna_zalezna_testy]], penguins$species)
        paste("Bartlett Test:", "p-value =", format.pval(bartlett_results$p.value))
      }
    })
    
    output$kruskal_test <- renderPrint({
      if ("Kruskal-Wallis" %in% input$testy) {
        kruskal_results <- kruskal.test(penguins[[input$zmienna_zalezna_testy]] ~ penguins$species)
        paste("Kruskal-Wallis Test:", "p-value =", format.pval(kruskal_results$p.value))
      }
    })
    
    output$tukey_test <- renderPrint({
      if ("TukeyHSD" %in% input$testy) {
        model <- aov(penguins[[input$zmienna_zalezna_testy]] ~ penguins$species)
        Tukey_results <- TukeyHSD(model)
        Tukey_results
      }
    })
  })
  
  observeEvent(input$pokaz_boxploty, {
    output$wykres_boxplot <- renderPlotly({
      req(input$zmienna_zalezna_plot)
      
      penguins %>%
        ggplot(aes(x = species, y = !!sym(input$zmienna_zalezna_plot))) +
        geom_boxplot(fill = "slategray2", color = "slategray3") +
        labs(title = paste("Wykres pudełkowy dla", input$zmienna_zalezna_plot, "względem gatunku pingwina"),
             x = "Gatunek",
             y = input$zmienna_zalezna_plot) +
        theme_light() +
        theme(plot.title = element_text(size = 13, face = "bold"))
    })
  })
  
  observeEvent(input$pokaz_histogramy, {
    output$wykres_histogram <- renderPlotly({
      req(input$zmienna_zalezna_plot)
      
      penguins %>%
        ggplot() + 
        geom_histogram(aes(x = !!sym(input$zmienna_zalezna_plot)), 
                       bins = input$liczba_kubelkow,
                       fill = "slategray2", 
                       color = "slategray3") +
        labs(title = paste("Histogram dla", input$zmienna_zalezna_plot),
             x = input$zmienna_zalezna_plot,
             y = "Częstość") +
        theme_light() +
        theme(plot.title = element_text(size = 13, face = "bold"))
      
    })
  })
  
  output$podsumowanie_anova <- renderPrint({
    req(input$zmienna_zalezna)
    model <- aov(as.formula(paste(input$zmienna_zalezna, "~ species")), data = penguins)
    cat("Jednoczynnikowa analiza ANOVA:")
    cat("")
    summary(model)
  })
}


shinyApp(ui = ui, server = server)