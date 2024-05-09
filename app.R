library(shiny)
library(ggplot2)

# Création du data frame avec les données
data <- data.frame(
  Banque = c("BNC", "BUH", "CAPITAL BANK", "CITIBANK", "SOGEBANK", "SOGEBEL", "UNIBANK", "TAUX MAXIMUM", "TAUX MINIMUM"),
  Achat = c(131.2500, 132.0000, 131.5000, 131.0000, 131.2500, 131.2500, 131.7500, 132.0000, 131.0000),
  Vente = c(133.7500, 133.7500, 133.7500, 134.0000, 133.7500, 133.7500, 133.7500, 134.0000, 133.7500),
  BRH = c(132.3145, 132.3145, 132.3145, 132.3145, 132.3145, 132.3145, 132.3145, 132.3145, 132.3145)
)

# Interface utilisateur
ui <- fluidPage(
  titlePanel("Tableau de bord des taux de change HTG/USD"),
  sidebarLayout(
    sidebarPanel(
      selectInput("banque", "Sélectionner une banque, taux maximum, taux minimum ou taux de référence BRH:",
                  choices = unique(data$Banque))
    ),
    mainPanel(
      plotOutput("graphique"),
      plotOutput("histogramme")
    )
  )
)

# Serveur
server <- function(input, output, session) {
  output$graphique <- renderPlot({
    banqueSelectionnee <- input$banque
    dataBanque <- subset(data, Banque == banqueSelectionnee)

    ggplot(dataBanque, aes(x = Achat, y = BRH, color = Banque)) +
      geom_point() +
      labs(title = "Graphe du taux d'achat et de vente par Banque",
           x = "Taux d'achat", y = "Taux de vente") +
      theme_minimal()
  })

  output$histogramme <- renderPlot({
    ggplot(data, aes(x = Achat, y = BRH)) +
      geom_point() +
      geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") + # Ajout de la ligne de pente 1
      geom_abline(intercept = 0, slope = -1, color = "blue", linetype = "dashed") + # Ajout de la ligne de pente -1
      labs(title = "Comparaison des taux d'achat par rapport au taux de référence BRH",
           x = "Taux d'achat", y = "Taux de référence BRH") +
      theme_minimal()
  })
}

# Lancez l'application Shiny
shinyApp(ui = ui, server = server)







