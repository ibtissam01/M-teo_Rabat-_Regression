#install.packages("shinydashboard")

library(shiny)
library(shinydashboard)
library(ggplot2)
model <- readRDS("Prediction_température.rds")

# Define UI for application :
ui <- dashboardPage(
  
  # Titre
  dashboardHeader(title = "Prédiction_température"),
  
  # Barre latérale
  dashboardSidebar(

    # Panel utilisateur 
    sidebarUserPanel(
      "ELGHAZI_SOUFIANE",
      icon("user")
      
    ),
    sidebarUserPanel(
      "Amine_Maasri",
      icon("user")
      
    ),
    sidebarUserPanel(
      "Ibtissam_Labyady",
      icon("user")
      
    )
    

    
  ),
  
  # Body
  dashboardBody(
    tabName = "features",
    fluidRow(box(valueBoxOutput("score_prediction"))),
    
    fluidRow(box(numericInput("var1",label="Humidité",value=0,min=0)),
             box(numericInput("var2",label="Précipitation",value=0,min=0))),
    
    fluidRow( box(numericInput("var3",label="Vitesse_Vent",value=0,min=0)),
              box(dateInput("date_input", label = "Date"))),
    fluidRow(
      box(
        title = "Validation Dataset",
        status = "info",
        solidHeader = TRUE,
        collapsible = TRUE,
        dataTableOutput("validation_data_table"),
        width = 12
      )
    ),
    
    fluidRow(
      box(plotOutput("histogramme_temp")),
      box(plotOutput("graph"))
      
    ),
    fluidRow(
      box(plotOutput("Temp_Vent")),
      box(plotOutput("Temp_Humid")),
      box(plotOutput("Tem_Prec"))
    ),
    fluidRow(
      box(plotOutput("variation_temp")),
      box(
        title = "Résumé du modèle",
        solidHeader = TRUE,
        verbatimTextOutput("model_summary")
      )
    )
    
    
  )


)
# Define server logic required to draw a histogram
server <- function(input, output) {
  prediction<-reactive({
    predict(
      model,
      data.frame(
        "Humidité"=input$var1,
        "Précipitation"=input$var2,
        "Vitesse_Vent"=input$var3
      ),
      type="response"
    )
  })
  prediction_label<-reactive({
    if (prediction() > 25) {
      "chaud"
    } else if (prediction() < 15) {
      "froid"
    } else {
      "moyen"
    }
    
  })
  prediction_prob<-reactive({
    plogis(prediction())
  })
  
  output$score_prediction <- renderValueBox({
    temp_prediction <- prediction()
    color <- if (temp_prediction > 25) {
      "red"
    } else if (temp_prediction < 15) {
      "purple"
    } else {
      "green"
    }
    valueBox(
      paste0(round(temp_prediction, 2)),
      paste0(prediction_label()),
      "Prédiction de température",
      icon = icon("thermometer-half"),
      color = color,
      width = 12
    )
  })
  output$histogramme_temp<-renderPlot({
      ggplot(weather_validation, aes(x=Température)) +
      geom_histogram(aes(y=..density..), binwidth = 0.1, fill="blue", alpha=0.5) +
      geom_density(alpha = .2, fill="red") +
      ggtitle("Distibution du Température") +
      xlab("Température") + ylab("Density")+
      coord_cartesian(xlim = c(10, 30))
    
  })
  
  output$Temp_Humid<-renderPlot({
    
    plot(Température ~Humidité ,data = weather_validation,main="Température & Humidité ",xlim = c(0, max(weather_validation$Humidité)), 
         ylim = c(0, max(weather_validation$Température)))
  })
  
  output$Temp_Vent<-renderPlot({
    
    plot(Température ~Vitesse_Vent ,data = weather_validation,main="Température & Vitesse_Vent ",xlim = c(0, max(weather_validation$Vitesse_Vent)), 
         ylim = c(0, max(weather_validation$Température)))
  })
  output$Tem_Prec<-renderPlot({
    
    
    plot(Température ~Précipitation ,data = weather_validation,main="Température & Précipitation ",xlim = c(0, max(weather_validation$Précipitation)),
         ylim = c(0, max(weather_validation$Température)))
  })
  
  output$variation_temp<-renderPlot({
    #visualiser les valeurs prédit et les vraies valeurs .
    ggplot(data=weather_validation, aes(x=validated_temp_multiple, y=weather_validation$Température)) + 
      geom_point(color="orange") + 
      geom_smooth(method="lm", se=FALSE, color="green") +
      labs(title="Courbe de prédiction", x="Valeurs prédites", y="Vraies valeurs") +
      theme_minimal()+
      coord_cartesian(xlim = c(0, 40), ylim = c(10, 40))
    
    
  })
  # Chargement de dataset
  weather_validation <- read.csv("C:/Users/elgha/OneDrive/Bureau/my data/ESI/cours/cycle_2année/S4/Modèles de régression/Projet/Dataset/2023-2022_Validation_Dataset.csv", sep = ";", dec = ".", encoding = "UTF-8")
  names(weather_validation)<- c("Année","Jour","Date","Température","Humidité","Précipitation","Vitesse_Vent")
  # Utilisation du modèle
  validated_temp_multiple<-predict(model,newdata=weather_validation)
  #perfomances:
  model_errors<- validated_temp_multiple - weather_validation$Température
  mean_model_error <- mean(abs(model_errors))
  # Affichage du dataset dans le box correspondant
  output$validation_data_table <- renderDataTable({
    weather_validation
  })
  # Fonction pour générer le résumé du modèle
  output$model_summary <- renderPrint({
    summary(model)
    cat("L'erreur de prédiction moyenne du modèle  est", round(mean_model_error, 2), "\n")
    print(summary(model))
  })
  output$graph<-renderPlot({
    plot(x = predict(model), y = residuals(model), 
         xlab = "Valeurs prédites", ylab = "Résidus",
         main = "Plot des résidus en fonction des valeurs prédites")
    
    
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
