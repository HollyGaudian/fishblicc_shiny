library("shiny")


ui <- fluidPage(
  sliderInput(inputId = "locationSlider",
              label = "Location",
              min = 0, #min length
              max = 100, #max length
              value = 50),
  radioButtons(inputId = "dist", 
               label = "Distribution type:",
               choices = c("Normal" = "norm",
                           "Uniform" = "unif",
                           "Log-normal" = "lnorm",
                           "Exponential" = "exp")) ,
  plotOutput("distPlot")
)

server <- function(input, output){
  output$distPlot <- renderPlot({
    dist <- switch(input$dist,
                   norm = rnorm,
                   unif = runif,
                   lnorm = rlnorm,
                   exp = rexp,
                   rnorm)
    
    hist(dist(500))
  })
}

shinyApp(ui, server)

  