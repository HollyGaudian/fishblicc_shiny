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





  dl_sens <-
    blicc_dat(
    gear_sel = ,
    sel_fun = 1,
    LLB = data$len,
    fq = data$fq,
    Linf = c(priorData$vonBLinf, 2),
    L95 = priorData$Lmat+5,
    L50 = priorData$Lmat,
    gear_names = c("Marine set bagnet")
    )

  prior_plot <- reactive({
    plot_prior(dl_sens)})



# Downloadable file ----
  observeEvent(input$save, {
    if (nrow(click_data) >= 2) {
      saved_curves <- tail(click_data, 2)
      saveRDS(saved_curves, file = "tmp.rda")

      shinyalert(
        title = "Saved Last Two Values",
        text = paste("Saved values: \n", paste("X:", saved_values$x, "Y:", saved_values$y, sep = ", ")),
        type = "success"
      )
    } else {
      shinyalert(
        title = "Error",
        text = "Not enough data points to save.",
        type = "error"
      )
    }
  })

  