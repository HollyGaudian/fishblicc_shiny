#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#1) plot length data as bar/histogram
#2) collect 2 parameters from clicking plot
#3) plot a normal selectivity curve over the data using the 2 parameter.
#Rinse and repeat.

library("shiny")
library("ggplot2")
library("readxl")
library("stats")
library("here")
library("dplyr")
library("fishblicc")
library("shinyjs")
library("shinyalert")
library("DT")
library("shinythemes")
library("shinyWidgets")
library("jsonlite")



lenfreq <- read.csv(file = here("data/bansis_len_freq_data.csv")) |>
  filter(sciname == "Harpadon nehereus") |>
  mutate(len=floor(length_cm)) |>
  group_by(len) |>
  summarise(fq=sum(frequency)) |>
  ungroup()

data <- data.frame(len = lenfreq$len, fq = lenfreq$fq)
print(data)

initial_peak <- 20
initial_slope <- 10


ui <- fluidPage(
  theme = shinytheme("darkly"),
  titlePanel("Plot Selectivity Curve"),
  sidebarLayout(
    sidebarPanel(
      chooseSliderSkin(
        skin = "Flat",
        color = "#941414"),
      selectInput("selectivity_functions", label = "Selectivity",
                  h4("Select:"), choices = list("None", "Logistic", "Double Normal"),
                  multiple = FALSE),
      actionButton("plot_btn", "Plot Graph"),
      sliderInput("slope_slider",
                  "Prior Slope:",
                  min = 0, #min(data$fq),
                  max = 250, #max(data$fq),
                  value = initial_slope),
      sliderInput("peak_slider",
                  "Prior Mean",
                  min = 0, #min(data$len),
                  max = 30, #max(data$len),
                  value = 12),
      actionButton("save", label = "Save Parameters"),
      actionButton("remove_row", "Remove Last row")
  ),
  mainPanel(
    h2("Plot"),
    fluidRow(
      column(width = 9,
                    h4("Collect Priors"),
                    plotOutput("plot1", click = "plot_click")
             ),
      column(width = 3,
                    h4("Parameter Table"),
                    tableOutput("parameter_table"))
    )
  )
))


server <- function(input, output, session) {

  shinyjs::enable()


#paraData <- read_xlsx(path =here("data/stock_parameters.xlsx"),
                          #sheet = "parms") |>
    #dplyr::select(Species, LWa_pub, LWb_pub, vonBLinf,
                  #vonBLinf_lo, vonBLinf_hi, vonBT0, vonBk, Lmat, Lmax)

#paraData <- paraData |>
    #filter(Species == "Harpadon nehereus")

#params <- reactiveValues(peak = initial_peak, slope = initial_slope)

  observeEvent(input$plot_btn, {
    print("Rendering plot...")
  })

  # Define the initial reactiveVal for data
  dataplot <- reactiveVal(data)


  #selctivity function curve
  add_curve <- function(selectivity, data) {
    if (selectivity == "None") {
      return(NULL)
    } else if (selectivity == "Logistic") {
      # Add logistic curve to plot
      return(geom_smooth(aes(y = stat(count)), method = "glm", stat = "smooth", position = "identity",
                         method.args = list(family = "binomial"), se = FALSE, color = "#941414"))
    } else if (selectivity == "Double Normal") {
      # Add double normal curve to plot (example parameters)
      mu1 <- mean(data$len) - 5
      mu2 <- mean(data$len) + 5
      sigma <- sd(data$len)

      curve_data <- data.frame(
        x = seq(min(data$len), max(data$len), length.out = 100),
        y = dnorm(seq(min(data$len), max(data$len), length.out = 100), mean = mu1, sd = sigma) +
          dnorm(seq(min(data$len), max(data$len), length.out = 100), mean = mu2, sd = sigma)
      )

      return(geom_line(data = curve_data, aes(x = x, y = y), color = "#941414"))
    } else {
      return(NULL)
    }
  }

  # Define reactiveVal for parameter data
  parameter_data <- reactiveVal(data.frame(peak = numeric(), slope = numeric()))


## 2. Create a plot ##
output$plot1 <- renderPlot({
  mu <- input$peak_slider
  sigma <- input$slope_slider


  req(input$plot_btn)
  print("Rendering plot...")

  ggplot(dataplot(), aes(x = len, y = fq)) +
    geom_bar(stat = "identity", fill = "#0f2667", alpha = 0.7) +
    add_curve(input$selectivity_functions, dataplot()) +
    #geom_smooth(method = "glm", family = gaussian(link = "identity"), se = FALSE,
    #formula = y ~ dnorm(x, mean = mu, sd = sigma), color = "#941414", linetype = "solid", size = 1) +
    labs(x = "Length", y = "Frequency") +
    theme_minimal() +
    theme(
      #panel.background = element_rect(fill = "transparent", color = NA),
      #plot.background = element_rect(fill = "transparent", color = NA),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.line = element_line(color = "black"),
      axis.text = element_text(color = "black", size = 15),  # Adjust size as needed
      axis.ticks = element_line(color = "black", size = 1),
      axis.title = element_text(color = "black", size = 15)
    )
})

    # observe save button
    observeEvent(input$save, {
      params <- isolate(c(peak = input$peak_slider, slope = input$slope_slider))
      parameter_data(cbind(parameter_data(), params))
      })


    ## observe remove button ##
    observeEvent(input$remove_row, {
      current_data <- parameter_data()
      if (nrow(current_data) > 0) {
        parameter_data(current_data[-nrow(current_data), ])
      }
    })

    # render parameter table
    output$parameter_table <- renderTable({
      parameter_data()
    })

    shinyjs::runjs(
      '$("#peak_slider").css("background-color", "#941414"); $("#slope_slider").css("background-color", "#941414");'
    )
}

shinyApp(ui = ui, server = server)


  # Table to display click coordinates
  #output$parameter_table <- renderTable({
    #parameter_data()
  #})

  # Button to save the latest parameters
  #observeEvent(input$save, {
    #new_row <- data.frame(
      #Peak_X = input$peak_slider,
      #Slope = input$slope_slider
    #)
    #parameter_data(rbind(parameter_data(), new_row))
  #})



    #each input is a factor so levels are consistent for plotting characteristics
  #click_x <- data.frame(x = input$plot_click$x,
  #y = input$plot_click$y,
  #selfun = factor(input$selfun,
  #levels = c("Logistic", "Normal", "Single Normal", "Double Normal")
  #),
  #row.names = NULL,
  #stringsAsFactors = FALSE
  #)
  # add row to the data.frame
  #values$DT <- rbind(values$DT, add_row)


  # function to add selected curve to plot

  #add_curve <- function(selectivity, data) {
    #if(selectivity == "None") {
       # return(NULL)
      #} else if (selectivity == "Logistic") {
        #return(geom_smooth(method = "glm", stat = "smooth", position = "identity", method.args = list(family = "binomial"), se = FALSE, color = "red"))
      #} else if (selectivity == "Normal") {
        #return(geom_density(aes(y = after_stat(y)), color = "red"))
      #} else if (selectivity == "Single Normal") {
       # mu <- mean(data$len)
        #sigma <- sd(data$len)
        #return(geom_function(fun = dnorm, args = list(mean = mu, sd = sigma), aes(y = after_stat(y)), color = "red"))
      #} else if (selectivity == "Double Normal") {
        #mu1 <- mean(data$len) - 5
        #mu2 <- mean(data$len) + 5
        #sigma <- sd(data$len)
        #return(
          #geom_function(fun = dnorm, args = list(mean = mu1, sd = sigma), aes(y = after_stat(y)), color = "red") +
          #geom_function(fun = dnorm, args = list(mean = mu2, sd = sigma), aes(y = after_stat(y)), color = "red")
      #)
      #} else {
       # return(NULL)
      #}
    #}




  ## 4. remove row on actionButton click ##
  #observeEvent(input$rem_point, {
    #rem_row <- values$DT[-nrow(values$DT), ]
    #values$DT <- rem_row
  #})



  #isolate(reactiveValuesToList(values))



  #observeEvent(input$save_button, {
    #current_priors <- bind_rows(priors(), data.frame(peak = input$peak, Slope = input$slope))
    #priors(current_priors)
  #})

#display on table
  #output$priors_table <- renderTable({
    #priors()
  #})
