
#
#1) plot length data as bar/histogram done
#2) collect 2 parameters from clicking plot done
#3) plot a normal selectivity curve over the data using the 2 parameter. done

library("here")
library("shiny")
library("ggplot2")
library("stats")
library("dplyr")
library("shinyjs")
library("shinyalert")
library("DT")
library("shinythemes")
library("shinyWidgets")
library("jsonlite")

source(here("R", "fishblicc_shiny_functions.R"))
# User Interface

ui <- fluidPage(
  theme = shinytheme("darkly"),
  titlePanel("Set Prior Selectivity"),
  sidebarLayout(
    sidebarPanel(
      chooseSliderSkin(
        skin = "Flat",
        color = "#941414"),
      actionButton("load", label = "Load Model"),
      uiOutput("Gears"),
      numericInput("NoofMix",
                   label = "Number of Selectivities:",
                   min = 1,
                   max = 3,
                   value = 1),
      numericInput("SelN",
                   label = "Current Selectivity:",
                   min = 1,
                   max = 3,
                   value = 1),
      selectInput("fun_type",
                  label = "Selected Type",
                  choices = as.list(select_types),
                  selected = select_types[2]), #   select_types[ld$fSel[ld$GSbase[1]]]),
      sliderInput("weight",
                  "Weight",
                  min = 0, max = 1, value = 1),
      uiOutput("select_param"),
      actionButton("plot_btn", "Plot Selectivity"),
      actionButton("save", label = "Save Parameters")
    ),
    mainPanel(h2("Selectivities"),
              fluidRow(
                column(
                  width = 9,
                  plotOutput("plot1", click = "plot_click")
                )
              ))
  )
)


server <- function(input, output, session) {

  shinyjs::enable()
  load(here("data", "yft_ld.rda"))  # temporary load
  ld <- yft_ld

  output$Gears <- renderUI({
    selectInput("Gear",
                label = "Gear",
                choices = as.list(ld$gname),
                selected = ld$gname[1])
  })

  NGears <- reactiveVal(
    value=4
    #match(input$Gear, ld$gname)
    )

  output$select_param <- renderUI({
   req(input$Gear, input$SelN)
   NGear <- match(input$Gear, ld$gname)
   NSelect <- SelectivityN(ld, NGear, input$SelN)
   if (ld$fSel[NSelect]==4) {
    list(
      sliderInput(
        "peak_slider",
        "Location",
        min = ld$LMP[1],
        max = ld$LMP[ld$NB],
        value = ld$LMP[ld$NB %/% 2]
      ),
      sliderInput(
          "slope_slider1",
          "Left Slope:",
          min = 0,
          max = ld$LMP[ld$NB] / 4,
          value = ld$LMP[ld$NB] / 10
        ),
        sliderInput(
          "slope_slider2",
          "Right Slope:",
          min = 0,
          max = ld$LMP[ld$NB] / 4,
          value = ld$LMP[ld$NB] / 10
        )
    )
    } else {
      list(
        sliderInput(
          "peak_slider",
          "Location",
          min = ld$LMP[1],
          max = ld$LMP[ld$NB],
          value = ld$LMP[ld$NB %/% 2]
        ),
        sliderInput(
        "slope_slider1",
        "Slope:",
        min = 0,
        max = ld$LMP[ld$NB] / 4,
        value = ld$LMP[ld$NB] / 10
        )
    )}
    })



  # Define the reactive for frequency data
  observe_df <- reactive({
    req(input$Gear)
    NGear <- match(input$Gear, ld$gname)
    data.frame(len = ld$LMP, fq = ld$fq[[NGear]])
  })

  # Define the reactive for expected frequency
  expect_df <- reactive({
    req(input$Gear, input$SelN, input$peak_slider, input$slope_slider1)
    NGear <- match(input$Gear, ld$gname)
    NSel <- SelectivityN(ld, NGear, input$SelN)
    NSample <- sum(ld$fq[[input$Gear]])
    mort <- mortality(ld, NSel, NGear)
    spar <- c(input$peak_slider, input$slope_slider1)
    expect <- expect_freq(ld, mort, spar, NSample)
  })


  ## 2. Create a plot ##
  output$plot1 <- renderPlot({
    req(input$plot_btn)

    ggplot(observe_df(), aes(x = len, y = fq)) +
      geom_bar(stat = "identity",
               fill = "#0f2667",
               alpha = 0.7) +
      geom_line(data = expect_df(), aes(x = LMP, y = Freq, color = Selectivity)) +
      labs(x = "Length", y = "Frequency") +
      theme_minimal() +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text = element_text(color = "black", size = 15),
        # Adjust size as needed
        axis.ticks = element_line(color = "black", linewidth = 1),
        axis.title = element_text(color = "black", size = 15)
      )
  })


  # observe save button
  observeEvent(input$save, {
    isolate(
      params <- c(
        peak = input$peak_slider,
        slope = input$slope_slider
      ))
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

# Code to generate multiple sliders dependent on other input

#
# output$selectivity_options <- renderUI({
#   selectivity_options <- lapply(1:NSelect(), function(i) {
#     tagList(
#       selectInput(paste0("selectivity_function_", i),
#                   label = paste("Selectivity Function", i),
#                   choices = list("None", "Logistic", "Normal"),
#                   selected = "None"),
#       sliderInput(paste0("weight_", i),
#                   "Weight",
#                   min = 0, max = 1, value = 1),
#       sliderInput(
#         "peak_slider",
#         "Prior Mean",
#         min = ld$LMP[1],
#         max = ld$LMP[ld$NB],
#         value = ld$LMP[ld$NB %/% 2]
#       ),
#       sliderInput(
#         "slope_slider",
#         "Prior Slope:",
#         min = 0,
#         max = ld$LMP[ld$NB] / 4,
#         value = ld$LMP[ld$NB] / 10
#       ),
#     )
#   })
#   do.call(tagList, selectivity_options)
