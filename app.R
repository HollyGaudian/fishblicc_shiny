
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



# Generate tabs -----------------------------------------------------------

generate_tab_layout <- function(tab_index) {
  tabPanel(
    paste("Tab", tab_index),
    sidebarPanel(
      chooseSliderSkin(
        skin = "Flat",
        color = "#941414"),
      numericInput("NoofMix",
                   label = "Number of Selectivities:",
                   min = 1,
                   max = 7,
                   value = 1),
      numericInput("SelN",
                   label = "Current Selectivity:",
                   min = 1,
                   max = 7,  # Placeholder, will be updated dynamically
                   value = 1),
      selectInput("fun_type",
                  label = "Selected Type",
                  choices = as.list(select_types),
                  selected = select_types[2]), #   select_types[data$fSel[data$GSbase[1]]]),
      sliderInput("weight",
                  "Weight",
                  min = 0, max = 1, value = 1),
      actionButton("plot_btn", "Plot Selectivity"),
    mainPanel(h3("Plot"),
              plotOutput(paste0("plot1", tab_index))
    )

    )
  )
}


# UI ----------------------------------------------------------------------

ui <- fluidPage(
  theme = shinytheme("darkly"),
  titlePanel("Selectivity"),
  sliderInput("num_tabs", "Number of gears:", min = 1, max = 7, value = 3),
  fileInput("load", label = "Load Model Data (.rda format)", accept = c(".rda")),
  uiOutput("dynamic_tabs"),
  uiOutput("gear"),
  uiOutput("seln_input"),
  uiOutput("select_param")
  )



server <- function(input, output, session) {

  shinyjs::enable()

  data <- reactiveVal(NULL)
  gears <- reactiveVal(NULL)
  seln <- reactiveVal(NULL)



# Selectivity bars --------------------------------------------------------
  # Reactive expression to ensure SelN does not exceed NoofMix
  max_sel_n <- reactive({
    req(input$NoofMix)
    max(input$SelN, input$NoofMix)
  })

  # Update SelN based on NoofMix
  observe({
    updateNumericInput(session, "SelN", max = max_sel_n())
  })

# Data Loading ------------------------------------------------------------

  loaded_data <- reactive({
    req(input$load)
    inFile <- input$load
    if (is.null(inFile))
      return(NULL)

    # Attempt to load the file with error handling
    loaded_data <- tryCatch(
      {
        load(inFile$datapath, envir=temp)
        return(get(ls(temp)[1], envir=temp))  # Replace "yft_ld" with the correct name of the loaded data object
      },
      error = function(e) {
        message("Error loading file: ", e$message)
        return(NULL)
      }
    )
    return(loaded_data)
  })

  observe({
    req(loaded_data())

    str(loaded_data())

    # Check if the loaded object contains the 'gname' column
    if (!is.null(loaded_data()) && "gname" %in% names(loaded_data())) {
      data(loaded_data())
      gears(unique(loaded_data()$gname))
    } else {
      # If 'gname' column does not exist, show a warning and assign NULL to gears
      warning("Column 'gname' not found in the loaded data.")
      gears(NULL)
    }
  })


# gname -------------------------------------------------------------------

  #output$gear <- renderUI({
    #req(gears())
    #if (!is.null(gears())) {
      #selectInput("gears_select", "Select Gear:", choices = gears(), selectize = FALSE)
    #} else {
      #HTML("No 'gname' column found in the loaded data.")
    #}
  #})

  # Render dropdown menu with options from "gname" column
  output$gear <- renderUI({
    req(data())
    selectInput("gear_select", "Select gear:",
                choices = unique(data()$gname))
  })
# Tabs Reactive--------------------------------------------------------------------


  tab_layouts <- reactive({
    num_tabs <- input$num_tabs

    tabs <- lapply(1:num_tabs, generate_tab_layout)

    do.call(tabsetPanel, tabs)
  })


  output$dynamic_tabs <- renderUI({
    tab_layouts()
  })

  observeEvent(input$num_tabs, {
    num_tabs <- input$num_tabs

    lapply(1:num_tabs, function(i) {
      output[[paste0("plot1", i)]] <- renderPlot({
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

      })

# select_param ------------------------------------------------------------

  NGears <- reactiveVal(
    value=4
    #match(input$Gear, data$gname)
    )

  output$select_param <- renderUI({
   req(input$Gear, input$SelN)
   NGear <- match(input$Gear, data$gname)
   NSelect <- SelectivityN(data, NGear, input$SelN)
   if (data$fSel[NSelect]==4) {
    list(
      sliderInput(
        "peak_slider",
        "Location",
        min = data$LMP[1],
        max = data$LMP[data$NB],
        value = data$LMP[data$NB %/% 2]
      ),
      sliderInput(
          "slope_slider1",
          "Left Slope:",
          min = 0,
          max = data$LMP[data$NB] / 4,
          value = data$LMP[data$NB] / 10
        ),
        sliderInput(
          "slope_slider2",
          "Right Slope:",
          min = 0,
          max = data$LMP[data$NB] / 4,
          value = data$LMP[data$NB] / 10
        )
    )
    } else {
      list(
        sliderInput(
          "peak_slider",
          "Location",
          min = data$LMP[1],
          max = data$LMP[data$NB],
          value = data$LMP[data$NB %/% 2]
        ),
        sliderInput(
        "slope_slider1",
        "Slope:",
        min = 0,
        max = data$LMP[data$NB] / 4,
        value = data$LMP[data$NB] / 10
        )
    )}
    })



# plotting functions ---------------------------------------------------------------

  # Define the reactive for frequency data
  observe_df <- reactive({
    req(input$Gear)
    NGear <- match(input$Gear, data$gname)
    data.frame(len = data$LMP, fq = data$fq[[NGear]])
  })

  # Define the reactive for expected frequency
  expect_df <- reactive({
    req(input$Gear, input$SelN, input$peak_slider, input$slope_slider1)
    NGear <- match(input$Gear, data$gname)
    NSel <- SelectivityN(data, NGear, input$SelN)
    NSample <- sum(data$fq[[input$Gear]])
    mort <- mortality(data, NSel, NGear)
    spar <- c(input$peak_slider, input$slope_slider1)
    expect <- expect_freq(data, mort, spar, NSample)
  })

  # Define the reactive for the mortality
  mort_ls <- reactive({
    req(input$Gear, input$SelN)
    NGear <- match(input$Gear, data$gname)
    NSel <- SelectivityN(data, NGear, input$SelN)
    data$fSel[NSel] <- match(input$fun_type, select_types)
    mortality(data, NSel, NGear)
  })


  # Define the reactive for expected frequency
  expect_df <- reactive({
    req(input$Gear, input$peak_slider, input$slope_slider1)
    NSample <- sum(data$fq[[input$Gear]])
    spar <- c(input$peak_slider, 1/input$slope_slider1)
    expect_freq(data, mort_ls(), spar, NSample)
  })


# slider edit -------------------------------------------------------------
  shinyjs::runjs(
    '$("#peak_slider").css("background-color", "#941414"); $("#slope_slider").css("background-color", "#941414");'
  )

  })
}

shinyApp(ui = ui, server = server)



# delete at end -----------------------------------------------------------

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
#         min = data$LMP[1],
#         max = data$LMP[data$NB],
#         value = data$LMP[data$NB %/% 2]
#       ),
#       sliderInput(
#         "slope_slider",
#         "Prior Slope:",
#         min = 0,
#         max = data$LMP[data$NB] / 4,
#         value = data$LMP[data$NB] / 10
#       ),
#     )
#   })
#   do.call(tagList, selectivity_options)
