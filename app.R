
#
#1) plot length data as bar/histogram done
#2) collect 2 parameters from clicking plot done
#3) plot a normal selectivity curve over the data using the 2 parameter. done

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

# The following are functions to help manipulate the fishblicc data object

#' Get selectivity function definitions for a single gear in a list
#'
get_selectivity <- function(ld, gear) {
  sel <- list()
  sel[[1]] <- ld$fq[[gear]]
  sel[[2]] <- with(ld, list(
    si = GSbase[gear],
    wt = 1,
    selfun = fSel[GSbase[gear]],
    par = exp(polSm[sp_i[GSbase[gear]]:sp_e[GSbase[gear]]])
  ))
  i <- 3
  si <- 2L*gear-1L
  if (ld$GSmix1[si] > 0) {
    for (gi in ld$GSmix1[si]:ld$GSmix1[si+1L]) {
      sel[[i]] <- with(ld,
                       list(
                     si = GSmix2[gi],
                     wt = with(ld, exp(polSm[NP + gi])),
                     selfun = with(ld, fSel[GSmix2[gi]]),
                     par = with(ld, exp(polSm[sp_i[GSmix2[gi]]:sp_e[GSmix2[gi]]]))
                     ))
      i <- i + 1
    }
  }
  return(sel)
}

#' Set selectivity function definitions for a single gear in a list
#'
set_selectivity <- function(ld, gear, sel) {
  # This is a complicated function that will need to rework the data object
  # because the links between the selectivity functions and the functions
  # themselves may have changed
  sel_ind <- integer(length(sel)-1)
  wt <- double(length(sel)-1)
  selfun <- sel_ind

  for (i in 2:length(sel)) {
    sel_ind[i-1] <- sel[[i]]$si
    wt[i-1] <- sel[[i]]$wt
    selfun[i-1] <- sel[[i]]$selfun
  }
  ld <- blicc_selfun(ld, sel_ind, selfun)
  ld <- blicc_gear_sel(ld, gear_sel = list(sel_ind), gear)
  if (length(sel_ind) > 1) ld <- blip_mix(ld, gear, mix_wt = wt)
  for (i in 2:length(sel)) {
    si <- sel[[i]]$si
    ld$polSm[ld$sp_i[si]:ld$sp_e[si]] <- log(sel[[i]]$par)
  }

  return(ld)
}

#' Number of selectivities associated with a gear
#'
NoofSelect <- function(ld, gear) {
  si <- 2L*gear-1L
  if (ld$GSmix1[si] > 0) {
    N <- 2L + ld$GSmix1[si] - ld$GSmix1[si+1L]
  } else {
    N <- 1L
  }
  return(N)
}

#' Get model function index from a slider reference
#'
SelectivityN <- function(ld, gear, N) {
  si <- 2L*gear-1L
  if (N==1 | ld$GSmix1[si] == 0) {
    return(ld$GSbase[gear])
  } else {
    mix_sel <-  (blicc_ld$GSmix1[si]:blicc_ld$GSmix1[si+1L])[N-1L]
    return(blicc_ld$GSmix2[mix_sel])
  }
}

#' Possible selectivity models
#'
select_types <- c("Logistic", "Normal",
"Single-side Normal", "Double-sided Normal")

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
  load(here("data", "yft_ld.rda"))
  ld <- yft_ld

  output$Gears <- renderUI({
    selectInput("Gear",
                label = "Gear",
                choices = as.list(ld$gname),
                selected = ld$gname[1])
  })

  output$select_param <- renderUI({
   req(input$Gear)
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



  # Define the initial reactiveVal for data
  dataplot <- reactive({
    req(input$Gear)
    NGear <- match(input$Gear, ld$gname)
    data.frame(len = ld$LMP, fq = ld$fq[[NGear]])
  })



  ## 2. Create a plot ##
  output$plot1 <- renderPlot({
    req(input$Gear)
    NGear <- match(input$Gear, ld$gname)
    mu <- input$peak_slider
    sigma <- input$slope_slider1
    weight <- max(ld$fq[[NGear]])*input$weight
    curve_data <- data.frame(x = ld$LMP,
                             y = exp(-0.5*((ld$LMP-mu)/sigma)^2)*weight)

    req(input$plot_btn)
    print("Rendering plot...")

    ggplot(dataplot(), aes(x = len, y = fq)) +
      geom_bar(stat = "identity",
               fill = "#0f2667",
               alpha = 0.7) +
      geom_line(data = curve_data, aes(x = x, y = y), color = "#740404") +
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
        axis.ticks = element_line(color = "black", size = 1),
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
