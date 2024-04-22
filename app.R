
#
#1) Add plot panel for plot_prior function for all gears with plot button
#2) Separate current selectivity parameters and weight
#3) Add edits to blicc_ld() data reactive on Gear_N or Sel_N change, or save option
#4) Add save option to data object

library("here")
library("shiny")
library("ggplot2")
library("stats")
library("shinyjs")
library("shinyalert")
library("shinythemes")
library("shinyWidgets")
library("jsonlite")

source(here("R", "fishblicc_shiny_functions.R"))


# UI ----------------------------------------------------------------------

ui <- fluidPage(
  theme = shinytheme("darkly"),
  titlePanel("Selectivity"),
  sidebarLayout(
    sidebarPanel(
    # File input
      fileInput("load", label = "Load Model Data (.rda format)",
                accept = c(".rda")),
    # Gears and selectivity components
      numericInput("NoofSel",
                   label = "Number of Selectivity Components:",
                   min = 0,
                   max = 7,
                   value = 0),
      radioButtons(
        "Gear_N",
        label = "Current Gear:",
        choices = "None",
        selected = NULL,
        inline = TRUE,
        width = NULL
      ),
      checkboxGroupInput("Select",
                         label = "Selectivity Components:",
                         choices = "None",
                         selected = NULL,
                         inline = TRUE,
                         width = NULL
      ),
      radioButtons(
        "Sel_N",
        label = "Current Selectivity:",
        choices = "None",
        selected = NULL,
        inline = TRUE,
        width = NULL
      ),
      selectInput("fun_type",
                  label = "Selected Type",
                  choices = as.list(select_types),
                  selected = select_types[1]),
      chooseSliderSkin(
        skin = "Flat",
        color = "#941414"),
      uiOutput("select_param")
    ),
    # plot selectivity
   mainPanel(h3("Selectivity"),
             plotOutput("plot1")
   ),
  )
  #uiOutput("dynamic_tabs"),
  # uiOutput("gear"),
  # uiOutput("seln_input"),
)



server <- function(input, output, session) {

  shinyjs::enable()

# Declarations ------------------------------------------------------------
  # These can be loaded but not edited (except blicc_ld data set)
  blicc_ld <- reactiveVal(NULL)  # blicc data list
  gear_names <- reactiveVal(NULL)
  NGears <- reactiveVal(0)

# Data Loading ------------------------------------------------------------

  file_data <- reactive({
    req(input$load)
    inFile <- input$load
    if (is.null(inFile))
      return(NULL)

    # Attempt to load the file with error handling
    tryCatch(
      {
        tmp <- new.env()
        load(inFile$datapath, envir=tmp)
        return(get(ls(envir=tmp)[1], envir=tmp))
      },
      error = function(e) {
        message("Error loading file: ", e$message)
        return(NULL)
      }
    )
  })

  # Update everything based on the loaded data
  observe({
    req(file_data())

    if (is.null(file_data())) {
      NGears(0)
      gear_names(NULL)
      updateNumericInput(session, "NoofSel", value = 0)
    } else {
      # Need to check data list is correct format blicc_ld
      if ("gname" %in% names(file_data())) {
        blicc_ld(file_data())
        NGears(file_data()$NG)
        gear_names(file_data()$gname)
        updateNumericInput(session, "NoofSel", value = file_data()$NS)
        updateRadioButtons(session, "Gear_N", label = "Current Gear:",
          choices = file_data()$gname,
          selected = file_data()$gname[1],
          inline = TRUE
        )
      }
    }
  })


  # Update Gear & Selectivity ------------------------------------------------
  # Includes updating blicc_ld() with new values
  # Change current gear
  observe({
    req(Cur_Gear(), blicc_ld())
    updateCheckboxGroupInput(session, "Select",
                             choices = as.character(1:NoofSelComp()),
                             selected = Cur_SelComp(),
                             inline = TRUE)
    updateRadioButtons(session, "Sel_N",
                       choices = as.character(Cur_SelComp()),
                       selected = Cur_SelComp()[1],
                       inline = TRUE)
    }) |>
  bindEvent(input$Gear_N)

  # Change current selectivity component
  observe({
    req(Cur_Sel(), blicc_ld())
    updateSelectInput(session, "fun_type",
                selected = select_types[blicc_ld()$fSel[Cur_Sel()]])
  }) |>
  bindEvent(input$Sel_N)


  # Reactives for gear and selectivity components --------------------------

  # gear index
  Cur_Gear <- reactive({
    return(match(input$Gear_N, gear_names()))
  })

  NoofSelComp <- reactive({
    return(input$NoofSel)
  })

  # The selectivity function being edited
  Cur_Sel <- reactive({
    req(input$Sel_N)
    return(as.integer(input$Sel_N))
  })

  Cur_SelComp <- reactive({
    req(blicc_ld(), Cur_Gear())
    return(get_sel_fun(blicc_ld(), Cur_Gear())) #get_sel_indices(blicc_ld(), input$Gear_N)
  })

  # The component selectivity index in the current gear
  Cur_SelCompIndex <- reactive({
    req(Cur_SelComp(), input$Gear_N, input$Sel_N)
    sc <- which(Cur_SelComp()==as.integer(input$Sel_N))
    if (length(sc)==0) sc <- 1  # for when input$Gear_N, input$Sel_N are not synchronised
    return(sc) #find gear selectivity component for current gear
  })

  # vector of selectivity parameters
  Cur_SelPar <- reactive({
    req(blicc_ld(), Cur_Sel())
    return(get_sel_par(blicc_ld(), Cur_Sel()))
  })

  # Current weight parameters
  Cur_SelWt <- reactive({
    req(blicc_ld(), Cur_Gear(), Cur_Sel())
    return(get_sel_wt(blicc_ld(), Cur_Gear(), Cur_Sel()))
  })

  Cur_SelFun <- reactive({
    req(input$fun_type)
    return(match(input$fun_type, select_types))
  })

# Select Parameters---------------------------------------------------------

  # This needs to change every time the selectivity function changes
  output$select_param <- renderUI({
    req(blicc_ld(), Cur_Sel(), input$Gear_N, Cur_SelCompIndex())
    min_length <- blicc_ld()$LMP[1]
    mid_length <- blicc_ld()$LMP[blicc_ld()$NB %/% 2]
    max_length <- blicc_ld()$LMP[blicc_ld()$NB]
    sel_par <- exp(Cur_SelPar())

    sliders <- list(
      sliderInput(
        "peak_slider",
        "Location",
        min = min_length,
        max = max_length,
        value = sel_par[1]
      ),
      sliderInput(
        "slope_slider1",
        "Left Slope:",
        min = 0,
        max = max_length,
        value = 1/sel_par[2]
      )
    )

    sl <- 3
    if (blicc_ld()$fSel[Cur_Sel()]==4) {
      sliders[[3]] <-
        sliderInput(
          "slope_slider2",
          "Right Slope:",
          min = 0,
          max = max_length,
          value = 1/sel_par[3]
        )
      sl <- 4
    }
    if (Cur_SelCompIndex() > 1)
      sliders[[sl]] <- sliderInput("weight_slider", "Mix Weight",
                  min = 0, max = 1, value = exp(Cur_SelWt()))
    return(sliders)
  })


#
# # plotting functions ---------------------------------------------------------------
#


  observe({
    output$plot1 <- renderPlot({
      req(observe_df(), expect_df())

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



  # Define the reactive for frequency data
  observe_df <- reactive({
    req(blicc_ld(), input$Gear_N)
    return(data.frame(len = blicc_ld()$LMP, fq = blicc_ld()$fq[[input$Gear_N]]))
  })

  # Define the reactive for the mortality
  mort_ls <- reactive({
    req(blicc_ld(), Cur_Sel(), Cur_Gear())
    return(mortality(blicc_ld(), Cur_Sel(), Cur_Gear()))
  })


  # Define the reactive for expected frequency
  expect_df <- reactive({
    req(blicc_ld(), Cur_Gear(), Cur_SelFun(), Cur_SelCompIndex(),
        input$peak_slider, input$slope_slider1)
    NSample <- sum(blicc_ld()$fq[[Cur_Gear()]])
    if (Cur_SelFun()==4)
      spar <- c(input$peak_slider, 1/input$slope_slider1, 1/input$slope_slider2)
    else
      spar <- c(input$peak_slider, 1/input$slope_slider1)
    if (Cur_SelCompIndex() > 1)
      wt <- input$weight_slider
    else
      wt <- 1
    return(expect_freq(blicc_ld(), mort_ls(), spar,
                       wt, NSample))
  })


# slider edit -------------------------------------------------------------
shinyjs::runjs(
  '$("#peak_slider").css("background-color", "#941414"); $("#slope_slider").css("background-color", "#941414");'
  )

}





# run app -------------------------------------------------------------

shinyApp(ui = ui, server = server)



# Tabs --------------------------------------------------------------------


# generate_tab_layout <- function(tab_index, gear_name) {
#   tabPanel(
#     title=gear_name,
#     value=tab_index,
#     sidebarPanel(
#       chooseSliderSkin(
#         skin = "Flat",
#         color = "#941414"),
#       checkboxGroupInput("Select",
#                          label = "Selectivity Components:",
#                          choices = as.character(1:7),
#                          selected = NULL,
#                          inline = TRUE,
#                          width = NULL
#       ),
#       radioButtons(
#         "Sel_N",
#         label = "Current Selectivity:",
#         choices = as.character(1:7),
#         selected = NULL,
#         inline = TRUE,
#         width = NULL
#       ),
#       selectInput("fun_type",
#                   label = "Selected Type",
#                   choices = as.list(select_types),
#                   selected = select_types[2]), #   select_types[data$fSel[data$GSbase[1]]]),
#       sliderInput("weight",
#                   "Weight",
#                   min = 0, max = 1, value = 1),
#       actionButton("plot_btn", "Plot Selectivity") #,
#       # mainPanel(h3("Plot"),
#       #           plotOutput(paste0("plot1", tab_index))
#       # )
#
#     )
#   )
# }



# gear_tabs <- reactive({
#   tabs <- list()
#   if (NGears()>0) {
#     for (i in 1:NGears())
#       tabs[[i]] <- generate_tab_layout(i, gear_names()[i])
#     #tabs <- lapply(1:NGears(), )
#
#     do.call(tabsetPanel, tabs)
#   }
# })
#
#
# output$dynamic_tabs <- renderUI({
#   gear_tabs()
# })
#
# observeEvent(input$num_tabs, {
#   updateCheckboxGroupInput(session, "Select",
#                            choices = as.character(1:NoofSelComp()),
#                            selected = get_sel_indices(blicc_ld(), input$num_tabs),
#                            inline = TRUE)
#
# })

