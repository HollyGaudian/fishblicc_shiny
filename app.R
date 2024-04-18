
#
#1) plot length data as bar/histogram done
#2) collect 2 parameters from clicking plot done
#3) plot a normal selectivity curve over the data using the 2 parameter. done

library("here")
library("shiny")
library("ggplot2")
library("stats")
#library("dplyr")
library("shinyjs")
library("shinyalert")
#library("DT")
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
  blicc_ld <- reactiveVal(NULL)  # blicc data list
  gear_names <- reactiveVal(NULL)
  # seln <- reactiveVal(NULL)
  NGears <- reactiveVal(0)
  fsel <- reactiveVal(0)         # Selectivity component functions
  sel_par <- reactiveValues()    # Selectivity parameters by selectivity component
  sel_fun <- reactiveValues()    # Selectivity components by gear
  sel_wts <- reactiveValues()    # Selectivity component weights by gear

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
      fsel(0)
    } else {
      # Need to check data list is correct format blicc_ld
      if ("gname" %in% names(file_data())) {
        blicc_ld(file_data())
        NGears(file_data()$NG)
        gear_names(file_data()$gname)
        fsel(file_data()$fSel)
        for (si in 1:file_data()$NS)
          sel_par[[as.character(si)]] <- with(file_data(), exp(polSm[sp_i[si]:sp_e[si]]))
        for (gi in 1:file_data()$NG) {
          sf <- file_data()$GSbase[gi]
          sw <- 1
          si <- (gi-1L)*2L+1L
          if (file_data()$GSmix1[si] > 0) {
            sf <- with(file_data(), c(sf, GSmix2[GSmix1[si]:GSmix1[si+1L]]))
            sw <- with(file_data(), c(sw, exp(polSm[NP+GSmix1[si]:GSmix1[si+1L]])))
            }
          # names(sf) <- as.character(sf)
          # names(sw) <- as.character(sf)
          sel_fun[[gear_names()[gi]]] <- sf
          sel_wts[[gear_names()[gi]]] <- sw
        }
        updateNumericInput(session, "NoofSel", value = file_data()$NS)
        updateRadioButtons(session, "Gear_N", label = "Current Gear:",
          choices = file_data()$gname,
          selected = file_data()$gname[1],
          inline = TRUE
        )
      }
    }
  })



  # Update Gear -----------------------------------------------------------

  Cur_Gear <- reactive({
    return(match(input$Gear_N, gear_names()))
  })

  # Change current gear
  observe({
    req(input$Gear_N, blicc_ld())
    sel_comp <- sel_fun[[input$Gear_N]] #get_sel_indices(blicc_ld(), input$Gear_N)
    updateCheckboxGroupInput(session, "Select",
                             choices = as.character(1:NoofSelComp()),
                             selected = sel_comp,
                             inline = TRUE)
    updateRadioButtons(session, "Sel_N",
                       choices = as.character(sel_comp),
                       selected = sel_comp[1],
                       inline = TRUE)
    }) |>
  bindEvent(input$Gear_N)

  ## Selectivity components ----

  NoofSelComp <- reactive({
    return(input$NoofSel)
  })

  # The selectivity function being edited
  Cur_Sel <- reactive({
    req(input$Sel_N)
    return(as.integer(input$Sel_N))
  })

  # The component selectivity index in the current gear
  Cur_SelComp <- reactive({
    req(input$Gear_N, input$Sel_N)
    sc <- which(sel_fun[[input$Gear_N]]==as.integer(input$Sel_N))
    if (length(sc)==0) sc <- 1  # for when input$Gear_N, input$Sel_N are not synchronised
    return(sc) #find gear selectivity component
  })

  observe({
    req(Cur_Sel())
    updateSelectInput(session,
                      "fun_type",
                      label = "Selected Type",
                      choices = as.list(select_types),
                      selected = select_types[fsel()[Cur_Sel()]])
    updateSliderInput(session,
          inputId = "peak_slider",
          value = sel_par[[as.character(Cur_Sel())]][1]
    )
    updateSliderInput(session,
          inputId = "slope_slider1",
          value = 1/sel_par[[as.character(Cur_Sel())]][2]
    )
    if (fsel()[Cur_Sel()]==4) {
      updateSliderInput(session,
          inputId = "slope_slider2",
          value = 1/sel_par[[as.character(Cur_Sel())]][3]
        )
    }
    if (Cur_SelComp() > 1)
      updateSliderInput(session,
                        inputId = "weight",
                        value = sel_wts[[input$Gear_N]][Cur_SelComp()])
  }) |>
  bindEvent(input$Sel_N)


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

# select_param ------------------------------------------------------------

  # This needs to change every time the selectivity function changes
  output$select_param <- renderUI({
    req(blicc_ld(), Cur_Sel(), input$Gear_N, Cur_SelComp())
    min_length <- blicc_ld()$LMP[1]
    mid_length <- blicc_ld()$LMP[blicc_ld()$NB %/% 2]
    max_length <- blicc_ld()$LMP[blicc_ld()$NB]
    sliders <- list(
      sliderInput(
        "peak_slider",
        "Location",
        min = min_length,
        max = max_length,
        value = mid_length
      ),
      sliderInput(
        "slope_slider1",
        "Left Slope:",
        min = 0,
        max = max_length / 4,
        value = max_length / 10
      )
    )

    sl <- 3
    if (fsel()[Cur_Sel()]==4) {
      sliders[[3]] <-
        sliderInput(
          "slope_slider2",
          "Right Slope:",
          min = 0,
          max = max_length / 4,
          value = max_length / 10
        )
      sl <- 4
    }
    if (Cur_SelComp() > 1)
      sliders[[sl]] <- sliderInput("weight", "Mix Weight",
                  min = 0, max = 1, value = sel_wts[[input$Gear_N]][Cur_SelComp()])
    return(sliders)
  })
<<<<<<< HEAD


=======
#
#
>>>>>>> 54b599bed965583d6dad7fd1daf352078dad8469
#
# # plotting functions ---------------------------------------------------------------
#
#   # Define the reactive for frequency data
  observe_df <- reactive({
    req(blicc_ld(), input$Gear_N)
    data.frame(len = blicc_ld()$LMP, fq = blicc_ld()$fq[[input$Gear_N]])
  })

  # Define the reactive for the mortality
  mort_ls <- reactive({
    req(blicc_ld(), Cur_Sel(), Cur_Gear())
    mortality(blicc_ld(), Cur_Sel(), Cur_Gear())
  })


  # Define the reactive for expected frequency
  expect_df <- reactive({
    req(blicc_ld(), input$Gear_N, input$peak_slider, input$slope_slider1)
    NSample <- sum(blicc_ld()$fq[[input$Gear_N]])
    spar <- c(input$peak_slider, 1/input$slope_slider1)
    expect_freq(blicc_ld(), mort_ls(), spar, NSample)
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

