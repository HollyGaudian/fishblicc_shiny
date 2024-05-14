
#
#1) Add plot panel for plot_prior function for all gears with plot button
#2) Separate current selectivity parameters and weight
#3) Add edits to blicc_ld() data reactive on Gear_N or Sel_N change, or save option
#4) Add save option to data object

library("here")
library("shiny")
library("ggplot2")
library("stats")
library("bslib")
library("shinyjs")
library("shinyalert")
library("shinythemes")
library("shinyWidgets")
library("jsonlite")
library("fishblicc")

source(here("R", "fishblicc_shiny_functions.R"))


# UI ----------------------------------------------------------------------

ui <- fluidPage(
  theme = shinytheme("darkly"),
  titlePanel("fishblicc Selectivity v0.5"),
  navset_card_tab(

    nav_panel("Data",
              fileInput("load", label = "Load Data (.rda/.csv format)",
                                       accept = c(".rda", ".csv")),
              numericInput("NoofSel",
                            label = "Number of Selectivity Components:",
                            min = 0,
                            max = 7,
                            value = 0),
              numericInput("Linf",
                           label = "Linf:",
                           min = 0,
                           max = 200,
                           value = 0),
              numericInput("M/K",
                           label = "M/K",
                           min = 0,
                           value = 0),
              numericInput("Galpha",
                           label = "Galpha",
                           min = 0,
                           value =0),
              numericInput("NBphi",
                           label = "NBphi",
                           value = 0)

    ),
    nav_panel("Plot",
              sidebarLayout(
                sidebarPanel(
                  actionButton("save_btn",
                    label = "Save"),
                  actionButton("load_btn",
                    label = "Load Plot"),
                  radioButtons(
                    "Gear_N",
                    label = "Current Gear:",
                    choices = "None",
                    selected = NULL,
                    inline = TRUE,
                    width = NULL),
                  checkboxGroupInput("SelComp",
                                     label = "Selectivity Components:",
                                     choices = "0",
                                     selected = NULL,
                                     inline = TRUE,
                                     width = NULL),
                  radioButtons(
                      "Sel_N",
                      label = "Current Component:",
                      choices = "0",
                      selected = NULL,
                      inline = TRUE,
                      width = NULL),
                  selectInput("fun_type",
                              label = "Component Function",
                              choices = as.list(select_types),
                              selected = select_types[1]),
                  chooseSliderSkin(
                    skin = "Flat",
                    color = "#941414"),
                  uiOutput("select_param")),

                mainPanel(
                  plotOutput("plot1"),
                  plotOutput("plot2"),
                          )
              )
    )
  )
)
  # sidebarLayout(
  #   sidebarPanel(
  #   # File input
  #     fileInput("load", label = "Load Model Data (.rda format)",
  #               accept = c(".rda")),
  #   # Gears and selectivity components
  #     numericInput("NoofSel",
  #                  label = "Number of Selectivity Components:",
  #                  min = 0,
  #                  max = 7,
  #                  value = 0),
  #     radioButtons(
  #       "Gear_N",
  #       label = "Current Gear:",
  #       choices = "None",
  #       selected = NULL,
  #       inline = TRUE,
  #       width = NULL
  #     ),
  #     checkboxGroupInput("SelComp",
  #                        label = "Selectivity Components:",
  #                        choices = "0",
  #                        selected = NULL,
  #                        inline = TRUE,
  #                        width = NULL
  #     ),
  #     radioButtons(
  #       "Sel_N",
  #       label = "Current Component:",
  #       choices = "0",
  #       selected = NULL,
  #       inline = TRUE,
  #       width = NULL
  #     ),
  #     selectInput("fun_type",
  #                 label = "Component Function",
  #                 choices = as.list(select_types),
  #                 selected = select_types[1]),
  #     chooseSliderSkin(
  #       skin = "Flat",
  #       color = "#941414"),
  #     uiOutput("select_param")
  #   ),
  #
  #   # plot selectivity
  #   mainPanel(actionButton("save_btn",
  #                          label = "Save"),
  #             actionButton("load_btn",
  #                          label = "Load Plot"),
  #             plotOutput("plot1"),
  #             plotOutput("plot2"),
  #
  #             )
  # )
#)



server <- function(input, output, session) {

  shinyjs::enable()

# Declarations ------------------------------------------------------------
  #Reactive values
  blicc_ld <- reactiveVal(NULL)  # blicc data list
  gear_names <- reactiveVal(NULL)
  NGears <- reactiveVal(0)
  Cur_Gear <- reactiveVal(0)
  Cur_Sel <- reactiveVal(0)
  Cur_SelFun <- reactiveVal(0)
  Cur_SelComp <- reactiveVal(0)
  Cur_SelCompIndex <- reactiveVal(0)
  Cur_SelPar <- reactiveVal(0)
  Cur_SelWt <- reactiveVal(0)
  mort_ls <- reactiveVal(NULL)
  #Non-reactive values
  Old_Gear <- Old_Sel <- -1L  # Control updating of blicc_ld
  blicc_ld <- NULL
  min_length <- mid_length <- max_length <- 0
  nm_slope <- lg_slope <- 0  #default log-slopes
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
      Cur_Gear(0)
      gear_names(NULL)
      updateNumericInput(session, "NoofSel", value = 0)
      Old_Gear <- Old_Sel <- 0L  # Control updating of blicc_ld
      blicc_ld <<- NULL
    } else {
      # Need to check data list is correct format blicc_ld
      if ("gname" %in% names(file_data())) {
        blicc_ld <<- file_data()
        NGears(file_data()$NG)
        Cur_Gear(1)
        gear_names(file_data()$gname)
        updateNumericInput(session, "NoofSel", value = file_data()$NS)
        updateRadioButtons(session, "Gear_N", label = "Current Gear:",
          choices = file_data()$gname,
          selected = file_data()$gname[1],
          inline = TRUE
        )
        updateNumericInput(session, "Linf", value =blicc_ld$poLinfm, step = "0.01")
        updateNumericInput(session, "M/K", value =blicc_ld$polMkm, step = "0.01")
        updateNumericInput(session, "Galpha", value =blicc_ld$polGam, step = "0.01")
        updateNumericInput(session, "NBphi", value =blicc_ld$polNB_phim, step = "0.01")
        min_length <<- blicc_ld$LMP[1]
        mid_length <<- blicc_ld$LMP[blicc_ld$NB %/% 2]
        max_length <<- blicc_ld$LMP[blicc_ld$NB]
        nm_slope <- -log(mid_length)
        lg_slope <- -log(mid_length*0.5)

      }
    }
  })


  # Gear & Selectivity Component --------------------------------------
  # Includes updating blicc_ld() with new values
  # Change current gear
  observe({
    req(input$Gear_N)
    cur <- match(input$Gear_N, gear_names())
    Cur_Gear(cur)
    cur_sel_comp <- get_sel_indices(blicc_ld, cur)
    Cur_SelComp(cur_sel_comp)
    updateCheckboxGroupInput(session, "SelComp",
                             choices = as.character(1:NoofSelComp()),
                             selected = cur_sel_comp,
                             inline = TRUE)
    updateRadioButtons(session, "Sel_N",
                       choices = as.character(Cur_SelComp()),
                       selected = cur_sel_comp[1],
                       inline = TRUE)
    mort_ls(NULL)
  }) |>
    bindEvent(input$Gear_N)


  # Change current selectivity component
  observe({
    req(input$Gear_N, input$Sel_N)
    cur <- as.integer(input$Sel_N)
    Cur_Sel(cur)
    fSel <- blicc_ld$fSel[cur]
    Cur_SelFun(fSel)
    updateSelectInput(session, "fun_type",
                      selected = select_types[fSel])
    sc <- which(Cur_SelComp()==cur)
    Cur_SelPar(get_sel_par(blicc_ld, cur))
    Cur_SelWt(get_sel_wt(blicc_ld, Cur_Gear(), cur))
    if (cur==0) {  #! (cur %in% Cur_SelComp())) {
      Cur_SelCompIndex(1)
      mort_ls(NULL)
    } else {
      Cur_SelCompIndex(sc)
      mort_ls(mortality(blicc_ld, cur, Cur_Gear()))
    }
  }) |>
    bindEvent(input$Sel_N)

#' Selectivity components
  observe({
    req(input$SelComp, input$Sel_N)
    if ((Old_Gear == Cur_Gear()) & (Old_Sel == Cur_Sel())) {
      sc <- as.integer(input$SelComp)
      Cur_SelComp(sc)
      wt1 <- hash_wt(blicc_ld)   # record current weights
      ld <- fishblicc::blicc_gear_sel(blicc_ld, list(sc), gear=Old_Sel)
      wt2 <- hash_wt(ld)   # record new locations
      ld$polSm[(ld$NP+1L):length(ld$polSm)]  <- wt1[[3]][match(wt2[[1]], wt1[[1]])]
      ld$polSm[is.na(ld$polSm)] <- log(0.5)
      cur_comp <- as.integer(input$Sel_N)
      if ( ! (cur_comp %in% sc)) cur_comp <- 1
      updateRadioButtons(session, "Sel_N",
                         choices = as.character(sc),
                         selected = cur_comp,
                         inline = TRUE)
      blicc_ld <<- ld
      if (! (Cur_Sel() %in% Cur_SelComp()))
        mort_ls(NULL)
      else
        mort_ls(mortality(blicc_ld, Cur_Sel(), Cur_Gear()))
    } else if (Cur_Sel() > 0) {
      Old_Gear <<- Cur_Gear()
      Old_Sel <<- Cur_Sel()
    }
  }) |>
    bindEvent(input$SelComp)


  observe({
    req(input$fun_type)
    if ((Old_Gear == Cur_Gear()) & (Old_Sel == Cur_Sel())) {
      cur <- match(input$fun_type, select_types)
      suppressWarnings({
        ld <- blicc_selfun(blicc_ld, sel_fun=cur, sel_indx=Old_Sel)
        spar <- get_sel_par(blicc_ld, Cur_Sel())
        if (any(is.na(spar))) {
          if (cur==1)
            ld <- blip_sel(ld, sel_indx=Old_Sel, loc = mid_length*0.5, lslope=lg_slope)
          else if (cur==4)
            ld <- blip_sel(ld, sel_indx=Old_Sel, loc = mid_length, lslope=c(nm_slope, nm_slope))
          else
            ld <- blip_sel(ld, sel_indx=Old_Sel, loc = mid_length, lslope=nm_slope)
        }
      })
      blicc_ld <<- ld
      Cur_SelFun(cur)
      Cur_SelPar(spar)
      Cur_SelWt(get_sel_wt(blicc_ld, Cur_Gear(), Cur_Sel()))
      if (! (Cur_Sel() %in% Cur_SelComp()))
        mort_ls(NULL)
      else
        mort_ls(mortality(blicc_ld, Cur_Sel(), Cur_Gear()))
    } else if (Cur_Sel() > 0) {
      Old_Gear <<- Cur_Gear()
      Old_Sel <<- Cur_Sel()
    }
  }) |>
  bindEvent(input$fun_type)


  observe({
    req(input$fun_type, input$peak_slider)
    ld <- blicc_ld
    if ((Old_Gear == Cur_Gear()) & (Old_Sel == Cur_Sel())) {
      ld <- blip_sel(ld, sel_indx=Old_Sel, loc=input$peak_slider)
    } else {
      Old_Gear <<- Cur_Gear()
      Old_Sel <<- Cur_Sel()
    }
    blicc_ld <<- ld
  }) |>
  bindEvent(input$peak_slider)


  observe({
    req(input$fun_type, input$slope_slider1)
    ld <- blicc_ld
    if ((Old_Gear == Cur_Gear()) & (Old_Sel == Cur_Sel())) {
      fun_type <- match(input$fun_type, select_types)
      if (fun_type==4) {
        req(input$slope_slider2)
        lsl <- -log(c(input$slope_slider1, input$slope_slider2))
      } else
        lsl <- -log(input$slope_slider1)
      ld <- blip_sel(ld, sel_indx=Old_Sel, loc=input$peak_slider, lslope=lsl)
    } else {
      Old_Gear <<- Cur_Gear()
      Old_Sel <<- Cur_Sel()
    }
    blicc_ld <<- ld
  }) |>
  bindEvent(input$slope_slider1)


  observe({
    req(input$slope_slider1, input$slope_slider2)
    ld <- blicc_ld
    if ((Cur_SelFun()==4 & Old_Gear == Cur_Gear()) & (Old_Sel == Cur_Sel())) {
      lsl <- -log(c(input$slope_slider1, input$slope_slider2))
        ld <- blip_sel(ld, sel_indx=Old_Sel, loc=input$peak_slider, lslope=lsl)
    } else {
      Old_Gear <<- Cur_Gear()
      Old_Sel <<- Cur_Sel()
    }
    blicc_ld <<- ld
  }) |>
  bindEvent(input$slope_slider2)


  observe({
    req(input$weight_slider)
    ld <- blicc_ld
    if ((Old_Gear == Cur_Gear()) & (Old_Sel == Cur_Sel())) {
      ld <- set_sel_wt(ld, Old_Gear, Old_Sel, input$weight_slider)
    } else {
      Old_Gear <<- Cur_Gear()
      Old_Sel <<- Cur_Sel()
    }
    blicc_ld <<- ld
  }) |>
  bindEvent(input$weight_slider)

  # Reactives gear & selectivity components --------------------------

  NoofSelComp <- reactive({
    return(input$NoofSel)
  })

  # Edit select parameters ---------------------------------------------------

  # This needs to change every time the selectivity function changes
  output$select_param <- renderUI({
    if (Cur_Sel()==0) return(NULL)
    fSel <- blicc_ld$fSel[Cur_Sel()]
    if (fSel==1) max_slope <- max_length else max_slope <- 2*max_length
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
        max = max_slope,
        value = 1/sel_par[2]
      )
    )

    sl <- 3
    if (fSel==4) {
      if (length(sel_par) < 3) sp <- max_length/2 else sp <- 1/sel_par[3]
      sliders[[3]] <-
        sliderInput(
          "slope_slider2",
          "Right Slope:",
          min = 0,
          max = max_slope,
          value = sp
        )
      sl <- 4
    }
    if (Cur_SelCompIndex() > 1)
      sliders[[sl]] <- sliderInput("weight_slider", "Mix Weight",
                  min = 0, max = 1, value = exp(Cur_SelWt()))
    return(sliders)
  })


#
# # Plotting functions ---------------------------------------------------------------
#


  #observe({
    output$plot1 <- renderPlot({
      if(input$load_btn) {
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
      } else {
        return(NULL)
      }
    })

  #})

    output$plot2 <- renderPlot({
    if(input$save_btn > 0) {
      req(observe_df(), expect_df())
      fishblicc::plot_prior(blicc_ld)
    } else {
      return(NULL)
    }
  })





  # Define the reactive for frequency data
  observe_df <- reactive({
    req(input$Gear_N)
    if (input$Gear_N == "None") return(NULL)
    return(data.frame(len = blicc_ld$LMP, fq = blicc_ld$fq[[input$Gear_N]]))
  })

  # Define the reactive for expected frequency
  expect_df <- reactive({
    req(mort_ls(), input$peak_slider, input$slope_slider1)
    NSample <- sum(blicc_ld$fq[[Cur_Gear()]])

#print(c(421L, mort_ls()$fSel, Cur_SelFun(), blicc_ld$fSel[Cur_Sel()]))

    if (Cur_SelFun()==4) {
      req(input$slope_slider2)
      spar <- c(input$peak_slider, 1/input$slope_slider1, 1/input$slope_slider2)
    } else
      spar <- c(input$peak_slider, 1/input$slope_slider1)
    if (Cur_SelCompIndex() > 1) {
      req(input$weight_slider)
      wt <- input$weight_slider
    } else
      wt <- 1
    return(expect_freq(blicc_ld, mort_ls(), spar,
                       wt, NSample))
  })

# JS sliders -------------------------------------------------------------
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
#       checkboxGroupInput("SelComp",
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
#   updateCheckboxGroupInput(session, "SelComp",
#                            choices = as.character(1:NoofSelComp()),
#                            selected = get_sel_indices(blicc_ld(), input$num_tabs),
#                            inline = TRUE)
#
# })

