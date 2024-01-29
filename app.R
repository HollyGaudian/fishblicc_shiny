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
library("here")
library("dplyr")

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "simplex"),
  titlePanel("Plot Normal Selectivitiy Curve"),
  sidebarLayout(position = "left",
                sidebarPanel(
    #radioButtons(label = "selfun"),
    #radioButtons(label = "Pick functions"),
    selectInput("Selectivity_functions", label = "Selectivity", h4("Select:"), choices = list("Logistic", "Normal", "Single Normal", "Double Normal"), multiple = FALSE),
    actionButton("Save", label = "Save"),
    actionButton("Priors", label = "Save priors")
  ),
  mainPanel(
    h2("Plot"),
    fluidRow(
      column(width = 9,
                    h4("Click plot to add points"),
                    actionButton("rem_point", "Remove Last Point"),
                    plotOutput("plot1", click = "plot_click")),
      column(width = 3,
                    h4("Table of points"),
                    tableOutput("table"))
  ))
  ))

server <- function(input, output){

  ## import data
  para <- read_excel("data/stock_parameters.xlsx")
  lenfreq <- read.csv(file = "data/bansis_len_freq_data.csv") |>
    filter(sciname == "Nemipterus japonicus") |>
    mutate(len=floor(length_cm)) |>
    group_by(len) |>
    summarise(fq=sum(frequency)) |>
    ungroup()



  ## 1. set up reactive dataframe ##
  values <- reactiveValues()
  values$DT <- data.frame(x = numeric(),
                          y = numeric(),
                          selfun = factor()
                          )
  dfR <- reactiveValues()
  dfR$df <- data.frame(len = lenfreq$length_cm, fq = lenfreq$frequency)

    ## 2. Create a plot ##
  output$plot1 = renderPlot({
    ggplot() +
      geom_point(values$DT, aes(x = x, y = y)) +
      geom_col(data=dfR$df, aes(x=len, y=fq), fill="#09306e", alpha=1) +
      lims(x = c(0, 75)) +
      theme(legend.position = "bottom") +
      labs(x="Length", y="Frequency")
      # include so that colours don't change as more colour/shape chosen
      #scale_color_discrete(drop = FALSE) +
      #scale_shape_discrete(drop = FALSE)
  })

  ## 3. add new row to reactive dataframe upon clicking plot ##
  observeEvent(input$plot_click, {
    # each input is a factor so levels are consistent for plotting characteristics
    add_row <- data.frame(x = input$plot_click$x,
                          y = input$plot_click$y,
                          selfun = factor(input$selfun,
                                          levels = c("Logistic", "Normal", "Single Normal", "Double Normal")
                                          ),
                          row.names = NULL,
                          stringsAsFactors = FALSE
                          )
    # add row to the data.frame
    values$DT <- rbind(values$DT, add_row)
  })

  ## 4. remove row on actionButton click ##
  observeEvent(input$rem_point, {
    rem_row <- values$DT[-nrow(values$DT), ]
    values$DT <- rem_row
  })

  ## 5. render a table of the growing dataframe ##
  output$table <- renderTable({
    values$DT
  })
  #isolate(reactiveValuesToList(values))

  # Downloadable file ----
  observeEvent(input$save, {
    finalDF <- isolate(values[["DT"]])
    save(finalDF, file=here("tmp.rda"))
  })

}

shinyApp(ui = ui, server = server)

# Run the application

#shinyApp(ui, server)





