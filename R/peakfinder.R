#' Interactively determine the peak of a densitiy estimation
#' 
#' A data frame with peak coordinates is shown in the second tab. Press 'Done'
#' to return the data frame to the R console.
#' 
#' @param x a time series
#' @examples
#' 
#' peakfinder(AirPassengers)
peakfinder <- function(x){

  # calc spectrum
  sp <- spectrum(x, method = "ar")
  df <- data_frame(x = sp$freq, y = c(sp$spec))

  ui <- miniPage(
    gadgetTitleBar("Peakfinder"),
    miniTabstripPanel(
      miniTabPanel("Spectrum", icon = icon("search"),
        miniContentPanel(
          dygraphOutput("oPlot")
        )
      ), 
      miniTabPanel("Selected Maximum", icon = icon("line-chart"),
        miniContentPanel(
          verbatimTextOutput("oMax")
        )
      )
    )
  )

  server <- function(input, output) {

    # interactive spectrum plot
    output$oPlot <- renderDygraph({
      dygraph(df) %>%
        dyRangeSelector() %>%
        dyOptions(logscale = TRUE)
    })

    # reactive peak calculation
    rPeakDf <- reactive({
      wd <- input$oPlot_date_window
      req(wd)
      df %>%
        filter(x >= wd[1], x <= wd[2]) %>%
        slice(which.max(y)) %>% 
        mutate(x.start = wd[1], x.end = wd[2])
    })

    # show peak df
    output$oMax <- renderPrint({
      rPeakDf()
    })

    # done, cancel buttons
    observe({
      if (input$done > 0){
        stopApp(returnValue = rPeakDf())
      }
    })
    observe({
      if (input$cancel > 0){
        x <- isolate(r_vars())
        stopApp(returnValue = invisible())
      }
    })
  }

  runGadget(shinyApp(ui = ui, server = server), stopOnCancel = FALSE)
}


