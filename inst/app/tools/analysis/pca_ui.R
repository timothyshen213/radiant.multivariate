pca_plots <- c("Scree" = "scree", "Biplot" = "biplot")

###############################
# Principal component analysis
###############################
pca_args <- as.list(formals(pca))

pca_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  pca_args$data_filter <- if (input$show_filter) input$data_filter else ""
  pca_args$dataset <- input$dataset
  for (i in r_drop(names(pca_args))) {
    pca_args[[i]] <- input[[paste0("pca_", i)]]
  }
  pca_args
})

output$ui_pca_vars <- renderUI({
  vars <- varnames()
  toSelect <- .get_class() %in% c("numeric", "integer", "date", "factor")
  vars <- vars[toSelect]
  selectInput(
    inputId = "pca_vars", label = "Variables:", choices = vars,
    selected = state_multiple("pca_vars", vars),
    multiple = TRUE, size = min(15, length(vars)), selectize = FALSE
  )
})

## add a spinning refresh icon if the tabel needs to be (re)calculated
run_refresh(pca_args, "pca", init = "vars", label = "Estimate model", relabel = "Re-estimate model")

output$ui_pca <- renderUI({
  req(input$dataset)
  tagList(
    wellPanel(
      actionButton("pca_run", "Estimate model", width = "100%", icon = icon("play", verify_fa = FALSE), class = "btn-success")
    ),
    wellPanel(
      uiOutput("ui_pca_vars"),
      checkboxInput("pca_scal", "Scale", state_init("pca_scal", TRUE)),
      checkboxInput("pca_cent", "Center", state_init("pca_cent", TRUE)),
      numericInput(
        "pca_pc", "Principal Components",
        min = 0, max = 100, step=1,
        value=state_init("pca_pc",2)
        )
      ),
    selectizeInput(
      "pca_plots",
      label = "Plot(s):", choices = pca_plots,
      selected = state_multiple("pca_plots", pca_plots, c("scree", "biplot")),
      multiple = TRUE,
      options = list(
        placeholder = "Select plot(s)",
        plugins = list("remove_button", "drag_drop")
      )
    )
    # ,
    # help_and_report(
    #   modal_title = "Poster component analysis",
    #   fun_name = "pca",
    #   help_file = inclMD(file.path(getOption("radiant.path.multivariate"), "app/tools/help/pca.md"))
    # )
  )
})

pca_plot <- reactive({
  plots <- input$pca_plots
  req(plots)
  ph <- plots %>%
    {
      if (length(.) == 1 && . == "dendro") 800 else 400
    }
  pw <- if (!radiant.data::is_empty(plots) && length(plots) == 1 && plots == "dendro") 900 else 650
  list(plot_width = pw, plot_height = ph * length(plots))
})

pca_plot_width <- function() {
  pca_plot() %>%
    {
      if (is.list(.)) .$plot_width else 650
    }
}

pca_plot_height <- function() {
  pca_plot() %>%
    {
      if (is.list(.)) .$plot_height else 400
    }
}

## output is called from the main radiant ui.R
output$summary_pca <- renderPrint({
  cat("Principal Component Analysis \n")
  .summary_pca()})
output$pca <- renderUI({

  pca_output_panels <- tagList(
    tabsetPanel(
    tabPanel(
      "Summary",
      # download_link("dl_km_means"), br(),
      verbatimTextOutput("summary_pca")
    ),
    tabPanel(
      "Plot",
      # download_link("dlp_kclus"),
      plotOutput("plot_pca", width = "100%", height = "100%")
    )
  )
)

  stat_tab_panel(
    menu = "Multivariate > Cluster",
    tool = "PCA",
    tool_ui = "ui_pca",
    output_panels = pca_output_panels
  )
})

.pca <- eventReactive(input$pca_run, {
  req(input$pca_vars)
  withProgress(message = "Estimating cluster solution", value = 1, {
    pci <- pca_inputs()
    pci$envir <- r_data
    do.call(pca, pci)
  })
})

.summary_pca <- reactive({
  if (not_pressed(input$pca_run)) {
    "** Press the Estimate button to generate cluster solution **"
  } else {
    summary(.pca())
  }
})

.plot_hclus <- eventReactive({
  c(input$pca_run, input$pca_plots)
  },
  {
    withProgress(
      message = "Generating plots", value = 1,
      capture_plot(plot(.pca(), plots = input$pca_plots))
      )
  }
)



