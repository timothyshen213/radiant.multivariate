###############################
# Principal component analysis
###############################

## add a spinning refresh icon if the tabel needs to be (re)calculated
run_refresh(pca_args, "pca", init = "vars", label = "Estimate model", relabel = "Re-estimate model")

output$ui_pca <- renderUI({
  req(input$dataset)
  tagList(
    wellPanel(
      actionButton("pca_run", "Estimate model", width = "100%", icon = icon("play", verify_fa = FALSE), class = "btn-success")
    ),
    wellPanel(
      checkboxInput("pca_scale", "Scale", state_init("pca_scale", TRUE)),
      checkboxInput("pca_center", "Center", state_init("pca_center", TRUE)),
      numericInput(
        "pca_pc", "Principal Components",
        min = 0, max = 100, step=1,
        value=state_init("pca_pc",2)
        )
      ),
      checkboxInput("pca_plot", "Plots", state_init("pca_plot", TRUE))
    ,
    help_and_report(
      modal_title = "Poster component analysis",
      fun_name = "pca",
      help_file = inclMD(file.path(getOption("radiant.path.multivariate"), "app/tools/help/pca.md"))
    )
  )
})


## output is called from the main radiant ui.R
output$pca <- renderUI({
  register_print_output("summary_pca", ".summary_pca")

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
  if (not_available(input$hc_vars)) {
    "This analysis requires one or more variables of type integer or numeric.\nIf these variable types are not available please select another dataset.\n\n" %>%
      suggest_data("toothpaste")
  } else if (not_pressed(input$hc_run)) {
    "** Press the Estimate button to generate cluster solution **"
  } else {
    summary(.pca())
  }
})
