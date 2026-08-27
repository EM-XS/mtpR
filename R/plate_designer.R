#' Interactive Shiny app to design plate layouts
#'
#' Launches a Shiny application for designing microtiter plate layouts: pick a
#' plate type, number of reactions, replicates and spacing, and get back a
#' destination plate map as a plot and a downloadable table.
#'
#' @section Status:
#' This GUI is **experimental**. The programmatic functions it wraps
#' ([replicate_reactions()], [add_plates()], [add_plate_replicates()],
#' [convert_well()], [plot_plate()]) are the supported interface.
#'
#' @return A [shiny::shinyApp()] object.
#' @export
design_plate <- function() {

  ui <- shiny::fluidPage(
    shiny::titlePanel("mtpR plate designer"),
    plate_designer_ui("Setup")
  )

  server <- function(input, output, session) {
    plate_designer_server("Setup")
  }

  shiny::shinyApp(ui, server)
}

# Plate types the designer offers, largest first.
.plate_designer_choices <- c("384", "96", "48", "24", "12")

#' Plate designer UI module
#'
#' @param id Namespace id for the module.
#' @return A Shiny UI definition.
#' @noRd
plate_designer_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::sidebarLayout(
    shiny::sidebarPanel(
      width = 4,
      shiny::numericInput(
        ns("rxnCount"), "Number of reactions",
        value = 8, min = 1, step = 1
      ),
      shiny::numericInput(
        ns("replicatecount"), "Replicates per reaction",
        value = 3, min = 1, step = 1
      ),
      shiny::numericInput(
        ns("platereplicates"), "Number of plate replicates",
        value = 1, min = 1, step = 1
      ),
      shiny::tags$hr(),
      shiny::selectInput(
        ns("destplate"), "Destination plate type",
        choices = .plate_designer_choices
      ),
      shiny::selectInput(
        ns("fillwise"), "Fill by rows or columns?",
        choices = c(Rows = "row", Columns = "col")
      ),
      shiny::selectInput(
        ns("deststartwell"), "Starting well",
        choices = NULL
      ),
      shiny::helpText(
        "Starting-well options follow the selected plate type and fill direction."
      ),
      shiny::tags$hr(),
      shiny::selectInput(
        ns("replicatestyle"), "Group by",
        choices = c(`Reaction, then replicates` = "Reaction",
                    `Replicate, then reactions` = "Replicate")
      ),
      shiny::numericInput(
        ns("interwell"), "Spacing between groups (wells)",
        value = 1, min = 1, step = 1
      ),
      shiny::numericInput(
        ns("intrawell"), "Spacing within a group (wells)",
        value = 1, min = 1, step = 1
      ),
      shiny::tags$hr(),
      shiny::actionButton(ns("createPlateMap"), "Create plate map",
                          class = "btn-primary"),
      shiny::downloadButton(ns("downloadCsv"), "Download CSV")
    ),
    shiny::mainPanel(
      width = 8,
      shiny::plotOutput(ns("PlatePlot"), height = "500px"),
      shiny::tags$hr(),
      DT::dataTableOutput(ns("PlateMap"))
    )
  )
}

#' Plate designer server module
#'
#' @param id Namespace id for the module.
#' @return The module server (called for its side effects).
#' @noRd
plate_designer_server <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      # Keep the starting-well choices in step with the chosen plate type and
      # fill direction, preserving the current pick when it is still valid.
      shiny::observeEvent(
        list(input$destplate, input$fillwise),
        {
          shiny::req(input$destplate, input$fillwise)
          choices <- start_well_choices(input$destplate, input$fillwise)
          selected <- if (isTRUE(input$deststartwell %in% choices)) {
            input$deststartwell
          } else {
            choices[1]
          }
          shiny::updateSelectInput(session, "deststartwell",
                                   choices = choices, selected = selected)
        },
        ignoreInit = FALSE
      )

      plate_map <- shiny::eventReactive(input$createPlateMap, {
        shiny::req(input$rxnCount, input$deststartwell, input$destplate,
                   input$replicatecount, input$platereplicates)

        dest_plate <- as.numeric(input$destplate)

        result <- tryCatch(
          data.frame(reaction_count = seq_len(input$rxnCount)) |>
            replicate_reactions(
              reaction_col_name = "reaction_count",
              num_replicates = input$replicatecount,
              priority = input$replicatestyle,
              inter_spacing = input$interwell,
              intra_spacing = input$intrawell,
              start_position = convert_well(
                input$deststartwell, "to_num",
                wise = input$fillwise, plate = dest_plate
              )
            ) |>
            add_plates(well_index_column = "position", plate_size = dest_plate) |>
            dplyr::mutate(
              Well.Position = convert_well(
                Well.Index, "to_well",
                wise = input$fillwise, plate = dest_plate
              )
            ) |>
            add_plate_replicates(plate_replicate_number_total = input$platereplicates) |>
            dplyr::mutate(
              `Plate Name` = as.character(
                stringr::str_glue("Plate_{Plate.Index}_{PlateReplicate.Number}")
              )
            ),
          error = function(e) e
        )

        if (inherits(result, "error")) {
          shiny::validate(paste0("Could not build the plate map: ",
                                 conditionMessage(result), "."))
        }
        result
      })

      output$PlatePlot <- shiny::renderPlot({
        plate_map() |>
          plot_plate(
            fill = "reaction_count",
            well_id = "Well.Position",
            facet_rows = "Plate.Index",
            facet_cols = "PlateReplicate.Number",
            plate = as.numeric(input$destplate)
          ) +
          ggplot2::labs(fill = "Reaction") +
          ggplot2::theme(legend.position = "bottom")
      })

      output$PlateMap <- DT::renderDataTable({
        DT::datatable(
          plate_map(),
          rownames = FALSE,
          extensions = "Buttons",
          options = list(
            dom = "Brtip",
            buttons = c("copy", "csv", "excel"),
            pageLength = -1
          )
        )
      })

      output$downloadCsv <- shiny::downloadHandler(
        filename = function() {
          paste0("plate_map_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
        },
        content = function(file) {
          utils::write.csv(plate_map(), file, row.names = FALSE)
        }
      )
    }
  )
}
