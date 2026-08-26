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

#' Plate designer UI module
#'
#' @param id Namespace id for the module.
#' @param label Label for the setup section.
#' @return A Shiny UI definition.
#' @noRd
plate_designer_ui <- function(id, label = "Setup") {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        3,
        shiny::numericInput(
          inputId = ns("rxnCount"),
          label = "Number of reactions",
          value = 1,
          step = 1,
          min = 1
        )
      ),
      shiny::column(
        3,
        shiny::numericInput(
          inputId = ns("platereplicates"),
          label = "Number of plate replicates",
          value = 1,
          step = 1,
          min = 1
        )
      ),
      shiny::column(
        3,
        shiny::numericInput(
          inputId = ns("replicatecount"),
          label = "Number of Replicates",
          value = 3,
          min = 1
        )
      )
    ),
    shiny::br(),
    shiny::fluidRow(
      shiny::column(
        3,
        shiny::selectInput(
          inputId = ns("destplate"),
          label = "Destination Plate Type",
          choices = c("384", "96", "48", "24", "12")
        )
      ),
      shiny::column(
        3,
        shiny::selectInput(
          inputId = ns("deststartwell"),
          label = "Starting Well",
          # TODO: make these choices react to `destplate` rather than
          # always listing 384-well positions.
          choices = convert_well(
            input = 1:384,
            direction = "to_well",
            wise = "col",
            plate = 384
          )
        )
      ),
      shiny::column(
        3,
        shiny::selectInput(
          inputId = ns("fillwise"),
          label = "Work by Rows or Columns?",
          choices = c(Rows = "row", Columns = "col")
        )
      )
    ),
    shiny::br(),
    shiny::fluidRow(
      shiny::column(
        3,
        shiny::selectInput(
          inputId = ns("replicatestyle"),
          label = "Prioritize Reactions or Replicates",
          choices = c("Reaction", "Replicate")
        )
      ),
      shiny::column(
        3,
        shiny::numericInput(
          inputId = ns("interwell"),
          label = "Spacing across Wells",
          value = 1,
          min = 1,
          step = 1
        )
      ),
      shiny::column(
        3,
        shiny::numericInput(
          inputId = ns("intrawell"),
          label = "Spacing between Wells",
          value = 1,
          min = 1,
          step = 1
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(
        3,
        shiny::actionButton(ns("createPlateMap"), label = "Create Plate Map")
      )
    ),
    shiny::br(),
    shiny::plotOutput(outputId = ns("PlatePlot")),
    DT::dataTableOutput(outputId = ns("PlateMap"))
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

      myReactives <- shiny::reactiveValues(Reactions.Dest = NULL)

      shiny::observeEvent(input$createPlateMap, {
        shiny::req(input$rxnCount, input$deststartwell, input$destplate)

        dest_plate <- as.numeric(input$destplate)

        myReactives$Reactions.Dest <- data.frame(reaction_count = seq_len(input$rxnCount)) |>
          replicate_reactions(
            reaction_col_name = "reaction_count",
            num_replicates = input$replicatecount,
            priority = input$replicatestyle,
            inter_spacing = input$interwell,
            intra_spacing = input$intrawell,
            start_position = convert_well(
              input = input$deststartwell,
              direction = "to_num",
              wise = input$fillwise,
              plate = dest_plate
            )
          ) |>
          add_plates(well_index_column = "position", plate_size = dest_plate) |>
          dplyr::mutate(
            Well.Position = convert_well(
              Well.Index,
              direction = "to_well",
              wise = input$fillwise,
              plate = dest_plate
            )
          ) |>
          add_plate_replicates(plate_replicate_number_total = input$platereplicates) |>
          dplyr::mutate(`Plate Name` = stringr::str_glue("Plate_{Plate.Index}_{PlateReplicate.Number}"))
      })

      output$PlatePlot <- shiny::renderPlot({
        shiny::req(myReactives$Reactions.Dest)
        myReactives$Reactions.Dest |>
          plot_plate(
            fill = "reaction_count",
            well_id = "Well.Position",
            facet_rows = "Plate.Index",
            facet_cols = "PlateReplicate.Number",
            plate = as.numeric(input$destplate)
          ) +
          ggplot2::labs(fill = "Reaction Number") +
          ggplot2::theme(legend.position = "bottom")
      })

      output$PlateMap <- DT::renderDataTable({
        shiny::req(myReactives$Reactions.Dest)
        myReactives$Reactions.Dest |>
          DT::datatable(
            extensions = "Buttons",
            options = list(
              dom = "Brtip",
              buttons = c("copy", "csv", "excel"),
              pageLength = -1
            )
          )
      })
    }
  )
}
