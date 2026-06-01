library(conflicted)
conflicts_prefer(
  dplyr::filter,
  dplyr::lag,
  dplyr::intersect,
  dplyr::setdiff,
  dplyr::setequal,
  dplyr::union,
  .quiet = TRUE
)

library(shiny)
library(toastui)
library(dplyr)
library(tibble)
library(readr)
library(bslib)


# options(shiny.useragg = TRUE, shiny.launch.browser = .rs.invokeShinyWindowExternal)

d <- readr::read_csv("battery.csv", show_col_types = FALSE) |>
  mutate(family_id = as.integer(factor(Family)),
         battery_id = as.integer(factor(Battery))) |>
  mutate(edition_id = row_number()) |>
  arrange(Family, Battery, edition_id) |>
  select(
    Family,
    Battery,
    Edition,
    starts_with("Year"),
    Reliability,
    Mean,
    SD,
    ends_with("_id")
  )

d_family <- d |>
  select(Family, family_id) |>
  unique() |>
  arrange(Family)

d_battery <- d |>
  select(Family, Battery, battery_id) |>
  unique() |>
  arrange(Family, Battery)

d_edition <- d |>
  select(-Family, -Battery)

d_person <- tibble(
  Edition = "WAIS-5 (2024)",
  Score = 100,
  Date = Sys.Date(),
  Weight = 1,
  `Flynn Effect` = "default"
)


library(toastui)
library(shiny)
# ui ----
ui <- page_fluid(
  title = "Composite IQ Calculator",
  theme = bs_theme(base_font = font_google("Roboto Condensed")),
  navset_bar(
  id = "mainPanel",navbar_options = navbar_options(position = "fixed-top"),
  # type = "pills",
  # header = tagList(
  #   use_theme(mytheme)
  # ),

  ## entry ----
  nav_panel(
    "Data Entry",
    value = "score",
    status = "primary",
    div(style = "margin-top: 10px;", fluidRow(
      column(
        width = 6,
        textInput(
          "txtPerson",
          label = "Name (optional)",
          placeholder = "Person's Name",
          width = "100%"
        )
      ), column(
        width = 6,
        dateInput("dateBirthdate", label = "Birthdate (required)", width = "100%")
      )
    )),

    fluidRow(column(
      width = 12, datagridOutput("grdScore", height = "auto")
    )),
  ),
  ## battery ----
  nav_panel(
    "Edit Test List",
    value = "edition",
    div(style = "margin-top: 10px;", fluidRow(column(
      width = 12, datagridOutput2("grdEdition")
    ))),
    fluidRow(column(
      width = 12,
      actionButton(inputId = "add_edition", label = "Add Test Battery")
    ))
  )
))

# server ----
server <- function(input, output, session) {
  ## edition ----
  output$grdEdition <- renderDatagrid2({
    datagrid(
      d,
      sortable = FALSE,
      pagination = 15,
      theme = "striped",
      filters = TRUE
    ) |>
      grid_col_button(
        column = "edition_id",
        inputId = "edition_remove_row",
        label = "Remove",
        icon = icon("trash"),
        status = "primary",
        btn_width = "115px",
        align = "center",
        header = "Remove"
      ) |>
      grid_columns(columns = "Battery",
                   sortable = TRUE,
                   minWidth = 300) |>
      grid_columns(columns = "Family", sortable = TRUE) |>
      grid_columns(columns = "Edition",
                   sortable = TRUE,
                   header = "Acronym",) |>
      grid_columns(columns = "Year_Published",
                   header = "Published",
                   align = "center") |>
      grid_columns(columns = "Year_Normed",
                   header = "Normed",
                   align = "center") |>
      grid_columns(columns = c("Mean", "SD", "Reliability"),
                   align = "center") |>
      grid_columns(columns = c("family_id", "battery_id"),
                   hidden = TRUE) |>
      grid_editor(column = c("Battery", "Family"),
                  type = "text") |>
      grid_editor(column = "Edition",
                  type = "text",
                  validation = validateOpts(unique = TRUE)) |>
      grid_editor(
        column = c("Year_Published", "Year_Normed", "Mean", "SD"),
        type = "number"
      ) |>
      grid_editor(
        column = "Reliability",
        type = "number",
        validation = validateOpts(
          min = 0,
          max = 1,
          required = FALSE
        )
      ) |>
      grid_editor_opts(editingEvent = "click") |>
      grid_click("edition_click")
  })

  ## add_edition ----
  observeEvent(input$add_edition, {
    showModal(
      modalDialog(
        title = "Add Test Battery",
        textInput(
          "edition_new",
          "Test Edition/Acronym (e.g., WAIS-5)",
          width = "100%"
        ),
        selectizeInput(
          "family_new",
          "Family",
          choices = c("", sort(
            unique(input$grdEdition_data$Family)
          )),
          width = "100%",
          selected = NA,
          multiple = FALSE,
          options = list(
            create = TRUE,
            persist = FALSE,
            placeholder = "Start typing .."
          )
        ),
        selectizeInput(
          "battery_new",
          "Battery",
          choices = c("", sort(
            unique(input$grdEdition_data$Battery)
          )),
          width = "100%",
          selected = NA,
          multiple = FALSE,
          options = list(
            create = TRUE,
            persist = FALSE,
            placeholder = "Start typing .."
          )
        ),
        fluidRow(
          column(
            width = 6,
            numericInput(
              "year_published_new",
              "Year Published",
              value = as.integer(format(Sys.Date(), "%Y")),
              width = "100%",
              step = 1
            )
          ),
          column(
            width = 6,
            numericInput(
              "year_normed_new",
              "Year Normed",
              value = as.integer(format(Sys.Date(), "%Y")) - 1L,
              width = "100%",
              step = 1
            ),
          )
        ),
        fluidRow(
          column(
            width = 4,
            numericInput(
              "reliability_new",
              "Reliability",
              value = 0.96,
              width = "100%",
              step = .01,
              max = 1,
              min = 0
            )
          ),
          column(
            width = 4,
            numericInput(
              "mean_new",
              "Mean",
              value = 100L,
              width = "100%",
              step = 1
            )
          ),
          column(
            width = 4,
            numericInput(
              "sd_new",
              "SD",
              value = 15L,
              width = "100%",
              step = 1
            ),
          )
        ),
        textOutput('error_msg'),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("add_edition_submit", "Add", class = "btn-primary"),
          textOutput('success_msg')
        ),
        easyClose = TRUE
      )
    )

  })

  observeEvent(input$family_new, {
    new_choices <- c("",
                     input$grdEdition_data |>
                       filter(Family == input$family_new) |>
                       pull(Battery))
    updateSelectizeInput(
      session,
      inputId = "battery_new",
      choices = new_choices,
      selected = new_choices[1] # Optional: Set a default selection
    )

  })

  output$error_msg <- renderText({
    shiny::validate(
      shiny::need(input$family_new != '', 'You must enter a test battery family.'),
      shiny::need(input$battery_new != '', "You must enter a test battery name"),
      shiny::need(
        input$edition_new != '',
        "You must enter a test edition name or acronym."
      )
    )

  })

  observeEvent(input$add_edition_submit, {
    req(input$family_new)
    req(input$battery_new)
    req(input$edition_new)
    req(input$year_published_new)
    output$success_msg <- renderText({
      "Success"
    })
    new_edition_id <- max(input$grdEdition_data$edition_id) + 1L
    new_battery_id <- input$grdEdition_data |>
      filter(Battery == input$battery_new) |>
      pull(battery_id) |>
      unique()
    if (length(new_battery_id) == 0) {
      new_battery_id <- max(input$grdEdition_data$battery_id) + 1L
    }

    new_family_id <- input$grdEdition_data |>
      filter(Family == input$family_new) |>
      pull(family_id) |>
      unique()
    if (length(new_family_id) == 0) {
      new_family_id <- max(input$grdEdition_data$family_id) + 1L
    }

    d_new <- d |>
      filter(FALSE) |>
      add_row(
        Family = input$family_new,
        Battery = input$battery_new,
        Edition = input$edition_new,
        Year_Published = input$year_published_new,
        Year_Normed = input$year_normed_new,
        Reliability = input$reliability_new,
        Mean = input$mean_new,
        SD = input$sd_new,
        family_id = new_family_id,
        battery_id = new_battery_id,
        edition_id = new_edition_id
      )

    grid_proxy_add_row(proxy = "grdEdition", data = d_new)



    removeModal()
  })
  ## remove edition ----
  observeEvent(input$edition_remove_row, {
    data <- input$grdEdition_data
    rowKey <- data$rowKey[data$edition_id == as.integer(input$edition_remove_row)]
    grid_proxy_delete_row("grdEdition", rowKey)
  })




  rScore <- reactiveVal({
    datagrid(d_person) |>
      grid_columns(columns = c("Score", "Date", "Weight", "Flynn Effect"),
                   align = "center") |>
      grid_editor(
        "Edition",
        type = "select",
        choices = d |> arrange(Family, Battery, Year_Published) |>
          mutate(ed = paste0(Edition, " (", Year_Published, ")")) |>
          pull(ed)
      ) |>
      grid_editor(c("Score", "Weight"), type = "number") |>
      grid_editor_date("Date", type = "date") |>
      grid_editor_opts(editingEvent = "click")
  })

  observeEvent(input$mainPanel, {
    if (input$mainPanel == "score") {
      req(input$grdEdition_data)
      dd <- isolate(input$grdEdition_data) |>
          arrange(Family, Battery, Year_Published) |>
          mutate(ed = paste0(Edition, " (", Year_Published, ")"))
      ed <- dd$Edition


      rScore({
        datagrid(d_person) |>
          grid_columns(
            columns = c("Score", "Date", "Weight", "Flynn Effect"),
            align = "center"
          ) |>
          grid_editor("Edition", type = "select", choices = ed, useListItemText = TRUE) |>
          grid_editor(c("Score", "Weight"), type = "number") |>
          grid_editor_date("Date", type = "date") |>
          grid_editor_opts(editingEvent = "click")
      })


    }

  })





  ## score ----
  output$grdScore <- renderDatagrid2({
    input$mainPanel
    rScore()
  })

}

shinyApp(ui, server)