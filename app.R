options(warn = 1)
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

library(cheetahR)
library(shiny)
library(shinyjs)
library(keys)
library(toastui)
library(dplyr)
library(tibble)
library(readr)
library(bslib)
library(scales)
library(brand.yml)
library(lubridate)
library(dm)
is_text_integer <- function(x) {
  grepl("^[+-]?[0-9]+$", x)
}

options(
  shiny.useragg = TRUE,
  shiny.launch.browser = .rs.invokeShinyWindowExternal
)

d <- readr::read_csv("battery.csv", show_col_types = FALSE) |>
  mutate(
    family_id = as.integer(factor(Family)),
    battery_id = as.integer(factor(Battery))
  ) |>
  mutate(edition_id = as.integer(row_number())) |>
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
  arrange(Family, Battery) %>%
  select(Battery, battery_id, family_id) |>
  unique()


d_edition <- d |>
  select(-Family, -Battery) |>
  select(battery_id, everything())

d_score <- tibble(
  edition_id = 1L,
  Score = 100,
  Date = Sys.Date(),
  Weight = 1,
  flynn_id = 1L,
  score_id = 1L
)

d_flynn <- tibble(Flynn = c("Default", "Always 2.94"), flynn_id = 1:2)

d_flynn_item <- tibble(
  Until = c(2007L, NA, NA),
  Effect = c(2.94, 1.3, 2.94),
  flynn_id = c(1L, 1L, 2L),
  flynn_item_id = 1:3
)


# db <- new_dm(list(
#   Family = d_family,
#   Battery = d_battery,
#   Edition = d_edition %>% select(-family_id),
#   Flynn = d_flynn,
#   FlynnItem = d_flynn_item,
#   Score = d_score
# )) %>%
#   dm_add_pk(Family, columns = family_id, autoincrement = TRUE) %>%
#   dm_add_pk(Battery, columns = battery_id, autoincrement = TRUE) %>%
#   dm_add_pk(Edition, columns = edition_id, autoincrement = TRUE) %>%
#   dm_add_pk(Flynn, columns = flynn_id, autoincrement = TRUE) %>%
#   dm_add_pk(Score, columns = score_id, autoincrement = TRUE) %>%
#   dm_add_pk(FlynnItem, columns = flynn_item_id, autoincrement = TRUE) %>%
#   dm_add_fk(Battery, family_id, Family) %>%
#   dm_add_fk(Edition, battery_id, Battery) %>%
#   dm_add_fk(Score, edition_id, Edition) %>%
#   dm_add_fk(Score, flynn_id, Flynn) %>%
#   dm_add_fk(FlynnItem, flynn_id, Flynn)

# db %>%
# dm::dm_examine_constraints()

# ui ----
ui <- page_navbar(
  title = "Composite IQ Calculator",
  window_title = "Composite IQ Calculator",
  theme = bs_theme(brand = "_brand.yml"),
  id = "mainPanel",
  header = tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "main.css"),
    useShinyjs(),
    useKeys(),
    keysInput("enter_key", "enter", global = TRUE)
  ),
  nav_panel(
    "Battery",
    value = "battery",
    page_fillable(
      layout_sidebar(
        # border = FALSE,
        sidebar = sidebar(
          ## family ----
          width = 250,
          # open = "always",
          datagridOutput2("grdFamily"),
          # datagridOutput2("grdBattery")
        ),
        tags$div(
          style = "display: flex; gap: 10px;",
          actionButton(
            "btn_add_edition",
            label = "Add Edition",
            class = "btn-primary"
          ),
          actionButton(
            "btn_update_edition",
            label = "Update Edition",
            class = "btn-primary"
          ),
          actionButton(
            "btn_remove_edition",
            label = "Remove Edition",
            class = "btn-primary"
          ),
          tags$p(
            "Edit list of available tests. Changes do not persist across sessions, but if you have suggestions for permanent additions, feel free to email me at ",
            tags$a(
              href = "mailto:w.joel.schneider@gmail.com?subject=Composite%20IQ%20Calculator%20Suggestions",
              title = "Composite IQ Calculator Suggestions",
              "w.joel.schneider@gmail.com",
              .noWS = "outside"
            ),
            "."
          )
        ),
        ## edition ----
        datagridOutput2("grdEdition")
      )
    )
  ),

  ## entry ----
  nav_panel(
    "Data Entry",
    value = "score",
    status = "primary",
    div(
      style = "margin-top: 0px;",
      fluidRow(
        column(
          width = 6,
          textInput(
            "txtPerson",
            label = "Name (optional)",
            placeholder = "Person's Name",
            width = "100%"
          )
        ),
        column(
          width = 6,
          dateInput(
            "dateBirthdate",
            label = "Birthdate (required)",
            width = "100%"
          )
        )
      )
    ),
    fluidRow(column(
      width = 12,
      datagridOutput2("grdScore", height = "auto")
    )),
    fluidRow(
      column(
        width = 9,
        tags$p(
          tags$strong("Privacy Note: "),
          "All information entered here is private. Because this app is deployed via ",
          tags$a(
            href = "https://posit-dev.github.io/r-shinylive/",
            "shinylive",
            .noWS = "outside"
          ),
          ", the app runs entirely on your local machine in your browser's code sandbox. That is, once the app itself is downloaded from its host server, no information entered into this app is ever sent back to the server. Thus, no outside party, not even the app's developer, will ever have access to the data entered here."
        )
      ),
      column(
        width = 3,
        div(
          style = "display: flex; justify-content: flex-end;",
          actionButton(
            inputId = "add_score",
            label = "Add Test Score",
            class = "btn-primary"
          )
        )
      )
    )
  ),
  ## flynn ----
  nav_panel(
    "Norm Obsolescence",
    value = "flynn",
    div(
      style = "margin-top: 0px;",
      fluidRow(
        column(
          width = 6,
          fluidRow(
            style = "display: flex; align-items: flex-end;",
            column(
              width = 9,
              tags$p(
                "Select a row to see rules"
              )
            ),
            column(
              width = 3,
              div(
                style = "display: flex; justify-content: flex-end;",
                actionButton(
                  inputId = "add_flynn",
                  label = "Add Flynn Rule",
                  class = "btn-primary"
                ),
              )
            )
          ),
          div(style = "margin-top: 10px;", datagridOutput2("grdFlynn"))
        ),
        column(
          width = 6,
          fluidRow(
            style = "display: flex; align-items: flex-end;",
            column(
              width = 9,
              tags$p(
                "Selected rule:",
                shiny::textOutput("current_flynn", inline = TRUE)
              )
            ),
            column(
              width = 3,
              div(
                style = "display: flex; justify-content: flex-end;",
                actionButton(
                  inputId = "add_flynn_item",
                  label = "Add Effect",
                  class = "btn-primary"
                )
              )
            )
          ),
          div(style = "margin-top: 10px;", datagridOutput2("grdFlynnItem")),
        )
      )
    ),
  ),
)

# server ----
server <- function(input, output, session) {
  # constants ----

  my_primary <- "#1f6187"
  my_primary_medium <- tinter::lighten(my_primary, .5)
  my_primary_light <- tinter::lighten(my_primary, .2)
  my_primary_lightest <- tinter::lighten(my_primary, .1)

  set_grid_theme(
    row.even.background = my_primary_lightest,
    cell.normal.border = my_primary_light,
    cell.normal.showVerticalBorder = TRUE,
    cell.normal.showHorizontalBorder = TRUE,
    cell.header.background = my_primary,
    cell.header.text = "#FEFEFE",
    cell.selectedHeader.background = "black",
    cell.focused.border = my_primary
  )

  # helper functions ----

  battery_id_label <- function(x, d) {
    di <- tibble(battery_id = x) |>
      left_join(d, by = join_by(battery_id))
    l <- di$Battery
    if (length(l) > 0) {
      l
    } else {
      x
    }
  }

  flynn_id_label <- function(x, d) {
    di <- tibble(flynn_id = x) |>
      left_join(d, by = join_by(flynn_id))
    l <- di$Flynn
    if (length(l) > 0) {
      l
    } else {
      x
    }
  }

  edition_id_label <- function(x, d) {
    di <- tibble(edition_id = x) |>
      left_join(d, by = join_by(edition_id))
    l <- di$Edition
    if (length(l) > 0) {
      paste0(l, " (", di$Year_Published, ")")
    } else {
      x
    }
  }

  # reactive data ----
  rd_family <- reactiveVal(d_family)
  rd_battery <- reactiveVal(d_battery)
  rd_flynn <- reactiveVal(d_flynn)
  rd_edition <- reactiveVal(d_edition)
  rfamily_id <- reactiveVal(integer(0))
  redition_id <- reactiveVal(integer(0))
  rbattery_id <- reactiveVal(integer(0))
  rclicking <- reactiveVal(FALSE)

  # Grids ----
  ## family ----
  output$grdFamily <- renderDatagrid2({
    datagrid(
      rd_family(),
      sortable = FALSE,
      theme = "striped",
      filters = FALSE,
      data_as_input = TRUE,
      bodyHeight = "auto",
      minRowHeight = 30,
      rowHeight = 30
    ) %>%
      grid_columns(
        column = "family_id",
        hidden = TRUE
      ) %>%
      grid_columns("Family") %>%
      grid_click("family_click")
  })

  ### family click ----
  observeEvent(input$family_click, {
    req(input$family_click$row)
    req(input$grdFamily_data)
    id <- input$grdFamily_data[input$family_click$row, "family_id", drop = TRUE]
    if (isTruthy(id)) {
      rfamily_id(id)
    }
  })

  # observeEvent(input$edition_click, {
  #   req(input$edition_click$row)
  #   d_e <- input$grdEdition[input$edition_click$row, ]
  #   rbattery_id(d_e$battery_id)
  #   redition_id(d_e$edition_id)
  #   rfamily_id(d_e$family)
  #
  # })

  ### add edition ----
  observeEvent(input$`btn_add_edition`, {
    family_choices <- c(
      "",
      rd_family() |>
        select(Family, family_id) |>
        arrange(Family) |>
        deframe()
    )

    battery_choices <- c(
      "",
      rd_battery() |>
        select(Battery, battery_id) |>
        arrange(Battery) |>
        deframe()
    )
    f_selected <- NA
    b_selected <- NA
    if (length(rfamily_id()) > 0) {
      print("family")
      f_selected <- as.character(rfamily_id())
      print(f_selected)
    }
    print(input$edition_click$row)

    if (isTruthy(input$edition_click$row)) {
      dr <- input$grdEdition_data[input$edition_click$row, ]
      b_selected <- as.character(dr$battery_id)
      f_selected <- as.character(dr$family_id)
    }
    showModal(
      modalDialog(
        title = "Add New Test",
        selectizeInput(
          inputId = "family_new",
          width = "100%",
          label = "Test Family (e.g., Wechsler)",
          selected = f_selected,
          choices = family_choices,
          multiple = FALSE,
          options = list(
            persist = FALSE,
            create = TRUE,
            placeholder = "Select or add new test family"
          )
        ),
        hidden(
          div(
            id = "hidden_battery",
            selectizeInput(
              inputId = "battery_new",
              width = "100%",
              label = "Test Battery (e.g., Wechsler Adult Intelligence Scale)",
              selected = b_selected,
              choices = battery_choices,
              multiple = FALSE,
              options = list(
                persist = FALSE,
                create = TRUE,
                placeholder = "Select or add new test battery"
              )
            ),
            hidden(
              div(
                id = "hidden_edition",
                textInput(
                  "edition_new",
                  "Test Edition/Acronym (e.g., WAIS-5)",
                  width = "100%"
                ),
                fluidRow(
                  column(
                    width = 6,
                    numericInput(
                      "year_published_new",
                      "Year Published",
                      value = NA_integer_,
                      width = "100%",
                      step = 1
                    )
                  ),
                  column(
                    width = 6,
                    numericInput(
                      "year_normed_new",
                      "Year Normed (optional)",
                      value = NA_integer_,
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
                      value = NA_real_,
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
                )
              )
            )
          )
        ),

        textOutput('add_battery_error_msg'),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("add_battery_submit", "Add", class = "btn-primary")
        ),
        easyClose = TRUE
      )
    )
  })

  #### select family new ----
  observeEvent(input$family_new, {
    if (!isTruthy(input$family)) {
      hide("hidden_battery")
    }
    req(input$family_new)
    fid <- add_new_family(input$family_new)
    shinyjs::show("hidden_battery")

    new_battery_choices <- c(
      "",
      rd_battery() |>
        filter(family_id == fid) |>
        select(Battery, battery_id) |>
        deframe()
    )
    updateSelectizeInput(
      session,
      inputId = "battery_new",
      choices = new_battery_choices,
      selected = NA # Optional: Set a default selection
    )
  })

  #### select battery new ----
  observeEvent(input$battery_new, {
    if (!isTruthy(input$battery_new)) {
      shinyjs::hide("hidden_edition")
    } else {
      shinyjs::show("hidden_edition")
    }
  })

  add_new_family <- function(new_fam) {
    if (is_text_integer(new_fam)) {
      fid <- as.integer(new_fam)
    } else {
      fid <- rd_family()$family_id
      if (isTruthy(fid)) {
        fid <- max(fid) + 1L
      } else {
        fid <- 1L
      }

      rd_family(
        isolate(rd_family()) %>%
          add_row(family_id = fid, Family = new_fam) %>%
          arrange(Family)
      )
    }
    fid
  }

  add_new_battery <- function(new_batt, fid) {
    if (!(fid %in% rd_family()$family_id)) {
      stop("Family does not exist")
    }

    if (is_text_integer(new_batt)) {
      bid <- as.integer(new_batt)
    } else {
      bid <- rd_battery()$battery_id
      if (isTruthy(bid)) {
        bid <- max(bid) + 1L
      } else {
        bid <- 1L
      }

      rd_battery(
        isolate(rd_battery()) %>%
          add_row(battery_id = bid, Battery = new_batt, family_id = fid) %>%
          arrange(Battery)
      )
    }
    bid
  }

  ### add battery submit ----
  observeEvent(input$add_battery_submit, {
    req(input$battery_new)
    req(input$family_new)
    fid <- add_new_family(input$family_new)
    bid <- add_new_battery(input$battery_new, fid)

    rd_edition(
      isolate(
        rd_edition() |>
          add_row()
      )
    )

    removeModal()
  })

  ### add battery error ----
  output$add_battery_error_msg <- renderText({
    req(input$add_battery_submit)
    validate(
      need(input$battery_new != '', "You must enter a test battery name."),
      need(
        input$batteryfamily_new != '',
        "You must select or enter a test family name."
      )
    )
  })

  ## edition ----
  output$grdEdition <- renderDatagrid2({
    if (length(rfamily_id()) > 0) {
      d_e <- rd_edition() %>% filter(family_id == rfamily_id())
    } else {
      d_e <- rd_edition()
    }
    d_b <- isolate(rd_battery())
    datagrid(
      d_e,
      sortable = FALSE,
      theme = "striped",
      filters = FALSE,
      data_as_input = TRUE,
      minRowHeight = 30,
      rowHeight = 30
    ) |>
      grid_columns(columns = "Edition", header = "Edition") |>
      grid_columns(
        columns = "Year_Published",
        header = "Published",
        align = "center"
      ) |>
      grid_columns(
        columns = "Year_Normed",
        header = "Normed",
        align = "center"
      ) |>
      grid_columns(
        columns = c("Mean", "SD", "Reliability"),
        align = "center"
      ) |>
      grid_columns(
        columns = c("family_id", "edition_id"),
        hidden = TRUE
      ) %>%
      grid_columns(
        "battery_id",
        align = "left",
        header = "Battery",
        width = 400
      ) |>
      grid_format("battery_id", formatter = \(x, d) battery_id_label(x, d_b)) |>
      # grid_format("Reliability", formatter = \(x) {
      #   signs::signs(as.numeric(x), accuracy = .01, trim_leading_zeros = TRUE)
      # }) |>
      grid_editor_opts(editingEvent = "click") |>
      grid_click("edition_click")
  })

  ### add edition error ----
  output$add_edition_error_msg <- renderText({
    req(input$add_edition_submit)
    shiny::validate(
      shiny::need(
        input$family_new != '',
        'You must enter a test battery family.'
      ),
      shiny::need(
        input$battery_new != '',
        "You must enter a test battery name"
      ),
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
  ## update edition ----
  observeEvent(input$btn_update_edition, {
    req(input$edition_click$row)
    dr <- input$grdEdition_data[input$edition_click$row, ]
    showModal(
      modalDialog(
        title = "Update Test",
        textInput(
          "edition_new",
          "Test Edition/Acronym (e.g., WAIS-5)",
          width = "100%",
          value = dr$Edition
        ),
        fluidRow(
          column(
            width = 6,
            numericInput(
              "year_published_new",
              "Year Published",
              value = dr$Year_Published,
              width = "100%",
              step = 1
            )
          ),
          column(
            width = 6,
            numericInput(
              "year_normed_new",
              "Year Normed (optional)",
              value = dr$Year_Normed,
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
              value = dr$Reliability,
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
              value = dr$Mean,
              width = "100%",
              step = 1
            )
          ),
          column(
            width = 4,
            numericInput(
              "sd_new",
              "SD",
              value = dr$SD,
              width = "100%",
              step = 1
            ),
          )
        ),

        textOutput('add_battery_error_msg'),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("update_battery_submit", "Update", class = "btn-primary")
        ),
        easyClose = TRUE
      )
    )
  })

  ### update battery submit ----
  observeEvent(input$update_battery_submit, {
    req(redition_id())
    rd_edition(
      isolate(
        rd_edition() |>
          add_row()
      )
    )

    removeModal()
  })

  ## remove edition ----
  observeEvent(input$btn_remove_edition, {
    req(input$edition_click$row)
    dr <- input$grdEdition_data[input$edition_click$row, ]
    showModal(modalDialog(
      title = "Confirmation",
      paste0("Remove ", dr$Edition, "?"),
      footer = tagList(
        modalButton("No"),
        actionButton("btn_remove_edition_yes", "Yes", class = "btn-danger")
      )
    ))
  })

  observeEvent(input$btn_remove_edition_yes, {
    dr <- input$grdEdition_data[input$edition_click$row, ]
    rd_edition(
      isolate(rd_edition()) |>
        filter(edition_id != as.integer(dr$edition_id))
    )

    removeModal()
  })

  ## score ----

  set_score <- function(d_s, d_e) {
    if ("rowKey" %in% colnames(d_s)) {
      d_s$rowKey <- NULL
    }

    if (is.null(isolate(input$grdFlynn_data))) {
      d_f <- d_flynn
    } else {
      d_f <- input$grdFlynn_data
    }

    ed <- d_e |>
      arrange(Family, Battery, Year_Published) |>
      mutate(ed = paste0(Edition, " (", Year_Published, ")")) |>
      pull(ed)

    datagrid(
      d_s |> arrange(Date),
      theme = "striped",
      filters = FALSE,
      sortable = FALSE,
      data_as_input = TRUE,
      bodyHeight = "auto",
      minRowHeight = 30,
      rowHeight = 30
    ) |>
      grid_col_button(
        column = "score_id",
        inputId = "score_remove_row",
        label = "Remove",
        icon = icon("trash"),
        status = "primary",
        btn_width = "115px",
        align = "center",
        header = "Remove"
      ) |>
      grid_columns(
        columns = c("Score", "Date", "Weight"),
        align = "center"
      ) |>
      grid_columns("flynn_id", align = "left", header = "Flynn Rule") |>
      grid_format("flynn_id", \(x) flynn_id_label(x = x, d = d_f)) |>
      grid_columns("edition_id", header = "Test", align = "left") |>
      grid_format("edition_id", \(x) edition_id_label(x, d = d_e))
    # grid_editor(c("Score", "Weight"), type = "number") |>
    # grid_editor_date("Date", type = "date") |>
    # grid_editor_opts(editingEvent = "click")
  }

  rScore <- reactiveVal({
    set_score(d_score, d)
  })

  # switch panels ----
  observeEvent(input$mainPanel, {
    if (input$mainPanel == "score") {
      req(input$grdEdition_data)
      req(input$grdScore_data)
      rScore({
        set_score(
          isolate(input$grdScore_data) |> select(-rowKey),
          isolate(input$grdEdition_data)
        )
      })
    }
  })

  output$grdScore <- renderDatagrid2({
    input$mainPanel
    rScore()
  })

  ## add_score ----
  observeEvent(input$add_score, {
    if (is.null(input$grdEdition_data)) {
      d_e <- d
    } else {
      d_e <- input$grdEdition_data |>
        select(-rowKey)
    }

    if (is.null(input$grdFlynn_data)) {
      d_f <- d_flynn$flynn_id |> `names<-`(d_flynn$Flynn)
    } else {
      if (is.data.frame(input$grdFlynn_data)) {
        d_f <- input$grdFlynn_data$flynn_id |>
          `names<-`(input$grdFlynn_data$Flynn)
      } else {
        d_f <- c(Default = 1L)
      }
    }

    showModal(
      modalDialog(
        title = "Add Test Score",
        selectizeInput(
          "score_edition_new",
          "Test Edition/Acronym (e.g., WAIS-5)",
          width = "100%",
          choices = c(`Type or Select` = "", unique(d_e$Edition)),
          multiple = FALSE
        ),
        fluidRow(
          column(
            width = 4,
            numericInput(
              "score_new",
              "Score",
              value = 100,
              width = "100%",
              step = 1
            )
          ),
          column(
            width = 4,
            dateInput(
              "date_new",
              "Date Given",
              width = "100%"
            ),
          ),
          column(
            width = 4,
            numericInput(
              "weight_new",
              "Weight",
              value = 1,
              width = "100%",
              step = .1
            ),
          )
        ),
        fluidRow(
          selectizeInput(
            "flynn_score_new",
            "Flynn Effect Rule",
            width = "100%",
            choices = names(d_f),
            selected = names(d_f[1]),
            multiple = FALSE
          ),
        ),
        textOutput('add_score_error_msg'),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("add_score_submit", "Add", class = "btn-primary")
        ),
        easyClose = TRUE
      )
    )
  })

  observeEvent(input$add_score_submit, {
    req(input$score_edition_new)
    req(input$score_new)
    req(input$date_new)
    req(input$weight_new)
    req(input$flynn_score_new)
    d_f <- isolate(input$grdFlynn_data)
    if (is.null(d_f)) {
      f_id <- d_flynn$flynn_id |> `names<-`(d_flynn$Flynn)
    } else {
      if (is.data.frame(d_f)) {
        f_id <- d_f$flynn_id |> `names<-`(d_f$Flynn)
      } else {
        f_id <- c(Default = 1L)
      }
    }

    f_id <- f_id[input$flynn_score_new]
    names(f_id) <- NULL

    d_s <- isolate(input$grdScore_data)

    if (is.logical(d_s)) {
      new_score_id <- 1L
      d_s <- d_score %>% filter(FALSE)
    } else {
      if (nrow(d_s) > 0) {
        new_score_id <- max(d_s$score_id) + 1L
        d_s <- d_s |>
          select(-rowKey) |>
          mutate(Date = as.Date(Date))
      } else {
        new_score_id <- 1L
        d_s <- d_score %>% filter(FALSE)
      }
    }

    if (is.null(input$grdEdition_data)) {
      d_ed <- d
    } else {
      d_ed <- isolate(input$grdEdition_data) |>
        select(-rowKey)
    }

    ed_id <- d_ed |>
      filter(Edition == as.character(input$score_edition_new)) |>
      pull(edition_id)

    d_new <- d_score |>
      filter(FALSE) |>
      add_row(
        edition_id = ed_id,
        Score = input$score_new,
        Date = input$date_new,
        Weight = input$weight_new,
        flynn_id = f_id,
        score_id = new_score_id
      )

    rScore({
      set_score(
        d_s |>
          add_row(d_new),
        d_ed
      )
    })

    removeModal()
  })

  ### add score error ----
  output$add_score_error_msg <- renderText({
    req(input$add_score_submit)
    shiny::validate(
      shiny::need(
        input$edition_new != '',
        "You must enter a test edition name or acronym."
      ),
      shiny::need(input$score_new != '', "You must enter a test battery name."),
      shiny::need(lubridate::is.Date(input$date_new), "You must enter a date."),
      shiny::need(input$weight_new != '', "You must enter a date."),
      shiny::need(input$flynn_score_new != '', "You must enter a date."),
    )
  })

  ## remove flynn ----
  observeEvent(input$score_remove_row, {
    f_id <- as.integer(input$score_remove_row)

    data <- input$grdScore_data |>
      filter(score_id != f_id)

    if (is.null(input$grdEdition_data)) {
      d_ed <- d
    } else {
      d_ed <- isolate(input$grdEdition_data) |>
        select(-rowKey)
    }

    rScore(set_score(data, d_ed))
  })

  ## flynn ----
  output$grdFlynn <- renderDatagrid2({
    datagrid(
      d_flynn,
      sortable = FALSE,
      theme = "striped",
      filters = FALSE,
      data_as_input = TRUE,
      bodyHeight = "auto",
      minRowHeight = 30,
      rowHeight = 30
    ) |>
      grid_col_button(
        column = "flynn_id",
        inputId = "flynn_remove_row",
        label = "Remove",
        icon = icon("trash"),
        status = "primary",
        btn_width = "115px",
        align = "center",
        header = "Remove"
      ) |>
      grid_columns(
        columns = "Flynn",
        # sortable = TRUE,
        minWidth = 300
      ) |>
      grid_editor(column = "Flynn", type = "text") %>%

      # grid_editor_opts(editingEvent = "click") |>
      grid_click("flynn_click")
    # grid_selection_row(inputId = "flynn_row", type = "radio")
  })

  # observeEvent(input$flynn_click, {
  #   print(input$flynn_click)
  # })
  #
  #   output$print_data <- renderPrint({
  #     # This reactive value automatically refreshes whenever a cell change completes
  #     req(input$gridFlynn_data)
  # print("hello")
  #     # input$my_grid_data contains the modified data frame
  #     input$my_grid_data
  #   })

  # set_flynn <- function() {
  #   print("set_flynn")
  #   req(input$grdFlynn_data)
  #   if (isTruthy(input$grdFlynn_data)) {
  #     rd_flynn(input$grdFlynn_data)
  #   }
  #
  # }

  current_flynn_row <- reactiveVal(1L)

  ### flynn rule text ----
  output$current_flynn <- renderText({
    req(input$mainPanel)
    if (!all(is.logical(input$grdFlynn_data))) {
      input$grdFlynn_data$Flynn[current_flynn_row()]
    } else {
      "Default"
    }
  })

  # add_flynn ----
  observeEvent(input$add_flynn, {
    showModal(
      modalDialog(
        title = "Add Flynn Effect Rule",
        textInput(
          "flynn_new",
          "Name of Flynn Effect Rule",
          width = "100%"
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("add_flynn_submit", "Add", class = "btn-primary")
        ),
        easyClose = TRUE
      )
    )
  })
  ### add_flynn submit ----
  observeEvent(input$add_flynn_submit, {
    req(input$flynn_new)
    if (is.logical(input$grdFlynn_data)) {
      new_flynn_id <- 1L
    } else {
      new_flynn_id <- max(input$grdFlynn_data$flynn_id) + 1L
    }

    d_new <- d_flynn |>
      filter(FALSE) |>
      add_row(
        Flynn = input$flynn_new,
        flynn_id = new_flynn_id
      )

    grid_proxy_add_row(proxy = "grdFlynn", data = d_new)

    removeModal()
  })

  ## flynn item ----
  set_flynn_item <- function(dfi, id) {
    datagrid(
      dfi |> filter(flynn_id == id),
      sortable = FALSE,
      theme = "striped",
      filters = FALSE,
      data_as_input = TRUE,
      bodyHeight = "auto",
      minRowHeight = 30,
      rowHeight = 30
    ) |>
      grid_col_button(
        column = "flynn_item_id",
        inputId = "flynn_item_remove_row",
        label = "Remove",
        icon = icon("trash"),
        status = "primary",
        btn_width = "115px",
        align = "center",
        header = "Remove"
      ) |>
      grid_columns(columns = "Until", align = "center") |>
      grid_columns(columns = "Effect", align = "center") |>
      grid_format("Effect", scales::label_number(accuracy = .01)) |>
      grid_format("Until", \(x) {
        ifelse(is.na(x), "Now", x)
      }) |>
      # grid_editor(column = "Until",
      #             type = "number") |>
      # grid_editor(column = "Effect",
      #             type = "number") |>
      grid_columns("flynn_id", hidden = TRUE)

    # grid_editor_opts(editingEvent = "click") |>
    # grid_click("flynn_item_click")
  }

  rFlynnItem <- reactiveVal(
    set_flynn_item(d_flynn_item, 1L)
  )

  output$grdFlynnItem <- renderDatagrid2({
    # req(input$flynn_click)
    req(input$grdFlynn_data)
    rFlynnItem()
  })

  ## remove flynn ----
  observeEvent(input$flynn_remove_row, {
    data <- input$grdFlynn_data
    f_id <- as.integer(input$flynn_remove_row)
    rowKey <- data$rowKey[data$flynn_id == f_id]
    grid_proxy_delete_row("grdFlynn", rowKey)
    d_flynn_item <<- d_flynn_item |>
      filter(flynn_id != f_id)
    rFlynnItem(set_flynn_item(d_flynn_item, 0L))
  })
  ### flynn click ----
  observeEvent(input$flynn_click, {
    selected_row <- input$flynn_click$row
    if (is.null(selected_row)) {
      rFlynnItem(set_flynn_item(d_flynn_item, 0L))
      current_flynn_row(0L)
    } else {
      selected_f_id <- input$grdFlynn_data$flynn_id[selected_row]

      rFlynnItem(set_flynn_item(d_flynn_item, selected_f_id))
      current_flynn_row(selected_f_id)
    }
  })

  # add flynn item ----
  observeEvent(input$add_flynn_item, {
    showModal(
      modalDialog(
        title = "Add Flynn Effect",
        numericInput(
          inputId = "until_new",
          label = "Until Year (leave blank if rule does not expire)",
          value = NA_real_,
          width = "100%"
        ),
        numericInput(
          inputId = "effect_new",
          label = "Effect size (points per decade)",
          value = 2.94,
          width = "100%"
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("add_flynn_item_submit", "Add", class = "btn-primary")
        ),
        easyClose = TRUE
      )
    )
  })

  observeEvent(input$add_flynn_item_submit, {
    req(input$effect_new)
    req(input$flynn_click$row)

    if (nrow(d_flynn_item) == 0) {
      new_flynn_item_id <- 1L
    } else {
      new_flynn_item_id <- max(d_flynn_item$flynn_item_id) + 1L
    }

    new_flynn_id <- input$grdFlynn_data[input$flynn_click$row, "flynn_id"]

    if (is.na(input$until_new)) {
      d_flynn_item <<- d_flynn_item |>
        filter_out(is.na(Until) & (flynn_id == new_flynn_id))
    } else {
      d_flynn_item <<- d_flynn_item |>
        filter_out((Until == input$until_new) & (flynn_id == new_flynn_id))
    }

    d_flynn_item <<- d_flynn_item |>
      add_row(
        Until = input$until_new,
        Effect = input$effect_new,
        flynn_id = new_flynn_id,
        flynn_item_id = new_flynn_id
      ) |>
      arrange(Until)

    rFlynnItem(set_flynn_item(d_flynn_item, new_flynn_id))

    removeModal()
  })

  ## remove flynn item ----
  observeEvent(input$flynn_item_remove_row, {
    req(input$flynn_click$row)
    data <- input$grdFlynnItem_data
    fi_id <- as.integer(input$flynn_item_remove_row)
    d_flynn_item <<- d_flynn_item |>
      filter(flynn_item_id != fi_id)
    f_id <- input$grdFlynn_data[input$flynn_click$row, "flynn_id"]
    rFlynnItem(set_flynn_item(d_flynn_item, f_id))
  })
}

shinyApp(ui, server)
