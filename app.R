
options(warn = 2)
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
library(scales)


options(shiny.useragg = TRUE, shiny.launch.browser = .rs.invokeShinyWindowExternal)

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

d <- readr::read_csv("battery.csv", show_col_types = FALSE) |>
  mutate(family_id = as.integer(factor(Family)),
         battery_id = as.integer(factor(Battery))) |>
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
  select(Family, Battery, battery_id) |>
  unique() |>
  arrange(Family, Battery)

d_edition <- d |>
  select(-Family, -Battery)

d_score <- tibble(
  edition_id = 1L,
  Score = 100,
  Date = Sys.Date(),
  Weight = 1,
  flynn_id = 1L,
  score_id = 1L
)



library(toastui)
library(shiny)
# ui ----
ui <- page_navbar(
  title = "Composite IQ Calculator",
  window_title = "Composite IQ Calculator",
  theme = bs_theme(),
  id = "mainPanel",
  header = tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "main.css")
  ),

  ## entry ----
  nav_panel(
    "Data Entry",
    value = "score",
    status = "primary",
    div(style = "margin-top: 0px;", fluidRow(
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
    fluidRow(column(
      width = 9,
      tags$p(
        tags$strong("Privacy Note: "),
        "All information entered here is private. Because this app is deployed via ",
        tags$a(href = "https://posit-dev.github.io/r-shinylive/", "shinylive", .noWS = "outside"),
        ", the app runs entirely on your local machine in your browser's code sandbox. That is, once the app itself is downloaded from its host server, no information entered into this app is ever sent back to the server. Thus, no outside party, not even the app's developer, will ever have access to the data entered here."
      )
    ), column(
      width = 3,
      div(
        style = "display: flex; justify-content: flex-end;",
        actionButton(
          inputId = "add_score",
          label = "Add Test Score",
          class = "btn-primary"
        )
      )
    ))

  ),
  ## battery ----
  nav_panel(
    "Edit Test List",
    value = "edition",
    fluidRow(column(
      width = 6,
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
    ), column(
      width = 6,
      div(
        style = "display: flex; justify-content: flex-end;",
        actionButton(
          inputId = "add_edition",
          label = "Add Test Battery",
          class = "btn-primary"
        )
      )
    )),
    fluidRow(column(width = 12, datagridOutput2("grdEdition")))

  ),
  ## flynn ----
  nav_panel(
    "Norm Obsolescence",
    value = "flynn",
    div(style = "margin-top: 0px;", fluidRow(
      column(
        width = 6,
        fluidRow(style = "display: flex; align-items: flex-end;",
                 column(width = 9, tags$p(
          "Select a row to see rules"
        )), column(
          width = 3,
          div(
            style = "display: flex; justify-content: flex-end;",
            actionButton(
              inputId = "add_flynn",
              label = "Add Flynn Rule",
              class = "btn-primary"
            ),
          )
        )),
        div(style = "margin-top: 10px;", datagridOutput2("grdFlynn"))
      ),
      column(
        width = 6,
        fluidRow(
          style = "display: flex; align-items: flex-end;",
          column(width = 9, tags$p(
            "Selected rule:",
            shiny::textOutput("current_flynn", inline = TRUE)
          )),
          column(
            width = 3,
            div(
              style = "display: flex; justify-content: flex-end;",
            actionButton(
              inputId = "add_flynn_item",
              label = "Add Effect",
              class = "btn-primary"
            )
          ))

        ),
        div(style = "margin-top: 10px;", datagridOutput2("grdFlynnItem")),

      )
    )),
  ),
)

# server ----
server <- function(input, output, session) {

  label_id <- function(x, d, id = "id", value = "value") {
    l <- d[d[, id] == x,][[value]]
    if (length(l) > 0) {
      l
    } else {
      x
    }
  }

  edition_id_label <- function(x, d) {
    di <- d |>
      filter(edition_id %in% x)
    l <- di$Edition
    if (length(l) > 0) {
      paste0(l, " (", di$Year_Published, ")")
    } else {
      x
    }
  }

  d_flynn <- tibble(Flynn = c("Default",
                              "Always 2.94"),
                    flynn_id = 1:2)

  d_flynn_item <- tibble(
    Until = c(2007L, NA,NA),
    Effect = c(2.94, 1.3, 2.94),
    flynn_id = c(1L,1L, 2L),
    flynn_item_id = 1:3)
  ## edition ----
  output$grdEdition <- renderDatagrid2({
    datagrid(
      d,
      sortable = FALSE,
      pagination = 15,
      theme = "striped",
      filters = FALSE,
      data_as_input = TRUE
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
                   # sortable = TRUE,
                   minWidth = 300) |>
      grid_columns(columns = "Family") |>
      grid_columns(columns = "Edition",
                   # sortable = TRUE,
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


    datagrid(d_s,
             theme = "striped",
             filters = FALSE, sortable = FALSE, data_as_input = TRUE, bodyHeight = "auto") |>
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
      grid_format("flynn_id", \(x) label_id(x, id = "flynn_id", value = "Flynn", d = d_f)) |>
      grid_columns("edition_id", header = "Test", align = "left") |>
      grid_format("edition_id", \(x) edition_id_label(x, d = d_e)) |>
      grid_editor(c("Score", "Weight"), type = "number") |>
      grid_editor_date("Date", type = "date") |>
      grid_editor_opts(editingEvent = "click")
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
        set_score(isolate(input$grdScore_data) |> select(-rowKey), isolate(input$grdEdition_data))
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
        d_f <- input$grdFlynn_data$flynn_id |> `names<-`(input$grdFlynn_data$Flynn)
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
        textOutput('score_error_msg'),
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
    if (nrow(d_s) > 0) {
      new_score_id <- max(d_s$score_id) + 1L
    } else {
      new_score_id <- 1L
    }

    if (is.null(input$grdEdition_data)) {
      d_ed <- d
    } else {
      d_ed <- isolate(input$grdEdition_data) |>
        select(-rowKey)
    }
    print("hello")

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
      set_score(d_s |>
                  select(-rowKey) |>
                  mutate(Date = as.Date(Date)) |>
                  add_row(d_new), d_ed)
    })

    removeModal()
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
      bodyHeight = "auto"
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
      grid_columns(columns = "Flynn",
                   # sortable = TRUE,
                   minWidth = 300) |>
      grid_editor(column = "Flynn",
                  type = "text",
                  validation = validateOpts(unique = TRUE)) |>

      # grid_editor_opts(editingEvent = "click") |>
      grid_click("flynn_click")
      # grid_selection_row(inputId = "flynn_row", type = "radio")
  })

  output$current_flynn <- renderText({
    req(input$flynn_click)
    if (!all(is.logical(input$grdFlynn_data))) {
      input$grdFlynn_data$Flynn[input$flynn_click$row]
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
          bodyHeight = "auto"
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
          grid_editor(column = "Until",
                      type = "number") |>
          grid_editor(column = "Effect",
                      type = "number") |>
          grid_columns("flynn_id", hidden = TRUE) |>

          grid_editor_opts(editingEvent = "click") |>
          grid_click("flynn_item_click")
  }

  rFlynnItem <- reactiveVal(
    set_flynn_item(d_flynn_item, 1L)
    )

  output$grdFlynnItem <- renderDatagrid2({
    req(input$flynn_click)
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

  observeEvent(input$flynn_click, {
    selected_row <- input$flynn_click$row
    if (is.null(selected_row)) {
      rFlynnItem(set_flynn_item(d_flynn_item, 0L))
    } else {
      selected_f_id <- input$grdFlynn_data$flynn_id[selected_row]

      rFlynnItem(set_flynn_item(d_flynn_item, selected_f_id ))


    }
  })
  # add flynn item ----
  observeEvent(input$add_flynn_item, {
    showModal(
      modalDialog(
        title = "Add Flynn Effect",
        numericInput(inputId = "until_new",
                     label = "Until Year (leave blank if rule does not expire)",
                     value = NA_real_,
                     width = "100%"
        ),
        numericInput(inputId = "effect_new",
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

    rFlynnItem(set_flynn_item(d_flynn_item, new_flynn_id ))



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