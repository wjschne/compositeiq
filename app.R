# packages ----

options(warn = 1)
library(conflicted)
conflicts_prefer(
  dplyr::filter,
  dplyr::lag,
  dplyr::intersect,
  dplyr::setdiff,
  dplyr::setequal,
  dplyr::union,
  shinyjs::show,
  .quiet = TRUE
)

library(shiny)
library(bsicons)
library(reactable)
library(shinyjs)
library(keys)
library(dplyr)
library(tibble)
library(readr)
library(bslib)
library(scales)
library(brand.yml)
library(lubridate)
library(htmltools)
library(purrr)
library(shinyvalidate)
library(shinyMatrix)
library(tidyr)
library(curl)
library(stringr)
library(fresh)


# constants ####
my_primary <- "#1f6187"
my_primary_medium <- tinter::lighten(my_primary, .5)
my_primary_light <- tinter::lighten(my_primary, .2)
my_primary_lightest <- tinter::lighten(my_primary, .1)

ymd(NA)

# helper functions ----

prob_label <- function(
  p,
  accuracy = 0.01,
  digits = NULL,
  max_digits = NULL,
  remove_leading_zero = TRUE,
  round_zero_one = TRUE
) {
  if (is.null(digits)) {
    l <- scales::number(p, accuracy = accuracy)
  } else {
    sig_digits <- abs(ceiling(log10(p + p / 1e+09)) - digits)
    pgt99 <- p > 0.99
    sig_digits[pgt99] <- abs(
      ceiling(log10(1 - p[pgt99])) -
        digits +
        1
    )
    sig_digits[
      ceiling(log10(p)) == log10(p) & (-log10(p) >= digits)
    ] <- sig_digits[
      ceiling(log10(p)) == log10(p) &
        (-log10(p) >= digits)
    ] -
      1
    sig_digits[is.infinite(sig_digits)] <- 0
    l <- purrr::map2_chr(p, sig_digits, formatC, format = "f", flag = "#")
  }
  if (remove_leading_zero) {
    l <- sub("^-0", "-", sub("^0", "", l))
  }
  if (round_zero_one) {
    l[p == 0] <- "0"
    l[p == 1] <- "1"
    l[p == -1] <- "-1"
  }
  if (!is.null(max_digits)) {
    if (round_zero_one) {
      l[round(p, digits = max_digits) == 0] <- "0"
      l[round(p, digits = max_digits) == 1] <- "1"
      l[round(p, digits = max_digits) == -1] <- "-1"
    } else {
      l[round(p, digits = max_digits) == 0] <- paste0(
        ".",
        paste0(rep("0", max_digits), collapse = "")
      )
      l[round(p, digits = max_digits) == 1] <- paste0(
        "1.",
        paste0(rep("0", max_digits), collapse = "")
      )
      l[round(p, digits = max_digits) == -1] <- paste0(
        "-1.",
        paste0(rep("0", max_digits), collapse = "")
      )
    }
  }
  l <- sub(pattern = "-", replacement = "−", x = l)
  Encoding(l) <- "UTF-8"
  dim(l) <- dim(p)
  l
}

make_actions <- function(row_id, tbl, name, disabled_edit, disabled_delete) {
  if (disabled_edit) {
    disabled_edit <- NA
  } else {
    disabled_edit <- NULL
  }

  if (disabled_delete) {
    disabled_delete <- NA
  } else {
    disabled_delete <- NULL
  }

  as.character(htmltools::tagList(
    htmltools::tags$button(
      class = paste0(
        "btn btn-outline-primary btn-sm",
        ifelse(is.na(disabled_edit), " border-0", "")
      ),
      disabled = disabled_edit,
      onclick = paste0(
        "Shiny.setInputValue('",
        tbl,
        "_edit_row', ",
        row_id,
        ", {priority: 'event'})"
      ),
      bs_icon("pencil", size = "1em", title = paste0("Edit ", name))
    ),
    htmltools::tags$button(
      class = paste0(
        "btn btn-outline-danger btn-sm",
        ifelse(is.na(disabled_delete), " border-0", "")
      ),
      disabled = disabled_delete,
      onclick = paste0(
        "Shiny.setInputValue('",
        tbl,
        "_delete_row', ",
        row_id,
        ", {priority: 'event'})"
      ),
      bs_icon("trash", size = "1em", title = paste0("Delete ", name))
    )
  ))
}

new_id <- function(d, id) {
  ids <- d[[id]]
  if (isTruthy(ids)) {
    nid <- max(ids) + 1L
  } else {
    nid <- 1L
  }
  nid
}

my_onclick <- JS(
  "function(rowInfo, column, state) {
    // 1. Condition based on column ID
    if (column.id === '.selection' || column.id === '.expander' || column.id === 'Actions') {
          return;
    }
        rowInfo.toggleRowSelected();

  }"
)

mygridtheme <- list(
  color = "#111",
  selectionBgColor = "#333",
  header_style = list(color = "red")
)
is_text_integer <- function(x) {
  grepl("^[+-]?[0-9]+$", x)
}

long_r <- function(
  age,
  interval,
  different = FALSE,
  b0 = .716,
  b1 = .003,
  b2 = -.258,
  b3 = 0,
  b4 = -.095,
  b5 = -.138,
  b6 = -.104
) {
  age <- age - 20
  interval <- interval - 5
  different <- 1 * different
  b0 -
    b1 * exp(b2 * age + b3 * age * interval) -
    b4 * exp(b5 * interval) +
    b6 * different
}


options(
  shiny.useragg = TRUE
  # shiny.launch.browser = .rs.invokeShinyWindowExternal
)

# data ####

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
  select(family_id, Family) |>
  unique() |>
  arrange(Family)

d_battery <- d |>
  arrange(Family, Battery) %>%
  select(battery_id, Battery, family_id) |>
  unique()


d_edition <- d |>
  select(-Family, -Battery) |>
  select(edition_id, Edition, everything())

d_score <- tibble(
  score_id = 1:3,
  Score = c(75, 70, 72),
  Date = (as.Date(c("2026-06-02", "2021-04-05", "2024-01-15"))),
  Weight = 1,
  edition_id = c(54L, 66L, 16L),
  flynn_id = 1L
) |>
  arrange(Date)

d_flynn <- tibble(Flynn = c("Default", "Always 2.94"), flynn_id = 1:2)

d_flynn_item <- tibble(
  Until = c(2007L, NA, NA),
  Effect = c(2.94, 1.3, 2.94),
  flynn_id = c(1L, 1L, 2L),
  flynn_item_id = 1:3
)

# library(dm)
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
#   dm_add_fk(FlynnItem, flynn_id, Flynn) |>
#   dm_add_uk(FlynnItem, c(flynn_id, Until), check = TRUE)
#
# db %>% dm::dm_examine_constraints()

# dm_validate(db)
# ui ----
ui <- page_navbar(
  title = "Composite IQ Calculator",
  window_title = "Composite IQ Calculator",
  theme = bs_theme(
    brand = TRUE,
    bootswatch = "minty",
    version = 5,
    `tooltip-bg` = my_primary,
    `tooltip-opacity` = 1,
    `tooltip-border-radius` = "6px",
    `tooltip-padding-y` = "10px",
    `tooltip-padding-x` = "12px"
  ),
  id = "mainPanel",
  header = tagList(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "main.css"),
      useShinyjs(),
      useKeys(),
      keysInput("enter_key", "enter", global = TRUE),
      use_googlefont("Roboto Condensed")
    )
  ),
  ## score ----
  nav_panel(
    "Data Entry",
    value = "score",
    status = "primary",
    div(
      style = "margin-top: 0px;",
      fluidRow(
        column(
          width = 3,
          textInput(
            "txtPerson",
            label = tagList(
              "Name (optional)",
              tooltip(
                bs_icon(
                  "info-circle-fill",
                  class = "text-info"
                ),
                tagList(
                  p("This information is used to create a report."),
                  "Like all other information in this app, the",
                  strong("Name"),
                  "is private because it stays locally on your machine. It is never sent to a third-party server."
                )
              )
            ),
            placeholder = "Person's Name",
            width = "100%"
          )
        ),
        column(
          width = 3,
          dateInput(
            "dateBirthdate",
            label = tagList(
              "Birthdate (YYYY-MM-DD)",
              tooltip(
                bs_icon(
                  "info-circle-fill",
                  class = "text-info"
                ),
                tagList(
                  p(
                    "This information is used to calculate the person's age at the time of testing, which is then use to estimate the correlations among the test scores."
                  ),

                  "Like all other information in this app, the",
                  strong("Birthdate"),
                  "is private because it stays locally on your machine. It is never sent to a third-party server."
                )
              )
            ),
            width = "100%",
            value = as.Date("2015-04-15")
          )
        ),
        column(
          width = 3,
          numericInput(
            "defaultReliability",
            "Default Reliability",
            min = 0,
            max = 1,
            step = 0.01,
            value = 0.96
          )
        ),
        column(
          width = 3,
          style = "display: flex; justify-content: flex-end; flex-direction: row; align-items: flex-end;",
          div(
            # style = "display: flex; justify-content: flex-end; flex-direction: row; align-items: flex-end;",
            actionButton(
              inputId = "add_score",
              label = "Add Test Score",
              class = "btn-primary"
            )
          )
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        reactableOutput("grdScore", height = "auto")
      )
    ),
    h4("Composite IQ"),
    div(
      reactableOutput("grdIQ", height = "auto")
    )
  ),
  ## correlations ----
  nav_panel(
    "Correlations",
    value = "correlations",
    p(
      "Estimated correlations can be overridden by changing values in the lower half of the matrix."
    ),
    fluidRow(column(
      12,
      uiOutput("tbl_cor")
    )),
    p(
      em("Note."),
      "Initial correlations are estimated from this equation adapted from",
      tooltip(
        trigger = tagList(
          tags$a(
            href = "https://doi.org/10.1037/bul0000425",
            "Breit, Scherrer, Tucker-Drob, and Preckel (2024)",
            .noWS = "after"
          ),
          ":"
        ),
        tagList(
          "Breit, M., Scherrer, V., Tucker-Drob, E. M., & Preckel, F. (2024). The stability of cognitive abilities: A meta-analytic review of longitudinal studies.",
          tags$em("Psychological Bulletin, 150", .noWS = "after"),
          "(4), 399–439."
        )
      )
    ),
    tags$img(
      src = "stability_equation.svg",
      width = 800,
      style = "background-color: white;"
    )
  ),
  ## family ----
  nav_panel(
    "Edit Battery",
    value = "battery",
    # page_fillable(
    layout_sidebar(
      # border = FALSE,
      sidebar = sidebar(
        open = "always",
        width = 425,
        div(
          actionButton(
            inputId = "add_family",
            label = "Add Test Family",
            class = "btn btn-primary"
          )
        ),
        reactableOutput("grdFamily")
      ),
      tags$div(
        style = "display: flex; gap: 10px;",
        actionButton(
          "add_edition",
          label = "Add Edition",
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
      reactableOutput("grdEdition")
    )
    # )
  ),
  ## flynn ----
  nav_panel(
    "Norm Obsolescence",
    value = "flynn",
    layout_sidebar(
      sidebar = sidebar(
        open = "always",
        width = 400,
        div(
          actionButton(
            inputId = "add_flynn",
            label = "Add Flynn Rule",
            class = "btn btn-primary"
          )
        ),
        reactableOutput("grdFlynn")
      ),
      div(
        style = "display: flex; gap: 10px;",
        shiny::uiOutput("ph_add_flynn_item"),
        textOutput("FlynnSelected")
      ),
      div(
        reactableOutput("grdFlynnItem")
      )
    )
  ),
  nav_spacer(),
  nav_item(
    tooltip(
      span("Privacy Note", bs_icon("info-circle-fill", class = "text-info")),
      tagList(
        h4("Privacy", class = "text-white"),
        p(
          "All information entered into this app is private."
        ),
        span(
          "Privacy is assured because the app is deployed via ",
          tags$a(
            href = "https://posit-dev.github.io/r-shinylive/",
            "shinylive",
            .noWS = "outside",
            style = "color: white"
          ),
          ", meaning that the app runs entirely on your local machine in your browser's code sandbox. That is, once the app itself is downloaded from its host server, no information entered into this app is ever sent back to the server. Thus, no outside party, not even the app's developer, will ever have access to the data entered here."
        )
      ),
      options = list(delay = list(show = 100, hide = 800))
    )
  )
)


# server ----
server <- function(input, output, session) {
  # constants ----
  # set_grid_theme(
  #   row.even.background = my_primary_lightest,
  #   cell.normal.border = my_primary_light,
  #   cell.normal.showVerticalBorder = TRUE,
  #   cell.normal.showHorizontalBorder = TRUE,
  #   cell.header.background = my_primary,
  #   cell.header.text = "#FEFEFE",
  #   cell.selectedHeader.background = "black",
  #   cell.focused.border = my_primary
  # )

  myreactabletheme <- reactableTheme(
    borderColor = my_primary_light,
    stripedColor = my_primary_lightest,
    highlightColor = my_primary_medium,
    cellPadding = "4px 4px",
    style = list(
      fontFamily = "Roboto Condensed, Arial, sans-serif"
    ),
    headerStyle = list(
      backgroundColor = my_primary,
      color = "white"
    )
  )

  # validatation rules ----

  ## valid person ----
  iv_person <- InputValidator$new()
  iv_person$add_rule(
    "dateBirthdate",
    sv_required("Required. Approximate, if unknown.")
  )
  iv_person$enable()

  ## valid family ----
  iv_family_add <- InputValidator$new()
  iv_family_add$add_rule("family_add_new", sv_required())

  ## valid edition ----
  iv_edition_add <- InputValidator$new()
  iv_edition_add$add_rule("family_new", sv_required())
  iv_edition_add$add_rule("battery_new", sv_required())
  iv_edition_add$add_rule("edition_new", sv_required())
  iv_edition_add$add_rule("mean_new", sv_required())
  iv_edition_add$add_rule("sd_new", sv_required())
  iv_edition_add$add_rule("year_normed_new", sv_optional())
  iv_edition_add$add_rule("reliability_new", sv_optional())
  iv_edition_add$add_rule("year_published_new", sv_required())
  iv_edition_add$add_rule("reliability_new", sv_between(0, 1))
  iv_edition_add$add_rule(
    "year_published_new",
    sv_between(
      left = 1904L,
      right = 2126L,
      message_fmt = "Implausible Year"
    )
  )
  iv_edition_add$add_rule(
    "year_normed_new",
    sv_between(
      left = 1904L,
      right = 2126L,
      message_fmt = "Implausible Year"
    )
  )

  ## valid flynn ----
  iv_flynn <- InputValidator$new()
  iv_flynn$add_rule("flynn_add_new", sv_required())

  iv_flynn_item <- InputValidator$new()
  iv_flynn_item$add_rule("flynn_until_new", sv_required())
  iv_flynn_item$add_rule("flynn_effect_new", sv_required())

  ## valid score ----
  iv_score <- InputValidator$new()
  iv_score_edit <- InputValidator$new()
  iv_score_edit$add_rule("score_new", sv_required())
  iv_score_edit$add_rule("date_new", sv_required())
  iv_score_edit$add_rule("score_flynn_new", sv_required())
  iv_score_edit$add_rule("weight_new", sv_required())
  iv_score$add_rule("score_edition_new", sv_required())
  iv_score$add_rule("score_edition_new", sv_not_equal("NA", "Required"))
  iv_score_edit$add_rule("date_new", function(value) {
    if (isTruthy(input$score_edition_new)) {
      pd <- rd_edition() |>
        filter(edition_id == input$score_edition_new) |>
        pull(Year_Published)
      if (year(value) < pd) {
        "Date Given is before test's publication."
      }
    }
  })
  iv_score_edit$add_rule(
    "date_new",
    sv_gt(
      isolate(input$dateBirthdate),
      "Date is before the person's birthdate."
    )
  )
  iv_score$add_validator(iv_score_edit)

  # reactive data ----
  rd_current <- reactiveVal()
  cor_n <- reactiveVal(0L)
  r_row <- reactiveVal(list(
    id = integer(0),
    name = character(0),
    data = tibble()
  ))
  current_flynn_row <- reactiveVal(1L)
  rd_family <- reactiveVal(d_family)
  rd_battery <- reactiveVal(d_battery)
  rd_flynn <- reactiveVal(d_flynn)
  rd_flynn_item <- reactiveVal(d_flynn_item)
  rd_edition <- reactiveVal(d_edition)
  rd_score <- reactiveVal(d_score)
  rfamily_id <- reactive({
    rd_family() |>
      slice(getReactableState("grdFamily", "selected")) |>
      pull(family_id)
  })
  current_family_row <- reactiveVal(1L)
  redition_id <- reactiveVal(integer(0))
  rbattery_id <- reactiveVal(integer(0))
  rflynn_id <- reactive({
    d_f <- rd_flynn() |>
      slice(getReactableState("grdFlynn", "selected"))

    currentFlynn <- d_f$Flynn
    if (isTruthy(currentFlynn)) {
      currentFlynn <- paste0("Selected Flynn Rule: ", currentFlynn)
    } else {
      currentFlynn <- "Select a Flynn Rule"
    }

    output$FlynnSelected <- renderText(currentFlynn)

    output$ph_add_flynn_item <- renderUI({
      if (nrow(d_f) > 0) {
        actionButton(
          inputId = "add_flynn_item",
          label = "Add Flynn Effect",
          class = "btn btn-primary"
        )
      } else {
        return(NULL)
      }
    })

    d_f |>
      pull(flynn_id)
  })

  # functions ----
  flynn_correction <- function(
    score,
    publication_year,
    norm_year,
    administration_date,
    iflynn_id = 1L
  ) {
    correction <- rd_flynn_item() |>
      dplyr::filter(flynn_id == iflynn_id) |>
      mutate(Until = ifelse(is.na(Until), Inf, Until)) |>
      mutate(
        norm_year = ifelse(is.na(norm_year), publication_year - 1L, norm_year)
      ) |>
      filter(Until > norm_year) |>
      arrange(Until) |>
      mutate(From = dplyr::lag(Until, default = -Inf), .before = Until) |>
      mutate(From = ifelse(is.infinite(From), norm_year, From)) |>
      mutate(
        administration_date = administration_date,
        date_from = ymd(From * 10000 + 101),
        date_until = ymd(ifelse(is.infinite(Until), NA, Until) * 10000 + 101)
      ) |>
      mutate(
        date_until = if_else(
          is.na(date_until),
          administration_date,
          date_until
        ),
        years_elapsed = interval(date_from, date_until) / dyears(1),
        correction = years_elapsed * Effect / -10
      ) |>
      filter(years_elapsed >= 0) |>
      pull(correction) |>
      sum()
    score + correction
  }

  # Grids ----

  ## family ----
  output$grdFamily <- renderReactable({
    current_data <- rd_family()
    current_data$Actions <- pmap_chr(
      tibble(
        row_id = current_data$family_id,
        name = current_data$Family,
        tbl = "family",
        disabled_edit = FALSE,
        disabled_delete = FALSE
      ),
      make_actions
    )

    reactable(
      current_data,
      showSortIcon = FALSE,
      pagination = FALSE,
      sortable = FALSE,
      striped = TRUE,
      defaultSelected = current_family_row(),
      selection = "single",
      onClick = my_onclick,
      theme = myreactabletheme,
      highlight = TRUE,
      columns = list(
        Family = colDef("Family"),
        family_id = colDef(show = FALSE),
        Actions = colDef(html = TRUE, sortable = FALSE, align = "center")
      )
    )
  })

  ### remove family ----
  observeEvent(input$family_delete_row, {
    req(input$family_delete_row)
    dr <- rd_family() |>
      filter(family_id == input$family_delete_row)
    r_row(list(id = dr$family_id, name = dr$Family, data = dr))
    showModal(modalDialog(
      title = "Confirmation",
      paste0("Remove ", dr$Family, "?"),
      footer = tagList(
        modalButton("No"),
        actionButton("btn_remove_family_submit", "Yes", class = "btn-danger")
      ),
      easyClose = TRUE
    ))
  })

  ### remove family submit ----
  observeEvent(input$btn_remove_family_submit, {
    d_new <- r_row()$data

    di_new <- isolate(rd_edition()) |>
      filter_out(family_id == r_row()$id)

    d_has_score <- rd_score() |>
      mutate(rn = row_number()) |>
      filter(
        edition_id %in%
          (rd_edition() |>
            filter(family_id == r_row()$id) |>
            pull(edition_id))
      )

    if (nrow(d_has_score) > 0) {
      showModal(modalDialog(
        title = "Data Conflict",
        paste0(
          "The ",
          r_row()$name,
          "test family cannot be deleted because batteries in this family are being used in the Data Entry table. Row(s): ",
          xfun::join_words(d_has_score$rn)
        ),
        footer = modalButton("Dismiss"),
        easyClose = TRUE
      ))
    } else {
      rd_family(d_new)
      rd_edition(di_new)

      if (nrow(d_new) == 0L) {
        current_family_row(NULL)
      } else {
        current_family_row(1L)
      }
      removeModal()
    }
  })

  ### add_family ----
  observeEvent(input$add_family, {
    iv_family_add$disable()
    iv_family_add$enable()
    showModal(
      modalDialog(
        title = "Add Test Family",
        textInput(
          "family_add_new",
          "Test Family",
          width = "100%"
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("family_add_submit", "Add", class = "btn-primary")
        ),
        easyClose = TRUE
      )
    )
  })

  #### family_add_submit ----
  observeEvent(input$family_add_submit, {
    if (iv_family_add$is_valid()) {
      new_family_id <- new_id(rd_family(), "family_id")

      d_new <- tibble(
        Family = input$family_add_new,
        family_id = new_family_id
      )

      d_f <- isolate(rd_family()) |>
        rows_insert(d_new, by = "family_id") |>
        arrange(Family)

      rd_family(d_f)

      current_family_row(which(d_f$family_id == new_family_id))
      iv_family_add$disable()
      removeModal()
    } else {
      showNotification("Fix incorrect/missing data", type = "error")
    }
  })

  ### edit_family ----
  observeEvent(input$family_edit_row, {
    req(input$family_edit_row)
    iv_family_add$disable()
    iv_family_add$enable()

    showModal(
      modalDialog(
        title = "Edit Test Family",
        textInput(
          "family_add_new",
          "Test Family",
          value = rd_family() |>
            filter(family_id == input$family_edit_row) |>
            pull(Family),
          width = "100%"
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("family_edit_submit", "Update", class = "btn-primary")
        ),
        easyClose = TRUE
      )
    )
  })

  #### family_edit_submit ----
  observeEvent(input$family_edit_submit, {
    req(input$family_edit_row)
    if (iv$is_valid()) {
      d_new <- tibble(
        Family = input$family_add_new,
        family_id = input$family_edit_row
      )

      d_f <- isolate(rd_family()) |>
        rows_update(d_new, by = "family_id")

      rd_family(d_f)
      iv_family_add$disable()
      removeModal()
    } else {
      showNotification("Fix incorrect/missing data", type = "error")
    }
  })

  ## edition ----

  sort_edition <- function(d) {
    d_b <- isolate(rd_battery())
    d_f <- isolate(rd_family())
    d %>%
      left_join(d_f, by = join_by(family_id)) %>%
      left_join(d_b, by = join_by(battery_id, family_id)) %>%
      select(Family, Battery, everything()) |>
      arrange(Family, Battery, Year_Published) |>
      select(-Family)
  }

  output$grdEdition <- renderReactable({
    req(current_family_row())
    current_data <- sort_edition(rd_edition())

    if (isTruthy(rfamily_id())) {
      current_data <- current_data |>
        filter(
          family_id == rfamily_id()
        )
    }

    current_data$Actions <- pmap_chr(
      tibble(
        row_id = current_data$edition_id,
        name = current_data$Edition,
        tbl = "edition",
        disabled_edit = FALSE,
        disabled_delete = FALSE
      ),
      make_actions
    )

    reactable(
      current_data,
      showSortIcon = FALSE,
      pagination = FALSE,
      striped = TRUE,
      defaultExpanded = TRUE,
      selection = "single",
      onClick = "select",
      theme = myreactabletheme,
      highlight = TRUE,
      # groupBy = "Battery",
      columns = list(
        Actions = colDef(html = TRUE, sortable = FALSE, align = "center"),
        Mean = colDef(align = "center", show = FALSE),
        SD = colDef(align = "center", show = FALSE),
        family_id = colDef(show = FALSE),
        edition_id = colDef(show = FALSE),
        battery_id = colDef(show = FALSE),
        Battery = colDef(width = 450),
        Year_Published = colDef("Published", align = "center"),
        Year_Normed = colDef("Normed", align = "center", show = FALSE),
        Edition = colDef(align = "left"),
        Reliability = colDef(align = "center", cell = function(value) {
          x <- signs::signs(
            value,
            accuracy = .01,
            trim_leading_zeros = TRUE
          )
          x[is.na(x)] <- ""
          x
        })
      )
    )
  })

  ### add edition ----
  observeEvent(input$add_edition, {
    iv_edition_add$disable()
    iv_edition_add$enable()
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
      f_selected <- as.character(rfamily_id())
    }

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
                      "Year Normed",
                      value = NA_integer_,
                      width = "100%",
                      step = 1
                    )
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
                    )
                  )
                )
              )
            )
          )
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("add_edition_submit", "Add", class = "btn-primary")
        ),
        easyClose = TRUE
      )
    )
  })

  #### select family new ----
  observeEvent(input$family_new, {
    if (!isTruthy(input$family_new)) {
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
      fid <- new_id(rd_family(), "family_id")

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
      bid <- new_id(rd_battery(), "battery_id")

      rd_battery(
        isolate(rd_battery()) %>%
          add_row(battery_id = bid, Battery = new_batt, family_id = fid) %>%
          arrange(Battery)
      )
    }
    bid
  }

  #### add edition submit ----
  observeEvent(input$add_edition_submit, {
    if (iv_edition_add$is_valid()) {
      d_e <- isolate(rd_edition())
      fid <- add_new_family(input$family_new)
      bid <- add_new_battery(input$battery_new, fid)
      eid <- new_id(d_e, "edition_id")

      d_e_new <- rows_insert(
        d_e,
        tibble(
          edition_id = eid,
          family_id = fid,
          battery_id = bid,
          Edition = input$edition_new,
          Year_Published = as.integer(input$year_published_new),
          Year_Normed = as.integer(input$year_normed_new),
          Mean = as.numeric(input$mean_new),
          SD = as.numeric(input$sd_new),
          Reliability = as.numeric(input$reliability_new)
        ),
        by = "edition_id"
      ) |>
        left_join(rd_family(), by = join_by(family_id)) |>
        left_join(rd_battery(), by = join_by(family_id, battery_id)) |>
        arrange(Family, Battery, Year_Published) |>
        select(-Family, -Battery)

      rd_edition(d_e_new)
      iv_edition_add$disable()
      removeModal()
    } else {
      showNotification("Fix incorrect/missing data", type = "error")
    }
  })

  ### update edition ----
  observeEvent(input$edition_edit_row, {
    req(input$edition_edit_row)
    iv_edition_add$disable()
    iv_edition_add$enable()
    d_e <- sort_edition(rd_edition())
    dr <- d_e |> filter(edition_id == input$edition_edit_row)
    redition_id(dr$edition_id)
    r_row(list(id = dr$edition_id, name = dr$Edition, data = dr))
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
              "Year Normed",
              value = dr$Year_Normed,
              width = "100%",
              step = 1
            )
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
            )
          )
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("update_battery_submit", "Update", class = "btn-primary")
        ),
        easyClose = TRUE
      )
    )
  })

  #### update battery submit ----
  observeEvent(input$update_battery_submit, {
    req(redition_id())
    if (iv_edition_add$is_valid()) {
      d_e <- rd_edition()
      d_new <- tibble(
        edition_id = redition_id(),
        Edition = input$edition_new,
        Year_Published = as.integer(input$year_published_new),
        Year_Normed = as.integer(input$year_normed_new),
        Mean = as.numeric(input$mean_new),
        SD = as.numeric(input$sd_new),
        Reliability = as.numeric(input$reliability_new)
      )

      rd_edition(dplyr::rows_update(d_e, d_new, by = "edition_id"))
      row_id <- d_e |>
        mutate(rid = row_number()) |>
        filter(edition_id == redition_id()) |>
        pull(rid)
      iv_edition_add$disable()
      removeModal()
    } else {
      showNotification(
        "Enter all data and fix any errors before updating this row.",
        type = "error"
      )
    }
  })

  r_remove_edition_id <- reactiveVal(integer(0))

  ### remove edition ----
  observeEvent(input$edition_delete_row, {
    req(input$edition_delete_row)
    d_e <- sort_edition(rd_edition())

    dr <- d_e |> filter(edition_id == input$edition_delete_row)

    r_remove_edition_id(dr$edition_id)

    showModal(modalDialog(
      title = "Confirmation",
      paste0("Remove ", dr$Edition, "?"),
      footer = tagList(
        modalButton("No"),
        actionButton("btn_remove_edition_yes", "Yes", class = "btn-danger")
      )
    ))
  })
  #### remove edition submit ----
  observeEvent(input$btn_remove_edition_yes, {
    req(r_remove_edition_id())
    rd_edition(
      isolate(rd_edition()) |>
        filter_out(edition_id == as.integer(r_remove_edition_id()))
    )

    r_remove_edition_id(integer(0))

    removeModal()
  })

  rd_edition_display <- reactiveVal(
    isolate(rd_edition()) |>
      left_join(isolate(rd_battery()), join_by(battery_id, family_id)) |>
      mutate(Edition = paste0(Battery, " (", Edition, ")"))
  )

  ## score ----
  output$grdScore <- renderReactable({
    req(input$dateBirthdate)
    cor_n(nrow(rd_score()))
    current_data <- rd_score() |>
      left_join(
        rd_edition() |>
          left_join(
            rd_battery(),
            join_by(battery_id, family_id)
          ),
        join_by(edition_id)
      ) |>
      left_join(
        rd_flynn() |> select(flynn_id, Flynn),
        by = join_by(flynn_id)
      ) |>
      mutate(
        Age = as.numeric(
          time_length(
            interval(input$dateBirthdate, Date),
            "years"
          )
        ),
        Corrected = pmap_dbl(
          list(
            score = Score,
            publication_year = Year_Published,
            norm_year = Year_Normed,
            administration_date = Date,
            iflynn_id = flynn_id
          ),
          flynn_correction
        )
      ) |>
      select(
        Score,
        Corrected,
        Battery,
        Edition,
        Year_Published,
        Date,
        Age,
        Flynn,
        Weight,
        everything()
      )

    rd_current(current_data)

    current_data$Actions <- pmap_chr(
      tibble(
        row_id = current_data$score_id,
        name = current_data$Edition,
        tbl = "score",
        disabled_edit = FALSE,
        disabled_delete = FALSE
      ),
      make_actions
    )

    corrected_width <- 4L + 1L * any(round(current_data$Corrected, 1) >= 100)
    score_width <- 2L + 1L * any(round(current_data$Score, 0) >= 100)
    age_width <- 3L + 1L * any(round(current_data$Age, 1) >= 10)

    reactable(
      current_data,
      showSortIcon = FALSE,
      pagination = FALSE,
      striped = TRUE,
      defaultExpanded = TRUE,
      selection = "single",
      onClick = "select",
      theme = myreactabletheme,
      highlight = TRUE,
      columns = list(
        Actions = colDef(
          html = TRUE,
          sortable = FALSE,
          align = "center"
        ),
        score_id = colDef(show = FALSE),
        flynn_id = colDef(show = FALSE),
        family_id = colDef(show = FALSE),
        battery_id = colDef(show = FALSE),
        edition_id = colDef(show = FALSE),
        Mean = colDef(show = FALSE),
        SD = colDef(show = FALSE),
        Reliability = colDef(show = FALSE),
        Score = colDef(align = "center", html = TRUE, cell = \(x) {
          formatC(x, digits = 0, format = "f", width = score_width) |>
            stringr::str_replace_all(" ", "&numsp;")
        }),
        Corrected = colDef(align = "center", html = TRUE, cell = \(x) {
          formatC(x, digits = 1, format = "f", width = corrected_width) |>
            stringr::str_replace_all(" ", "&numsp;")
        }),
        Age = colDef(
          align = "center",
          html = TRUE,
          style = function(value) {
            if (value < 0) {
              list(color = "red")
            }
          },
          cell = \(x) {
            formatC(x, digits = 1, format = "f", width = age_width) |>
              stringr::str_replace_all(" ", "&numsp;")
          }
        ),
        Date = colDef(align = "center"),
        Flynn = colDef(align = "center"),
        Weight = colDef(align = "center"),
        Year_Published = colDef(header = "Published", align = "center"),
        Year_Normed = colDef(show = FALSE),
        Battery = colDef(width = 500)
      )
    )
  })

  ### add_score ----
  observeEvent(input$add_score, {
    c_edition <- rd_edition() |>
      mutate(
        Edition = paste0(
          Edition,
          " (",
          Year_Published,
          ")"
        )
      ) |>
      select(Edition, edition_id) |>
      deframe()
    c_flynn <- rd_flynn() |>
      select(Flynn, flynn_id) |>
      deframe()

    c_family <- rd_family() |>
      select(Family, family_id) |>
      deframe()

    c_battery <- rd_battery() |>
      select(Battery, battery_id) |>
      deframe()

    iv_score$disable()
    iv_score$enable()

    showModal(
      modalDialog(
        size = "l",
        title = "Add Test Score",
        fluidRow(
          column(
            3,
            selectInput(
              "score_family_new",
              "Select Family",
              width = "100%",
              choices = c_family,
              selected = c_family[names(c_family) == "Wechsler"],
              multiple = FALSE,
              selectize = FALSE,
              size = length(c_family)
            )
          ),
          column(
            6,
            selectInput(
              "score_battery_new",
              "Select Battery",
              width = "100%",
              choices = c_battery,
              multiple = FALSE,
              size = 6L,
              selectize = FALSE
            ),
            selectInput(
              "score_edition_new",
              "Select Edition",
              width = "100%",
              choices = c_edition,
              multiple = FALSE,
              selectize = FALSE,
              size = 8L
            )
          ),
          column(
            3,

            numericInput(
              "score_new",
              "Score",
              value = 100,
              width = "100%",
              step = 1
            ),

            dateInput(
              "date_new",
              "Date Given",
              width = "100%"
            ),
            numericInput(
              "weight_new",
              "Weight",
              value = 1,
              width = "100%",
              step = .1
            ),

            selectInput(
              "score_flynn_new",
              "Flynn Effect Rule",
              width = "100%",
              choices = c_flynn,
              selected = 1L,
              multiple = FALSE
            )
          )
        ),
        footer = tagList(
          p("Not seeing your test? Cancel and add it on the Edit Battery tab."),
          modalButton("Cancel"),
          actionButton("add_score_submit", "Add", class = "btn-primary")
        ),
        easyClose = TRUE
      )
    )
  })

  #### add score family change ----
  observeEvent(input$score_family_new, {
    req(input$score_family_new)

    family_name <- rd_family() |>
      filter(family_id == input$score_family_new) |>
      pull(Family)

    if (input$score_family_new == "NA") {
      c_battery <- rd_battery() |>
        select(Battery, battery_id) |>
        deframe()

      c_edition <- rd_edition() |>
        mutate(
          Edition = paste0(
            Edition,
            " (",
            Year_Published,
            ")"
          )
        ) |>
        select(Edition, edition_id) |>
        deframe()
    } else {
      c_battery <- rd_battery() |>
        filter(family_id == as.integer(input$score_family_new)) |>
        select(Battery, battery_id) |>
        deframe()

      c_edition <- rd_edition() |>
        filter(family_id == as.integer(input$score_family_new)) |>
        mutate(
          Edition = paste0(
            Edition,
            " (",
            Year_Published,
            ")"
          )
        ) |>
        select(Edition, edition_id) |>
        deframe()
    }

    if (family_name == "Kaufman") {
      s_battery <- c_battery[
        names(c_battery) == "Kaufman Assessment Battery for Children"
      ]
    } else {
      s_battery <- c_battery[1L]
    }

    updateSelectInput(
      session,
      "score_battery_new",
      choices = c_battery,
      selected = s_battery
    )

    updateSelectInput(
      session,
      "score_edition_new",
      choices = c_edition,
      selected = c_edition[1L]
    )
    runjs("$('#score_new').focus();")
  })

  observeEvent(input$score_edition_new, {
    req(input$score_edition_new)
    runjs("$('#score_new').focus();")
  })
  #### add score battery change ----
  observeEvent(input$score_battery_new, {
    req(input$score_battery_new)
    req(input$score_battery_new != "NA")

    c_edition <- rd_edition() |>
      filter(battery_id == as.integer(input$score_battery_new)) |>
      mutate(
        Edition = paste0(
          Edition,
          " (",
          Year_Published,
          ")"
        )
      ) |>
      select(Edition, edition_id) |>
      deframe()

    updateSelectizeInput(
      session,
      "score_edition_new",
      choices = c_edition,
      selected = c_edition[length(c_edition)]
    )

    runjs("$('#score_new').focus().select();")
  })
  #### add score submit ----
  observeEvent(input$add_score_submit, {
    if (iv_score$is_valid()) {
      new_score_id <- new_id(rd_score(), "score_id")

      f_id <-
        d_new <- tibble(
          edition_id = as.integer(input$score_edition_new),
          Score = input$score_new,
          Date = input$date_new,
          Weight = input$weight_new,
          flynn_id = as.integer(input$score_flynn_new),
          score_id = new_score_id
        )

      rd_score(
        isolate(rd_score()) |>
          rows_insert(d_new, by = "score_id")
      )

      cor_n(nrow(rd_score()))

      iv_score$disable()
      removeModal()
    } else {
      showNotification("Fix incorrect/missing data", type = "error")
    }
  })

  ### edit_score ----
  observeEvent(input$score_edit_row, {
    iv_score$disable()
    iv_score$enable()

    c_flynn <- rd_flynn() |>
      select(Flynn, flynn_id) |>
      deframe()

    dr_new <- rd_score() |>
      filter(score_id == input$score_edit_row) |>
      left_join(rd_edition(), by = join_by(edition_id))
    r_row(dr_new)

    showModal(
      modalDialog(
        title = paste("Edit", dr_new$Edition, "Score"),
        hidden(
          numericInput(
            "score_edition_new",
            label = NULL,
            value = dr_new$edition_id
          )
        ),
        numericInput(
          "score_new",
          "Score",
          value = dr_new$Score,
          width = "100%",
          step = 1
        ),
        fluidRow(
          column(
            6,
            dateInput(
              "date_new",
              "Date Given",
              width = "100%",
              value = dr_new$Date
            )
          ),
          column(
            6,
            numericInput(
              "weight_new",
              "Weight",
              value = dr_new$Weight,
              width = "100%",
              step = .1
            )
          )
        ),

        selectInput(
          "score_flynn_new",
          "Flynn Effect Rule",
          width = "100%",
          choices = c_flynn,
          selected = dr_new$flynn_id,
          multiple = FALSE
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("edit_score_submit", "Update", class = "btn-primary")
        ),
        easyClose = TRUE
      )
    )
  })
  #### edit score submit ----
  observeEvent(input$edit_score_submit, {
    if (iv_score_edit$is_valid()) {
      d_new <- tibble(
        edition_id = r_row()$edition_id,
        Score = input$score_new,
        Date = input$date_new,
        Weight = input$weight_new,
        flynn_id = as.integer(input$score_flynn_new),
        score_id = r_row()$score_id
      )

      rd_score(
        isolate(rd_score()) |>
          rows_update(d_new, by = "score_id")
      )

      cor_n(nrow(rd_score()))

      iv_score$disable()
      removeModal()
    } else {
      showNotification("Fix incorrect/missing data", type = "error")
    }
  })

  ### remove score ----
  observeEvent(input$score_delete_row, {
    req(input$score_delete_row)

    dr <- isolate(rd_score()) |>
      filter(score_id == input$score_delete_row)

    r_row(dr)

    dr <- dr |>
      left_join(rd_edition(), by = join_by(edition_id))

    showModal(modalDialog(
      title = "Confirmation",
      paste0("Remove ", dr$Edition, " = ", dr$Score, "?"),
      footer = tagList(
        modalButton("No"),
        actionButton("btn_remove_score_yes", "Yes", class = "btn-danger")
      )
    ))
  })

  #### remove score submit ----
  observeEvent(input$btn_remove_score_yes, {
    req(input$btn_remove_score_yes)
    rd_score(
      isolate(rd_score()) |>
        dplyr::rows_delete(r_row() |> select(score_id), by = "score_id")
    )
    cor_n(nrow(rd_score()))
    removeModal()
  })

  ## flynn ----
  output$grdFlynn <- renderReactable({
    current_data <- rd_flynn()
    current_data$Actions <- pmap_chr(
      tibble(
        row_id = current_data$flynn_id,
        tbl = "flynn",
        name = current_data$Flynn,
        disabled_edit = current_data$flynn_id == 1,
        disabled_delete = current_data$flynn_id == 1
      ),
      make_actions
    )

    reactable(
      current_data,
      showSortIcon = FALSE,
      pagination = FALSE,
      sortable = FALSE,
      striped = TRUE,
      defaultSelected = current_flynn_row(),
      selection = "single",
      onClick = my_onclick,
      theme = myreactabletheme,
      highlight = TRUE,
      columns = list(
        Flynn = colDef("Flynn Rule"),
        flynn_id = colDef(show = FALSE),
        Actions = colDef(html = TRUE, sortable = FALSE, align = "center")
      )
    )
  })

  ### remove flynn ----
  observeEvent(input$flynn_delete_row, {
    req(input$flynn_delete_row)
    showModal(modalDialog(
      title = "Confirmation",
      paste0("Remove this Flynn Rule?"),
      footer = tagList(
        modalButton("No"),
        actionButton("btn_remove_flynn_yes", "Yes", class = "btn-danger")
      )
    ))
  })

  observeEvent(input$btn_remove_flynn_yes, {
    d_new <- isolate(rd_flynn()) |>
      filter_out(flynn_id == input$flynn_delete_row)
    rd_flynn(d_new)
    di_new <- isolate(rd_flynn_item()) |>
      filter_out(flynn_id == input$flynn_delete_row)
    rd_flynn_item(di_new)

    current_flynn_row(nrow(rd_flynn()))

    removeModal()
  })

  ### add_flynn ----
  observeEvent(input$add_flynn, {
    iv_flynn$disable()
    iv_flynn$enable()
    showModal(
      modalDialog(
        title = "Add Flynn Effect Rule",
        textInput(
          "flynn_add_new",
          "Name of Flynn Effect Rule",
          width = "100%"
        ),
        numericInput(
          inputId = "flynn_add_effect_new",
          label = "Current Effect size (points per decade)",
          value = 1.3,
          width = "100%",
          step = .01
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("flynn_add_submit", "Add", class = "btn-primary")
        ),
        easyClose = TRUE
      )
    )
  })

  #### flynn_add_submit ----
  observeEvent(input$flynn_add_submit, {
    if (iv_flynn$is_valid()) {
      new_flynn_id <- new_id(rd_flynn(), "flynn_id")

      d_new <- tibble(
        Flynn = input$flynn_add_new,
        flynn_id = new_flynn_id
      )

      d_f <- isolate(rd_flynn())
      rd_flynn(rows_insert(d_f, d_new, by = "flynn_id"))

      d_fi <- isolate(rd_flynn_item())

      d_new_item <- tibble(
        flynn_item_id = new_id(d_fi, "flynn_item_id"),
        flynn_id = new_flynn_id,
        Effect = input$flynn_add_effect_new,
        Until = NA
      )

      rd_flynn_item(
        rows_insert(
          d_fi,
          d_new_item,
          by = "flynn_item_id"
        )
      )

      current_flynn_row(nrow(rd_flynn()))
      iv_flynn$disable()
      removeModal()
    } else {
      showNotification("Fix incorrect/missing data", type = "error")
    }
  })

  ### edit_flynn ----
  observeEvent(input$flynn_edit_row, {
    req(input$flynn_edit_row)
    iv_flynn$disable()
    iv_flynn$enable()
    showModal(
      modalDialog(
        title = "Edit Flynn Effect Rule",
        textInput(
          "flynn_add_new",
          "Name of Flynn Effect Rule",
          value = rd_flynn() |>
            filter(flynn_id == input$flynn_edit_row) |>
            pull(Flynn),
          width = "100%"
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("flynn_edit_submit", "Add", class = "btn-primary")
        ),
        easyClose = TRUE
      )
    )
  })

  #### flynn_edit_submit ----
  observeEvent(input$flynn_edit_submit, {
    if (iv_flynn_edit$is_valid()) {
      d_new <- tibble(
        Flynn = input$flynn_edit_new,
        flynn_id = input$flynn_edit_row
      )

      d_f <- isolate(rd_flynn()) |>
        rows_update(d_new, by = "flynn_id")

      rd_flynn(d_f)
      iv_flynn$disable()
      removeModal()
    } else {
      showNotification("Fix incorrect/missing data", type = "error")
    }
  })

  ## flynn item ----
  output$grdFlynnItem <- renderReactable({
    current_data <- rd_flynn_item()
    current_data$Actions <- pmap_chr(
      list(
        row_id = current_data$flynn_item_id,
        tbl = "flynn_item",
        name = current_data$Until,
        disabled_edit = FALSE,
        disabled_delete = is.na(current_data$Until)
      ),
      make_actions
    )

    if (isTruthy(rflynn_id())) {
      current_data <- current_data |> filter(flynn_id == rflynn_id())
    } else {
      current_data <- current_data |> filter(FALSE)
    }
    reactable(
      current_data,
      showSortIcon = FALSE,
      pagination = FALSE,
      sortable = FALSE,
      striped = TRUE,
      selection = "single",
      onClick = my_onclick,
      fullWidth = FALSE,
      theme = myreactabletheme,
      highlight = TRUE,
      outlined = TRUE,
      columns = list(
        flynn_id = colDef(show = FALSE),
        flynn_item_id = colDef(show = FALSE),
        Actions = colDef(html = TRUE, width = 150, align = "center"),
        Effect = colDef(align = "right", format = colFormat(digits = 2)),
        Until = colDef(align = "right", cell = function(value) {
          value[is.na(value)] <- "Now"
          value
        })
      )
    )
  })

  ### add flynn item ----
  observeEvent(input$add_flynn_item, {
    req(rflynn_id())
    req(rflynn_id() > 0)
    iv_flynn_item$disable()
    iv_flynn_item$enable()
    showModal(
      modalDialog(
        title = "Add Flynn Effect",
        numericInput(
          inputId = "flynn_until_new",
          label = "Until Year (leave blank if rule does not expire)",
          value = NA_real_,
          width = "100%"
        ),
        numericInput(
          inputId = "flynn_effect_new",
          label = "Effect size (points per decade)",
          value = 2.94,
          width = "100%",
          step = .01
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("add_flynn_item_submit", "Add", class = "btn-primary")
        ),
        easyClose = TRUE
      )
    )
  })

  n_check <- reactiveVal(FALSE)

  observeEvent(input$add_flynn_item_submit, {
    req(rflynn_id())
    req(rflynn_id() > 0)

    if (iv_flynn_item$is_valid()) {
      if (nrow(rd_flynn_item()) == 0) {
        new_flynn_item_id <- 1L
      } else {
        new_flynn_item_id <- max(rd_flynn_item()$flynn_item_id) + 1L
      }

      d_new <- tibble(
        flynn_item_id = new_flynn_item_id,
        flynn_id = rflynn_id(),
        Until = input$flynn_until_new,
        Effect = input$flynn_effect_new
      )

      d_fi <- isolate(rd_flynn_item()) |>
        dplyr::rows_insert(d_new, by = "flynn_item_id") |>
        arrange(flynn_id, Until)

      nc <- count(d_fi |> select(flynn_id, Until), flynn_id, Until) |>
        filter(n > 1) |>
        nrow()

      n_check(nc > 0)

      if (
        (!n_check()) &&
          (input$flynn_until_new > 1904L || is.na(input$flynn_until_new))
      ) {
        rd_flynn_item(d_fi)
        iv_flynn_item$disable()
        removeModal()
      } else {
        showNotification("Fix incorrect/missing data", type = "error")
      }
    }
  })

  observeEvent(input$flynn_item_delete_row, {
    req(input$flynn_item_delete_row)
    di_new <- isolate(rd_flynn_item()) |>
      filter_out(flynn_item_id == input$flynn_item_delete_row)
    rd_flynn_item(di_new)
  })

  hide_until <- reactiveVal(FALSE)

  ### edit_flynnItem ----
  observeEvent(input$flynn_item_edit_row, {
    req(input$flynn_item_edit_row)
    d_fi <- rd_flynn_item() %>%
      filter(flynn_item_id == input$flynn_item_edit_row)
    r_row(list(id = d_fi$flynn_item_id, data = d_fi))
    iv_flynn_item$disable()
    iv_flynn_item$enable()
    showModal(
      modalDialog(
        title = "Edit Flynn Effect",
        hidden(
          div(
            id = "hidden_until",
            numericInput(
              inputId = "until_new",
              label = "Until Year (leave blank if rule does not expire)",
              value = d_fi$Until,
              width = "100%"
            )
          )
        ),
        numericInput(
          inputId = "effect_new",
          label = "Effect size (points per decade)",
          value = d_fi$Effect,
          width = "100%",
          step = .01
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            "edit_flynn_item_submit",
            "Update",
            class = "btn-primary"
          )
        ),
        easyClose = TRUE
      )
    )

    if (is.na(d_fi$Until)) {
      hide("hidden_until")
    } else {
      show("hidden_until")
    }
  })

  observeEvent(input$edit_flynn_item_submit, {
    req(input$flynn_item_edit_row)
    d_fi <- rd_flynn_item() %>%
      filter(flynn_item_id == input$flynn_item_edit_row)

    d_new <- tibble(
      flynn_item_id = input$flynn_item_edit_row,
      flynn_id = d_fi$flynn_id,
      Until = input$until_new,
      Effect = input$effect_new
    )

    d_fi <- isolate(rd_flynn_item()) |>
      dplyr::rows_update(d_new, by = c("flynn_item_id", "flynn_id")) |>
      arrange(flynn_id, Until)

    nc <- count(d_fi |> select(flynn_id, Until), flynn_id, Until) |>
      filter(n > 1) |>
      nrow()

    n_check(nc > 0)
    if ((!n_check()) && (input$until_new > 1904L || is.na(input$until_new))) {
      rd_flynn_item(d_fi)
      iv_flynn_item$disable()
      removeModal()
    }
  })
  # correlation ----

  r_cor <- reactive({
    n <- cor_n()
    req(n > 1L)
    d_s <- isolate(rd_score()) |>
      left_join(isolate(rd_edition()), join_by("edition_id"))
    # matrix(d_s$Score) |>
    # `rownames<-`(paste0(d_s$Edition, " (", d_s$Date, ")"))

    bday <- isolate(input$dateBirthdate)
    # bday <- as.Date("2010-11-01")

    d_s <- rd_score() |>
      left_join(rd_edition(), join_by("edition_id")) |>
      left_join(rd_family(), join_by("family_id")) |>
      mutate(
        Age = as.numeric(time_length(
          interval(bday, Date),
          "years"
        ))
      ) |>
      mutate(row_id = row_number())

    d_r <- combn(d_s$row_id, 2) |>
      t() |>
      `colnames<-`(c("y", "x")) |>
      as_tibble() |>
      mutate(pair_id = row_number()) |>
      pivot_longer(-pair_id, values_to = "row_id") |>
      left_join(
        d_s |> select(score_id, Age, family_id, row_id),
        by = join_by(row_id)
      ) |>
      pivot_wider(
        values_from = c(score_id, Age, family_id, row_id)
      ) |>
      mutate(
        r = long_r(
          age = ifelse(Age_x < Age_y, Age_x, Age_y),
          interval = abs(Age_x - Age_y),
          different = (family_id_y != family_id_x) * 1
        )
      )

    m_r <- matrix(NA_real_, nrow = nrow(d_s), ncol = nrow(d_s))
    m_r[cbind(d_r$row_id_x, d_r$row_id_y)] <- d_r$r
    m_r[cbind(d_r$row_id_y, d_r$row_id_x)] <- d_r$r
    diag(m_r) <- 1

    # diag(m_r) <- 1
    colnames(m_r) <- d_s$Edition
    rownames(m_r) <- d_s$Edition
    return(m_r)
  })

  output$tbl_cor <- renderUI({
    d_s <- isolate(rd_score()) |>
      left_join(isolate(rd_edition()), join_by("edition_id"))
    m_r <- r_cor()
    num_rows <- nrow(m_r)
    num_cols <- nrow(m_r)
    # Generate HTML rows
    table_rows <- lapply(0:num_rows, function(r) {
      # Generate cells for each row
      cells <- lapply(0:num_cols, function(c) {
        input_id <- paste0("cell_", r, "_", c)
        symmetric_id <- paste0("display_", r, "_", c)

        if (r == 0) {
          if (c == 0) {
            tags$th("", scope = "col")
          } else {
            tags$th(
              d_s$Edition[c],
              scope = "col",
              class = "text-center cortable"
            )
          }
        } else {
          if (r == c) {
            tags$td(1, class = "border border-primary cordisplay")
          } else {
            if (c == 0) {
              tags$th(
                paste0(
                  d_s$Edition[r],
                  " (",
                  as.character(format(d_s$Date[r], "%b %Y")),
                  ")"
                ),
                scope = "row",
                class = "th-primary corrowname"
              )
            } else {
              if (r < c) {
                tags$td(
                  textOutput(symmetric_id),
                  class = "border border-primary cordisplay"
                )
              } else {
                tags$td(
                  numericInput(
                    inputId = input_id,
                    label = NULL, # Remove standard label for dense grid
                    value = signs::signs(
                      m_r[r, c],
                      accuracy = .01,
                      trim_leading_zeros = TRUE
                    ),
                    step = .01,
                    width = "100%"
                  ),
                  class = "border border-primary corinput"
                )
              }
            }
          }
        }
      })
      if (r == 0) {
        tags$thead(tags$tr(cells))
      } else {
        tags$tr(cells)
      }
    })
    # Wrap elements into a standard HTML table
    tags$table(
      class = "cortable",
      table_rows[1],
      # class = "table table-bordered border-primary",
      tags$tbody(table_rows[-1])
    )
  })

  observe({
    n <- cor_n()
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        if (i < j) {
          # mr[cbind(c(i, j), c(j, i))] <- input[[mirror_id]]
          local({
            jj <- j
            ii <- i

            mirror_id <- paste0("cell_", jj, "_", ii) # the symmetric upper cell

            output[[paste0("display_", ii, "_", jj)]] <- renderText({
              val <- input[[mirror_id]]
              if (is.null(val) || is.na(val)) {
                ""
              } else {
                signs::signs(val, accuracy = .01, trim_leading_zeros = TRUE)
              }
            })
          })
        }
      }
    }
    # r_cor(mr)
  })

  # composite iq ----
  output$grdIQ <- renderReactable({
    req(rd_current())
    cor_n(nrow(rd_score()))
    m_r <- r_cor()
    for (i in seq(cor_n())) {
      for (j in seq(cor_n())) {
        if (isTruthy(input[[paste0("cell_", 1, "_", 1)]])) {
          m_r[i, j] <- as.numeric(input[[paste0("cell_", 1, "_", 1)]])
        }
      }
    }

    iq_var <- sum(r_cor())
    iq_sd <- sqrt(iq_var)
    d_s <- rd_current() |>
      rename(Uncorrected = Score) |>
      mutate(id = row_number()) |>
      select(id, Uncorrected, Corrected, Mean, SD, Reliability) |>
      pivot_longer(c(Uncorrected, Corrected)) |>
      mutate(
        z = (value - Mean) / SD,
        SS = z * 15 + 100,
        SEE = 15 * sqrt(Reliability - Reliability^2),
        UB = qnorm(.975) * SEE + SS,
        LB = qnorm(.025) * SEE + SS
      )

    d_iq <- d_s |>
      summarise(SS = sum(SS), rxx = sum(Reliability), k = n(), .by = name) |>
      mutate(
        rxx = (rxx - k + sum(m_r)) / sum(m_r),
        SS = (SS - 100 * k) / sqrt(sum(m_r)) + 100,
        SEE = 15 * sqrt(rxx - rxx^2),
        UB = qnorm(.975) * SEE + SS,
        LB = qnorm(.025) * SEE + SS,
        CI = paste0(scales::number(LB, 1), "&ndash;", scales::number(UB, 1)),
        Percentile = as.character(
          as.numeric(
            prob_label(
              pnorm(SS, 100, 15),
              digits = 2
            )
          ) *
            100
        )
      )

    d_display <- d_iq |>
      select(-UB, -LB, -SEE, -k) |>
      rename(Reliability = rxx, type = name) |>
      mutate(
        SS = round(SS, 0) |> as.integer() |> as.character(),
        Reliability = prob_label(Reliability, digits = 2)
      ) |>
      select(
        `Composite IQ` = SS,
        Percentile,
        CI,
        Reliability,
        everything()
      ) |>
      pivot_longer(-type, names_to = "rownames") |>
      pivot_wider(names_from = type)

    reactable(
      d_display,
      showSortIcon = FALSE,
      pagination = FALSE,
      striped = TRUE,
      defaultExpanded = TRUE,
      theme = myreactabletheme,
      highlight = TRUE,
      fullWidth = FALSE,
      columns = list(
        Uncorrected = colDef(align = "center", html = TRUE, width = 150),
        Corrected = colDef(align = "center", html = TRUE, width = 150),
        rownames = colDef(name = "", align = "right", width = 150)
      )
    )
  })
}

shinyApp(ui, server)
