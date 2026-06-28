options(shiny.useragg = TRUE)

# Workaround for Chromium browser download bug in Shinylive
downloadButton <- function(...) {
  tag <- shiny::downloadButton(...)
  tag$attribs$download <- NULL
  tag
}

systemfonts::register_font(
  name = "rbc",
  plain = "www/fonts/RobotoCondensed-Regular.ttf",
  bold = "www/fonts/RobotoCondensed-Bold.ttf",
  italic = "www/fonts/RobotoCondensed-Italic.ttf",
  bolditalic = "www/fonts/RobotoCondensed-BoldItalic.ttf"
)
systemfonts::get_from_google_fonts("Roboto Condensed")

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
library(unusualprofile)
library(ggplot2)
library(ggtext)
library(scales)
library(distributional)
library(ggdist)
library(writexl)
library(readxl)
library(tinter)
library(ggnormalviolin)
# library(thematic)
library(ragg)


# thematic_shiny(font = "auto")
# library(showtext)
# library(sysfonts)

# font_add(
#   "Roboto Condensed",
#   regular = "www/fonts/RobotoCondensed-Regular.ttf",
#   bold = "www/fonts/RobotoCondensed-Bold.ttf",
#   italic = "www/fonts/RobotoCondensed-Italic.ttf"
# )
# showtext_auto()

# constants ####
my_primary <- "#1f6187"
my_primary_medium <- lighten(my_primary, .5)
my_primary_light <- lighten(my_primary, .18)
my_primary_lightest <- lighten(my_primary, .08)
my_primary_dark <- darken(my_primary, .7)
my_primary_darkest <- darken(my_primary, .9)

# helper functions ----

new_counter <- function(start = 0L) {
  i <- start
  function() {
    i <<- i + 1L
    i
  }
}

cm_plot <- function(
  x,
  ...,
  p_tail = .05,
  family = "Roboto Condensed",
  score_digits = ifelse(min(x$sigma) >= 10, 0, 2)
) {
  if (length(unique(x$d_score$id)) > 1) {
    stop("Can only plot one case at a time")
  }
  break_width <- max(x$sigma)
  break_min <- min(x$mu - 10 * x$sigma)
  break_max <- max(x$mu + 10 * x$sigma)
  minor_break_width <- ifelse(
    break_width %% 3 == 0,
    break_width / 3,
    break_width / 2
  )
  major_breaks <- seq(break_min, break_max, break_width)
  minor_breaks <- seq(break_min, break_max, minor_break_width)
  label_independent <- "Composite IQ"

  label_dependent <- paste0(
    "Profile Unusualness (Given CIQ = ",
    x$d_score |> filter(Role == "Independent") |> pull(Score) |> round(0),
    ") = ",
    formatC(x$dCM, digits = 2, format = "f"),
    ", *p* = ",
    prob_label(x$dCM_p)
  )
  x$d_score %>%
    mutate(
      SD = ifelse(
        test = is.na(zSEE),
        yes = sigma,
        no = zSEE * sigma
      ),
      yhat = ifelse(is.na(Predicted), mu, Predicted),
      id = factor(id),
      Role = factor(
        Role,
        levels = c("Independent", "Dependent"),
        labels = c(label_independent, label_dependent)
      ),
      mycp = if_else(
        Role == label_dependent,
        paste0("<br>*cp* = ", prob_label(cp, digits = 2, max_digits = 4)),
        ""
      ),
      myp = paste0("*p* = ", prob_label(p, digits = 2, max_digits = 4)),
      plabel = paste0(myp, mycp)
    ) %>%
    ggplot(aes(
      x = Variable,
      y = Score,
      fill = Role
    )) +
    facet_grid(
      cols = vars(!!quote(Role)),
      scales = "free",
      space = "free"
    ) +
    geom_normalviolin(
      mapping = aes(
        mu = yhat,
        sigma = SD,
        face_right = Role == label_dependent,
        face_left = FALSE,
        fill = Variable
      ),
      p_tail = p_tail,
      tail_alpha = 0.15,
      width = 0.85,
      alpha = .6
    ) +
    geom_normalviolin(
      mapping = aes(
        mu = mu,
        sigma = sigma,
        face_left = TRUE,
        face_right = FALSE,
        fill = Variable
      ),
      tail_alpha = 0.1,
      width = 0.85,
      p_tail = p_tail,
      alpha = .3
    ) +
    geom_point(mapping = aes(color = id)) +
    geom_richtext(
      mapping = aes(
        label = formatC(Score, score_digits, format = "f"),
        group = id
      ),
      color = "black",
      label.color = NA,
      text.color = "black",
      label.padding = margin(),
      label.margin = margin(r = 4, unit = "pt"),
      fill = NA,
      vjust = .5,
      hjust = 1.2,
      family = family,
      size = 12
    ) +
    geom_richtext(
      mapping = aes(
        group = id,
        label = plabel
      ),
      # label.color = NA,
      color = "gray10",
      label.padding = margin(),
      label.margin = margin(l = 10, unit = "pt"),
      fill = NA,
      label.colour = NA,
      lineheight = .9,
      vjust = .5,
      hjust = 0,
      size = 8,
      family = family
    ) +
    scale_y_continuous(
      "Scores",
      breaks = major_breaks,
      minor_breaks = minor_breaks
    ) +
    scale_x_discrete(NULL, expand = expansion(add = .65)) +
    labs(
      # title = paste0(
      #   "Conditional Mahalanobis Distance (*d*<sub>*CM*</sub>) = ",
      #   formatC(x$dCM, digits = 2, format = "f"),
      #   ", *p* = ",
      #   prob_label(x$dCM_p)
      # ),
      caption = "*p* = Population proportion, *cp* = Conditional proportion"
    ) +
    theme_light(base_family = family, base_size = 24) +
    theme(
      legend.position = "none",
      plot.caption = element_markdown(),
      strip.text.x.top = element_markdown(
        lineheight = 1.3,
        margin = margin(t = 5, b = 3, unit = "mm")
      ),
      strip.background = element_rect(fill = "gray33"),
      # title = element_markdown(),
      plot.title = element_markdown(size = 20)
    ) +
    scale_color_grey() +
    scale_fill_viridis_d(alpha = .2, begin = .1, end = .8)
}

composite_score <- function(
  x,
  R,
  mu_x = 100,
  sigma_x = 15,
  mu_composite = 100,
  sigma_composite = 15,
  w = NULL
) {
  k <- length(x)
  if (length(mu_x) == 1) {
    mu_x <- rep(mu_x, k)
  }
  if (length(mu_x) != length(x)) {
    stop("x and mu_x must be the same length.")
  }
  if (length(sigma_x) == 1) {
    sigma_x <- rep(sigma_x, k)
  }
  if (length(sigma_x) != length(x)) {
    stop("x and mu_x must be the same length.")
  }
  if ((nrow(R) != k) | (ncol(R) != k) | !is.matrix(R)) {
    stop("R must a square matrix with the same size as x.")
  }
  if (length(mu_composite) != 1) {
    stop("mu_composite must be a vector of length 1.")
  }
  if (length(sigma_composite) != 1) {
    stop("sigma_composite must be a vector of length 1.")
  }
  if (is.null(w)) {
    w <- rep(1, length(x))
  }
  if (length(w) != length(x)) {
    stop("w must the the same length as x.")
  }

  sigma_composite *
    (sum(w * (x - mu_x)) /
      sqrt(sum(diag(sigma_x * w) %*% R %*% diag(sigma_x * w)))) +
    mu_composite
}

composite_correlation <- function(R, w) {
  cov2cor(t(w) %*% R %*% w)
}

prob_label <- function(
  p,
  accuracy = 0.01,
  digits = NULL,
  max_digits = NULL,
  remove_leading_zero = TRUE,
  round_zero_one = TRUE,
  phantom_text = NULL,
  phantom_color = NULL,
  percentile = FALSE
) {
  if (is.null(digits)) {
    l <- number(p, accuracy = accuracy)
  } else {
    abs_p <- abs(p)
    sig_digits <- abs(ceiling(log10(abs_p + abs_p / 1e+09)) - digits)
    pgt99 <- (abs_p > 0.99) & !is.na(p)
    sig_digits[pgt99] <- abs(ceiling(log10(1 - abs_p[pgt99])) - digits + 1)

    sig_digits[
      ceiling(log10(abs_p)) == log10(abs_p) &
        (-log10(abs_p) >= digits)
    ] <-
      sig_digits[
        ceiling(log10(abs_p)) == log10(abs_p) &
          (-log10(abs_p) >= digits)
      ] -
      1

    sig_digits[is.infinite(sig_digits)] <- 0

    l <- map2_chr(p, sig_digits, \(pp, ss) {
      if (is.na(pp)) {
        ""
      } else {
        formatC(pp, ss, format = "f", flag = "#")
      }
    })
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
  l <- sub(pattern = "-", replacement = "\u2212", x = l)
  if (!is.null(phantom_text)) {
    phantom_text <- paste0(
      ifelse(p < 0, "\u2212", ""),
      phantom_text
    )
    if (is.null(phantom_color)) {
      phantom_color <- "white"
    }
    l <- paste0(
      l,
      "<span style='color: ",
      phantom_color,
      "'>",
      phantom_text,
      "</span>"
    )
  }
  if (percentile) {
    l <- as.character(as.numeric(l) * 100)
  }

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

d <- read_csv("battery.csv", show_col_types = FALSE) |>
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
  arrange(Date) %>%
  filter(FALSE)

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
    `tooltip-bg` = "var(--bs-primary)",
    `tooltip-color` = "var(--bs-light)",
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
      use_googlefont("Roboto Condensed"),
      tags$style(HTML(
        "
      .rt-tr-striped {--var(theme-lightblue)}
      .corinput {
        background-color: var(--bs-primary) !important;
        color: var(--bs-light) !important;
        }
      table.cortable input[type='number']  {
        background-color: var(--bs-primary) !important;
        color: var(--bs-light) !important;
      }
    "
      )),
      tags$script(HTML(
        "
  $(document).on('shiny:connected', function() {
    var mq = window.matchMedia('(prefers-color-scheme: dark)');
    Shiny.setInputValue('dark_mode', mq.matches);
    mq.addEventListener('change', function(e) {
      Shiny.setInputValue('dark_mode', e.matches);
    });
  });
"
      ))
    )
  ),
  ## score ----
  nav_panel(
    "Data Entry",
    value = "score",
    div(
      style = "margin-top: 0px;",
      fluidRow(
        style = "display: flex; align-items: flex-end;",
        column(
          width = 4,
          textInput(
            "txtPerson",
            label = tooltip(
              span(
                "Name (optional)",
                bs_icon(
                  "info-circle-fill",
                  class = "text-info"
                )
              ),
              tagList(
                p("This information is used to create a report."),
                "Like all other information in this app, the",
                strong("Name"),
                "is private because it stays locally on your machine. It is never sent to a third-party server."
              )
            ),
            placeholder = "Person's Name",
            width = "100%"
          )
        ),
        column(
          width = 4,
          dateInput(
            "dateBirthdate",
            label = tooltip(
              span(
                "Birthdate (YYYY-MM-DD)",
                bs_icon(
                  "info-circle-fill",
                  class = "text-info"
                )
              ),
              tagList(
                p(
                  "This information is used to calculate the person's age at the time of testing, which is then use to estimate the correlations among the test scores."
                ),

                "Like all other information in this app, the",
                strong("Birthdate"),
                "is private because it stays locally on your machine. It is never sent to a third-party server."
              )
            ),
            width = "100%",
            value = as.Date("2015-04-15")
          )
        ),
        column(
          width = 4,
          numericInput(
            inputId = "defaultReliability",
            label = tooltip(
              span(
                "Default Reliability",
                bs_icon(
                  "info-circle-fill",
                  class = "text-info"
                )
              ),
              "When a reliability coefficient for a test is unknown, this value is used as the default value."
            ),
            min = 0,
            max = 1,
            step = 0.01,
            value = 0.96,
            width = "100%"
          )
        )
      )
    ),
    fluidRow(
      column(
        width = 6,
        style = "display: flex; justify-content: flex-start; flex-direction: row; align-items: flex-end; gap: 10px;",
        hidden(
          div(
            id = "hidden_add_score",
            actionButton(
              inputId = "add_score",
              label = "Add Test Score",
              class = "btn-primary"
            )
          )
        ),
        div(
          id = "my_file_input_container",
          tooltip(
            fileInput(
              "loaddata",
              label = NULL,
              buttonLabel = "Import",
              accept = ".xlsx",
              placeholder = NULL
            ),
            "Previously exported files can be imported. Before importing, export any information you would like to save."
          )
        ),
        div(
          tooltip(
            downloadButton(
              "dl",
              "Export",
              icon = NULL,
              class = "btn btn-secondary"
            ),
            "Exporting means saving all scores and other changes to an Excel file. The file will be saved to your browser's download folder. It is probably better to move the file to a more permanent location where it can be imported later."
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
    h4("Composite IQ", id = "hciq"),
    div(
      reactableOutput("grdIQ", height = "auto")
    ),
    p(
      "Created by",
      tags$a(
        "W. Joel Schneider",
        href = "https://wjschne.github.io/",
        target = "_blank"
      )
    ),
    p(
      "A full discussion on why, how, and when to make a composite IQ, see Schneider, W. J., Reynolds, C. R., McGrew, K. S., & Salekin, K. L. (2026).",
      a(
        "Life-and-death psychometrics: Generalizable best methods for combining scores in intellectual disability and other diagnostic assessments",
        href = "https://doi.org/10.1037/jpn0000032",
        .noWS = "after"
      ),
      ".",
      em("Journal of Pediatric Neuropsychology, 12", .noWS = "after"),
      "(2), 47",
      HTML("&ndash;", .noWS = "outside"),
      "67."
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
  ## plot ----
  nav_panel(
    "Plot",
    value = "composite_plot",
    div(
      style = "display: flex; gap: 10px;",
      "Original Scores",
      input_switch("toggle_correct_plot", label = "Corrected Scores", TRUE),
      div(
        class = "right-aligned-div",
        tooltip(
          span(
            "About This Plot",
            bs_icon(
              "info-circle-fill",
              class = "text-info"
            )
          ),
          "The large, brightly colored normal distribution in the center of the plot is the population distribution of IQ, with a mean of 100 and a standard deviation of 15. The individual IQ scores are along the bottom, and the composite IQ is one row above. Each IQ has a point showing the score, a thick line showing the 68% confidence interval, a thin line showing 95% confidence interval, and a normal distribution showing the likely distribution of true scores conditioned on the observed score. The confidence intervals and conditional distributions are not centered on the observed score because the estimated true score regresses to the population mean. The type of confidence intervals used here are equivalent to Bayesian credible intervals."
        )
      )
    ),
    div(
      plotOutput("ciq", height = 600)
    )
  ),
  ## outlier ----
  nav_panel(
    "Outliers",
    value = "outlier",
    div(
      style = "display: flex; gap: 10px;",
      "Original Scores",
      input_switch("toggle_correct", label = "Corrected Scores", TRUE),
      div(
        class = "right-aligned-div",
        tooltip(
          span(
            "About This Plot",
            bs_icon(
              "info-circle-fill",
              class = "text-info"
            )
          ),
          span(
            "Each IQ has a population distribution on the left with a mean of 100 and standard deviation of 15. The individual IQs have a conditional normal distribution on the right. These are not confidence intervals. They are the distributions conditioned on composite IQ. That is, they show the likely distributions of the individual IQs given the composite IQ. When the conditional proportion is very high or very low, the observed IQ is outside its usual range. The profile unusualness statistic tells how unusual the profile is after controlling for the composite IQ. The statistic is a conditional Mahalanobis distance, which has a chi square distribution with ",
            em("k"),
            " &minus; 1 degrees of freedom. The proportion (",
            em("p", .noWS = "outside"),
            ") associated with the conditional Mahalanobis distance tells how unusual the profile is compared to all other profiles with the same composite IQ."
          )
        )
      )
    ),
    div(
      reactableOutput("grdOutlier", height = "auto")
    ),
    plotOutput("plot_cm", height = 600, width = 600L)
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
        div(
          actionButton(
            "add_edition",
            label = "Add Edition",
            class = "btn-primary",
            width = "175px"
          )
        ),
        tags$span(
          "This list refreshes to its defaults in each session. For persistent changes, use the Export and Import buttons on the Entry tab. If you have suggestions for permanent additions, feel free to email me at ",
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
      div(
        reactableOutput("grdEdition", height = "auto")
      )
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
        uiOutput("ph_add_flynn_item"),
        textOutput("FlynnSelected")
      ),
      div(
        reactableOutput("grdFlynnItem")
      )
    )
  ),
  nav_panel(
    "Calculations",
    value = "calculations",
    h5("Composite Score"),
    tags$img(
      src = "composite_equation.svg",
      width = 400,
      style = "background-color: white;"
    ),
    h5("Composite Reliability"),
    tags$img(
      src = "composite_reliability_equation.svg",
      width = 650,
      style = "background-color: white;"
    ),
    h5("Confidence Intervals"),
    tags$img(
      src = "ci_equation.svg",
      width = 1000,
      style = "background-color: white;"
    )
  ),
  nav_spacer(),
  nav_item(input_dark_mode(id = "dark_mode")),
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

# constants ----
myreactabletheme <- reactableTheme(
  color = my_primary_darkest,
  backgroundColor = my_primary_light,
  borderColor = "var(--rt-border)",
  stripedColor = "var(--bs-dark)",
  highlightColor = my_primary_medium,
  rowStripedStyle = list(
    backgroundColor = my_primary_lightest,
    color = my_primary_dark
  ),
  cellPadding = "4px 4px",
  style = list(
    fontFamily = "Roboto Condensed, Arial, sans-serif"
  ),
  headerStyle = list(
    backgroundColor = my_primary,
    color = "white"
  )
)


# server ----
server <- function(input, output, session) {
  # validatation rules ----

  ## valid person ----
  iv_person <- InputValidator$new()
  iv_person$add_rule(
    "dateBirthdate",
    sv_required("Required. Approximate if unknown.")
  )
  iv_person$enable()

  ## valid family ----
  iv_family_add <- InputValidator$new()
  iv_family_add$add_rule("family_add_new", sv_required())

  ## valid edition ----
  iv_edition_add <- InputValidator$new()
  iv_edition_edit <- InputValidator$new()
  iv_edition_add$add_rule("family_new", sv_required())
  iv_edition_add$add_rule("battery_new", sv_required())
  iv_edition_edit$add_rule("edition_new", sv_required())
  iv_edition_edit$add_rule("mean_new", sv_required())
  iv_edition_edit$add_rule("sd_new", sv_required())
  iv_edition_edit$add_rule("year_normed_new", sv_optional())
  iv_edition_edit$add_rule("reliability_new", sv_optional())
  iv_edition_edit$add_rule("year_published_new", sv_required())
  iv_edition_edit$add_rule("reliability_new", sv_between(0, 1))
  iv_edition_edit$add_rule(
    "year_published_new",
    sv_between(
      left = 1904L,
      right = 2126L,
      message_fmt = "Implausible Year"
    )
  )
  iv_edition_edit$add_rule(
    "year_normed_new",
    sv_between(
      left = 1904L,
      right = 2126L,
      message_fmt = "Implausible Year"
    )
  )
  iv_edition_add$add_validator(iv_edition_edit)

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

  # current correlations with user overrides
  r_cor <- reactiveVal()
  # estimated correlations
  r_cor_estimate <- reactiveVal()
  # number of rows in rd_score and r_cor
  cor_n <- reactiveVal(0L)
  # pass info across functions
  r_row <- reactiveVal(list(
    id = integer(0),
    name = character(0),
    data = tibble()
  ))

  # current selected flynn rule
  current_flynn_row <- reactiveVal(1L)

  # data for IQ
  rd_iq <- reactiveVal()
  # data for family
  rd_family <- reactiveVal(d_family)
  # data for battery
  rd_battery <- reactiveVal(d_battery)
  # data for flynn rule
  rd_flynn <- reactiveVal(d_flynn)
  # data for flynn rule year
  rd_flynn_item <- reactiveVal(d_flynn_item)
  # data for edition
  rd_edition <- reactiveVal(d_edition)
  # data for score
  rd_score <- reactiveVal(d_score)
  # current selected family
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

  # counters ----
  if (nrow(d_edition) > 0) {
    edition_id_counter <- new_counter(max(d_edition$edition_id))
  } else {
    edition_id_counter <- new_counter()
  }

  if (nrow(d_score) > 0) {
    score_id_counter <- new_counter(max(d_score$score_id))
  } else {
    score_id_counter <- new_counter()
  }

  # functions ----
  flynn_correction <- function(
    score,
    publication_year,
    norm_year,
    administration_date,
    iflynn_id = 1L
  ) {
    correction <- rd_flynn_item() |>
      filter(flynn_id == iflynn_id) |>
      mutate(Until = ifelse(is.na(Until), Inf, Until)) |>
      mutate(
        norm_year = ifelse(is.na(norm_year), publication_year - 1L, norm_year)
      ) |>
      filter(Until > norm_year) |>
      arrange(Until) |>
      mutate(From = lag(Until, default = -Inf), .before = Until) |>
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

  # download ----

  output$dl <- downloadHandler(
    filename = function() {
      fn <- paste0("composite_iq_", ymd(Sys.Date()), ".xlsx")
      if (isTruthy(input$txtPerson)) {
        fn <- paste0(input$txtPerson, "_", fn)
      }
      showModal(modalDialog(
        title = "File Export",
        paste0(
          "A file (",
          fn,
          ") has been exported to your browser's download folder. For safekeeping, move the file to a folder you can remember when you want to import the data."
        ),
        easyClose = TRUE,
        footer = modalButton("Dismiss")
      ))
      fn
    },
    content = function(file) {
      if (is.null(rd_iq())) {
        d_ciq <- tibble(CIQ = numeric(0))
      } else {
        d_ciq <- rd_iq() |>
          select(-data)
      }

      d_list <- list(
        `Composite IQ` = d_ciq,
        Person = tibble(
          Person = input$txtPerson,
          Birthdate = input$dateBirthdate,
          Reliability = input$defaultReliability
        ),
        Score = rd_score(),
        Edition = rd_edition(),
        Battery = rd_battery(),
        Family = rd_family(),
        Flynn = rd_flynn(),
        Flynn_Item = rd_flynn_item(),
        Correlation = r_cor() %>%
          as_tibble(rownames = "rowid")
      )

      write_xlsx(d_list, path = file)
    }
  )

  observeEvent(input$loaddata, {
    req(input$loaddata)
    fn <- input$loaddata$datapath

    sh <- readxl::excel_sheets(fn)

    ss <- c(
      "Family",
      "Battery",
      "Edition",
      "Flynn",
      "Flynn_Item",
      "Score",
      "Correlation",
      "Person"
    )

    missing_sheets <- setdiff(ss, sh)

    if (length(missing_sheets) > 0L) {
      showModal(modalDialog(
        title = "File import error",
        paste0(
          "The import was unsuccessful because the file has missing sheets: ",
          paste0(missing_sheets, collapse = ", "),
          "."
        ),
        easyClose = TRUE,
        footer = modalButton("Dismiss")
      ))
    }

    req(length(missing_sheets) == 0L)
    isolate({
      purrr::walk(ss, \(s) {
        dd <- read_excel(fn, sheet = s)

        if (s == "Family") {
          rd_family(dd)
        }
        if (s == "Battery") {
          rd_battery(dd)
        }
        if (s == "Edition") {
          rd_edition(dd)
        }
        if (s == "Flynn") {
          rd_flynn(dd)
        }
        if (s == "Flynn_Item") {
          rd_flynn_item(dd)
        }
        if (s == "Score") {
          rd_score(mutate(dd, Date = as.Date(ymd(Date))))
        }
        if (s == "Correlation") {
          if (nrow(dd) > 0L) {
            r_cor(
              dd %>%
                column_to_rownames("rowid") %>%
                as.matrix()
            )
          } else {
            r_cor(dd)
          }
        }

        if (s == "Person") {
          if (isTruthy(dd$Person)) {
            updateTextInput(
              session,
              "txtPerson",
              value = dd$Person
            )
          }

          if (isTruthy(dd$Birthdate)) {
            updateDateInput(
              session,
              "dateBirthdate",
              value = as.Date(dd$Birthdate)
            )
          }

          if (isTruthy(dd$Reliability)) {
            updateNumericInput(
              session,
              "defaultReliability",
              value = dd$Reliability
            )
          }
        }
      })
    })
  })

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
    d_new <- isolate(rd_family()) |>
      filter_out(family_id == r_row()$id)

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

    req(length(unique(current_data$family_id)) < 2L)

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

    current_data$Reliability <- signs::signs(
      current_data$Reliability,
      accuracy = .01,
      trim_leading_zeros = TRUE
    )

    current_data$Reliability[is.na(current_data$Reliability)] <- paste0(
      "\u2002",
      signs::signs(
        input$defaultReliability,
        accuracy = .01,
        trim_leading_zeros = TRUE
      ),
      "?"
    )

    reactable(
      current_data,
      showSortIcon = FALSE,
      pagination = FALSE,
      striped = TRUE,
      selection = "single",
      onClick = "select",
      theme = myreactabletheme,
      highlight = TRUE,
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
        Reliability = colDef(align = "center")
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
      eid <- edition_id_counter()

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

  ### edit edition ----
  observeEvent(input$edition_edit_row, {
    req(input$edition_edit_row)
    iv_edition_edit$disable()
    iv_edition_edit$enable()
    d_e <- sort_edition(rd_edition())
    dr <- d_e |> filter(edition_id == input$edition_edit_row)
    redition_id(dr$edition_id)
    r_row(list(id = dr$edition_id, name = dr$Edition, data = dr))
    showModal(
      modalDialog(
        title = "Update Test",
        hidden(
          numericInput(
            "family_new",
            label = NULL,
            value = 1
          )
        ),
        hidden(
          numericInput(
            "battery_new",
            label = NULL,
            value = 1
          )
        ),
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

    if (iv_edition_edit$is_valid()) {
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

      rd_edition(rows_update(d_e, d_new, by = "edition_id"))
      row_id <- d_e |>
        mutate(rid = row_number()) |>
        filter(edition_id == redition_id()) |>
        pull(rid)
      iv_edition_edit$disable()
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
    if (isTruthy(input$dateBirthdate)) {
      show("hidden_add_score")
    } else {
      hide("hidden_add_score")
    }
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
        Edition,
        Score,
        Corrected,
        Battery,
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

    current_data <- current_data |>
      mutate(
        Edition = pmap_chr(
          list(e = Edition, b = Battery, p = Year_Published),
          \(e, b, p) {
            paste(tooltip(
              span(e, bs_icon("info-circle")),
              paste0(b, ": ", e, " (", p, ")")
            ))
          }
        )
      ) |>
      arrange("Date")

    corrected_width <- 2L + 1L * any(round(current_data$Corrected, 0) >= 100)
    score_width <- 2L + 1L * any(round(current_data$Score, 0) >= 100)
    age_width <- 3L + 1L * any(round(current_data$Age, 1) >= 10)

    reactable(
      current_data,
      showSortIcon = FALSE,
      pagination = FALSE,
      striped = TRUE,
      defaultExpanded = FALSE,
      selection = "single",
      onClick = "select",
      fullWidth = FALSE,
      theme = myreactabletheme,
      highlight = TRUE,
      details = function(index, name) {
        htmltools::div(reactable(
          current_data[index, ] |>
            select(
              # Battery,
              Published = Year_Published,
              Normed = Year_Normed,
              Reliability,
              Mean,
              SD,
              Weight,
              Flynn
            ),
          # theme = myreactabletheme,
          columns = list(
            # Battery = colDef(width = 400),
            Published = colDef(align = "center"),
            Normed = colDef(align = "center"),
            Weight = colDef(align = "center"),
            Mean = colDef(align = "center"),
            SD = colDef(align = "center"),
            Reliability = colDef(align = "center")
          )
        ))
      },
      columns = list(
        Actions = colDef(
          html = TRUE,
          sortable = FALSE,
          align = "center"
        ),
        Edition = colDef(html = TRUE, name = "Test", width = 200),
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
          formatC(x, digits = 0, format = "f", width = corrected_width) |>
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
        Flynn = colDef(align = "center", show = FALSE),
        Weight = colDef(align = "center", show = FALSE),
        Year_Published = colDef(
          header = "Published",
          align = "center",
          show = FALSE
        ),
        Year_Normed = colDef(show = FALSE),
        Battery = colDef(show = FALSE)
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
    # updateSelectInput(inputId = "score_family_new", selected = NA)
    # updateSelectInput(inputId = "score_battery_new", selected = NA)
    # updateSelectInput(inputId = "score_edition_new", selected = NA)

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
              # selected = c_family_i,
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
      new_score_id <- score_id_counter()

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
        rows_delete(r_row() |> select(score_id), by = "score_id")
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
        rows_insert(d_new, by = "flynn_item_id") |>
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
      rows_update(d_new, by = c("flynn_item_id", "flynn_id")) |>
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
  observe({
    n <- cor_n()
    if (n < 2L) {
      nav_hide("mainPanel", "correlations")
      nav_hide("mainPanel", "composite_plot")
      nav_hide("mainPanel", "outlier")
    } else {
      nav_show("mainPanel", "correlations")
      nav_show("mainPanel", "composite_plot")
      nav_show("mainPanel", "outlier")
    }
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
    r_cor_estimate(m_r)
    r_cor(m_r)
  })

  output$tbl_cor <- renderUI({
    req(r_cor_estimate())
    d_s <- isolate(rd_score()) |>
      left_join(isolate(rd_edition()), join_by("edition_id"))
    m_r <- r_cor_estimate()
    num_rows <- nrow(m_r)
    num_cols <- nrow(m_r)
    # Generate HTML rows
    table_rows <- lapply(0:num_rows, function(r) {
      iid <- d_s$score_id[r]
      # Generate cells for each row
      cells <- lapply(0:num_cols, function(c) {
        jid <- d_s$score_id[c]
        input_id <- paste0("cell_", iid, "_", jid)
        symmetric_id <- paste0("display_", iid, "_", jid)

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
            tags$td(1, class = "border border-primary-subtle cordisplay")
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
                  class = "border border-primary-subtle cordisplay"
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
                    max = 1,
                    min = -1,
                    step = .01,
                    width = "100%"
                  ),
                  class = "border border-primary-subtle corinput"
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
    req(n > 0L)
    d_s <- isolate(rd_score())
    m_r <- r_cor()
    req(nrow(d_s) == nrow(m_r))

    ids <- as.character(d_s$score_id)
    rownames(m_r) <- ids
    colnames(m_r) <- ids
    pair_id <- combn(ids, m = 2) |>
      t() |>
      `colnames<-`(c("x", "y")) |>
      as_tibble()
    mirror_ids <- paste0("cell_", pair_id$y, "_", pair_id$x)
    input_ids <- paste0("display_", pair_id$x, "_", pair_id$y)
    vals <- purrr::pmap_dbl(pair_id, \(x, y) {
      mirror_id <- paste0("cell_", y, "_", x)
      if (is.null(input[[mirror_id]])) {
        m_r[x, y]
      } else {
        input[[mirror_id]]
      }
    })

    pair_id$v <- vals

    pwalk(pair_id, \(v, x, y) {
      output[[paste0("display_", x, "_", y)]] <- renderText(v)
      if (!is.null(m_r[x, y])) {
        m_r[x, y] <<- v
        m_r[y, x] <<- v
      }
    })

    r_cor(m_r)
  })

  # composite iq ----
  output$grdIQ <- renderReactable({
    req(input$dateBirthdate)
    req(input$mainPanel == "score")
    req(input$defaultReliability)
    req(rd_current())
    cor_n(nrow(rd_score()))
    shinyjs::hide("hciq")
    req(cor_n() > 1L)
    if (!isTruthy(r_cor())) {
      r_cor(r_cor_estimate())
    }
    req(r_cor())
    shinyjs::show("hciq")

    m_r <- r_cor()
    iq_var <- sum(r_cor())
    iq_sd <- sqrt(iq_var)
    d_s <- rd_current() |>
      rename(Original = Score) |>
      mutate(id = row_number()) |>
      select(id, Original, Corrected, Mean, SD, Reliability, Weight) |>
      mutate(
        Reliability = ifelse(
          is.na(Reliability),
          input$defaultReliability,
          Reliability
        )
      )

    r_r <- m_r

    diag(r_r) <- d_s$Reliability
    w <- matrix(d_s$Weight)
    rxx_iq <- (t(w) %*% r_r %*% w)[1, 1] / (t(w) %*% m_r %*% w)[1, 1]

    d_iq <- d_s |>
      pivot_longer(c(Original, Corrected)) |>
      nest(.by = name) |>
      mutate(
        SS = map_dbl(data, \(d) {
          composite_score(
            x = d$value,
            R = m_r,
            w = d$Weight
          )
        }),
        rxx = rxx_iq,
      ) |>
      mutate(
        SEE = 15 * sqrt(rxx - rxx^2),
        est_true = rxx * (SS - 100) + 100,
        UB = qnorm(.975) * SEE + est_true,
        LB = qnorm(.025) * SEE + est_true,
        CI = paste0(scales::number(LB, 1), "&ndash;", scales::number(UB, 1)),
        Percentile = as.character(
          as.numeric(
            prob_label(
              pnorm(SS, 100, 15),
              digits = 2,
              max_digits = 5
            )
          ) *
            100
        )
      )

    rd_iq(d_iq)

    d_display <- d_iq |>
      select(-UB, -LB, -SEE, -data, -est_true) |>
      rename(Reliability = rxx, type = name) |>
      mutate(
        SS = round(SS, 0) |> as.integer() |> as.character(),
        Reliability = prob_label(Reliability, digits = 2, max_digits = 5)
      ) |>
      select(
        `Composite IQ` = SS,
        `95% CI` = CI,
        Percentile,
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
        Original = colDef(align = "center", html = TRUE, width = 150),
        Corrected = colDef(align = "center", html = TRUE, width = 150),
        rownames = colDef(name = "", align = "right", width = 150)
      )
    )
  })

  ## testplot ----

  output$ciq <- renderPlot({
    req(input$dateBirthdate)
    req(input$mainPanel == "composite_plot")
    req(input$defaultReliability)
    req(rd_current())
    cor_n(nrow(rd_score()))
    req(cor_n() > 1L)
    req(nrow(rd_iq()) > 0L)

    if (is.logical(input$dark_mode)) {
      isdark <- input$dark_mode
    } else {
      isdark <- input$dark_mode == "dark"
    }
    if (isdark) {
      fg <- "gray95"
      bg <- "gray5"
    } else {
      bg <- "gray95"
      fg <- "gray5"
    }

    if (input$toggle_correct_plot) {
      d_s <- rd_current() %>%
        rename(score = Corrected)
    } else {
      d_s <- rd_current() %>%
        rename(score = Score)
    }

    d_s <- d_s %>%
      mutate(
        SEE = 15 * sqrt(Reliability - Reliability^2),
        est_true = Reliability * (score - 100) + 100
      )

    d_iq <- rd_iq() |>
      slice(ifelse(input$toggle_correct_plot, 2L, 1L)) %>%
      rename(score = SS)

    suppressWarnings(
      ggplot(d_iq, aes(x = score)) +
        stat_slab(
          data = tibble(score = 100, est_true = 100, SEE = 15),
          mapping = aes(
            fill = after_stat(level),
            xdist = dist_normal(est_true, SEE)
          ),
          p_limits = c(0.000001, .999999),
          .width = 2 * (pnorm(seq(105, 160, 5), 100, 15) - .5),
          height = .925
        ) +
        geom_vline(
          xintercept = seq(40, 160, 5),
          linewidth = .25,
          color = fg,
          alpha = .6
        ) +
        stat_slabinterval(
          aes(xdist = dist_normal(est_true, SEE)),
          data = d_s,
          show_point = FALSE,
          p_limits = c(0.000001, .999999),
          height = .15,
          slab_fill = fg,
          slab_alpha = .2,
          interval_color = fg,
          color = fg
        ) +
        stat_slabinterval(
          aes(xdist = dist_normal(est_true, SEE)),
          show_point = FALSE,
          p_limits = c(0.000001, .999999),
          y = .2,
          height = .2,
          slab_fill = fg,
          interval_color = fg,
          slab_alpha = .3,
          color = fg
        ) +
        geom_text(
          y = 0.2,
          color = fg,
          lineheight = .85,
          aes(label = paste0("Composite IQ\n", round(score))),
          size = 24,
          size.unit = "pt",
          vjust = -.2,
          family = "Roboto Condensed",
        ) +
        geom_point(y = 0.2, size = 3, color = fg) +
        geom_point(data = d_s, y = 0, size = 3, color = fg) +
        ggrepel::geom_text_repel(
          data = d_s,
          aes(label = paste0(Edition, "\n", round(score)), y = 0),
          size = 20 / .pt,
          family = "Roboto Condensed",
          lineheight = .85,
          vjust = -.5,
          force_pull = 0,
          color = fg,
          nudge_y = .02,
          min.segment.length = 0
        ) +
        scale_fill_viridis_d(end = .9, begin = 0, alpha = .5) +
        theme_minimal(base_family = "Roboto Condensed", base_size = 20) +
        theme(
          legend.position = "none",
          panel.grid = element_blank(),
          plot.background = element_rect(bg, color = NA),
          axis.text.x = element_text(color = fg, size = 20)
        ) +
        scale_x_continuous(
          NULL,
          limits = c(40, 160),
          breaks = seq(40, 160, 15),
          minor_breaks = seq(40, 160, 5)
        ) +
        scale_y_continuous(NULL, breaks = NULL, expand = expansion()) +
        coord_cartesian(xlim = c(40, 160), clip = FALSE)
    )
  })

  # outlier ----
  output$grdOutlier <- renderReactable({
    req(input$dateBirthdate)
    req(input$mainPanel == "outlier")
    req(rd_iq())

    n <- nrow(rd_current())
    if (nrow(r_cor()) != n) {
      r_cor(r_cor_estimate())
    }

    d_s <- rd_current() |>
      mutate(switcher = input$toggle_correct) |>
      mutate(Score = if_else(switcher, Corrected, Score)) |>
      select(Edition, Date, Score, Weight, SD, Mean)

    d_iq <- tibble(
      name = c("CIQ", d_s$Edition),
      value = c(rd_iq()$SS[input$toggle_correct * 1 + 1], d_s$Score)
    ) |>
      pivot_wider()
    w <- cbind(
      d_s$Weight,
      diag(nrow(d_s))
    )

    rownames(w) <- d_s$Edition
    colnames(w) <- c("CIQ", d_s$Edition)

    R <- composite_correlation(
      R = r_cor(),
      w
    )

    cm <- unusualprofile::cond_maha(
      d_iq,
      v_dep = d_s$Edition,
      v_ind_composites = "CIQ",
      R = R,
      sigma = c(15, d_s$SD),
      mu = c(100, d_s$Mean)
    )

    output$plot_cm <- renderPlot(
      cm_plot(cm, family = "Roboto Condensed"),
      height = 700,
      width = 700 + 100 * n
    )

    predicted_label <- paste0(
      "Predicted from CIQ = ",
      round(rd_iq()$SS[input$toggle_correct * 1 + 1], 0)
    )

    cm$d_score[seq(1, n), ] |>
      mutate(Date = rd_current()$Date, .before = 1L) |>
      mutate(Edition = rd_current()$Edition, .before = 1L) |>
      mutate(
        p = prob_label(p, digits = 2, max_digits = 6),
        cp = prob_label(cp, digits = 2, max_digits = 6),
        Deviation = signs::signs(round(Score - Predicted, 1)),
        Predicted = scales::number(Predicted, .1)
      ) |>
      select(
        Edition,
        Date,
        Score,
        `Population Proportion` = p,
        Predicted,
        Deviation,
        `Conditional Proportion` = cp,
      ) |>
      reactable(
        showSortIcon = FALSE,
        pagination = FALSE,
        striped = TRUE,
        defaultExpanded = TRUE,
        theme = myreactabletheme,
        highlight = TRUE,
        fullWidth = FALSE,
        columns = list(
          Score = colDef(
            format = colFormat(digits = 0),
            align = "center",
            name = ifelse(input$toggle_correct, "Corrected", "Original")
          ),
          Edition = colDef("Test"),
          Date = colDef(align = "center"),
          Predicted = colDef(
            align = "center",
            name = predicted_label,
            width = 200
          ),
          `Population Proportion` = colDef(align = "center", width = 200),
          `Conditional Proportion` = colDef(align = "center", width = 200),
          `Deviation` = colDef(align = "center")
        )
      )
  })
}


shinyApp(ui, server)
