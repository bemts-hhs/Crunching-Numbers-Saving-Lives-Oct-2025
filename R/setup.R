###_____________________________________________________________________________
### Custom functions and other utilities for the Annual Trauma Report 2024 ----
### For any analyses, these data must be loaded for the applicable section
### This script should be reviewed and ran first before going to any other
### scripts in the project
###_____________________________________________________________________________

# Utilize the air package for code formatting

### packages ----

# these packages are utilized in this project and must be loaded
# renv::install(c(
#   'renv',
#   'usethis',
#   'devtools',
#   'tidyverse',
#   'nemsqar',
#   'quarto',
#   'qr',
#   'gt',
#   'showtext',
#   'systemfonts',
#   'sysfonts',
#   'webshot2'
# ))

###_____________________________________________________________________________
# Font setup ----
###_____________________________________________________________________________

# Enable showtext globally (ensures vector font rendering)
showtext::showtext_auto()
showtext::showtext_opts(dpi = 600) # use 600 dpi for publication-grade output

# Locate Work Sans fonts (system-installed or Google fonts)
all_fonts <- systemfonts::system_fonts()

work_sans <- all_fonts |>
  dplyr::filter(name == "WorkSans-Regular") |>
  dplyr::pull(path)

work_sans_extrabold <- all_fonts |>
  dplyr::filter(name == "WorkSans-ExtraBold") |>
  dplyr::pull(path)

# Register font family
sysfonts::font_add(
  family = "Work Sans",
  regular = work_sans,
  bold = work_sans_extrabold
)

###_____________________________________________________________________________
# Custom functions for statistics ----
###_____________________________________________________________________________

# The function modifies several aspects of the `{gt}` table:
# - Row Groups: Custom styling for row group text and background fill.
# - Column Labels & Spanners: Adjusted font size, color, and alignment.
# - Table Body: Formats text with different alignments and font styles.
# - Borders: Adds top borders to row groups and left borders to selected
#   columns.
# - Source Notes: Includes `{fontawesome}` icons and relevant metadata.
tab_style_hhs <- function(
  gt_object,
  row_groups = 14,
  column_labels = 14,
  title = 20,
  subtitle = 18,
  spanners = 16,
  body = 14,
  source_note = 12,
  footnote = 12,
  message_text,
  row_group_fill = "#E0A624",
  row_group_fill_alpha = 0.5,
  bold_first_col = 1,
  border_cols,
  border_color1 = "#19405B",
  border_color2 = "#70C8B8"
) {
  out <- gt_object |>

    # Set the font for the table
    gt::opt_table_font(
      font = "Work Sans",
      stack = NULL,
      weight = NULL,
      style = NULL,
      add = TRUE
    ) |>

    # Style the stub (row names) section
    gt::tab_style(
      locations = gt::cells_stub(),
      style = gt::cell_text(
        size = gt::px(body),
        font = "Work Sans SemiBold",
        color = "black",
        align = "left"
      )
    ) |>

    # Style the row groups
    gt::tab_style(
      style = gt::cell_text(
        size = gt::px(row_groups),
        font = "Work Sans SemiBold",
        color = "#03617A",
        align = "left"
      ),
      locations = gt::cells_row_groups(groups = gt::everything())
    ) |>

    # Apply background color to row groups
    gt::tab_style(
      style = gt::cell_fill(
        color = row_group_fill,
        alpha = row_group_fill_alpha
      ),
      locations = gt::cells_row_groups(groups = gt::everything())
    ) |>

    # Add top border to row groups
    gt::tab_style(
      style = gt::cell_borders(
        sides = "top",
        color = border_color1,
        weight = gt::px(3) # Adjust thickness as needed
      ),
      locations = gt::cells_row_groups(groups = gt::everything())
    ) |>

    # Style column labels
    gt::tab_style(
      style = gt::cell_text(
        size = gt::px(column_labels),
        font = "Work Sans SemiBold",
        color = "#03617A",
        align = "center",
        style = "italic"
      ),
      locations = gt::cells_column_labels(gt::everything())
    ) |>

    # Style the table title
    gt::tab_style(
      style = gt::cell_text(
        font = "Work Sans ExtraBold",
        color = "#19405B",
        size = gt::px(title)
      ),
      locations = gt::cells_title(groups = "title")
    ) |>

    # Style the table subtitle
    gt::tab_style(
      style = gt::cell_text(
        font = "Work Sans SemiBold",
        color = "#70C8B8",
        size = gt::px(subtitle)
      ),
      locations = gt::cells_title(groups = "subtitle")
    ) |>

    # Style the spanner labels (column headers spanning multiple columns)
    gt::tab_style(
      style = gt::cell_text(
        font = "Work Sans SemiBold",
        color = "#03617A",
        size = gt::px(spanners),
        align = "center"
      ),
      locations = gt::cells_column_spanners()
    ) |>

    # Style the first column (typically used for labels)
    gt::tab_style(
      style = gt::cell_text(
        font = "Work Sans SemiBold",
        color = "black",
        size = gt::px(body),
        align = "left"
      ),
      locations = gt::cells_body(columns = {{ bold_first_col }})
    ) |>

    # Style all other body cells (except first column)
    gt::tab_style(
      style = gt::cell_text(
        font = "Work Sans",
        color = "black",
        size = gt::px(body),
        align = "center"
      ),
      locations = gt::cells_body(columns = -1)
    ) |>

    # Style row names (stub)
    gt::tab_style(
      style = gt::cell_text(
        font = "Work Sans Black",
        size = gt::px(body),
        color = "black",
        align = "left"
      ),
      locations = gt::cells_stub(rows = gt::everything())
    ) |>

    # Style footnotes text
    gt::tab_style(
      style = gt::cell_text(
        weight = "normal",
        font = "Work Sans",
        size = gt::px(footnote),
        color = "#19405B"
      ),
      locations = gt::cells_source_notes()
    ) |>

    # Style source note text
    gt::tab_style(
      style = gt::cell_text(
        weight = "normal",
        font = "Work Sans",
        size = gt::px(source_note),
        color = "#19405B"
      ),
      locations = gt::cells_footnotes()
    ) |>

    # Add a left-side border to specified columns
    gt::tab_style(
      locations = gt::cells_body(columns = {{ border_cols }}),
      style = gt::cell_borders(
        sides = c("left"),
        weight = gt::px(2),
        color = border_color2
      )
    ) |>

    # Align all columns except the first one to the center
    gt::cols_align(align = "center", columns = 2)

  return(out)
}

###_____________________________________________________________________________
# Data import functions ----
###_____________________________________________________________________________

import_nemsqa_result <- function(measure, type = c("result", "population")) {
  # The measure will be a NEMSQA measure name with title case text and an '-'
  # in-between the name and number, like Airway-01

  # validate type
  type <- match.arg(type, choices = c("result", "population"))

  # use the correct file extension based on type
  # path_extension is defined by the corresponding environment variables that
  # are loaded into memory prior to running this function
  if (type == "result") {
    path_extension <- overall
  } else {
    path_extension <- population
  }

  # clean measure name to match file structure
  measure_sub <- measure |>
    tolower() |>
    gsub(pattern = "-", replacement = "_")

  # read in the file
  data <- readr::read_csv(
    file = glue::glue(
      "{base_path}{measure}/{type}/{measure_sub}{path_extension}"
    )
  )
}

###_____________________________________________________________________________
# Formatting for GT tables for this project ----
###_____________________________________________________________________________
format_gt <- function(gt_object) {
  # format the gt object
  out <- gt_object |>
    dplyr::select(-prop_label) |> # unneeded column
    dplyr::rename_with(
      # all columns to title case
      ~ stringr::str_to_title(string = .)
    ) |>
    gt::gt() |>
    gt::fmt_number(
      # number formatting with comma separation at thousands etc.
      columns = Numerator:Denominator,
      drop_trailing_zeros = TRUE
    ) |>
    gt::fmt_percent(
      # % formatting for all proportions
      columns = Prop:Upper_ci,
      decimals = 2,
      drop_trailing_zeros = TRUE
    ) |>
    gt::cols_label(
      # change some names explicitly
      Pop ~ "Population",
      Prop ~ "Performance"
    ) |>
    gt::cols_merge(
      # merge proportion and corresponding binomial confidence intervals
      columns = Prop:Upper_ci,
      pattern = "{1} [{2}-{3}]"
    ) |>
    tab_style_hhs(
      border_cols = 2:tidyselect::last_col(),
      column_labels = 18,
      body = 16
    )

  # return the gt object, formatted
  return(out)
}
