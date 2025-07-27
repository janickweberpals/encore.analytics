#' Create Study Design Diagram
#'
#' @description
#' Creates a ggplot2-based study design diagram showing measurement windows
#' relative to an index date (cohort entry). Based on principles from
#' Schneeweiss et al. (2019) Ann Intern Med.
#' [![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
#'
#' @param data A data.frame/tibble with study design parameters
#' @param variable_col Column name containing variable names (default: "variable")
#' @param label_col Column name containing variable labels (default: "label")
#' @param dimension_col Column name containing dimension (= Covariate ascertainment window, Washout Window, Eligibility Assessment Window, etc.) names (default: "dimension")
#' @param min_col Column name containing measurement window start (default: "measurement_min")
#' @param max_col Column name containing measurement window end (default: "measurement_max")
#' @param index_date_label Label for the index date line and plot title (default: "Index Date (Cohort Entry)")
#' @param time_unit Time unit for x-axis (default: "Days")
#' @param show_variables_legend Show legend with variables in each dimension (default: TRUE)
#' @param box_height Height of dimension boxes (default: 0.8)
#' @param text_size Base text size (default: 11)
#' @param colors Custom color palette. Can be:
#'   - NULL (default): uses RColorBrewer Set3 palette
#'   - Unnamed character vector: colors assigned to dimensions in order of appearance
#'   - Named character vector/list: dimension names as keys, colors as values
#' @param text_left_align If TRUE, left-aligns text for index point measurements; useful when measurements window are too small to fit large text elements (default: FALSE)
#' @param text_left_align_cutoffs Numeric vector of length 2 specifying cutoffs for left alignment (default: c(-90, 0)). If text_left_align is TRUE, text will be left-aligned for measurements with min_time >= -90 and max_time <= 0.
#'
#' @return A ggplot2 object
#'
#' @export
#'
#' @references
#' Schneeweiss S, Rassen JA, Brown JS, Rothman KJ, Happe L, Arlett P, Dal Pan G, Goettsch W, Murk W, Wang SV.
#' Graphical Depiction of Longitudinal Study Designs in Health Care Databases.
#' Ann Intern Med. 2019 Mar 19;170(6):398-406. doi: 10.7326/M18-3079. Epub 2019 Mar 12. PMID: 30856654.
#'
#' @examples
#'\dontrun{
#' # Example data structure for study design diagram
#' library(encore.analytics)
#' library(tibble)
#'
#' params <- tribble(
#'   ~variable, ~label, ~encoding, ~dimension, ~measurement_min, ~measurement_max,
#'   "c_age", "Age [yrs]", "continuous", "Covariate Assessment Window", 0, 0,
#'   "c_sex", "Sex", "binary", "Covariate Assessment Window", 0, 0,
#'   "c_ecog", "ECOG Performance Status", "ordinal", "Eligibility Assessment Window", -90, -1,
#'   "c_comorbidity", "Comorbidity Score", "continuous", "Covariate Assessment Window", -365, -1,
#'   "c_prior_tx", "Prior Treatment", "binary", "Washout Window", -180, -1,
#'   "outcome", "Overall Survival", "time-to-event", "Follow-up Period", 0, 365
#'   )
#'
#' # Basic usage
#' design_diagram(params)
#'
#' # With custom colors using named list/vector (dimension name -> color)
#' custom_colors <- c(
#'  "Covariate Assessment Window" = "steelblue",
#'  "Eligibility Assessment Window" = "darkblue",
#'  "Washout Window" = "darkgrey",
#'  "Follow-up Period" = "forestgreen"
#'  )
#'
#' design_diagram(params, colors = custom_colors)
#'}
design_diagram <- function(data,
                           variable_col = "variable",
                           label_col = "label",
                           dimension_col = "dimension",
                           min_col = "measurement_min",
                           max_col = "measurement_max",
                           index_date_label = "Index Date\n(Cohort Entry)",
                           time_unit = "Days",
                           show_variables_legend = TRUE,
                           box_height = 0.6,
                           text_size = 16,
                           colors = NULL,
                           text_left_align = FALSE,
                           text_left_align_cutoffs = c(-90, 0)
){

  # input checks

  # input must be a data.frame/tibble
  assertthat::assert_that(is.data.frame(data),msg = "data must be a data frame or tibble")

  # column names must be character strings (check this BEFORE checking if they exist)
  assertthat::assert_that(
    is.character(variable_col), is.character(label_col),
    is.character(dimension_col), is.character(min_col), is.character(max_col),
    msg = "Column name parameters must be character strings"
  )

  # required columns must be present
  required_cols <- c(variable_col, label_col, dimension_col, min_col, max_col)
  assertthat::assert_that(all(required_cols %in% names(data)), msg = paste("Missing required columns:", paste(setdiff(required_cols, names(data)), collapse = ", ")))

  # index date label and time unit must be single character strings
  assertthat::assert_that(
    is.character(index_date_label), length(index_date_label) == 1,
    is.character(time_unit), length(time_unit) == 1,
    msg = "Label parameters must be character strings"
  )

  # title and subtitle must be character strings
  assertthat::assert_that(
    is.numeric(box_height), box_height > 0, box_height <= 1,
    msg = "box_height must be numeric between 0 and 1"
  )

  # text size must be a positive number
  assertthat::assert_that(
    is.numeric(text_size), text_size > 0,
    msg = "text_size must be a positive number"
  )

  # text_left_align_cutoffs must be a numeric vector of length 2
  assertthat::assert_that(
    is.numeric(text_left_align_cutoffs) && length(text_left_align_cutoffs) == 2,
    msg = "text_left_align_cutoffs must be a numeric vector of length 2"
  )

  # if text_left_align is TRUE, text_left_align_cutoffs must be specified
  if(text_left_align){
    assertthat::assert_that(
      is.numeric(text_left_align_cutoffs) && length(text_left_align_cutoffs) == 2,
      msg = "text_left_align_cutoffs must be a numeric vector of length 2"
    )
  }

  # Standardize column names for processing
  plot_data <- data |>
    dplyr::select(
      variable = dplyr::all_of(variable_col),
      label = dplyr::all_of(label_col),
      dimension = dplyr::all_of(dimension_col),
      min_time = dplyr::all_of(min_col),
      max_time = dplyr::all_of(max_col)
    ) |>
    # Identify point-in-time measurements at index date
    dplyr::mutate(
      is_index_point = (min_time == 0 & max_time == 0),
      # For index point measurements, create small rectangle overlay
      min_time_adj = ifelse(is_index_point, 0, min_time),
      max_time_adj = ifelse(is_index_point, 0, max_time)
    )

  # Get unique dimensions and assign colors
  dimensions <- unique(plot_data$dimension)
  n_dimensions <- length(dimensions)

  if (is.null(colors)) {
    # Use default color palette
    if (n_dimensions <= 3) {
      colors <- RColorBrewer::brewer.pal(max(3, n_dimensions), "Set3")[1:n_dimensions]
    } else if (n_dimensions <= 8) {
      colors <- RColorBrewer::brewer.pal(n_dimensions, "Set3")
    } else {
      colors <- grDevices::rainbow(n_dimensions, alpha = 0.7)
    }
    # Create color mapping with default colors
    color_mapping <- stats::setNames(colors, dimensions)
  } else if (is.character(colors) && is.null(names(colors))) {
    # User provided unnamed vector of colors
    if (length(colors) < n_dimensions) {
      stop("Number of colors provided (", length(colors), ") is less than number of dimensions (", n_dimensions, ")")
    }
    color_mapping <- stats::setNames(colors[1:n_dimensions], dimensions)
  } else if (is.character(colors) && !is.null(names(colors))) {
    # User provided named vector/list of colors (dimension name -> color)
    missing_dims <- setdiff(dimensions, names(colors))
    if (length(missing_dims) > 0) {
      stop("Colors not provided for dimensions: ", paste(missing_dims, collapse = ", "))
    }
    color_mapping <- colors[dimensions]  # Reorder to match dimension order
  } else {
    stop("Invalid colors parameter. Must be NULL, unnamed character vector, or named character vector/list.")
  }

  # Create unique dimension-time window combinations for y-positioning
  unique_windows <- plot_data |>
    dplyr::select(dimension, min_time_adj, max_time_adj) |>
    dplyr::distinct() |>
    dplyr::arrange(min_time_adj, max_time_adj, dimension) |>
    dplyr::mutate(
      window_id = paste(dimension, min_time_adj, max_time_adj, sep = "_"),
      # Assign unique letters to each dimension-window combination
      dimension_letter = letters[dplyr::row_number()]
    ) |>
    # Reorder by dimension_letter (ascending alphabetical order) for y-positioning
    dplyr::arrange(dimension_letter) |>
    dplyr::mutate(
      y_pos = (max(dplyr::row_number()) - dplyr::row_number()) * 0.8 + 1  # Reverse order so 'a' is at top
    )

  # Join position and color data
  plot_data <- plot_data |>
    dplyr::left_join(
      unique_windows |> dplyr::select(dimension, min_time_adj, max_time_adj, y_pos, dimension_letter),
      by = c("dimension", "min_time_adj", "max_time_adj")
    ) |>
    dplyr::left_join(
      data.frame(dimension = names(color_mapping), color = as.character(color_mapping), stringsAsFactors = FALSE),
      by = "dimension"
    ) |>
    dplyr::mutate(
      dimension_label = paste0(dimension, "^", dimension_letter)
    )

  # Calculate plot range with padding
  time_range <- range(c(plot_data$min_time_adj, plot_data$max_time_adj), na.rm = TRUE)
  time_padding <- diff(time_range) * 0.1
  x_limits <- c(time_range[1] - time_padding, time_range[2] + time_padding)

  # Calculate the y position for the index date label (above the highest box)
  max_y_pos <- max(unique_windows$y_pos)
  min_y_pos <- min(unique_windows$y_pos)
  index_label_y <- max_y_pos + box_height/2 + 0.8  # Well above the highest box
  x_axis_label_y <- min_y_pos - box_height/2 - 0.8  # Well below the lowest box and timeline

  # Create the base plot
  p <- ggplot2::ggplot(plot_data) +
    # Add measurement window rectangles
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = min_time_adj,
        xmax = max_time_adj,
        ymin = y_pos - box_height/2, ymax = y_pos + box_height/2,
        fill = dimension
      ),
      alpha = 0.7,
      color = "black",
      linewidth = 0.5
    )

  # Add dimension labels inside boxes with superscript for non-index measurements (only if they exist)
  non_index_data <- plot_data |> dplyr::filter(!is_index_point)

  if (nrow(non_index_data) > 0) {
    p <- p +
      ggplot2::geom_text(
        data = non_index_data |>
          dplyr::group_by(dimension, y_pos, color, dimension_letter, min_time_adj, max_time_adj) |>
          dplyr::summarize(
            min_time = min(min_time_adj),
            max_time = max(max_time_adj),
            n_vars = dplyr::n(),
            .groups = "drop"
          ) |>
          dplyr::mutate(
            mid_time = (min_time + max_time) / 2,
            box_width = max_time - min_time,
            time_window = ifelse(
              min_time == max_time,
              paste0("[", min_time, "]"),
              paste0("[", min_time, " to ", max_time, "]")
            ),
            # Use Unicode superscript characters instead of R expressions
            superscript_letter = dplyr::case_when(
              dimension_letter == "a" ~ "\u1d43",
              dimension_letter == "b" ~ "\u1d47",
              dimension_letter == "c" ~ "\u1d9c",
              dimension_letter == "d" ~ "\u1d48",
              dimension_letter == "e" ~ "\u1d49",
              dimension_letter == "f" ~ "\u1da0",
              dimension_letter == "g" ~ "\u1d4d",
              dimension_letter == "h" ~ "\u02b0",
              TRUE ~ paste0("(", dimension_letter, ")")  # fallback
            ),
            label_text = paste0(dimension, superscript_letter, "\n", time_window),
            # Check if text fits inside box (rough estimate: 1 character ≈ 0.6 time units)
            text_width_estimate = nchar(dimension) * 0.6,
            text_fits = box_width > text_width_estimate,
            # Position text: if min_time >= -90 AND max_time <= 0, place to the left; otherwise use fit logic
            should_place_left = min_time >= text_left_align_cutoffs[1] & max_time <= text_left_align_cutoffs[2],
            text_x = ifelse(should_place_left & text_left_align, min_time - 0.5,
                            ifelse(text_fits, mid_time, min_time - 0.5)),
            text_hjust = ifelse(should_place_left & text_left_align, 1,
                                ifelse(text_fits, 0.5, 1))  # right-align if left, center if inside, right if outside
          ),
        ggplot2::aes(x = text_x, y = y_pos, label = label_text, hjust = text_hjust),
        size = text_size * 0.3,
        fontface = "bold",
        color = "black",
        parse = FALSE  # No parsing needed
      )
  }

  # Add dimension labels to the left for index point measurements (only if they exist)
  index_data <- plot_data |> dplyr::filter(is_index_point)

  if (nrow(index_data) > 0) {
    p <- p +
      ggplot2::geom_text(
        data = index_data |>
          dplyr::group_by(dimension, y_pos, color, dimension_letter) |>
          dplyr::summarize(
            n_vars = dplyr::n(),
            .groups = "drop"
          ) |>
          dplyr::mutate(
            # Use Unicode superscript characters
            superscript_letter = dplyr::case_when(
              dimension_letter == "a" ~ "\u1d43",
              dimension_letter == "b" ~ "\u1d47",
              dimension_letter == "c" ~ "\u1d9c",
              dimension_letter == "d" ~ "\u1d48",
              dimension_letter == "e" ~ "\u1d49",
              dimension_letter == "f" ~ "\u1da0",
              dimension_letter == "g" ~ "\u1d4d",
              dimension_letter == "h" ~ "\u02b0",
              TRUE ~ paste0("(", dimension_letter, ")")  # fallback
            ),
            label_text = paste0(dimension, superscript_letter, "\n[0]")
          ),
        ggplot2::aes(x = -0.8, y = y_pos, label = label_text, color = dimension),
        size = text_size * 0.3,
        fontface = "bold",
        hjust = 1,  # Right align text to position it to the left of the rectangle
        parse = FALSE
      )
  }

  # Continue with the rest of the plot
  p <- p +
    # Add vertical line for index date (only through the main plot area, not extending to labels)
    ggplot2::annotate(
      "segment",
      x = 0, xend = 0,
      y = min_y_pos - box_height/2 - 0.5, yend = max_y_pos + box_height/2 + 0.5,
      linetype = "solid",
      color = "black",
      linewidth = 1.2
    ) +
    # Add index date label above the vertical line
    ggplot2::annotate(
      "text",
      x = 0,
      y = index_label_y,
      label = index_date_label,
      hjust = 0.5,
      vjust = 0.5,
      size = text_size * 0.35,
      fontface = "bold",
      color = "black"
    ) +
    # Add x-axis label centered below the vertical line
    ggplot2::annotate(
      "text",
      x = 0,
      y = x_axis_label_y,
      label = paste("Time from Index Date (", time_unit, ")", sep = ""),
      hjust = 0.5,
      vjust = 0.5,
      size = text_size * 0.32,
      fontface = "bold",
      color = "black"
    ) +
    # Add horizontal timeline
    ggplot2::geom_hline(
      yintercept = min_y_pos - box_height/2 - 0.3,  # Position timeline below the lowest box
      color = "black",
      linewidth = 0.8
    ) +
    # Add timeline arrows
    ggplot2::annotate(
      "segment",
      x = x_limits[1], xend = x_limits[2],
      y = min_y_pos - box_height/2 - 0.3, yend = min_y_pos - box_height/2 - 0.3,
      arrow = ggplot2::arrow(length = ggplot2::unit(0.3, "cm"), ends = "both"),
      color = "black",
      linewidth = 0.8
    ) +
    # Customize scales
    ggplot2::scale_fill_manual(values = color_mapping, guide = "none") +
    ggplot2::scale_color_manual(values = color_mapping, guide = "none") +
    ggplot2::scale_x_continuous(
      breaks = scales::breaks_pretty(n = 8),
      expand = c(0.02, 0)
    ) +
    ggplot2::scale_y_continuous(
      limits = c(x_axis_label_y - 0.2, index_label_y + 0.2),
      expand = c(0, 0)
    ) +
    # Apply theme
    ggplot2::theme_minimal(base_size = text_size) +
    ggplot2::theme(
      axis.title.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),  # Remove default x-axis title
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      plot.title = ggplot2::element_blank(),  # Remove plot title
      panel.border = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(20, 20, 20, 20)
    )

  # Add variables legend if requested
  if (show_variables_legend) {
    # Create legend text for each dimension-window combination
    legend_data <- plot_data |>
      dplyr::arrange(dimension_letter, variable) |>
      dplyr::group_by(dimension, dimension_letter, min_time_adj, max_time_adj) |>
      dplyr::summarize(
        variables = paste(label, collapse = ", "),
        .groups = "drop"
      ) |>
      dplyr::arrange(dimension_letter) |>  # Sort by alphabetical order
      dplyr::mutate(
        time_window = ifelse(
          min_time_adj == max_time_adj,
          paste0("[", min_time_adj, "]"),
          paste0("[", min_time_adj, " to ", max_time_adj, "]")
        ),
        legend_text = paste0(dimension_letter, ") ", dimension, " ", time_window, ": ", variables)
      )

    legend_text <- paste(legend_data$legend_text, collapse = "\n")

    # Add legend as caption
    p <- p +
      ggplot2::labs(caption = paste("Variables by dimension:\n", legend_text, sep = "")) +
      ggplot2::theme(
        plot.caption = ggplot2::element_text(
          hjust = 0,
          size = text_size * 0.8,
          margin = ggplot2::margin(t = 15)
        )
      )
  }

  return(p)

}
