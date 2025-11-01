#' Create Diverging Stacked Bar Chart for Likert Scale Data
#'
#' @description
#' Generates a diverging stacked bar chart (also known as a Likert plot) from
#' survey or questionnaire data with ordered categorical responses. The function
#' centers the chart around a neutral midpoint, with negative responses on the
#' left and positive responses on the right.
#'
#' @param data A data frame in long format with at minimum two columns: one for
#'   the question/item and one for the response category. If NULL, uses built-in
#'   sample data. Default is NULL.
#' @param item_col Character string specifying the column name containing the
#'   items/questions. Default is "item".
#' @param response_col Character string specifying the column name containing
#'   the response categories. Default is "response".
#' @param center_value The value to be treated as the center/neutral point.
#'   Can be numeric (e.g., 3 for a 1-5 scale) or character (e.g., "Neutral").
#'   If NULL, automatically calculates as the middle value. Default is NULL.
#' @param center_separate Logical or character indicating how to handle the center value.
#'   Options: FALSE (include in main chart), TRUE (separate facet), "remove" (exclude
#'   from output), or "only" (return only the neutral data). Only applicable when
#'   there's an odd number of response levels. Default is FALSE.
#' @param palette Character string specifying a ColorBrewer palette name.
#'   Use "BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", or "RdYlBu" for
#'   diverging palettes. Default is "RdBu".
#' @param reverse_palette Logical indicating whether to reverse the color palette.
#'   Default is FALSE.
#' @param neutral_color Character string specifying the color for the neutral/center
#'   category. If NULL, uses a medium gray or the palette's center color.
#'   Default is NULL.
#' @param item_labels Optional named vector for custom item labels. Names should
#'   match values in item_col. Default is NULL (uses original labels).
#' @param response_order Optional character vector specifying the order of response
#'   categories from most negative to most positive. If NULL, uses factor levels
#'   or alphabetical order. Default is NULL.
#' @param title Character string for plot title. Default is NULL.
#' @param subtitle Character string for plot subtitle. Default is NULL.
#' @param show_percentages Logical indicating whether to display percentage labels
#'   on bars. Default is TRUE.
#' @param percentage_size Numeric value for the size of percentage labels.
#'   Default is 3.
#'
#' @return A ggplot2 object containing the diverging stacked bar chart.
#'
#' @examples
#' # Use default sample data
#' plot_likert_diverging()
#'
#' # With center value in separate facet
#' plot_likert_diverging(center_separate = TRUE)
#'
#' # Remove center value entirely
#' plot_likert_diverging(center_separate = "remove")
#'
#' # Get only neutral data for custom plotting
#' plot_likert_diverging(center_separate = "only")
#'
#' # Custom palette
#' plot_likert_diverging(palette = "PuOr", reverse_palette = TRUE)
#'
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @importFrom RColorBrewer brewer.pal
#' @importFrom scales percent_format
#'
#' @export
plot_likert_diverging <- function(
  data = NULL,
  item_col = "item",
  response_col = "response",
  center_value = NULL,
  center_separate = FALSE,
  palette = "RdBu",
  reverse_palette = FALSE,
  neutral_color = NULL,
  item_labels = NULL,
  response_order = NULL,
  title = NULL,
  subtitle = NULL,
  show_percentages = TRUE,
  percentage_size = 3
) {
  # Load required packages
  require(ggplot2)
  require(dplyr)
  require(tidyr)
  require(RColorBrewer)
  require(scales)

  # Generate sample data if none provided
  if (is.null(data)) {
    set.seed(123)
    data <- data.frame(
      item = rep(
        c(
          "Overall Satisfaction",
          "Would Recommend",
          "Ease of Use",
          "Value for Money"
        ),
        each = 150
      ),
      response = sample(
        c(
          "Strongly Disagree",
          "Disagree",
          "Neutral",
          "Agree",
          "Strongly Agree"
        ),
        600,
        replace = TRUE,
        prob = c(0.1, 0.15, 0.2, 0.35, 0.2)
      )
    )
  }

  # Rename columns to standard names for internal processing
  data <- data |>
    rename(item_var = all_of(item_col), response_var = all_of(response_col))

  # Determine response order
  if (is.null(response_order)) {
    if (is.factor(data$response_var)) {
      response_order <- levels(data$response_var)
    } else {
      response_order <- sort(unique(data$response_var))
      message(
        "No response_order specified. Using: ",
        paste(response_order, collapse = ", ")
      )
    }
  }

  # Set response as ordered factor
  data$response_var <- factor(
    data$response_var,
    levels = response_order,
    ordered = TRUE
  )

  # Determine center value
  n_levels <- length(response_order)
  if (is.null(center_value)) {
    if (n_levels %% 2 == 1) {
      center_idx <- ceiling(n_levels / 2)
      center_value <- response_order[center_idx]
      message("Auto-detected center value: ", center_value)
    } else {
      center_value <- NA # No center for even number of levels
    }
  }

  # Determine which responses are negative, neutral, and positive
  has_center <- !is.na(center_value) && center_value %in% response_order

  if (has_center) {
    center_idx <- which(response_order == center_value)
    negative_levels <- response_order[1:(center_idx - 1)]
    positive_levels <- response_order[(center_idx + 1):n_levels]
  } else {
    # Even number of levels - split in half
    center_idx <- n_levels / 2
    negative_levels <- response_order[1:center_idx]
    positive_levels <- response_order[(center_idx + 1):n_levels]
  }

  # Calculate percentages for each item-response combination
  plot_data <- data |>
    group_by(item_var, response_var, .drop = FALSE) |>
    summarise(count = n(), .groups = "drop_last") |>
    mutate(
      total = sum(count),
      percentage = count / total * 100
    ) |>
    ungroup()

  # Convert item to factor for consistent ordering
  plot_data <- plot_data |>
    mutate(item_var = factor(item_var))

  # Apply custom item labels if provided
  if (!is.null(item_labels)) {
    plot_data <- plot_data |>
      mutate(item_var = recode(item_var, !!!item_labels))
  }

  # Classify each response as negative, center, or positive
  plot_data <- plot_data |>
    mutate(
      response_type = case_when(
        response_var %in% negative_levels ~ "negative",
        response_var %in% positive_levels ~ "positive",
        TRUE ~ "center"
      )
    )

  # Calculate positions for diverging layout
  # Key insight: negative responses go LEFT of center, positive go RIGHT
  # Neutral sits AT the center, split 50/50 across zero

  # For negative responses: stack from the center going left
  # Order: most extreme negative furthest left
  negative_data <- plot_data |>
    filter(response_type == "negative") |>
    group_by(item_var) |>
    arrange(item_var, response_var) |> # Least negative closest to center
    mutate(
      cum_pct = cumsum(percentage),
      # If there's a center, start from -center_pct/2, otherwise start from 0
      offset = if (has_center && isFALSE(center_separate)) {
        first(plot_data$percentage[
          plot_data$response_type == "center" &
            plot_data$item_var == first(item_var)
        ])
      } else {
        0
      }
    ) |>
    ungroup() |>
    mutate(
      x_start = -(cum_pct + offset / 2),
      x_end = -(cum_pct - percentage + offset / 2)
    ) |>
    select(-offset)

  # For positive responses: stack from the center going right
  positive_data <- plot_data |>
    filter(response_type == "positive") |>
    group_by(item_var) |>
    arrange(item_var, response_var) |> # Least positive closest to center
    mutate(
      cum_pct = cumsum(percentage),
      # If there's a center, start from center_pct/2, otherwise start from 0
      offset = if (has_center && isFALSE(center_separate)) {
        first(plot_data$percentage[
          plot_data$response_type == "center" &
            plot_data$item_var == first(item_var)
        ])
      } else {
        0
      }
    ) |>
    ungroup() |>
    mutate(
      x_start = cum_pct - percentage + offset / 2,
      x_end = cum_pct + offset / 2
    ) |>
    select(-offset)

  # Handle center data - keep it separate initially to control positioning
  center_data_calc <- plot_data |>
    filter(response_type == "center")

  # Combine negative and positive (center handled separately below)
  plot_data <- bind_rows(negative_data, positive_data)

  # Split data if center is separate and adjust positioning
  if (has_center && center_separate == "only") {
    # Return only neutral data
    center_data_only <- center_data_calc |>
      mutate(
        x_start = 0,
        x_end = percentage,
        facet = "Neutral"
      )

    plot_data_final <- center_data_only
  } else if (has_center && center_separate == "remove") {
    # Exclude center entirely - recalculate positions without offset
    negative_data_no_center <- plot_data |>
      filter(response_type == "negative") |>
      group_by(item_var) |>
      arrange(item_var, response_var) |>
      mutate(
        cum_pct = cumsum(percentage),
        x_start = -cum_pct,
        x_end = -(cum_pct - percentage)
      ) |>
      ungroup()

    positive_data_no_center <- plot_data |>
      filter(response_type == "positive") |>
      group_by(item_var) |>
      arrange(item_var, response_var) |>
      mutate(
        cum_pct = cumsum(percentage),
        x_start = cum_pct - percentage,
        x_end = cum_pct
      ) |>
      ungroup()

    plot_data_final <- bind_rows(
      negative_data_no_center,
      positive_data_no_center
    ) |>
      mutate(facet = "Response Distribution")
  } else if (has_center && isTRUE(center_separate)) {
    # Center goes in separate facet, starting at 0
    center_data_separate <- center_data_calc |>
      mutate(
        x_start = 0,
        x_end = percentage,
        facet = "Neutral"
      )

    main_data <- plot_data |>
      mutate(facet = "Response Distribution")

    plot_data_final <- bind_rows(main_data, center_data_separate)
  } else {
    # Center stays in main chart, centered around 0
    if (nrow(center_data_calc) > 0) {
      center_data_main <- center_data_calc |>
        mutate(
          x_start = -percentage / 2,
          x_end = percentage / 2
        )

      plot_data <- bind_rows(plot_data, center_data_main)
    }

    plot_data_final <- plot_data |>
      mutate(facet = "Response Distribution")
  }

  # Ensure item_var is a factor for numeric conversion
  plot_data_final <- plot_data_final |>
    mutate(
      item_var = factor(item_var, levels = rev(levels(factor(item_var))))
    )

  # Get unique items for y-axis
  item_levels <- levels(plot_data_final$item_var)

  # Set up color palette
  # Get colors for the full scale
  if (n_levels <= 11) {
    colors_raw <- brewer.pal(max(3, n_levels), palette)
  } else {
    colors_raw <- colorRampPalette(brewer.pal(11, palette))(n_levels)
  }

  if (reverse_palette) {
    colors_raw <- rev(colors_raw)
  }

  # Create named vector matching response_order
  color_map <- setNames(colors_raw, response_order)

  # Override center color if specified or to ensure visibility
  if (has_center && center_separate != "remove") {
    if (!is.null(neutral_color)) {
      # Use user-specified color
      color_map[center_value] <- neutral_color
    } else if (center_separate == TRUE || center_separate == "only") {
      # Use medium gray for separate facet
      color_map[center_value] <- "#999999"
    } else {
      # Use a visible neutral color (medium gray)
      color_map[center_value] <- "#8C8C8C"
    }
  }

  # Create the plot using geom_rect for precise positioning
  p <- ggplot(
    plot_data_final,
    aes(
      xmin = x_start,
      xmax = x_end,
      ymin = as.numeric(item_var) - 0.35,
      ymax = as.numeric(item_var) + 0.35,
      fill = response_var
    )
  ) +
    geom_rect() +
    scale_fill_manual(
      values = color_map,
      name = "Response",
      breaks = response_order,
      drop = FALSE
    ) +
    scale_y_continuous(
      breaks = seq_along(item_levels),
      labels = item_levels
    ) +
    labs(
      title = title,
      subtitle = subtitle,
      x = "Percentage",
      y = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.y = element_text(hjust = 1)
    )

  # Add faceting if center is separate
  if (
    has_center &&
      (isTRUE(center_separate) || identical(center_separate, "only"))
  ) {
    p <- p +
      facet_wrap(~facet, scales = "free_x") +
      theme(strip.text = element_text(face = "bold"))
  }

  # Add percentage labels if requested
  if (show_percentages) {
    label_data <- plot_data_final |>
      filter(percentage > 3) |> # Only show labels for segments > 3%
      mutate(
        label_pos = (x_start + x_end) / 2,
        y_pos = as.numeric(factor(item_var))
      )

    p <- p +
      geom_text(
        data = label_data,
        aes(
          x = label_pos,
          y = y_pos,
          label = paste0(round(percentage, 0), "%")
        ),
        size = percentage_size,
        color = "white",
        fontface = "bold",
        inherit.aes = FALSE
      )
  }

  # Set axis limits and labels
  if (center_separate == "only") {
    # For neutral only, simple 0 to 100 scale
    p <- p +
      scale_x_continuous(
        labels = function(x) paste0(x, "%")
      )
  } else if (
    !has_center || center_separate == "remove" || isTRUE(center_separate)
  ) {
    max_val <- max(abs(c(plot_data_final$x_start, plot_data_final$x_end)))
    p <- p +
      scale_x_continuous(
        limits = c(-max_val, max_val),
        labels = function(x) paste0(abs(x), "%"),
        breaks = seq(-100, 100, 25)
      )
  } else {
    p <- p +
      scale_x_continuous(
        labels = function(x) paste0(abs(x), "%")
      )
  }

  return(p)
}

# Example usage with default data
# plot_likert_diverging()
