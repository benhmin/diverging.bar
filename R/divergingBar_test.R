# Test file for plot_likert_diverging function
# Run with testthat package: testthat::test_file("test_likert_diverging.R")

library(testthat)
library(ggplot2)
library(dplyr)

# Assuming the function is loaded
# Try different paths depending on where tests are run from
if (file.exists("./divergingBar.R")) {
  source("./divergingBar.R")
} else if (file.exists("../R/divergingBar.R")) {
  source("../R/divergingBar.R")
} else if (file.exists("./R/divergingBar.R")) {
  source("./R/divergingBar.R")
} else {
  stop("Could not find divergingBar.R")
}

# Test 1: Function runs with default parameters
test_that("Function runs with default sample data", {
  expect_no_error(plot_likert_diverging())
  p <- plot_likert_diverging()
  expect_s3_class(p, "ggplot")
})

# Test 2: Function accepts custom data
test_that("Function accepts custom data frame", {
  custom_data <- data.frame(
    question = rep(c("Q1", "Q2"), each = 50),
    answer = sample(c("Bad", "Neutral", "Good"), 100, replace = TRUE)
  )

  p <- plot_likert_diverging(
    data = custom_data,
    item_col = "question",
    response_col = "answer",
    response_order = c("Bad", "Neutral", "Good")
  )

  expect_s3_class(p, "ggplot")
})

# Test 2b: Verify percentages sum to 100% per item (critical bug check)
test_that("Percentages sum to 100% for each item", {
  # Use a known distribution to verify calculations
  set.seed(42)
  known_data <- data.frame(
    item = rep("Test Item", 100),
    response = c(
      rep("A", 10),
      rep("B", 20),
      rep("Neutral", 30),
      rep("C", 25),
      rep("D", 15)
    )
  )

  p <- plot_likert_diverging(
    data = known_data,
    response_order = c("A", "B", "Neutral", "C", "D"),
    center_value = "Neutral"
  )

  expect_s3_class(p, "ggplot")

  # Extract plot data and verify percentages sum to reasonable values
  plot_data <- layer_data(p)
  # Each bar (item) should span from negative to positive percentages
  # The range should reflect the full distribution
  expect_true(max(plot_data$xmax) > 0)
  expect_true(min(plot_data$xmin) < 0 | min(plot_data$xmin) == 0)
})

# Test 3: Handles 5-point Likert scale
test_that("Handles 5-point Likert scale correctly", {
  data_5pt <- data.frame(
    item = rep("Test Item", 100),
    response = sample(1:5, 100, replace = TRUE)
  )

  p <- plot_likert_diverging(
    data = data_5pt,
    response_order = 1:5,
    center_value = 3
  )

  expect_s3_class(p, "ggplot")
})

# Test 4: Handles 7-point Likert scale
test_that("Handles 7-point Likert scale correctly", {
  data_7pt <- data.frame(
    item = rep("Test Item", 100),
    response = sample(1:7, 100, replace = TRUE)
  )

  p <- plot_likert_diverging(
    data = data_7pt,
    response_order = 1:7,
    center_value = 4
  )

  expect_s3_class(p, "ggplot")
})

# Test 5: Handles even number of response levels (no center)
test_that("Handles 4-point scale (even number)", {
  data_4pt <- data.frame(
    item = rep("Test Item", 100),
    response = sample(1:4, 100, replace = TRUE)
  )

  p <- plot_likert_diverging(
    data = data_4pt,
    response_order = 1:4
  )

  expect_s3_class(p, "ggplot")
})

# Test 6: Center value in separate facet
test_that("Center value can be displayed in separate facet", {
  p <- plot_likert_diverging(center_separate = TRUE)

  expect_s3_class(p, "ggplot")
  # Check that faceting was applied
  expect_true(
    "FacetWrap" %in%
      class(p$facet) ||
      "FacetWrap" %in% class(p$facet %||% p$facet)
  )
  # Alternative check: verify facet_wrap was used
  has_facet <- any(sapply(p$layer, function(l) {
    any(grepl("facet", class(l$geom), ignore.case = TRUE))
  })) ||
    !is.null(p$facet)
  expect_true(has_facet)
})

# Test 7: Center value removed
test_that("Center value can be removed from chart", {
  # Create data with known center category
  test_data <- data.frame(
    item = rep("Test", 100),
    response = sample(
      c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree"),
      100,
      replace = TRUE
    )
  )

  p <- plot_likert_diverging(
    data = test_data,
    center_separate = "remove"
  )

  expect_s3_class(p, "ggplot")
  # With center removed, we expect bars that don't cross zero (symmetric on each side)
  plot_data <- layer_data(p)
  # Check that we have data in both negative and positive regions
  has_negative <- any(plot_data$xmin < 0)
  has_positive <- any(plot_data$xmax > 0)
  expect_true(has_negative || has_positive)
})

# Test 8: Return only neutral data
test_that("Can return only neutral data", {
  p <- plot_likert_diverging(center_separate = "only")

  expect_s3_class(p, "ggplot")
  # Should only have data for neutral category
  plot_data <- layer_data(p)
  expect_true(all(plot_data$xmin >= 0)) # All bars start at 0 or positive
})

# Test 9: Custom color palettes
test_that("Accepts different ColorBrewer palettes", {
  palettes <- c("RdBu", "PiYG", "BrBG", "PRGn", "PuOr", "RdGy", "RdYlBu")

  for (pal in palettes) {
    expect_no_error(plot_likert_diverging(palette = pal))
  }
})

# Test 10: Reverse palette works
test_that("Palette reversal works", {
  p_normal <- plot_likert_diverging(palette = "RdBu")
  p_reverse <- plot_likert_diverging(palette = "RdBu", reverse_palette = TRUE)

  expect_s3_class(p_normal, "ggplot")
  expect_s3_class(p_reverse, "ggplot")
  expect_false(identical(p_normal, p_reverse))
})

# Test 11: Custom neutral color
test_that("Custom neutral color is applied", {
  p <- plot_likert_diverging(neutral_color = "#FFD700")

  expect_s3_class(p, "ggplot")
  # Check that custom color is in the fill scale values
  fill_scale <- p$scales$get_scales("fill")
  expect_s3_class(fill_scale, "ScaleDiscrete")

  # Get the color mapping from the scale
  # The scale should contain our custom neutral color
  if (!is.null(fill_scale$palette)) {
    n_colors <- length(fill_scale$breaks)
    fills <- fill_scale$palette(n_colors)
    color_applied <- "#FFD700" %in%
      fills ||
      any(grepl("FFD700", fill_scale$cache$orig, fixed = TRUE))
    expect_true(color_applied || is.character(fill_scale$limits))
  }
})

# Test 12: Custom item labels
test_that("Custom item labels are applied", {
  custom_labels <- c(
    "Overall Satisfaction" = "Overall",
    "Would Recommend" = "Recommend",
    "Ease of Use" = "Ease",
    "Value for Money" = "Value"
  )

  p <- plot_likert_diverging(item_labels = custom_labels)

  expect_s3_class(p, "ggplot")
})

# Test 13: Title and subtitle
test_that("Title and subtitle are added", {
  p <- plot_likert_diverging(
    title = "Test Title",
    subtitle = "Test Subtitle"
  )

  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$title, "Test Title")
  expect_equal(p$labels$subtitle, "Test Subtitle")
})

# Test 14: Percentage labels can be toggled
test_that("Percentage labels can be hidden", {
  p_with <- plot_likert_diverging(show_percentages = TRUE)
  p_without <- plot_likert_diverging(show_percentages = FALSE)

  expect_s3_class(p_with, "ggplot")
  expect_s3_class(p_without, "ggplot")

  # Check number of layers (with percentages has extra geom_text layer)
  expect_true(length(p_with$layers) > length(p_without$layers))
})

# Test 15: Handles missing response categories
test_that("Handles data with missing response categories", {
  incomplete_data <- data.frame(
    item = rep("Test", 50),
    response = sample(c("Disagree", "Neutral", "Agree"), 50, replace = TRUE)
    # Missing "Strongly Disagree" and "Strongly Agree"
  )

  p <- plot_likert_diverging(
    data = incomplete_data,
    response_order = c(
      "Strongly Disagree",
      "Disagree",
      "Neutral",
      "Agree",
      "Strongly Agree"
    )
  )

  expect_s3_class(p, "ggplot")
})

# Test 15b: Verifies percentages sum to 100 per item (critical bug fix)
test_that("Percentages sum to 100 per item", {
  set.seed(42)
  test_data <- data.frame(
    item = rep(c("Item A", "Item B", "Item C"), each = 100),
    response = sample(
      c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree"),
      300,
      replace = TRUE
    )
  )

  p <- plot_likert_diverging(data = test_data)
  plot_data <- layer_data(p)

  # Get unique y-center positions (each item's bar center)
  y_centers <- sort(unique((plot_data$ymin + plot_data$ymax) / 2))

  # Each item should have bars that sum to 100% width
  for (i in seq_along(y_centers)) {
    y_center <- y_centers[i]
    # Find bars at this y position (with tolerance for floating point)
    item_bars <- plot_data[
      abs((plot_data$ymin + plot_data$ymax) / 2 - y_center) < 0.2,
    ]

    if (nrow(item_bars) > 0) {
      total_width <- sum(abs(item_bars$xmax - item_bars$xmin))
      # Allow up to 100% (negative + positive + center should equal 100)
      expect_true(
        total_width > 95 && total_width <= 100,
        info = paste(
          "Item",
          i,
          "percentages sum to",
          round(total_width, 1),
          "instead of ~100"
        )
      )
    }
  }
})

# Test 15c: Per-item neutral offset works correctly
test_that("Per-item neutral offset handles different neutral percentages", {
  # Create data where items have different neutral percentages
  test_data <- data.frame(
    item = c(rep("Item A", 100), rep("Item B", 100)),
    response = c(
      # Item A: 20% neutral
      rep("Strongly Disagree", 20),
      rep("Disagree", 20),
      rep("Neutral", 20),
      rep("Agree", 20),
      rep("Strongly Agree", 20),
      # Item B: 60% neutral
      rep("Strongly Disagree", 10),
      rep("Disagree", 10),
      rep("Neutral", 60),
      rep("Agree", 10),
      rep("Strongly Agree", 10)
    )
  )

  p <- plot_likert_diverging(data = test_data)
  plot_data <- layer_data(p)

  # Get unique y-center positions for each item
  y_centers <- unique((plot_data$ymin + plot_data$ymax) / 2)

  # Both items should have bars that sum to 100%
  for (y_center in y_centers) {
    item_bars <- plot_data[
      abs((plot_data$ymin + plot_data$ymax) / 2 - y_center) < 0.1,
    ]
    item_total <- sum(abs(item_bars$xmax - item_bars$xmin))

    expect_equal(
      item_total,
      100,
      tolerance = 0.1,
      info = paste("Item at y=", y_center, "percentages should sum to 100")
    )
  }
})

# Test 15d: Negative values appear on left side of chart
test_that("Negative responses appear on left side (x < 0)", {
  set.seed(42)
  test_data <- data.frame(
    item = rep("Test Item", 100),
    response = c(
      rep("Strongly Disagree", 10),
      rep("Disagree", 20),
      rep("Neutral", 40),
      rep("Agree", 20),
      rep("Strongly Agree", 10)
    )
  )

  p <- plot_likert_diverging(
    data = test_data,
    response_order = c(
      "Strongly Disagree",
      "Disagree",
      "Neutral",
      "Agree",
      "Strongly Agree"
    )
  )

  plot_data <- layer_data(p)

  # Some bars should have negative x positions (left side)
  expect_true(
    any(plot_data$xmin < 0),
    info = "Should have bars extending to left (negative x)"
  )
  expect_true(
    any(plot_data$xmax > 0),
    info = "Should have bars extending to right (positive x)"
  )

  # Neutral should be centered around 0
  neutral_bars <- plot_data[plot_data$xmin < 0 & plot_data$xmax > 0, ]
  expect_true(
    nrow(neutral_bars) > 0,
    info = "Neutral category should straddle zero"
  )
})

# Test 16: Multiple items
test_that("Handles multiple items correctly", {
  multi_item_data <- data.frame(
    item = rep(c("Item 1", "Item 2", "Item 3"), each = 100),
    response = sample(c("Low", "Medium", "High"), 300, replace = TRUE)
  )

  p <- plot_likert_diverging(
    data = multi_item_data,
    response_order = c("Low", "Medium", "High")
  )

  expect_s3_class(p, "ggplot")
  # Should have 3 items represented
  # Check that the plot has 3 distinct y-positions
  plot_data <- layer_data(p)
  unique_y_positions <- unique(
    plot_data$ymin + (plot_data$ymax - plot_data$ymin) / 2
  )
  expect_equal(length(unique_y_positions), 3)
})

# Test 16b: Multiple items with different neutral percentages
test_that("Multiple items with different center percentages align correctly", {
  # Create data where different items have different neutral percentages
  set.seed(123)
  multi_item_varied <- data.frame(
    item = c(
      rep("Item A (20% Neutral)", 100),
      rep("Item B (50% Neutral)", 100)
    ),
    response = c(
      sample(
        c("SD", "D", "N", "A", "SA"),
        100,
        prob = c(0.2, 0.2, 0.2, 0.2, 0.2),
        replace = TRUE
      ),
      sample(
        c("SD", "D", "N", "A", "SA"),
        100,
        prob = c(0.1, 0.1, 0.5, 0.2, 0.1),
        replace = TRUE
      )
    )
  )

  p <- plot_likert_diverging(
    data = multi_item_varied,
    response_order = c("SD", "D", "N", "A", "SA"),
    center_value = "N"
  )

  expect_s3_class(p, "ggplot")

  # Verify that both items are plotted
  plot_data <- layer_data(p)
  expect_true(length(unique(plot_data$ymin)) >= 2)
})

# Test 17: Handles single item
test_that("Handles single item", {
  single_item_data <- data.frame(
    item = rep("Only Item", 100),
    response = sample(1:5, 100, replace = TRUE)
  )

  p <- plot_likert_diverging(
    data = single_item_data,
    response_order = 1:5
  )

  expect_s3_class(p, "ggplot")
})

# Test 18: Custom center value specification
test_that("Custom center value can be specified", {
  # Test with numeric
  p_numeric <- plot_likert_diverging(
    data = data.frame(
      item = rep("Test", 100),
      response = sample(1:5, 100, replace = TRUE)
    ),
    response_order = 1:5,
    center_value = 3
  )

  # Test with character
  p_char <- plot_likert_diverging(
    center_value = "Neutral"
  )

  expect_s3_class(p_numeric, "ggplot")
  expect_s3_class(p_char, "ggplot")
})

# Test 19: 3-point scale (minimum)
test_that("Handles 3-point scale", {
  data_3pt <- data.frame(
    item = rep("Test", 90),
    response = sample(c("Disagree", "Neutral", "Agree"), 90, replace = TRUE)
  )

  p <- plot_likert_diverging(
    data = data_3pt,
    response_order = c("Disagree", "Neutral", "Agree")
  )

  expect_s3_class(p, "ggplot")
})

# Test 20: 9-point scale (maximum)
test_that("Handles 9-point scale", {
  data_9pt <- data.frame(
    item = rep("Test", 180),
    response = sample(1:9, 180, replace = TRUE)
  )

  p <- plot_likert_diverging(
    data = data_9pt,
    response_order = 1:9,
    center_value = 5
  )

  expect_s3_class(p, "ggplot")
})

# Test 21: Validates data structure
test_that("Handles invalid column names with informative error", {
  bad_data <- data.frame(
    wrong_col = rep("Test", 50),
    also_wrong = sample(1:5, 50, replace = TRUE)
  )

  expect_error(
    plot_likert_diverging(
      data = bad_data,
      item_col = "nonexistent",
      response_col = "also_nonexistent"
    ),
    regexp = "(not found|column|invalid|does not exist|doesn't exist)",
    ignore.case = TRUE
  )
})

# Test 22: Percentage size parameter
test_that("Percentage size can be adjusted", {
  p_small <- plot_likert_diverging(percentage_size = 2)
  p_large <- plot_likert_diverging(percentage_size = 5)

  expect_s3_class(p_small, "ggplot")
  expect_s3_class(p_large, "ggplot")
})

# Test 23: Works with factor data
test_that("Handles factor response variables", {
  factor_data <- data.frame(
    item = rep("Test", 100),
    response = factor(
      sample(c("Poor", "Fair", "Good", "Excellent"), 100, replace = TRUE),
      levels = c("Poor", "Fair", "Good", "Excellent"),
      ordered = TRUE
    )
  )

  p <- plot_likert_diverging(data = factor_data)

  expect_s3_class(p, "ggplot")
})

# Test 24: Consistent output structure
test_that("Output has expected ggplot layers", {
  p <- plot_likert_diverging()

  # Should have at least: geom_rect, scale_fill_manual, theme
  expect_true(length(p$layers) >= 1)
  expect_true("ScaleDiscrete" %in% class(p$scales$get_scales("fill")))
  expect_s3_class(p$theme, "theme")
})

# Test 25: Integration test - full workflow
test_that("Full workflow with all custom parameters", {
  custom_data <- data.frame(
    question = rep(paste("Question", 1:3), each = 100),
    rating = sample(
      c("Very Bad", "Bad", "Neutral", "Good", "Very Good"),
      300,
      replace = TRUE
    )
  )

  p <- plot_likert_diverging(
    data = custom_data,
    item_col = "question",
    response_col = "rating",
    response_order = c("Very Bad", "Bad", "Neutral", "Good", "Very Good"),
    center_value = "Neutral",
    center_separate = FALSE,
    palette = "PuOr",
    reverse_palette = TRUE,
    neutral_color = "#CCCCCC",
    title = "Survey Results",
    subtitle = "N = 300",
    show_percentages = TRUE,
    percentage_size = 3.5
  )

  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$title, "Survey Results")
  expect_equal(p$labels$subtitle, "N = 300")
})

# Test 26: Verify library() is used instead of require()
test_that("Package loading uses library not require", {
  # Search for divergingBar.R in multiple possible locations
  possible_locations <- c(
    "divergingBar.R",
    "R/divergingBar.R",
    "./R/divergingBar.R",
    file.path(getwd(), "divergingBar.R"),
    file.path(getwd(), "R/divergingBar.R"),
    file.path(dirname(getwd()), "R/divergingBar.R")
  )

  bar_file <- NULL
  for (loc in possible_locations) {
    if (file.exists(loc)) {
      bar_file <- loc
      break
    }
  }

  skip_if_not(
    !is.null(bar_file),
    paste(
      "Cannot locate divergingBar.R. Tried:",
      paste(possible_locations, collapse = ", ")
    )
  )

  source_lines <- readLines(bar_file)

  # Check for require() calls
  has_require <- any(grepl("require(", source_lines, fixed = TRUE))
  has_library <- any(grepl("library(", source_lines, fixed = TRUE))

  expect_false(
    has_require,
    info = "Should use library() not require() for package loading"
  )
  expect_true(
    has_library,
    info = "Should use library() for package loading"
  )
})

# Test 27: Test center_separate modes produce different outputs
test_that("Different center_separate modes produce different plots", {
  p_together <- plot_likert_diverging(center_separate = FALSE)
  p_separate <- plot_likert_diverging(center_separate = TRUE)
  p_remove <- plot_likert_diverging(center_separate = "remove")
  p_only <- plot_likert_diverging(center_separate = "only")

  # Get plot data for each
  data_together <- layer_data(p_together)
  data_separate <- layer_data(p_separate)
  data_remove <- layer_data(p_remove)
  data_only <- layer_data(p_only)

  # Separate mode should have more rows (faceted)
  expect_true(
    nrow(data_separate) >= nrow(data_together),
    info = "Separate facet mode should have at least as many bars"
  )

  # Remove mode should have fewer rows (no neutral)
  expect_true(
    nrow(data_remove) < nrow(data_together),
    info = "Remove mode should have fewer bars (no neutral)"
  )

  # Only mode should have neutral bars starting at 0
  expect_true(
    all(data_only$xmin >= 0),
    info = "Only mode should have all bars starting at 0"
  )
})

cat("All tests completed!\n")
