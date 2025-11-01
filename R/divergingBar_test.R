# Test file for plot_likert_diverging function
# Run with testthat package: testthat::test_file("test_likert_diverging.R")

library(testthat)
library(ggplot2)
library(dplyr)

# Assuming the function is loaded
# source("path/to/plot_likert_diverging.R")

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
    expect_true("FacetWrap" %in% class(p$facet))
})

# Test 7: Center value removed
test_that("Center value can be removed from chart", {
    p <- plot_likert_diverging(center_separate = "remove")

    expect_s3_class(p, "ggplot")
    # Verify only 4 response categories in the plot data
    plot_data <- layer_data(p)
    expect_true(length(unique(plot_data$fill)) <= 4)
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
    n_colors <- length(fill_scale$breaks)
    fills <- fill_scale$palette(n_colors)
    expect_true("#FFD700" %in% fills)
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
    # Should have 3 bars (one for each item)
    plot_data <- layer_data(p)
    expect_equal(length(unique(plot_data$ymin)), 3)
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
test_that("Handles invalid column names gracefully", {
    bad_data <- data.frame(
        wrong_col = rep("Test", 50),
        also_wrong = sample(1:5, 50, replace = TRUE)
    )

    expect_error(
        plot_likert_diverging(
            data = bad_data,
            item_col = "nonexistent",
            response_col = "also_nonexistent"
        )
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

cat("All tests completed!\n")
