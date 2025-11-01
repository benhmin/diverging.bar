context("plot_likert_diverging")

test_that("default call returns ggplot object", {
    p <- plot_likert_diverging()
    expect_s3_class(p, "ggplot")
})

test_that("center_separate = TRUE returns ggplot object", {
    p <- plot_likert_diverging(center_separate = TRUE)
    expect_s3_class(p, "ggplot")
})

test_that("center_separate = 'remove' returns ggplot object", {
    p <- plot_likert_diverging(center_separate = "remove")
    expect_s3_class(p, "ggplot")
})

test_that("center_separate = 'only' returns ggplot object", {
    p <- plot_likert_diverging(center_separate = "only")
    expect_s3_class(p, "ggplot")
})

test_that("custom palette works", {
    p <- plot_likert_diverging(palette = "PuOr", reverse_palette = TRUE)
    expect_s3_class(p, "ggplot")
})
