vijScale = function(pal, type = "fill") {
    themePal <- get('theme', envir = parent.frame())$palette
    palette <- vijPalette(pal, type, themePal)
    return(discrete_scale(aesthetics = type, palette = palette, na.value="gray"))
}

vijPalette = function(pal, type = "fill", themePal = NULL) {
    palType <- strsplit(pal, "::")[[1]][1]
    palName <- strsplit(pal, "::")[[1]][2]
    if (is.na(palName)) {
        palName <- palType
        palType <- "brewer"
    }
    if (is.null(themePal))
        themePal <- get('theme', envir = parent.frame())$palette
    if (palName == "jmv") {
        jmvPalette <- function(n) jmvcore::colorPalette(n, pal = themePal, type = type)
        return(jmvPalette)
    } else if (palType == "brewer") {
        return(scales::pal_brewer(palette = palName))
    } else if (palType == "viridis") {
        return(scales::pal_viridis(option = palName))
    } else if (palType == "dichromat") {
        return(scales::pal_dichromat(palName))
    }
}



vijTitlesAndLabels = function(options, defaults = list(), plotType = '') {
    horizontal <- options[["horizontal"]]  %||% FALSE
    # Title & Subtitle
    if (plotType == '') {
        title <- options$titleText %||% ''
        subtitle <- options$subtitleText %||% ''
        caption <- options$captionText %||% ''
    } else {
        title <- options[[paste0(plotType,"TitleText")]] %||% ''
        subtitle <- options[[paste0(plotType,"SubtitleText")]] %||% ''
        caption <- options[[paste0(plotType,"CaptionText")]] %||% ''
    }
    # Title
    if (title == "")
        title <- NULL
    else if (title == "default")
        title <- defaults$title
    # Subtitle
    if (subtitle == "")
        subtitle <- NULL
    else if (subtitle == "default")
        subtitle <- defaults$subtitle
    # Caption
    if (caption == "")
        caption <- NULL
    else if (caption == "default")
        caption <- defaults$caption
    # Legend
    legend <- options[["legendText"]] %||% ''
    if (legend == "")
        legend <- defaults$legend
    # xAxis
    if (horizontal)
        x <- options[["yAxisText"]] %||% ''
    else
        x <- options[["xAxisText"]] %||% ''
    if (x == "")
        x <- defaults$x
    # yAxis
    y <- options[["yAxisText"]] %||% ''
    if (horizontal)
        y <- options[["xAxisText"]] %||% ''
    else
        y <- options[["yAxisText"]] %||% ''
    if (y == "")
        y <- defaults$y

    return(ggplot2::labs(title = title, subtitle = subtitle, caption = caption, fill = legend, color = legend, shape = legend, size = legend, x = x, y = y))
}

# vijAxes = function(options) {
#     #xLabelFontSize <- options$xLabelFontSize
#     #xLabelAlign <- options$xLabelAlign
#     #yLabelFontSize <- options$yLabelFontSize
#     #yLabelAlign <- options$yLabelAlign
#     xAxisLabelFontSize <- options$xAxisLabelFontSize
#     xAxisLabelRotation <- options$xAxisLabelRotation
#     yAxisLabelFontSize <- options$yAxisLabelFontSize
#     yAxisLabelRotation <- options$yAxisLabelRotation
#     return(ggplot2::theme(
#         axis.text.x = ggplot2::element_text(
#                 size = xAxisLabelFontSize,
#                 angle = xAxisLabelRotation
#             ),
#             axis.text.y = ggplot2::element_text(
#                 size = yAxisLabelFontSize,
#                 angle = yAxisLabelRotation)
#         )
#     )
# }

vijTitleAndLabelFormat = function(options, showLegend = TRUE) {
    horizontal <- options[["horizontal"]]  %||% FALSE
    if (showLegend) {
        legendPosition  <- options$legendPosition
        legendFontSize <- as.integer(options$legendFontSize)
    } else {
        legendPosition <- "none"
        legendFontSize <- 14
    }
    xAxisLabelFontSize <- options[["xAxisLabelFontSize"]] %||% 12
    xAxisLabelRotation <- options[["xAxisLabelRotation"]] %||% 0
    yAxisLabelFontSize <- options[["yAxisLabelFontSize"]] %||% 12
    yAxisLabelRotation <- options[["yAxisLabelRotation"]] %||% 0
    return(ggplot2::theme(
        # Title, subtitle and caption
        plot.title = element_text(
            size = options$titleFontSize,
            face = options$titleFontFace,
            hjust = as.numeric(options$titleAlign)),
        plot.subtitle = element_text(
            size = options$subtitleFontSize,
            face = options$subtitleFontFace,
            hjust = as.numeric(options$subtitleAlign),
            margin = margin(-5, 0, 15, 0)),
        plot.caption = element_text(
            size = options$captionFontSize,
            face = options$captionFontFace,
            hjust = as.numeric(options$captionAlign)),
        # Legend
        legend.title=element_text(
            size = (legendFontSize + 1)),
        legend.text=element_text(
            size = legendFontSize),
        legend.position = legendPosition,
        # Axis Titles
        axis.title.x = element_text(
            size = options[["xAxisFontSize"]] %||% 14,
            hjust = as.numeric(options[["xAxisPosition"]] %||% 0)
        ),
        axis.title.y = element_text(
            size = options[["yAxisFontSize"]] %||% 14,
            hjust = as.numeric(options[["yAxisPosition"]] %||% 0)
        ),
        # Axis Labels
        axis.text.x = ggplot2::element_text(
            size = xAxisLabelFontSize,
            angle = xAxisLabelRotation
        ),
        axis.text.y = ggplot2::element_text(
            size = yAxisLabelFontSize,
            angle = yAxisLabelRotation)
    ))
}
