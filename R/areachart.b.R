
# This file is a generated template, your changes will not be overwritten

areachartClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "areachartClass",
    inherit = areachartBase,
    private = list(
        .init = function() {
            # Set the size of the plot
            userWidth <- as.numeric(self$options$plotWidth)
            userHeight <- as.numeric(self$options$plotHeight)
            # Check min size
            if ((userWidth != 0 && userWidth < 200) || (userHeight != 0 && userHeight < 200))
                jmvcore::reject(.("Plot size must be at least 200px (or 0 = default)"))

            width <- 600
            height <- 400
            # Compute the size according to options
            if (userWidth * userHeight == 0) {
                if (!is.null(self$options$group) || length(self$options$vars) > 1) {
                    if (self$options$legendPosition %in% c('top','bottom'))
                        height <- height + 50
                    else
                        width <- width + 50
                }
            }
            if (userWidth >0)
                width = userWidth
            if (userHeight >0)
                height = userHeight
            image <- self$results$plot
            image$setSize(width, height)
        },
        .run = function() {
            oneVariable <- (self$options$mode == "oneVariable")
            if (oneVariable) {
                timeVar <- self$options$timeVar
                depVars <- self$options$var
                groupVar <- self$options$group
                varNames <- c(timeVar, depVars, groupVar)
            } else {
                timeVar <- self$options$timeVar1
                depVars <- self$options$vars
                varNames <- c(timeVar, depVars)
            }
            if (length(depVars) == 0 || is.null(timeVar))
                return()
            data <- jmvcore::select(self$data, varNames)
            # Delete row with missing time
            data <- subset(data, !is.na(data[timeVar]))
            # Be sure dep var are numeric
            for (aVar in depVars)
                data[[aVar]] <- jmvcore::toNumeric(data[[aVar]])
            image <- self$results$plot
            image$setState(data)
        },
        .plot = function(image, ggtheme, theme, ...) {
            plotData <- image$state
            oneVariable <- (self$options$mode == "oneVariable")
            if (oneVariable) {
                timeVar <- self$options$timeVar
                depVar <- self$options$var
                groupVar <- self$options$group
            } else {
                timeVar <- self$options$timeVar1
                if (length(self$options$vars) > 0 && !is.null(timeVar)) {
                    plotData <- plotData %>%
                                    tidyr::gather(key = "Variables", value = "Values", -timeVar)
                    # Transform "Variables" as factor to keep the variable order.
                    plotData$Variables <- factor(plotData$Variables, levels = self$options$vars)
                    depVar <- "Values"
                    groupVar <- "Variables"
                } else {
                    depVar <- NULL
                }
            }

            if (is.null(depVar) || is.null(timeVar))
                return(FALSE)

            timeVar <- ensym(timeVar)
            depVar <- ensym(depVar)
            if (!is.null(groupVar))
                groupVar <- ensym(groupVar)

            # Time format
            timeVarIsDate <- self$options$isDate
            if (timeVarIsDate) {
                if (self$options$dateFormat == "auto") {
                    timeVarAsDate <- private$.convertToDate(plotData[[timeVar]], "iso")
                    if (is.null(timeVarAsDate)) {
                        timeVarAsDate <- private$.convertToDate(plotData[[timeVar]], "us")
                        if (is.null(timeVarAsDate)) {
                            timeVarAsDate <- private$.convertToDate(plotData[[timeVar]], "eu")
                        }
                    }
                } else {
                    timeVarAsDate <- private$.convertToDate(plotData[[timeVar]], self$options$dateFormat)
                }

                if (!is.null(timeVarAsDate)) {
                    plotData[[timeVar]] <- timeVarAsDate
                } else {
                    jmvcore::reject(paste(self$options$timeVar, .("doesn't have a valid date format.")))
                    timeVarIsDate <- FALSE
                }
            }

            # Plot options (lines)
            if (self$options$showLine)
                lineWidth <- self$options$lineWidth
            else
                lineWidth <- 0

            # Plot options (position)
            if (self$options$position == "identity")
                alpha = 0.5
            else
                alpha = 1

            if (is.null(groupVar))
                plot <- ggplot(plotData, aes(x = !!timeVar, group = 1)) +
                            geom_area(aes(y = !!depVar, fill = "OneVar"),
                                      position = self$options$position,
                                      color='black', size = lineWidth,
                                      alpha = alpha, show.legend = FALSE)
            else
                plot <- ggplot(plotData, aes(x = !!timeVar, group = !!groupVar)) +
                            geom_area(aes(y = !!depVar, fill = !!groupVar),
                                      position = self$options$position,
                                      color='black', size = lineWidth,
                                      alpha = alpha)

            # Theme and colors
            plot <- plot + ggtheme + vijScale(self$options$colorPalette, "fill")

            if (timeVarIsDate) {
                plot <- plot + scale_x_date(labels = private$.myDateLabel, date_breaks = self$options$dateBreak)
            }

            if (self$options$position == "fill")
                plot <- plot + scale_y_continuous(labels = scales::label_percent())

            if (!oneVariable) {
                if(length(self$options$vars) > 1) {
                    showLegend = TRUE
                    yLab <- .("Values")
                } else {
                    showLegend = FALSE
                    yLab <- self$options$vars
                }
            } else {
                showLegend = TRUE
                yLab <- depVar
            }

            # Axis range
            if (self$options$yAxisRangeType == "manual") { # Horizontal and manual
                plot <- plot + coord_cartesian(ylim = c(self$options$yAxisRangeMin, self$options$yAxisRangeMax))
            }

            # Titles & Labels
            defaults <- list(y = yLab, x = timeVar, legend = groupVar)
            plot <- plot + vijTitlesAndLabels(self$options, defaults) + vijTitleAndLabelFormat(self$options, showLegend = showLegend)
            plot <- plot + theme(legend.key.spacing.y = unit(1, "mm"), legend.byrow = TRUE)

            # Suppress black border
            plot <- plot + guides(fill = guide_legend(override.aes = list(color = NULL)))

            return(plot)
        },
        .myDateLabel = function(dateVector) {
            mapply(private$.formatDate, dateVector)
        },
        .formatDate = function(aDate) {
            longMonths <- .("January,February,March,April,May,June,July,August,September,October,November,December")
            longMonths <- strsplit(longMonths, ",")[[1]]
            shortMonths <- .("Jan.,Feb.,Mar.,Apr.,May,June,July,Aug.,Sept.,Oct.,Nov.,Dec.")
            shortMonths <- strsplit(shortMonths, ",")[[1]]
            shortMonth <- shortMonths[as.integer(format.Date(aDate, "%m"))]
            longMonth <- longMonths[as.integer(format.Date(aDate, "%m"))]
            firstUp <- function(s){paste0(toupper(substring(s, 1, 1)), substring(s, 2))}
            if (self$options$displayFormat == "%Y %B %e")
                return(paste(format.Date(aDate, "%Y"), longMonth, trimws(format.Date(aDate, "%e"))))
            else if (self$options$displayFormat == "%e %B %Y")
                return(paste(format.Date(aDate, "%e"), longMonth, format.Date(aDate, "%Y")))
            else if (self$options$displayFormat == "%B %Y")
                return(paste(firstUp(longMonth), format.Date(aDate, "%Y")))
            else if (self$options$displayFormat == "%B %y")
                return(paste(firstUp(longMonth), format.Date(aDate, "%y")))
            else if (self$options$displayFormat == "%b %Y")
                return(paste(firstUp(shortMonth), format.Date(aDate, "%Y")))
            else if (self$options$displayFormat == "%b %y")
                return(paste(firstUp(shortMonth), format.Date(aDate, "%y")))
            else if (self$options$displayFormat == "%Y %B")
                return(paste(format.Date(aDate, "%Y"), longMonth))
            else if (self$options$displayFormat == "%Y %b")
                return(paste(format.Date(aDate, "%Y"), shortMonth))
            else if (self$options$displayFormat == "%b")
                return(firstUp(shortMonth))
            else if (self$options$displayFormat == "%B")
                return(firstUp(longMonth))
            else
                return(format.Date(aDate, self$options$displayFormat))
        },
        .convertToDate = function(dAsString, fmt) {
            n <- length(na.omit(dAsString))

            if (fmt == "iso")
                dAsDate <- as.Date(dAsString, optional = TRUE, tryFormats = c("%Y-%m-%d", "%Y/%m/%d", "%Y.%m.%d", "%Y%m%d"))
            else if (fmt == "us")
                dAsDate <- as.Date(dAsString, optional = TRUE, tryFormats = c("%m-%d-%Y", "%m/%d/%Y", "%m.%d.%Y", "%m%d%Y"))
            else if (fmt== "eu")
                dAsDate <- as.Date(dAsString, optional = TRUE, tryFormats = c("%d-%m-%Y", "%d/%m/%Y", "%d.%m.%Y", "%d%m%Y"))
            else
                dAsDate <- NULL

            if (length(na.omit(dAsDate)) != n || min(as.numeric(format(dAsDate, "%Y")), na.rm=T) < 100)
                dAsDate <- NULL

            return(dAsDate)
        })
)
