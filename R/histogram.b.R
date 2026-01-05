
# This file is a generated template, your changes will not be overwritten

histogramClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "histogramClass",
    inherit = histogramBase,
    private = list(
        .init = function() {
            # Set the size of the plot
            userWidth <- as.numeric(self$options$plotWidth)
            userHeight <- as.numeric(self$options$plotHeight)
            # Check min size
            if ((userWidth != 0 && userWidth < 200) || (userHeight != 0 && userHeight < 200))
                reject("Plot size must be at least 200px (or 0 = default)")
            # Compute the size according to facet
            if (userWidth * userHeight == 0) {
                if (!is.null(self$options$facet)) {
                    nbOfFacet <- nlevels(self$data[[self$options$facet]])
                    nbOfColumn <-self$options$facetNumber
                    nbOfRow <- ceiling(nbOfFacet / nbOfColumn )
                    if (self$options$facetBy == "column") {
                        height <- max(400,250*nbOfRow)
                        width <- max(600, 300*nbOfColumn)
                    } else {
                        height <- max(400,250*nbOfColumn)
                        width <- max(600, 300*nbOfRow)
                    }
                } else {
                    width <- 600
                    height <- 400
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
            if (! is.null(self$options$aVar)) {
                plotData <- self$data[c(self$options$aVar, self$options$group, self$options$facet)]
                plotData[[self$options$aVar]] <- jmvcore::toNumeric(plotData[[self$options$aVar]])
                image <- self$results$plot
                image$setState(plotData)
            } else {
                return(FALSE)
            }
        },
        .plot = function(image, ggtheme, theme, ...) {  # <-- the plot function
            if (is.null(self$options$aVar))
                return(FALSE)
            plotData <- image$state

            xVar <- self$options$aVar
            xVar <- ensym(xVar)

            if (!is.null(self$options$group)) {
                groupVar <- self$options$group
                groupVar <- ensym(groupVar)
            } else {
                groupVar <- NULL
            }

            if (!is.null(self$options$facet)) {
                facetVar <- self$options$facet
                facetVar <- ensym(facetVar)
            } else {
                facetVar <- NULL
            }

            plotData <- jmvcore::naOmit(plotData)

            # Set bin width and boundary
            if (self$options$binWidth == 0)
                binWidth <- NULL
            else
                binWidth <- self$options$binWidth
            if (self$options$binBoundary == 0)
                binBoundary <- NULL
            else
                binBoundary <- self$options$binBoundary

            # set the border color
            if (self$options$borderColor == "none") {
                borderColor <- NA
            } else {
                borderColor = self$options$borderColor
            }
            # set the fill color
            if (self$options$fillColor == "none") {
                fillColor <- NA
            } else {
                fillColor = self$options$fillColor
            }

            # Define geom_historgram argument list
            if (self$options$histtype == "density") {
                hist_arg = list(aes(y = after_stat(density)))
            } else {
                hist_arg = list()
            }
            if (self$options$grouping != "none" && !is.null(groupVar)) {
                hist_arg[["position"]] <- self$options$grouping
                if (self$options$grouping == "identity" && (fillColor != "white" || self$options$usePalette == "forFilling")) {
                    hist_arg[["alpha"]] <- 0.5
                }
                if (self$options$usePalette == "forFilling") {
                    hist_arg[["color"]] <- borderColor
                } else {
                    hist_arg[["fill"]] <- fillColor
                }
            } else {
                hist_arg[["fill"]] <- fillColor
                hist_arg[["color"]] <- borderColor
            }
            hist_arg[["binwidth"]] <- binWidth
            hist_arg[["boundary"]] <- binBoundary

            plot <- ggplot(plotData, aes(x = !!xVar, fill = !!groupVar, color = !!groupVar))
            plot <- plot + do.call(geom_histogram, hist_arg)

            plot <- plot + labs(x = self$options$aVar)

            # Normal Curve
            if (self$options$normalCurve && is.null(groupVar)) {
                # get binwidth value from ggplot dat
                ggData <- layer_data(plot)
                bw <- unique(ggData$xmax - ggData$xmin)[1]
                if (self$options$histtype == "density")
                    plot <- plot + ggh4x::stat_theodensity(na.rm=TRUE, color='red', linewidth = 1)
                else
                    plot <- plot + ggh4x::stat_theodensity(aes(y = after_stat(count)*bw), na.rm=TRUE, color='red', linewidth = 1)
            }

            # Y-Axix label
            if (self$options$histtype == "density") {
                yLab <- .("Density")
                plot <- plot  + scale_y_continuous(labels = scales::comma)
            } else {
                yLab <- .("Count")
            }

            # Axis Limits
            if (self$options$yAxisRangeType == "manual")
                yLim <- c(self$options$yAxisRangeMin, self$options$yAxisRangeMax)
            else
                yLim <- NULL
            if (self$options$xAxisRangeType == "manual")
                xLim <- c(self$options$xAxisRangeMin, self$options$xAxisRangeMax)
            else
                xLim <- NULL
            plot <- plot + coord_cartesian(ylim = yLim, xlim = xLim)

            # Facet
            if (!is.null(facetVar)) {
                if (self$options$facetBy == "column")
                    plot <- plot + facet_wrap(vars(!!facetVar), ncol = as.numeric(self$options$facetNumber))
                else
                    plot <- plot + facet_wrap(vars(!!facetVar), nrow = as.numeric(self$options$facetNumber))
            }

            # Theme and colors
            plot <- plot + ggtheme + vijScale(self$options$colorPalette, "fill") +
                                    vijScale(self$options$colorPalette, "color")

            # Legend spacing
            plot <- plot + theme(legend.key.spacing.y = unit(1, "mm"), legend.byrow = TRUE)

            # Titles & Labels
            defaults <- list(legend = groupVar, x = xVar, y = yLab)
            plot <- plot + vijTitlesAndLabels(self$options, defaults) +
                            vijTitleAndLabelFormat(self$options)

            return(plot)
        }
    )
)
