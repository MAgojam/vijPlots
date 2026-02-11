
# This file is a generated template, your changes will not be overwritten

histogramClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "histogramClass",
    inherit = histogramBase,
    private = list(
        .init = function() {
            # Default size
            # single facet : w = 550 + 50, h = 350 + 50
            # multiple facets : w = 450*ncol + 50 , h = 300*nrow + 50
            # legend : h + 50 if top/bttom, w + 100 if left/right

            # Stretchable dimensions
            if (!is.null(self$options$facet)) {
                nbOfFacet <- nlevels(self$data[[self$options$facet]])
                if (self$options$facetBy == "column") {
                    nbOfColumn <- self$options$facetNumber
                    nbOfRow <- ceiling(nbOfFacet / nbOfColumn)
                } else {
                    nbOfRow <- self$options$facetNumber
                    nbOfColumn <- ceiling(nbOfFacet / nbOfRow)
                }
                height <- max(350,300*nbOfRow)
                width <- max(550, 450*nbOfColumn)
            } else {
                width <- 550
                height <- 350
            }
            # Fixed dimension
            fixed_height <- 50 # X-Axis legend
            fixed_width <- 50 # Y-Axis legend
            if (!is.null(self$options$group)) {
                if (self$options$legendPosition %in% c('top','bottom'))
                    fixed_height <- fixed_height + 50
                else
                    fixed_width <- fixed_width + 100
            }
            # Set the image dimensions
            image <- self$results$plot
            if (is.null(image$setSize2)) { # jamovi < 2.7.16
                image$setSize(width + fixed_width, height + fixed_height)
            } else {
                image$setSize2(width, height, fixed_width, fixed_height)
            }
        },
        .run = function() {
            if (! is.null(self$options$aVar) && nrow(self$data) != 0) {
                plotData <- self$data[c(self$options$aVar, self$options$group, self$options$facet)]
                plotData[[self$options$aVar]] <- jmvcore::toNumeric(plotData[[self$options$aVar]])
                image <- self$results$plot
                image$setState(plotData)
            } else {
                return(FALSE)
            }
        },
        .plot = function(image, ggtheme, theme, ...) {  # <-- the plot function
            if (is.null(image$state))
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
            if (!is.null(groupVar)) {
                hist_arg[["position"]] <- self$options$groupingN
                hist_arg[["show.legend"]] <- TRUE
                #if (self$options$groupingN == "identity" && (fillColor != "white" || self$options$usePalette == "forFilling")) {
                if (self$options$groupingN == "identity") {
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

            # get binwidth value from ggplot dat
            ggData <- layer_data(plot)
            bw <- unique(ggData$xmax - ggData$xmin)[1]

            # Normal Curve
            if (self$options$normalCurve) {
                linetype = ifelse(self$options$dashedDensity,2,1)
                if (!is.null(groupVar)) {
                    if (self$options$histtype == "density")
                        plot <- plot + ggh4x::stat_theodensity(na.rm=TRUE, linewidth = self$options$normalCurveLineSize, linetype = linetype, show.legend = FALSE)
                    else
                        plot <- plot + ggh4x::stat_theodensity(aes(y = after_stat(count)*bw), na.rm=TRUE, linewidth = self$options$normalCurveLineSize, linetype = linetype, show.legend = FALSE)
                } else {
                    if (self$options$histtype == "density")
                        plot <- plot + ggh4x::stat_theodensity(na.rm=TRUE, color='red', linewidth = self$options$normalCurveLineSize, linetype = linetype)
                    else
                        plot <- plot + ggh4x::stat_theodensity(aes(y = after_stat(count)*bw), na.rm=TRUE, color='red', linewidth = self$options$normalCurveLineSize, linetype = linetype)
                }
            }

            # Density
            if (self$options$density) {
                if (!is.null(groupVar)) {
                    if (self$options$histtype == "density") {
                        plot <- plot + geom_density(aes(y = after_stat(density)), alpha = self$options$densityOpacity, linewidth = self$options$densityLineSize)
                    } else {
                        plot <- plot + geom_density(aes(y = after_stat(count) * bw), alpha = self$options$densityOpacity, linewidth = self$options$densityLineSize)
                    }
                } else {
                    if (self$options$histtype == "density") {
                        plot <- plot + geom_density(aes(y = after_stat(density)), fill = fillColor, alpha = self$options$densityOpacity, linewidth = self$options$densityLineSize)
                    } else {
                        plot <- plot + geom_density(aes(y = after_stat(count) * bw), fill = fillColor, alpha = self$options$densityOpacity, linewidth = self$options$densityLineSize)
                    }
                }
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
            plot <- plot + ggtheme + vijScale(self$options$colorPalette, "fill", drop = FALSE) +
                                    vijScale(self$options$colorPalette, "color", drop = FALSE)

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
