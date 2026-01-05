
# This file is a generated template, your changes will not be overwritten

lollipopClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "lollipopClass",
    inherit = lollipopBase,
    private = list(
        .init = function() {
            # Set the size of the plot
            userWidth <- as.numeric(self$options$plotWidth)
            userHeight <- as.numeric(self$options$plotHeight)
            # Check min size
            if ((userWidth != 0 && userWidth < 200) || (userHeight != 0 && userHeight < 200))
                reject("Plot size must be at least 200px (or 0 = default)")

            if (userWidth * userHeight == 0) {
                if (!is.null(self$options$group))
                    nbOfLevel <- nlevels(self$data[[self$options$group]])
                else
                    nbOfLevel <- 4

                # width/height between 400 and 600
                if (self$options$horizontal) {
                    height <- min(max(300,nbOfLevel*75),800)
                    width <- 600
                } else {
                    width <- min(max(400,nbOfLevel*100),600)
                    height <- 400
                }

                if (!is.null(self$options$facet)) {
                    nbOfFacet <- nlevels(self$data[[self$options$facet]])
                    nbOfColumn <-self$options$facetNumber
                    nbOfRow <- ceiling(nbOfFacet / nbOfColumn)

                    if (self$options$facetBy == "column") {
                        height <- max(height,(height-100)*nbOfRow)
                        width <- max(width, (width-100)*nbOfColumn)
                    } else {
                        height <- max(height,(height-100)*nbOfColumn)
                        width <- max(width, (width-100)*nbOfRow)
                    }
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
            if (!is.null(self$options$aVar) && !is.null(self$options$group)) {
                plotData <- self$data[c(self$options$aVar, self$options$group, self$options$facet)]
                plotData[[self$options$aVar]] <- jmvcore::toNumeric(plotData[[self$options$aVar]])
                plotData <- jmvcore::naOmit(plotData)
                image <- self$results$plot
                image$setState(plotData)
            }
        },
        .plot = function(image, ggtheme, theme, ...) {  # <-- the plot function
            if (is.null(self$options$aVar) || is.null(self$options$group))
                return(FALSE)
            plotData <- image$state

            aVar <- self$options$aVar
            aVar <- ensym(aVar)
            groupVar <- self$options$group
            groupVar <- ensym(groupVar)

            if (!is.null(self$options$facet)) {
                facetVar <- self$options$facet
                facetVar <- ensym(facetVar)
            } else {
                facetVar <- NULL
            }

            orderFun <- self$options$yaxis
            if (orderFun == "minmax" || orderFun == "identity")
                orderFun <- max

            if (self$options$order == "decreasing")
                #plot <- ggplot(plotData, aes(x = reorder(!!groupVar,!!aVar, orderFun, decreasing = TRUE) , y = !!aVar))
                plot <- ggplot(plotData, aes(x = forcats::fct_reorder(!!groupVar,!!aVar, .fun = orderFun, .desc = TRUE) , y = !!aVar))
            else if (self$options$order == "increasing")
                #plot <- ggplot(plotData, aes(x = reorder(!!groupVar,!!aVar, orderFun, decreasing = FALSE) , y = !!aVar))
                plot <- ggplot(plotData, aes(x = forcats::fct_reorder(!!groupVar,!!aVar, .fun = orderFun, .desc = FALSE) , y = !!aVar))
            else
                plot <- ggplot(plotData, aes(x = !!groupVar, y = !!aVar))

            summaryFun <- self$options$yaxis
            if (summaryFun == "minmax") {
                mainColor <- self$options$dotColor
                lightColor <- colorspace::lighten(mainColor, 0.4)
                darkColor <- colorspace::darken(mainColor, 0.2)
                plot <- plot +
                    stat_summary(fun = max, fun.min = min, fun.max = max, color = self$options$lineColor) +
                    stat_summary(fun = min, geom = "point", size = self$options$dotSize, color=lightColor) +
                    stat_summary(fun = max, geom = "point", size = self$options$dotSize, color = darkColor)
            } else {
                plot <- plot +
                    stat_summary(fun = summaryFun, geom = "segment", aes(yend = 0), size = self$options$lineWidth, color = self$options$lineColor)+
                    stat_summary(fun = summaryFun, geom = "point", size = self$options$dotSize, color = self$options$dotColor)
            }

            # Axis Limits & flip
            if (self$options$horizontal) {
                if (self$options$xAxisRangeType == "manual") {
                    plot <- plot + coord_flip(ylim = c(self$options$xAxisRangeMin, self$options$xAxisRangeMax))
                } else {
                    plot <- plot + coord_flip()
                }
            } else if (self$options$yAxisRangeType == "manual") { # Horizontal and manual
                plot <- plot + coord_cartesian(ylim = c(self$options$yAxisRangeMin, self$options$yAxisRangeMax))
            }

            # Axis Labels
            if (self$options$yaxis == "mean")
                ylabel = paste(.("Mean of"), aVar)
            else if (self$options$yaxis == "median")
                ylabel = paste(.("Median of"), aVar)
            else if (self$options$yaxis == "min")
                ylabel = paste(.("Minimum of"), aVar)
            else if (self$options$yaxis == "max")
                ylabel = paste(.("Maximum of"), aVar)
            else
                ylabel = aVar

            # facet
            if (!is.null(self$options$facet)) {
                facetVar <- self$options$facet
                facetVar <- ensym(facetVar)
                if (self$options$facetBy == "column")
                    plot <- plot + facet_wrap(vars(!!facetVar), ncol = as.numeric(self$options$facetNumber))
                else
                    plot <- plot + facet_wrap(vars(!!facetVar), nrow = as.numeric(self$options$facetNumber))
            }

            # Theme and colors
            plot <- plot + ggtheme

            # Titles & Labels
            defaults <- list(y = ylabel, x = groupVar)
            plot <- plot + vijTitlesAndLabels(self$options, defaults) + vijTitleAndLabelFormat(self$options, showLegend = FALSE)

            return(plot)
        })
)
