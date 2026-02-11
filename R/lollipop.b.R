
# This file is a generated template, your changes will not be overwritten

lollipopClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "lollipopClass",
    inherit = lollipopBase,
    private = list(
        .init = function() {
            if (!is.null(self$options$group))
                nbOfLevel <- nlevels(self$data[[self$options$group]])
            else
                nbOfLevel <- 5

            # Stretchable dimensions
            if (self$options$horizontal) {
                height <- min(max(250,nbOfLevel*50),650)
                width <- 400
            } else {
                width <- min(max(350,nbOfLevel*75),600)
                height <- 350
            }
            # With facets
            if (!is.null(self$options$facet)) {
                nbOfFacet <- nlevels(self$data[[self$options$facet]])
                if (self$options$facetBy == "column") {
                    nbOfColumn <- self$options$facetNumber
                    nbOfRow <- ceiling(nbOfFacet / nbOfColumn)
                } else {
                    nbOfRow <- self$options$facetNumber
                    nbOfColumn <- ceiling(nbOfFacet / nbOfRow)
                }
                height <- max(height, 0.75*height*nbOfRow)
                width <- max(width, 0.75*width*nbOfColumn)
            }
            # Fixed dimension
            if (self$options$horizontal) {
                fixed_height <- 50
                fixed_width <- 100
            } else {
                fixed_height <- 50
                fixed_width <- 75
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
            if (!is.null(self$options$aVar) && !is.null(self$options$group) && nrow(self$data) != 0) {
                plotData <- self$data[c(self$options$aVar, self$options$group, self$options$facet)]
                plotData[[self$options$aVar]] <- jmvcore::toNumeric(plotData[[self$options$aVar]])
                plotData <- jmvcore::naOmit(plotData)
                image <- self$results$plot
                image$setState(plotData)
            }
        },
        .plot = function(image, ggtheme, theme, ...) {  # <-- the plot function
            if (is.null(image$state))
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

            plot <- plot + scale_x_discrete(drop = FALSE) # keep unused levels

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
