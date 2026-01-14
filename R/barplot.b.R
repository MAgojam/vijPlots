
# This file is a generated template, your changes will not be overwritten

barplotClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "barplotClass",
    inherit = barplotBase,
    private = list(
        .init = function() {
            # Set the size of the plot
            userWidth <- as.numeric(self$options$plotWidth)
            userHeight <- as.numeric(self$options$plotHeight)
            # Check min size
            if ((userWidth != 0 && userWidth < 200) || (userHeight != 0 && userHeight < 200))
                reject("Plot size must be at least 200px (or 0 = default)")

            if (userWidth * userHeight == 0) {
                if( !is.null(self$options$columns)) {
                    width <- 600 #+ 50 * nlevels(self$data[[self$options$columns]]))
                } else {
                    width <- 500
                }
                if (!is.null(self$options$facet)) {
                    nbOfFacet <- nlevels(self$data[[self$options$facet]])
                    nbOfColumn <-self$options$facetNumber
                    nbOfRow <- ceiling(nbOfFacet / nbOfColumn )

                    if (self$options$facetBy == "column") {
                        height <- max(400,300*nbOfRow)
                        width <- max(500, 200*nbOfColumn)
                    } else {
                        height <- max(400,300*nbOfColumn)
                        width <- max(500, 200*nbOfRow)
                    }
                } else {
                    height <- 400
                }
                if (self$options$legendPosition %in% c('top','bottom')) {
                    width <- width - 50
                    height <- height + 50
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
            if( ! is.null(self$options$rows) ) {
                plotData <- self$data[c(self$options$rows, self$options$columns, self$options$facet)]
                if( self$options$ignoreNA )
                    plotData <- jmvcore::naOmit(plotData)
                image <- self$results$plot
                image$setState(plotData)
            }
        },

        .plot = function(image, ggtheme, theme, ...) {
            if (is.null(self$options$rows))
                return(FALSE)
            plotData <- image$state
            rows <- self$options$rows
            rows <- ensym(rows)

            columns <- self$options$columns
            if (!is.null(columns))
                columns <- ensym(columns)

            if (self$options$borderColor == "none")
                borderColor = NA
            else
                borderColor = self$options$borderColor

            # Percent format (scales)
            doPercent <- label_percent(accuracy = as.numeric(self$options$accuracy), suffix = .("%"), decimal.mark = .("."))
            # yScaleFactor is used for manual range computation (1 = count, 100 = percent)
            yScaleFactor <- 1 # default to count
            # ggplot with base AES and sorting
            if (self$options$order == "decreasing")
                plot <- ggplot(plotData, aes(x = forcats::fct_infreq(!!rows)))
            else if (self$options$order == "increasing")
                plot <- ggplot(plotData, aes(x = forcats::fct_rev(forcats::fct_infreq(!!rows))))
            else
                plot <- ggplot(plotData, aes(x = !!rows))
            ## One variable
            if (is.null(columns)) {
                if (self$options$singleColor) {
                    nbColors <- attr(vijPalette(self$options$colorPalette, "fill"),"nlevels")
                    colorNo <- self$options$colorNo
                    oneColorOfPalette <- vijPalette(self$options$colorPalette, "fill")(nbColors)[min(colorNo,nbColors)]
                }
                # One variable with Percentage
                if (self$options$yaxis1var == "percent") {
                    if (self$options$singleColor) {
                        plot <- plot + geom_bar(aes(y = after_stat(prop), group=1),
                                                fill = oneColorOfPalette, color = borderColor)
                    } else {
                        plot <- plot + geom_bar(aes(y = after_stat(prop), by = 1, fill = !!rows), stat = StatProp, color = borderColor)
                        plot <- plot + guides(fill = FALSE)
                    }
                    plot <- plot + scale_y_continuous(labels=percent_format())
                    yLab <- .("Percent")
                    yScaleFactor <- 100 # for manual range
                    if( self$options$showLabels ) {
                        if (self$options$textColor == "auto") { # using hex_bw
                            if (self$options$singleColor) {
                                plot <- plot + geom_text(aes(y = after_stat(prop), group=1, label = doPercent(after_stat(prop)),
                                                                color = after_scale(ggstats::hex_bw(oneColorOfPalette))),
                                                     stat = StatProp, position = position_stack(vjust = 0.5), fontface = "bold")
                            } else {
                                plot <- plot + geom_text(aes(y = after_stat(prop), group=1, label = doPercent(after_stat(prop)),
                                                             fill = !!rows, color = after_scale(ggstats::hex_bw(.data$fill))),
                                                         stat = StatProp, position = position_stack(vjust = 0.5), fontface = "bold")
                            }
                        } else {
                            plot <- plot + geom_text(aes(y = after_stat(prop), group=1, label = doPercent(after_stat(prop))),
                                                    stat = StatProp, position = position_stack(vjust = 0.5),
                                                    color = self$options$textColor, fontface = "bold")
                        }
                    }
                # One variable with Count
                } else {
                    if (self$options$singleColor) {
                        plot <- plot + geom_bar(fill = oneColorOfPalette, color = borderColor)
                    } else {
                        plot <- plot + geom_bar(aes(fill = !!rows), color = borderColor)
                        plot <- plot + guides(fill = FALSE)
                    }
                    if (self$options$showLabels) {
                        if (self$options$textColor == "auto") { # using hex_bw
                            if (self$options$singleColor) {
                                plot <- plot + geom_text(aes(label = after_stat(count), y = after_stat(count),
                                                             color = after_scale(ggstats::hex_bw(oneColorOfPalette))),
                                                         stat = "count", position = position_stack(vjust = 0.5),fontface = "bold")
                            } else {
                                plot <- plot + geom_text(aes(label = after_stat(count), y = after_stat(count),
                                                             fill = !!rows, color = after_scale(ggstats::hex_bw(.data$fill))),
                                                         stat = "count", position = position_stack(vjust = 0.5), fontface = "bold")
                            }
                        } else {
                            plot <- plot + geom_text(aes(label = after_stat(count), y = after_stat(count)),
                                                stat = "count", position = position_stack(vjust = 0.5),
                                             color = self$options$textColor, fontface = "bold")
                        }
                    }
                    yLab <- .("Count")
                }
            ## Two variables
            } else {
                plot <- plot + geom_bar(aes(fill = !!columns, by = !!rows), position = self$options$position, color = borderColor)
                # Two variables with Percentage (position = fill)
                if (self$options$position == "fill") {
                    plot <- plot + scale_y_continuous(labels=percent_format())
                    if (self$options$showLabels) {
                        if (self$options$textColor == "auto") { # using hex_bw
                            plot <- plot + geom_text(aes(fill = !!columns, by = !!rows, label=doPercent(after_stat(prop)),
                                                         color = after_scale(ggstats::hex_bw(.data$fill))),
                                                     stat = StatProp, position = position_fill(.5), fontface = "bold")
                        } else {
                            plot <- plot + geom_text(aes(fill = !!columns, by = !!rows, label=doPercent(after_stat(prop))), stat = StatProp, position = position_fill(.5),
                                                     color = self$options$textColor, fontface = "bold")
                        }
                    }
                    yLab <- .("Percent")
                    yScaleFactor <- 100 # for manual range
                # Two variables with count (dodge)
                } else if (self$options$position == "dodge" || self$options$position == "dodge2") {
                    if (self$options$showLabels) {
                        if (self$options$textColor == "auto") { # using hex_bw
                            plot <- plot + geom_text(aes(fill = !!columns, label = after_stat(count), y = after_stat(count/2),
                                                         color = after_scale(ggstats::hex_bw(.data$fill))),
                                            position = position_dodge(width = 0.9),
                                            stat = "count", fontface = "bold")
                        } else {
                            plot <- plot + geom_text(aes(fill = !!columns, label = after_stat(count), y = after_stat(count/2)),
                                                     position = position_dodge(width = 0.9),
                                                     stat = "count",
                                                     color = self$options$textColor, fontface = "bold")
                        }
                    }
                    yLab <- .("Count")
                # Two variables with count (staked)
                } else { # Stacked
                    if (self$options$showLabels) {
                        if (self$options$textColor == "auto") { # using hex_bw
                            plot <- plot + geom_text(aes(fill = !!columns, label = after_stat(count), y = after_stat(count),
                                                         color = after_scale(ggstats::hex_bw(.data$fill))),
                                                position = position_stack(vjust = 0.5),
                                                stat = "count", fontface = "bold")
                        } else {
                            plot <- plot + geom_text(aes(fill = !!columns, label = after_stat(count), y = after_stat(count)),
                                                     position = position_stack(vjust = 0.5),
                                                     stat = "count",
                                                     color = self$options$textColor, fontface = "bold")
                        }
                    }
                    yLab <- .("Count")
                }
            }

            # Axis Limits & flip
            if (self$options$horizontal) {
                if (self$options$xAxisRangeType == "manual") {
                    plot <- plot + coord_flip(ylim = c(self$options$xAxisRangeMin/yScaleFactor, self$options$xAxisRangeMax/yScaleFactor))
                } else {
                    plot <- plot + coord_flip()
                }
            } else if (self$options$yAxisRangeType == "manual") { # Horizontal and manual
                plot <- plot + coord_cartesian(ylim = c(self$options$yAxisRangeMin/yScaleFactor, self$options$yAxisRangeMax/yScaleFactor))
            }

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
            plot <- plot + ggtheme + vijScale(self$options$colorPalette, "fill")

            # Titles & Labels
            defaults <- list(y = yLab, x = rows, legend = columns)
            plot <- plot + vijTitlesAndLabels(self$options, defaults) + vijTitleAndLabelFormat(self$options)

            # Legend position
            plot <- plot + theme(legend.key.spacing.y = unit(1, "mm"), legend.byrow = TRUE)

            return(plot)
        })
)
