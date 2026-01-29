
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
                jmvcore::reject(.("Plot size must be at least 200px (or 0 = default)"))

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
            if (!is.null(self$options$rows) && nrow(self$data) != 0) {
                plotData <- self$data[c(self$options$rows, self$options$columns, self$options$facet)]
                if( self$options$ignoreNA )
                    plotData <- jmvcore::naOmit(plotData)
                image <- self$results$plot
                image$setState(plotData)
            }
        },

        .plot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state))
                return(FALSE)
            plotData <- image$state

            #### Variables ####

            category <- self$options$rows
            category <- ensym(category)

            columns <- self$options$columns
            if (!is.null(columns))
                group <- ensym(columns)
            else
                group <- NULL

            if (self$options$borderColor == "none")
                borderColor = NA
            else
                borderColor = self$options$borderColor

            position <- self$options$barType
            positionStack <- (position == "stack")
            if(position == "dodge2")
                position <- position_dodge2(preserve = "single")

            yaxis <- self$options$yaxis


            if (self$options$order == "decreasing")
                plotData[[category]] <- forcats::fct_infreq(plotData[[category]])
            else if (self$options$order == "increasing")
                plotData[[category]] <- forcats::fct_rev(forcats::fct_infreq(plotData[[category]]))

            reverseStack <- (!self$options$reverseStack && positionStack)

            if (reverseStack)
                position <- position_stack(reverse = TRUE)

            # Percent format (scales)
            doPercent <- scales::label_percent(accuracy = as.numeric(self$options$accuracy), suffix = .("%"), decimal.mark = self$options[['decSymbol']])
            if (self$options$horizontal)
                doNumber <- function(x){ifelse(x<10, paste0(" ",x), x)}
            else
                doNumber <- as.character

            # Correct Single color option
            if (positionStack || !is.null(group))
                singleColor <- FALSE
            else
                singleColor <- self$options$singleColor

            if (singleColor) {
                nbColors <- attr(vijPalette(self$options$colorPalette, "fill"),"nlevels")
                colorNo <- self$options$colorNo
                oneColorOfPalette <- vijPalette(self$options$colorPalette, "fill")(nbColors)[min(colorNo,nbColors)]
            }

            # Correct labelPosition option
            if (positionStack)
                labelPosition <- "middle"
            else
                labelPosition <- self$options$labelPosition

            # Correct textColor option
            if (labelPosition == "top")
                textColor <- "black"
            else if (self$options$textColor == "auto" && singleColor)
                textColor <- ggstats::hex_bw(oneColorOfPalette)
            else
                textColor <- self$options$textColor


            #### AES ####

            if (is.null(group)) { # No group
                bby <- 1
                ffill <- category
                if (positionStack)
                    xx <- 1
                else
                    xx <- category
            } else { # with group
                xx <- category
                ffill <- group
                if (self$options$percentWithin == "group")
                    bby <- group
                else
                    bby <- category
            }

            plot <- ggplot(plotData, aes(x = !!xx, fill = !!ffill, by = !!bby))

            #### Bars ####

            if (yaxis == "count") {
                if (singleColor)
                    plot <- plot + geom_bar(aes(y = after_stat(count)), stat = "count", position = position,
                                            color = borderColor, fill = oneColorOfPalette)
                else
                    plot <- plot + geom_bar(aes(y = after_stat(count)), stat = "count", position = position,
                                            color = borderColor)
            } else {
                if (singleColor)
                    plot <- plot + geom_bar(aes(y = after_stat(prop)), stat = ggstats::StatProp, position = position,
                                            color = borderColor, fill = oneColorOfPalette)
                else
                    plot <- plot + geom_bar(aes(y = after_stat(prop)), stat = ggstats::StatProp, position = position,
                                            color = borderColor)
            }

            #### Labels ####

            if (self$options$showLabels) {
                # Default justifications
                vjust2 <- 0.5
                hjust2 <- 0.5
                vfactor <- 1

                # Change justifications
                if (positionStack) {
                    if (reverseStack)
                        labPosition <- position_stack(vjust = 0.5, reverse = TRUE)
                    else
                        labPosition <- position_stack(vjust = 0.5)
                } else {
                    if (self$options$labelPosition == "middle") {
                        labPosition <- position_dodge(width = 0.9)
                        vfactor <- 2
                    } else {
                        labPosition <- position_dodge(width = 0.9)
                        if (self$options$horizontal) {
                            hjust2 <- -0.2
                        } else {
                            vjust2 <- -0.6
                        }
                    }
                }

                # geom_text
                if (yaxis == "count") {
                    if (textColor == "auto") {
                        plot <- plot + geom_text(aes(y = after_stat(count)/vfactor, label = doNumber(after_stat(count)),
                                                                                        color = after_scale(ggstats::hex_bw(.data$fill))),
                                                 stat = "count", position = labPosition, vjust = vjust2, hjust = hjust2,
                                                 fontface = "bold", size = self$options$labelFontSize / .pt)
                    } else {
                        plot <- plot + geom_text(aes(y = after_stat(count)/vfactor, label = doNumber(after_stat(count))),
                                                 stat = "count", position = labPosition, vjust = vjust2, hjust = hjust2,
                                                 color = textColor, fontface = "bold", size = self$options$labelFontSize / .pt)
                    }
                } else { # Percent
                    if (textColor == "auto") {
                        plot <- plot + geom_text(aes(y = after_stat(prop)/vfactor, label = doPercent(after_stat(prop)),
                                                                                        color = after_scale(ggstats::hex_bw(.data$fill))),
                                                 stat = ggstats::StatProp, position = labPosition, vjust = vjust2, hjust = hjust2,
                                                 fontface = "bold", size = self$options$labelFontSize / .pt)
                    } else {
                        plot <- plot + geom_text(aes(y = after_stat(prop)/vfactor, label = doPercent(after_stat(prop))),
                                                 stat = ggstats::StatProp, position = labPosition, vjust = vjust2, hjust = hjust2,
                                                 color = textColor, fontface = "bold", size = self$options$labelFontSize / .pt)
                    }
                }
            }

            #### Finishing ####

            # Legend
            if (is.null(group) && !positionStack)
                plot <- plot + guides(fill = "none")

            # Axis labels and scales
            if (yaxis == "count") {
                yLab <- .("Count")
                yScaleFactor <- 1
            } else {
                yLab <- .("Percent")
                yScaleFactor <- 100
                plot <- plot + scale_y_continuous(labels = scales::label_percent())
            }
            # Axis Limits & flip
            if (self$options$horizontal) {
                if (self$options$xAxisRangeType == "manual") { # Horizontal and manual
                    plot <- plot + coord_flip(ylim = c(self$options$xAxisRangeMin/yScaleFactor, self$options$xAxisRangeMax/yScaleFactor))
                } else {
                    if (self$options$showLabels && self$options$labelPosition == "top")
                        plot <- plot + coord_flip(clip = "off", ylim = layer_scales(plot)$y$get_limits()*1.1) # Gives more room for labels !
                    else
                        plot <- plot + coord_flip(clip = "off")
                }
            } else {
                if (self$options$yAxisRangeType == "manual") {
                    plot <- plot + coord_cartesian(ylim = c(self$options$yAxisRangeMin/yScaleFactor, self$options$yAxisRangeMax/yScaleFactor))
                } else {
                    plot <- plot + coord_cartesian(clip = "off")
                }
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
            defaults <- list(y = yLab, x = category, legend = group)
            plot <- plot + vijTitlesAndLabels(self$options, defaults) + vijTitleAndLabelFormat(self$options)

            # Legend position
            plot <- plot + theme(legend.key.spacing.y = unit(1, "mm"), legend.byrow = TRUE)

            #self$results$text$setContent(plot)

            return(plot)

        })
)
