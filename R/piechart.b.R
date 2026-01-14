
# This file is a generated template, your changes will not be overwritten

piechartClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "piechartClass",
    inherit = piechartBase,
    private = list(
        .init = function() {
            # Set the size of the plot
            userWidth <- as.numeric(self$options$plotWidth)
            userHeight <- as.numeric(self$options$plotHeight)
            # Check min size
            if ((userWidth != 0 && userWidth < 200) || (userHeight != 0 && userHeight < 200))
                reject(.("Plot size must be at least 200px (or 0 = default)"))

            # Compute the size according to facet
            if( userWidth * userHeight == 0 ) {
                if( !is.null(self$options$facet)) {
                    nbOfFacet <- nlevels(self$data[[self$options$facet]])
                    nbOfColumn <-self$options$facetNumber
                    nbOfRow <- ceiling(nbOfFacet / nbOfColumn )

                    if( self$options$facetBy == "column" ) {
                        height <- max(400,300*nbOfRow)
                        width <- max(600, 200*nbOfColumn)
                    } else {
                        height <- max(400,300*nbOfColumn)
                        width <- max(600, 200*nbOfRow)
                    }
                } else {
                    width <- 600
                    height <- 400
                }
                if (self$options$legendPosition %in% c('top','bottom')) {
                    height <- height + 50
                    width <- width - 50
                }
            }
            if( userWidth >0 )
                width = userWidth
            if( userHeight >0 )
                height = userHeight
            image <- self$results$plot
            image$setSize(width, height)
        },

        .run = function() {
            if( ! is.null(self$options$aVar) ) {
                plotData <- self$data[c(self$options$aVar, self$options$facet)]
                plotData <- jmvcore::naOmit(plotData)
                image <- self$results$plot
                image$setState(plotData)
            }
        },
        .plot = function(image, ggtheme, theme, ...) {
            if( is.null(self$options$aVar) )
                return(FALSE)
            plotData <- image$state
            aVar <- self$options$aVar
            aVar <- ensym(aVar)

            if( !is.null(self$options$facet) ) {
                facetVar <- self$options$facet
                facetVar <- ensym(facetVar)
            } else {
                facetVar <- NULL
            }

            # set the border color
            if( self$options$borderColor == "none") {
                borderColor <- NA
            } else {
                borderColor = self$options$borderColor
            }

            # Percent format (scales)
            doPercent <- label_percent(accuracy = as.numeric(self$options$accuracy), suffix = .("%"), decimal.mark = .("."))

            if(self$options$donut) {
                plot <- ggplot(plotData, aes(x = 10, fill = !!aVar, by = 1)) + xlim(c(8.5,10.5))
            } else {
                plot <- ggplot(plotData, aes(x = "", fill = !!aVar, by = 1))
            }

            plot <- plot + geom_bar(position = "fill", color = borderColor) + coord_polar("y")

            # Labels
            if( self$options$labels == "count" ) {
                if (self$options$textColor == "auto") { # using hex_bw
                    plot <- plot + geom_text(aes(fill = !!aVar, label = after_stat(count),
                                                 color = after_scale(ggstats::hex_bw(.data$fill))),
                                             stat = "count", position = position_fill(vjust = 0.5), fontface = "bold")
                } else {
                    plot <- plot + geom_text(aes(label = after_stat(count)), stat = "count",
                                                 position = position_fill(vjust = 0.5),
                                                 color = self$options$textColor, fontface = "bold")
                }
            } else if( self$options$labels == "percent" ) {
                if (self$options$textColor == "auto") { # using hex_bw
                    plot <- plot + geom_text(aes(label = doPercent(after_stat(prop)),
                                                 color = after_scale(ggstats::hex_bw(.data$fill))),
                                             stat = StatProp, position = position_fill(vjust = 0.5), fontface = "bold")
                } else {
                    plot <- plot + geom_text(aes(label = doPercent(after_stat(prop))), stat = StatProp, position = position_fill(vjust = 0.5),
                                             color = self$options$textColor, fontface = "bold")
                }
            }

            # Facet
            if( !is.null(facetVar) ) {
                if( self$options$facetBy == "column")
                    plot <- plot + facet_wrap(vars(!!facetVar), ncol = as.numeric(self$options$facetNumber), scales = "free")
                else
                    plot <- plot + facet_wrap(vars(!!facetVar), nrow = as.numeric(self$options$facetNumber), scales = "free")
            }

            # Theme and colors
            plot <- plot + ggtheme + vijScale(self$options$colorPalette, "fill")

            # Titles & Labels
            defaults <- list(y = "", x = "", legend = aVar)
            plot <- plot + vijTitlesAndLabels(self$options, defaults) + vijTitleAndLabelFormat(self$options)
            plot <- plot + theme(legend.key.spacing.y = unit(1, "mm"), legend.byrow = TRUE)

            plot

            # Labs
            plot <- plot + theme(axis.ticks = element_blank(),
                                 axis.line.x = element_blank(), axis.line.y = element_blank(),
                                 axis.text.x = element_blank(),axis.text.y = element_blank(),
                                 panel.grid.major = element_blank(), panel.grid.minor = element_blank())

            return(plot)
        }

    )
)
