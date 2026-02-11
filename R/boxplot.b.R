
# This file is a generated template, your changes will not be overwritten

boxplotClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "boxplotClass",
    inherit = boxplotBase,
    private = list(
        .init = function() {
            # Stretchable dimensions
            if( is.null(self$options$group) ) {
                if (self$options$horizontal) {
                    height <- min(max(100*length(self$options$vars), 300), 600)
                    width <- 600
                } else {
                    height <- 400
                    width <- min(max(150*length(self$options$vars), 400), 800)
                }
            } else {
                if (self$options$horizontal) {
                    height <- min(max(75*length(self$options$vars) * nlevels(self$data[[self$options$group]]), 300), 600)
                    width <- 600
                } else {
                    height <- 400
                    width <- min(max(100*length(self$options$vars) * nlevels(self$data[[self$options$group]]),400), 800)
                }
            }
            # Fixed dimension
            fixed_height <- 50 # X-Axis legend
            fixed_width <- 50 # Y-Axis legend
            if (!is.null(self$options$group) && length(self$options$vars) > 1) {
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
            if( length(self$options$vars) == 0 || nrow(self$data) == 0)
                return()
            labelVarName <- self$options$label
            groupVarName <- self$options$group
            depVarNames <- self$options$vars
            varNames <- c(labelVarName,groupVarName, depVarNames)
            #data <- jmvcore::select(self$data, varNames)
            data <- self$data[varNames]
            # Remove case with missing group
            if (!is.null(groupVarName) & self$options$ignoreNA) {
                data <- subset(data, !is.na(data[groupVarName]))
            }
            # Be sure dep var are numeric
            for (varName in depVarNames)
                data[[varName]] <- jmvcore::toNumeric(data[[varName]])
            image <- self$results$plot
            image$setState(data)
        },
        .plot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state))
                return(FALSE)
            plotData <- image$state
            labelVarName <- self$options$label
            groupVarName <- self$options$group
            depVarNames <- self$options$vars

            if (self$options$staple)
                stapleWidth <- as.numeric(self$options$stapleWidth)
            else
                stapleWidth <- 0

            notches <- self$options$notches
            notchWidth <- as.numeric(self$options$notchWidth)

            if( is.null(labelVarName) ) {
                labelVar = NULL
            } else {
                labelVar <- ensym(labelVarName)
            }

            if( is.null(groupVarName) ) {
                groupVar = NULL
            } else {
                groupVar <- ensym(groupVarName)
            }

            # Compute the outliers
            if (!is.null(labelVar)) {
                for (varName in depVarNames) {
                    outlierVar <- paste0(".outliers_",varName)
                    outlierVar <- ensym(outlierVar)
                    varName <- ensym(varName)
                    if (is.null(groupVar)) {
                        plotData <- plotData %>%
                            dplyr::mutate(!!outlierVar := ifelse(private$.isOutlier(!!varName), as.character(!!labelVar), NA))
                    } else {
                        plotData <- plotData %>%
                            dplyr::group_by(!!groupVar) %>%
                            dplyr::mutate(!!outlierVar := ifelse(private$.isOutlier(!!varName), as.character(!!labelVar), NA))
                    }
                }
            }

            if (self$options$horizontal)
                labAngle = 60
            else
                labAngle = 0

            if (self$options$horizontal)
                nudgeX <- (400/image$height)*0.06
            else
                nudgeX <- (400/image$width)*0.04

            # One color only
            if (self$options$singleColor) {
                nbColors <- attr(vijPalette(self$options$colorPalette, "fill"),"nlevels")
                colorNo <- self$options$colorNo
                oneColorOfPalette <- vijPalette(self$options$colorPalette, "fill")(nbColors)[min(colorNo,nbColors)]
            }

            # Building the plot
            plot <- ggplot(plotData)
            for (varName in depVarNames) {
                aVar <- ensym(varName)
                if (is.null(groupVar)) {
                    if (self$options$singleColor) {
                        plot <- plot + geom_boxplot(aes(y = !!aVar, x = !!varName), fill = oneColorOfPalette,
                                                    outliers = self$options$showOutliers, staplewidth = stapleWidth,
                                                    notch = notches, notchwidth = notchWidth)
                    } else {
                        plot <- plot + geom_boxplot(aes(y = !!aVar, x = !!varName, fill = !!varName),
                                                    outliers = self$options$showOutliers, staplewidth = stapleWidth,
                                                    notch = notches, notchwidth = notchWidth)
                    }
                    if (!is.null(labelVar) & self$options$showOutliers) {
                        outlierVar <- paste0(".outliers_",varName)
                        outlierVar <- ensym(outlierVar)
                        plot <- plot + geom_text(aes(x = !!varName, y = !!aVar, label = !!outlierVar), na.rm = TRUE, hjust = 0,
                                                 nudge_x = nudgeX, angle = labAngle)
                    }

                    plot <- plot + guides(fill = FALSE)
                    if( self$options$showMean ) {
                        plot <- plot + stat_summary(aes(y = !!aVar, x = !!varName), fun = mean, geom = "point",
                                                    shape = 15, size = 3)
                    }
                } else { # Several groups
                    if (length(depVarNames) > 1) {
                        plot <- plot + geom_boxplot(aes(y = !!aVar, x = !!varName, fill = !!groupVar),
                                                    outliers = self$options$showOutliers, staplewidth = stapleWidth,
                                                    notch = notches, notchwidth = notchWidth, key_glyph = draw_key_rect, show.legend = TRUE)
                        if (self$options$showMean) {
                            plot <- plot + stat_summary(aes(y = !!aVar, x = !!varName, group = !!groupVar), fun = mean, geom = "point",
                                                        position = position_dodge(.75), shape = 15, size = 3, show.legend = FALSE)
                        }
                        if (!is.null(labelVar) & self$options$showOutliers) {
                            outlierVar <- paste0(".outliers_",varName)
                            outlierVar <- ensym(outlierVar)
                            plot <- plot + geom_text(aes(x = !!varName,y = !!aVar, label = !!outlierVar, group = !!groupVar), na.rm = TRUE,
                                                     hjust = 0, position = ggpp::position_dodgenudge(x = nudgeX, width = .75), angle = labAngle)
                        }
                    } else { # single var & several groups
                        if (self$options$singleColor) {
                            plot <- plot + geom_boxplot(aes(y = !!aVar, x = !!groupVar), fill = oneColorOfPalette,
                                                        outliers = self$options$showOutliers, staplewidth = stapleWidth,
                                                        notch = notches, notchwidth = notchWidth)
                        } else {
                            plot <- plot + geom_boxplot(aes(y = !!aVar, x = !!groupVar, fill = !!groupVar),
                                                        outliers = self$options$showOutliers, staplewidth = stapleWidth,
                                                        notch = notches, notchwidth = notchWidth)
                        }

                        if (self$options$showMean) {
                            plot <- plot + stat_summary(aes(y = !!aVar, x = !!groupVar), fun = mean, geom = "point",
                                                        shape = 15, size = 3)
                        }
                        if (!is.null(labelVar) & self$options$showOutliers) {
                            outlierVar <- paste0(".outliers_",varName)
                            outlierVar <- ensym(outlierVar)
                            plot <- plot + geom_text(aes(x = !!groupVar, y = !!aVar, label = !!outlierVar), na.rm = TRUE,
                                                     hjust = 0, nudge_x = nudgeX, angle = labAngle)
                        }
                    }
                }
            }

            # Sort variables / levels by median
            if (length(depVarNames) > 1) {
                if (self$options$order == "none") {
                    plot <- plot + scale_x_discrete(limits = depVarNames)
                } else {
                    orderedVars <- order(sapply(plotData[,depVarNames], median, na.rm = TRUE), decreasing = (self$options$order == "decreasing"))
                    plot <- plot + scale_x_discrete(limits = depVarNames[orderedVars])
                }
            } else if (!is.null(groupVar) && self$options$order != "none") {
                orderedLevelsData <- forcats::fct_reorder(plotData[[groupVar]], plotData[[aVar]], .desc = (self$options$order == "decreasing"))
                orderedLevels <- levels(addNA(orderedLevelsData, ifany=TRUE))
                plot <- plot + scale_x_discrete(limits = orderedLevels, drop = FALSE)
            } else if (!is.null(groupVar)) {
                plot <- plot + scale_x_discrete(drop = FALSE) # keep unused levels
            }

            # Theme and colors
            plot <- plot + ggtheme + vijScale(self$options$colorPalette, "fill", drop = FALSE)

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

            plot <- plot + theme(legend.key.spacing.y = unit(1, "mm"), legend.byrow = TRUE)

            # Titles & Labels
            defaults <- list(legend = groupVar)
            if (!is.null(groupVar) && length(depVarNames) == 1) {
                defaults$x <- groupVarName
                defaults$y <- depVarNames
                showLegend <- FALSE
            } else {
                defaults$x = NULL
                showLegend <- TRUE
            }
            if (length(depVarNames) > 1) {
                defaults$x <- NULL
                defaults$y <- NULL
            }
            plot <- plot + vijTitlesAndLabels(self$options, defaults) + vijTitleAndLabelFormat(self$options, showLegend = showLegend)

            return(plot)
        },
        .isOutlier = function(x) {
            q1 <- quantile(x, .25, na.rm=T)
            q3 <- quantile(x, .75, na.rm=T)
            iqr <- IQR(x, na.rm=T)
            return(x < q1 - 1.5*iqr | x > q3 + 1.5*iqr)
        }

    )
)
