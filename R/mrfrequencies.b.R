mrfrequenciesClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "mrfrequenciesClass",
    inherit = mrfrequenciesBase,
    private = list(
        .init = function() {
            morevar <- (self$options$mode == "morevar")
            # Show help message (and hide results)
            if ((!morevar && is.null(self$options$repVar)) || (morevar && length(self$options$resps) < 1)) {
                self$results$responses$setVisible(FALSE)
                self$results$plot$setVisible(FALSE)
                return()
            } else {
                self$results$helpMessage$setVisible(FALSE)
            }
            table <- self$results$responses
            # Set custom name for options column
            if (morevar)
                table$getColumn('var')$setTitle(self$options$optionname)
            else
                table$getColumn('var')$setTitle(self$options$repVar)
            # Set the rows
            if (morevar) {
                for (i in seq_along(self$options$resps))
                    table$addRow(rowKey = i)
            } else {
                aCol <- self$data[[self$options$repVar]]
                uniqueValues <- unique(unlist(strsplit(levels(aCol), split = self$options$separator)))
                uniqueValues <- uniqueValues[uniqueValues != ""]
                for (i in seq_along(uniqueValues))
                    table$addRow(rowKey = i)
            }
            # Add the "total" row
            if (self$options$showTotal) {
                table$addRow(rowKey='.total', values=list(var="Total"))
                table$addFormat(rowKey=".total", col=1, Cell.BEGIN_GROUP)
            }
            # Set the size of the plot
            userWidth <- as.numeric(self$options$plotWidth)
            userHeight <- as.numeric(self$options$plotHeight)
            # Check min size
            if ((userWidth != 0 && userWidth < 200) || (userHeight != 0 && userHeight < 200))
                reject(.("Plot size must be at least 200px (or 0 = default)"))
            if (userWidth * userHeight == 0) {
                # use as default the obsolete (and hidden) size menu preset
                width <- switch (self$options$size,
                                 "small" = 300,
                                 "medium" = 400,
                                 "large" = 600,
                                 "wide" = 700,
                                 "huge" = 800,
                                 400)
                height <- switch (self$options$size,
                                 "small" = 200,
                                 "medium" = 300,
                                 "large" = 400,
                                 "wide" = 400,
                                 "huge" = 500,
                                 300)
            }
            if (userWidth >0)
                width = userWidth
            if (userHeight >0)
                height = userHeight
            image <- self$results$plot
            image$setSize(width, height)
        },
        .run = function() {
            if (self$options$mode == "morevar") { # Several dychotomous variables
                if (length(self$options$resps) < 1) {
                    return()
                } else {
                    myresult <- private$.multipleResponse(self$data, self$options$resps, self$options$endorsed, self$options$order)
                }
            } else { # One Multiple Value Variables
                if (is.null(self$options$repVar) || self$options$separator == '') {
                    return()
                } else {
                    rawData <- self$data[[self$options$repVar]]
                    oneHotData <- private$.oneHotEncoding(rawData, self$options$separator, self$options$emptyAsNA)
                    myresult <- private$.multipleResponse(oneHotData, names(oneHotData), 1, self$options$order)
                }
            }

            table <- self$results$responses
            for(i in 1:(nrow(myresult$df)-1))
                table$setRow(rowNo=i,
                             values=list(var=myresult$df[i,1],
                                         freq=myresult$df[i,2],
                                         responsepercent=myresult$df[i,3],
                                         casepercent=myresult$df[i,4]))

            if ( self$options$showTotal ) {
                i <- nrow(myresult$df)
                table$setRow(rowKey=".total",
                             values=list(var="Total",
                                         freq=myresult$df[i,2],
                                         responsepercent=myresult$df[i,3],
                                         casepercent=myresult$df[i,4]))
            }

            table$setNote('noc', paste(.("Number of cases:"), myresult$nrOfCases) , init=FALSE)

            image <- self$results$plot
            image$setState(myresult$df[1:(nrow(myresult$df)-1),])

        },
        .plot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state))
                return(FALSE)

            plotData <- image$state

            # to be sure the factor ordering is kept
            plotData$Option <- factor(plotData$Option, levels=plotData$Option)

            if (self$options$yaxis == "responses") {
                plot <- ggplot(plotData, aes(Option, Responses )) + scale_y_continuous(labels=percent_format())
                yLab <- .("% of Responses")
                yScaleFactor <- 100 # yScaleFactor is used for manual range computation (1 = count, 100 = percent)
            } else if (self$options$yaxis == "cases") {
                plot <- ggplot(plotData, aes(Option, Cases )) + scale_y_continuous(labels=percent_format())
                yLab <- .("% of Cases")
                yScaleFactor <- 100
            } else {
                plot <- ggplot(plotData, aes(Option, Frequency ))
                yLab <- .("Counts")
                yScaleFactor <- 1
            }

            if (self$options$singleColor) {
                nbColors <- attr(vijPalette(self$options$colorPalette, "fill"),"nlevels")
                colorNo <- self$options$colorNo
                oneColorOfPalette <- vijPalette(self$options$colorPalette, "fill")(nbColors)[min(colorNo,nbColors)]
                plot <- plot + geom_col(fill = oneColorOfPalette)
            } else {
                plot <- plot + geom_col(aes(fill = Option)) + guides(fill = FALSE)
            }

            # Theme and colors
            plot <- plot + ggtheme + vijScale(self$options$colorPalette, "fill")

            # Axis range
            if (self$options$yAxisRangeType == "manual") { # Horizontal and manual
                plot <- plot + coord_cartesian(ylim = c(self$options$yAxisRangeMin/yScaleFactor, self$options$yAxisRangeMax/yScaleFactor))
            }

            # Titles & Labels
            defaults <- list(y = yLab, x = "")
            plot <- plot + vijTitlesAndLabels(self$options, defaults) + vijTitleAndLabelFormat(self$options, showLegend = FALSE)

            return(plot)

        },
        .multipleResponse = function (data, items = NULL, endorsedOption = 1, order='none') {
            # From userfriendlyscience package
            data = data[, items]
            nrOfEndorsements = sum(data == endorsedOption, na.rm = TRUE)
            if( length(items) == 1 ) {
                endorsementsPerItem <- nrOfEndorsements
                names(endorsementsPerItem) <- items[1]
                nrOfCases <- sum(!is.na(data))
            } else {
                endorsementsPerItem = colSums(data == endorsedOption, na.rm = TRUE)
                nrOfCases = sum(!apply(apply(data, 1, is.na), 2, all))
            }
            totals = as.numeric(c(endorsementsPerItem, nrOfEndorsements))
            res <- data.frame(c(names(endorsementsPerItem), "Total"),
                              totals, (totals/nrOfEndorsements),
                              (totals/nrOfCases))
            names(res) <- c("Option", "Frequency", "Responses", "Cases")
            # Sort
            n <- length(items)
            if (order == 'decreasing') {
                res<-res[c(order(res$Frequency[1:n], decreasing = TRUE),n+1),]
            } else if (order == 'increasing') {
                res<-res[c(order(res$Frequency[1:n], decreasing = FALSE),n+1),]
            }

            return( list('nrOfCases'=nrOfCases, 'df'=res) )

        },
        .oneHotEncoding = function (aCol, separator, na = TRUE) {
            uniqueValues <- unique(unlist(strsplit(levels(aCol), split = separator)))
            uniqueValues <- uniqueValues[uniqueValues != ""]
            onehotDF <- data.frame("X__priVate__X" = 1:length(aCol))
            for(j in uniqueValues) {
                onehotDF[, j] <- ifelse(grepl(j, aCol, fixed = TRUE),1,0)
            }
            if (na)
                onehotDF[is.na(aCol),] <- NA
            onehotDF[,"X__priVate__X"] <- NULL
            return(onehotDF)
        }
    )
)
