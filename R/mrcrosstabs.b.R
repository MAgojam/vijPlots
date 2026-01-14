mrcrosstabsClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "mrcrosstabsClass",
    inherit = mrcrosstabsBase,
    private = list(
        .init = function() {
            morevar <- (self$options$mode == "morevar")
            # Show help message (and hide results)
            if ((!morevar && (is.null(self$options$repVar) || self$options$separator == ''))
                    || (morevar && length(self$options$resps) < 1)) {
                self$results$crosstab$setVisible(FALSE)
                self$results$plot$setVisible(FALSE)
                return()
            } else {
                self$results$helpMessage$setVisible(FALSE)
            }
            table <- self$results$crosstab
            # Table title
            if ( self$options$computedValues == "options" ) {
                table$setTitle(.("Crosstab (% by row)"))
            } else if (self$options$computedValues == "cases") {
                table$setTitle(.("Crosstab (% of Cases)"))
            } else if (self$options$computedValues == "responses") {
                table$setTitle(.("Crosstab (% of Responses)"))
            }
            # Table rows
            if (morevar) {
                for (i in seq_along(self$options$resps))
                    table$addRow(rowKey = i, list(var = self$options$resps[i]))
            } else {
                aCol <- self$data[[self$options$repVar]]
                uniqueValues <- unique(unlist(strsplit(levels(aCol), split = self$options$separator)))
                uniqueValues <- uniqueValues[uniqueValues != ""]
                for (i in seq_along(uniqueValues))
                    table$addRow(rowKey = i, list(var = uniqueValues[i]))
            }
            # Add the "total" row here (to prevent flickering)
            if ( self$options$totalRow ) {
                table$addRow(rowKey='.total', values=list(var="Total"))
                table$addFormat(rowKey=".total", col=1, Cell.BEGIN_GROUP)
            }
            if ( self$options$showNbOfCases && ( self$options$computedValues == "count" || self$options$computedValues == "options") ) {
                table$addRow(rowKey='.nbofcases', values=list(var=.("Number of cases")))
                table$addFormat(rowKey=".nbofcases", col=1, Cell.BEGIN_GROUP)
            }
            # Set custom name for options column
            if (morevar)
                table$getColumn('var')$setTitle(self$options$optionname)
            else
                table$getColumn('var')$setTitle(self$options$repVar)
            # Cell Type
            if ( self$options$computedValues == "count" ) {
                cellType = "integer"
                cellFormat = ""
            } else {
                cellType = "number"
                cellFormat = "pc"
            }
            # Columns
            if (morevar)
                groupVar <- self$options$group
            else
                groupVar <- self$options$group2
            if ( ! is.null(groupVar) ) {
                gLevels <- levels(self$data[,groupVar])
                for(i in seq_along(gLevels))
                    table$addColumn(name = gLevels[i], type=cellType, format=cellFormat, superTitle=groupVar)
            }
            table$addColumn(name = "Total", title=.("Overall"), type=cellType, format=cellFormat, visible=self$options$overall)

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
                if (length(self$options$resps) < 1 || is.null(self$options$group)) {
                    return()
                } else {
                    nGroups <- nlevels(self$data[,self$options$group])
                    nAnswers <- length(self$options$resps)
                    crosstab <- private$.crossTab(self$data, self$options$resps, self$options$group,
                                                  self$options$endorsed, self$options$order, self$options$computedValues)
                }
            } else { # One Multiple Value Variables
                if (is.null(self$options$repVar) || self$options$separator == '' || is.null(self$options$group2)) {
                    return()
                } else {
                    nGroups <- nlevels(self$data[,self$options$group2])
                    rawData <- self$data[[self$options$repVar]]
                    oneHotData <- private$.oneHotEncoding(rawData, self$options$separator, self$options$emptyAsNA)
                    answerList <- names(oneHotData)
                    nAnswers <- length(answerList)
                    oneHotData[, self$options$group2] <- self$data[,self$options$group2]
                    crosstab <- private$.crossTab(oneHotData, answerList, self$options$group2,
                                                          1, self$options$order, self$options$computedValues)
                }
            }

            table <- self$results$crosstab
            # Filling the table
            for(i in 1:(nrow(crosstab)-2))
                table$setRow(rowKey=i,
                             values = c(var=rownames(crosstab)[i], crosstab[i,]))
            if ( self$options$totalRow ) {
                table$setRow(rowKey='.total', values = c(var="Total", crosstab[i+1,]))
            }
            if ( self$options$showNbOfCases && (self$options$computedValues == "count" || self$options$computedValues == "options") ) {
                table$setRow(rowKey='.nbofcases', values = append(list(var=.("Number of cases")), crosstab[i+2,]))
            }
            image <- self$results$plot
            image$setState(crosstab[1:nAnswers,1:nGroups])
        },

        .plot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state))
                return(FALSE)

            if (self$options$mode == "morevar") {
                groupVar <- self$options$group
                optionName <- self$options$optionname
            } else {
                groupVar <- self$options$group2
                optionName <- self$options$repVar
            }

            #self$results$text$setContent(image$state)
            #return(FALSE)

            # Data
            plotData <- cbind("Options" = factor(rownames(image$state), levels=rownames(image$state)),image$state)
            plotData <- pivot_longer(plotData, cols=colnames(image$state), names_to = groupVar, values_to = "Count")
            plotData[[groupVar]] <- factor(plotData[[groupVar]], levels = names(image$state) )
            # Plot
            optionsVar <- "Options"
            if (self$options$xaxis == "xcols") {
                xVarName <- ensym(groupVar)
                zVarName <- ensym(optionsVar)
                plot <- ggplot(plotData, aes(x=!!xVarName, y=Count)) +
                            geom_col( aes(fill=!!zVarName), position = self$options$bartype)
                xLab <- groupVar
                gLab <- optionName
            } else {
                xVarName <- ensym(optionsVar)
                zVarName <- ensym(groupVar)
                plot <- ggplot(plotData, aes(x=!!xVarName, y=Count)) +
                            geom_col( aes(fill=!!zVarName), position = self$options$bartype)
                xLab <- optionName
                gLab <- groupVar
            }

            # Theme and colors
            plot <- plot + ggtheme + vijScale(self$options$colorPalette, "fill")

            # Y scale and lab
            if (self$options$computedValues == "responses") {
                plot <- plot + scale_y_continuous(labels=percent_format())
                yLab <- .("% of Responses")
                yScaleFactor <- 100 # yScaleFactor is used for manual range computation (1 = count, 100 = percent)
            } else if (self$options$computedValues == "cases") {
                plot <- plot + scale_y_continuous(labels=percent_format())
                yLab <- .("% of Cases")
                yScaleFactor <- 100
            } else if (self$options$computedValues == "options") {
                plot <- plot + scale_y_continuous(labels=percent_format())
                yLab <- paste(.("% within"), optionName)
                yScaleFactor <- 100
            } else {
                yLab <- .("Count")
                yScaleFactor <- 1
            }

            # Axis range
            if (self$options$yAxisRangeType == "manual") { # Horizontal and manual
                plot <- plot + coord_cartesian(ylim = c(self$options$yAxisRangeMin/yScaleFactor, self$options$yAxisRangeMax/yScaleFactor))
            }

            # Titles & Labels
            defaults <- list(y = yLab, x = xLab, legend = gLab)
            plot <- plot + vijTitlesAndLabels(self$options, defaults) + vijTitleAndLabelFormat(self$options)
            plot <- plot + theme(legend.key.spacing.y = unit(1, "mm"), legend.byrow = TRUE)

            return(plot)

        },

        .crossTab = function (data, items = NULL, group = NULL, endorsedOption = 1, order='none', values='count') {
            options = data[, items]
            groups = list(data[,group])
            # CrossTab
            crossTab <- aggregate(options == endorsedOption, groups, sum, na.rm = TRUE)
            # Save group names
            groupNames <- crossTab[,1]
            # Transpose the content of the table
            crossTab <- as.data.frame(t(crossTab[,-1]))
            # Rename the columns of the table
            names(crossTab) <- groupNames
            # Compute the number of cases by group
            if (length(items) > 1)
                NbOfCases <- aggregate( !apply(apply(options, 1, is.na), 2, all), groups, sum )
            else
                NbOfCases <- aggregate( !is.na(options), groups, sum )
            NbOfCases <- c( NbOfCases[,2], sum(NbOfCases[,2]))
            # Add the margin column to crosstab
            crossTab <- cbind(crossTab, "Total" = rowSums(crossTab))
            # Sorting crosstab
            if (order == 'decreasing') {
                crossTab<-crossTab[order(crossTab$Total, decreasing = TRUE),]
            } else if (order == 'increasing') {
                crossTab<-crossTab[order(crossTab$Total, decreasing = FALSE),]
            }
            # add margin row to  crosstab
            NbOfResps <- colSums(crossTab)
            crossTab <- rbind(crossTab, "Total" = NbOfResps)
            #
            if (values == 'cases') {
                crossTab <- sweep( crossTab , 2, NbOfCases, "/")
                crossTab <- rbind(crossTab, "Nb of Cases"=NbOfCases)
            } else if (values == 'responses' ) {
                crossTab <- sweep( crossTab , 2, NbOfResps, "/")
                crossTab <- rbind(crossTab, "Nb of Cases"=NbOfCases)
            } else if (values == 'options' ) {
                crossTab <- rbind(crossTab, "Nb of Cases"=NbOfCases)
                crossTab <- sweep( crossTab , 1, rowSums(crossTab)/2, "/")
            } else {
                crossTab <- rbind(crossTab, "Nb of Cases"=NbOfCases)
            }
            # Return result
            return(crossTab)
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
