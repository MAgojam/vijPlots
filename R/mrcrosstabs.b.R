mrcrosstabsClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "mrcrosstabsClass",
    inherit = mrcrosstabsBase,
    private = list(
        .init = function() {
            table <- self$results$crosstab
            # Table title
            if ( self$options$computedValues == "options" ) {
                table$setTitle(.("Crosstab (% by row)"))
            } else if ( self$options$computedValues == "cases" ) {
                table$setTitle(.("Crosstab (% of Cases)"))
            } else if ( self$options$computedValues == "responses" ) {
                table$setTitle(.("Crosstab (% of Responses)"))
            }
            # Table rows
            for (i in seq_along(self$options$resps))
                table$addRow(rowKey = i)
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
            table$getColumn('var')$setTitle(self$options$optionname)
            # Cell Type
            if ( self$options$computedValues == "count" ) {
                cellType = "integer"
                cellFormat = ""
            } else {
                cellType = "number"
                cellFormat = "pc"
            }
            # Columns
            if ( ! is.null(self$options$group) ) {
                groups <- self$data[,self$options$group]
                for(i in 1:nlevels(groups))
                    table$addColumn(name = levels(groups)[i], type=cellType, format=cellFormat, superTitle=self$options$group)
            }
            table$addColumn(name = "Total", title=.("Overall"), type=cellType, format=cellFormat, visible=self$options$overall)

            # Set the size of the plot
            image <- self$results$plot
            size <- self$options$size
            if ( size == "small" )
                image$setSize(300, 200)
            else if ( size == "medium" )
                image$setSize(400,300)
            else if ( size == "large" )
                image$setSize(600,400)
            else if ( size == "huge" )
                image$setSize(800,500)
        },

        .run = function() {
            table <- self$results$crosstab
            # Exceptions
            if ( length(self$options$resps) < 1 )
                return()
            if ( length(self$options$resps) == 1 || is.null(self$options$group)) {
                for(i in 1:length(self$options$resps)) {
                    #table$setCell(rowKey = self$options$resps[i] ,1,self$options$resps[i])
                    table$setCell(rowNo = i, 1, self$options$resps[i])
                }
                return()
            }
            # Computation
            groups <- self$data[,self$options$group]
            crosstab <- private$.crossTab(self$data, self$options$resps, self$options$group,
                                          self$options$endorsed, self$options$order, self$options$computedValues)
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
            image$setState(crosstab[1:length(self$options$resps),1:nlevels(groups)])
        },

        .plot = function(image, ggtheme, theme, ...) {  # <-- the plot function
            if (is.null(image$state))
                return(FALSE)

            # Data
            plotData <- cbind("Options" = factor(rownames(image$state), levels=rownames(image$state)),image$state)
            plotData <- pivot_longer(plotData, cols=colnames(image$state), names_to = self$options$group, values_to = "Count")
            plotData[[self$options$group]] <- factor(plotData[[self$options$group]], levels = names(image$state) )
            # Plot
            optionsVar <- "Options"
            groupVar <- self$options$group
            if (self$options$xaxis == "xcols") {
                xVarName <- ensym(groupVar)
                zVarName <- ensym(optionsVar)
                plot <- ggplot(plotData, aes(x=!!xVarName, y=Count)) +
                            geom_col( aes(fill=!!zVarName), position = self$options$bartype)
                xLab <- groupVar
                gLab <- self$options$optionname
            } else {
                xVarName <- ensym(optionsVar)
                zVarName <- ensym(groupVar)
                plot <- ggplot(plotData, aes(x=!!xVarName, y=Count)) +
                            geom_col( aes(fill=!!zVarName), position = self$options$bartype)
                xLab <- self$options$optionname
                gLab <- groupVar
            }

            # Theme and colors
            plot <- plot + ggtheme + vijScale(self$options$colorPalette, "fill")

            # Y scale and lab
            if (self$options$computedValues == "responses") {
                plot <- plot + scale_y_continuous(labels=percent_format())
                yLab <- .("% of Responses")
            } else if (self$options$computedValues == "cases") {
                plot <- plot + scale_y_continuous(labels=percent_format())
                yLab <- .("% of Cases")
            } else if (self$options$computedValues == "options") {
                plot <- plot + scale_y_continuous(labels=percent_format())
                yLab <- paste(.("% within"), self$options$optionname)
            } else {
                yLab <- .("Count")
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
            NbOfCases <- aggregate( !apply(apply(options, 1, is.na), 2, all), groups, sum )
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
        }
    )
)
