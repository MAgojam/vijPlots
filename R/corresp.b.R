
# This file is a generated template, your changes will not be overwritten

correspClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "correspClass",
    inherit = correspBase,
    #### Active bindings ---- from jmv/conttables.b.R
    active = list(
        countsName = function() {
            if ( ! is.null(self$options$counts)) {
                return(self$options$counts)
            } else if ( ! is.null(attr(self$data, "jmv-weights-name"))) {
                return (attr(self$data, "jmv-weights-name"))
            }
            NULL
        }
    ),
    private = list(
        .varName = list(),
        .setVarNames = function(vars) {
            if (self$options$descAsVarName) {
                for (aVar in vars) {
                    aVarName <- attr(self$data[[aVar]], "jmv-desc", TRUE)
                    if (!is.null(aVarName))
                        private$.varName[[aVar]] <- aVarName
                    else
                        private$.varName[[aVar]] <- aVar
                }
            } else {
                private$.varName[vars] <- vars
            }
        },
        .getData = function() {
            rowVarName <- self$options$rows
            colVarName <- self$options$cols
            if (is.null(rowVarName) || is.null(colVarName))
                return(NULL)
            data <- private$.cleanData()
            data <- jmvcore::naOmit(data[,c(rowVarName,colVarName,'.COUNTS')])
            return(data)
        },
        .getProfile = function(contingencyTable, supplementaryRows, supplementaryCols) {
            # This function is tricky because of supplementaryPoints. Maybe it's possible to simplify it.
            # The idea is to build the contingencyTable of active columns and rows without deleting the supplementary ones
            # but setting them to 0 then compute the % for the supplementary rows based on active margins
            # then adds then back to the main table...
            rowProfiles <- contingencyTable                     # copy contingencyTable
            rowProfiles[supplementaryRows,] <- 0                # set supplementary rows to 0
            rowProfiles <- addmargins(rowProfiles, margin=1)    # Add margin row (sum)
            rowProfiles[supplementaryRows,] <-contingencyTable[supplementaryRows,]  # set supplementary rows back
            rowProfiles[,supplementaryCols] <- 0                # Empty supplementary columns
            # Compute margin
            tmpRowProfiles <- addmargins(rowProfiles, margin=2)
            rowMargins <- tmpRowProfiles[-nrow(tmpRowProfiles),ncol(tmpRowProfiles)]
            rowMargins[supplementaryRows]<-0
            rowMargins <- addmargins(as.matrix(rowMargins), margin = 1)
            #
            rowProfiles <- proportions(rowProfiles, margin = 1)             # Compute % per lines
            rowProfiles <- addmargins(rowProfiles,margin=2)                 # Add margin column (sum)
            supplCols <- as.matrix(contingencyTable[,supplementaryCols])    # Table of supplementary Cols
            supplCols[supplementaryRows,] <- 0                              # set supplementary rows to 0
            supplCols <- addmargins(supplCols, margin = 1)                  # add margin
            supplCols <-supplCols / rowMargins[,1]                          # compute % per lines
            supplCols[supplementaryRows,] <- 0                              # Remplace NaN by 0
            rowProfiles[,supplementaryCols] <- supplCols                    # Replace supplementary cols in row profiles table
            #
            rownames(rowProfiles)[nrow(rowProfiles)] <- .("Mass")
            colnames(rowProfiles)[ncol(rowProfiles)] <- .("Active Margin")
            return(rowProfiles)
        },
        .getContingencyTable = function(contingencyTable, supplementaryRows, supplementaryCols) {
            savedSupplementaryRows <- contingencyTable[supplementaryRows,]
            savedSupplementaryCols <- contingencyTable[,supplementaryCols]
            contingencyTable[supplementaryRows,] <- 0                # set supplementary rows to 0
            contingencyTable[,supplementaryCols] <- 0                # Empty supplementary columns
            contingencyTable <- addmargins(contingencyTable, margin=c(1,2))    # Add margin row (sum)
            # Set the supplementary rows and columns back
            contingencyTable[supplementaryRows,1:(ncol(contingencyTable)-1)] <- savedSupplementaryRows
            contingencyTable[1:(nrow(contingencyTable)-1),supplementaryCols] <- savedSupplementaryCols
            # Delete values and margins for supplementary rows/columns
            for (i in supplementaryRows) {
                for (j in supplementaryCols) {
                    contingencyTable[i,j] <- NA
                }
            }
            contingencyTable[supplementaryRows,ncol(contingencyTable)] <- NA
            contingencyTable[nrow(contingencyTable), supplementaryCols] <- NA
            return(contingencyTable)
        },
        # Modified from summary.ca(): Summarizing ca objects (ca package 0.70)
        .caExtra = function(object, nd = 2){
            obj <- object
            # principal coordinates:
            K   <- nd
            I   <- dim(obj$rowcoord)[1] ; J <- dim(obj$colcoord)[1]
            svF <- matrix(rep(obj$sv[1:K], I), I, K, byrow = TRUE)
            svG <- matrix(rep(obj$sv[1:K], J), J, K, byrow = TRUE)
            rpc <- obj$rowcoord[,1:K] * svF
            cpc <- obj$colcoord[,1:K] * svG
            # rows:
            r.names <- obj$rownames
            rpc0 <- ca::cacoord(obj, type = "principal", rows = TRUE)
            r.ctr <- matrix(NA, nrow = length(r.names), ncol = nd, dimnames=list(r.names,1:nd))
            for (i in 1:nd){
                r.ctr[,i] <- obj$rowmass * rpc[,i]^2 /obj$sv[i]^2
            }
            r.co2 <- matrix(NA, nrow = length(r.names), ncol = nd, dimnames=list(r.names,1:nd))
            for (i in 1:nd){
                r.co2[,i] <- rpc0[,i]^2 / apply(rpc0^2, 1, sum)
            }
            r.qlt <- data.frame(qlt = rowSums(r.co2))
            r.out <- list(qlt = r.qlt, ctr = r.ctr, co2 = r.co2)
            # columns:
            c.names <- obj$colnames
            cpc0 <- ca::cacoord(obj, type = "principal", cols = TRUE)
            c.ctr <- matrix(NA, nrow = length(c.names), ncol = nd, dimnames=list(c.names,1:nd))
            for (i in 1:nd){
                c.ctr[,i] <- obj$colmass * cpc[,i]^2 /  obj$sv[i]^2
            }
            c.co2 <- matrix(NA, nrow = length(c.names), ncol = nd, dimnames=list(c.names,1:nd))
            for (i in 1:nd){
                c.co2[,i] <- cpc0[,i]^2 / apply(cpc0^2, 1, sum)
            }
            c.qlt <- data.frame(qlt = rowSums(c.co2))
            c.out <- list(qlt = c.qlt, ctr = c.ctr, co2 = c.co2)
            #	output:
            out <- list(rows  = r.out, cols = c.out)
            return(out)
        },
        .init = function() {
            # Weight message
            countsName <- self$countsName
            if ( ! is.null(countsName)) {
                message <- ..('The data is weighted by the variable {}.', countsName)
                type <- NoticeType$WARNING
                weightsNotice <- jmvcore::Notice$new(
                    self$options,
                    name='.weights',
                    type=type,
                    content=message)
                self$results$insert(1, weightsNotice)
            }
            #
            if (is.null(self$options$rows) || is.null(self$options$cols)) {
                self$results$contingency$setVisible(FALSE)
                self$results$rowProfiles$setVisible(FALSE)
                self$results$colProfiles$setVisible(FALSE)
                self$results$eigenvalues$setVisible(FALSE)
                self$results$rowSummary$setVisible(FALSE)
                self$results$colSummary$setVisible(FALSE)
                self$results$rowplot$setVisible(FALSE)
                self$results$colplot$setVisible(FALSE)
                self$results$biplot$setVisible(FALSE)
                self$results$helpMessage$setVisible(TRUE)
                private$.showHelp()
            } else {
                self$results$helpMessage$setVisible(FALSE)
            }
            # Set the size of the plot
            userWidth <- as.numeric(self$options$plotWidth)
            userHeight <- as.numeric(self$options$plotHeight)
            if (userWidth == 0)
                userWidth <- 600
            if (userHeight == 0)
                userHeight <- 600
            image <- self$results$rowplot
            image$setSize(userWidth, userHeight)
            image <- self$results$colplot
            image$setSize(userWidth, userHeight)
            image <- self$results$biplot
            image$setSize(userWidth, userHeight)
        },
        .run = function() {
            data <- private$.getData()
            if (is.null(data)) {
                self$results$contingency$addColumn(".", type="text")
                self$results$rowProfiles$addColumn(".", type="text")
                self$results$colProfiles$addColumn(".", type="text")
                self$results$rowSummary$addColumn(".", type="text")
                self$results$colSummary$addColumn(".", type="text")
                self$results$eigenvalues$addRow(".")
                self$results$eigenvalues$setNote("chisq", NULL)
                return()
            }

            rowVarName <- self$options$rows
            colVarName <- self$options$cols
            countsVarName <- self$countsName

            # Set variable names
            private$.setVarNames(c(rowVarName, colVarName))

            #### Contingency Table (base) ####

            if (!is.null(countsVarName)) {
                formula <- jmvcore::composeFormula('.COUNTS', c(rowVarName, colVarName))
                contingencyTable <- xtabs(formula, data)
            } else {
                contingencyTable <- table(self$data[[rowVarName]], self$data[[colVarName]])
            }

            #### Supplementary Rows & Column ####

            # Rows
            if (is.null(self$options$supplementaryRows) || self$options$supplementaryRows == "0" || self$options$supplementaryRows == "") {
                supplementaryRows <- NULL
            } else {
                supplementaryRows <- sort(unique(as.numeric(unlist(strsplit(self$options$supplementaryRows,",")))))
                if (any(is.na(supplementaryRows))) {
                    reject("Supplementary row numbers must be a list of numbers, e.g. 1,2,9")
                } else {
                    nmax <- nlevels(self$data[[rowVarName]])
                    if (!all(supplementaryRows %in% 1:nmax))
                        reject("Supplementary row numbers must be between 1 and {nmax}", code=NULL, nmax=nmax)
                }
            }
            # Columns
            if (is.null(self$options$supplementaryCols) || self$options$supplementaryCols == "0" || self$options$supplementaryCols == "") {
                supplementaryCols <- NULL
            } else {
                supplementaryCols <- sort(unique(as.numeric(unlist(strsplit(self$options$supplementaryCols,",")))))
                if (any(is.na(supplementaryCols))) {
                    reject("Supplementary column numbers must be a list of numbers, e.g. 1,2,9")
                } else {
                    nmax <- nlevels(self$data[[colVarName]])
                    if (!all(supplementaryCols %in% 1:nmax))
                        reject("Supplementary column numbers must be between 1 and {nmax}", code=NULL, nmax=nmax)
                }
            }
            # Modify the supplementary row/col names
            for (i in supplementaryRows)
                rownames(contingencyTable)[i] <- paste(rownames(contingencyTable)[i], "*")
            for (j in supplementaryCols)
                colnames(contingencyTable)[j] <- paste(colnames(contingencyTable)[j], "*")

            #### Dimensions and axes  ####

            # Solution dimension
            maxDim = min(nrow(contingencyTable)-length(supplementaryRows), ncol(contingencyTable)-length(supplementaryCols)) - 1
            nDim <-self$options$dimNum
            if (nDim > maxDim)
                reject("Number of dimensions must be less than or equal to {maxDim}", code=NULL, maxDim = maxDim)
            # Axis
            xaxis <- self$options$xaxis
            yaxis <- self$options$yaxis
            if (xaxis > nDim || yaxis > nDim)
                reject("Axis numbers must be less than or equal to the number of dimensions ({nDim})", code=NULL, nDim=nDim)
            if (xaxis == yaxis)
                reject("Axis numbers cannot be equal!")

            #### Normalisation ####

            normalizationString <- private$.normalizationTitle(self$options$normalization)

            #### Contingency Table (with supplementary rows/columns ####

            fullTable <- private$.getContingencyTable(contingencyTable, supplementaryRows, supplementaryCols)
            rownames(fullTable)[nrow(fullTable)] <- .("Active Margin")
            colnames(fullTable)[length(colnames(fullTable))] <- .("Active Margin")
            self$results$contingency$addColumn(rowVarName, type="text", title = private$.varName[[rowVarName]])
            for (col in colnames(fullTable)) {
                if (col != .("Active Margin"))
                    self$results$contingency$addColumn(col, type="integer", superTitle = private$.varName[[colVarName]])
                else
                    self$results$contingency$addColumn(col, type="integer")
            }
            for (i in seq(nrow(fullTable))) {
                self$results$contingency$addRow(i, values = fullTable[i,])
                self$results$contingency$setCell(rowNo = i, rowVarName, rownames(fullTable)[i])
            }
            self$results$contingency$addFormat(rowNo = nrow(fullTable), 1, Cell.BEGIN_END_GROUP)
            # Change NaN/NA to NULL. Is there another way to have empty cells ?
            for (i in seq(nrow(fullTable))) {
                for (j in seq(ncol(fullTable))) {
                    if (is.na(fullTable[i,j]))
                        self$results$contingency$setCell(rowNo = i, colnames(fullTable)[j], NULL)
                }
            }
            if (!is.null(supplementaryRows) || !is.null(supplementaryCols))
                self$results$contingency$setNote("supp",.("* : Supplementary rows/columns"))

            #### Row and Column Profile Tables ####

            if(self$options$showProfiles) {
                # Row Profiles
                rowProfiles <- private$.getProfile(contingencyTable, supplementaryRows, supplementaryCols)
                self$results$rowProfiles$addColumn(rowVarName, type = "text", title = private$.varName[[rowVarName]])
                for (j in seq(ncol(rowProfiles))) {
                    self$results$rowProfiles$addColumn(colnames(rowProfiles)[j], type = "number", format = "zto", superTitle = private$.varName[[colVarName]])
                }
                for (i in seq(nrow(rowProfiles))) {
                    self$results$rowProfiles$addRow(i, values = rowProfiles[i,])
                    self$results$rowProfiles$setCell(rowNo = i, rowVarName, rownames(rowProfiles)[i])
                }
                self$results$rowProfiles$addFormat(rowNo = nrow(rowProfiles), 1, Cell.BEGIN_END_GROUP)
                if (!is.null(supplementaryRows) || !is.null(supplementaryCols))
                    self$results$rowProfiles$setNote("supp",.("* : Supplementary rows/columns"))
                # Column Profiles
                colProfiles <- t(private$.getProfile(t(contingencyTable),supplementaryCols, supplementaryRows))
                self$results$colProfiles$addColumn(rowVarName, type = "text", title = private$.varName[[rowVarName]])
                for (j in seq(ncol(colProfiles))) {
                    self$results$colProfiles$addColumn(colnames(colProfiles)[j], type = "number", format = "zto", superTitle = private$.varName[[colVarName]])
                }
                for (i in seq(nrow(colProfiles))) {
                    self$results$colProfiles$addRow(i, values = colProfiles[i,])
                    self$results$colProfiles$setCell(rowNo = i, rowVarName, rownames(colProfiles)[i])
                }
                self$results$colProfiles$addFormat(rowNo = nrow(colProfiles), 1, Cell.BEGIN_END_GROUP)
                if (!is.null(supplementaryRows) || !is.null(supplementaryCols))
                    self$results$colProfiles$setNote("supp",.("* : Supplementary rows/columns"))
            }

            #### Chi-Squared test ####

            activeContingencyTable <- contingencyTable
            if (!is.null(supplementaryRows))
                activeContingencyTable <- activeContingencyTable[-supplementaryRows,]
            if (!is.null(supplementaryCols))
                activeContingencyTable <- activeContingencyTable[,-supplementaryCols]
            chisqres <- chisq.test(activeContingencyTable)
            if (round(chisqres$statistic,2) == 0)
                return()

            #### Compute CA ####
            if (is.null(supplementaryRows))
                suprow <- NA
            else
                suprow <- supplementaryRows
            if (is.null(supplementaryCols))
                supcol <- NA
            else
                supcol <- supplementaryCols
            results <- ca::ca(contingencyTable, suprow = suprow, supcol = supcol)
            results_dim <- private$.caExtra(results, nd = nDim)
            results_coord <- ca::cacoord(results, type = self$options$normalization)

            #### Inertia Table ####

            singular <- results$sv
            inertia <- results$sv**2
            totalInertia <- sum(results$sv**2)
            percentInertia <- inertia / totalInertia
            cumulativeInertia <- 0
            # Populate the inertia table
            for (i in seq_along(singular)) {
                cumulativeInertia <- cumulativeInertia + percentInertia[i]
                self$results$eigenvalues$addRow(i, values = list(
                    dim = i,
                    singular = singular[i],
                    inertia = inertia [i],
                    proportion = percentInertia[i],
                    cumulative = cumulativeInertia
                ))
            }
            # Add total row
            self$results$eigenvalues$addRow(rowKey="Total", values = list(
                dim = "Total",
                singular = "",
                inertia = totalInertia,
                proportion = 1,
                cumulative = 1
            ))
            self$results$eigenvalues$addFormat(rowKey="Total", 1, Cell.BEGIN_END_GROUP)
            # Chi-squared test
            self$results$eigenvalues$setNote("chisq",
                                             paste0("X-squared = ", round(chisqres$statistic,2), ", df = ", chisqres$parameter, ",
                               p-value = ",format.pval(chisqres$p.value, eps = 0.001)),
                                             init = FALSE)

            #### Summary Tables ####

            if(self$options$showSummaries) {
                # Row Summary Table
                self$results$rowSummary$addColumn(name = "id", title = "#", type = "integer")
                self$results$rowSummary$addColumn(name = "row", title = private$.varName[[rowVarName]], type = "text")
                self$results$rowSummary$addColumn(name = "margin", title = "Mass", type = "number", format = "zto")
                for (i in seq(nDim))
                    self$results$rowSummary$addColumn(name = paste0("score",i), title = paste("Dim",i), superTitle = .("Coordinates†"), type = "number", format = "zto")
                self$results$rowSummary$addColumn(name = "inertia", title = "Inertia", type = "number", format = "zto")
                for (i in seq(nDim))
                    self$results$rowSummary$addColumn(name = paste0("contrib",i), title = paste("Dim",i), superTitle = "Contributions", type = "number", format = "zto")
                self$results$rowSummary$addColumn(name = "qlt", title = "QLT", type = "number", format = "zto")
                for (i in seq(nDim))
                    self$results$rowSummary$addColumn(name = paste0("cos",i), title = paste("Dim",i), superTitle = "CO2", type = "number", format = "zto")
                # Populate Row Summary
                for (i in seq(nrow(contingencyTable))) {
                    theValues = list(
                        id = i,
                        row = results$rownames[i],
                        margin = if(is.na(results$rowmass[i])) NULL else results$rowmass[i],
                        inertia = if(is.na(results$rowinertia[i])) NULL else results$rowinertia[i],
                        qlt = results_dim$rows$qlt[i,])
                    for (j in seq(nDim)) {
                        theValues[[paste0("score",j)]] <- results_coord$rows[i,j]
                        theValues[[paste0("contrib",j)]] <- if(is.na(results_dim$rows$ctr[i,j])) NULL else results_dim$rows$ctr[i,j]
                        theValues[[paste0("cos",j)]] <- results_dim$rows$co2[i,j]
                    }
                    self$results$rowSummary$addRow(i, values = theValues)
                }
                if (!is.null(supplementaryRows))
                    self$results$rowSummary$setNote("supp",.("* : Supplementary rows"))
                self$results$rowSummary$setNote("norm", paste("† :", normalizationString))

                # Column Summary Table
                self$results$colSummary$addColumn(name = "id", title = "#", type = "integer")
                self$results$colSummary$addColumn(name = "col", title = private$.varName[[colVarName]], type = "text")
                self$results$colSummary$addColumn(name = "margin", title = "Mass", type = "number", format = "zto")
                for (i in seq_len(nDim))
                    self$results$colSummary$addColumn(name = paste0("score",i), title = paste("Dim",i), superTitle = .("Coordinates†"), type = "number", format = "zto")
                self$results$colSummary$addColumn(name = "inertia", title = "Inertia", type = "number", format = "zto")
                for (i in seq_len(nDim))
                    self$results$colSummary$addColumn(name = paste0("contrib",i), title = paste("Dim",i), superTitle = "Contributions", type = "number", format = "zto")
                self$results$colSummary$addColumn(name = "qlt", title = "QLT", type = "number", format = "zto")
                for (i in seq_len(nDim))
                    self$results$colSummary$addColumn(name = paste0("cos",i), title = paste("Dim",i), superTitle = "CO2", type = "number", format = "zto")
                # Populate Col Summary
                for (i in seq(ncol(contingencyTable))) {
                    theValues = list(
                        id = i,
                        col = results$colnames[i],
                        margin = if(is.na(results$colmass[i])) NULL else results$colmass[i],
                        inertia = if(is.na(results$colinertia[i])) NULL else results$colinertia[i],
                        qlt = results_dim$cols$qlt[i,])
                    for (j in seq(nDim)) {
                        theValues[[paste0("score",j)]] <- results_coord$columns[i,j]
                        theValues[[paste0("contrib",j)]] <- if(is.na(results_dim$cols$ctr[i,j])) NULL else results_dim$cols$ctr[i,j]
                        theValues[[paste0("cos",j)]] <- results_dim$cols$co2[i,j]
                    }
                    self$results$colSummary$addRow(i, values = theValues)
                }
                if (!is.null(supplementaryCols))
                    self$results$colSummary$setNote("supp",.("* : Supplementary columns"))
                self$results$colSummary$setNote("norm", paste("† :", normalizationString))
            }
            if (length(singular) < 2)
                return()
            if (singular[2] < .Machine$double.eps)
                return()

            # Plots
            rowplot <- self$results$rowplot
            rowplot$setState(results)
            colplot <- self$results$colplot
            colplot$setState(results)
            biplot <- self$results$biplot
            biplot$setState(results)
        },
        .caplot = function(plotType, image, ggtheme, theme) {
            if (is.null(image$state))
                return(FALSE)

            # Plot data
            results <- image$state
            coord <- ca::cacoord(results, type = self$options$normalization)
            # Supplementary Row & Column Colors
            # 1 = row, 2 = rowsup, 3 = column, 4 = colsup
            if (plotType != 'column') { # rowplat and biplot
                coord$rows <- cbind(coord$rows, "sup"=1)
                coord$rows[results$rowsup,"sup"] <- 2
                ptcoord <- as.data.frame(coord$rows)
            } else {
                ptcoord <- NA
            }
            if (plotType != 'row') { # colplot and biplot
                coord$columns <- cbind(coord$columns, "sup"=3)
                coord$columns[results$colsup,"sup"] <- 4
                ptcoord <- as.data.frame(rbind(ptcoord,coord$columns))
            }
            ptcoord$sup <- factor(ptcoord$sup, levels = c(1,2,3,4))
            # ptcoord dataframe containt the row and column coordinates
            # ptcoord$sup is the type of point (1 = row, 2 = rowsup, 3 = column, 4 = colsup)

            # Plot inertia
            singular <- results$sv
            inertia <- results$sv**2
            totalInertia <- sum(results$sv**2)
            percentInertia <- round(100*inertia / totalInertia, 1)
            # Plot axis
            xaxis <- self$options$xaxis
            xaxisdim <- paste0("Dim", xaxis)
            xaxisstr <- paste0("Dimension ",xaxis, " (", percentInertia[xaxis], "%)")
            yaxis <- self$options$yaxis
            yaxisdim <- paste0("Dim", yaxis)
            yaxisstr <- paste0("Dimension ",yaxis, " (", percentInertia[yaxis], "%)")

            # Building the plot
            plot <-  ggplot(ptcoord, aes(x = ptcoord[,xaxisdim], y = ptcoord[,yaxisdim], color = ptcoord$sup, shape = ptcoord$sup))
            plot <- plot + geom_point()
            plot <- plot + ggrepel::geom_text_repel(aes(label = rownames(ptcoord)), show.legend = FALSE)
            plot <- plot + geom_hline(yintercept = 0, linetype = 2) + geom_vline(xintercept = 0, linetype = 2)

            # Apply jmv theme
            plot <- plot + ggtheme

            # Axes
            plot <- plot + labs(x = xaxisstr, y = yaxisstr)
            if (self$options$fixedRatio)
                plot <- plot + coord_fixed()

            # Set point colors
            plot <- plot +
                scale_color_manual(
                    values=c("1" = self$options$rowColor, "2" = self$options$supColor, "3" = self$options$colColor, "4" = self$options$supColor),
                    breaks=c("1", "3", "2", "4"),
                    labels = c(self$options$rows, self$options$cols, .("Suppl. Row"), .("Suppl. Column"))) + labs(color = "") +
                scale_shape_manual(values = c(19, 19, 17, 17), breaks = c("1","2","3","4")) +
                theme(legend.text = element_text(size=10))
            plot <- plot + guides(color = "none", shape = "none")

            # Plot frame
            plot <- plot + theme(axis.line = element_line(linewidth = 0), panel.border = element_rect(color = "black", fill = NA, size = 1))

            # Plot title
            title <- self$options$get( paste0(plotType,"Title") )
            defaultTitle <- switch(plotType,
                                   row = paste(.("Row Points for"), private$.varName[[self$options$rows]]),
                                   column = paste(.("Column Points for"), private$.varName[[self$options$cols]]),
                                   biplot = jmvcore::format(.("Row and Column Points for {rows} and {cols}"),
                                                            rows = private$.varName[[self$options$rows]],
                                                            cols = private$.varName[[self$options$cols]])
            )
            if (!(title %in% c("", " "))) {
                if (title == "default")
                    title <- defaultTitle
                plot <- plot + labs(title = title)
                plot <- plot + theme(plot.title = element_text(
                    size = self$options$titleFontSize,
                    face = self$options$titleFontFace,
                    hjust = as.numeric(self$options$titleAlign)))
            }
            # Plot subtitle
            subtitle <- self$options$get( paste0(plotType,"Subtitle") )
            defaultSubtitle <- private$.normalizationTitle(self$options$normalization)
            if (!(subtitle %in% c("", " "))) {
                if (subtitle == "default")
                    subtitle <- defaultSubtitle
                plot <- plot + labs(subtitle = subtitle)
                plot <- plot + theme(plot.subtitle = element_text(
                    size = self$options$subtitleFontSize,
                    face = self$options$subtitleFontFace,
                    hjust = as.numeric(self$options$subtitleAlign),
                    margin = margin(-5, 0, 15, 0)))
            }

            return(plot)
        },
        .rowplot = function(image, ggtheme, theme, ...) {
            return(private$.caplot(plotType = 'row', image, ggtheme, theme))
        },
        .colplot = function(image, ggtheme, theme, ...) {
            return(private$.caplot(plotType = 'column', image, ggtheme, theme))
        },
        .biplot = function(image, ggtheme, theme, ...) {
            return(private$.caplot(plotType = 'biplot', image, ggtheme, theme))
        },

        #### Helper functions ---- modified from jmv/conttables.b.R
        .cleanData = function(B64 = FALSE) {

            data <- self$data

            rowVarName <- self$options$rows
            colVarName <- self$options$cols
            countsName <- self$options$counts

            columns <- list()

            if ( ! is.null(rowVarName)) {
                columns[[rowVarName]] <- as.factor(data[[rowVarName]])
            }
            if ( ! is.null(colVarName)) {
                columns[[colVarName]] <- as.factor(data[[colVarName]])
            }

            if ( ! is.null(countsName)) {
                columns[['.COUNTS']] <- jmvcore::toNumeric(data[[countsName]])
            } else if ( ! is.null(attr(data, "jmv-weights"))) {
                columns[['.COUNTS']] <- jmvcore::toNumeric(attr(data, "jmv-weights"))
            } else {
                columns[['.COUNTS']] <- as.integer(rep(1, nrow(data)))
            }

            if (B64)
                names(columns) <- jmvcore::toB64(names(columns))

            attr(columns, 'row.names') <- paste(seq_len(length(columns[[1]])))
            class(columns) <- 'data.frame'

            columns
        },
        .normalizationTitle = function(type){
            normalizationString <- switch(type,
                                      principal = .("Principal"),
                                      symbiplot = .("Symetric"),
                                      rowprincipal = .("Row Principal"),
                                      colprincipal = .("Column Principal"),
                                      standard = .("Standard")
                                )
            normalizationTitle <- jmvcore::format(.("{normalization} normalization"), normalization = normalizationString)
            return(normalizationTitle)
        },
        .showHelp = function(){
            self$results$helpMessage$setContent('
            	<style>
					.block {
  						border: 2px solid gray;
  						border-radius: 15px;
  						background-color: WhiteSmoke;
  						padding: 0px 20px;
  						text-align: justify;
					}
				</style>
            <div class="block">
            <p><strong>Correspondence Plot Help</strong></p>

            <p>This module uses <a href = "https://CRAN.R-project.org/package=ca" target="_blank">ca R package<a/>
            by Michael Greenacre, Oleg Nenadic and Michael Friendly. In-depth information can be found
            in the package documentation on CRAN site.</p>

            <p>It computes <strong>Correspondence Analysis (CA)</strong> for two categorical variables (the SPSS way).
            The data may be weighted using <em>jamovi</em> built-in weight system or using the "Counts" variable.</p>

            <p><strong>Supplementary row or column</strong> numbers may be entered as integer lists : 1,3,6</p>

            <p>Four normalizations (scaling of row and column scores before plotting) are avalaible :
            <ul>
                <li><strong>Principal:</strong> Row an columns scores are scaled by eigenvalues.</li>
                <li><strong>Symetric:</strong> Row an columns scores are scaled by the square root of eigenvalues. </li>
                <li><strong>Row Principal:</strong> Only row scores are scaled by eigenvalues.</li>
                <li><strong>Column Principal:</strong> Only column scores are scaled by eigenvalues.</li>
                <li><strong>Standard:</strong> The raw coordinates without normalization.</li>
            </ul></p>

            <p>Each plot (Rows, Columns, Biplot) will fit the plot dimensions set in <strong>Plot Options</strong>
            while maintaining a <strong>Fixed X/Y Ratio</strong> to 1 unless the corresponding option is uncheked.</p>

            <p>A sample file is included at Open > Data Library > vijPlots > Smoking</p>

            </div>')
        }
    )
)
