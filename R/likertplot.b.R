
# This file is a generated template, your changes will not be overwritten

likertplotClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "likertplotClass",
    inherit = likertplotBase,
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
        .mannU = function(var, group, data, level1=1, level2=2) { # Two groups
            variable <- jmvcore::toNumeric(data[[var]])
            levels <- levels(data[[group]])
            group1 <- variable[data[[group]] == levels[level1]]
            group2 <- variable[data[[group]] == levels[level2]]
            res1 <- wilcox.test(group1, group2)
            res2 <- wilcox.test(group2, group1)
            statistic <- min(res1$statistic, res2$statistic)
            return( list('statistic' = statistic, 'p.value' = res1$p.value) )
        },
        .init = function() {
            # Set the size of the plot
            image <- self$results$plot
            image$setSize(self$options$plotWidth, self$options$plotHeight)
        },
        .run = function() {
            if( length( self$options$liks) == 0  ) {
                return()
            } else {
                self$results$helpMessage$setVisible(FALSE)
            }

            mainData <- self$data[c(self$options$liks, self$options$group)]

            # Check if ordered factor
            for (ques in self$options$liks) {
                varAttrib <- attr(mainData[[ques]],"class",TRUE)
                if ( ("factor" %in% varAttrib)  && !("ordered" %in% varAttrib) )
                    reject("Likert Plot requires ordinal (or numeric) variables")
            }

            # Check if canBeNumeric (if median/mean/tests/to integer / tidy up requested)
            canbeNum <- TRUE
            for (ques in self$options$liks)
                canbeNum <- canbeNum && jmvcore::canBeNumeric(mainData[[ques]])

            if (!canbeNum && self$options$frequencyTable && (self$options$showMedian || self$options$showMean))
                reject("Median and mean require numeric variables")
            if (!canbeNum && (self$options$showMannU || self$options$showKW || self$options$showPostHoc))
                reject("Comparison tests require numeric variables")
            if (!canbeNum && self$options$toInteger)
                reject("Cannot convert text variables to integers")
            if (!canbeNum && self$options$tidyUp)
                reject("Cannot tidy up text variables")

            # Set variable names
            private$.setVarNames(c(self$options$liks, self$options$group))

            # Convert to integer
            if (self$options$toInteger) {
                for (var in self$options$liks) {
                    mainData[[var]] <- factor(jmvcore::toNumeric(mainData[[var]]))
                    attr(mainData[[var]], "values") <- as.integer(levels(mainData[[var]]))
                    attr(mainData[[var]], "class") <- c("ordered","factor")
                }
            }

            # Tidy up levels
            if (self$options$tidyUp) {
                all_values <- c()
                level_value <- list()
                # Save all levels and values from the variables
                for (ques in self$options$liks) {
                    levels <- levels(mainData[[ques]])
                    values <- attr(mainData[[ques]], "values", TRUE)
                    all_values <- c(all_values, values)
                    for (i in seq_along(levels)){ # for each value, save the associate level (once)
                        if(is.null(level_value[[as.character(values[i])]]))
                            level_value[[as.character(values[i])]] <- levels[i]
                    }
                }
                # Sort the value and the corresponding levels
                tidyValues <- sort(unique(all_values))
                tidyLevels <- unlist(level_value[as.character(tidyValues)], use.names=FALSE)
                # Set the variable again as factors with tidy levels/values
                for (ques in self$options$liks) {
                    mainData[[ques]] <- factor(jmvcore::toNumeric(mainData[[ques]]), levels = tidyValues, labels = tidyLevels)
                    attr(mainData[[ques]], "values") <- tidyValues
                    attr(mainData[[ques]], "class") <- c("ordered","factor")
                    attr(mainData[[ques]], "jmv-retain-unused") <- TRUE
                }
            }

            # Missing group cases
            if (!is.null(self$options$group)) {
                if (!self$options$ignoreNA) {
                    # change NA to "NA"
                    if ( sum(is.na(mainData[[self$options$group]])) > 0)
                        mainData[[self$options$group]] <- forcats::fct_na_value_to_level(mainData[[self$options$group]], level="NA")
                }
            }
            # Cleaning the group variable name (it would crash gglikert)
            if( ! is.null(self$options$group) ) {
                groupingVar <- jmvcore::toB64(self$options$group)
                names(mainData)[length(names(mainData))] <- groupingVar
            } else {
                groupingVar <- NULL
            }

            # Set image data
            image <- self$results$plot
            image$setState(mainData)

            # Data for table
            ggLikertData <- ggstats::gglikert_data(tibble::as_tibble(mainData), include = self$options$liks, sort = self$options$sorting)
            questions <- levels(ggLikertData[['.question']])
            nq <- length(questions)
            if (!is.null(groupingVar)) {
                groups <- levels(ggLikertData[[groupingVar]])
                ng <- length(groups)
            } else {
                ng <- 0
            }

            # Frequency table
            oneFreqIsNull <- 1
            if (self$options$frequencyTable) {
                if (self$options$frequencies == "counts") {
                    fType <- 'integer'
                    fmt <- ''
                } else {
                    fType <- 'number'
                    fmt <- 'pc'
                }

                if (ng == 0) { # Freq table without grouping variable
                    self$results$frequencies$addColumn("Sum", type="integer", title = "N")
                    for (col in levels(ggLikertData[['.answer']])) {
                        #self$results$frequencies$addColumn(col, type = fType, format = fmt, title = paste0(strwrap(col, 15), collapse="<br />"))
                        self$results$frequencies$addColumn(col, type = fType, format = fmt, title = col)
                    }
                    if (self$options$showMedian)
                        self$results$frequencies$addColumn("Median", type = "number", title = .("Median"))
                    if (self$options$showMean) {
                        self$results$frequencies$addColumn("Mean", type = "number")
                        self$results$frequencies$addColumn("SD", type = "number", title = .("SD"))
                    }

                    freqTable <- table(ggLikertData[c('.question','.answer')])
                    sumCol <- marginSums(freqTable, margin = 1)
                    if (self$options$frequencies == "percentages")
                        freqTable <- proportions(freqTable, margin = 1)
                    freqTable <- cbind(freqTable, "Sum" = sumCol)

                    for (ques in questions) {
                        values = as.list(freqTable[ques,])
                        values["ques"] <- private$.varName[[ques]]
                        numericData <-jmvcore::toNumeric(mainData[[ques]])
                        if (self$options$showMedian)
                            values['Median'] <- as.numeric(median(numericData, na.rm = TRUE))
                        if (self$options$showMean) {
                            values["Mean"] <- mean(numericData, na.rm = TRUE)
                            values["SD"] <- sd(numericData, na.rm = TRUE)
                        }
                        self$results$frequencies$addRow(rowKey = ques, values = values)
                    }
                } else { # Freq table by group
                    # Build the frequence tables (for each group)
                    freqTables <- list()
                    for (aGroup in groups) {
                        groupData <- subset(ggLikertData, ggLikertData[groupingVar] == aGroup)
                        freqTable <- table(groupData[c('.question','.answer')])
                        sumCol <- marginSums(freqTable, margin = 1)
                        if (self$options$frequencies == "percentages")
                            freqTable <- proportions(freqTable, margin = 1)
                        freqTable <- cbind(freqTable, "Sum" = sumCol)
                        freqTables[[aGroup]] <- freqTable
                    }
                    self$results$frequencies$addColumn(self$options$group, type = "text")
                    self$results$frequencies$addColumn("Sum", type="integer", title = "N")
                    for (col in levels(ggLikertData[['.answer']])) {
                        #self$results$frequencies$addColumn(col, type = fType, format = fmt, title = paste0(strwrap(col, 15), collapse="<br />"))
                        self$results$frequencies$addColumn(col, type = fType, format = fmt, title = col)
                    }
                    if (self$options$showMedian)
                        self$results$frequencies$addColumn("Median", type = "number", title = .("Median"))
                    if (self$options$showMean) {
                        self$results$frequencies$addColumn("Mean", type = "number")
                        self$results$frequencies$addColumn("SD", type = "number", title = .("SD"))
                    }
                    for (ques in questions) {
                        firstGroup <- TRUE
                        for (group in groups) {
                            groupAndQues <- paste0(group,ques)
                            values = as.list(freqTables[[group]][ques,])
                            if (firstGroup) # Workaround to not use combineBelow
                                values["ques"] <- private$.varName[[ques]]
                            else
                                values["ques"] <- " "
                            values[self$options$group] = group
                            numericData <- jmvcore::toNumeric(mainData[[ques]])[mainData[[groupingVar]] == group]
                            if (self$options$showMedian)
                                values["Median"] <- as.numeric(median(numericData, na.rm = TRUE))
                            if (self$options$showMean) {
                                values["Mean"] <- mean(numericData, na.rm = TRUE)
                                values["SD"] <- sd(numericData, na.rm = TRUE)
                            }
                            self$results$frequencies$addRow(rowKey = groupAndQues, values = values)
                            if (firstGroup)
                                self$results$frequencies$addFormat(rowKey = groupAndQues, 1, Cell.BEGIN_GROUP)
                            firstGroup <- FALSE
                            oneFreqIsNull <- oneFreqIsNull * values[["Sum"]]
                        }
                        self$results$frequencies$addFormat(rowKey = groupAndQues, 1, Cell.END_GROUP)
                    }
                }
            } # End Frenquency Table

            # p correction method
            adjustMethod <- self$options$adjustMethod
            adjustMethodStr <- paste0(toupper(substring(adjustMethod, 1, 1)), substring(adjustMethod, 2))
            if (adjustMethod == "BH")
                adjustMethodStr <- "Benjamini-Hochberg"
            else if (adjustMethod == "BY")
                adjustMethodStr <- "Benjamini-Yekutieli"

            # 2-group Mann Whitney U
            if ( ng > 1 && self$options$showMannU) {
                if (ng != 2) {
                    self$results$comp$uTestTable$setNote("p","Mann-Whitney tests require two groups")
                    for (ques in questions) { # Empty table
                        self$results$comp$uTestTable$setRow(rowKey = ques,
                                                            values = list("ques" = private$.varName[[ques]], statistic = NULL, p.value = NULL, adjusted.p = NULL))
                    }
                } else {
                    p <- c()
                    for (ques in questions) {
                        mannU <- private$.mannU(ques, groupingVar, mainData)
                        mannU[["ques"]] <- private$.varName[[ques]]
                        self$results$comp$uTestTable$setRow(rowKey = ques, values = mannU)
                        p <- c(p, mannU[["p.value"]])
                    }
                    if (self$options$pValue == "overall") {
                        self$results$comp$uTestTable$addColumn(name = "adjusted.p", title = .("Adj. p"), type = 'number', format = 'zto,pvalue')
                        adjustedp <- p.adjust(p, method = adjustMethod)
                        for (i in 1:nq) {
                            self$results$comp$uTestTable$setCell(rowNo = i, col = "adjusted.p", adjustedp[i])
                        }
                        self$results$comp$uTestTable$setNote("adj",
                            jmvcore::format(.("p-values are adjusted using {method} method."), method = adjustMethodStr))
                    }
                }
            }

            # Kruskal-Wallis tests
            if (ng > 1 && self$options$showKW) {
                p <- c()
                for (ques in questions) {
                    res <- kruskal.test(jmvcore::toNumeric(mainData[[ques]]), mainData[[groupingVar]])
                    res[["ques"]] <- private$.varName[[ques]]
                    self$results$comp$kwTable$setRow(rowKey = ques, values = res)
                    p <- c(p, res[["p.value"]])
                }
                if (self$options$pValue == "overall") {
                    self$results$comp$kwTable$addColumn(name = "adjusted.p", title = .("Adj. p"), type = 'number', format = 'zto,pvalue')
                    adjustedp <- p.adjust(p, method = adjustMethod)
                    for (i in 1:nq) {
                        self$results$comp$kwTable$setCell(rowNo = i, col = "adjusted.p", adjustedp[i])
                    }
                    self$results$comp$kwTable$setNote("adj",
                        jmvcore::format(.("p-values are adjusted using {method} method."), method = adjustMethodStr))
                }
            }

            # Pairwise comparison table
            if (ng > 1 && self$options$showPostHoc) {
                if (oneFreqIsNull == 0)
                    reject("Comparison tests cannot be computed with empty (N=0) groups")
                # Set title and statistic column title
                self$results$comp$pwTable$setTitle(switch(self$options$postHoc,
                                                          "conover" = .("Conover's Pairwise Comparisons"),
                                                          "dunn" = .("Dunn's Pairwise Comparisons"),
                                                          "dscf" = .("Dwass-Steel-Critchlow-Fligner Pairwise Comparisons")
                                                    )
                )
                statString <- switch(self$options$postHoc,
                                     "conover" = "T",
                                     "dunn" = "Z",
                                     "dscf" = "W*"
                )

                # Compute tests
                res.statistics <- list()
                res.p.values <- list()
                res.p.adjusted <- list()
                for (ques in questions) {
                    if (self$options$postHoc == "conover") {
                        testRes <- private$.conoverTest(jmvcore::toNumeric(mainData[[ques]]), mainData[[groupingVar]])
                    } else if (self$options$postHoc == "dunn") {
                        testRes <- private$.dunnTest(jmvcore::toNumeric(mainData[[ques]]), mainData[[groupingVar]])
                    } else if (self$options$postHoc == "dscf") {
                        testRes <- private$.dscfAllPairsTest(mainData[[ques]], mainData[[groupingVar]])
                    }
                    res.p.values[[ques]] <- testRes$p.values
                    res.statistics[[ques]] <- testRes$statistics
                    # Compute groupwise adjusted p
                    if (self$options$pValue == "group" && self$options$postHoc != "dscf") {
                        res.p.adjusted[[ques]] <- p.adjust(res.p.values[[ques]], method = adjustMethod)
                    }
                }

                # Compute overall adjusted p
                if (self$options$pValue == "overall" && self$options$postHoc != "dscf") {
                    pvalues <- c()
                    for (ques in questions) {
                        pvalues <- c(pvalues, res.p.values[[ques]])
                    }
                    pvalues <- p.adjust(pvalues, method = adjustMethod)

                    i = 0
                    for (ques in questions) {
                        k = length(res.p.values[[ques]])
                        res.p.adjusted[[ques]] <- pvalues[(i+1):(i+k)]
                        names(res.p.adjusted[[ques]]) <- names(res.p.values[[ques]])
                        i <- i + k
                    }
                }

                self$results$comp$pwTable$setStatus('running')

                # Add table's columns
                for (ques in questions) {
                    #superTitle <- paste0(strwrap(private$.varName[[ques]], ifelse(self$options$pValue == "overall",28,15)), collapse="<br />")
                    superTitle <- private$.varName[[ques]]
                    self$results$comp$pwTable$addColumn(name = paste(ques, "stat"), title = statString, superTitle = superTitle, type = 'number')
                    self$results$comp$pwTable$addColumn(name = paste(ques, "p"), title = "p",
                                                        superTitle = superTitle, type = 'number', format = 'zto,pvalue')
                    if (self$options$pValue != "none" && self$options$postHoc != "dscf")
                        self$results$comp$pwTable$addColumn(name = paste(ques, "p.adj"), title = .("Adj. p"), superTitle = superTitle, type = 'number', format = 'zto,pvalue')
                }

                # Populate table
                k <- 0
                for (i in 1:(ng-1)) {
                    for (j in (i+1):ng) {
                        values <- list("group1" = groups[i], "group2" = groups[j])
                        compStr1 <- paste(groups[i], "-", groups[j])
                        compStr2 <- paste(groups[j], "-", groups[i])
                        k <- k +1
                        for (ques in questions) {
                            # Fix because conover.test & dunn.test don't keep the factor order
                            if (is.na((res.statistics[[ques]])[compStr1])) {
                                compStr <- compStr2
                                res.statistics[[ques]][compStr2] <- - res.statistics[[ques]][compStr2]
                            } else {
                                compStr <- compStr1
                            }
                            values[paste(ques, "stat")] <- (res.statistics[[ques]])[compStr]
                            values[paste(ques, "p")] <- (res.p.values[[ques]])[compStr]
                            if (self$options$pValue != "none")
                                values[paste(ques, "p.adj")] <- (res.p.adjusted[[ques]])[compStr]
                        }
                        self$results$comp$pwTable$addRow(rowKey = paste0(i,"/",j), values = values)
                    }
                }
                # Add Note
                if (self$options$pValue == "overall" && oneFreqIsNull != 0 && self$options$postHoc != "dscf"){
                    self$results$comp$pwTable$setNote("adj",
                        jmvcore::format(.("p-values are adjusted overall using {method} method."), method = adjustMethodStr))
                }
                if (self$options$pValue == "group" && oneFreqIsNull != 0 && self$options$postHoc != "dscf") {
                    self$results$comp$pwTable$setNote("adj",
                        jmvcore::format(.("p-values are adjusted groupwise using {method} method."), method = adjustMethodStr))
                }
                if (oneFreqIsNull != 0 && self$options$postHoc == "dscf") {
                    self$results$comp$pwTable$setNote("adj",.("DSCF p-values are adjusted groupwise."))
                }
                self$results$comp$pwTable$setStatus('complete')
            }
        },
        .plot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state))
                return(FALSE)
            mainData <- image$state

            if (self$options$reverseLikert ) { #&& !self$options$toInteger) {
                for (var in self$options$liks)
                    mainData[[var]] <- forcats::fct_rev(mainData[[var]])
            }

            # Variable Label
            variable_labels <- c()
            for (var in self$options$liks)
                variable_labels[var] <- private$.varName[[var]]
            # Get the group variable name
            if( ! is.null(self$options$group) ) {
                groupingVar <- names(mainData)[length(names(mainData))]
            } else {
                groupingVar <- NULL
            }

            if (!is.null(groupingVar)) {
                # Remove cases with missing group or change NA to "NA"
                if (self$options$ignoreNA)
                    mainData <- subset(mainData, !is.na(mainData[groupingVar]))
            }

            # options
            textSize = self$options$textSize
            accuracy <- as.numeric(self$options$accuracy)
            hLabelWrap <- as.numeric(self$options$hLabelWrap)
            vLabelWrap <- as.numeric(self$options$vLabelWrap)
            if (self$options$hideLabelsBelow)
                hideLabelsBelow <- 0.05
            else
                hideLabelsBelow <- 0.01
            # Doing the plot
            if( self$options$type == 'centered' ) {
                # Group setup
                if( ! is.null(groupingVar) ) {
                    if( self$options$groupBy == "variable" ) {
                        yOption <- groupingVar
                        facetRows <- vars(.question)
                    } else {
                        yOption <- ".question"
                        facetRows <- vars(!!ensym(groupingVar))
                    }
                } else {
                    yOption <- ".question"
                    facetRows <- NULL
                }
                # Do Likert Plot (centered)
                plot <- ggstats::gglikert(as_tibble(mainData), include = self$options$liks,
                                          sort = self$options$sorting,
                                          add_labels = self$options$addLabels,
                                          labels_size = 0.8*textSize / .pt ,
                                          labels_accuracy = accuracy,
                                          labels_hide_below = hideLabelsBelow,
                                          labels_color = self$options$labelColor,
                                          facet_label_wrap = vLabelWrap,
                                          y_label_wrap = hLabelWrap,
                                          add_totals = self$options$addTotals,
                                          y = yOption, facet_rows = facetRows,
                                          variable_labels = variable_labels)
            } else {
                # Group setup
                if( ! is.null(groupingVar) ) {
                    if( self$options$groupBy == "variable" ) {
                        yOption <- groupingVar
                        facetRows <- vars(.question)
                    } else {
                        yOption <- ".question"
                        facetRows <- vars(!!ensym(groupingVar))
                    }
                } else {
                    yOption <- ".question"
                    facetRows <- NULL
                }
                # Do Likert Plot (stacked)
                plot <- ggstats::gglikert_stacked(as_tibble(mainData), include = self$options$liks,
                                                  sort = self$options$sorting,
                                                  add_labels = self$options$addLabels,
                                                  labels_size = 0.8*textSize / .pt ,
                                                  labels_accuracy = accuracy,
                                                  labels_hide_below = hideLabelsBelow,
                                                  labels_color = self$options$labelColor,
                                                  y_label_wrap = hLabelWrap,
                                                  add_median_line = self$options$addMedianLine,
                                                  y = yOption,
                                                  variable_labels = variable_labels)
                plot <- plot + facet_grid(rows = facetRows, labeller = label_wrap_gen(vLabelWrap))
            }
            plot <- plot + theme(text = element_text(size=textSize))
            if (self$options$reverseLikert)
                plot <- plot + scale_fill_brewer(palette = self$options$plotColor, direction = -1)
            else
                plot <- plot + scale_fill_brewer(palette = self$options$plotColor)

            #plot <- plot + theme(strip.text = element_text(size = 18, colour = "red", angle = 0), strip.position = "top")

            # Title & subtitle
            plot <- plot + vijTitlesAndLabels(self$options) + vijTitleAndLabelFormat(self$options, showLegend = TRUE)

            return(plot)
        },
        # Modified from https://github.com/cran/PMCMRplus/blob/master/R/dscfAllPairsTest.R
        .dscfAllPairsTest = function(x, g){
            OK <- complete.cases(x, g)
            x <- x[OK]
            g <- g[OK]
            k <- nlevels(g)
            n <- tapply(x, g, length)
            glev <- levels(g)
            ## Function to get ties for tie adjustment
            getties <- function(x){
                t <- table(x)
                C <- sum((t^3 - t) / 12)
                C
            }
            ## function for pairwise comparisons
            compare.stats <-function(i, j){
                m <- n[j]
                nn <- n[i]
                xraw <- c(x[g==glev[i]], x[g==glev[j]])
                rankx <- rank(xraw)
                lev <- c(g[g==glev[i]], g[g==glev[j]])

                ## make sure to drop unneeded levels
                id <- !glev %in% c(glev[i], glev[j])
                exclude <- glev[id]
                lev <- droplevels(lev, exclude = exclude)

                R <- tapply(rankx, lev, sum)
                # vijPlots
                U <- c(m*nn + (m * (m + 1) / 2), m * nn + (nn * (nn + 1) / 2)) - R
                #Umn <- min(U)
                Umn <- U[1]
                S <- m + nn
                VAR <- (m * nn / (S * (S - 1))) * ((S^3 - S) / 12 - getties(rankx))
                PSTAT <- sqrt(2) * (Umn - m * nn / 2) / sqrt(VAR)
                PSTAT
            }
            PSTAT <- pairwise.table(compare.stats,levels(g), p.adjust.method="none")
            PVAL <- ptukey(abs(PSTAT), nmeans = k, df = Inf, lower.tail = FALSE)
            # vijPlots : change the format of result returned
            p.values = list()
            statistics = list()
            for(i in 1:(k-1)) {
                for(j in i:(k-1)) {
                    gname <- paste(colnames(PSTAT)[i],"-",rownames(PSTAT)[j])
                    p.values[gname] <- PVAL[j,i]
                    statistics[gname] <- - PSTAT[j,i] # i - j
                }
            }
            return(list(p.values = p.values, statistics = statistics))
        },
        # Modified from https://github.com/cran/PMCMRplus/blob/master/R/kwAllPairsConoverTest.R
        .conoverTest = function(x, g,  p.adjust.method = "none"){
            ## Kruskal-Wallis functions
            gettiesKruskal <- function(x) {
                n <- length(x)
                t <- table(x)
                C <- 1 - sum(t^3 - t) / (n^3 - n)
                C <- min(1, C)
                return(C)
            }
            HStat <- function(r, g) {
                ni <- tapply(!is.na(r), g, length)
                N <- sum(ni)

                H <- (12 / (N * (N + 1))) *
                    sum(tapply(r, g, "sum") ^ 2 / ni) - 3 * (N + 1)
                H
            }
            # Main computation
            OK <- complete.cases(x, g)
            x <- x[OK]
            g <- g[OK]
            k <- nlevels(g)
            x.rank <- rank(x)
            R.bar <- tapply(x.rank, g, mean,na.rm=T)
            R.n <- tapply(!is.na(x), g, length)
            g.unique <- unique(g)
            k <- length(g.unique)
            n <- sum(R.n)

            ## Kruskal-Wallis statistic
            H <- HStat(x.rank, g)
            C <- gettiesKruskal(x.rank)
            H.cor <- H / C

            if (C == 1) {
                S2 <- n * (n + 1) / 12
            } else {
                # warning("Ties are present. Quantiles were corrected for ties.")
                S2 <-   ( 1 / (n - 1)) * (sum(x.rank^2) - (n * (((n + 1)^2) / 4)))
            }

            compare.stats <- function(i,j) {
                dif <- R.bar[i] - R.bar[j]
                B <- (1 / R.n[i] + 1 / R.n[j])
                D <- (n - 1 - H.cor) / (n - k)
                tval <- dif / sqrt(S2 * B * D)
                return(tval)
            }
            PSTAT <- pairwise.table(compare.stats, levels(g), p.adjust.method = "none" )

            compare.levels <- function(i,j) {
                dif <- abs(R.bar[i] - R.bar[j])
                B <- (1 / R.n[i] + 1 / R.n[j])
                D <- (n - 1 - H.cor) / (n - k)
                tval <- dif / sqrt(S2 * B * D)
                pval <- 2 * pt(abs(tval), df=n - k, lower.tail=FALSE)
                return(pval)
            }

            PVAL <- pairwise.table(compare.levels, levels(g), p.adjust.method = p.adjust.method )

            # vijPlots : change the format of result returned
            p.values = list()
            statistics = list()
            for(i in 1:(k-1)) {
                for(j in i:(k-1)) {
                    gname <- paste(colnames(PSTAT)[i],"-",rownames(PSTAT)[j])
                    p.values[gname] <- PVAL[j,i]
                    statistics[gname] <- - PSTAT[j,i] # i - j
                }
            }

            return( list(p.values = p.values, statistics = statistics) )
        },
        # Modified from https://github.com/cran/PMCMRplus/blob/master/R/kwAllPairsDunnTest.R
        .dunnTest = function(x, g,  p.adjust.method = "none"){
            gettiesDunn <- function(x){
                n <- length(x)
                t <- table(x)
                C <- sum(t^3 - t) / (12 * (n - 1))
                return(C)
            }
            OK <- complete.cases(x, g)
            x <- x[OK]
            g <- g[OK]
            k <- nlevels(g)
            x.rank <- rank(x)
            R.bar <- tapply(x.rank, g, mean,na.rm=T)
            R.n <- tapply(!is.na(x), g, length)
            g.unique <- unique(g)
            k <- length(g.unique)
            n <- sum(R.n)
            ## get the ties
            C <- gettiesDunn(x.rank)
            # if (C != 0) warning("Ties are present. z-quantiles were corrected for ties.")
            compare.stats <- function(i,j) {
                dif <- R.bar[i] - R.bar[j] # vijplots: remove abs
                A <- n * (n+1) / 12
                B <- (1 / R.n[i] + 1 / R.n[j])
                zval <- dif / sqrt((A - C) * B)
                return(zval)
            }
            PSTAT <- pairwise.table(compare.stats,levels(g), p.adjust.method="none" )
            compare.levels <- function(i,j) {
                dif <- abs(R.bar[i] - R.bar[j])
                A <- n * (n+1) / 12
                B <- (1 / R.n[i] + 1 / R.n[j])
                zval <- dif / sqrt((A - C) * B)
                pval <- 2 * pnorm(abs(zval), lower.tail = FALSE)
                return(pval)
            }
            PVAL <- pairwise.table(compare.levels,levels(g), p.adjust.method = p.adjust.method)

            # vijPlots : change the format of result returned
            p.values = list()
            statistics = list()
            for(i in 1:(k-1)) {
                for(j in i:(k-1)) {
                    gname <- paste(colnames(PSTAT)[i],"-",rownames(PSTAT)[j])
                    p.values[gname] <- PVAL[j,i]
                    statistics[gname] <- - PSTAT[j,i] # i - j
                }
            }

            return( list(p.values = p.values, statistics = statistics) )
        }
    )
)
