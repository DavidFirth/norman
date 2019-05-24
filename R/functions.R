#' Get the full path of a folder
#'
#' @param folder Character, the relative path to a folder from the
#' current working directory
#' @return Character, the full path to the folder.  (invisibly)
#' @export
print_path <- function(folder) {
    oldpath <- getwd()
    setwd(folder)
    thepath <- unlist(strsplit(getwd(), split = "/"))
    setwd(oldpath)
    path <- thepath
    if (path[1] == "") path <- path[-1]
    path[1] <- paste0("    ", path[1])
    cat(path, sep = "\n    -> ")
    invisible(thepath)
}

#' Print and return the list of files in a folder
#'
#' @param folder Character, the relative path to a folder from the
#' current working directory
#' @return Character; a vector of file names
#'
#' @importFrom knitr kable
#'
#' @export
print_file_listing <- function(folder) {
    oldpath <- getwd()
    setwd(folder)
    filenames <- list.files(pattern = "*.csv")
    fileinfo <- file.info(filenames)[, "mtime", drop = FALSE]
    setwd(oldpath)
    fileinfo$mtime <- gsub(":..$", "", fileinfo$mtime)
    print(kable(fileinfo, col.names = c("Last modified")))
    return(filenames)
}

#' Check the marks folder contents against file "modules_expected_here.txt"
#'
#' @param working_directory Character; the full path to the folder for this report
#' @param module_codes Character; the vector of module codes used in the report
#' @return A list with two character components, named
#' \code{extras} and \code{missing}
#'
#' @importFrom knitr kable
#'
#' @export
check_modules_expected <- function(working_directory, module_codes){
    checkfile <- paste0(working_directory, "/", "modules_expected_here.txt")
    checkfile_exists <- file.exists(checkfile)
    if (checkfile_exists) {
        modules_expected <- scan(
            paste0(working_directory, "/", "modules_expected_here.txt"),
            what = character())
        extras <- module_codes[!(module_codes %in% modules_expected)]
        if (length(extras) == 0) extras <- "none"
        missing <- modules_expected[!(modules_expected %in% module_codes)]
        if (length(missing) == 0) missing <- "none"
    } else {
        extras <- missing <-
            "(no checking done because the file
           \\small  `modules_expected_here.txt` \\normalsize was not provided)"
    }
    list(extras = extras, missing = missing)
}

#' Print and return a table of module effects
#'
#' @param module_codes Character; the vector of module codes used in the report
#' @param mdd List of median differences
#' @return A 2-column data frame with columns \code{Effect} and \code{Count}
#' @export
get_module_effects <- function(module_codes, mdd) {
    count <- numeric(length(mdd))
    names(count) <- names(mdd)
    for (i in names(mdd)) {
        count[i] <- sum(mdd[[i]][2,])
    }
    mdfit <- mdfit$coef
    names(mdfit) <- module_codes
    mdfit <- round(mdfit - mean(mdfit), 1)
    mdf <- data.frame(Effect = mdfit, Count = count)
    mdf <- mdf[order(mdf$Effect, decreasing = TRUE), ]
    thetable <- kable(mdf)
    # fix the minus signs in the table
    thetable <- gsub("(-)([0-9]+\\.[0-9])", "\\$\\1\\2\\$", thetable)
    print(thetable)
    return(mdf)
    }

#' Get the module effect for a given module code
#'
#' @param module_code Character; the length-5 module code
#' @param mdf Object of class \code{lm}, the module fitted to median differences
#' @return Character; the module effect, rounded to 1 decimal place.
#' @export
print_module_effect <- function(module_code, mdf){
    effect <- round(mdf[module_code, "Effect"], 1)
    if (effect < 0) pm <- "minus"
    if (effect > 0) pm <- "plus"
    paste(pm, sprintf("%2.1f", abs(effect)))
}

#' Make a report page for each module
#'
#' @param working_directory Character; the full path to the folder for the report
#' @param module_codes Character; the vector of module codes used in the report
#' @param module_names Character, or NULL if no "module_names.csv" file was provided
#' @param keep_tmpdir Logical; whether to keep the working "tmp" directory
#' @return Character; R Markdown text for the module pages that were made
#' @export
make_module_pages <- function(working_directory, module_codes, module_names,
                              keep_tmpdir = FALSE) {
  #  marks_matrix <<- cbind(rowMeans(marks_matrix, na.rm = TRUE), marks_matrix)
  #  marks_df <<- as.data.frame(marks_matrix)
  #  names(marks_df)[1] <<- "overall_mean"
    out <- NULL
    template <- scan(system.file("rmarkdown", "templates", "module-template.Rmd",
                                 package = "norman"),
                     what = character(), sep = "\n",
                     blank.lines.skip = FALSE)
    tmpdir <- file.path(working_directory, "tmp")
    dir.create(tmpdir, showWarnings = FALSE)
    if (!is.null(module_names)) {
        name_known <- row.names(module_names)
    } else name_known <- ""
    for (i in module_codes) {
        thefile <- gsub("module-code", i, template)
        module_name_replacement <- if (i %in% name_known) {
                                       module_names[i, ]
                                   } else "(module name not provided)"
        thefile <- gsub("module-name", module_name_replacement, thefile)
        newpage <- ""
        if (i != module_codes[1]) newpage <- "\\newpage\n"
        writeLines(thefile, paste0(tmpdir, "/", i, ".Rmd"))
        out <- c(out, newpage,
                 knitr::knit_child(paste0(tmpdir, "/", i, ".Rmd")))
    }
    if (!keep_tmpdir) unlink(tmpdir, recursive = TRUE)
    return(out)
}


#' Make a module-specific stem and leaf plot
#'
#' @param module_code Character; the length-5 module code
#' @param module_marks List of module marks, one component per module
#'
#' @return A character vector for display
#'
#' @importFrom aplpack stem.leaf
#'
#' @export
stemleaf  <- function(module_code, module_marks) {
    result <- stem.leaf(module_marks[[module_code]], unit = 1, m = 1,
                                 Min = 0, Max = 100,
                                 trim.outliers = FALSE,
                                 na.rm = TRUE,
                                 printresult = FALSE)
    return(result$display)
}

#' Make a module-specific scatterplot
#'
#' @param module_code Character; the length-5 module code
#' @param marks_matrix Numeric, the matrix of marks
#' @param student_overall_mean The row means of marks_matrix
#' @return A \code{ggplot} object
#'
#' @import ggplot2
#'
#' @export
scatter  <- function(module_code, marks_matrix, student_overall_mean) {
    options(warn = -1)
    final_years_exams <- "ST903" %in% colnames(marks_matrix)
    if (final_years_exams) {
        ST903 <- !is.na(marks_matrix[, "ST903"])
        ST952 <- !is.na(marks_matrix[, "ST952"])
        ST415 <- !is.na(marks_matrix[, "ST415"])
        ST404 <- !is.na(marks_matrix[, "ST404"])
        group <- rep("BSc and other", nrow(marks_matrix))
        group <- ifelse(ST903 & ST952, "MSc", group)
        group <- ifelse(ST415, "M degree 4th year", group)
        group <- ifelse(ST404, "M degree 3rd year", group)
        group <- as.factor(group)
        cbPalette <- c("#56B4E9", "#009E73", "#E0D442", "#CC79A7")
        ##  For colourblind-friendly colours
    }
    module_mark <- marks_matrix[, module_code]
    thegraph <- if (!final_years_exams) {
                    ggplot(, aes(y = module_mark, x = student_overall_mean)) +
                        geom_abline(slope = 1, intercept = 0, col = "grey") +
                        geom_point(colour = "#555555")
                } else {
                    ggplot(, aes(y = module_mark, x = student_overall_mean,
                                 color = group)) +  ## shape = group ?
                        geom_abline(slope = 1, intercept = 0, col = "grey") +
                        geom_point() +
                        scale_colour_manual(values=cbPalette) +
                        labs(color = "")
                }
    thegraph <- thegraph +
        theme(aspect.ratio = 1) +
        labs(x = "Average of ST modules taken this year", y = module_code) +
        expand_limits(y = c(0, 100), x = c(0, 100)) +
        theme(panel.grid.minor = element_line(colour="white", size=0.5)) +
        scale_y_continuous(minor_breaks = c(0, 40, 50, 60, 70, 100),
                           breaks = c(0, 40, 50, 60, 70, 100)) +
        theme(panel.grid.minor = element_line(colour="white", size=0.5)) +
        scale_x_continuous(minor_breaks = c(0, 40, 50, 60, 70, 100),
                           breaks = c(0, 40, 50, 60, 70, 100))
    thegraph
}

#' Make a matrix of 7-number summaries for all modules
#'
#' @param marks_matrix The full matrix of marks for the modules
#' @return A matrix
#'
#' @importFrom stats quantile
#' @importFrom stats sd
#'
#' @export
raw_mark_summaries <- function(marks_matrix){
    M <- ncol(marks_matrix)
    result <- matrix(NA, M, 8)
    rownames(result) <- colnames(marks_matrix)
    colnames(result) <- c("(N)", "Zeros", "1st Qu.", "Median",
                          "3rd Qu.", "Max.", "Mean", "S.D.")
    result[, "(N)"] <- apply(marks_matrix, 2, function(col) sum(!is.na(col)))
    result[, "Zeros"] <- apply(marks_matrix, 2, function(col) {
        sum(col == 0, na.rm = TRUE)})
#    result[, "Min."] <- apply(marks_matrix, 2, function(col) min(col, na.rm = TRUE))
    result[, "1st Qu."] <- apply(marks_matrix, 2, function(col)
        quantile(col, 0.25, na.rm = TRUE))
    result[, "Median"] <- apply(marks_matrix, 2, function(col)
        quantile(col, 0.5, na.rm = TRUE))
    result[, "3rd Qu."] <- apply(marks_matrix, 2, function(col)
        quantile(col, 0.75, na.rm = TRUE))
    result[, "Max."] <- apply(marks_matrix, 2, function(col) max(col, na.rm = TRUE))
    result[, "Mean"] <- apply(marks_matrix, 2, function(col) mean(col, na.rm = TRUE))
    result[, "S.D."] <- apply(marks_matrix, 2, function(col) sd(col, na.rm = TRUE))
    result <- round(result, 1)
    return(result)
}

#' Make a matrix of percentages in each degree class range, for all modules
#'
#' @param marks_matrix The full matrix of marks for the modules
#' @param dp Numeric, the number of decimal places to round the result to
#' @return A matrix
#' @export
raw_mark_classes <-  function(marks_matrix, dp = 0){
    M <- ncol(marks_matrix)
    result <- matrix(NA, M, 6)
    rownames(result) <- colnames(marks_matrix)
    colnames(result) <- c("(zero)", "1--39", "40--49", "50--59",
                          "60--69", "70+")
    N <- apply(marks_matrix, 2, function(col) sum(!is.na(col)))
    result[, "(zero)"] <- 100 * apply(marks_matrix, 2, function(col) {
                                  sum(col == 0, na.rm = TRUE)}) / N
    result[, "1--39"] <- 100 * apply(marks_matrix, 2, function(col) {
                                  sum((col >= 1) & (col <= 39), na.rm = TRUE)}) / N
    result[, "40--49"] <- 100 * apply(marks_matrix, 2, function(col) {
                                  sum((col >= 40) & (col <= 49), na.rm = TRUE)}) / N
    result[, "50--59"] <- 100 * apply(marks_matrix, 2, function(col) {
                                  sum((col >= 50) & (col <= 59), na.rm = TRUE)}) / N
    result[, "60--69"] <- 100 * apply(marks_matrix, 2, function(col) {
                                  sum((col >= 60) & (col <= 69), na.rm = TRUE)}) / N
    result[, "70+"] <- 100 * apply(marks_matrix, 2, function(col) {
                                 sum((col >= 70), na.rm = TRUE)}) / N
    return(round(result, dp))
}

#' Compute a matrix of median differences
#'
#' @param xmat A numeric matrix
#' @return A square numeric matrix, with size equal to the number of
#' columns in \code{xmat}
#'
#' @examples
#' #
#' # Toy example from
#' # https://statgeek.net/2019/04/26/robust-measurement-from-a-2-way-table
#' #
#' x <- structure(c(NA, NA, 10, NA, NA, 20, NA, NA, 30, 45, 55, NA, 60, 60, 50),
#'   .Dim = c(3L, 5L), .Dimnames = structure(list(student = c("i", "j", "k"),
#'   module = c("A", "B", "C", "D", "E")), .Names = c("student", "module")))
#' print(x)
#' meddiff(x)
#'
#' @importFrom stats median
#'
#' @export
meddiff <- function(xmat) {
    ## rows are students, columns are modules
    S <- nrow(xmat)
    M <- ncol(xmat)
    result <- matrix(NA, M, M)
    rownames(result) <- colnames(result) <- colnames(xmat)
    for (m in 1:(M-1)) {
        for (mm in (m+1):M) {
            diffs <- xmat[, m] - xmat[, mm]
            result[m, mm] <- median(diffs, na.rm = TRUE)
            result[mm, m] <- sum(!is.na(diffs))
        }
    }
    return(result)
}

#' A version of \code{meddiff} to compute median differences in a different format
#'
#' @param xmat A numeric matrix
#' @return A list, with one vector component for each column of \code{xmat}
#'
#' @examples
#' #
#' # Toy example from
#' # https://statgeek.net/2019/04/26/robust-measurement-from-a-2-way-table
#' #
#' x <- structure(c(NA, NA, 10, NA, NA, 20, NA, NA, 30, 45, 55, NA, 60, 60, 50),
#'   .Dim = c(3L, 5L), .Dimnames = structure(list(student = c("i", "j", "k"),
#'   module = c("A", "B", "C", "D", "E")), .Names = c("student", "module")))
#' print(x)
#' meddiff_for_display(x)
#'
#' @importFrom stats median
#'
#' @export
meddiff_for_display <- function(xmat) {
    ## rows are students, columns are modules
    S <- nrow(xmat)
    M <- ncol(xmat)
    result <- vector("list", M)
    meddiffs <- matrix(NA, 2, M)
    names(result) <- colnames(meddiffs) <- colnames(xmat)
    rownames(meddiffs) <- c("Median difference", "Count")
    for (m in 1:M) {
        for (mm in (1:M)) {
            diffs <- xmat[, m] - xmat[, mm]
            meddiffs[1, mm] <- round(median(diffs, na.rm = TRUE), 0)
            meddiffs[2, mm] <- sum(!is.na(diffs))
        }
        is.na(meddiffs[1, m]) <- TRUE
        result[[m]] <- meddiffs[, !is.na(meddiffs)[1,,drop = FALSE], drop = FALSE]
    }
    return(result)
}

#' List all median within-student differences between modules
#'
#' @param mdd A list
#' @return \code{invisible(NULL)}
#'
#' @examples
#' #
#' # Toy example from
#' # https://statgeek.net/2019/04/26/robust-measurement-from-a-2-way-table
#' #
#' x <- structure(c(NA, NA, 10, NA, NA, 20, NA, NA, 30, 45, 55, NA, 60, 60, 50),
#'   .Dim = c(3L, 5L), .Dimnames = structure(list(student = c("i", "j", "k"),
#'   module = c("A", "B", "C", "D", "E")), .Names = c("student", "module")))
#' print(x)
#' mdd <- meddiff_for_display(x)
#' list_all_median_differences(mdd)
#'
#' @export
list_all_median_differences <- function(mdd) {
    for (module in names(mdd)) {
        cat(rep("-", 96), "\n", sep = "")
        cat(module, "--- comparisons with:\n")
        print(mdd[[module]])
    }
    cat("\n", rep("#", 37), "     END OF LIST     ", rep("#", 38), "\n", sep = "")
    invisible(NULL)
}

#' Extract module effects from the median differences
#'
#' @examples
#' #
#' # Toy example from
#' # https://statgeek.net/2019/04/26/robust-measurement-from-a-2-way-table
#' #
#' x <- structure(c(NA, NA, 10, NA, NA, 20, NA, NA, 30, 45, 55, NA, 60, 60, 50),
#'   .Dim = c(3L, 5L), .Dimnames = structure(list(student = c("i", "j", "k"),
#'   module = c("A", "B", "C", "D", "E")), .Names = c("student", "module")))
#' print(x)
#' md <- meddiff(x)
#' the_fit <- meddiff_fit(md)$coef
#' names(the_fit) <- gsub("^X", "", names(the_fit))
#' the_fit
#'
#' @param m A numeric matrix of mdeian differences as computed by \code{meddiff}
#' @return A \code{lm} model object
#'
#' @importFrom stats model.matrix
#' @importFrom stats lm
#'
#' @export
meddiff_fit <- function(m) {
    ## m needs to be fully (weakly) connected above the diagonal
    ## -- otherwise we can't fit the linear model
    upper <- upper.tri(m)
    diffs <- m[upper]
    weights <- t(m)[upper]
    rows <- factor(row(m)[upper])
    cols <- factor(col(m)[upper])
    X <- cbind(model.matrix(~ rows - 1), 0) - cbind(0, model.matrix(~ cols - 1))
    colnames(X) <- colnames(m)
    rownames(X) <- paste0(colnames(m)[rows], "-", colnames(m)[cols])
    #result <- lm.wfit(X, diffs, weights)
    result <- lm(diffs ~ X - 1, weights = weights)
    result$coefficients[is.na(result$coefficients)] <- 0
    return(result)
}


