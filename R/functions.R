#' @export
print_path <- function(folder) {
    oldpath <- getwd()
    setwd(folder)
    thepath <- unlist(strsplit(getwd(), split = "/"))
    setwd(oldpath)
    if (thepath[1] == "") path <- thepath[-1]
    path[1] <- paste0("    ", path[1])
    cat(path, sep = "\n    -> ")
    invisible(thepath)
}

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

#' @export
check_modules_expected <- function(){
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
    list(extras=extras, missing=missing)
}

#' @export
get_module_effects <- function() {
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

#' @export
print_module_effect <- function(module_code){
    effect <- round(mdf[module_code, "Effect"], 1)
    if (effect < 0) pm <- "minus"
    if (effect > 0) pm <- "plus"
    paste(pm, sprintf("%2.1f", abs(effect)))
}

#' @export
make_module_pages <- function(keep_tmpdir = FALSE) {
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
    if (exists("module_names")) {
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


#' @export
scatter  <- function(module_code) {
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

#' @export
raw_mark_classes <-  function(marks_matrix){
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
    return(round(result, 0))
}

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

#' @export
fit <- function(m) {
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

#' @export
upd <- function() {
    devtools::install_github("DavidFirth/norman")
}

