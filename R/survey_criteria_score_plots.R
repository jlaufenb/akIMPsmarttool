

#' Plot Pie Chart of Survey Total Benefits
#'
#' @param smart_tool_output A data.frame containing SMART tool inputs and outputs including survey priority scores and ranks returned by the \code{imp_smart_tool} function.
#' @param save_output Logical value controlling whether output is saved as CSV. Default is FALSE.
#' @param save_filepath File path to folder where output is saved. File names are internally generated.
#'
#' @return Pie chart plot
#' @export
#'
benefits_piechart <- function(smart_tool_output, save_output = FALSE, save_filepath = NULL){
    # Pie chart of survey benefits
    old.par = par(no.readonly = TRUE)
    slices = rowSums(smart_tool_output[,grepl("wgt_score", names(smart_tool_output)) & !grepl("5", names(smart_tool_output))])
    lbls = smart_tool_output$survey_name
    pct = round(slices/sum(slices)*100)
    lbls = paste(lbls, pct) # add percents to labels
    lbls = paste(lbls,"%",sep = "") # ad % to labels
    if(save_output){
        if(is.null(save_filepath))save_filepath = getwd()
        png(paste0(save_filepath,"/",attributes(smart_tool_output)$refuge_code,
                   "_survey_benefit_score_piechart_", gsub("-","",Sys.Date()),".png"),
            height = 6.5, width = 6.5, units = "in", res = 192)
        pie(slices,labels = lbls, col=rainbow(length(lbls)), cex = slices, main = "Pie Chart of Survey Benefits")
        dev.off()
    }else{
        pie(slices,labels = lbls, col=rainbow(length(lbls)), cex = slices, main = "Pie Chart of Survey Benefits")
    }
    par(old.par)
    }

#' Plot Pie Chart of Survey Total Costs
#'
#' @param smart_tool_output A data.frame containing SMART tool inputs and outputs including survey priority scores and ranks returned by the \code{imp_smart_tool} function.
#' @param save_output Logical value controlling whether output is saved as CSV. Default is FALSE.
#' @param save_filepath File path to folder where output is saved. File names are internally generated.
#'
#' @return Pie chart plot
#' @export
#'
costs_piechart <- function(smart_tool_output, save_output = FALSE, save_filepath = NULL){
    # Pie chart of survey costs
    old.par = par(no.readonly = TRUE)
    slices = smart_tool_output$annual_weeks * smart_tool_output$imp_frequency
    lbls = smart_tool_output$survey_name
    pct = round(slices/sum(slices)*100)
    lbls = paste(lbls, pct) # add percents to labels
    lbls = paste(lbls,"%",sep = "") # ad % to labels
    cexs = 0.5 + ((slices - min(slices))/(diff(range(slices))))
    if(save_output){
        if(is.null(save_filepath))save_filepath = getwd()
        png(paste0(save_filepath,"/",attributes(smart_tool_output)$refuge_code,
                   "_survey_cost_score_piechart_", gsub("-","",Sys.Date()),".png"),
            height = 6.5, width = 6.5, units = "in", res = 192)
        pie(slices,labels = lbls, col=rainbow(length(lbls)), cex = cexs, main = "Pie Chart of Survey Costs")
        dev.off()
    }else{
        pie(slices,labels = lbls, col=rainbow(length(lbls)), cex = cexs, main = "Pie Chart of Survey Costs")
    }
    par(old.par)
}


#' Plot Pie Chart of SMART Tool Criteria Weights
#'
#' @param smart_tool_output A data.frame containing SMART tool inputs and outputs including survey priority scores and ranks returned by the \code{imp_smart_tool} function.
#' @param save_output Logical value controlling whether output is saved as CSV. Default is FALSE.
#' @param save_filepath File path to folder where output is saved. File names are internally generated.
#'
#' @return Pie chart plot
#' @export
#'
weights_piechart <- function(smart_tool_output, save_output = FALSE, save_filepath = NULL){
    # Pie chart of criteria weights
    old.par = par(no.readonly = TRUE)
    slices = attributes(smart_tool_output)$criteria_weights
    lbls = paste0(attributes(smart_tool_output)$criteria_names, c(rep(" (benefit)",8)," (cost)"))
    pct = round(slices/sum(slices)*100)
    lbls = paste(lbls, pct) # add percents to labels
    lbls = paste(lbls,"%",sep = "") # ad % to labels
    cexs = 0.5 + ((slices - min(slices))/(diff(range(slices))))
    if(save_output){
        if(is.null(save_filepath))save_filepath = getwd()
        png(paste0(save_filepath,"/",attributes(smart_tool_output)$refuge_code,
                   "_criteria_weight_piechart_", gsub("-","",Sys.Date()),".png"),
            height = 6.5, width = 6.5, units = "in", res = 192)
        par(mar = c(8,8,8,8))
        pie(slices,labels = lbls, col=rainbow(length(lbls)), cex = 0.5, main = "Pie Chart of Criteria Weights")
        dev.off()
    }else{
        par(mar = c(8,8,8,8))
        pie(slices,labels = lbls, col=rainbow(length(lbls)), cex = 0.5, main = "Pie Chart of Criteria Weights")
    }
    par(old.par)
}



#' Plot Histograms of Normalized and Weighted Survey Scores
#'
#' @param smart_tool_output A data.frame containing SMART tool inputs and outputs including survey priority scores and ranks returned by the \code{imp_smart_tool} function.
#' @param save_output Logical value controlling whether output is saved as CSV. Default is FALSE.
#' @param save_filepath File path to folder where output is saved. File names are internally generated.
#'
#' @return Histogram plot
#' @export
#'
survey_scores_hists <- function(smart_tool_output, save_output = FALSE, save_filepath = NULL){
    old.par = par(no.readonly = TRUE)
    roundUp = function(x,to=10){to*(x%/%to + as.logical(x%%to))}
    norm_scores = smart_tool_output[, grep("norm_score", colnames(smart_tool_output))]
    weighted_norm_scores = smart_tool_output[, grep("wgt_score", colnames(smart_tool_output))]
    plotx = hist(unlist(norm_scores), breaks = seq(0, 1, 0.02), plot = FALSE)
    ploty = hist(unlist(weighted_norm_scores), breaks = seq(0, 1, 0.02), plot = FALSE)
    ylims = c(0,max(roundUp(max(plotx$counts)), roundUp(max(ploty$counts))))
    if(save_output){
        if(is.null(save_filepath))save_filepath = getwd()
        png(paste0(save_filepath,"/",attributes(smart_tool_output)$refuge_code,
                   "_survey_scores_hists_", gsub("-","",Sys.Date()),".png"),
            height = 9, width = 6.5, units = "in", res = 192)
        par(mfrow = c(2,1))
        plot(plotx, ylim = ylims, col = "gray",
             xlab = "Normalized criteria score", main = "")
        plot(ploty, ylim = ylims, col = "gray",
             xlab = "Weighted normalized criteria score", main = "")
        dev.off()
    }else{
        par(mfrow = c(2,1))
        plot(plotx, ylim = ylims, col = "gray",
             xlab = "Normalized criteria score", main = "")
        plot(ploty, ylim = ylims, col = "gray",
             xlab = "Weighted normalized criteria score", main = "")
    }
    par(old.par)
}




#' Helper Function for Plot Function from Plot.matrix Package
#'
#' @param matplot Value returned from \code{plot.matrix} package.
#' @param mar Argument for margins passed on to \code{plot} function.
#'
#' @return Plot of matrix
#' @export
#'
plot_scores <- function(matplot, mar = c(8, 14, 4, 4)){
    par(mar = mar + 0.01 ) # adapt margins
    if(!is.null(matplot$plot))do.call("plot", matplot$plot)
    if(!is.null(matplot$cell.polygon))lapply(matplot$cell.polygon, do.call, what = "polygon")
    if(!is.null(matplot$cell.text))lapply(matplot$cell.text, do.call, what = "text")
    if(!is.null(matplot$axis.col))do.call("axis", matplot$axis.col)
    if(!is.null(matplot$axis.row))do.call("axis", matplot$axis.row)
    if(!is.null(matplot$key.axis))do.call("axis", matplot$key.axis)
    if(!is.null(matplot$key.polygon))lapply(matplot$key.polygon, do.call, what = "polygon")
}

#' Plot Normalized Survey Scores as Color-Coded Matrix
#'
#' @param smart_tool_output A data.frame containing SMART tool inputs and outputs including survey priority scores and ranks returned by the \code{imp_smart_tool} function.
#' @param save_output Logical value controlling whether output is saved as CSV. Default is FALSE.
#' @param save_filepath File path to folder where output is saved. File names are internally generated.
#'
#' @return Plot of matrix
#' @export
#'
normalized_score_gridplot <- function(smart_tool_output, save_output = FALSE, save_filepath = NULL){
    old.par = par(no.readonly = TRUE)
    nsurveys = nrow(smart_tool_output)
    norm_mat = smart_tool_output[,grep("norm_score",  names(smart_tool_output))]
    names(norm_mat) = attributes(smart_tool_output)$criteria_names
    rownames(norm_mat) = paste0(rownames(norm_mat), " (", format(round(rowSums(norm_mat),2),nsmall = 2),")")
    norm_truezeros = which(c(t(norm_mat)) == 0)
    norm_littles = which(c(t(norm_mat)) > 0 & c(t(norm_mat)) < 0.01)
    norm_matplot = plot(t(norm_mat), cex = 0.6, xlab = "", ylab = "", main = "Normalized Criteria Scores",
                         breaks = seq(0,1,0.1), col = terrain.colors(n = length(seq(0,1,0.1)) - 1, alpha = 0.5), digits = 2,
                         axis.col = list(side = 1, las = 2, cex.axis = 0.7, labels = rep("",nsurveys)),
                         axis.row = list(side = 2, las = 2, cex.axis = 0.7))
    for (i in 1:length(norm_matplot$cell.text)){
        norm_matplot$cell.text[[i]]$labels = gsub("\\+","", norm_matplot$cell.text[[i]]$labels)
        if(i %in% norm_truezeros){norm_matplot$cell.text[[i]]$labels = "0.00"}
        if(i %in% norm_littles){
            norm_matplot$cell.text[[i]]$labels = "<0.01"
        }
    }
    norm_matplot$key.axis$labels = gsub("\\+","",norm_matplot$key.axis$labels)
    norm_matplot$key.axis$cex.axis = 0.9
    norm_matplot$key.axis$pos = 23.5
    if(save_output){
        if(is.null(save_filepath))save_filepath = getwd()
        png(paste0(save_filepath,"/",attributes(smart_tool_output)$refuge_code,
                   "_norm_criteria_scores_grid_", gsub("-","",Sys.Date()),".png"),
            height = 6.5, width = 9, units = "in", res = 1200)
        plot_scores(norm_matplot, mar = c(7, 12, 4, 2))
        text(norm_matplot$axis.col$at, par("usr")[3]-0.5, adj = 1, xpd = NA,
             labels = rownames(norm_mat), srt = 45, cex = 0.7)
        mtext("Proposed Surveys (summed survey score)", side = 1, line = 6)
        mtext("Prioritization Criteria", side = 2, line = 11)
        dev.off()
    }else{
        plot_scores(norm_matplot, mar = c(7, 12, 4, 2))
        text(norm_matplot$axis.col$at, par("usr")[3]-0.5, adj = 1, xpd = NA,
             labels = rownames(norm_mat), srt = 45, cex = 0.7)
        mtext("Proposed Surveys (summed survey score)", side = 1, line = 6)
        mtext("Prioritization Criteria", side = 2, line = 11)
    }
    par(old.par)
}


#' Plot Weighted Normalized Survey Scores as Color-Coded Matrix
#'
#' @param smart_tool_output A data.frame containing SMART tool inputs and outputs including survey priority scores and ranks returned by the \code{imp_smart_tool} function.
#' @param save_output Logical value controlling whether output is saved as CSV. Default is FALSE.
#' @param save_filepath File path to folder where output is saved. File names are internally generated.
#'
#' @return Plot of matrix
#' @export
#'
weighted_score_gridplot <- function(smart_tool_output, save_output = FALSE, save_filepath = NULL){
    old.par = par(no.readonly = TRUE)
    nsurveys = nrow(smart_tool_output)
    weighted_norm_mat = smart_tool_output[,grep("wgt_score",  names(smart_tool_output))]
    names(weighted_norm_mat) = paste0(attributes(smart_tool_output)$criteria_names,
                                       format(round(attributes(smart_tool_output)$criteria_weights,3),nsmall = 3),")")
    rownames(weighted_norm_mat) = paste0(rownames(weighted_norm_mat), " (",
                                          format(round(rowSums(weighted_norm_mat),2),nsmall = 2),")")
    wgtnorm_truezeros = which(c(t(weighted_norm_mat)) == 0)
    wgtnorm_littles = which(c(t(weighted_norm_mat)) > 0 & c(t(weighted_norm_mat)) < 0.01)
    wgtnorm_matplot = plot(t(weighted_norm_mat), cex = 0.6, xlab = "", ylab = "",
                            main = "Weighted Normalized Criteria Scores", breaks = seq(0,0.5,0.05),
                            col = terrain.colors(n = length(seq(0,0.5,0.05)) - 1, alpha = 0.5), digits = 2,
                            axis.col = list(side = 1, las = 2, cex.axis = 0.7, labels = rep("",nsurveys)),
                            axis.row = list(side = 2, las = 2, cex.axis = 0.7))
    for (i in 1:length(wgtnorm_matplot$cell.text)){
        wgtnorm_matplot$cell.text[[i]]$labels = gsub("\\+","", wgtnorm_matplot$cell.text[[i]]$labels)
        if(i %in% wgtnorm_truezeros){wgtnorm_matplot$cell.text[[i]]$labels = "0.00"}
        if(i %in% wgtnorm_littles){
            wgtnorm_matplot$cell.text[[i]]$labels = "<0.01"
        }
    }
    wgtnorm_matplot$key.axis$labels = gsub("\\+","",wgtnorm_matplot$key.axis$labels)
    wgtnorm_matplot$key.axis$cex.axis = 0.9
    wgtnorm_matplot$key.axis$pos = 23.5
    if(save_output){
        if(is.null(save_filepath))save_filepath = getwd()
        png(paste0(save_filepath,"/",attributes(smart_tool_output)$refuge_code,
                   "_wgtnorm_criteria_scores_grid_", gsub("-","",Sys.Date()),".png"),
            height = 6.5, width = 9, units = "in", res = 192)
        plot_scores(wgtnorm_matplot, mar = c(7, 12, 4, 2))
        text(wgtnorm_matplot$axis.col$at, par("usr")[3]-0.5, adj = 1, xpd = NA,
             labels = rownames(weighted_norm_mat), srt = 45, cex = 0.7)
        mtext("Proposed Surveys (summed weighted survey score)", side = 1, line = 6)
        mtext("Prioritization Criteria (criteria weight)", side = 2, line = 11)
        dev.off()
    }else{
        plot_scores(wgtnorm_matplot, mar = c(7, 12, 4, 2))
        text(wgtnorm_matplot$axis.col$at, par("usr")[3]-0.5, adj = 1, xpd = NA,
             labels = rownames(weighted_norm_mat), srt = 45, cex = 0.7)
        mtext("Proposed Surveys (summed weighted survey score)", side = 1, line = 6)
        mtext("Prioritization Criteria (criteria weight)", side = 2, line = 11)
    }
    par(old.par)
}
