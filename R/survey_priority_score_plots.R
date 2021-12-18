

#' Plot Bar Chart of Survey Priority Scores
#'
#' @param smart_tool_output A data.frame containing SMART tool inputs and outputs including survey priority scores and ranks returned by the \code{imp_smart_tool} function.
#' @param combined Logical value controlling whether to add survey priority scores excluding cost to the plot. Default is FALSE.
#' @param save_output Logical value controlling whether output is saved as CSV. Default is FALSE.
#' @param save_filepath File path to folder where output is saved. File names are internally generated.
#'
#' @return Bar plot
#' @export
#'
priority_score_barchart <- function(smart_tool_output, combined = FALSE, save_output = FALSE, save_filepath = NULL){
    note = c("","*")[as.numeric(smart_tool_output$source)]
    if(!combined){
        end_point =  0.5 + nrow(smart_tool_output) * 2 - 1
        if(save_output){
            if(is.null(save_filepath))save_filepath = getwd()
            png(paste0(save_filepath,"/",attributes(smart_tool_output)$refuge_code,"_survey_priority_score_barchart_", gsub("-","",Sys.Date()),".png"),
                height = 6.5, width = 9, units = "in", res = 192)
            par(mar = c(8,5,2,2))
            barplot(smart_tool_output$survey_priority_score, beside = TRUE, axes = FALSE, col = gray.colors(10)[4], ylim = c(0,1),
                    ylab = "Priority score", xlab = "", main = "", space = 1, names.arg = FALSE)
            axis(2, at = seq(0,1,0.2))
            text(seq(1.5, end_point, by = 2), par("usr")[3] - 0.025,
                 srt = 60, adj = 1, xpd = TRUE,
                 labels = paste0(smart_tool_output$survey_name, note), cex = 1)
            dev.off()
        }else{
            par(mar = c(8,5,2,2))
            barplot(smart_tool_output$survey_priority_score, beside = TRUE, axes = FALSE, col = gray.colors(10)[4], ylim = c(0,1),
                    ylab = "Priority score", xlab = "", main = "", space = 1, names.arg = FALSE)
            axis(2, at = seq(0,1,0.2))
            text(seq(1.5, end_point, by = 2), par("usr")[3] - 0.025,
                 srt = 60, adj = 1, xpd = TRUE,
                 labels = paste0(smart_tool_output$survey_name, note), cex = 1)
        }
    }
    if(combined){
        end_point =  nrow(smart_tool_output) * 3
        plot_df = data.frame(rbind(smart_tool_output$survey_priority_score,smart_tool_output$benefits_only_score),row.names = c("wcost","nocost"))
        names(plot_df) = smart_tool_output$survey_name
        if(save_output){
            if(is.null(save_filepath))save_filepath = getwd()
            png(paste0(save_filepath,"/",attributes(smart_tool_output)$refuge_code,"_combined_survey_priority_score_barchart_", gsub("-","",Sys.Date()),".png"),
                height = 6.5, width = 9, units = "in", res = 192)
            par(mar = c(8,5,2,2))
            barplot(height = as.matrix(plot_df), beside = TRUE, axes = FALSE, col = gray.colors(10)[c(4,10)], ylim = c(0,1),
                    ylab = "Priority score", xlab = "", main = "", names.arg = rep("",ncol(plot_df)))
            axis(2, at = seq(0,1,0.2))
            text(seq(2, end_point, by = 3), par("usr")[3] - 0.025,
                 srt = 60, adj = 1, xpd = TRUE,
                 labels = paste0(smart_tool_output$survey_name, note), cex = 1)
            dev.off()
        }else{
            par(mar = c(8,5,2,2))
            barplot(height = as.matrix(plot_df), beside = TRUE, axes = FALSE, col = gray.colors(10)[c(4,10)], ylim = c(0,1),
                    ylab = "Priority score", xlab = "", main = "", names.arg = rep("",ncol(plot_df)))
            axis(2, at = seq(0,1,0.2))
            text(seq(2, end_point, by = 3), par("usr")[3] - 0.025,
                 srt = 60, adj = 1, xpd = TRUE,
                 labels = paste0(smart_tool_output$survey_name, note), cex = 1)
        }
    }
}



#' Plot Pie Chart of Survey Priority Scores
#'
#' @param smart_tool_output A data.frame containing SMART tool inputs and outputs including survey priority scores and ranks returned by the \code{imp_smart_tool} function.
#' @param save_output Logical value controlling whether output is saved as CSV. Default is FALSE.
#' @param save_filepath File path to folder where output is saved. File names are internally generated.
#'
#' @return Pie chart plot
#' @export
#'
priority_score_piechart <- function(smart_tool_output, save_output = FALSE, save_filepath = NULL){
    # Pie chart of survey priority score
    slices = smart_tool_output$survey_priority_score
    lbls = smart_tool_output$survey_name
    pct = round(slices/sum(slices)*100)
    lbls = paste(lbls, pct) # add percents to labels
    lbls = paste(lbls,"%",sep = "") # ad % to labels
    if(save_output){
        if(is.null(save_filepath))save_filepath = getwd()
        png(paste0("output/tet/survey_priority_score_piechart_", gsub("-","",Sys.Date()),".png"),
            height = 6.5, width = 6.5, units = "in", res = 192)
        pie(slices,labels = lbls, col = rainbow(length(lbls)),
            cex = smart_tool_output$survey_priority_score * 0.5,
            main = "Pie Chart of Survey Priority (benefit-cost) Scores")
        dev.off()
    }else{
        pie(slices,labels = lbls, col = rainbow(length(lbls)),
            cex = smart_tool_output$survey_priority_score * 0.5,
            main = "Pie Chart of Survey Priority (benefit-cost) Scores")
    }
}
