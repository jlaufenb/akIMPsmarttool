

#' Title
#'
#' @param smart_tool_output 
#' @param save_output 
#' @param save_filepath 
#'
#' @return
#' @export
#'
#' @examples
priority_score_barchart <- function(smart_tool_output, save_output = FALSE, save_filepath = NULL){
    # Bar chart of survey priority scores
    old.par <- par(no.readonly = TRUE)
    end_point <-  0.5 + nrow(smart_tool_output) + nrow(smart_tool_output) - 1
    bar_cols <- gray.colors(2)[as.numeric(smart_tool_output$source)]
    if(save_output){
        if(is.null(save_filepath))save_filepath <- getwd()
        png(paste0("output/tet/survey_priority_score_barchart_", gsub("-","",Sys.Date()),".png"),
            height = 6.5, width = 9, units = "in", res = 192)
        par(mar = c(8,5,2,2))
        barplot(smart_tool_output$survey_priority_score, beside = TRUE, axes = FALSE, col = bar_cols,
                ylab = "Priority score", xlab = "", main = "", space = 1, names.arg = FALSE)
        axis(2)
        text(seq(1.5, end_point, by = 2), par("usr")[3] - 0.025, 
             srt = 60, adj = 1, xpd = TRUE,
             labels = smart_tool_output$survey_name, cex = 1)
        dev.off()
    }else{
        par(mar = c(8,5,2,2))
        barplot(smart_tool_output$survey_priority_score, beside = TRUE, axes = FALSE, col = bar_cols,
                ylab = "Priority score", xlab = "", main = "", space = 1, names.arg = FALSE)
        axis(2)
        text(seq(1.5, end_point, by = 2), par("usr")[3] - 0.025, 
             srt = 60, adj = 1, xpd = TRUE,
             labels = smart_tool_output$survey_name, cex = 1)
    }
    par(old.par)
}


#' Title
#'
#' @param smart_tool_output 
#' @param save_output 
#' @param save_filepath 
#'
#' @return
#' @export
#'
#' @examples
priority_score_piechart <- function(smart_tool_output, save_output = FALSE, save_filepath = NULL){
    # Pie chart of survey priority score
    old.par <- par(no.readonly = TRUE)
    slices <- smart_tool_output$survey_priority_score
    lbls <- smart_tool_output$survey_name
    pct <- round(slices/sum(slices)*100)
    lbls <- paste(lbls, pct) # add percents to labels
    lbls <- paste(lbls,"%",sep = "") # ad % to labels
    if(save_output){
        if(is.null(save_filepath))save_filepath <- getwd()
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
    par(old.par)
}
