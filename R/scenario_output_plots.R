
#' Plot Top Performing Survey Portfolios Given Scenario Constraints
#'
#' @param scenario_output A data.frame of ranked portfolios for a given scenario returned by the \code{scenario_optimization_tool} function.
#' @param ndisplay Number of portfolios to display. Default is top 10.
#' @param save_output Logical value controlling whether output is saved as CSV. Default is FALSE.
#' @param save_filepath File path to folder where output is saved. File names are internally generated.
#'
#' @return Grid plot of top-peforming survey portfolios.
#' @export
#'
portfolio_grid_plot <- function(scenario_output, ndisplay = 10, save_output = FALSE, save_filepath = NULL){
    nsurveys = attributes(scenario_output)$nsurveys
    scen_mat = scenario_output[1:ndisplay,c(1:nsurveys,which(grepl("imp_priority_score", names(scenario_output))))]
    roundvals = scen_mat
    scen_mat[,which(!grepl("imp", names(scen_mat)))] = scen_mat[,which(!grepl("imp", names(scen_mat)))] - 1
    names(scen_mat)[which(grepl("imp", names(scen_mat)))] = "Priority Score"
    raw_scenvals = unlist(scen_mat)
    zeros = which(raw_scenvals == (-1))
    ones = which(raw_scenvals == 0)
    scores = which(!raw_scenvals > 0)
    cols = c(rgb(red = 91, green = 155, blue = 213, maxColorValue = 255), # not selected
              rgb(red = 255, green = 192, blue = 0, maxColorValue = 255), # selected
              rgb(red = 112, green = 173, blue = 71, maxColorValue = 255)) # values
    val_levs = as.numeric(cut(raw_scenvals, breaks = c(-2,-1,0,max(scen_mat)), include.lowest = FALSE, right = TRUE))
    matplot_cols = cols[val_levs]
    matplot_vals = text_xadd = rep(NA,length(matplot_cols))
    cell_xadd = vector("list",length(matplot_cols))
    status = c("NS","S")
    roundvals$imp_priority_score = format(round(roundvals$imp_priority_score,2), nsmall = 2)
    roundvals = unlist(roundvals)
    for(i in 1:length(matplot_vals)){
        if(val_levs[i] %in% 3){
            matplot_vals[i] = roundvals[i]
            cell_xadd[[i]] = c(0,0,0.5,0.5)
            text_xadd[i] = 0.25
        }else{
            matplot_vals[i] = status[val_levs[i]]
            cell_xadd[[i]] = rep(0,4)
            text_xadd[i] = 0
        }
    }
    names(scen_mat)[1:22] = paste0(names(scen_mat)[1:22], " (",
                                    format(round(attributes(scenario_output)$survey_priority_scores,2), nsmall = 2),")")
    scen_matplot = plot(as.matrix(scen_mat), cex = 0.5, xlab = "", ylab = "",
                         main = paste0("Scenario ",attributes(scenario_output)$scenario$scenario_name,
                                       " Top ",ndisplay, " Survey Sets"),
                         breaks = c(-2,-1,0, max(scen_mat)), col = matplot_cols, digits = 2,
                         axis.col = list(side = 1, las = 2, cex.axis = 0.7, labels = rep("",ncol(scen_mat))),
                         axis.row = list(side = 2, las = 2, cex.axis = 0.9),
                         xlim = c(0,26)
    )
    scen_matplot$key.axis$labels = c("Not Selected (NS)", "Selected (S)", "Values")
    scen_matplot$key.axis$at = scen_matplot$key.axis$at[1:3] + (diff(scen_matplot$key.axis$at)/2) #seq(1,ndisplay, length.out = 5)[2:4]
    scen_matplot$key.axis$tick = FALSE
    scen_matplot$key.axis$lwd = 0
    scen_matplot$key.axis$cex.axis = 0.9
    scen_matplot$key.axis$line = -1.5
    scen_matplot$axis.col$at = scen_matplot$axis.col$at + c(rep(0,nsurveys),0.25)
    scen_matplot$axis.row$pos = 0.5
    cexs = c(0.9,0.9,0.9)
    for(i in 1:length(scen_matplot$key.polygon)){
        scen_matplot$key.polygon[[i]]$col = cols[i]
        scen_matplot$key.polygon[[i]]$x = scen_matplot$key.polygon[[i]]$x + rep(0.5, 4)
    }
    for (i in 1:length(scen_matplot$cell.polygon)){
        scen_matplot$cell.polygon[[i]]$col = matplot_cols[i]
        scen_matplot$cell.polygon[[i]]$x = scen_matplot$cell.polygon[[i]]$x + cell_xadd[[i]]
        scen_matplot$cell.text[[i]]$labels = matplot_vals[i]
        scen_matplot$cell.text[[i]]$cex = cexs[val_levs[i]]
        scen_matplot$cell.text[[i]]$x = scen_matplot$cell.text[[i]]$x + text_xadd[i]
    }
    if(save_output){
        if(is.null(save_filepath))save_filepath = getwd()
        png(paste0(save_filepath, "/",attributes(scenario_output)$refuge_code,
                   "_top",ndisplay,"_surveysets_scenario_", attributes(scenario_output)$scenario$scenario_name, "_", gsub("-","",Sys.Date()),".png"),
            height = 6.5, width = 9, units = "in", res = 192)
        plot_scores(scen_matplot, c(8,4,4,6))
        text(scen_matplot$axis.col$at, par("usr")[3]-0.5, adj = 1, xpd = NA,
             labels = colnames(scen_mat), srt = 45, cex = 0.7)
        mtext("Proposed Surveys (summed weighted survey scores) and Priority Score", side = 1, line = 7)
        mtext("Survey Set Rank", side = 2, line = 3)
        dev.off()
    }else{
        plot_scores(scen_matplot, c(8,4,4,6))
        text(scen_matplot$axis.col$at, par("usr")[3]-0.5, adj = 1, xpd = NA,
             labels = colnames(scen_mat), srt = 45, cex = 0.7)
        mtext("Proposed Surveys (summed weighted survey scores) and Priority Score", side = 1, line = 7)
        mtext("Survey Set Rank", side = 2, line = 3)
    }
}



