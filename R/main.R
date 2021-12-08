
#' Title
#'
#' @param refuge_code 
#' @param start_year 
#' @param nyears 
#' @param annual_staff_budget 
#' @param survey_scores 
#' @param criteria_weights 
#' @param save_output 
#' @param save_filepath 
#'
#' @return
#' @export
#'
#' @examples
imp_smart_tool <- function(refuge_code = NULL, start_year = NULL, nyears = NULL, annual_staff_budget = NULL,
                           survey_scores = NULL, criteria_weights = NULL, save_output = FALSE, save_filepath = NULL){
    imp_staff_budget <- annual_staff_budget * nyears
    surveydat_df <- read.csv(survey_scores)
    criteria_df <- read.csv(criteria_weights)
    if(sum(criteria_df$name_lev1 %in% "Cost")==1)criteria_df$name_lev2[criteria_df$name_lev1 %in% "Cost"] <- "Cost"
    raw_scores <- surveydat_df[,grep("criterion", colnames(surveydat_df))]
    norm_scores <- as.data.frame(lapply(raw_scores, function(x)(x - min(x))/(diff(range(x)))))
    names(norm_scores) <- gsub("criterion","norm_score", names(norm_scores))
    rownames(norm_scores) <- surveydat_df$survey_name
    weighted_norm_scores <- data.frame(t(apply(norm_scores,1,function(x,wgt){x * wgt}, wgt = criteria_df$final_weight)))
    names(weighted_norm_scores) <- gsub("norm", "wgt", names(weighted_norm_scores))
    rownames(weighted_norm_scores) <- surveydat_df$survey_name
    priority_scores <- as.matrix(norm_scores) %*% criteria_df$final_weight
    smart_tool_output <- data.frame(surveydat_df,norm_scores, weighted_norm_scores,survey_priority_score = priority_scores)
    smart_tool_output <- smart_tool_output[order(smart_tool_output$survey_priority_score, decreasing = TRUE),]
    smart_tool_output$survey_priority_rank <- 1:nrow(smart_tool_output)
    smart_tool_output$source <- factor(smart_tool_output$source)
    if(is.null(save_filepath))save_filepath <- getwd()
    if(save_output)write.csv(smart_tool_output, file = paste0(save_filepath,"/",refuge_code,"_imp_smart_tool_output_",gsub("-","",Sys.Date()),".csv"), row.names = FALSE)
    attr(smart_tool_output, "refuge_code") <- refuge_code
    attr(smart_tool_output, "start_year") <- start_year
    attr(smart_tool_output, "nyears") <- nyears
    attr(smart_tool_output, "annual_staff_budget") <- annual_staff_budget
    attr(smart_tool_output, "imp_staff_budget") <- imp_staff_budget
    attr(smart_tool_output, "criteria_names") <- criteria_df$name_lev2
    attr(smart_tool_output, "criteria_weights") <- criteria_df$final_weight
    return(smart_tool_output)
    
}




#' Title
#'
#' @param smart_tool_output 
#' @param survey_schedule 
#' @param save_output 
#' @param save_filepath 
#'
#' @return
#' @export
#'
#' @examples
survey_schedule_plot <- function(smart_tool_output, survey_schedule = NULL, save_output = FALSE, save_filepath = NULL){
    old.par <- par(no.readonly = TRUE)
    nyears <- attributes(smart_tool_output)$nyears
    years <- attributes(smart_tool_output)$start_year + 0:(nyears-1)
    survey_sched_df <- read.csv(survey_schedule)
    neworder <- match(smart_tool_output$survey_name, survey_sched_df$survey_name)
    annual_surv_sched <- as.matrix(survey_sched_df[neworder,grepl("year", names(survey_sched_df))])
    annual_surv_cost <- matrix(rep(smart_tool_output$annual_weeks[neworder], nyears), ncol = nyears) * annual_surv_sched
    colnames(annual_surv_cost) <- years
    rownames(annual_surv_cost) <- paste0(rownames(smart_tool_output), " (", format(round(smart_tool_output$survey_priority_score,2),nsmall = 2),")")
    prop_budget <- annual_surv_cost/attributes(smart_tool_output)$annual_staff_budget
    pct_budget <- prop_budget * 100
    pct_budget[pct_budget %in% 0] <- NA
    cost_vals <- annual_surv_cost
    cost_vals[cost_vals %in% 0] <- NA
    ann_cost_matplot <- plot(pct_budget, cex = 0.5, xlab = "", ylab = "", main = "Annual Survey Implementation Schedule and Costs",
                             na.print = FALSE, na.col = "gray",
                             breaks = seq(0,100,20), col = terrain.colors(5), digits = 1,
                             axis.col = list(side = 1, las = 1, cex.axis = 0.7),
                             axis.row = list(side = 2, las = 2, cex.axis = 0.7),
    )
    ann_cost_matplot$key.axis$labels <- c("0",paste0(seq(20,100,20),"% of \nannual\nbudget"))
    ann_cost_matplot$key.axis$cex.axis <- 0.75
    for (i in 1:length(ann_cost_matplot$cell.polygon)){
        if(!is.na(pct_budget[i]))ann_cost_matplot$cell.text[[i]]$labels <- paste0(format(round(annual_surv_cost[i], digits=1), nsmall = 1), " wks")
    }
    if(save_output){
        if(is.null(save_filepath))save_filepath <- getwd()
        png(paste0(save_filepath,"/",attributes(smart_tool_output)$refuge_code,"_annual_survey_schedule_wcost_", gsub("-","",Sys.Date()),".png"), 
            height = 6.5, width = 9, units = "in", res = 192)
        plot_scores(ann_cost_matplot, mar = c(4, 8, 4, 4))
        dev.off()
    }else{
        plot_scores(ann_cost_matplot, mar = c(4, 8, 4, 4))
    }
    par(old.par)
    return(annual_surv_cost)
}



#' Title
#'
#' @param optim_scenario 
#' @param smart_tool_output 
#' @param save_output 
#' @param save_filepath 
#' @param max_output 
#' @param create_scatterplots 
#' @param save_scatterplots 
#'
#' @return
#' @export
#'
#' @examples
optimize_scenario <- function(optim_scenario, smart_tool_output, save_output = FALSE, save_filepath = NULL, 
                              max_output = 10000, create_scatterplots = FALSE, save_scatterplots = FALSE){
    old.par <- par(no.readonly = TRUE)
    nsurveys <- nrow(smart_tool_output)
    nall <- 2^nsurveys
    # Find all possible combinations for set of proposed surveys  
    fullset <- as.matrix(expand.grid(replicate(nsurveys, 0:1, simplify = FALSE)))
    # Calculate IMP utility metric (imp_priority_score), total IMP cost in staff weeks (imp_total_weeks), and 
    # number of selected surveys (imp_count) for each combination.
    imp_annual_weeks <- fullset %*% smart_tool_output$annual_weeks
    imp_total_weeks <- fullset %*% (smart_tool_output$annual_weeks * smart_tool_output$imp_frequency)
    imp_count <- fullset %*% rep(1,nsurveys)
    imp_priority_score <- fullset %*% smart_tool_output$survey_priority_score
    #
    if(!is.null(optim_scenario$survey_constraints)){optim_scenario$survey_constraints <- which(smart_tool_output$survey_name %in% optim_scenario$survey_constraints)}
    nconst_survs <- length(optim_scenario$survey_constraints)
    # Constrain full set of possible IMPs to final set to use for optimization
    if(is.null(optim_scenario$survey_constraints)){
        budgconst <- !(imp_total_weeks > attributes(smart_tool_output)$imp_staff_budget)
        if(!optim_scenario$constrain_budget)budgconst <-  rep(TRUE,nall)
        scenario_output <- cbind(fullset,imp_annual_weeks,imp_total_weeks,imp_count,imp_priority_score)[budgconst,]
        colnames(scenario_output) <- c(smart_tool_output$survey_name,c("imp_annual_weeks","imp_total_weeks","imp_count","imp_priority_score"))
        scenario_output <- as.data.frame(scenario_output[order(scenario_output[,"imp_priority_score"],decreasing = TRUE),])
    }else{
        selconst <- !(fullset[,optim_scenario$survey_constraints]%*%rep(1,nconst_survs)<nconst_survs)
        budgconst <- !(imp_total_weeks > attributes(smart_tool_output)$imp_staff_budget)
        if(!optim_scenario$constrain_budget)budgconst <-  rep(TRUE,nall)
        scenario_output <- cbind(fullset,imp_annual_weeks,imp_total_weeks,imp_count,imp_priority_score)[selconst & budgconst,]
        colnames(scenario_output) <- c(smart_tool_output$survey_name,c("imp_annual_weeks","imp_total_weeks","imp_count","imp_priority_score"))
        scenario_output <- as.data.frame(scenario_output[order(scenario_output[,"imp_priority_score"],decreasing = TRUE),])
    }
    if(is.null(save_filepath))save_filepath <- getwd()
    if(save_output){
        if(nrow(scenario_output)>max_output)out <- scenario_output[1:max_output,]
        write.csv(out, file = paste0(save_filepath, "/",attributes(smart_tool_output)$refuge_code,"_portfolio_output_scenario_",
                                     optim_scenario$scenario_name, "_", gsub("-","",Sys.Date()),".csv"), 
                  row.names = FALSE)
    }
    attr(scenario_output,"refuge_code") <- attributes(smart_tool_output)$refuge_code
    attr(scenario_output,"scenario") <- optim_scenario
    attr(scenario_output,"nsurveys") <- nrow(smart_tool_output)
    attr(scenario_output,"survey_priority_scores") <- smart_tool_output$survey_priority_score
    if(create_scatterplots){
        col_bins <- cut(scenario_output$imp_priority_score, breaks = quantile(scenario_output$imp_priority_score, probs = c(0,0.01, 0.025,0.05,0.25,0.50,0.75,0.95,0.975,0.99, 1), na.rm=TRUE),
                        include.lowest = TRUE)
        #
        xy_scatter <- function(scenario_output, col_bins, imp_priority_score, imp_total_weeks){
            plot(scenario_output$imp_total_weeks,scenario_output$imp_priority_score,type = "p",pch = 21, xlab = "Total IMP Weeks", ylab = "Total Utility",
                 bg = terrain.colors(nlevels(col_bins))[col_bins], cex = log(scenario_output$imp_count/10)*2,
                 ylim = c(min(scenario_output$imp_priority_score), max(imp_priority_score)), 
                 xlim = c(min(scenario_output$imp_total_weeks),max(imp_total_weeks)), 
                 frame.plot = FALSE)
            abline(v=attributes(smart_tool_output)$imp_staff_budget)
            abline(h=max(imp_priority_score), lty = 2)
            points(max(imp_total_weeks), max(imp_priority_score), pch = 21, bg = "black", cex = log(nsurveys/10)*2)
        }
        s3d_asp1 <- function(scenario_output, col_bins, imp_priority_score, imp_total_weeks, nsurveys){
            s3d <- scatterplot3d::scatterplot3d(scenario_output$imp_count, scenario_output$imp_total_weeks, scenario_output$imp_priority_score, 
                                                pch = 21, type = "h", lty.hplot = 3,
                                                cex.axis = .75,
                                                xlim = c(min(scenario_output$imp_count),nsurveys), 
                                                ylim = c(min(scenario_output$imp_total_weeks),max(imp_total_weeks)), 
                                                zlim = c(min(scenario_output$imp_priority_score),max(imp_priority_score)),
                                                cex.symbols = log(scenario_output$imp_count/10)/0.5, 
                                                scale.y = 1, 
                                                angle = 120, 
                                                asp = 2,
                                                bg = terrain.colors(nlevels(col_bins))[col_bins],
                                                xlab = "Number of Surveys", ylab = "Total IMP Weeks",
                                                zlab = "Total Utility")
            s3d$points3d(max(imp_count), max(imp_total_weeks), max(imp_priority_score),
                         col = "black", type = "h", pch = 21, bg = "black", cex = log(max(imp_count)/10)/0.5)
            return(s3d)
        }
        s3d_asp2 <- function(scenario_output, col_bins, imp_priority_score, imp_total_weeks, nsurveys){
            s3d <- scatterplot3d::scatterplot3d(scenario_output$imp_count, scenario_output$imp_total_weeks, scenario_output$imp_priority_score, 
                                                pch = 21, type = "h", lty.hplot = 3,
                                                cex.axis = .75,
                                                xlim = c(min(scenario_output$imp_count),nsurveys), 
                                                ylim = c(min(scenario_output$imp_total_weeks),max(imp_total_weeks)), 
                                                zlim = c(min(scenario_output$imp_priority_score),max(imp_priority_score)),
                                                cex.symbols = log(scenario_output$imp_count/10)/0.5, 
                                                scale.y = 1, 
                                                angle = 25, 
                                                asp = 3,
                                                bg = terrain.colors(nlevels(col_bins))[col_bins],
                                                xlab = "Number of Surveys", ylab = "Total IMP Weeks",
                                                zlab = "Total Utility")
            s3d$points3d(max(imp_count), max(imp_total_weeks), max(imp_priority_score),
                         col = "black", type = "h", pch = 21, bg = "black", cex = log(max(imp_count)/10)/0.5)
            return(s3d)
        }
        
        if(save_scatterplots){
            constrain_budget <- attributes(scenario_output)$scenario$constrain_budget
            png(paste0(save_filepath, "/",attributes(smart_tool_output)$refuge_code,"_portfolio_scatter2d_scenario_",optim_scenario$scenario_name, "_", gsub("-","",Sys.Date()),".png"),
                height = 6.5, width = 6.5, units = "in", res = 192)
            xy_scatter(scenario_output, col_bins, imp_priority_score, imp_total_weeks)
            dev.off()
            png(paste0(save_filepath, "/",attributes(smart_tool_output)$refuge_code,"_portfolio_scatter3d_asp1_scenario_",optim_scenario$scenario_name, "_", gsub("-","",Sys.Date()),".png"),
                height = 6.5, width = 9, units = "in", res = 192)
            s3d_asp1(scenario_output, col_bins, imp_priority_score, imp_total_weeks, nsurveys)
            dev.off()
            png(paste0(save_filepath, "/",attributes(smart_tool_output)$refuge_code,"_portfolio_scatter3d_asp2_scenario_",optim_scenario$scenario_name, "_", gsub("-","",Sys.Date()),".png"),
                height = 6.5, width = 9, units = "in", res = 192)
            s3d_asp2(scenario_output, col_bins, imp_priority_score, imp_total_weeks, nsurveys)
            dev.off()
        }else{
            xy_scatter(scenario_output, col_bins, imp_priority_score, imp_total_weeks)
            s3d_asp1(scenario_output, col_bins, imp_priority_score, imp_total_weeks, nsurveys)
            s3d_asp2(scenario_output, col_bins, imp_priority_score, imp_total_weeks, nsurveys)
        }
    }
    par(old.par)
    return(scenario_output)
}
