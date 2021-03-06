% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/survey_priority_score_plots.R
\name{priority_score_barchart}
\alias{priority_score_barchart}
\title{Plot Bar Chart of Survey Priority Scores}
\usage{
priority_score_barchart(
  smart_tool_output,
  combined = FALSE,
  save_output = FALSE,
  save_filepath = NULL
)
}
\arguments{
\item{smart_tool_output}{A data.frame containing SMART tool inputs and outputs including survey priority scores and ranks returned by the \code{imp_smart_tool} function.}

\item{combined}{Logical value controlling whether to add survey priority scores excluding cost to the plot. Default is FALSE.}

\item{save_output}{Logical value controlling whether output is saved as CSV. Default is FALSE.}

\item{save_filepath}{File path to folder where output is saved. File names are internally generated.}
}
\value{
Bar plot
}
\description{
Plot Bar Chart of Survey Priority Scores
}
