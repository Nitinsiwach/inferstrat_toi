#Creating box plots on adamcorrectfile
file <- read.csv("adamcorrectfile.csv")
file$month <- as.integer(substr(as.character(file$IssueDate), 4,5))
gnp_true <- file[file$GNP_Jacket_comb == 1,]
gnp_false <- file[file$GNP_Jacket_comb != 1,]
split_data <- list(gnp_true, gnp_false)
selection <- gnp_false
periods <- list(c(10,11,12,1,2), c(4:12,1:2))
child_data <- function(index) {
  selection <- selection[selection$month %in% index, ]
  return(selection)
}

final_data <- mapply(child_data, periods, SIMPLIFY = F)
i <- 1:length(periods) 
bp_df_gnpfalse <- function(x) {
  return(final_data[[x]][,"Avg.Speed"])
}
bp_list_gnpfalse <- mapply(bp_df_gnpfalse, i)
selection <- gnp_true
periods <- list(c(10,11,12,1,2), c(4:12,1:2))
child_data <- function(index) {
  selection <- selection[selection$month %in% index, ]
  return(selection)
}

final_data <- mapply(child_data, periods, SIMPLIFY = F)
i <- 1:length(periods) 
bp_df_gnptrue <- function(x) {
  return(final_data[[x]][,"Avg.Speed"])
}
bp_list_gnptrue <- mapply(bp_df_gnptrue, i)
names(bp_list_gnpfalse) <- c("oct - feb", "apr - feb")
names(bp_list_gnptrue) <- c("oct - feb", "apr - feb")
df_of <- list(avgs.gnpf = bp_list_gnpfalse[[1]], avgs.gnpt = bp_list_gnptrue[[1]])
df_af <- list(avgs.gnpf = bp_list_gnpfalse[[2]], avgs.gnpt = bp_list_gnptrue[[2]])
names(df_of) <- c("GNP = F", "GNP = T")
names(df_af) <- c("GNP = F", "GNP = T")
boxplot_of <- boxplot(df_of, las = 2, col = c("red", "violet", "blue", "yellow", "turquoise"),range= 1, varwidth = T)
boxplot_af <- boxplot(df_af, las = 2, col = c("red", "violet", "blue", "yellow", "turquoise"), range = 1, varwidth = T)
