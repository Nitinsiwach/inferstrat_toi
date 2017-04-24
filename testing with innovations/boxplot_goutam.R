#Creating box plots on adamcorrectfile
file <- read.csv("adamcorrectfile.csv")
file$month <- as.integer(substr(as.character(file$IssueDate), 4,5))
gnp_true <- file[file$GNP_Jacket_comb == 1,]
gnp_false <- file[file$GNP_Jacket_comb != 1,]
split_data <- list(gnp_true, gnp_false)
selection <- gnp_false
periods <- list(4:11, 4:9, c(10,11,12,1,2), c(12,1,2), c(4:12,1:2))
child_data <- function(index) {
  selection <- selection[selection$month %in% index, ]
  return(selection)
}

final_data <- mapply(child_data, periods, SIMPLIFY = F)
i <- 1:length(periods) 
bp_df <- function(x) {
  return(final_data[[x]][,"Avg.Speed"])
  }
bp_list <- mapply(bp_df, i)
names(bp_list) <- c("apr - nov", "apr - sep", "oct - feb", "dec - feb", "apr - feb")
boxplot(bp_list, las = 2, col = c("red", "violet", "blue", "yellow", "turquoise"), range = 1, varwidth = T)
