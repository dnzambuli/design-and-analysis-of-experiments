# Update the function to ensure the output from apply is always a matrix
calculate_ss <- function(data) {
  total <- sum(data)
  cf <- total^2 / length(data)
  total_ss <- sum((data)^2) - cf
  
  # Ensuring matrix output by checking dimensions
  safe_apply <- function(data, margin, FUN) {
    result <- apply(data, margin, FUN)
    if (!is.matrix(result)) {  # If result is not a matrix, convert it
      result <- matrix(result, nrow = 1)
    }
    return(result)
  }
  
  rep_ss <- sum(colSums(safe_apply(data, c(1, 2), function(x) sum(x)^2))) / (2*3) - cf
  fert_ss <- sum(colSums(safe_apply(data, c(2), function(x) sum(x)^2))) / (4*3) - cf
  error_a_ss <- sum(colSums(safe_apply(data, 2, function(x) sum(x)^2))) / 3 - cf - rep_ss - fert_ss
  
  irr_ss <- sum(colSums(safe_apply(data, c(1), function(x) sum(x)^2))) / (4*2) - cf
  fert_irr_ss <- sum(colSums(safe_apply(data, 1, function(x) sum(x)^2))) / 4 - cf
  error_b_ss <- total_ss - rep_ss - fert_ss - error_a_ss - irr_ss - fert_irr_ss
  
  return(c(total_ss, rep_ss, fert_ss, error_a_ss, irr_ss, fert_irr_ss, error_b_ss))
}

# Assuming data is a matrix
data <- matrix(agri_df$Yields, nrow = nrow(agri_df), ncol = ncol(agri_df))
ss_values <- calculate_ss(data)
names(ss_values) <- c("total_ss", "rep_ss", "fert_ss", "error_a_ss", "irr_ss", "fert_irr_ss", "error_b_ss")
ss_values


df_rep = length(as.numeric(unique(agri_df$replicates))) -1 
df_fertilizer = 1
df_irrigation = 2
df_fer_irr = df_fertilizer * df_irrigation
df_error = length(as.numeric(unique(agri_df$treatments))) - 1
df_error_fer = df_rep * df_fertilizer
df_error_irr = df_rep * df_fertilizer * df_irrigation

ms_fertilizer = ss_values["fert_ss"]/ df_fertilizer
ms_error_fert = ss_values["error_a_ss"]/ df_error_fer
ms_irrigation = ss_values["irr_ss"]/ df_irrigation
ms_irr_fer =  ss_values["fert_irr_ss"]/ df_fer_irr
ms_error_irr = ss_values["error_b_ss"]/ df_irrigation

f_fertilizer = ms_fertilizer/ms_error_fert
f_irrigation = ms_irrigation/ms_error_irr
f_irr_fer = ms_irr_fer/ms_error_irr

p_fert = pf(f_fertilizer, df_fertilizer, df_error_fer, lower.tail = FALSE)
p_irr = pf(f_irrigation, df_irrigation, df_error_irr, lower.tail = FALSE)
p_irr_fer = pf(f_irr_fer, df_fer_irr, df_error_irr, lower.tail = FALSE)


Source = c('Replicate', 'Fertilizer', 'Error(a)', 'Irrigation', 'FertilizerxIrrigation', 'Error(b)', 'Total')
DF = c(df_rep, df_fertilizer, df_error_fer, df_irrigation, df_fer_irr, df_error_fer, df_error)
SS = c(ss_values["rep_ss"], ss_values["fert_ss"], ss_values["error_a_ss"], ss_values["irr_ss"], ss_values["fert_irr_ss"], ss_values["error_b_ss"], ss_values["total_ss"])
MS = c(NA, ms_fertilizer, ms_error_fert, ms_irrigation, ms_irr_fer, NA, NA)
FStat = c(NA, f_fertilizer, NA, f_irrigation, f_irr_fer, NA, NA)
PValue = c(NA, p_fert, NA, p_irr, p_irr_fer, NA, NA)

data.frame(
  Source,DF, SS, MS, FStat, PValue
)
