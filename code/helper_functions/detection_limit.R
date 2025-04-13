detection_limit <- function(measurements_matrix, dl_matrix) {
  #' Apply detection limits from Gitter to set the precision
  #'
  #' @measurements_matrix a data frame with concentrations, all columns must be numerical
  #' @dl_matrix a data frame with detection limits, all columns must be numerical, must have the same dimensions as `measurements_matrix`
  #' @return a data frame with the significant numbers adjusted based on detection limit, numerical
  #' @examples
  #' adjusted_concentrations <- detection_limit(measurements_matrix = measurements, dl_matrix = dl)
  stopifnot(
    all.equal(dim(measurements_matrix), dim(dl_matrix)),
    all.equal(rownames(measurements_matrix), rownames(dl_matrix)),
    apply(dl_matrix,2,is.numeric),
    apply(measurements_matrix,2,is.numeric)
  )
    output_matrix <- matrix(data = NA, nrow = nrow(dl_matrix), ncol = ncol(dl_matrix))
    for (i in 1:ncol(dl_matrix)) {
      for (j in 1:nrow(dl_matrix)) {
        if(dl_matrix[j,i] < 1) { 
          output_matrix[j,i] <- format(round(measurements_matrix[j,i], 1), nsmall = 1)
        } else {
          output_matrix[j,i] <- format(round(measurements_matrix[j,i], 0), nsmall = 0)
        }
      }
    }
  output_matrix <- apply(output_matrix,2,as.numeric)
  output_matrix <- as.data.frame(output_matrix)
  return(output_matrix)
}
