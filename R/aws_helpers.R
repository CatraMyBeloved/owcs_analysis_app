#' -----------------------------------------------------------------------------
#' aws_helpers.R
#' 
#' Description: Contains helper functions to interact with AWS S3 storage for
#'  persistent data storage
#' 
#' Author: CatraMyBeloved
#' Date Created: 03-04-2025
#' Last Modified: 02-04-2025
#' -----------------------------------------------------------------------------


library(aws.s3)


download_from_s3 <- function(bucket, key, local_path = NULL) {
  if (is.null(local_path)) {
    local_path <- basename(key)
  }
  
  # Attempt to download the file
  result <- tryCatch({
    aws.s3::save_object(
      object = key,
      bucket = bucket,
      file = local_path
    )
    return(TRUE)
  }, error = function(e) {
    return(FALSE)
  })
  
  return(result)
}
upload_to_s3 <- function(bucket, file_path, key = NULL) {
  if (is.null(key)) {
    key <- basename(file_path)
  }
  
  # Attempt to upload the file
  result <- tryCatch({
    aws.s3::put_object(
      file = file_path,
      object = key,
      bucket = bucket
    )
    return(TRUE)
  }, error = function(e) {
    return(FALSE)
  })
  
  return(result)
}