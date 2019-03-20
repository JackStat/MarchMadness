#' @title checkSubmission
#'
#' @description Verifies that the submission file is set up properly
#'
#'
#' @param submission
#' @export


checkSubmission <- function(submission){
  if(nrow(submission) != 2278){
    stop("Your submission file should have 2278 rows")
  }
  if(ncol(submission) != 2){
    stop("Your submission file should have 2 columns (ID and Pred)")
  }
  if("ID" %in% names(submission)){
    stop("Your submission file needs to have an ID column")
  }
  if("Pred" %in% names(submission)){
    stop("Your submission file needs to have an Pred column")
  }
  if(class(submission$Pred) != "numeric"){
    stop("Your Pred column needs to be numeric")
  }

}
