#' Takes an emtools model object and runs tests of goodness-of-fit
#'
#' @param model An enmtools.model object
#' @param env A set of environmental layers.  Should be the same layers used for the empirical model.
#' @param eval.data Which data to evaluate model fit on.  Can choose "simulated" for an RTS-style test, "test.data" for a Bohl et al.-style test.
#' @param eval.metric Metric or vector of metrics to use for model evaluation.
#' @param bias Method for including bias in randomization steps.  Either a spatRaster representing relative sampling effort in the study area or "autocorrelation" to generate replicates with similar autocorrelation to empirical occurrence data.
#' @param nreps Number of replicates to perform
#' @param keep.reps Whether to keep models for individual randomization reps
#'
#' @return An enmtools.sigfit object containing p values and useful plots
#'
#' @examples
#' \donttest{
#' }


model.sigtest <- function(model, eval.data = "sim", eval.metric = "AUC", bias = NA, nreps = 100, keep.reps = FALSE){
  
  # Get empirical eval metrics from model object
  
  # Extract information needed for reps from model object
  # What type of mode
  # Formula
  # Presence data
  # Number of background points
  # Get mask of study area from model$suitability
  
  # Check that study area and bias layer (if provide) overlap, clip bias estimate by mask
  # Do same for env
  
  # Set up output object
  
  # Initialize layers for tracking mean and variance of replicate models
  
  # Iterate over nreps
  for(i in 1:nreps){
    
    # Simulate points either from bias layer or autocorrelation
    
    # Build model using same settings as empirical model
    
    # Project model onto env
    
    # Add this rep model to mean and variance rasters - dig up online variance algorithm
    
    for(j in 1:length(eval.metric)){ 
      
      # Get eval metrics for this rep, stuff into vector
      
    }
    
    # Stuff rep into output object if keep.reps = TRUE
    
  }
  
  # Calculate p values and store
  
  # Make plots of eval metric distributions and empirical values
  
  # Stuff mean and variance rasters into output object
  
}