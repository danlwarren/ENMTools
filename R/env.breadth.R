#' Calculates breadth of a model in environment space using latin hypercube sampling
#'
#' @param model An enmtools.model object or a model object that can be projected using the predict() function
#' @param env A raster or raster stack of environmental data.
#' @param tolerance How close do successive overlap metrics have to be before we decide we're close enough to the final answer
#' @param max.reps Maximum number of attempts that will be made to find suitable starting conditions
#'
#' @export env.breadth

env.breadth <- function(model, env, tolerance = .001, max.reps = 10){
  
  if(inherits(model, "enmtools.model")){
    model <- model$model
  }
  
  # Setting it up so we can handle either a set of rasters or a list of minima and maxima
  if(inherits(env, c("raster", "RasterStack", "RasterBrick", "RasterLayer"))){
    mins <- minValue(env)
    maxes <- maxValue(env)
  } else if (inherits(env, "list")){
    mins <- unlist(lapply(env, min))
    maxes <- unlist(lapply(env, max))
  }
  
  # These two are tracking whether we have good enough starting conditions
  # and how many times we've tried
  continue <- FALSE
  n.reps <- 0
  
  # Some of the DM and BC models were barfing on certain starting conditions
  # so I've put this bit in here to make it try a few times.
  while(continue == FALSE & n.reps < max.reps){
    
    # Draw a starting latin hypercube scheme
    this.lhs <- randomLHS(10000, length(names(env)))
    predict.table <- t(t(this.lhs) * (maxes  - mins) + mins)
    
    # Use that sample space to get a starting overlap value
    colnames(predict.table) <- names(env)
    pred <- as.numeric(predict(model, data.frame(predict.table), type = "response"))
    
    pred[which(pred == 0)] <- 1e-40
    
    # No meaningful prediction
    if(sd(pred) == 0){
      output <- list(env.B1 = NA,
                     env.B2 = NA)
      
      return(output)
    }
    
    this.B1 <- calc.B1(pred)
    this.B2 <- calc.B2(pred)
    
    # Check to see if the value is usable, roll again if not
    if(!is.nan(this.B1) & !is.nan(this.B2)){
      continue <- TRUE
    } else {
      n.reps <- n.reps + 1
    }
  }
  
  # If we fail to find useful starting conditions we'll just barf an NA and give up
  if(n.reps == max.reps){
    cat("\n\nCould not find suitable starting conditions for environmental overlap, returning NA\n\n")
    this.B1 <- NA
    this.B2 <- NA
  } else {
    
    # So here we've got good starting conditions and we're going to keep going
    # with the LHS design until we get a minimum difference between subsequent
    # samples (delta < tolerance)
    delta <- 1
    
    # print(paste(this.diff, delta))
    
    while(delta > tolerance){
      
      # Keep track of our last value
      # old.diff <- this.diff
      
      # Add 1000 rows to the LHS and build a new predict table
      this.lhs <- randomLHS(10000, length(names(env)))
      predict.table <- t(t(this.lhs) * (maxes  - mins) + mins)
      colnames(predict.table) <- names(env)
      
      # Make new predictions and recalculate metrics
      pred <- predict(model, data.frame(predict.table), type = "response")
      
      pred[which(pred == 0)] <- 1e-40
      
      # Can't make a prediction, return NAs and die
      if(sd(pred) == 0){
        output <- list(env.D = NA,
                       env.I = NA,
                       env.cor = NA)
        
        return(output)
      }
      
      this.B1 <- c(this.B1, calc.B1(pred))
      this.B2 <- c(this.B2, calc.B2(pred))
      
      
      
      # Calculate delta for this iteration
      delta <- max(c(abs(mean(this.B1) - mean(this.B1[-length(this.B1)])),
                     abs(mean(this.B2) - mean(this.B2[-length(this.B2)]))), na.rm=TRUE)
      
    }
  }
  
  output <- list(output <- list(env.B1 = mean(this.B1),
                                env.B2 = mean(this.B2)))
  
  return(output)
}



