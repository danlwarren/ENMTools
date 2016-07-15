#' background.test Conduct a background test (also called a similarity test), as described in Warren et al. 2008.
#' This test can either be run as an asymmetric test (species.1 vs background of species.2) or as a symmetric test
#' (background of species.1 vs background of species.2).  For GLM, Bioclim, and Domain models the replicates will be
#' constructed from the background points supplied for each species.  For Maxent, the replicates will be constructed
#' from the range rasters stored in the enmtools.species objects.
#'
#' @param species.1 An emtools.species object from which presence points (asymmetric) or background (symmetric) will be sampled.
#' @param species.2 An enmtools.species object from which background will be sampled.
#' @param env A RasterLayer or RasterStack object containing environmental data
#' @param type The type of model to construct, currently accepts "glm", "mx", "bc", or "dm"
#' @param f A function to use for model fitting.  Only required for GLM models at the moment.
#' @param nreps Number of replicates to perform
#' @param ... Additional arguments to be passed to model fitting functions.
#'
#' @return results A list containing replicates, models for the empirical data, and summary statistics and plots.
#'
#' @keywords background, equivalency, enmtools, hypothesis testing
#'
#' @export background.test
#' @export background.precheck
#' @export print.background.test
#' @export summary.background.test
#' @export plot.background.test
#'
#' @examples
#' background.test(ahli, allogus, env, type = "glm", f = layer.1 + layer.2 + layer.3, nreps = 10, type = "asymmetric", ...)
#'

background.test <- function(species.1, species.2, env, type, f = NULL, nreps = 99, test.type = "asymmetric", ...){

#   if(type == "mx"){
#     bg.source = "range"
#   }

  background.precheck(species.1, species.2, env, type, f, nreps, test.type)

  if(type == "glm"){

    cat("\nBuilding empirical models...\n")
    empirical.species.1.model <- enmtools.glm(f, species.1, env, ...)
    empirical.species.2.model <- enmtools.glm(f, species.2, env, ...)
    empirical.overlap <- raster.overlap(empirical.species.1.model, empirical.species.2.model)
    reps.overlap <- unlist(empirical.overlap)

    cat("\nBuilding replicate models...\n")
    for(i in 1:nreps){
      cat(paste("\nReplicate", i, "...\n"))

      rep.species.1 <- species.1
      rep.species.2 <- species.2


      if(test.type == "symmetric"){

        #background sampling for species 1, only run when we're doing a symmetric test
        combined.points <- rbind(rep.species.1$presence.points, rep.species.1$background.points)
        sample.vector <- sample(nrow(combined.points))
        combined.points <- combined.points[sample.vector,]
        rep.species.1$presence.points <- combined.points[1:nrow(species.1$presence.points),]
        rep.species.1$background.points <- combined.points[(nrow(species.1$presence.points) + 1):nrow(combined.points),]

      }

      # Background sampling for species 2, run regardless of the type of test
      combined.points <- rbind(rep.species.2$presence.points, rep.species.2$background.points)
      sample.vector <- sample(nrow(combined.points))
      combined.points <- combined.points[sample.vector,]
      rep.species.2$presence.points <- combined.points[1:nrow(species.2$presence.points),]
      rep.species.2$background.points <- combined.points[(nrow(species.2$presence.points) + 1):nrow(combined.points),]

      # Building the models for reps
      rep.species.1.glm <- enmtools.glm(f, rep.species.1, env, ...)
      rep.species.2.glm <- enmtools.glm(f, rep.species.2, env, ...)

      # Appending to results
      reps.overlap <- rbind(reps.overlap, unlist(raster.overlap(rep.species.1.glm, rep.species.2.glm)))
    }
  }

  if(type == "mx"){

    cat("\nBuilding empirical models...\n")
    empirical.species.1.model <- enmtools.maxent(species.1, env, ...)
    empirical.species.2.model <- enmtools.maxent(species.2, env, ...)
    empirical.overlap <- raster.overlap(empirical.species.1.model, empirical.species.2.model)
    reps.overlap <- unlist(empirical.overlap)

    cat("\nBuilding replicate models...\n")
    for(i in 1:nreps){
      cat(paste("\nReplicate", i, "...\n"))

      rep.species.1 <- species.1
      rep.species.2 <- species.2

      if(test.type == "symmetric"){

        #background sampling for species 1, only run when we're doing a symmetric test
        combined.points <- rbind(rep.species.1$presence.points, rep.species.1$background.points)
        sample.vector <- sample(nrow(combined.points))
        combined.points <- combined.points[sample.vector,]
        rep.species.1$presence.points <- combined.points[1:nrow(species.1$presence.points),]
        rep.species.1$background.points <- combined.points[(nrow(species.1$presence.points) + 1):nrow(combined.points),]

      }

      # Background sampling for species 2, run regardless of the type of test
      combined.points <- rbind(rep.species.2$presence.points, rep.species.2$background.points)
      sample.vector <- sample(nrow(combined.points))
      combined.points <- combined.points[sample.vector,]
      rep.species.2$presence.points <- combined.points[1:nrow(species.2$presence.points),]
      rep.species.2$background.points <- combined.points[(nrow(species.2$presence.points) + 1):nrow(combined.points),]

      rep.species.1.mx <- enmtools.maxent(rep.species.1, env, ...)
      rep.species.2.mx <- enmtools.maxent(rep.species.2, env, ...)

      # Appending to results
      reps.overlap <- rbind(reps.overlap, unlist(raster.overlap(rep.species.1.mx, rep.species.2.mx)))
    }
  }

  if(type == "bc"){

    cat("\nBuilding empirical models...\n")

    empirical.species.1.model <- enmtools.bc(species.1, env, ...)
    empirical.species.2.model <- enmtools.bc(species.2, env, ...)
    empirical.overlap <- raster.overlap(empirical.species.1.model, empirical.species.2.model)
    reps.overlap <- unlist(empirical.overlap)

    cat("\nBuilding replicate models...\n")
    for(i in 1:nreps){
      cat(paste("\nReplicate", i, "...\n"))

      rep.species.1 <- species.1
      rep.species.2 <- species.2

      if(test.type == "symmetric"){

        #background sampling for species 1, only run when we're doing a symmetric test
        combined.points <- rbind(rep.species.1$presence.points, rep.species.1$background.points)
        sample.vector <- sample(nrow(combined.points))
        combined.points <- combined.points[sample.vector,]
        rep.species.1$presence.points <- combined.points[1:nrow(species.1$presence.points),]
        rep.species.1$background.points <- combined.points[(nrow(species.1$presence.points) + 1):nrow(combined.points),]

      }

      # Background sampling for species 2, run regardless of the type of test
      combined.points <- rbind(rep.species.2$presence.points, rep.species.2$background.points)
      sample.vector <- sample(nrow(combined.points))
      combined.points <- combined.points[sample.vector,]
      rep.species.2$presence.points <- combined.points[1:nrow(species.2$presence.points),]
      rep.species.2$background.points <- combined.points[(nrow(species.2$presence.points) + 1):nrow(combined.points),]

      rep.species.1.bc <- enmtools.bc(rep.species.1, env, ...)
      rep.species.2.bc <- enmtools.bc(rep.species.2, env, ...)

      # Appending to results
      reps.overlap <- rbind(reps.overlap, unlist(raster.overlap(rep.species.1.bc, rep.species.2.bc)))
    }
  }

  if(type == "dm"){

    cat("\nBuilding empirical models...\n")
    empirical.species.1.model <- enmtools.dm(species.1, env, ...)
    empirical.species.2.model <- enmtools.dm(species.2, env, ...)
    empirical.overlap <- raster.overlap(empirical.species.1.model, empirical.species.2.model)
    reps.overlap <- unlist(empirical.overlap)

    cat("\nBuilding replicate models...\n")
    for(i in 1:nreps){
      cat(paste("\nReplicate", i, "...\n"))

      rep.species.1 <- species.1
      rep.species.2 <- species.2

      if(test.type == "symmetric"){

        #background sampling for species 1, only run when we're doing a symmetric test
        combined.points <- rbind(rep.species.1$presence.points, rep.species.1$background.points)
        sample.vector <- sample(nrow(combined.points))
        combined.points <- combined.points[sample.vector,]
        rep.species.1$presence.points <- combined.points[1:nrow(species.1$presence.points),]
        rep.species.1$background.points <- combined.points[(nrow(species.1$presence.points) + 1):nrow(combined.points),]

      }

      # Background sampling for species 2, run regardless of the type of test
      combined.points <- rbind(rep.species.2$presence.points, rep.species.2$background.points)
      sample.vector <- sample(nrow(combined.points))
      combined.points <- combined.points[sample.vector,]
      rep.species.2$presence.points <- combined.points[1:nrow(species.2$presence.points),]
      rep.species.2$background.points <- combined.points[(nrow(species.2$presence.points) + 1):nrow(combined.points),]

      rep.species.1.dm <- enmtools.dm(rep.species.1, env, ...)
      rep.species.2.dm <- enmtools.dm(rep.species.2, env, ...)

      # Appending to results
      reps.overlap <- rbind(reps.overlap, unlist(raster.overlap(rep.species.1.dm, rep.species.2.dm)))
    }
  }

  rownames(reps.overlap) <- c("empirical", paste("rep", 1:nreps))

  p.values <- apply(reps.overlap, 2, function(x) 1 - mean(x > x[1]))

  d.plot <- qplot(reps.overlap[2:nrow(reps.overlap),"D"], geom = "density", fill = "density", alpha = 0.5) +
    geom_vline(xintercept = reps.overlap[1,"D"], linetype = "longdash") +
    xlim(0,1) + guides(fill = FALSE, alpha = FALSE) + xlab("D")

  i.plot <- qplot(reps.overlap[2:nrow(reps.overlap),"I"], geom = "density", fill = "density", alpha = 0.5) +
    geom_vline(xintercept = reps.overlap[1,"I"], linetype = "longdash") +
    xlim(0,1) + guides(fill = FALSE, alpha = FALSE) + xlab("I")

  cor.plot <- qplot(reps.overlap[2:nrow(reps.overlap),"rank.cor"], geom = "density", fill = "density", alpha = 0.5) +
    geom_vline(xintercept = reps.overlap[1,"rank.cor"], linetype = "longdash") +
    xlim(-1,1) + guides(fill = FALSE, alpha = FALSE) + xlab("Rank Correlation")

  # mean(bg.dm[,1] > bg.dm[1,1])

  output <- list(reps.overlap = reps.overlap,
                 p.values = p.values,
                 empirical.species.1.model = empirical.species.1.model,
                 empirical.species.2.model = empirical.species.1.model,
                 d.plot = d.plot,
                 i.plot = i.plot,
                 cor.plot = cor.plot)

  class(output) <- "background.test"

  return(output)

}

background.precheck <- function(species.1, species.2, env, type, f, nreps, test.type){

  if(!"enmtools.species" %in% class(species.1)){
    stop("Species.1 is not an enmtools.species object!")
  }

  if(!"enmtools.species" %in% class(species.2)){
    stop("Species.2 is not an enmtools.species object!")
  }

  if(!grepl("Raster", class(env))){
    stop("Environmental layers are not a RasterLayer or RasterStack object!")
  }

  if(type == "glm"){
    if(is.null(f)){
      stop("Type is set to GLM and no formula has been supplied!")
    }

    if(!"formula" %in% class(f)){
      stop("Type is set to GLM and f is not a formula object!")
    }
  }

#   if(type == "mx"){
#     if(!grepl("raster", class(species.1$range), ignore.case = TRUE)){
#       stop("Maxent cannot use points for background, and species 1 does not have a range raster!")
#     }
#
#     if(!grepl("raster", class(species.2$range), ignore.case = TRUE)){
#       stop("Maxent cannot use points for background, and species 2 does not have a range raster!")
#     }
#   }


  if(!type %in% c("glm", "mx", "bc", "dm")){
    stop(paste("Model type", type, "not understood! Select either bc, dm, mx, or glm."))
  }

  check.species(species.1)

  if(!any(c("data.frame") %in% class(species.1$presence.points))){
    stop("Species 1 presence.points do not appear to be an object of class data.frame")
  }

  if(!any(c("data.frame") %in% class(species.1$background.points))){
    stop("Species 1 background.points do not appear to be an object of class data.frame")
  }

  check.species(species.2)

#   if(bg.source == "points"){
#
#     if(!any(c("data.frame") %in% class(species.2$presence.points))){
#       stop("Species 2 presence.points do not appear to be an object of class data.frame")
#     }
#
#     if(!any(c("data.frame") %in% class(species.2$background.points))){
#       stop("Species 2 background.points do not appear to be an object of class data.frame")
#     }
#
#     if(any(!colnames(species.1$background.points) %in% colnames(species.2$background.points))){
#       stop("Column names for species background points do not match!")
#     }
#
#     if(any(!colnames(species.1$presence.points) %in% colnames(species.2$presence.points))){
#       stop("Column names for species presence points do not match!")
#     }
#   }

#   if(bg.source == "range"){
#     if(!grepl("raster", class(species.1$range), ignore.case = TRUE)){
#       stop("Species 1 does not have a range raster!")
#     }
#
#     if(!grepl("raster", class(species.2$range), ignore.case = TRUE)){
#       stop("Species 2 does not have a range raster!")
#     }
#   }


}


summary.background.test <- function(bg){

  cat("\n\nbackground test p-values:\n")
  print(bg$p.values)

  cat("\n\nReplicates:\n")
  print(kable(head(bg$reps.overlap)))

  plot(bg)

}

print.background.test <- function(bg){

  summary(bg)

}

plot.background.test <- function(bg){
  grid.arrange(bg$d.plot, bg$i.plot, bg$cor.plot)
}
