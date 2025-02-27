### Isla's version of ENMTools functions ###

# The identity.test() function from ENMTools was producing the following error:
# --> long vectors not supported yet: complete.cases.c:192



# complete.cases ----------------------------------------------------------


# adapting the complete.cases function to work with my large datasets

my.complete.cases <- function(df){
  
  # initialize empty dataframe for complete cases of df
  df_cc <- data.frame(COL1 = double(), COL2 = double())
  
  # set colnames of df_cc to the colnames from df
  colnames(df_cc) <- colnames(df)
  
  # check each row of df, if complete case, append to df_cc
  i <- 1
  while (i <= nrow(df)) {
    if (i + 999999 <= nrow(df)) {
      temp <- df[i:(i+999999),]
      cc <- temp[complete.cases(temp),]
      df_cc <- rbind(df_cc, cc)
    }
    if (i + 999999 > nrow(df)) {
      temp <- df[i:nrow(df),]
      cc <- temp[complete.cases(temp),]
      df_cc <- rbind(df_cc, cc)
      print("complete.cases: last one!")
    }
    i <- i + 1000000
  }
  rm(temp, cc)
  return(df_cc)
}



# raster.cor --------------------------------------------------------------


# raster.cor function, adapted to use my.complete.cases()

my.raster.cor <- function(x, y, method="spearman"){
  
  if(inherits(x, "enmtools.model")){
    x <- x$suitability
  }
  if(inherits(y, "enmtools.model")){
    y <- y$suitability
  }
  
  df <- cbind(terra::values(x), terra::values(y))
  df <- my.complete.cases(df)
  
  return(cor(df[,1], df[,2], method=method))
}



# raster.overlap ----------------------------------------------------------


# raster.overlap function, adapted to use my.raster.cor()

my.raster.overlap <- function(x, y, verbose=FALSE){
  
  if(any(grepl("enmtools", class(x)))){
    x <- x$suitability
  }
  if(any(grepl("enmtools", class(y)))){
    y <- y$suitability
  }
  
  if(verbose){
    print(paste("Starting overlap at: ", Sys.time()))
  }
  
  x <- raster.standardize(x)
  y <- raster.standardize(y)
  
  # calculate D
  print(paste("Calculating D at: ", Sys.time()))
  D <- 1 - as.numeric(terra::global(abs(x - y), "sum", na.rm = TRUE)/2)
  
  # calculate I
  print(paste("Calculating I at: ", Sys.time()))
  I <- 1 - as.numeric(terra::global((sqrt(x) - sqrt(y))^2, "sum", na.rm = TRUE)/2)
  
  # calculate rank correlation
  print(paste("Calculating rho at: ", Sys.time()))
  rho <- my.raster.cor(x, y)
  
  print(paste("Overlap calculations complete!"))
  
  results <- list(D = D, I = I, rho = rho)
  return(results)
}



# identity.test -----------------------------------------------------------


# identity.test function, adapted to use my.raster.overlap()

my.identity.test <- function(species.1, species.2, suitability.1, suitability.2, env, nreps = 99, clamp = TRUE, verbose = FALSE){
  
  # combine presence points for random permutations
  combined.presence.points <- rbind(species.1$presence.points, species.2$presence.points)
  
  # clamping layers here instead of doing it for each replicate model
  if (clamp == TRUE){
    message("\nClamping env layers...\n")
    
    combined.all.points <- rbind(species.1$presence.points, species.2$presence.points, species.1$background.points)
    
    # adding env values for these points
    this.df <- as.data.frame(terra::extract(env, combined.all.points, ID = FALSE))
    
    env <- clamp.env(this.df, env)
  }
  
  # calculate empirical overlap
  #message("\nCalculating empirical overlap...\n")
  #empirical.overlap <- c(unlist(my.raster.overlap(suitability.1, suitability.2, verbose = TRUE)))
  empirical.overlap <- c(unlist(list(D = 0.5, I = 0.5, rho = 0.5)))

  # store overlap values
  reps.overlap <- empirical.overlap
  
  # build replicate models
  message("\nBuilding replicate models...\n")
  
  if (requireNamespace("progress", quietly = TRUE)){
    pb <- progress::progress_bar$new(
      format = " [:bar] :percent eta :eta",
      total = nreps, clear = FALSE, width = 60)
  }
  
  for (i in 1:nreps){
    if (verbose == TRUE){message(paste("\nReplicate", i, "...\n"))}
    
    if (requireNamespace("progress", quietly = TRUE)){pb$tick()}
    
    # randomize presence points
    combined.presence.points <- combined.presence.points[sample(nrow(combined.presence.points)),]
    rep.species.1 <- species.1
    rep.species.2 <- species.2
    rep.species.1$presence.points <- combined.presence.points[1:nrow(species.1$presence.points),]
    rep.species.2$presence.points <- combined.presence.points[(nrow(species.1$presence.points) + 1):nrow(combined.presence.points),]
    
    # build models for rep i
    message(paste("\nBuilding rep", i, "models...\n"))
    rep.species.1.suitability <- my.maxent(rep.species.1, env, factors = "Habitat_Type", clamp = FALSE, verbose = TRUE)
    rep.species.2.suitability <- my.maxent(rep.species.2, env, factors = "Habitat_Type", clamp = FALSE, verbose = TRUE)
    
    # calculate replicate overlap
    message(paste("\nCalculating rep", i, "overlap...\n"))
    reps.overlap <- rbind(reps.overlap, c(unlist(my.raster.overlap(rep.species.1.suitability, rep.species.2.suitability, verbose = TRUE))))
  }
  
  rownames(reps.overlap) <- c("empirical", paste("rep", 1:nreps))
  
  message(paste("Calculating p-values at: ", Sys.time()))
  p.values <- apply(reps.overlap, 2, function(x) rank(x)[1]/length(x))
  
  reps.overlap <- as.data.frame(reps.overlap)
  
  # plots for D, I, rank.cor (rho)
  message(paste("Making plots for D, I, and rho at: ", Sys.time()))
  d.plot <- ggplot(reps.overlap[2:nrow(reps.overlap),], aes(x = .data$D, fill = "density", alpha = 0.5)) +
    geom_histogram(binwidth = 0.05) +
    geom_vline(xintercept = reps.overlap[1,"D"], linetype = "longdash") +
    xlim(-.05,1.05) + guides(fill = "none", alpha = "none") + xlab("D") +
    theme(plot.title = element_text(hjust = 0.5))
  
  i.plot <- ggplot(reps.overlap[2:nrow(reps.overlap),], aes(x = .data$I, fill = "density", alpha = 0.5)) +
    geom_histogram(binwidth = 0.05) +
    geom_vline(xintercept = reps.overlap[1,"I"], linetype = "longdash") +
    xlim(-.05,1.05) + guides(fill = "none", alpha = "none") + xlab("I") +
    theme(plot.title = element_text(hjust = 0.5))
  
  cor.plot <- ggplot(reps.overlap[2:nrow(reps.overlap),], aes(x = .data$rank.cor, fill = "density", alpha = 0.5)) +
    geom_histogram(binwidth = 0.05) +
    geom_vline(xintercept = reps.overlap[1,"rank.cor"], linetype = "longdash") +
    xlim(-1.05,1.05) + guides(fill = "none", alpha = "none") + xlab("Rank Correlation") +
    theme(plot.title = element_text(hjust = 0.5))
  
  output <- list(description = paste("Identity test:", species.1$species.name, "vs.", species.2$species.name),
                 reps.overlap = reps.overlap,
                 p.values = p.values,
                 d.plot = d.plot,
                 i.plot = i.plot,
                 cor.plot = cor.plot)
  message(paste("All done Identity Test at: ", Sys.time()))
  
  return(output)
  
}



# maxent ------------------------------------------------------------------


# maxent function, but with ONLY the steps I need for building my replicate models in my.identity.test()

my.maxent <- function(species, env, factors, clamp = FALSE, verbose = FALSE){
  
  message("\nStarting MaxEnt!\n")
  print(paste("\nMaxEnt - checking env at:", Sys.time()))
  #env <- check.raster(env, "env")
  
  print(paste("\nMaking presence and background point data frames..."))
  p.df <- terra::as.data.frame(species$presence.points, geom = "XY")
  a.df <- terra::as.data.frame(species$background.points, geom = "XY")
  
  if (length(names(env)) == 1){
    oldname <- names(env)
    env <- c(env, env)
    env[[2]][!is.na(env[[2]])] <- 0
    names(env) <- c(oldname, "dummyvar")
    print("Only one predictor layer was provided so a dummy variable was created to be compatible with dismo.")
  }
  
  message(paste("\nStarting MaxEnt model at:", Sys.time()))
  
  if(verbose){
    this.mx <- dismo::maxent(raster::stack(env), p = p.df, a = a.df, factors = factors)
    print("\nModel built! Starting suitability prediction...")
    suitability <- terra::predict(env, this.mx, type = "response", na.rm = TRUE, cores = 5)
  } else {
    invisible(capture.output(this.mx <- dismo::maxent(raster::stack(env), p = p.df, a = a.df, factors = factors)))
    invisible(capture.output(suitability <- terra::predict(env, this.mx, type = "response", na.rm = TRUE, cores = 5)))
  }
  
  return(suitability)
}
