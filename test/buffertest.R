env <- stack("~/GitHub/SDM-Sim/Australia CliMond/CM10_1975H_Bio_ASCII_V1.grd")[[1:19]]

# Automate the process of downloading data and removing duds and dupes
species.from.gbif <- function(genus, species, name = NA, env){

  # Name it after the species epithet unless told otherwise
  if(is.na(name)){
    name <- species
  }

  # Get GBIF data
  this.sp <- enmtools.species(presence.points = gbif(genus = genus, species = species)[,c("lon", "lat")],
                              species.name = name)

  # Rename columns, get rid of duds
  colnames(this.sp$presence.points) <- c("Longitude", "Latitude")
  this.sp$presence.points <- this.sp$presence.points[complete.cases(extract(env, this.sp$presence.points)),]
  this.sp$presence.points <- this.sp$presence.points[!duplicated(this.sp$presence.points),]

  return(this.sp)
}

mel <- species.from.gbif("Melaleuca", "pungens", name = "Melaleuca pungens", env)

mel$range <- background.raster.buffer(mel$presence.points, 50000, mask = env)
mel.glm1 <- enmtools.glm(mel, env, f = presence ~ poly(bio1, 2) * poly(bio8, 2), test.prop = 0.2)
visualize.enm(mel.glm1, env, layers = c("bio1", "bio8"))


mel$range <- background.raster.buffer(mel$presence.points, 100000, mask = env)
mel.glm2 <- enmtools.glm(mel, env, f = presence ~ poly(bio1, 2) * poly(bio8, 2), test.prop = 0.2)
visualize.enm(mel.glm2, env, layers = c("bio1", "bio8"))

mel$range <- background.raster.buffer(mel$presence.points, 200000, mask = env)
mel.glm3 <- enmtools.glm(mel, env, f = presence ~ poly(bio1, 2) * poly(bio8, 2), test.prop = 0.2)
visualize.enm(mel.glm3, env, layers = c("bio1", "bio8"))

mel$range <- background.raster.buffer(mel$presence.points, 400000, mask = env)
mel.glm4 <- enmtools.glm(mel, env, f = presence ~ poly(bio1, 2) * poly(bio8, 2), test.prop = 0.2)
visualize.enm(mel.glm4, env, layers = c("bio1", "bio8"))


mel$range <- background.raster.buffer(mel$presence.points, 800000, mask = env)
mel.glm5 <- enmtools.glm(mel, env, f = presence ~ poly(bio1, 2) * poly(bio8, 2), test.prop = 0.2)
visualize.enm(mel.glm5, env, layers = c("bio1", "bio8"))

mel$range <- background.raster.buffer(mel$presence.points, 1600000, mask = env)
mel.glm6 <- enmtools.glm(mel, env, f = presence ~ poly(bio1, 2) * poly(bio8, 2), test.prop = 0.2)
visualize.enm(mel.glm6, env, layers = c("bio1", "bio8"))

glms <- list(mel.glm1, mel.glm2, mel.glm3, mel.glm4, mel.glm5, mel.glm6)
lapply(glms, function(x) x$test.evaluation@cor)
lapply(glms, function(x) x$env.test.evaluation@cor)
