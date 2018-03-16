#####################################################################################
#' @title Trait data organiseation 
#' @author James Margrove 

# Clear workspace
rm(list=ls())

# Import data 
leaf_data <- read.table("./data/traits/leaf.txt", header = TRUE)
root_data <- read.table("./data/traits/root.txt", header = TRUE)
si_data <- read.table("./data/traits/stomatal_index.txt", header = TRUE)
density_data <- read.table("./data/traits/wood_density.txt", header = TRUE)

# Create traits data frame 
traits <- data.frame(sp = levels(leaf_data$sp))

# leaf traits 
leaf_data$sma <- with(leaf_data, weight/AREA/samples)
traits$slm <- with(leaf_data, tapply(sma, sp, mean))
traits$slm_sd <- with(leaf_data, tapply(sma, sp, sd))

# stomatal index 
traits$si <- with(si_data, tapply(stinx, sp, mean))
traits$si_sd <- with(si_data, tapply(stinx, sp, sd))

# wood density 
traits$dden <- with(density_data, tapply(dden, sp, mean))
traits$dden_sd <- with(density_data, tapply(dden, sp, sd))

# root data (air density, and dry wood density)
traits$raden <- with(root_data, tapply(aden, sp, mean))
traits$raden_sd <- with(root_data, tapply(aden, sp, sd))
traits$rdden <- with(root_data, tapply(dden, sp, mean))
traits$rdden_sd <- with(root_data, tapply(dden, sp, sd))

# adult wood density data 
sp_names <- read.table("./data/species_names.txt", header = TRUE)
sp_names <- merge(sp_names, traits, by = "sp")[,1:2]
colnames(sp_names)[2] <- "Species"
gwddb <- read.table("./data/traits/GlobalWoodDensityDatabase.txt", header = TRUE)
adden_data <- droplevels(merge(sp_names, gwddb, by = "Species"))[, c(2, 5)]
# Missing ptom - with we know is 0.52 from 
adden_data <- rbind(adden_data, data.frame(sp = "ptom", dden = 0.52))
# take the means
adden_data <- adden_data[order(as.character(adden_data$sp)),]
adden_data$sp <- factor(adden_data$sp, levels = unique(as.character(adden_data$sp)))
traits$adden <- with(adden_data, tapply(dden, sp, mean))
traits$adden_sd <- with(adden_data, tapply(dden, sp, sd))

# initial size 
size_data <- read.table("./data/Experiment, mort, leafAB, dden, wden,sla.txt", header = TRUE);
traits$dia <- with(size_data, tapply(dia, sp, mean))
traits$dia_sd <- with(size_data, tapply(dia, sp, sd))

# biomass traits - Root Mass Fraction 
bio_data <- read.table("./data/initial_biomass.txt", header = TRUE)
bio_data$rmf <- with(bio_data, rootweight/total)
bio_data$lmf <- with(bio_data, leafweight/total)
# for traits 
traits$rmf <- with(bio_data, tapply(rmf, sp, mean))
traits$rmf_sd <- with(bio_data, tapply(rmf, sp, sd))
traits$lmf <- with(bio_data, tapply(lmf, sp, mean))
traits$lmf_sd <- with(bio_data, tapply(lmf, sp, sd))

# Hear now we have 9 traits....
write.table(traits, "./data/traits.txt")


