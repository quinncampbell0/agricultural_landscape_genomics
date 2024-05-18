

library(ggplot2)
library(genesysr)
library(data.table)
library(tidyverse)
library(wbstats)
library(scales)
library(ggforce)
library(scales)


setwd("G:/My Drive/Sorghum/R_systematicreview/")


#############################################################################################################################
# Download food crop data

# Gather Genesys data (must sign up for Genesys account to access this data)
setup_sandbox()
user_login()

# Food Crops
food <- read.csv("./input_data/FAO_foodcrops.csv")
filters <- list(taxonomy = list(genus = food$genus))
columns = c("accessionName","accessionNumber","acquisitionDate","aliases1.aliasType","aliases1.name","aliases1.usedBy","aliases2.aliasType","aliases2.name","aliases2.usedBy",
            "ancest","available","breederCode1","coll.collCode1","coll.collDate","coll.collName1","coll.collNumb","coll.collSite","coll.collName1","coll.collNumb","coll.collSite",
            "countryOfOrigin.name","createdDate","crop.name","crop.shortName","cropName","duplSite1", "elevation","genus","doi","geo.latitude","geo.longitude","geo.tileIndex",
            "historic","institute.allowMaterialRequests","institute.code","institute.country.name","institute.latitude","institute.longitude","institute.type","origCty","taxonomy.active",
            "taxonomy.currentTaxonomySpecies.genusSpecies","taxonomy.grinTaxonomySpecies.lifeFormCode","taxonomy.grinTaxonomySpecies.speciesName","taxonomy.currentTaxonomySpecies.speciesName",
            "taxonomy.currentTaxonomySpecies.subspeciesAuthority","taxonomy.currentTaxonomySpecies.subspeciesName","taxonomy.currentTaxonomySpecies.subtaxa",
            "taxonomy.currentTaxonomySpecies.subtaxaAuthority","taxonomy.currentTaxonomySpecies.varietyName")

# columns = c("accessionName","accessionNumber","historic","available")

for(i in food$genus){
  file_name <- paste0("G:/My Drive/Sorghum/R_systematicreview/genesys_download/FAO.foodcrops_",i,".csv")
  if(file.exists(file_name)) {next}
  else{
    current_genus <- genesysr::get_accessions(list(taxonomy = list(genus = c(i))), fields = columns, size = 10000)
    write.csv(current_genus, file = file_name, row.names=F)
  }
}

count_list <- list()
for(j in 1:length(food$genus)){
  i <- food$genus[j]
  file_name <- paste0("G:/My Drive/Sorghum/R_systematicreview/genesys_download/FAO.foodcrops_",i,".csv")
  crop <- read.csv(file_name)
  crop1 <- crop[which(crop$historic == 0),]
  num_acc <- dim(crop1)[1]
  avail <- crop1[which(crop1$available == 1),]
  num_avail <- dim(avail)[1]
  count_list[[j]] <- c(i,num_acc, num_avail)
}

count_df <- as.data.frame(do.call(rbind, count_list))
colnames(count_df) <- c("Genus","number_accessions", "number_available")
#write.csv(count_df, "genesys_FAOfoodcrops_count.csv", row.names = F)

count_df$number_accessions <- as.numeric(count_df$number_accessions)
count_df$number_available <- as.numeric(count_df$number_available)

count_df$type <- food$type

count_df[45,2:3] <- c(560230,93823) # set numbers for Triticum manually

#write.csv(count_df, "FAO_foodcrops_accessioncounts.csv")

#############################################################################################################################
# Download forage data

forage <- read.csv("FAO_foragecrops.csv")

for(i in 1:length(forage$species)){
  species <- forage$species[i]
  genus  <- strsplit(species, " ")[[1]][1]
  epithet <- strsplit(species, " ")[[1]][2]
  sp.text <- gsub(" ", "", species)
  file_name <- paste0("G:/My Drive/Sorghum/R_systematicreview/genesys_download/FAO.foragecrops_",sp.text,".csv")
  if(file.exists(file_name)) {next}
  else{
    current_species <- genesysr::get_accessions(list(taxonomy = list(genus = c(genus), species = c(epithet))), fields = columns, size = 10000)
    write.csv(current_species, file = file_name, row.names=F)
  }
}

# Create Count for each species

for(j in 1:length(forage$genus)){
  i <- forage$species[j]
  species <- gsub(" ","",i)
  file_name <- paste0("G:/My Drive/Sorghum/R_systematicreview/genesys_download/FAO.foragecrops_",species,".csv")
  crop <- read.csv(file_name)
  crop1 <- crop[which(crop$historic == 0),]
  forage$num_acc[j] <- dim(crop1)[1]
  avail <- crop1[which(crop1$available == 1),]
  forage$num_avail[j] <- dim(avail)[1]
}


#write.csv(forage, "FAO_foragecrops_accessioncounts.csv")

#########################################################################################################################
# Organize and combine data

count_df <- read.csv("FAO_foodcrops_accessioncounts.csv")

ggplot(count_df, aes(x=reorder(Genus, -number_accessions), y = number_accessions)) +
  geom_col(fill = "lightblue") +
  theme_bw() +
  labs(y = "Number of Accessions", x = "Genus") +
  geom_col(aes(y = number_available), fill = "darkblue") +
  theme(strip.text = element_text(size = 12), axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  facet_wrap(~type, scales = "free", drop = T) +
  theme(strip.background=element_rect(colour="black",
                                      fill="lightgrey"))

####### Forage Crops ########

forage <- read.csv("FAO_foragecrops_accessioncounts.csv")

forage_genus <- forage %>% group_by(Genus = genus) %>% 
  summarize(number_accessions = sum(num_acc),number_available = sum(num_avail))

forage_genus$type  <- "forage"

ggplot(forage, aes(x = genus, y = num_acc)) +
  geom_col()

ggplot(forage, aes(x=reorder(genus, -num_acc), y = num_acc)) +
  geom_col(fill = "lightblue") +
  theme_bw() +
  labs(y = "Number of Accessions", x = "Genus") +
  geom_col(aes(y = num_avail), fill = "darkblue") +
  theme(strip.text = element_text(size = 12), axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  facet_wrap(~forage_type, scales = "free", drop = T) +
  theme(strip.background=element_rect(colour="black",
                                      fill="lightgrey"))

###################################
## Combining forage and food crops

FAO_food <- count_df
FAO_food$Crop <- food$crop_name

FAO_forage <- forage_genus
FAO_forage$Crop <- forage_genus$Genus

FAO_all <- rbind(FAO_food, FAO_forage)

#write.csv(FAO_all, "FAO_allcrops_accessioncounts.csv", row.names = F)

FAO_all <- read.csv("FAO_allcrops_accessioncounts.csv")

ggplot(FAO_all, aes(x=reorder(Crop, -number_accessions), y = number_accessions)) +
  geom_col(fill = "lightblue") +
  theme_bw() +
  labs(y = "Number of Accessions", x = "Genus") +
  geom_col(aes(y = number_available), fill = "darkblue") +
  theme(strip.text = element_text(size = 12), axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  facet_wrap(~type, scales = "free", drop = T) +
  theme(strip.background=element_rect(colour="black",
                                      fill="lightgrey"))

FAO_all$type <- factor(FAO_all$type, levels = c("cereal","legume","root/tuber","vegetable/seed","fruit","forage"))

FAO_all_crop <- FAO_all %>% group_by(Crop,type) %>% 
  summarize(number_accessions = sum(number_accessions), number_available = sum(number_available))

ggplot(FAO_all_crop, aes(x=reorder(Crop, -number_accessions), y = number_accessions)) +
  geom_col(fill = "lightblue", position = "dodge") +
  theme_bw() +
  labs(y = "Number of Accessions", x = "Genus") +
  geom_col(aes(y = number_available), fill = "darkblue") +
  theme(strip.text = element_text(size = 12), axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  facet_wrap(~type, scales = "free", drop = T) +
  theme(strip.background=element_rect(colour="black",
                                      fill="lightgrey"))