
library(ggplot2)
library(genesysr)
library(data.table)
library(tidyverse)
library(wbstats)
library(scales)
library(ggforce)
library(scales)


setwd("G:/My Drive/Sorghum/R_systematicreview/")

############################################################################################################################################
# Figure 2B - Improved/Landrace/Wild Collections

coll_dat <- read.csv('./input_data/collection_size_figure_data.csv')

head(coll_dat)
str(coll_dat)

group.colors <- c(landrace = "#6BA292", improved = "#941C2F", wild_weedy ="#E6AF2E")
#alpha.colors = alpha(group.colors, alpha = 0.24)

# Reorder the levels of the status factor
coll_dat$status <- factor(coll_dat$status, levels = c("landrace","improved", "wild_weedy"))

# Then proceed with your ggplot code
coll_dat1 <- coll_dat %>%
  arrange(status)

#tiff(filename = "figure_collection_size.tif", width = 3000, height = 3300, unit = "px")

ggplot(data = coll_dat1) +
  theme_minimal() +
  theme(axis.text = element_blank(),    # remove geographic coordinates
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        strip.text = element_text(size = 20)) + # remove ticks
  geom_circle(aes(x0 = coord, y0 = coord, r = count/2, fill= status), alpha = 0.5, color = NA) + # , color = NA
  coord_fixed() +
  facet_wrap(~Crop, scale = "fixed") +
  #Specify colours
  scale_fill_manual(values=group.colors) +
  scale_color_manual(values = group.colors) + # scale_color_manual(values = alpha.colors) +
  guides(fill = "none", color = "none")

#dev.off() 

## Barley Plot (fix colors from previous plot)
barley <- coll_dat[which(coll_dat$Crop == 'Barley'),]

barley$status <- factor(barley$status, levels = c("improved", "landrace", "wild_weedy"))

barley1 <- barley %>%
  arrange(status)

ggplot(data = barley1) +
  theme_minimal() +
  theme(axis.text = element_blank(),    # remove geographic coordinates
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        strip.text = element_text(size = 20)) + # remove ticks
  geom_circle(aes(x0 = coord, y0 = coord, r = count/2, fill= status), alpha = 0.5, color = NA) + # , color = NA
  coord_fixed() +
  facet_wrap(~Crop, scale = "fixed") +
  #Specify colours
  scale_fill_manual(values=group.colors) +
  scale_color_manual(values = group.colors) + # scale_color_manual(values = alpha.colors) +
  guides(fill = "none", color = "none")


############################################################################################################################################
### Figure 4 - Number of Accessions in Genesys by crop

FAO_all_crop <- read.csv("./input_data/FAO_allcrops_numacc.csv")

FAO_all_crop$type <- factor(FAO_all_crop$type, levels = c("cereal","legume","root/tuber","vegetable/seed","fruit","forage"))

FAO_all_crop$number_accessions <- as.numeric(FAO_all_crop$number_accessions)
FAO_all_crop$number_available <- as.numeric(FAO_all_crop$number_available)

# Final figure 4
FAO_all_crop %>% 
  mutate(
    type = fct_relevel(type, "cereal","legume","root/tuber","vegetable/seed","fruit","forage"),
    Crop = fct_reorder(Crop, number_accessions)
  ) %>% 
ggplot(aes(x = Crop, y = number_accessions, fill = type)) +
  geom_col(alpha = 0.5, width = 0.85) +
  geom_col(aes(y = number_available),alpha = 1.5, width = 0.85) +
  scale_fill_brewer(palette = "Dark2") +
  scale_y_continuous(expand = c(0, 0.1)) +
  coord_flip() +
  facet_wrap(~type, scales = "free") +
  labs(
    y = "Number of Accessions"
  ) +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, unit = "cm"),
    plot.title = element_text(size = 15, face = "bold"),
    strip.text = element_text(size = 11, face = "bold"),
    strip.placement = "outside",
    axis.title.x = element_text(margin = margin(t = 0.5, b = 0.5, unit = "cm")),
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = 10, angle = 0),
    axis.text.x = element_text(size = 10, angle = 45),
    legend.position = "none",
    panel.grid.major.y = element_blank(),
  )  +
  scale_y_continuous(labels = scales::comma_format())


###################################################################################################################################
# Figure 5 - Venn Diagram, Data Availability

if (!require(devtools)) install.packages("devtools")
devtools::install_github("yanlinlin82/ggvenn")

library(ggvenn)

df <- read.csv("./input_data/data_availability_FAOcrops.csv")

df[df == "No"] <- FALSE
df[df == "Yes"] <- TRUE

df[,2:6] <- sapply(df[,2:6],as.logical)

colnames(df) <- c('Crop', 'EAA','Genome', 'Georeferenced', 'Genotyped', 'Phenotyped')

ggvenn(
  df,
  c('Genome', 'Georeferenced', 'Genotyped', 'Phenotyped'),
  fill_color = c("#0073C2FF", "#EFC000FF", "#868686FF", "#CD534CFF"),
  stroke_size = 0.5, set_name_size = 4
)




