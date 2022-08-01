
# -------------------------------------------------------------

# Diagnostics of reef fisheries in Brazil

# Linda Eggertsen et al.


# load packages
require(here)
require(openxlsx)
source ("R/functions.R")

# create directory to host the results

dir.create("output")

# load fisheries data (Freire et al. 2021)

fisheries <- read.xlsx (here ("data", "FINAL_RECONSTRUCTED_Brazil_1950_2015_CommercialEtapaII_04072021_IP_Freire.xlsx"),
                        sheet = 2)
# adjust name
fisheries$Sector [which(fisheries$Sector == "industrial (LS, C)")] <- "Industrial (LS, C)"




# load data of Pinheiro et al. 2018 (BR reef fish)
reef_fish <- read.csv (here ("data","brazilian-reef-fish-table-04-mar-18-website.xlsx - Database.csv"))
reef_fish<-reef_fish[which(reef_fish$Relation == "RES"),] # REef fish  (RESident fish)

# mistake on the database
reef_fish$Genus [grep ("Ocyurus chrysurus", reef_fish$Species)] <- "Ocyurus"


# trait data (GASPAR database)
#traits <- read.xlsx (here ("data", "GASPAR_Data-1.xlsx"),
#                     sheet = 3)

# trait data (QUimbayo database)

traits <- read.csv (here ("data", 
                          "Atributos_especies_Atlantico_&_Pacifico_Oriental_2020_04_28.csv"),
                    sep = ";")



traits$Genus <- firstup(traits$Genus)  


# ajusts  
traits$Body_size <- as.numeric(gsub (",",".",traits$Body_size))
traits$Trophic_level <- as.numeric(gsub (",",".",traits$Trophic_level))
traits$Depth_range <- as.numeric(gsub (",",".",traits$Depth_range))
traits$Depth_max<-as.numeric(gsub (",",".",traits$Depth_max))
traits$Depth_min<-as.numeric(gsub (",",".",traits$Depth_min))

# average depth
traits$Depth_mean <- rowMeans(cbind (traits$Depth_max,
                            
                                      traits$Depth_min),
                              na.rm=T)



# fisheries ~year
require(dplyr)

# genus
genus <- strsplit(fisheries$TaxonName," ")
genus <- do.call(rbind, genus)
fisheries$Genus_match <- genus[,1]



## matching with nutrients (predictions from Hicks et al. 2019)

nutrients <- read.csv (here ("data", "Species_Nutrient_Predictions.csv"))


#  genus
nutrients$Genus <- sapply(strsplit (nutrients$species, "_"), "[",1)



# bind nutrient content to the trait dataset
traits <- cbind (traits, 
                 nutrients[match (traits$Genus,
                                  
                                   (nutrients$Genus)),])




# fisheries data (matching with Pinheiro et al. 2018)
# reef fish genus

table(unique(fisheries$Genus_match) %in% reef_fish$Genus )
fisheries_wtrait<-fisheries[which(fisheries$Genus_match %in% reef_fish$Genus),]



# match trait & fisheries
fisheries_wtrait <- cbind (fisheries_wtrait,
                           
                           traits [match (fisheries_wtrait$Genus,traits$Genus ),]
                           
                           )

# here we have the sharks
unique(fisheries_wtrait[is.na(fisheries_wtrait$Class),"Genus_match"])



# equal column names (remove one)
table(fisheries_wtrait[,25] == fisheries_wtrait[,117])
fisheries_wtrait<- fisheries_wtrait[,-117]


unique(fisheries_wtrait [which (fisheries_wtrait$Year %in% seq (1970,1973) & 
                                   fisheries_wtrait$Region == "Sul"),"TaxonName"])

# ---------------------------
# plotting
# catch year
catch_year <- fisheries_wtrait %>%
  
  group_by(Year,Sector,Region) %>% 

  
  
  summarize(sum_catch=sum(CatchAmount_t),
  ) 

# recode region
catch_year$Region <- recode_factor(catch_year$Region,    
  "Norte" = "North",
          "NE" = "Northeastern", 
          "Sul" = "South",
          "SE" = "Southeast")

require(ggplot2)
require(ggrepel)


catch_year_plot <- ggplot (catch_year, aes (x=Year, 
                         y=sum_catch,
                         colour = Sector)) + 
  facet_wrap(~Region,scales = "fixed")+
  geom_line(size=1.5) + 
  scale_fill_viridis_d(option ="viridis", begin = 0.3,end=0.8) + 
  theme_classic() + 
  scale_colour_viridis_d(option ="viridis", begin = 0.3,end=0.8) + 
  theme (legend.position = c(0.12,0.90),
         axis.title = element_text(size=15),
         strip.text.x = element_text(size = 14, color = "black", 
                                     face = "bold"),
         strip.background = element_rect(color="black", 
                                         fill="gray60",
                                         size=1.5, linetype="solid"
         )) + 
  ylab ("Sum of the Catch Amount (T)") 
  



pdf (here ("output", "catch_year_plot.pdf"),height=6,width=6)
catch_year_plot
dev.off()



# ========================
# how fisheries changed over time in terms of spp composition?
# filter reef fish



# reshape
require(reshape)

year_composition <- cast (fisheries_wtrait, 
                          
                          formula = Year ~ Genus_match, 
      
                        value = "CatchAmount_t",
                     
                           fun.aggregate =  sum,
                        
                        drop = F
        )



# vegan:: beta diversity between year
#  hellinger transformed dataset
require(vegan)
dist_composition <- vegdist (decostand(year_composition,'hell'),
                                method = "bray",na.rm=T)



# pcoa
require(ape)
pcoa_fish_year <- pcoa(dist_composition)
# variance explained by the first and second axes
Exp_axis1<-pcoa_fish_year$values$Eigenvalues[1]/sum(pcoa_fish_year$values$Eigenvalues)*100
Exp_axis2<-pcoa_fish_year$values$Eigenvalues[2]/sum(pcoa_fish_year$values$Eigenvalues)*100


#pcoa_fish_year <- melt (pcoa_fish_year)
pcoa_fish_year <- cbind (pcoa_fish_year$vectors,
                         year = year_composition$Year)

# dataframe with data
pcoa_fish_year<-as.data.frame(pcoa_fish_year)

# ordination (projection of beta diversity )
# help here
# https://ggplot2.tidyverse.org/reference/geom_path.html
ordination1<-ggplot(data=pcoa_fish_year,
       aes(x=Axis.1,y=Axis.2)) + 
  geom_point(aes(colour=as.numeric(year)),shape=1,size=3) + # add the point markers
   geom_path(aes(colour=as.numeric(year)),alpha=0.5)+

  geom_text(aes(label=year),
                size=2.5,vjust=-1) +
  #geom_path(aess(group=year)) +# add the site labels
  #scale_colour_manual(values=c("A" = "red", "B" = "blue")) +
  coord_equal() +
  theme_bw() +
  
  theme(legend.position = "none")



# correlation of genus to the axes

correlation <-data.frame( cor (decostand(year_composition,'hell'),
                    pcoa_fish_year[,1:2],
                    method = "pearson"))

rownames(correlation) <- colnames(year_composition[,-1])
correlation$species <- rownames(correlation)

# order

correlation[order(correlation[,1],decreasing=T),][1:10,]
correlation[order(correlation[,1],decreasing=F),][1:9,]
correlation[order(correlation[,2],decreasing=T),][1:3,]



# project genus names
ordination1<-ordination1 + geom_text_repel(data = correlation[order(correlation[,2],decreasing=T),][1:3,],
                        aes (x=Axis.1*0.2,
                             y = Axis.2*0.2,
                             label = (species)),
                        size=3,fontface = "italic",
                        colour="#1363DF",
                        max.overlaps = 100) +
  geom_text_repel(data = correlation[order(correlation[,1],decreasing=F),][1:9,],
            aes (x=Axis.1*0.2,
                 y = Axis.2*0.2,
                 label = (species)),
            size=3,fontface = "italic",
            colour = "#06283D",
            max.overlaps = 100) + 
  geom_text_repel(data = correlation[order(correlation[,1],decreasing=T),][1:16,],
            aes (x=Axis.1*0.22,
                 y = Axis.2*0.22,
                 label = (species)),
            size=3,fontface = "italic",
            colour = "#47B5FF",
            max.overlaps = 100) + 
  xlab (paste ("Axis 1 (", round(Exp_axis1,2), "%)",sep="")) +
  ylab (paste ("Axis 2 (", round(Exp_axis2,2), "%)",sep=""))


## =============================================== 
# change in composition per region
# a pesca mudou ao longo do tempo?

year_composition_region <- lapply (unique(fisheries_wtrait$Region), function (i)
  
  cast (fisheries_wtrait[which(fisheries_wtrait$Region == i),], 
        
        formula = Year ~ Genus_match, # Genus
        
        value = "CatchAmount_t",
        
        fun.aggregate = sum,
        
        drop= F
  )
)



## FAMILIES
library(tidyverse)

# vegan beta diversity
require(vegan)
dist_composition_pcoa <- lapply (year_composition_region, function (i){
  
  
  dist_composition <- vegdist (decostand(i,'hell'),
                               method = "bray",na.rm=T)
  
  # pcoa
  pcoa_fish_year <- pcoa(dist_composition)
  ;
  pcoa_fish_year
})


# composition per region
comp_change <- bind_rows(pcoa_fish_year %>% 
                           bind_cols(region = "Brazil", Year = unique(fisheries_wtrait$Year)),
                          dist_composition_pcoa[[1]]$vectors %>% 
                           bind_cols(region = "Norte", Year = unique(fisheries_wtrait$Year)),
                         dist_composition_pcoa[[2]]$vectors %>% 
                           bind_cols(region = "NE", Year = unique(fisheries_wtrait$Year)),
                         dist_composition_pcoa[[3]]$vectors %>% 
                           bind_cols(region = "SE", Year = unique(fisheries_wtrait$Year)),
                         dist_composition_pcoa[[4]]$vectors %>% 
                           bind_cols(region = "Sul", Year = unique(fisheries_wtrait$Year))) %>% 
  data.frame() %>% dplyr::rename(change = Axis.1) %>% 
  ggplot(aes(x = Year, y = change)) +
  geom_point() +
  facet_wrap(~ region, ncol = 5) +
  theme_classic() +
  geom_smooth() +
  labs(x = "", y = "Change in catch\ncomposition (Beta)") + 
  theme (legend.position = c(0.12,0.90),
         axis.title = element_text(size=15),
         strip.text.x = element_text(size = 14, color = "black", 
                                     face = "bold"),
         strip.background = element_rect(color="black", 
                                         fill="gray60",
                                         size=1.5, linetype="solid"
         )) 


# arrange plots
require(gridExtra)

pdf (here ("output", "fisheries_composition.pdf"),height=6,width=7)

compostion1<-grid.arrange(ordination1,
             comp_change,
             ncol=5,nrow=6,
             layout_matrix = rbind (c (1,1,1,1,1),
                                    c (1,1,1,1,1),
                                    c (1,1,1,1,1),
                                    c (1,1,1,1,1),
                                    c (2,2,2,2,2),
                                    c (2,2,2,2,2)))


dev.off()


# ----------------------------------------------------------------
# how do fisheries changed in terms of functional composition?


# trait composition

average_traits <- fisheries_wtrait %>% 
  
                        
                        group_by(Genus_match) %>% 
  
                          summarise (Body_size = mean (Body_size,na.rm=T),  
                                     Trophic_level = mean (Trophic_level,na.rm=T), 
                                     Depth_range = mean (Depth_range,na.rm=T), 
                                     Diel_activity = getmode (Diel_activity), 
                                     Size_group = getmode (Size_group), 
                                     Level_water = getmode (Level_water))  %>%
  
                          
                          select (Genus_match, Body_size, Trophic_level, Depth_range, 
                                  Diel_activity, Size_group, Level_water)  
                          


# transform categorical into ranking traits
# group size
average_traits$Size_group [which(average_traits$Size_group == "sol")] <- 1
average_traits$Size_group [which(average_traits$Size_group == "pair")] <- 2
average_traits$Size_group [which(average_traits$Size_group == "smallg")] <- 3
average_traits$Size_group [which(average_traits$Size_group == "medg")] <- 4
average_traits$Size_group [which(average_traits$Size_group == "largeg")] <- 5
average_traits$Size_group <- ordered (average_traits$Size_group) # ordered


# level water
average_traits$Level_water [which(average_traits$Level_water == "bottom")] <- 1
average_traits$Level_water [which(average_traits$Level_water == "low")] <- 2
average_traits$Level_water [which(average_traits$Level_water == "high")] <- 3
average_traits$Level_water <- ordered (average_traits$Level_water) # ordered


# interaction (if missing data in gorup size, keep at least the water level)
average_traits$Interaction.Level.Group <- ifelse (is.na(as.numeric(average_traits$Level_water) * as.numeric(average_traits$Size_group)),
                                                  as.numeric(average_traits$Level_water),
                                                  as.numeric(average_traits$Level_water) * as.numeric(average_traits$Size_group))

# daily activity
average_traits$Diel_activity <- ifelse (average_traits$Diel_activity %in% c("night", "both"),
                                        1,0)
average_traits<- data.frame(average_traits)
rownames (average_traits) <- average_traits$Genus_match
average_traits<- average_traits[,-1]



# functional composition  
require(SYNCSA)


# organize data
organized_data <- organize.syncsa(
  
      decostand(year_composition,'hell', na.rm=T),
                average_traits[,-which(colnames(average_traits) %in% c ("Diel_activity",
                                                                        "Level_water", 
                                                                        "Size_group"))])

# functional composition
funct_comp <- matrix.t(organized_data$community, 
                       organized_data$traits, 
                       scale = TRUE, 
                       ranks = TRUE, 
                       notification = TRUE)


# composition
# vegan:: beta diversity between year
#  hellinger transformed dataset
require(vegan)
dist_funct_composition <- vegdist (funct_comp$matrix.T,
                                  method = "euclidean",na.rm=T)



# pcoa
require(ape)
pcoa_fish_year_func <- pcoa(dist_funct_composition)
# variance explained by the first and second axes
(Exp_axis1_func<-pcoa_fish_year_func$values$Eigenvalues[1]/sum(pcoa_fish_year_func$values$Eigenvalues)*100)
(Exp_axis2_func<-pcoa_fish_year_func$values$Eigenvalues[2]/sum(pcoa_fish_year_func$values$Eigenvalues)*100)


#pcoa_fish_year <- melt (pcoa_fish_year)
pcoa_fish_year_func <- cbind (pcoa_fish_year_func$vectors,
                         year = as.numeric(rownames(organized_data$community)))

# dataframe with data
pcoa_fish_year_func<-as.data.frame(pcoa_fish_year_func)



# ordination (projection of beta diversity )
# help here
# https://ggplot2.tidyverse.org/reference/geom_path.html
ordination1_func<-ggplot(data=pcoa_fish_year_func,
                    aes(x=Axis.1,y=Axis.2)) + 
  geom_point(aes(colour=as.numeric(year)),shape=1,size=3) + # add the point markers
  geom_path(aes(colour=as.numeric(year)),alpha=0.5)+
  
  geom_text(aes(label=year),
            size=2.5,vjust=-1) +
  #geom_path(aess(group=year)) +# add the site labels
  #scale_colour_manual(values=c("A" = "red", "B" = "blue")) +
  coord_equal() +
  theme_bw() +
  theme(legend.position = "none")




# correlation of genus to the axes
correlation_func <-data.frame( cor (funct_comp$matrix.T,
                               pcoa_fish_year_func[,1:2],
                               method = "pearson"))
correlation_func$trait <- rownames(correlation_func)

# order

correlation_func[order(correlation_func[,1],decreasing=T),]
correlation_func[order(correlation_func[,1],decreasing=F),]
correlation_func[order(correlation_func[,2],decreasing=T),]



# project genus names
ordination1_func<-ordination1_func + 
  
  geom_text_repel(data = correlation_func[order(correlation_func[,2],
                                                decreasing=T),],
                                           aes (x=Axis.1*0.02,
                                                y = Axis.2*0.02,
                                                label = (trait)),
                                           size=3,fontface = "italic",
                                           colour="#1363DF",
                                           max.overlaps = 100) + 
  xlab (paste ("Axis 1 (", round(Exp_axis1_func,2), "%)",sep="")) +
  ylab (paste ("Axis 2 (", round(Exp_axis2_func,2), "%)",sep=""))



## =============================================== 
# change in composition per region
# a pesca mudou ao longo do tempo?

func_composition_region <- lapply (unique(fisheries_wtrait$Region), function (i){
  
  # trait composition
        
  average_traits <- fisheries_wtrait[which(fisheries_wtrait$Region == i),] %>% 
          
          
          group_by(Genus_match) %>% 
          
          summarise (Body_size = mean (Body_size,na.rm=T),  
                     Trophic_level = mean (Trophic_level,na.rm=T), 
                     Depth_range = mean (Depth_range,na.rm=T), 
                     Diel_activity = getmode (Diel_activity), 
                     Size_group = getmode (Size_group), 
                     Level_water = getmode (Level_water))  %>%
          
          
          select (Genus_match, Body_size, Trophic_level, Depth_range, 
                  Diel_activity, Size_group, Level_water)  
        
        # transform categorical into ranking traits
        # group size
        average_traits$Size_group [which(average_traits$Size_group == "sol")] <- 1
        average_traits$Size_group [which(average_traits$Size_group == "pair")] <- 2
        average_traits$Size_group [which(average_traits$Size_group == "smallg")] <- 3
        average_traits$Size_group [which(average_traits$Size_group == "medg")] <- 4
        average_traits$Size_group [which(average_traits$Size_group == "largeg")] <- 5
        average_traits$Size_group <- ordered (average_traits$Size_group) # ordered
        
        # level water
        average_traits$Level_water [which(average_traits$Level_water == "bottom")] <- 1
        average_traits$Level_water [which(average_traits$Level_water == "low")] <- 2
        average_traits$Level_water [which(average_traits$Level_water == "high")] <- 3
        average_traits$Level_water <- ordered (average_traits$Level_water) # ordered
        
        # daily activity
        average_traits$Diel_activity <- ifelse (average_traits$Diel_activity %in% c("night", "both"),
                                                1,0)
        average_traits<- data.frame(average_traits)
        rownames (average_traits) <- average_traits$Genus_match
        average_traits<- average_traits[,-1]
        
        # taxonomic composition per region (create here)
        year_composition_region <- cast (fisheries_wtrait[which(fisheries_wtrait$Region == i),], 
                
                formula = Year ~ Genus_match, # Genus
                
                value = "CatchAmount_t",
                fun.aggregate = sum
          
        )
        
        
        # functional composition  
        # organize data
        organized_data <- organize.syncsa(
          
          decostand(year_composition_region,'hell'),
          average_traits[,-which(colnames(average_traits) == "Diel_activity")])
        
        # functional composition
        funct_comp <- matrix.t(organized_data$community, 
                               organized_data$traits, 
                               scale = TRUE, 
                               ranks = TRUE, 
                               notification = TRUE)
        
        # composition
        # vegan:: beta diversity between year
        #  hellinger transformed dataset
        require(vegan)
        dist_funct_composition <- vegdist (funct_comp$matrix.T,
                                           method = "euclidean",na.rm=T)
        
        
        
        # pcoa
        pcoa_fish_year_func <- pcoa(dist_funct_composition)
        pcoa_fish_year_func
        
})



# composition per region
comp_change_func <- bind_rows(pcoa_fish_year_func %>% 
                           bind_cols(region = "Brazil", Year = unique(fisheries_wtrait$Year)),
                           func_composition_region[[1]]$vectors %>% 
                           bind_cols(region = "Norte", Year = unique(fisheries_wtrait$Year)),
                           func_composition_region[[2]]$vectors %>% 
                           bind_cols(region = "NE", Year = unique(fisheries_wtrait$Year)),
                           func_composition_region[[3]]$vectors %>% 
                           bind_cols(region = "SE", Year = unique(fisheries_wtrait$Year)),
                           func_composition_region[[4]]$vectors %>% 
                           bind_cols(region = "Sul", Year = unique(fisheries_wtrait$Year))) %>% 
  data.frame() %>% dplyr::rename(change = Axis.1) %>% 
  ggplot(aes(x = Year, y = change)) +
  geom_point() +
  facet_wrap(~ region, ncol = 5) +
  theme_classic() +
  geom_smooth() +
  labs(x = "", y = "Change in catch\n functional composition (Beta functional)")+
  theme (legend.position = c(0.12,0.90),
         axis.title = element_text(size=15),
         strip.text.x = element_text(size = 14, color = "black", 
                                     face = "bold"),
         strip.background = element_rect(color="black", 
                                         fill="gray60",
                                         size=1.5, linetype="solid"
         )) 


# arrange plots
pdf (here ("output", "fisheries_functional_composition.pdf"),height=6,width=7)
compostion1_func<-grid.arrange(ordination1_func,
                          comp_change_func,
                          ncol=5,nrow=6,
                          layout_matrix = rbind (c (1,1,1,1,1),
                                                 c (1,1,1,1,1),
                                                 c (1,1,1,1,1),
                                                 c (1,1,1,1,1),
                                                 c (2,2,2,2,2),
                                                 c (2,2,2,2,2)))


dev.off()



# ============================

# average trait per genus

size_genus <- tapply (traits$Body_size,
                      list (traits$Genus),
                      mean,na.rm=T)


# depth
depth_genus <- tapply (traits$Depth_range,
                       list (traits$Genus),
                       mean,na.rm=T)


# TL
TL_genus <- tapply (traits$Trophic_level,
                       list (traits$Genus),
                       mean,na.rm=T)


# going deeper in depth and food chain?

nsp_choose <- 15 #  n species to choose (among the ranked spp)


# separate data per year (it will be used several times)
# using data already filtered for reef fish species

fish_year <- split (fisheries_wtrait, fisheries_wtrait$Year)




# size and depth
fish_year_size_depth_TL <- lapply (fish_year, function (i) {
  
  
  fish_year_genus <- tapply (i$CatchAmount_t,
                             list (i$Genus_match),
                             sum)
  # Mean size among the most frequent nsp_choose 
  size_collect_species <- names (fish_year_genus[order(fish_year_genus,decreasing=T)][1:nsp_choose])
  size_collect_species <- size_genus[which(names (size_genus) %in% size_collect_species)]
  mean_size_collect_species<- mean(size_collect_species,na.rm=T)
  # mean depth of the most frequent nsp
  # five more frequentt secies
  depth_collect_species <- names (fish_year_genus[order(fish_year_genus,decreasing=T)][1:nsp_choose])
  depth_collect_species <- depth_genus[which(names (depth_genus) %in% depth_collect_species)]
  mean_depth_collect_species<- mean(depth_collect_species,na.rm=T)
  # TL
  TL_collect_species <- names (fish_year_genus[order(fish_year_genus,decreasing=T)][1:nsp_choose])
  TL_collect_species <- TL_genus[which(names (TL_genus) %in% TL_collect_species)]
  mean_TL_collect_species<- mean(TL_collect_species,na.rm=T)
  
  
  # bind 
  averages_size_depth_TL <- data.frame (size= mean_size_collect_species,
                                     depth = mean_depth_collect_species,
                                     TL = mean_TL_collect_species)
  ; # return
  averages_size_depth_TL
  
  
})



# dataframe
fish_year_df <- data.frame(size = do.call(rbind,fish_year_size_depth_TL))
fish_year_df$year <- rownames(fish_year_df)



# melt
fish_year_df <- melt (fish_year_df, id.var = "year")
levels(fish_year_df$variable)[which(levels(fish_year_df$variable) == "size.size")] <- "Size" 
levels(fish_year_df$variable)[which(levels(fish_year_df$variable) == "size.depth")] <- "Depth" 
levels(fish_year_df$variable)[which(levels(fish_year_df$variable) == "size.TL")] <- "TL" 


# 
plot_size_depth<-ggplot (fish_year_df, 
                         aes (x=as.numeric(year), 
                              y=value
                              )) + 
  
  geom_smooth(method = "gam",
              formula = y ~ s(x, bs = "cs",k=4)) + 
  facet_wrap (~variable,ncol=3,scales = "free")+
  
  geom_point() + 
  
  xlab ("Year") + ylab ("Value") +
  theme_classic() +
  theme (legend.position = c(0.12,0.90),
         axis.title = element_text(size=15),
         strip.text.x = element_text(size = 14, color = "black", 
                                     face = "bold"),
         strip.background = element_rect(color="black", 
                                         fill="gray60",
                                         size=1.5, linetype="solid"
         )) 
  
  

pdf (here ("output", "size_depth_catch_fish.pdf"),height=4,width=8)
plot_size_depth
dev.off()


# ------------------------------------------------------------

# THE TRAIT SPACE

# the regional trait space
average_traits_whole_region <- traits[,-97] %>% 
  
  group_by(Genus) %>% 
  
  summarise (Body_size = mean (Body_size,na.rm=T),  
             Trophic_level = mean (Trophic_level,na.rm=T), 
             Depth_range = mean (Depth_range,na.rm=T), 
             Diel_activity = getmode (Diel_activity), 
             Size_group = getmode (Size_group), 
             Level_water = getmode (Level_water))  %>%
  
  
  select (Genus, Body_size, Trophic_level, Depth_range, 
          Diel_activity, Size_group, Level_water)  


# the first row is empty
average_traits_whole_region <- average_traits_whole_region[-1,]

# transform categorical into ranking traits
# group size
average_traits_whole_region$Size_group [which(average_traits_whole_region$Size_group == "sol")] <- 1
average_traits_whole_region$Size_group [which(average_traits_whole_region$Size_group == "pair")] <- 2
average_traits_whole_region$Size_group [which(average_traits_whole_region$Size_group == "smallg")] <- 3
average_traits_whole_region$Size_group [which(average_traits_whole_region$Size_group == "medg")] <- 4
average_traits_whole_region$Size_group [which(average_traits_whole_region$Size_group == "largeg")] <- 5
average_traits_whole_region$Size_group <- ordered (average_traits_whole_region$Size_group) # ordered

# level water
average_traits_whole_region$Level_water [which(average_traits_whole_region$Level_water == "bottom")] <- 1
average_traits_whole_region$Level_water [which(average_traits_whole_region$Level_water == "low")] <- 2
average_traits_whole_region$Level_water [which(average_traits_whole_region$Level_water == "high")] <- 3
average_traits_whole_region$Level_water <- ordered (average_traits_whole_region$Level_water) # ordered

# daily activity
average_traits_whole_region$Diel_activity <- ifelse (average_traits_whole_region$Diel_activity %in% c("night", "both"),
                                        1,0)
average_traits_whole_region<- data.frame(average_traits_whole_region)
rownames (average_traits_whole_region) <- average_traits_whole_region$Genus
average_traits_whole_region<- average_traits_whole_region[,-1]

# reef fish genera
average_traits_whole_region <- average_traits_whole_region[which(rownames(average_traits_whole_region) %in% 
                                                                   reef_fish$Genus),]

# distance matrix
# turning ranks into numerics
average_traits_whole_region$Size_group <- as.numeric(average_traits_whole_region$Size_group)
average_traits_whole_region$Level_water <- as.numeric(average_traits_whole_region$Level_water)
average_traits_whole_region$interaction_level_group <- ifelse (is.na(average_traits_whole_region$Level_water * average_traits_whole_region$Size_group),
                                                               average_traits_whole_region$Level_water,
                                                               average_traits_whole_region$Level_water * average_traits_whole_region$Size_group)

# standardize traits
std_traits_whole <-decostand (average_traits_whole_region[,-which(colnames(average_traits_whole_region) %in% c("Diel_activity", 
                                                                                                    "Size_group", 
                                                                                                    "Level_water"))],
                              "standardize",na.rm=T)
# distance matrix
gower_matrix <- vegdist (std_traits_whole,
                         method="gower",na.rm=T)


# principal coordinate analysis
# Building the functional space based on a PCOA 
pcoa_whole<-pcoa(gower_matrix,correction = "cailliez") # quasieuclid() transformation to make the gower matrix as euclidean. nf= number of axis 

#barplot(pco$eig) # barplot of eigenvalues for each axis 
(Inertia2<-(pcoa_whole$values$Eigenvalues[1]+pcoa_whole$values$Eigenvalues[2]+pcoa_whole$values$Eigenvalues[3]) /(sum(pcoa_whole$values$Eigenvalues[which(pcoa_whole$values$Eigenvalues>0)]))) # percentage of inertia explained by the two first axes

## only the frst axis
(Inertia.first <- (pcoa_whole$values$Eigenvalues[1]) /(sum(pcoa_whole$values$Eigenvalues[which(pcoa_whole$values$Eigenvalues>0)])))
## only the frst axis
(Inertia.scnd <- (pcoa_whole$values$Eigenvalues[2]) /(sum(pcoa_whole$values$Eigenvalues[which(pcoa_whole$values$Eigenvalues>0)])))
## only the frst axis
(Inertia.trd <- (pcoa_whole$values$Eigenvalues[3]) /(sum(pcoa_whole$values$Eigenvalues[which(pcoa_whole$values$Eigenvalues>0)])))
Inertia.first+Inertia.scnd

## complete space
all <- data.frame (pcoa_whole$vectors[,1:2],
              ext = F,
              sp = rownames(average_traits_whole_region))
a <- all [chull(all[,1:2], y = NULL),] # its convex hull

# match ordination and traits
# coral associated
catched_fish <-cbind(all, ext1=ifelse(all$sp %in% 
                                      fisheries_wtrait$Genus_match,T,F))
catched_fish <-catched_fish[which(catched_fish$ext1==T),]
catched_fish_set <- catched_fish [chull(catched_fish, y = NULL),]

# catched_fish_set per region

catched_fish_region <- lapply (year_composition_region, function (i){
  
  # identify catched spp
  catched_fish <-cbind(all, ext1=ifelse(all$sp %in% 
                                          colnames(i),T,F))
  catched_fish <-catched_fish[which(catched_fish$ext1==T),]
  catched_fish_set <- catched_fish [chull(catched_fish, y = NULL),]
  catched_fish_set
})
names(catched_fish_region) <- unique(fisheries_wtrait$Region)

## plot A (complete space)
plotA <- ggplot(a, aes(Axis.1, Axis.2)) + 
  geom_point(size=2) + theme_bw()+
  geom_polygon(data=a, aes (Axis.1,Axis.2),
               alpha=0.6,
               fill="gray",
               colour = "black",
               size=1,
               linetype = 2) + # complete space
  geom_polygon(data=catched_fish_set, aes (Axis.1,Axis.2),
               alpha=0.3,
               fill="#5BB318",
               colour = "#2B7A0B",
               size=1,
               linetype = 3) +
  xlab(paste ("Axis I:", round(Inertia.first*100,2),"%"))+
  ylab(paste ("Axis II:", round(Inertia.scnd*100,2),"%"))
 

## correlations to project trait values into the ordination
correlations <- cor (data.matrix(data.frame (average_traits_whole_region[,-which(colnames(average_traits_whole_region) %in% c("Diel_activity", 
                                                                                                                              "Size_group", 
                                                                                                                              "Level_water"))],
                                pcoa_whole$vectors[,1:3])),
                     use = "complete.obs")
correlations<-correlations [-which(rownames(correlations) %in% c("Axis.1","Axis.2","Axis.3")),
                                   c("Axis.1","Axis.2")]# interesting correlations

# show traits in the trait space


plotA <- plotA + geom_segment(aes(x = 0, y = 0, 
                         xend = correlations[1,1]*0.2, 
                         yend = correlations[1,2]*0.2),size = 1,
                     color="black",
                     arrow = arrow(length = unit(.35, "cm")))  + 
  ## annotate
  annotate(geom="text",x=correlations[1,1]*0.19,
           y=correlations[1,2]*0.30,label="Total length",
           color="black") +
  
  geom_segment(aes(x = 0, y = 0, 
                   xend = correlations[2,1]*0.2, 
                   yend = correlations[2,2]*0.2),size = 1,
               color="black",
               arrow = arrow(length = unit(.35, "cm"))) + 
  annotate(geom="text",x=correlations[2,1]*0.25,
           y=correlations[2,2]*0.22,label="Trophic level",
           color="black") +
  
  geom_segment(aes(x = 0, y = 0, 
                   xend = correlations[3,1]*0.2, 
                   yend = correlations[3,2]*0.2),size = 1,
               color="black",
               arrow = arrow(length = unit(.35, "cm"))) + 
  annotate(geom="text",x=correlations[3,1]*0.35,
           y=correlations[3,2]*0.2,label="Depth range",
           color="black") +
  
  geom_segment(aes(x = 0, y = 0, 
                   xend = correlations[4,1]*0.2, 
                   yend = correlations[4,2]*0.2),size = 1,
               color="black",
               arrow = arrow(length = unit(.35, "cm"))) + 
  annotate(geom="text",x=correlations[4,1]*0.25,
           y=correlations[4,2]*0.29,label="Level water : Group size",
           color="black")  
  
 


# region spaces
plotB <- lapply (catched_fish_region, function (i)
  
  
  ggplot(a, aes(Axis.1,Axis.2)) + 
    geom_point(size=2) + theme_bw()+
    geom_polygon(data=a, aes (Axis.1,Axis.2),
                 alpha=0.6,
               fill="gray",
               colour = "black",
               size=1,
               linetype = 2) + # complete space
  geom_polygon(data=i, aes (Axis.1,Axis.2),
               alpha=0.3,
               fill="#5BB318",
               colour = "#2B7A0B",
               size=1,
               linetype = 3) 

)


# save
pdf (here ("output", "trait_space_fishing.pdf"),height=7,width=5)

grid.arrange(plotA,
             plotB[[1]]+theme(axis.text = element_blank(),
                              axis.title = element_blank())+
               ggtitle("North"),
             plotB[[2]]+theme(axis.text = element_blank(),
                              axis.title = element_blank())+
               ggtitle("Northeast"),
             plotB[[3]]+theme(axis.text = element_blank(),
                              axis.title = element_blank())+
               ggtitle("Southeast"),
             plotB[[4]]+theme(axis.text = element_blank(),
                              axis.title = element_blank())+
               ggtitle("South"),
             layout_matrix = rbind (c(1,1),
                                    c(1,1),
                                    c(2,3),
                                    c(4,5)))


dev.off()

# ==============================================================

# Are fisheries getting nutritionally poorer over time?
# zinc

zinc_genus <- tapply (fisheries_wtrait$Zinc_mu,
                       list (fisheries_wtrait$Genus_match,
                             fisheries_wtrait$Region),
                       mean,na.rm=T)

# iron
iron_genus <- tapply (fisheries_wtrait$Iron_mu,
                      list (fisheries_wtrait$Genus_match,
                            fisheries_wtrait$Region),
                      mean,na.rm=T)

# omega 3
omega_genus <- tapply (fisheries_wtrait$Omega_3_mu,
                      list (fisheries_wtrait$Genus_match,
                            fisheries_wtrait$Region),
                      mean,na.rm=T)

# protein
protein_genus <- tapply (fisheries_wtrait$Protein_mu,
                      list (fisheries_wtrait$Genus_match,
                            fisheries_wtrait$Region),
                      mean,na.rm=T)

# calcium
calcium_genus <- tapply (fisheries_wtrait$Calcium_mu,
                         list (fisheries_wtrait$Genus_match,
                               fisheries_wtrait$Region),
                         mean,na.rm=T)

# selenium
selenium_genus <- tapply (fisheries_wtrait$Selenium_mu,
                         list (fisheries_wtrait$Genus_match,
                               fisheries_wtrait$Region),
                         mean,na.rm=T)

# vitA
vitA_genus <- tapply (fisheries_wtrait$Vitamin_A_mu,
                          list (fisheries_wtrait$Genus_match,
                                fisheries_wtrait$Region),
                          mean,na.rm=T)

# list of nutrient data
nutrient_data <- list (zinc_genus,iron_genus,omega_genus,protein_genus,
                       calcium_genus,selenium_genus , vitA_genus)
# extract data
fish_year_nutrition <- lapply (fish_year, function (i) 
  do.call(rbind, lapply (nutrient_data, function (k)  { # bind nutrient data
  
  
  fish_year_genus <- tapply (i$CatchAmount_t,
                             list (i$Genus_match,
                                   i$Region),
                             sum)
  
  # Mean size among the most frequent nsp_choose 
  # across regions
  nut_reg <- do.call(cbind,   # melt nutrient data per region
    
    lapply (seq (1,ncol (fish_year_genus)), function (reg) {
  
        nut_collect_species <- names (fish_year_genus[,reg])[order(fish_year_genus[,reg],decreasing=T)][1:nsp_choose]
        nut_collect_species <- k[which(names (k[,reg]) %in% nut_collect_species),reg]
        mean_nut_collect_species<- mean(nut_collect_species,na.rm=T)
        mean_nut_collect_species
  }))
  colnames(nut_reg) <- colnames(fish_year_genus)
  ; # return
  nut_reg
  
  
})))
# name rows
fish_year_nutrition<- lapply (fish_year_nutrition, function (i) {
  rownames (i)<- c("Zinc","Iron", "Omega-3", "Protein","Calcium","Selenium","Vitamin-A")
  ;
  i
})
# name year
names (fish_year_nutrition) <- names(fish_year)
# melt
fish_year_nutrition <-( do.call(rbind.data.frame, fish_year_nutrition))
fish_year_nutrition$year <- sapply (strsplit ( rownames(fish_year_nutrition), "\\."), "[",1)
fish_year_nutrition$nutrient <- sapply (strsplit ( rownames(fish_year_nutrition), "\\."), "[",2)
# melt to fit ggplot format
fish_year_nutrition<- melt(fish_year_nutrition,id.var=c("year", "nutrient"))


# plot 

plot_nut <- ggplot (fish_year_nutrition, aes (x=year, 
                                  y=value,
                                  group=variable,
                                  colour=variable)) + 
  geom_point(alpha=0.2)+
  facet_wrap(~nutrient,scales = "free_y",ncol=7)+
  geom_smooth(method = "gam",
              formula = y ~ s(x, bs = "cs",k=4))+
  scale_x_discrete(
    
    breaks = seq(min(fish_year_nutrition$year), 
                 max(fish_year_nutrition$year), by = 20),
    
  ) + 
  ylab ("Content of nutrients") + 
  xlab("Year")+
  theme_classic() + 
  theme(axis.text = element_text(size=5),
        axis.title = element_text(size=13),
        strip.text = element_text(face="bold"),
        strip.text.x = element_text(size = 10, color = "black", 
                                    face = "bold"),
        strip.background = element_rect(color="black", 
                                        fill="gray60",
                                        size=1.5, linetype="solid"
        )) + 
  scale_colour_viridis_d(name = "Region")
  


plot_nut



# ordination to show the nutrition content of fish genus
genus_nutrient_composition <- lapply (nutrient_data, function (i) 
  
          apply (i,1, mean,na.rm=T)
          
)
genus_nutrient_composition <- do.call(cbind,genus_nutrient_composition)
colnames(genus_nutrient_composition) <- c("Zinc","Iron", "Omega-3", "Protein","Calcium","Selenium","Vitamin-A")



# removing missing data
genus_nutrient_composition<-genus_nutrient_composition[which(rowSums(genus_nutrient_composition>0,
                                                                     na.rm=T)>0),]

genus_data <- rownames(genus_nutrient_composition)


# standardize values
genus_nutrient_composition<-apply (genus_nutrient_composition, 2,scale)
rownames(genus_nutrient_composition) <- genus_data


# weigth nutrient composition by catch amount
catch_amount_genus <- tapply (fisheries_wtrait$CatchAmount_t,
                          list (fisheries_wtrait$Genus_match),
                      sum,na.rm=T)
# removing missing data
catch_amount_genus<-catch_amount_genus[which(rownames (catch_amount_genus) %in% genus_data)]
catch_amount_genus[order(catch_amount_genus,decreasing=T)]

# standardize values
catch_amount_genus<-decostand ((catch_amount_genus),method= "standardize")
catch_amount_genus[order(catch_amount_genus)]


# distance matrix
dist_nut <- vegdist (genus_nutrient_composition*catch_amount_genus[,1],  # weighted matrix
                     "euclidean")

# ordination
pcoa_nut <- pcoa(dist_nut)


# variance explained by the first and second axes
Exp_axis1<-pcoa_nut$values$Eigenvalues[1]/sum(pcoa_nut$values$Eigenvalues)*100
Exp_axis2<-pcoa_nut$values$Eigenvalues[2]/sum(pcoa_nut$values$Eigenvalues)*100

# data

#pcoa_fish_year <- melt (pcoa_fish_year)
pcoa_nut <- data.frame (pcoa_nut$vectors[,1:2],
                   sp = rownames(pcoa_nut$vectors))



# corelation to project nutrients
correlation_nut <-data.frame( cor (genus_nutrient_composition,
                                   pcoa_nut[,1:2],
                               method = "pearson"))
correlation_nut$nutrient <- rownames(correlation_nut)
# ordination
# help here
# https://ggplot2.tidyverse.org/reference/geom_path.html
ordination_nut<-ggplot(data=pcoa_nut,
                    aes(x=Axis.1,y=Axis.2)) + 
  geom_point(colour="black",alpha=0.5,stroke=1.5,
             shape=1,size=3) + # add the point markers
  #geom_path(aes(colour=as.numeric(sp)),alpha=0.5)+
  
  geom_text_repel(aes(label=sp ),size=3,vjust=1,
                  max.overlaps = 50)+#ifelse(Axis.1>1.5 |
                            #   Axis.1 < -1 |
                            #   Axis.2 >1 |
                            #   Axis.2 < -2,as.character(sp),'')),
            #size=2.5,vjust=1) +
  #geom_path(aess(group=year)) +# add the site labels
  #scale_colour_manual(values=c("A" = "red", "B" = "blue")) +
  coord_equal() +
  theme_bw() +
  theme(legend.position = "none")




# project genus names
ordination_nut<-ordination_nut + 
  
                    geom_text_repel(data = correlation_nut,
                                           aes (x=Axis.1*4,
                                                y = Axis.2*4,
                                                label = (nutrient)),
                                           size=5,fontface = "italic",
                                           colour="#1363DF",
                                           max.overlaps = 100)  + 
  xlab (paste ("Axis 1 (", round(Exp_axis1,2), "%)",sep="")) +
  ylab (paste ("Axis 2 (", round(Exp_axis2,2), "%)",sep=""))

ordination_nut


# arrange


pdf (here ("output", "nutrients.pdf"),height=5,width=9)

composition2<-grid.arrange(ordination_nut,
             plot_nut,
             ncol=5,nrow=5,
             layout_matrix = rbind (c (1,1,1,1,1),
                                    c (1,1,1,1,1),
                                    c (1,1,1,1,1),
                                    c (2,2,2,2,2),
                                    c (2,2,2,2,2)))


dev.off()

# ======================

# 
#source (here ("R", "function_poncho.R"))
#
#pdf("year.pdf")
#poncho (log(year_composition+1),
#        gradient = seq(1,nrow(year_composition)),
#        col = as.numeric(as.factor(fisheries_wtrait$Diet_2012[match (colnames(year_composition),
#                                                fisheries_wtrait$Genus)])),lty = 0)
#
#dev.off()
#


# ===========================
# tRAIT COMSPOTION


TL_fisheries <- tapply (fisheries_wtrait$CatchAmount_t,
        list(fisheries_wtrait$Year,
             fisheries_wtrait$Diet,
             fisheries_wtrait$Region),
        sum, default = 0) 

TL_fisheries<-melt (TL_fisheries)
colnames(TL_fisheries) <- c("Year", "TrGroup", "Region", "value")

## pot trnds over time
trophic_level_trends <- ggplot (TL_fisheries, aes (x=Year, y=value,
                     colour = TrGroup)) + 
  facet_wrap(~Region,scales = "free")+
  geom_line(size=1) + 
  xlab ("Year") + 
  ylab ("Sum catch (thousands of tonnes)") + 
  theme_classic() + 
  scale_colour_viridis_d(option = "viridis")

pdf (here ("output", "TL_trends.pdf"),height=5,width=6)
trophic_level_trends
dev.off()


## ======================================
# herbivorous over time



selected_fish_trend <- tapply (fisheries_wtrait$CatchAmount_t,
                  list(fisheries_wtrait$Year,
                       #fisheries_wtrait$Diet_2012,
                       fisheries_wtrait$Region,
                       fisheries_wtrait$Genus_match),
                  sum) 

selected_fish_trend<-melt (selected_fish_trend)
colnames(selected_fish_trend) <- c("Year",  "Region", "Genus_match","value")

# plot
sel_spp_trends <- ggplot (selected_fish_trend[which(selected_fish_trend$Genus_match %in% 
                      c("Scarus", 
                        "Sparisoma",
                        "Balistes", 
                        "Lutjanus",
                        "Mycteroperca",
                        "Epinephelus",
                        "Ocyurus")),],
        
        
        aes (x=Year, y=log(value),
             colour = Genus_match),alpha=0.5) + 
  facet_wrap(~Region,scales = "free")+
  geom_line(size=1) + 
  xlab ("Year") + 
  ylab ("Sum catch (thousands of tonnes)") + 
  theme_classic()  +
  scale_colour_viridis_d(option ="viridis", begin=0.1,end=0.9)
  





## --------------------
# trends
# species per region and year
# + current
# regional scale




agg1<-aggregate(CatchAmount_t~Region+Genus_match+Year,sum,data=fisheries_wtrait)
# change region names
agg1$Region[which(agg1$Region == "Sul")] <- "South"
agg1$Region[which(agg1$Region == "SE")] <- "Southeast"
agg1$Region[which(agg1$Region == "NE")] <- "Northeast"
agg1$Region[which(agg1$Region == "Norte")] <- "North"


# plot
trend_sel_sp_region <- ggplot(agg1[which(agg1$Genus_match %in% 
                    c("Scarus", 
                      "Sparisoma",
                     "Balistes",
                     "Mycteroperca",
                     "Epinephelus")),],
       aes (x=Year, y=sqrt(CatchAmount_t),colour = Genus_match))+
  geom_point() + 
 
  facet_wrap(~Region,scales = "free")+
  
  poison_smooth(formula = y ~ s(x, bs = "cs",k=4)) + theme_classic() + 
  scale_colour_viridis_d(option = "magma", begin=0.1,end=0.9, name = "Genus") + 
  theme (strip.text = element_text(face="bold"),
         strip.text.x = element_text(size = 10, color = "black", 
                                     face = "bold"),
         strip.background = element_rect(color="black", 
                                         fill="gray60",
                                         size=1.5, linetype="solid"
         ))
  
trend_sel_sp_region



# lutjanids



trend_sel_sp_region_lutjanus <- ggplot(agg1[which(agg1$Genus_match %in% 
                                           c("Lutjanus",
                                             "Ocyurus")),],
                              aes (x=Year, y=sqrt(CatchAmount_t),colour = Genus_match))+
  geom_point() + 
  facet_wrap(~Region,scales = "free")+
  poison_smooth(formula = y ~ s(x, bs = "cs",k=4)) + theme_classic() + 
  scale_colour_viridis_d(option = "magma", begin=0.1,end=0.5, name= "") + 
  theme (strip.text = element_text(face="bold"),
         strip.text.x = element_text(size = 10, color = "black", 
                                     face = "bold"),
         strip.background = element_rect(color="black", 
                                         fill="gray60",
                                         size=1.5, linetype="solid"
         ))

trend_sel_sp_region_lutjanus


## cartilaginous fish

trend_sel_sp_region_cart <- ggplot(agg1[which(agg1$Genus_match %in% 
                                           c("Carcharhinus", "Ginglymostoma")),],
                              aes (x=Year, y=sqrt(CatchAmount_t),colour = Genus_match))+
  geom_point() + 
  facet_wrap(~Region,scales = "free")+
  poison_smooth(formula = y ~ s(x, bs = "cs",k=4)) + theme_classic() + 
  scale_colour_viridis_d(option = "magma", begin=0.1,end=0.6, name = "") + 
  theme (strip.text = element_text(face="bold"),
         strip.text.x = element_text(size = 10, color = "black", 
                                     face = "bold"),
         strip.background = element_rect(color="black", 
                                         fill="gray60",
                                         size=1.5, linetype="solid"
         ))

# save
pdf (here ("output", "sel_spp_trends.pdf"),height=12,width=7)

grid.arrange (trend_sel_sp_region+theme (axis.title.x = element_blank(), 
                                         axis.text = element_text(size=7), 
                                          legend.key.width = unit(0.1,"cm"),
                                         legend.key.height = unit(0.5,"cm"),
                                         legend.title = element_blank(),
                                         legend.text = element_text(size=8)),
              trend_sel_sp_region_lutjanus + theme (axis.title.x = element_blank(), 
                                                    axis.text = element_text(size=7),
                                                    legend.key.width = unit(0.1,"cm"),
                                                    legend.key.height = unit(0.5,"cm"),
                                                    legend.title = element_blank(),
                                                    legend.text = element_text(size=8)),
              trend_sel_sp_region_cart+theme (axis.title.x = element_blank(), 
                                              axis.text = element_text(size=7),
                                              legend.key.width = unit(0.1,"cm"),
                                              legend.key.height = unit(0.5,"cm"),
                                              legend.title = element_blank(),
                                              legend.text = element_text(size=8)))

dev.off()






