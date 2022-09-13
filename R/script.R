
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
traits$Depth_mean<- apply (cbind (traits$Depth_max,  
                                  traits$Depth_min),1,mean,na.rm=T)


# aggregate data of QUimbayo et al. at the genus level
require(dplyr)

traits <- traits %>% 
  
  group_by(Genus) %>% 
  
  summarise (Body_size = mean (Body_size,na.rm=T),  
             Trophic_level = mean (Trophic_level,na.rm=T), 
             Depth_range = mean (Depth_range,na.rm=T), 
             Depth_mean = mean (Depth_mean,na.rm=T),
             Depth_max = mean (Depth_max,na.rm=T), 
             Depth_min = mean (Depth_min,na.rm=T),
             Diet = getmode(Diet),
             Diel_activity = getmode (Diel_activity), 
             Size_group = getmode (Size_group), 
             Level_water = getmode (Level_water))  %>%
  
  
  select (Genus, Body_size, Trophic_level, 
          Depth_min,Depth_max,Depth_range,
          Depth_mean, Diet,
          Diel_activity, Size_group, Level_water)




# fisheries ~year
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
                                  
                                   (nutrients$Genus)),
                           grep("mu", colnames(nutrients))]) # mu is the averaged nutrient estimates




# fisheries data (matching with Pinheiro et al. 2018)
# reef fish genus
table(unique(fisheries$Genus_match) %in% reef_fish$Genus )
fisheries_wtrait<-fisheries[which(fisheries$Genus_match %in% reef_fish$Genus),]




# match trait & fisheries
fisheries_wtrait <- cbind (fisheries_wtrait,
                           
                           traits [match (fisheries_wtrait$Genus,traits$Genus ),]
                           
                           )


# check some examples

traits [grep("Haemulon", traits$Genus),]
traits [grep("Kyphosus", traits$Genus),]
fisheries_wtrait [grep("Haemulon", fisheries_wtrait$Genus_match),]
fisheries_wtrait [grep("Kyphosus", fisheries_wtrait$Genus_match),]


# omnivore spp
unique(fisheries_wtrait [grep("om", fisheries_wtrait$Diet), "Genus"])




# equal column names (remove one)
#table(fisheries_wtrait[,25] == fisheries_wtrait[,117])
#fisheries_wtrait<- fisheries_wtrait[,-117]


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



# plot
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
  theme (legend.position = "none",
         axis.title = element_blank(),
         strip.text.x = element_text(size = 14, color = "black", 
                                     face = "bold"),
         strip.background = element_rect(color="black", 
                                         fill="gray60",
                                         size=1.5, linetype="solid"
         ))

  


# overall trend
overall_trend <- fisheries_wtrait %>%
  
  group_by(Year,Sector) %>% 
  
  
  
  summarize(sum_catch=sum(CatchAmount_t),
  ) 


# plot overall trend
catch_overall_trend <- ggplot (overall_trend, aes (x=Year, 
                                            y=sum_catch,
                                            colour = Sector)) + 
  geom_line(size=1.5) + 
  scale_fill_viridis_d(option ="viridis", begin = 0.3,end=0.8) + 
  theme_classic() + 
  scale_colour_viridis_d(option ="viridis", begin = 0.3,end=0.8) + 
  theme (legend.position = c(0.22,0.85),
         axis.title = element_blank(),
         strip.text.x = element_text(size = 14, color = "black", 
                                     face = "bold"),
         strip.background = element_rect(color="black", 
                                         fill="gray60",
                                         size=1.5, linetype="solid"
         )) 



# arrange plots
require(gridExtra)

pdf (here ("output", "catch_year_plot.pdf"),height=6,width=6)

grid.arrange (catch_overall_trend, 
              catch_year_plot,
              nrow =4, ncol =4,
              layout_matrix = rbind (c(1,1,1,1),
                                     c(1,1,1,1),
                                     c(2,2,2,2),
                                     c(2,2,2,2)),
              left = "Sum of the Catch Amount (T)"
)
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



# 5% of the year catch
percentage_for_bycatch <- 0.01

# filter
bycatch_composition <- rowSums(year_composition [,-1])*percentage_for_bycatch

# filter
year_composition_filtered <- lapply (seq (1,nrow (year_composition)), function (i)
  
      unlist(ifelse (year_composition[i,-1] < bycatch_composition[i], 0,
              year_composition[i,-1]))

)
year_composition_filtered <- do.call(rbind, year_composition_filtered)
rownames(year_composition_filtered) <- year_composition$Year
colnames(year_composition_filtered) <- colnames(year_composition)[-1]


# remove species without catches
year_composition_filtered <- year_composition_filtered[,which(colSums(year_composition_filtered)>0)]


# vegan:: beta diversity between year
#  hellinger transformed dataset
require(vegan)
dist_composition <- vegdist (decostand(year_composition_filtered,'hell'),
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



# point size (catch per year)
catch_year <- tapply (fisheries_wtrait$CatchAmount_t,
                      list(fisheries_wtrait$Year),
                      sum) 

# bind
pcoa_fish_year$catch <- (catch_year) # point size (half size)


# ordination (projection of beta diversity )
# help here
# https://ggplot2.tidyverse.org/reference/geom_path.html
ordination1<-ggplot(data=pcoa_fish_year,
       aes(x=Axis.1,y=Axis.2)) + 
  geom_point(aes(colour=as.numeric(year),
                 size = catch),
             
             shape=1)+
   geom_path(aes(colour=as.numeric(year)),alpha=0.5)+

  geom_text(aes(label=year),
                size=2.5,vjust=-1) +
  #geom_path(aess(group=year)) +# add the site labels
  #scale_colour_manual(values=c("A" = "red", "B" = "blue")) +
  coord_equal() +
  theme_bw() +
  
  theme(legend.position = "none")


# correlation of genus to the axes

correlation <-data.frame( cor (decostand(year_composition_filtered,'hell'),
                    pcoa_fish_year[,1:2],
                    method = "pearson"))

rownames(correlation) <- colnames(year_composition_filtered)
correlation$species <- rownames(correlation)

# order

correlation[order(correlation[,1],decreasing=T),][1:8,]
correlation[order(correlation[,1],decreasing=F),][1:8,]
correlation[order(correlation[,2],decreasing=T),][1:8,]



# project genus names
ordination1<-ordination1 + geom_text_repel(data = correlation[order(correlation[,2],decreasing=T),][1:10,],
                        aes (x=Axis.1*0.2,
                             y = Axis.2*0.2,
                             label = (species)),
                        size=2,fontface = "italic",
                        colour="#1363DF",
                        max.overlaps = 100) +
  geom_text_repel(data = correlation[order(correlation[,1],decreasing=F),][1:10,],
            aes (x=Axis.1*0.2,
                 y = Axis.2*0.2,
                 label = (species)),
            size=2,fontface = "italic",
            colour = "#06283D",
            max.overlaps = 100) + 
  geom_text_repel(data = correlation[order(correlation[,1],decreasing=T),][1:10,],
            aes (x=Axis.1*0.22,
                 y = Axis.2*0.22,
                 label = (species)),
            size=2,fontface = "italic",
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



# 5% of the year catch
bycatch_region <- lapply (year_composition_region, function (i) 
  
        rowSums( i [,-1])*percentage_for_bycatch
        
)

# filter
year_region_composition_filtered <- lapply (seq (1,length (year_composition_region)), function (i) 
  
        lapply (seq (1,nrow (year_composition_region[[i]])), function (k)
  
                unlist(ifelse (year_composition_region[[i]][k,-1] < bycatch_region[[i]][k], 
                               0,
                               year_composition_region[[i]][k,-1]))
            
))


# melt
year_region_composition_filtered <-lapply (year_region_composition_filtered, function (i) 
  
          do.call(rbind, i)
          
)          

# set names
year_region_composition_filtered <- lapply (seq (1,length (year_region_composition_filtered)), function (i){
  
  rownames(year_region_composition_filtered[[i]]) <- year_composition$Year
  colnames(year_region_composition_filtered[[i]]) <- colnames(year_composition_region[[i]])[-1]
  year_composition_region[[i]]
})
# remove genus without catches
year_region_composition_filtered <- lapply (year_region_composition_filtered, function (i) 
  
    i[,which(colSums(i)>0)]
    
)



# vegan beta diversity
library(tidyverse)
require(vegan)
dist_composition_pcoa <- lapply (year_composition_region, function (i){
  
  
  dist_composition <- vegdist (decostand(i,'hell'),
                               method = "bray",na.rm=T)
  
  # pcoa
  pcoa_fish_year <- pcoa(dist_composition)
  
  # variance explained by the first and second axes
  Exp_axis1<-pcoa_fish_year$values$Eigenvalues[1]/sum(pcoa_fish_year$values$Eigenvalues)*100
  Exp_axis2<-pcoa_fish_year$values$Eigenvalues[2]/sum(pcoa_fish_year$values$Eigenvalues)*100
  
  
  #pcoa_fish_year <- melt (pcoa_fish_year)
  pcoa_fish_year <- cbind (pcoa_fish_year$vectors,
                           year = year_composition$Year)
  
  # dataframe with data
  pcoa_fish_year<-as.data.frame(pcoa_fish_year)
  
  ;
  pcoa_fish_year
})


# composition per region
comp_change <- bind_rows(pcoa_fish_year %>% 
                           bind_cols(region = "Brazil", Year = unique(fisheries_wtrait$Year)),
                          dist_composition_pcoa[[1]] %>% 
                           bind_cols(region = "Norte", Year = unique(fisheries_wtrait$Year)),
                         dist_composition_pcoa[[2]] %>% 
                           bind_cols(region = "NE", Year = unique(fisheries_wtrait$Year)),
                         dist_composition_pcoa[[3]] %>% 
                           bind_cols(region = "SE", Year = unique(fisheries_wtrait$Year)),
                         dist_composition_pcoa[[4]] %>% 
                           bind_cols(region = "Sul", Year = unique(fisheries_wtrait$Year))) %>% 
  data.frame() %>% dplyr::rename(change = Axis.1) %>% 
  ggplot(aes(x = Year, y = change)) +
  geom_point() +
  facet_wrap(~ region, ncol = 5) +
  theme_classic() +
  geom_smooth() +
  labs(x = "", y = "Change in composition\n(First PCoA Axis)") + 
  theme (legend.position = c(0.12,0.90),
         axis.title = element_text(size=15),
         strip.text.x = element_text(size = 14, color = "black", 
                                     face = "bold"),
         strip.background = element_rect(color="black", 
                                         fill="gray60",
                                         size=1.5, linetype="solid"
         )) 



# ordination per region
# point size (catch per year and region)
catch_year_region <- tapply (fisheries_wtrait$CatchAmount_t,
                      list(fisheries_wtrait$Region,
                           fisheries_wtrait$Year),
                      sum) 



# ordination (projection of beta diversity )
# help here
# https://ggplot2.tidyverse.org/reference/geom_path.html

# one plot per region 
ordination1_reg <- lapply (seq(1,length(dist_composition_pcoa)), function (i) {
        
        ordination1_reg<-ggplot(data=dist_composition_pcoa[[i]],
                            aes(x=Axis.1,y=Axis.2)) + 
          geom_point(aes(colour=as.numeric(year),
                         size = catch_year_region[i,]),
                     
                     shape=1)+
          geom_path(aes(colour=as.numeric(year)),alpha=0.5)+
          
          geom_text(aes(label=year),
                    size=2.5,vjust=-1) +
          #geom_path(aess(group=year)) +# add the site labels
          #scale_colour_manual(values=c("A" = "red", "B" = "blue")) +
          coord_equal() +
          theme_bw() +
          
          theme(legend.position = "none",
                axis.text = element_blank(),
                axis.title = element_blank())
        
        
        # correlation of genus to the axes
        
        correlation <-data.frame( cor (decostand(year_region_composition_filtered[[i]][,-1],'hell'),
                                       dist_composition_pcoa[[i]][,1:2],
                                       method = "pearson"))
        
        rownames(correlation) <- colnames(year_region_composition_filtered[[i]])[-1]
        correlation$species <- rownames(correlation)
        
        # order
        
        correlation[order(correlation[,1],decreasing=T),][1:8,]
        correlation[order(correlation[,1],decreasing=F),][1:8,]
        correlation[order(correlation[,2],decreasing=T),][1:8,]
        
        
        
        # project genus names
        ordination1_reg<-ordination1_reg + 
          
          geom_text_repel(data = correlation[order(correlation[,2],decreasing=T),][1:5,],
                                                   aes (x=Axis.1*0.2,
                                                        y = Axis.2*0.2,
                                                        label = (species)),
                                                   size=1.5,fontface = "italic",
                                                   colour="#1363DF",
                                                   max.overlaps = 100) +
          geom_text_repel(data = correlation[order(correlation[,1],decreasing=F),][1:5,],
                          aes (x=Axis.1*0.2,
                               y = Axis.2*0.2,
                               label = (species)),
                          size=1.5,fontface = "italic",
                          colour = "#06283D",
                          max.overlaps = 100) + 
          geom_text_repel(data = correlation[order(correlation[,1],decreasing=T),][1:5,],
                          aes (x=Axis.1*0.22,
                               y = Axis.2*0.22,
                               label = (species)),
                          size=1.5,fontface = "italic",
                          colour = "#47B5FF",
                          max.overlaps = 100) 
        
          ordination1_reg

})



# arrange plots
pdf (here ("output", "fisheries_composition.pdf"),height=10,width=10)

compostion1<-grid.arrange(
                 ordination1,
                 comp_change,
                 ordination1_reg[[1]],
                 ordination1_reg[[2]],
                 ordination1_reg[[3]],
                 ordination1_reg[[4]],
             ncol=8,nrow=6,
             layout_matrix = rbind (c (1,1,1,1,1,1,1,1),
                                    c (1,1,1,1,1,1,1,1),
                                    c (2,2,2,2,2,2,2,2),
                                    c (2,2,2,2,2,2,2,2),
                                    c (3,3,4,4,5,5,6,6),
                                    c (3,3,4,4,5,5,6,6)))


dev.off()


# ----------------------------------------------------------------
# how do fisheries changed in terms of functional composition?


# trait composition

average_traits <- fisheries_wtrait %>% 
  
                        
                        group_by(Genus_match) %>% 
  
                          summarise (Body_size = mean (Body_size,na.rm=T),  
                                     Trophic_level = mean (Trophic_level,na.rm=T), 
                                     Depth_range = mean (Depth_range,na.rm=T), 
                                     Depth_mean = mean (Depth_mean,na.rm=T),
                                     Depth_max = mean (Depth_max,na.rm=T), 
                                     Depth_min = mean (Depth_min,na.rm=T),
                                     Diel_activity = getmode (Diel_activity), 
                                     Size_group = getmode (Size_group), 
                                     Level_water = getmode (Level_water))  %>%
  
                          
                          select (Genus_match, Body_size, Trophic_level, 
                                  Depth_min,Depth_max,Depth_range,
                                  Depth_mean, Diel_activity, Size_group, Level_water)  
                          


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
                                                                        "Size_group",
                                                                        "Depth_range",
                                                                        "Depth_mean"))])

# functional composition
funct_comp <- matrix.t(organized_data$community, 
                       organized_data$traits, 
                       scale = F, 
                       ranks = F, 
                       notification = TRUE)


# correlation of overall trends and traits
data_funct_trend <- data.frame (funct_comp$matrix.T,
                              beta = pcoa_fish_year$Axis.1,
                              year = rownames(pcoa_fish_year))

data_funct_trend <- melt (data_funct_trend,id.vars = c("year","beta")) # melt


pdf (here ("output", "composition_funct.pdf"))
ggplot (data_funct_trend, aes (x=beta, y = value)) +
  geom_point() + 
  facet_wrap(~variable, scale = "free_y") +
  geom_smooth(method = "gam",
              formula = y ~ s(x, bs = "cs",k=4),col ="#5BB318") + 
  ylab ("Community Weighted Means of Trait Values") + 
  xlab ("First PCoA Axis") + 
  theme (strip.text.x = element_text(size = 10, color = "black", 
                                     face = "bold"),
         strip.background = element_rect(color="black", 
                                         fill="gray60",
                                         size=1.5, linetype="solid"
         ))
dev.off()



## composition
## vegan:: beta diversity between year
##  hellinger transformed dataset
#require(vegan)
#dist_funct_composition <- vegdist (funct_comp$matrix.T,
#                                  method = "euclidean",na.rm=T)
#
#
#
## pcoa
#require(ape)
#pcoa_fish_year_func <- pcoa(dist_funct_composition)
## variance explained by the first and second axes
#(Exp_axis1_func<-pcoa_fish_year_func$values$Eigenvalues[1]/sum(pcoa_fish_year_func$values$Eigenvalues)*100)
#(Exp_axis2_func<-pcoa_fish_year_func$values$Eigenvalues[2]/sum(pcoa_fish_year_func$values$Eigenvalues)*100)
#
#
##pcoa_fish_year <- melt (pcoa_fish_year)
#pcoa_fish_year_func <- cbind (pcoa_fish_year_func$vectors,
#                         year = as.numeric(rownames(organized_data$community)))
#
## dataframe with data
#pcoa_fish_year_func<-as.data.frame(pcoa_fish_year_func)
#
#
## point size (catch per year)
#
#catch_year <- tapply (fisheries_wtrait$CatchAmount_t,
#                      list(fisheries_wtrait$Year),
#                      sum) 
## bind
#pcoa_fish_year_func$catch <- (catch_year) # point size (half size)
#
## ordination (projection of beta diversity )
## help here
## https://ggplot2.tidyverse.org/reference/geom_path.html
#ordination1_func <- ggplot(data=pcoa_fish_year_func,
#                    aes(x=Axis.1,y=Axis.2)) + 
#  
#  geom_point(aes(colour=as.numeric(year),
#                 size = catch),
#                
#               shape=1) + # add the point markers
#  geom_path(aes(colour=as.numeric(year)),alpha=0.5)+
#  
#  geom_text(aes(label=year),
#            size=2.5,vjust=-1) +
#  #geom_path(aess(group=year)) +# add the site labels
#  #scale_colour_manual(values=c("A" = "red", "B" = "blue")) +
#  coord_equal() +
#  theme_bw() +
#  theme(legend.position = "none")
#
#
#ordination1_func
#
## correlation of genus to the axes
#correlation_func <-data.frame( cor (funct_comp$matrix.T,
#                               pcoa_fish_year_func[,1:2],
#                               method = "pearson"))
#correlation_func$trait <- rownames(correlation_func)
#
## order
#
#correlation_func[order(correlation_func[,1],decreasing=T),]
#correlation_func[order(correlation_func[,1],decreasing=F),]
#correlation_func[order(correlation_func[,2],decreasing=T),]
#
#
#
## project genus names
#ordination1_func<-ordination1_func + 
#  
#  geom_text_repel(data = correlation_func[order(correlation_func[,2],
#                                                decreasing=T),],
#                                           aes (x=Axis.1*0.02,
#                                                y = Axis.2*0.02,
#                                                label = (trait)),
#                                           size=3,fontface = "italic",
#                                           colour="#1363DF",
#                                           max.overlaps = 100) + 
#  xlab (paste ("Axis 1 (", round(Exp_axis1_func,2), "%)",sep="")) +
#  ylab (paste ("Axis 2 (", round(Exp_axis2_func,2), "%)",sep=""))
#
#
#
### =============================================== 
## change in composition per region
## a pesca mudou ao longo do tempo?
#
#func_composition_region <- lapply (unique(fisheries_wtrait$Region), function (i){
#  
#  # trait composition
#        
#  average_traits <- fisheries_wtrait[which(fisheries_wtrait$Region == i),] %>% 
#          
#          
#          group_by(Genus_match) %>% 
#          
#          summarise (Body_size = mean (Body_size,na.rm=T),  
#                     Trophic_level = mean (Trophic_level,na.rm=T), 
#                     Depth_range = mean (Depth_range,na.rm=T), 
#                     Diel_activity = getmode (Diel_activity), 
#                     Size_group = getmode (Size_group), 
#                     Level_water = getmode (Level_water))  %>%
#          
#          
#          select (Genus_match, Body_size, Trophic_level, Depth_range, 
#                  Diel_activity, Size_group, Level_water)  
#        
#        # transform categorical into ranking traits
#        # group size
#        average_traits$Size_group [which(average_traits$Size_group == "sol")] <- 1
#        average_traits$Size_group [which(average_traits$Size_group == "pair")] <- 2
#        average_traits$Size_group [which(average_traits$Size_group == "smallg")] <- 3
#        average_traits$Size_group [which(average_traits$Size_group == "medg")] <- 4
#        average_traits$Size_group [which(average_traits$Size_group == "largeg")] <- 5
#        average_traits$Size_group <- ordered (average_traits$Size_group) # ordered
#        
#        # level water
#        average_traits$Level_water [which(average_traits$Level_water == "bottom")] <- 1
#        average_traits$Level_water [which(average_traits$Level_water == "low")] <- 2
#        average_traits$Level_water [which(average_traits$Level_water == "high")] <- 3
#        average_traits$Level_water <- ordered (average_traits$Level_water) # ordered
#        
#        # daily activity
#        average_traits$Diel_activity <- ifelse (average_traits$Diel_activity %in% c("night", "both"),
#                                                1,0)
#        average_traits<- data.frame(average_traits)
#        rownames (average_traits) <- average_traits$Genus_match
#        average_traits<- average_traits[,-1]
#        
#        # taxonomic composition per region (create here)
#        year_composition_region <- cast (fisheries_wtrait[which(fisheries_wtrait$Region == i),], 
#                
#                formula = Year ~ Genus_match, # Genus
#                
#                value = "CatchAmount_t",
#                fun.aggregate = sum
#          
#        )
#        
#        
#        # functional composition  
#        # organize data
#        organized_data <- organize.syncsa(
#          
#          decostand(year_composition_region,'hell'),
#          average_traits[,-which(colnames(average_traits) == "Diel_activity")])
#        
#        # functional composition
#        funct_comp <- matrix.t(organized_data$community, 
#                               organized_data$traits, 
#                               scale = TRUE, 
#                               ranks = TRUE, 
#                               notification = TRUE)
#        
#        # composition
#        # vegan:: beta diversity between year
#        #  hellinger transformed dataset
#        require(vegan)
#        dist_funct_composition <- vegdist (funct_comp$matrix.T,
#                                           method = "euclidean",na.rm=T)
#        
#        
#        
#        # pcoa
#        pcoa_fish_year_func <- pcoa(dist_funct_composition)
#        pcoa_fish_year_func
#        
#})
#
#
#
## composition per region
#comp_change_func <- bind_rows(pcoa_fish_year_func %>% 
#                           bind_cols(region = "Brazil", Year = unique(fisheries_wtrait$Year)),
#                           func_composition_region[[1]]$vectors %>% 
#                           bind_cols(region = "Norte", Year = unique(fisheries_wtrait$Year)),
#                           func_composition_region[[2]]$vectors %>% 
#                           bind_cols(region = "NE", Year = unique(fisheries_wtrait$Year)),
#                           func_composition_region[[3]]$vectors %>% 
#                           bind_cols(region = "SE", Year = unique(fisheries_wtrait$Year)),
#                           func_composition_region[[4]]$vectors %>% 
#                           bind_cols(region = "Sul", Year = unique(fisheries_wtrait$Year))) %>% 
#  data.frame() %>% dplyr::rename(change = Axis.1) %>% 
#  ggplot(aes(x = Year, y = change)) +
#  geom_point() +
#  facet_wrap(~ region, ncol = 5) +
#  theme_classic() +
#  geom_smooth() +
#  labs(x = "", y = "Change in catch\n functional composition (Beta functional)")+
#  theme (legend.position = c(0.12,0.90),
#         axis.title = element_text(size=15),
#         strip.text.x = element_text(size = 14, color = "black", 
#                                     face = "bold"),
#         strip.background = element_rect(color="black", 
#                                         fill="gray60",
#                                         size=1.5, linetype="solid"
#         )) 
#
#
## arrange plots
#pdf (here ("output", "fisheries_functional_composition.pdf"),height=6,width=7)
#compostion1_func<-grid.arrange(ordination1_func,
#                          comp_change_func,
#                          ncol=5,nrow=6,
#                          layout_matrix = rbind (c (1,1,1,1,1),
#                                                 c (1,1,1,1,1),
#                                                 c (1,1,1,1,1),
#                                                 c (1,1,1,1,1),
#                                                 c (2,2,2,2,2),
#                                                 c (2,2,2,2,2)))
#
#
#dev.off()



# ============================

# average trait per genus

size_genus <- tapply (traits$Body_size,
                      list (traits$Genus),
                      mean,na.rm=T)


# depth
depth_genus_max <- tapply (traits$Depth_max,
                       list (traits$Genus),
                       mean,na.rm=T)

# depth
depth_genus_min <- tapply (traits$Depth_min,
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
  
  # max depth of the most frequent nsp
  depth_collect_species_max <- names (fish_year_genus[order(fish_year_genus,decreasing=T)][1:nsp_choose])
  depth_collect_species_max <- depth_genus_max [which(names (depth_genus_max ) %in% depth_collect_species_max)]
  max_depth_collect_species<- mean(depth_collect_species_max,na.rm=T)
  
  # min depth of the most frequent nsp
  depth_collect_species_min <- names (fish_year_genus[order(fish_year_genus,decreasing=T)][1:nsp_choose])
  depth_collect_species_min <- depth_genus_min[which(names (depth_genus_min) %in% depth_collect_species_min)]
  min_depth_collect_species<- mean(depth_collect_species_min,na.rm=T)
  
  # TL
  TL_collect_species <- names (fish_year_genus[order(fish_year_genus,decreasing=T)][1:nsp_choose])
  TL_collect_species <- TL_genus[which(names (TL_genus) %in% TL_collect_species)]
  mean_TL_collect_species<- mean(TL_collect_species,na.rm=T)
  
  
  # bind 
  averages_size_depth_TL <- data.frame (size= mean_size_collect_species,
                                     depth.max = max_depth_collect_species,
                                     depth.min = min_depth_collect_species,
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
levels(fish_year_df$variable)[which(levels(fish_year_df$variable) == "size.depth.max")] <- "Max Depth" 
levels(fish_year_df$variable)[which(levels(fish_year_df$variable) == "size.depth.min")] <- "Min Depth" 
levels(fish_year_df$variable)[which(levels(fish_year_df$variable) == "size.TL")] <- "TL" 


# 
plot_size_depth<-ggplot (fish_year_df, 
                         aes (x=as.numeric(year), 
                              y=value
                              )) + 
  
  geom_smooth(method = "gam",
              formula = y ~ s(x, bs = "cs",k=4),col ="#5BB318") + 
  facet_wrap (~variable,ncol=2,scales = "free")+
  
  geom_point() + 
  
  xlab ("Year") + ylab ("Value") +
  theme (legend.position = c(0.12,0.90),
         axis.title = element_text(size=15),
         strip.text.x = element_text(size = 14, color = "black", 
                                     face = "bold"),
         strip.background = element_rect(color="black", 
                                         fill="gray60",
                                         size=1.5, linetype="solid"
         )) 
  
  

# save plot

pdf (here ("output", "size_depth_catch_fish.pdf"),height=6,width=5)
plot_size_depth
dev.off()




# ------------------------------------------------------------




# THE TRAIT SPACE

# the regional trait space
average_traits_whole_region <- traits %>% 
  
  group_by(Genus) %>% 
  
  summarise (Body_size = mean (Body_size,na.rm=T),  
             Trophic_level = mean (Trophic_level,na.rm=T), 
             Depth_max = mean (Depth_max,na.rm=T), 
             Diel_activity = getmode (Diel_activity), 
             Size_group = getmode (Size_group), 
             Level_water = getmode (Level_water))  %>%
  
  
  select (Genus, Body_size, Trophic_level, Depth_max, 
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

# create directory to receive the figures
dir.create(here ("output", "figs_animation"))

# project year trait space and save

plots_year <- lapply (seq (min (fisheries_wtrait$Year), max(fisheries_wtrait$Year)), function (t) {
    
    # define year
    year <- t
    
    # choose the dataset of one specific year
    chosen_set <- fisheries_wtrait[which(fisheries_wtrait$Year == year),]
    
    # filter genus
    #catched_fish <-cbind(all, ext1=ifelse(all$sp %in% 
    #                                      chosen_set$Genus_match,T,F))
    #catched_fish <-catched_fish[which(catched_fish$ext1==T),]
    #catched_fish_set <- catched_fish [chull(catched_fish, y = NULL),]
    
    # points proportional to the catch
    catch_year_genus <- chosen_set %>%
      
      group_by(Genus_match,Year) %>% 
      
      summarize(sum_catch=sum(CatchAmount_t,na.rm=T),
      ) 
    
    
    # 5% threshold to consider bycatch
    bycatch_year <-sum(catch_year_genus$sum_catch,na.rm=T)*percentage_for_bycatch
    catch_year_genus<- catch_year_genus[which(catch_year_genus$sum_catch >bycatch_year),]
    
    
    # size
    all_year <- data.frame (all , 
                      catch_year_genus [match (all$sp, 
                                               catch_year_genus$Genus_match), 
                                        "sum_catch"])
    
    # rm NAs
    all_year<-all_year[is.na(all_year$sum_catch) != T,]
    
    
    #year convex hull
    a_year <- all_year [chull(all_year[,1:2], y = NULL),] # its convex hull
    
    
    
    ## plot A (complete space)
    plotA <- ggplot(a, aes(Axis.1, Axis.2)) + 
      geom_point(size=3,shape=3 ) + theme_bw()+
    
      # polygon
      
      geom_polygon(data=a, aes (Axis.1,Axis.2),
                   alpha=0.6,
                   fill="gray",
                   colour = "black",
                   size=1,
                   linetype = 2)+ 
      theme_bw() +
      ylim (c(-0.3,0.3))+
      xlim (c(-0.4,0.5))+
      xlab(paste ("Axis I:", round(Inertia.first*100,2),"%"))+
      ylab(paste ("Axis II:", round(Inertia.scnd*100,2),"%")) + 
      geom_label(data = a, aes (x=Axis.1, y=Axis.2, label=(sp)),
                      size=3,
                 nudge_x = 0.02,
                 nudge_y = 0.02)+ 
      theme (legend.position = "none")
    
    plotA
    
    # complete space
    plotA2 <- plotA + geom_polygon(data=a_year, aes (Axis.1,Axis.2),
                   alpha=0.3,
                   fill="#5BB318",
                   colour = "#2B7A0B",
                   size=1,
                   linetype = 3) +
      # point size
      geom_point(data = all_year [is.na (all_year$sum_catch) != T,], 
                 aes (size=(sum_catch),alpha=0.5)) +
      ggtitle (t)
     
    
    plotA2
    
    # genus with large amount of catch
    most_catched <- all_year[order(all_year$sum_catch, decreasing=T),]
    
    # plot the five most catched
    nsp_to_plot <- 10
    plotA2 <- plotA2 + geom_text_repel(data = most_catched[1:nsp_to_plot,], 
                            aes (x=Axis.1, y=Axis.2, label=(sp)),
                            size=3,
                            nudge_x = 0.02,
                            nudge_y = 0.02) 
    
    ## correlations to project trait values into the ordination
    correlations <- cor (data.matrix(data.frame (average_traits_whole_region[,-which(colnames(average_traits_whole_region) %in% c("Diel_activity", 
                                                                                                                                  "Size_group", 
                                                                                                                                  "Level_water"))],
                                    pcoa_whole$vectors[,1:3])),
                         use = "complete.obs")
    correlations<-correlations [-which(rownames(correlations) %in% c("Axis.1","Axis.2","Axis.3")),
                                       c("Axis.1","Axis.2")]# interesting correlations
    
    # show traits in the trait space
    
    
    plotA2 <- plotA2 + geom_segment(aes(x = 0, y = 0, 
                             xend = correlations[1,1]*0.2, 
                             yend = correlations[1,2]*0.2),size = 1,
                         color="#2B7A0B",alpha=0.08,
                         arrow = arrow(length = unit(.35, "cm")))  + 
      ## annotate
      annotate(geom="text",x=correlations[1,1]*0.22,
               y=correlations[1,2]*0.30,label="Total length",
               color="#2B7A0B",alpha=0.5) +
      
      geom_segment(aes(x = 0, y = 0, 
                       xend = correlations[2,1]*0.2, 
                       yend = correlations[2,2]*0.2),size = 1,
                   color="#2B7A0B",alpha=0.08,
                   arrow = arrow(length = unit(.35, "cm"))) + 
      annotate(geom="text",x=correlations[2,1]*0.27,
               y=correlations[2,2]*0.22,label="Trophic level",
               color="#2B7A0B",alpha=0.5) +
      
      geom_segment(aes(x = 0, y = 0, 
                       xend = correlations[3,1]*0.2, 
                       yend = correlations[3,2]*0.2),size = 1,
                   color="#2B7A0B",alpha=0.08,
                   arrow = arrow(length = unit(.35, "cm"))) + 
      annotate(geom="text",x=correlations[3,1]*0.37,
               y=correlations[3,2]*0.2,label="Max depth",
               color="#2B7A0B",alpha=0.5) +
      
      geom_segment(aes(x = 0, y = 0, 
                       xend = correlations[4,1]*0.2, 
                       yend = correlations[4,2]*0.2),size = 1,
                   color="#2B7A0B",alpha=0.1,
                   arrow = arrow(length = unit(.35, "cm"))) + 
      annotate(geom="text",x=correlations[4,1]*0.25,
               y=correlations[4,2]*0.23,label="Level water : Group size",
               color="#2B7A0B",alpha=0.5)  
      
     
    plotA2
    ggsave (filename = here ("output", "figs_animation", paste ("fig", t, ".png"))) 
    
})


#anime

require(magick)
list_img <- list.files(path = here ("output", "figs_animation"), full.names = T)

##https://cran.r-project.org/web/packages/magick/vignettes/intro.html
a_image<-image_read(list_img)
animation <-  image_animate(a_image, fps = 1)
image_write(animation, here ("output","animation_BR_fisheries.gif"))




# trait space per region
# catched_fish_set per region
catched_fish_region <- lapply (year_region_composition_filtered, function (i){
  
  # identify catched spp
  region_bycatch <- rowSums(i[,-1])*percentage_for_bycatch
  
  # filter
  region_amount <- lapply (seq (1,nrow (i)), function (k)
    
    unlist(ifelse (i[k,-1] < region_bycatch[k], 0,
                   i[k,-1]))
    
  )
  region_amount <- do.call(rbind, region_amount)
  rownames(region_amount) <- year_composition$Year
  colnames(region_amount) <- colnames(i)[-1]
  
  
  # remove species without catches
  region_amount <- region_amount[,which(colSums(region_amount)>0)]
  
  # filter year
  #amount_year <- region_amount[which(rownames(region_amount) == year),]
  #amount_year <- amount_year[which(amount_year >0)] # larger than zero
  year_composition_set <- colnames(i)[which(colnames(i) %in% colnames(region_amount))]
  catched_fish <-cbind(all, ext1=ifelse(all$sp %in% 
                                          year_composition_set,T,F))
  catched_fish <-catched_fish[which(catched_fish$ext1==T),]
  catched_fish_set <- catched_fish [chull(catched_fish, y = NULL),]
  res <- list (notbycatch = year_composition_set,
                catched_fish = catched_fish_set)
  res
})
names(catched_fish_region) <- unique(fisheries_wtrait$Region)




# region spaces
plotB <- lapply (catched_fish_region, function (i)
  
  ggplot(a, aes(Axis.1,Axis.2)) + 
    geom_point(size=2,shape=3) + theme_bw()+
    geom_polygon(data=a, aes (Axis.1,Axis.2),
                 alpha=0.6,
                 fill="gray",
                 colour = "black",
                 size=1,
                 linetype = 2) + # complete space
    geom_polygon(data=i$catched_fish, aes (Axis.1,Axis.2),
                 alpha=0.3,
                 fill="#5BB318",
                 colour = "#2B7A0B",
                 size=1,
                 linetype = 3) 
  
)


# the overall trait space
# points proportional to the catch
catch_year_genus <- fisheries_wtrait %>%
  
  group_by(Genus_match) %>% 
  
  summarize(sum_catch=sum(CatchAmount_t,na.rm=T),
  ) 

#bycatch_year <-sum(catch_year_genus$sum_catch,na.rm=T)*percentage_for_bycatch
#catch_year_genus<- catch_year_genus[which(catch_year_genus$sum_catch >bycatch_year),]

# size
all_overall <- data.frame (all , 
                        catch_year_genus [match (all$sp, 
                                                 catch_year_genus$Genus_match), 
                                          "sum_catch"])
# Catched that is not bycatch
notbycatch<-unique(unlist(sapply (catched_fish_region,"[[","notbycatch")))
all_overall <- all_overall [which(rownames(all_overall) %in% notbycatch),]

# rm NAs
all_overall<-all_overall[is.na(all_overall$sum_catch) != T,]

#year convex hull
a_overall <- all_overall [chull(all_overall[,1:2], y = NULL),] # its convex hull

# complete space
plotA <- ggplot(a, aes(Axis.1,Axis.2)) + 
  geom_point(size=2,shape=3) + theme_bw()+
  geom_polygon(data=a, aes (Axis.1,Axis.2),
               alpha=0.6,
               fill="gray",
               colour = "black",
               size=1,
               linetype = 2) +
  geom_text(data = a, aes (x=Axis.1, y=Axis.2, label=(sp)),
            size=3)+ 
  theme (legend.position = "none")

plotA2 <- plotA+ geom_polygon(data=a_overall, aes (Axis.1,Axis.2),
                              alpha=0.3,
                              fill="#5BB318",
                              colour = "#2B7A0B",
                              size=1,
                              linetype = 3) +
  # point size
  geom_point(data = all_overall [is.na (all_overall$sum_catch) != T,], 
             aes (size=(sum_catch),alpha=0.3))+
  theme(legend.position = "none")

# genus with large amount of catch
most_catched <- all_overall[order(all_overall$sum_catch, decreasing=T),]

# plot the five most catched
nsp_to_plot <- 100
plotA2 <- plotA2 + geom_text_repel(data = most_catched[1:nsp_to_plot,], 
                                   aes (x=Axis.1, y=Axis.2, label=(sp)),
                                   size=4) 

# save
pdf (here ("output", "trait_space_fishing.pdf"),height=9,width=6)

grid.arrange(plotA2,
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


# -----------------------------


# variation in diet over time


TL_fisheries <- tapply (fisheries_wtrait$CatchAmount_t,
                        list(fisheries_wtrait$Year,
                             fisheries_wtrait$Diet,
                             fisheries_wtrait$Region),
                        sum, default = 0) 

TL_fisheries<-melt (TL_fisheries)
colnames(TL_fisheries) <- c("Year", "TrGroup", "Region", "value")
# change region names

TL_fisheries$Region <- recode_factor(TL_fisheries$Region, 
                                     "Sul" = "South", 
                                     "SE" = "Southeast",
                                     "NE" = "Northeast", 
                                     "Norte" = "North")

# order diet according to TL
TL_fisheries$TrGroup <- factor(TL_fisheries$TrGroup,
                               levels = c("fc", "im", "is", "om", "hm", "hd"))

## pot trnds over time
trophic_level_trends <- ggplot (TL_fisheries, aes (x=Year, y=log(value+1),
                                                   colour = TrGroup)) + 
  facet_wrap(~Region,scales = "free")+
  geom_line(size=1) + 
  xlab ("Year") + 
  ylab ("Sum catch (thousands of tonnes, log(x+1))") + 
  scale_colour_viridis_d(option = "viridis") + 
  theme (strip.text = element_text(face="bold"),
         strip.text.x = element_text(size = 10, color = "black", 
                                     face = "bold"),
         strip.background = element_rect(color="black", 
                                         fill="gray60",
                                         size=1.5, linetype="solid"
         )) 

pdf (here ("output", "TL_trends.pdf"),height=6,width=6)
trophic_level_trends
dev.off()


## ======================================


# trends for selected genera



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
pdf (here ("output", "sel_spp_trends.pdf"),height=13,width=7)

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













# ------------------------------------------


# (not in this paper)







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
#protein_genus <- tapply (fisheries_wtrait$Protein_mu,
#                      list (fisheries_wtrait$Genus_match,
#                            fisheries_wtrait$Region),
#                      mean,na.rm=T)

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
nutrient_data <- list (zinc_genus,iron_genus,omega_genus,
                       #protein_genus,
                       calcium_genus,selenium_genus , vitA_genus)

# removing bycatch
nutrient_data <- lapply (nutrient_data, function (i)

  i [which(rownames(i) %in% notbycatch),]
)

# match with catch amount data
#nutrient_data <- lapply (nutrient_data, function (i)
#  
#  i <- cbind (i, amount = catch_year_genus [match (rownames(i), 
#                           catch_year_genus$Genus_match), 
#                    "sum_catch"])
#  
#)

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
  rownames (i)<- c("Zinc","Iron", "Omega-3", 
                   #"Protein",
                   "Calcium","Selenium","Vitamin-A")
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
  scale_colour_viridis_d(name = "Region", begin=0,end=1)
  


plot_nut



# ordination to show the nutrition content of fish genus
genus_nutrient_composition <- lapply (nutrient_data, function (i) 
  
          apply (i,1, mean,na.rm=T)
          
)
genus_nutrient_composition <- do.call(cbind,genus_nutrient_composition)
colnames(genus_nutrient_composition) <- c("Zinc","Iron", "Omega-3",
                                          #"Protein",
                                          "Calcium","Selenium","Vitamin-A")



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
  theme(legend.position = "none") + 
  xlab (paste ("Axis 1 (", round(Exp_axis1,2), "%)",sep="")) +
  ylab (paste ("Axis 2 (", round(Exp_axis2,2), "%)",sep=""))+
  xlim (c(-4.75,3)) + 
  ylim (c(-2.5,2)) 




# project genus names
ordination_nut<-ordination_nut + 
  
                    geom_text(data = correlation_nut,
                                           aes (x=Axis.1*5,
                                                y = Axis.2*5,
                                                label = (nutrient)),
                                           size=5,fontface = "italic",
                                           colour="#5BB318",
                                           max.overlaps = 100)  
# arrows
ordination_nut <- ordination_nut + geom_segment(aes(x = 0, y = 0, 
                                  xend = correlation_nut[1,1]*4, 
                                  yend = correlation_nut[1,2]*4),size = 1,
                              color="#2B7A0B",alpha=0.05,
                              arrow = arrow(length = unit(.35, "cm"))) + 
   geom_segment(aes(x = 0, y = 0, 
                     xend = correlation_nut[2,1]*4, 
                     yend = correlation_nut[2,2]*4),size = 1,
                 color="#2B7A0B",alpha=0.05,
                 arrow = arrow(length = unit(.35, "cm"))) + 
  geom_segment(aes(x = 0, y = 0, 
                   xend = correlation_nut[3,1]*4, 
                   yend = correlation_nut[3,2]*4),size = 1,
               color="#2B7A0B",alpha=0.05,
               arrow = arrow(length = unit(.35, "cm"))) + 
  geom_segment(aes(x = 0, y = 0, 
                   xend = correlation_nut[4,1]*4, 
                   yend = correlation_nut[4,2]*4),size = 1,
               color="#2B7A0B",alpha=0.05,
               arrow = arrow(length = unit(.35, "cm"))) + 
  geom_segment(aes(x = 0, y = 0, 
                   xend = correlation_nut[5,2]*4, 
                   yend = correlation_nut[5,2]*4),size = 1,
               color="#2B7A0B",alpha=0.05,
               arrow = arrow(length = unit(.35, "cm"))) + 
  geom_segment(aes(x = 0, y = 0, 
                   xend = correlation_nut[6,1]*4, 
                   yend = correlation_nut[6,2]*4),size = 1,
               color="#2B7A0B",alpha=0.05,
               arrow = arrow(length = unit(.35, "cm")))



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





