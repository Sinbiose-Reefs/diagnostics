
# -------------------------------------------------------------

# Diagnostics of reef fisheries in Brazil
# Linda Eggertsen et al.


# load functions and packages
source ("R/functions.R")
source ("R/packages.R")

# create directory to host the results
dir.create("output")

# load fisheries data (Freire et al. 2021)
fisheries <- read.xlsx (here ("data", "FINAL_RECONSTRUCTED_Brazil_1950_2015_CommercialEtapaII_04072021_IP_Freire.xlsx"),
                        sheet = 2)

# adjust name
fisheries$Sector [which(fisheries$Sector == "industrial (LS, C)")] <- "Industrial (LS, C)"



# load data of Pinheiro et al. 2018 (BR reef fish)
reef_fish <- read.csv (here ("data","brazilian-reef-fish-table-04-mar-18-website.xlsx - Database.csv"))
reef_fish<-reef_fish[which(reef_fish$Relation == "RES"),] # REef fish  (RESident fish according to Pinheiro et al. 2018)



# mistake on the database
reef_fish$Genus [grep ("Ocyurus chrysurus", reef_fish$Species)] <- "Ocyurus"


# trait data (Quimbayo et al. trait database)

traits <- read.csv (here ("data", 
                          "Atributos_especies_Atlantico_&_Pacifico_Oriental_2020_04_28.csv"),
                    sep = ";")


# genus first letter to up
traits$Genus <- firstup(traits$Genus)  


# ajusts in trait values  
traits$Body_size <- as.numeric(gsub (",",".",traits$Body_size))
traits$Trophic_level <- as.numeric(gsub (",",".",traits$Trophic_level))
traits$Depth_range <- as.numeric(gsub (",",".",traits$Depth_range))
traits$Depth_max<-as.numeric(gsub (",",".",traits$Depth_max))
traits$Depth_min<-as.numeric(gsub (",",".",traits$Depth_min))

# average depth
traits$Depth_mean<- apply (cbind (traits$Depth_max,  
                                  traits$Depth_min),1,mean,na.rm=T)  # row-wise average




# aggregate data of QUimbayo et al. at the genus level
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
  
  
  dplyr::select (Genus, Body_size, Trophic_level, 
          Depth_min,Depth_max,Depth_range,
          Depth_mean, Diet,#Diel_activity,
           Size_group, Level_water)

# load shark traits (compiled from fishbase)
shark_traits <- read.xlsx(here ("data", "traits_shark.xlsx"))
shark_traits <- shark_traits %>% 
  
  group_by(Genus) %>% 
  
  summarise (Body_size = mean (Body_size,na.rm=T),  
             Trophic_level = mean (Trophic_level,na.rm=T), 
             Depth_range = mean (Depth_range,na.rm=T), 
             Depth_mean = mean (Depth_mean,na.rm=T),
             Depth_max = mean (Depth_max,na.rm=T), 
             Depth_min = mean (Depth_min,na.rm=T),
             Size_group = getmode (Size_group), 
             Level_water = getmode (Level_water),
             Diet =  getmode (Diet))  %>%
  
  
  dplyr::select (Genus, Body_size, Trophic_level, 
          Depth_min,Depth_max,Depth_range,
          Depth_mean, Diet,
          Size_group, Level_water)


# bind 

traits <- rbind (traits,
                 shark_traits)


# fisheries ~year
# genus
genus <- strsplit(fisheries$TaxonName," ")
genus <- do.call(rbind, genus)
fisheries$Genus_match <- genus[,1]




# fisheries data (matching with Pinheiro et al. 2018)
# reef fish genus
table(unique(fisheries$Genus_match) %in% reef_fish$Genus )
fisheries_wtrait<-fisheries[which(fisheries$Genus_match %in% reef_fish$Genus),]



# match trait & fisheries
fisheries_wtrait <- cbind (fisheries_wtrait,
                           
                           traits [match (fisheries_wtrait$Genus,traits$Genus ),]
                           
                           )


# recode region
fisheries_wtrait$Region <- recode_factor(fisheries_wtrait$Region,    
                                         "Norte" = "North",
                                         "NE" = "Northeastern", 
                                         "Sul" = "South",
                                         "SE" = "Southeast")


# check some examples

traits [grep("Haemulon", traits$Genus),]
traits [grep("Kyphosus", traits$Genus),]
fisheries_wtrait [grep("Haemulon", fisheries_wtrait$Genus_match),]
fisheries_wtrait [grep("Kyphosus", fisheries_wtrait$Genus_match),]
traits [grep("Carcha", traits$Genus),]

# omnivore spp
unique(fisheries_wtrait [grep("om", fisheries_wtrait$Diet), "Genus"])



# equal column names (remove one)
#table(fisheries_wtrait[,25] == fisheries_wtrait[,117])
#fisheries_wtrait<- fisheries_wtrait[,-117]


unique(fisheries_wtrait [which (fisheries_wtrait$Year %in% seq (1970,1973) & 
                                   fisheries_wtrait$Region == "South"),"TaxonName"])


# remove taxa likely with taxonomic misidentification
unique(fisheries_wtrait$TaxonName)[order(unique(fisheries_wtrait$TaxonName))]

fisheries_wtrait <- fisheries_wtrait %>% 
  
  filter(TaxonName %in% c("Myrichthys breviceps" , "Pomacanthus paru") == F)



# barplot for policy brief




# summed catches
sum_catches<- fisheries_wtrait %>%
  
  filter (Year > 2000) %>%
  
  group_by(Sector,Region) %>% 
  
  summarize(sum_catch= (mean(CatchAmount_t))) %>%
  
  group_by  (Region) %>%
  
  summarize(mean_sum_catch= (sum(sum_catch)))
  
  

fisheries_wtrait %>%
  
  filter (Year > 2000) %>%
  
  group_by(Sector,Region) %>% 
  
  summarize(mean_catch= mean(CatchAmount_t)) %>%
  
  right_join (sum_catches, by = "Region") %>%
  
  mutate (prop = mean_catch/mean_sum_catch) %>%
  
  
  ggplot(aes (x=Region,y=mean_catch,fill=Sector,col=Sector))+
  
  geom_bar(position="fill", stat="identity") +
  
  #  coord_polar("y")
  geom_text(aes(x=Region,y =mean_catch/sum(mean_catch),
                label=paste (
                          round (mean_catch,2),
                          round(prop,2),
                          sep = " / ")
                          
                      ),col="black")


ggsave (file =  ("../puttingBRMap/output/contr_fisheries.pdf"))



# contribution of artisanal and industria 

fisheries_wtrait %>%
  
  #filter (Year > 2000) %>%
  
  group_by(Sector) %>% 
  
  summarize(mean_catch=mean(CatchAmount_t)
            
  )
# ---------------------------
# plotting


# catch year
catch_year <- fisheries_wtrait %>%
  
  group_by(Year,Sector,Region) %>% 

  
  summarize(sum_catch=sum(CatchAmount_t)
            
            
  ) 


# plot

catch_year_plot <- ggplot (catch_year, aes (x=Year, 
                         y=sum_catch,
                         colour = Sector)) + 
  facet_wrap(~Region,scales = "fixed")+
  geom_line(linewidth=1.5) + 
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
# species most matched per region


sp_region <- fisheries_wtrait %>%
  
  group_by(Region, TaxonName) %>% 
  
  
  summarize(sum_catch=sum(CatchAmount_t)
            
            
  ) 


# 1% of the year catch
percentage_for_bycatch <-  0.01


# total region
total_region <- fisheries_wtrait %>%
  
  group_by(Region) %>% 
  
  
  summarize(sum_catch=sum(CatchAmount_t)) %>% # summed catched per region
  
  
  mutate (bycatch_region = sum_catch*percentage_for_bycatch) # region bycatch 
              

# match
sp_region$bycatch <- total_region$bycatch_region [match (sp_region$Region, total_region$Region)]


# difference
sp_region <- sp_region %>% 
  mutate (diff = sum_catch >= bycatch) %>%
  filter (diff== T) %>% 
  group_by (Region) 


# total N spp per region
sp_region %>%
  tally()

# composition per region
sp_region$TaxonName [which (sp_region$Region == "North")]
sp_region$TaxonName [which (sp_region$Region == "Northeastern")]
sp_region$TaxonName [which (sp_region$Region == "Southeast")]
sp_region$TaxonName [which (sp_region$Region == "South")]



# ========================
# how fisheries changed over time in terms of spp composition?
# filter reef fish



# table of composition per year

year_composition <- cast (fisheries_wtrait, 
                          
                          formula = Year ~ Genus_match, 
      
                        value = "CatchAmount_t",
                     
                           fun.aggregate =  sum,
                        
                        drop = F
        )



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
dist_composition <- vegdist (decostand(year_composition_filtered,'hell'),
                                method = "bray",
                             na.rm=T)


# pcoa
pcoa_fish_year <- pcoa(dist_composition)
# variance explained by the first and second axes
(Exp_axis1<-pcoa_fish_year$values$Eigenvalues[1]/sum(pcoa_fish_year$values$Eigenvalues)*100)
(Exp_axis2<-pcoa_fish_year$values$Eigenvalues[2]/sum(pcoa_fish_year$values$Eigenvalues)*100)
Exp_axis1+Exp_axis2


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

# adjust year to plot
pcoa_fish_year$year_plot<-ifelse (pcoa_fish_year$year %in% seq(1950,2020,5),
                             pcoa_fish_year$year,
                                         "")


# ordination (projection of beta diversity )
# help here : https://ggplot2.tidyverse.org/reference/geom_path.html
ordination1<-ggplot(data=pcoa_fish_year,
       aes(x=Axis.1,y=Axis.2)) + 
  geom_point(aes(colour=catch,
                 #fill=catch,
                 size = catch),
             
             shape=19) +
  
  scale_colour_viridis_c(option = "magma",
                         direction=-1)+
  
   geom_path(colour = "turquoise4",
             alpha=0.5,
             linewidth=0.75)+ # aes(colour=as.numeric(year))
  
  
  geom_text(aes(label=year_plot),
                size=2.5,vjust=-1) +
  #geom_path(aess(group=year)) +# add the site labels
  #scale_colour_manual(values=c("A" = "red", "B" = "blue")) +
  coord_equal() +
  theme_bw() +
  
  theme(legend.position = "right")

ordination1

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
ordination1 <- ordination1 + geom_text_repel(data = correlation[order(correlation[,2],decreasing=T),][1:10,],
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
# change in composition per region over time



year_composition_region <- lapply (unique(fisheries_wtrait$Region), function (i)
  
  cast (fisheries_wtrait[which(fisheries_wtrait$Region == i),], 
        
        formula = Year ~ Genus_match, # Genus
        
        value = "CatchAmount_t",
        
        fun.aggregate = sum,
        
        drop= F
  )
)


# naming
names(year_composition_region) <- unique(fisheries_wtrait$Region)



# year catch
bycatch_region <- lapply (year_composition_region, function (i) 
  
        rowSums( i [,-1])*percentage_for_bycatch
        
)


# filter
year_region_composition_filtered <- lapply (seq (1,length (year_composition_region)), function (i)  # each region
  
        lapply (seq (1,nrow (year_composition_region[[i]])), function (k) # each year
  
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
  ; # return
  year_composition_region[[i]]
})


# remove genus without catches
year_region_composition_filtered <- lapply (year_region_composition_filtered, function (i) 
  
    i[,which(colSums(i)>0)]
    
)


names(year_region_composition_filtered) <- unique(fisheries_wtrait$Region)



# lapply (year_region_composition_filtered,dim)

#  beta diversity
# bycatch not removed 
# "year_region_composition_filtered" is the object in which I removed bycatch 

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
  
  ; # return
  
  pcoa_fish_year

  })


# composition per region
comp_change <- bind_rows(pcoa_fish_year %>% 
                           bind_cols(region = "Brazil", Year = unique(fisheries_wtrait$Year)),
                          dist_composition_pcoa[[1]] %>% 
                           bind_cols(region = "North", Year = unique(fisheries_wtrait$Year)),
                         dist_composition_pcoa[[2]] %>% 
                           bind_cols(region = "Northeast", Year = unique(fisheries_wtrait$Year)),
                         dist_composition_pcoa[[3]] %>% 
                           mutate_at (vars (Axis.1), funs (.*-1)) %>%
                           bind_cols(region = "Southeast", Year = unique(fisheries_wtrait$Year)),
                         dist_composition_pcoa[[4]] %>% 
                           bind_cols(region = "South", Year = unique(fisheries_wtrait$Year))) %>% 
  data.frame() %>% dplyr::rename(change = Axis.1) %>% 
  ggplot(aes(x = Year, y = change)) +
  geom_point(colour="turquoise4") +
  facet_wrap(~ region, ncol = 5) +
  theme_classic() +
  geom_smooth(col = "turquoise3") +
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
# help here : https://ggplot2.tidyverse.org/reference/geom_path.html

# change the direction of southeast ordination
dist_composition_pcoa[[3]]  <- dist_composition_pcoa[[3]] %>% 
  mutate_at (vars (Axis.1), funs (.*-1))

# one plot per region 
ordination1_reg <- lapply (seq(1,length(dist_composition_pcoa)), function (i) {
        
  
        # adjust the year to plot 
        dist_composition_pcoa[[i]]$year<-ifelse (dist_composition_pcoa[[i]]$year %in% seq(1950,2020,5),
                dist_composition_pcoa[[i]]$year,
                "")
  
        ordination1_reg <-ggplot(data=dist_composition_pcoa[[i]],
                            aes(x=Axis.1,y=Axis.2)) + 
          geom_point(aes(colour=catch_year_region[i,],
                         size = catch_year_region[i,]),
                     shape=19)+
                     scale_colour_viridis_c(option = "magma",direction=-1)+
                     
                     
          geom_path(aes(colour=as.numeric(year)),alpha=0.5)+
          
          geom_text(aes(label=year),
                    size=2.5,vjust=-1) +
          ggtitle (names(year_composition_region)[[i]])+
          coord_equal() +
          theme_bw() +
          
          
          theme(legend.position = "none")
        
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
                                                   size=2,fontface = "italic",
                                                   colour="#1363DF",
                                                   max.overlaps = 100) +
          geom_text_repel(data = correlation[order(correlation[,1],decreasing=F),][1:5,],
                          aes (x=Axis.1*0.2,
                               y = Axis.2*0.2,
                               label = (species)),
                          size=2,fontface = "italic",
                          colour = "#06283D",
                          max.overlaps = 100) + 
          geom_text_repel(data = correlation[order(correlation[,1],decreasing=T),][1:5,],
                          aes (x=Axis.1*0.22,
                               y = Axis.2*0.22,
                               label = (species)),
                          size=2,fontface = "italic",
                          colour = "#47B5FF",
                          max.overlaps = 100) +
          xlim(-0.38, 0.38)+
          ylim(-0.38,0.38)
        
          ordination1_reg

})



# arrange plots
pdf (here ("output", "fisheries_composition.pdf"),height=7,width=7)

compostion1<-grid.arrange(
                 ordination1,
                 comp_change,
                 ncol=8,nrow=4,
             layout_matrix = rbind (c (1,1,1,1,1,1,1,1),
                                    c (1,1,1,1,1,1,1,1),
                                    c (2,2,2,2,2,2,2,2),
                                    c (2,2,2,2,2,2,2,2)))


dev.off()

# composition per region

pdf (here ("output", "fisheries_composition_region.pdf"),height=10,width=10)

grid.arrange(
  ordination1_reg[[2]], 
  ordination1_reg[[1]], 
  ordination1_reg[[3]],
  ordination1_reg[[4]],
  ncol=2
)

dev.off()


## save objects to trait-based analyses

save (fisheries_wtrait,year_composition,traits,reef_fish,
      percentage_for_bycatch,year_region_composition_filtered,
      pcoa_fish_year,file =  here ("data", "fisheries_wtrait.R"))

rm(list=ls())

