# Laure Gallien	
# le 12 July 2016

library(gdata)
library(reshape2)


if (Sys.getenv("USER")=="jasper") {setwd("/Users/jasper/Dropbox/Shared/CapeCommunities/Data/Raw/")}
if (Sys.getenv("USER")=="Laure") {setwd("~/Dropbox/GIT/2016_CapeCom/Data/LaurePrep/")}


######################################################################
######################################################################
# Load the data
######################################################################
######################################################################

#------------------------------------------------
# Get plot treatment info
#------------------------------------------------
treat <- read.table("plot_treatments_4Aug16.txt", header=T, stringsAsFactors = F)
head(treat)
dim(treat) # 51  33
str(treat)

#------------------------------------------------
# Get TRAIT data updated by Doug on the 6 Nov 2016
#------------------------------------------------
# t02 <- read.delim("~/Dropbox/GIT/2016_CapeCom/Data/Raw/Quantitative_traits_to_check_and_fill_27Oct16_Updated_byDoug_6Nov17.txt", stringsAsFactors = F)
# head(t02)
# dim(t02) # 1072  12
# str(t02)

#------------------------------------------------
# Get the species x site data : 10 x 10m
#------------------------------------------------
# 2002 / 2008 / 2011 / 2014
#........
p02 <- read.delim("Plot_x_spc_RawSpcID/PlotSpc_2002_RawSpcID_7Sept16.txt", stringsAsFactors = F)
p08 <- read.delim("Plot_x_spc_RawSpcID/PlotSpc_2008_RawSpcID_7Sept16.txt", stringsAsFactors = F)
p11 <- read.delim("Plot_x_spc_RawSpcID/PlotSpc_2011_RawSpcID_7Sept16.txt", stringsAsFactors = F)
p14 <- read.delim("Plot_x_spc_RawSpcID/PlotSpc_2014_RawSpcID_7Sept16.txt", stringsAsFactors = F)

p02[1:10, 1:10]
p08[1:10, 1:10]
p11[1:10, 1:10]
p14[1:10, 1:10]

class(unlist(p02)) ; class(unlist(p08)) ; class(unlist(p11)) ; class(unlist(p14))


#------------------------------------------------
# Get the species x site data : 1 x 1m
#------------------------------------------------
# 2002 / 2008 / 2011 / 2014
#........
sp02_Abun <- read.delim("Plot_x_spc_RawSpcID/subPlotSpc_2002_NbIndiv_RawSpcID_7Sept16.txt", stringsAsFactors = F)
sp02_Cov <- read.delim("Plot_x_spc_RawSpcID/subPlotSpc_2002_PropCover_RawSpcID_7Sept16.txt", stringsAsFactors = F)

sp08_Abun <- read.delim("Plot_x_spc_RawSpcID/subPlotSpc_2008_NbIndiv_RawSpcID_7Sept16.txt", stringsAsFactors = F)
sp08_Cov <- read.delim("Plot_x_spc_RawSpcID/subPlotSpc_2008_PropCover_RawSpcID_7Sept16.txt", stringsAsFactors = F)

sp11_Abun <- read.delim("Plot_x_spc_RawSpcID/subPlotSpc_2011_NbIndiv_RawSpcID_7Sept16.txt", stringsAsFactors = F)
sp11_Cov <- read.delim("Plot_x_spc_RawSpcID/subPlotSpc_2011_PropCover_RawSpcID_7Sept16.txt", stringsAsFactors = F)

sp14_Abun <- read.delim("Plot_x_spc_RawSpcID/subPlotSpc_2014_NbIndiv_RawSpcID_7Sept16.txt", stringsAsFactors = F)
sp14_Cov <- read.delim("Plot_x_spc_RawSpcID/subPlotSpc_2014_PropCover_RawSpcID_7Sept16.txt", stringsAsFactors = F)

sp02_Abun[1:10, 1:10] ; sp02_Cov[1:10, 1:10]
sp08_Abun[1:10, 1:10] ; sp08_Cov[1:10, 1:10]
sp11_Abun[1:10, 1:10] ; sp11_Cov[1:10, 1:10]
sp14_Abun[1:10, 1:10] ; sp14_Cov[1:10, 1:10]

class(unlist(sp02_Abun)) ; class(unlist(sp08_Abun)) ; class(unlist(sp11_Abun)) ; class(unlist(sp14_Abun))


#------------------------------------------------
# Get the species list with correspondance Old vs New names
#------------------------------------------------
# synJ <- read.csv("~/Desktop/Dropbox/GIT/2016_CapeCom/Data/Raw/allnames_old.csv")
# synJ2 <- synJ[,c("Best_source", as.character(unique(synJ$Best_source)))]
# for(i in names(synJ2)) synJ2[,i] <- as.character(synJ2[,i])
# synJ2$Final <- sapply(1:nrow(synJ2), function(x) synJ2[x, synJ2[x, "Best_source"]])
# head(synJ2)
# dim(synJ2) # 2124
# write.table(synJ2[,c("Original", "Final")], file="Synonym_List_29July16.txt", sep="\t", quote=F, row.names=F)

# Then I manually added/corrected species names + integrated Doug's last suggestions + check Doug's suggestions on TNRS
# The new corrected file is "Synonym_List_LaureProposition_UpdateWithDougComments24Jan17.xls"

syn <- read.xls("Synonym_List_LaureProposition_UpdateWithDougComments24Jan17.xls", stringsAsFactors = F)
dim(syn) # 2134  5
dim(unique(syn)) # 2134
head(syn, 20)

# Summarize all modifications
syn$Final2 <- ifelse(is.na(syn$Doug_suggestion), syn$Laure_Proposition, syn$Doug_suggestion)
syn[!is.na(syn$TNRS_resultsOnDougSuggestions), "Final2"] <- syn[!is.na(syn$TNRS_resultsOnDougSuggestions), "TNRS_resultsOnDougSuggestions"]

syn <- syn[, c("Original", "Final2")]
names(syn) <- c("ini", "fin")
syn <- unique(syn)
syn <- rbind(syn, data.frame(ini=syn$fin, fin=syn$fin))
syn <- unique(as.data.frame(syn))
dim(syn) # 2670 2
head(syn, 30)


######################################################################
######################################################################
# Check and change spc names on the TRAIT data
######################################################################
######################################################################
# head(t02)
# dim(t02) # 1072  12
# 
# old <- t02$SpcID
# length(unique(old)) # 1072
# 
# # Simplify species names when only genera are known
# #...........................................
# SPnum <- unlist(sapply(paste("_sp", 1:20, sep=""), function(x) grep(x, old)))
# old[SPnum]
# 
# # replace the spX by sp1
# old[SPnum] <- sapply(strsplit(old[SPnum], "_"), function(x) paste(x[1], "_sp1", sep=""))
# old
# t02$SpcID <- old
# 
# # remove duplicates
# dim(t02) # 1072
# dim(unique(t02)) # 1033
# t02 <- unique(t02)
# row.names(t02) <- 1:nrow(t02)
# t02[which(t02$SpcID %in% t02$SpcID[duplicated(t02$SpcID)]), ]
# t02.1 <- t02[-c(67, 794),]
# dim(t02.1) # 1031
# t02 <- t02.1
# old <- t02$SpcID
# length(unique(old)) # 1031
# 
# # Correct the trait  data with the synonymy list
# #...........................................
# length(old[old %in% syn$ini]) # 1031
# out <- old[!old %in% syn$ini] # 0
# COR <- merge(data.frame(oldID=old), syn, by.x="oldID", by.y="ini")
# head(COR)
# str(COR)
# COR[,1] <- as.character(COR[,1])
# dim(COR) # 1039
# dim(unique(COR)) # 1039
# 
# t.dupli <- COR[,1][duplicated(COR[,1])]
# COR[which(COR[,1] %in% t.dupli),]
# COR2 <- COR
# COR2 <- COR2[-which(row.names(COR2) %in% row.names(COR2[which(COR2[,1] %in% t.dupli),][!COR2[which(COR2[,1] %in% t.dupli),]$fin %in% t.dupli,])),]
# dim(COR2) # 1031
# 
# tr2 <- merge(COR2, t02, by.x = "oldID", by.y="SpcID")
# tr2 <- tr2[, 2:ncol(tr2),]
# names(tr2)[1] <- "SpcID"
# head(tr2)
# dim(t02)  # 1031
# dim(tr2) # 1031
# 
# # some new duplicated names because of the synonymy issue
# tr2 <- unique(tr2)
# dim(tr2) # 1029
# length(unique(tr2$SpcID)) # 1002
# tr.dupli <- tr2[which(tr2$SpcID %in% tr2$SpcID[duplicated(tr2$SpcID)]),]
# 
# tr.dupli$sumNA <- apply(tr.dupli, 1, function(x) sum(is.na(x)))
# head(tr.dupli)
# tr.dupli <- tr.dupli[which(tr.dupli$sumNA<11),]
# tr.dupli <- tr.dupli[order(tr.dupli$SpcID),]
# 
# tr.dupli[which(tr.dupli$SpcID == "Chaenostoma_hispidum"),] <- tr.dupli[which(tr.dupli$SpcID == "Chaenostoma_hispidum" & tr.dupli$Growth_Form=="h"),]
# tr.dupli[which(tr.dupli$SpcID == "Conyza_canadensis"),] <- tr.dupli[which(tr.dupli$SpcID == "Conyza_canadensis" & tr.dupli$fire.recruiters=="n"),]
# tr.dupli[which(tr.dupli$SpcID == "Erica_curviflora"),] <- tr.dupli[which(tr.dupli$SpcID == "Erica_curviflora" & tr.dupli$plant_height=="b"),]
# tr.dupli[which(tr.dupli$SpcID == "Fagelia_bituminosa"),] <- tr.dupli[which(tr.dupli$SpcID == "Fagelia_bituminosa" & tr.dupli$Dispersal=="p"),]
# tr.dupli[which(tr.dupli$SpcID == "Fagelia_bituminosa"),] <- tr.dupli[which(tr.dupli$SpcID == "Fagelia_bituminosa" & tr.dupli$Dispersal=="p"),]
# tr.dupli[which(tr.dupli$SpcID == "Ficinia_capillifolia"),] <- tr.dupli[which(tr.dupli$SpcID == "Ficinia_capillifolia" & tr.dupli$Life_span_=="p"),]
# tr.dupli[which(tr.dupli$SpcID == "Hypochoeris_radicata"),] <- tr.dupli[which(tr.dupli$SpcID == "Hypochoeris_radicata" & tr.dupli$Life_span_=="p"),]
# tr.dupli[which(tr.dupli$SpcID == "Liparia_parva"),] <- tr.dupli[which(tr.dupli$SpcID == "Liparia_parva" & tr.dupli$Life_span_=="p"),]
# tr.dupli[which(tr.dupli$SpcID == "Pelargonium_cucullatum"),] <- tr.dupli[which(tr.dupli$SpcID == "Pelargonium_cucullatum" & tr.dupli$Life_span_=="p"),]
# tr.dupli[which(tr.dupli$SpcID == "Penaea_mucronata"),] <- tr.dupli[which(tr.dupli$SpcID == "Penaea_mucronata" & tr.dupli$Growth_Form=="e"),]
# tr.dupli[which(tr.dupli$SpcID == "Pentameris_barbata"),] <- tr.dupli[which(tr.dupli$SpcID == "Pentameris_barbata" & tr.dupli$Regeneration=="es"),]
# tr.dupli[which(tr.dupli$SpcID == "Phylica_imberbis"),] <- tr.dupli[which(tr.dupli$SpcID == "Phylica_imberbis" & tr.dupli$Growth_Form=="e"),]
# tr.dupli[which(tr.dupli$SpcID == "Pseudoselago_serrata"),] <- tr.dupli[which(tr.dupli$SpcID == "Pseudoselago_serrata" & tr.dupli$Life_span_=="e"),]
# tr.dupli[which(tr.dupli$SpcID == "Searsia_glauca"),] <- tr.dupli[which(tr.dupli$SpcID == "Searsia_glauca" & tr.dupli$fire.recruiters=="n"),]
# tr.dupli[which(tr.dupli$SpcID == "Searsia_laevigata"),] <- tr.dupli[which(tr.dupli$SpcID == "Searsia_laevigata" & tr.dupli$distribution=="re"),]
# tr.dupli[which(tr.dupli$SpcID == "Searsia_lucida"),] <- tr.dupli[which(tr.dupli$SpcID == "Searsia_lucida" & tr.dupli$fire.recruiters=="n"),]
# tr.dupli[which(tr.dupli$SpcID == "Searsia_rosmarinifolia"),] <- tr.dupli[which(tr.dupli$SpcID == "Searsia_rosmarinifolia" & tr.dupli$fire.recruiters=="n"),]
# tr.dupli[which(tr.dupli$SpcID == "Searsia_tomentosa"),] <- tr.dupli[which(tr.dupli$SpcID == "Searsia_tomentosa" & tr.dupli$Seed_size=="b"),]
# tr.dupli[which(tr.dupli$SpcID == "Selago_scabrida"),] <- tr.dupli[which(tr.dupli$SpcID == "Selago_scabrida" & tr.dupli$Growth_Form=="e"),]
# tr.dupli[which(tr.dupli$SpcID == "Syncarpha_speciosissima"),] <- tr.dupli[which(tr.dupli$SpcID == "Syncarpha_speciosissima" & tr.dupli$Life_span_=="a"),]
# tr.dupli[which(tr.dupli$SpcID == "Ursinia_chrysanthemoides"),] <- tr.dupli[which(tr.dupli$SpcID == "Ursinia_chrysanthemoides" & tr.dupli$Dispersal=="w"),]
# tr.dupli[order(tr.dupli$SpcID),1:5]
# tr.dupli <- unique(tr.dupli)
# 
# tr3 <- rbind(tr2[-which(tr2$SpcID %in% tr2$SpcID[duplicated(tr2$SpcID)]),], tr.dupli[,-which(names(tr.dupli)=="sumNA")])
# dim(tr3) # 1002
# dim(unique(tr3)) # 1002
# length(unique(tr3$SpcID)) # 1002
# tr3 <- tr3[order(tr3$SpcID),]
# tail(tr3)
# 
# write.table(tr3, file="All_Trait_Quanti_26Jan17.txt", quote=F, row.names=F, sep="\t")
t02 <- read.delim("All_Trait_Quanti_26Jan17.txt", stringsAsFactors = F)
head(t02)
dim(t02) # 1002  12
str(t02)


######################################################################
######################################################################
# Check and change spc names on the PLOT data
######################################################################
######################################################################


#********************************************************************
# 2002
#********************************************************************
p02[1:10, 1:10]
dim(p02) # 312  828

old <- names(p02)
length(old) # 828
length(unique(old)) # 828

# Check that all species are in the list
length(old[old %in% syn$ini]) # 788
out <- old[!old %in% syn$ini]

out2 <- substring(out, 1, nchar(out)-1)
out2[out2=="Acacia_melanoxylon_.31.2"] <- "Acacia_melanoxylon"
out2[out2=="Aspalathus_.grey_green."] <- "Aspalathus_grey_green"
out2[out2=="Aspalathus_.long_peduncle."] <- "Aspalathus_long_peduncle"
out2[out2=="Aspalathus_.silver."] <- "Aspalathus_silver"
out2[out2=="Restio_perplexus_.28.3."] <- "Restio_perplexus"
out2[out2=="Scirpoides_thunbergii_.5.2"] <- "Scirpoides_thunbergii"
out2[!out2 %in% syn$ini]

out2

# adapt them to the correspondance list
old_corr <- old
old_corr[!old_corr %in% syn$ini] <- out2
length(old_corr[old_corr %in% syn$ini]) # 828

# replace the species names by the correct ones
COR <- cbind(oldID=old, oldCORR=old_corr)
COR <- merge(COR, syn, by.x="oldCORR", by.y="ini")
head(COR)
dim(COR) # 828

COR[!(row.names(COR) %in% row.names(na.omit(COR))), ] # 0

p02n <- p02
TempNam <- names(p02n)
for(i in names(p02n)) { 
  TempNam[TempNam==i] <- COR[which(COR$oldID==i), "fin"] 
  print(paste(i, "-->", COR[which(COR$oldID==i), "fin"] ))
} 
names(p02n) <- TempNam
dim(p02n) # 312  828
p02n[1:10, 1:10]

# resume the duplicates
p02n1 <- as.data.frame(cbind(spcID=names(p02n), t(p02n)), row.names=1:length(p02n))
p02n1[1:10, 1:10]
dim(p02n1) #   828  313

dupli <- unique(as.character(p02n1$spcID[duplicated(p02n1$spcID)]))
for(i in dupli) { 
  t.row <- row.names(p02n1[which(p02n1==i),])
  remp <- as.numeric(apply(p02n1[t.row, 2:ncol(p02n1)], 2, max)) 
  for(j in t.row) p02n1[j, 2:ncol(p02n1)] <- remp ; print(i)
}
p02n1 <- unique(p02n1)
dim(p02n1) #   810  313
p02n1[1:10, 1:10]

row.names(p02n1) <- p02n1$spcID 
p02n2 <- as.data.frame(t(p02n1[,2:ncol(p02n1)]))
class(p02n2)
str(p02n2)
unique(unlist(p02n2)) # 0  1

for(i in 1:ncol(p02n2)) p02n2[,i] <- as.numeric(as.character(p02n2[,i]))

# Final checks
p02n2[1:10, 1:10]
dim(p02n2)  # 312 sites  811 species

range(p02n2)
sort(colSums(p02n2))

# Remove species without presences
p02n3 <- p02n2[,which(colSums(p02n2)>0)]
p02n3[1:10, 1:10]
dim(p02n3)  # 312 sites  799 species

# Save it
write.table(p02n3, file="PlotSpc_2002_2Fev17.txt", sep="\t", quote=F)



#********************************************************************
# 2008
#********************************************************************
p08[1:10, 1:10]
dim(p08) # 263  812

old <- names(p08)
length(old) # 812
length(unique(old)) # 812

# Check that all species are in the list
length(old[old %in% syn$ini]) # 711
out <- old[!old %in% syn$ini]
out <- sapply(strsplit(out, "7."), function(x) ifelse(length(x)>1, paste(x[1], x[2], sep="7/"), x))

out2 <- out[!out %in% syn$ini]
out2 <- substring(out2, 1, nchar(out2)-1)
out2[out2=="Acrosanthes_2007/"] <- "Acrosanthes_2007/179"
out2[out2=="Cassine_maritima._robsonodendro"] <- "Cassine_maritima"
out2[out2=="Erica_cf_imbricata_200"] <- "Erica_imbricata"
out2[out2=="Erica_sp_2007/"] <- "Erica_sp"
out2[out2=="Erica_sp1_2007/"] <- "Erica_sp1"
out2[out2=="Eucalyptus_.blue_gum"] <- "Eucalyptus_blue_gum"
out2[out2=="Gladiolus_spp_20"] <- "Gladiolus_spp"
out2[out2=="Tetraria_sp_2007/"] <- "Tetraria_sp"
out2[out2=="Thesium_sp_2007/"] <- "Thesium_sp"
out2[!out2 %in% syn$ini]

out2

# adapt them to the correspondance list
old_corr <- old
old_corr[!old_corr %in% syn$ini] <- out
old_corr[!old_corr %in% syn$ini] <- out2
length(old_corr[old_corr %in% syn$ini]) # 812

# replace the species names by the correct ones
COR <- cbind(oldID=old, oldCORR=old_corr)
COR <- merge(COR, syn, by.x="oldCORR", by.y="ini")
head(COR)
dim(COR) # 812
dim(unique(COR)) # 812

COR[!(row.names(COR) %in% row.names(na.omit(COR))), ] # 0

p08n <- p08
TempNam <- names(p08n)
for(i in names(p08n)) { 
  TempNam[TempNam==i] <- COR[which(COR$oldID==i), "fin"] 
  print(paste(i, "-->", COR[which(COR$oldID==i), "fin"] ))
} 
names(p08n) <- TempNam
dim(p08n) # 263  812
p08n[1:10, 1:10]

# resume the duplicates
p08n1 <- as.data.frame(cbind(spcID=names(p08n), t(p08n)), row.names=1:length(p08n))
p08n1[1:10, 1:10]
dim(p08n1) #   812  264

dupli <- unique(as.character(p08n1$spcID[duplicated(p08n1$spcID)]))
for(i in dupli) { 
  t.row <- row.names(p08n1[which(p08n1==i),])
  remp <- as.numeric(apply(p08n1[t.row, 2:ncol(p08n1)], 2, max)) 
  for(j in t.row) p08n1[j, 2:ncol(p08n1)] <- remp ; print(i)
}
p08n1 <- unique(p08n1)
dim(p08n1) #   701  264
p08n1[1:10, 1:10]

row.names(p08n1) <- p08n1$spcID 
p08n2 <- as.data.frame(t(p08n1[,2:ncol(p08n1)]))
class(p08n2)
str(p08n2)
unique(unlist(p08n2)) # 0  1

for(i in 1:ncol(p08n2)) p08n2[,i] <- as.numeric(as.character(p08n2[,i]))

# Final checks
p08n2[1:10, 1:10]
dim(p08n2)  # 263 sites  701 species

range(p08n2)
sort(colSums(p08n2))

# Remove species without presences
p08n3 <- p08n2[,which(colSums(p08n2)>0)]
p08n3[1:10, 1:10]
dim(p08n3)  # 263 sites  701 species

# homogenize the rownames
row.names(p08n3) <- gsub(".", "_", row.names(p08n3), fixed=T)

# Save it
write.table(p08n3, file="PlotSpc_2008_2Fev17.txt", sep="\t", quote=F)



#********************************************************************
# 2011
#********************************************************************
p11[1:10, 1:10]
dim(p11) # 62   381

old <- names(p11)
length(old) # 381
length(unique(old)) # 381

# Check that all species are in the list
length(old[old %in% syn$ini]) # 351
out <- old[!old %in% syn$ini]

out2 <- substring(out, 1, nchar(out)-1)
out2 <- gsub(".", "", out2, fixed=T)
out2[out2=="Chrysocoma_"] <- "Chrysocoma_?"
out2[out2=="Cyperus_denudatus__trangular_stemmed_sedge"] <- "Cyperus_denudatus_?_trangular_stemmed_sedge"
out2[out2=="Lampranthus_emarginatus_"] <- "Lampranthus_emarginatus"
out2[out2=="Lechnalia"] <- "Lechnalia?"
out2[out2=="Pillia_sp"] <- "Pillia_sp?"
out2[out2=="Restio_quadratus_"] <- "Restio_quadratus"
out2[out2=="Spoedoselagus"] <- "Spoedoselagus??"
out2[out2=="Syncarpha_sp_spiciosisima"] <- "Syncarpha_sp_spiciosisima?"
out2[out2=="Thamnochortis_lucinsorenariu"] <- "Thamnochortis_lucins/orenarius"
out2[out2=="Tracheandra_tabularus_"] <- "Tracheandra_tabularus_?"
out2[out2=="Trachyandra_sp1_diff_from_above"] <- "Trachyandra_sp1_diff_from_above?"
out2[out2=="Tretraria_sen"] <- "Tretraria_sen...?"
out2[out2=="Wildenovia_lannamois"] <- "Wildenovia_lannamois?"
out2[!out2 %in% syn$ini]

out2

# adapt them to the correspondance list
old_corr <- old
old_corr[!old_corr %in% syn$ini] <- out2
length(old_corr[old_corr %in% syn$ini]) # 381

# replace the species names by the correct ones
COR <- cbind(oldID=old, oldCORR=old_corr)
COR <- merge(COR, syn, by.x="oldCORR", by.y="ini")
head(COR)
dim(COR) # 381

COR[!(row.names(COR) %in% row.names(na.omit(COR))), ] # 0

p11n <- p11
TempNam <- names(p11n)
for(i in names(p11n)) { 
  TempNam[TempNam==i] <- COR[which(COR$oldID==i), "fin"] 
  print(paste(i, "-->", COR[which(COR$oldID==i), "fin"] ))
} 
names(p11n) <- TempNam
dim(p11n) # 62  381
p11n[1:10, 1:10]

# resume the duplicates
p11n1 <- as.data.frame(cbind(spcID=names(p11n), t(p11n)), row.names=1:length(p11n))
p11n1[1:10, 1:10]
dim(p11n1) #   381  63

dupli <- unique(as.character(p11n1$spcID[duplicated(p11n1$spcID)]))
for(i in dupli) { 
  t.row <- row.names(p11n1[which(p11n1==i),])
  remp <- as.numeric(apply(p11n1[t.row, 2:ncol(p11n1)], 2, max)) 
  for(j in t.row) p11n1[j, 2:ncol(p11n1)] <- remp ; print(i)
}
p11n1 <- unique(p11n1)
dim(p11n1) #   291  63
p11n1[1:10, 1:10]

row.names(p11n1) <- p11n1$spcID 
p11n2 <- as.data.frame(t(p11n1[,2:ncol(p11n1)]))
class(p11n2)
str(p11n2)
unique(unlist(p11n2)) # 0  1

for(i in 1:ncol(p11n2)) p11n2[,i] <- as.numeric(as.character(p11n2[,i]))

# Final checks
p11n2[1:10, 1:10]
dim(p11n2)  # 62 sites  291 species

range(p11n2)
sort(colSums(p11n2))

# Remove species without presences
p11n3 <- p11n2[,which(colSums(p11n2)>0)]
p11n3[1:10, 1:10]
dim(p11n3)  # 62 sites  291 species

# homogenize the rownames
row.names(p11n3) <- gsub(".", "_", row.names(p11n3), fixed=T)

# Save it
write.table(p11n3, file="PlotSpc_2011_2Fev17.txt", sep="\t", quote=F)



#********************************************************************
# 2014
#********************************************************************
p14[1:10, 1:10]
dim(p14) # 141  476

old <- names(p14)
length(old) # 476
length(unique(old)) # 476

# Check that all species are in the list
length(old[old %in% syn$ini]) # 467
out <- old[!old %in% syn$ini]

out2 <- substring(out, 1, nchar(out)-1)
out2 <- gsub(".", "", out2, fixed=T)
out2[!out2 %in% syn$ini]

out2

# adapt them to the correspondance list
old_corr <- old
old_corr[!old_corr %in% syn$ini] <- out2
length(old_corr[old_corr %in% syn$ini]) # 476

# replace the species names by the correct ones
COR <- cbind(oldID=old, oldCORR=old_corr)
COR <- merge(COR, syn, by.x="oldCORR", by.y="ini")
head(COR)
dim(COR) # 476

COR[!(row.names(COR) %in% row.names(na.omit(COR))), ] # 0

p14n <- p14
TempNam <- names(p14n)
for(i in names(p14n)) { 
  TempNam[TempNam==i] <- COR[which(COR$oldID==i), "fin"] 
  print(paste(i, "-->", COR[which(COR$oldID==i), "fin"] ))
} 
names(p14n) <- TempNam
dim(p14n) # 141  476
p14n[1:10, 1:10]

# resume the duplicates
p14n1 <- as.data.frame(cbind(spcID=names(p14n), t(p14n)), row.names=1:length(p14n))
p14n1[1:10, 1:10]
dim(p14n1) #   476  142

dupli <- unique(as.character(p14n1$spcID[duplicated(p14n1$spcID)]))
for(i in dupli) { 
  t.row <- row.names(p14n1[which(p14n1==i),])
  remp <- as.numeric(apply(p14n1[t.row, 2:ncol(p14n1)], 2, max)) 
  for(j in t.row) p14n1[j, 2:ncol(p14n1)] <- remp ; print(i)
}
p14n1 <- unique(p14n1)
dim(p14n1) #   436  142
p14n1[1:10, 1:10]

row.names(p14n1) <- p14n1$spcID 
p14n2 <- as.data.frame(t(p14n1[,2:ncol(p14n1)]))
class(p14n2)
str(p14n2)
unique(unlist(p14n2)) # 0  1

for(i in 1:ncol(p14n2)) p14n2[,i] <- as.numeric(as.character(p14n2[,i]))

# Final checks
p14n2[1:10, 1:10]
dim(p14n2)  # 141 sites  436 species

range(p14n2)
sort(colSums(p14n2))

# Remove species without presences
p14n3 <- p14n2[,which(colSums(p14n2)>0)]
# homogenize the rownames
row.names(p14n3) <- gsub(".", "_", row.names(p14n3), fixed=T)
p14n3[1:10, 1:10]
dim(p14n3)  # 141 sites  436 species

# Save it
write.table(p14n3, file="PlotSpc_2014_2Fev17.txt", sep="\t", quote=F)



######################################################################
######################################################################
# Check and change spc names on the SUB-plot data
######################################################################
######################################################################

#********************************************************************
#********************************************************************
# 2002
#********************************************************************
#********************************************************************
sp02_Abun[1:10, 1:10] 
sp02_Cov[1:10, 1:10]

dim(sp02_Abun) # 624  824
dim(sp02_Cov)  # 624  824

old <- names(sp02_Abun)
length(old) # 824
length(unique(old)) # 824

dd <- old[duplicated(old)] # 0

# Check that all species are in the list
length(old[old %in% syn$ini]) # 798
out <- old[!old %in% syn$ini]

out2 <- gsub("_NA", "", out)
out2 <- gsub(".", "", out2, fixed=T)
out2[out2=="Aspalathus_greygreen"] <- "Aspalathus_grey_green"
out2[out2=="Aspalathus_long"] <- "Aspalathus_long_peduncle"
out2[out2=="Aspalathus_retroflexa"] <- "Aspalathus_retroflexa_ssp._retroflexa"
out2[out2=="Ehrharta_rupestris"] <- "Ehrharta_rupestris_ssp_tricostata"
out2[out2=="Epilobium_tetragonium"] <- "Epilobium_tetragonum"
out2[out2=="Felicia_tenella"] <- "Felicia_tenella_ssp._Tenella"
out2[out2=="Monopsis_debilis"] <- "Monopsis_debilis_var_depressa"
out2[out2=="Olea_europaea"] <- "Olea_europea"
out2[out2=="Trifolium_angustifolia"] <- "Trifolium_angustifolium"

out2[!out2 %in% syn$ini]

# adapt them to the correspondance list
old_corr <- old
old_corr[!old_corr %in% syn$ini] <- out2
length(old_corr[old_corr %in% syn$ini]) # 824

# replace the species names by the correct ones
COR <- cbind(oldID=old, oldCORR=old_corr)
COR <- merge(COR, syn, by.x="oldCORR", by.y="ini")
head(COR)
dim(COR) # 824

COR[!(row.names(COR) %in% row.names(na.omit(COR))), ] # 0


#====================
# ABUNDANCE DATA
#====================

sp02_An <- sp02_Abun
TempNam <- names(sp02_An)
for(i in names(sp02_An)) { 
  TempNam[TempNam==i] <- COR[which(COR$oldID==i), "fin"] 
  print(paste(i, "-->", COR[which(COR$oldID==i), "fin"] ))
} 
names(sp02_An) <- TempNam
dim(sp02_An) # 624  824
sp02_An[1:10, 1:10]

# resume the duplicates
sp02_An1 <- as.data.frame(cbind(spcID=names(sp02_An), t(sp02_An)), row.names=1:length(sp02_An))
sp02_An1[1:10, 1:10]
dim(sp02_An1) #   824 625

dupli <- unique(as.character(sp02_An1$spcID[duplicated(sp02_An1$spcID)]))
for(i in dupli) { 
  t.row <- row.names(sp02_An1[which(sp02_An1==i),])
  remp <- as.numeric(apply(sp02_An1[t.row, 2:ncol(sp02_An1)], 2, max)) 
  for(j in t.row) sp02_An1[j, 2:ncol(sp02_An1)] <- remp ; print(i)
}
sp02_An1 <- unique(sp02_An1)
dim(sp02_An1) #   811  625
sp02_An1[1:10, 1:10]

row.names(sp02_An1) <- sp02_An1$spcID 
sp02_An2 <- as.data.frame(t(sp02_An1[,2:ncol(sp02_An1)]))
class(sp02_An2)
str(sp02_An2)
sort(as.numeric(as.character(unique(unlist(sp02_An2))))) # 0-32

for(i in 1:ncol(sp02_An2)) sp02_An2[,i] <- as.numeric(as.character(sp02_An2[,i]))

# Final checks
sp02_An2[1:10, 1:10]
dim(sp02_An2)  # 624 sites  811 species
range(sp02_An2)
sort(colSums(sp02_An2))

#====================
# COVER DATA
#====================

sp02_Cn <- sp02_Cov
TempNam <- names(sp02_Cn)
for(i in names(sp02_Cn)) { 
  TempNam[TempNam==i] <- COR[which(COR$oldID==i), "fin"] 
  print(paste(i, "-->", COR[which(COR$oldID==i), "fin"] ))
} 
names(sp02_Cn) <- TempNam
dim(sp02_Cn) # 624  824
sp02_Cn[1:10, 1:10]

# resume the duplicates
sp02_Cn1 <- as.data.frame(cbind(spcID=names(sp02_Cn), t(sp02_Cn)), row.names=1:length(sp02_Cn))
sp02_Cn1[1:10, 1:10]
dim(sp02_Cn1) #   824 625

dupli <- unique(as.character(sp02_Cn1$spcID[duplicated(sp02_Cn1$spcID)]))
for(i in dupli) { 
  t.row <- row.names(sp02_Cn1[which(sp02_Cn1==i),])
  remp <- as.numeric(apply(sp02_Cn1[t.row, 2:ncol(sp02_Cn1)], 2, max)) 
  for(j in t.row) sp02_Cn1[j, 2:ncol(sp02_Cn1)] <- remp ; print(i)
}
sp02_Cn1 <- unique(sp02_Cn1)
dim(sp02_Cn1) #   811  625
sp02_Cn1[1:10, 1:10]

row.names(sp02_Cn1) <- sp02_Cn1$spcID 
sp02_Cn2 <- as.data.frame(t(sp02_Cn1[,2:ncol(sp02_Cn1)]))
class(sp02_Cn2)
str(sp02_Cn2)
sort(as.numeric(as.character(unique(unlist(sp02_Cn2))))) # 0-95.0

for(i in 1:ncol(sp02_Cn2)) sp02_Cn2[,i] <- as.numeric(as.character(sp02_Cn2[,i]))

# Final checks
sp02_Cn2[1:10, 1:10]
dim(sp02_Cn2)  # 624 sites  811 species
range(sp02_Cn2)
sort(colSums(sp02_Cn2))


#====================
# HOMOGENEIZE THE ABUNDANCE AND COVER DATA
#====================

# WARNING, SPECIES WITH 0 ABUNDANCES BUT COVER > 0 ARE SHADING THE PLOT BUT THEIR ROOT IS OUTSIDE
# WE WILL GIVE THEM AN ABUNDANCE OF 0.1

# Check species presences in each site
sp02_An3 <- data.frame(PlotID=row.names(sp02_An2), sp02_An2)
tt1 <- melt(sp02_An3, id="PlotID")
tt1 <- tt1[which(tt1$value>0),][,1:2]
tt1 <- unique(tt1[order(tt1$PlotID),])
head(tt1) ; dim(tt1) # 6162  2

sp02_Cn3 <- data.frame(PlotID=row.names(sp02_Cn2), sp02_Cn2)
tt2 <- melt(sp02_Cn3, id="PlotID")
tt2 <- tt2[which(tt2$value>0),][,1:2]
tt2 <- unique(tt2[order(tt2$PlotID),])
head(tt2) ; dim(tt2)  # 6244  2

tt1$comb <- paste(tt1[,1], tt1[,2], sep="_")
tt2$comb <- paste(tt2[,1], tt2[,2], sep="_")
head(tt1) ; head(tt2)

TT1 <- tt1[!tt1$comb %in% tt2$comb,]
TT2 <- tt2[!tt2$comb %in% tt1$comb,]
for(i in 1:3) { TT1[,i] <- as.character(TT1[,i]) ; TT2[,i] <- as.character(TT2[,i]) }

# attribute an abundance of 0.1 to missing species abundances
for(i in 1:nrow(TT2)){ sp02_An3[TT2[i,"PlotID"], TT2[i, "variable"]] <- 0.1 ; print(i) }
for(i in 1:nrow(TT1)){ sp02_Cn3[TT1[i,"PlotID"], TT1[i, "variable"]] <- 0.1 ; print(i) }

# Remove species without presences
sp02_An3 <- sp02_An3[,-grep("PlotID", names(sp02_An3))]
sp02_Cn3 <- sp02_Cn3[,-grep("PlotID", names(sp02_Cn3))]

sp02_An3 <- sp02_An3[,which(colSums(sp02_An3)>0)]
sp02_Cn3 <- sp02_Cn3[,which(colSums(sp02_Cn3)>0)]

# homogenize the rownames
row.names(sp02_An3) <- gsub("p", "", row.names(sp02_An3), fixed=T)
row.names(sp02_Cn3) <- gsub("p", "", row.names(sp02_Cn3), fixed=T)

pc <- data.frame(num=paste(rep(1:52, each=12), paste(rep(1:6, each=2), 1:12, sep="_"), sep="_"), 
                 row.names = paste(rep(1:52, each=12), letters[1:12], sep="_"))
row.names(sp02_An3) <- as.character(pc[row.names(sp02_An3),1])
row.names(sp02_Cn3) <- as.character(pc[row.names(sp02_Cn3),1])

sp02_An3[1:10, 1:10]
sp02_Cn3[1:10, 1:10]
dim(sp02_An3)  # 624 sites  595 species
dim(sp02_Cn3)  # 624 sites  595 species
hist(rowSums(sp02_An3))  
hist(rowSums(sp02_Cn3))
rowSums(sp02_An3)[rowSums(sp02_An3)==0]
rowSums(sp02_Cn3)[rowSums(sp02_Cn3)==0]

# Save it
write.table(sp02_An3, file="subPlotSpc_2002_NbIndiv_2Fev17.txt", sep="\t", quote=F)
write.table(sp02_Cn3, file="subPlotSpc_2002_PropCover_2Fev17.txt", sep="\t", quote=F)
# sp02_An3 <- read.delim("subPlotSpc_2002_NbIndiv_2Fev17.txt", stringsAsFactors = F)
# sp02_Cn3 <- read.delim("subPlotSpc_2002_PropCover_2Fev17.txt", stringsAsFactors = F)

#********************************************************************
#********************************************************************
# 2008
#********************************************************************
#********************************************************************
sp08_Abun[1:10, 1:10] 
sp08_Cov[1:10, 1:10]

dim(sp08_Abun) # 527  595
dim(sp08_Cov)  # 527  595

old <- names(sp08_Abun)
length(old) # 595
length(unique(old)) # 595

dd <- old[duplicated(old)] # 0

# Check that all species are in the list
length(old[old %in% syn$ini]) # 543
out <- old[!old %in% syn$ini]
out <- sapply(strsplit(out, "7."), function(x) ifelse(length(x)>1, paste(x[1], x[2], sep="7/"), x))

out2 <- out[!out %in% syn$ini]
out2 <- substring(out2, 1, nchar(out2)-1)
out2[out2=="Erica_sp_2007/"] <- "Erica_sp"
out2[out2=="Ischyrolepis_tenuissima_2007/"] <- "Ischyrolepis_tenuissima"
out2[out2=="Struthiola_ciliata_._S.argentiu"] <- "Struthiola_ciliata"
out2[out2=="Thesium_sp_2007/"] <- "Thesium_sp"
out2[out2=="Vygie_20"] <- "Vygie_2007/"
out2[!out2 %in% syn$ini]

# adapt them to the correspondance list
old_corr <- old
old_corr[!old_corr %in% syn$ini] <- out
old_corr[!old_corr %in% syn$ini] <- out2
length(old_corr[old_corr %in% syn$ini]) # 595

# replace the species names by the correct ones
COR <- cbind(oldID=old, oldCORR=old_corr)
COR <- merge(COR, syn, by.x="oldCORR", by.y="ini")
head(COR)
dim(COR) # 595

COR[!(row.names(COR) %in% row.names(na.omit(COR))), ] # 0

#====================
# ABUNDANCE DATA
#====================

sp08_An <- sp08_Abun
TempNam <- names(sp08_An)
for(i in names(sp08_An)) { 
  TempNam[TempNam==i] <- COR[which(COR$oldID==i), "fin"] 
  print(paste(i, "-->", COR[which(COR$oldID==i), "fin"] ))
} 
names(sp08_An) <- TempNam
dim(sp08_An) # 527  595
sp08_An[1:10, 1:10]

# resume the duplicates
sp08_An1 <- as.data.frame(cbind(spcID=names(sp08_An), t(sp08_An)), row.names=1:length(sp08_An))
sp08_An1[1:10, 1:10]
dim(sp08_An1) #   595  528

dupli <- unique(as.character(sp08_An1$spcID[duplicated(sp08_An1$spcID)]))
for(i in dupli) { 
  t.row <- row.names(sp08_An1[which(sp08_An1==i),])
  remp <- as.numeric(apply(sp08_An1[t.row, 2:ncol(sp08_An1)], 2, max)) 
  for(j in t.row) sp08_An1[j, 2:ncol(sp08_An1)] <- remp ; print(i)
}
sp08_An1 <- unique(sp08_An1)
dim(sp08_An1) #   538  528
sp08_An1[1:10, 1:10]

row.names(sp08_An1) <- sp08_An1$spcID 
sp08_An2 <- as.data.frame(t(sp08_An1[,2:ncol(sp08_An1)]))
class(sp08_An2)
str(sp08_An2)
sort(as.numeric(as.character(unique(unlist(sp08_An2))))) # 0-90

for(i in 1:ncol(sp08_An2)) sp08_An2[,i] <- as.numeric(as.character(sp08_An2[,i]))

# Final checks
sp08_An2[1:10, 1:10]
dim(sp08_An2)  # 527 sites  538 species
range(sp08_An2) # 0  90
sort(colSums(sp08_An2))

#====================
# COVER DATA
#====================

sp08_Cn <- sp08_Cov
TempNam <- names(sp08_Cn)
for(i in names(sp08_Cn)) { 
  TempNam[TempNam==i] <- COR[which(COR$oldID==i), "fin"] 
  print(paste(i, "-->", COR[which(COR$oldID==i), "fin"] ))
} 
names(sp08_Cn) <- TempNam
dim(sp08_Cn) # 527  595
sp08_Cn[1:10, 1:10]

# resume the duplicates
sp08_Cn1 <- as.data.frame(cbind(spcID=names(sp08_Cn), t(sp08_Cn)), row.names=1:length(sp08_Cn))
sp08_Cn1[1:10, 1:10]
dim(sp08_Cn1) #   528  595

dupli <- unique(as.character(sp08_Cn1$spcID[duplicated(sp08_Cn1$spcID)]))
for(i in dupli) { 
  t.row <- row.names(sp08_Cn1[which(sp08_Cn1==i),])
  remp <- as.numeric(apply(sp08_Cn1[t.row, 2:ncol(sp08_Cn1)], 2, max)) 
  for(j in t.row) sp08_Cn1[j, 2:ncol(sp08_Cn1)] <- remp ; print(i)
}
sp08_Cn1 <- unique(sp08_Cn1)
dim(sp08_Cn1) #   538  528
sp08_Cn1[1:10, 1:10]

row.names(sp08_Cn1) <- sp08_Cn1$spcID 
sp08_Cn2 <- as.data.frame(t(sp08_Cn1[,2:ncol(sp08_Cn1)]))
class(sp08_Cn2)
str(sp08_Cn2)
sort(as.numeric(as.character(unique(unlist(sp08_Cn2))))) # 0-100

for(i in 1:ncol(sp08_Cn2)) sp08_Cn2[,i] <- as.numeric(as.character(sp08_Cn2[,i]))

# Final checks
sp08_Cn2[1:10, 1:10]
dim(sp08_Cn2)  # 527 sites  538 species
range(sp08_Cn2)  # 0 100
sort(colSums(sp08_Cn2))


#====================
# HOMOGENEIZE THE ABUNDANCE AND COVER DATA
#====================

# WARNING, SPECIES WITH 0 ABUNDANCES BUT COVER > 0 ARE SHADING THE PLOT BUT THEIR ROOT IS OUTSIDE
# WE WILL GIVE THEM AN ABUNDANCE OF 0.1

# Check species presences in each site
sp08_An3 <- data.frame(PlotID=row.names(sp08_An2), sp08_An2)
tt1 <- melt(sp08_An3, id="PlotID")
tt1 <- tt1[which(tt1$value>0),][,1:2]
tt1 <- unique(tt1[order(tt1$PlotID),])
head(tt1) ; dim(tt1) # 4065  2

sp08_Cn3 <- data.frame(PlotID=row.names(sp08_Cn2), sp08_Cn2)
tt2 <- melt(sp08_Cn3, id="PlotID")
tt2 <- tt2[which(tt2$value>0),][,1:2]
tt2 <- unique(tt2[order(tt2$PlotID),])
head(tt2) ; dim(tt2)  # 4437  2

tt1$comb <- paste(tt1[,1], tt1[,2], sep="_")
tt2$comb <- paste(tt2[,1], tt2[,2], sep="_")
head(tt1) ; head(tt2)

TT1 <- tt1[!tt1$comb %in% tt2$comb,]
TT2 <- tt2[!tt2$comb %in% tt1$comb,]
for(i in 1:3) { TT1[,i] <- as.character(TT1[,i]) ; TT2[,i] <- as.character(TT2[,i]) }

# attribute an abundance of 0.1 to missing species abundances
for(i in 1:nrow(TT2)){ sp08_An3[TT2[i,"PlotID"], TT2[i, "variable"]] <- 0.1 ; print(i) }
for(i in 1:nrow(TT1)){ sp08_Cn3[TT1[i,"PlotID"], TT1[i, "variable"]] <- 0.1 ; print(i) }

# Remove species without presences
sp08_An3 <- sp08_An3[,-grep("PlotID", names(sp08_An3))]
sp08_Cn3 <- sp08_Cn3[,-grep("PlotID", names(sp08_Cn3))]

sp08_An3 <- sp08_An3[,which(colSums(sp08_An3)>0)]
sp08_Cn3 <- sp08_Cn3[,which(colSums(sp08_Cn3)>0)]


# homogenize the rownames
row.names(sp08_An3) <- gsub(".", "_", row.names(sp08_An3), fixed=T)
row.names(sp08_Cn3) <- gsub(".", "_", row.names(sp08_Cn3), fixed=T)

sp08_An3[1:10, 1:10]
sp08_Cn3[1:10, 1:10]
dim(sp08_An3)  # 527 sites  538 species
dim(sp08_Cn3)  # 527 sites  538 species
hist(rowSums(sp08_An3))  
hist(rowSums(sp08_Cn3))
rowSums(sp08_An3)[rowSums(sp08_An3)==0]
rowSums(sp08_Cn3)[rowSums(sp08_Cn3)==0]

# Save it
write.table(sp08_An3, file="subPlotSpc_2008_NbIndiv_2Fev17.txt", sep="\t", quote=F)
write.table(sp08_Cn3, file="subPlotSpc_2008_PropCover_2Fev17.txt", sep="\t", quote=F)


#********************************************************************
#********************************************************************
# 2011
#********************************************************************
#********************************************************************
sp11_Abun[1:10, 1:10] 
sp11_Cov[1:10, 1:10]

dim(sp11_Abun) # 131  354
dim(sp11_Cov)  # 131  354

old <- names(sp11_Abun)
length(old) # 354
length(unique(old)) # 354

dd <- old[duplicated(old)] # 0

# Check that all species are in the list
length(old[old %in% syn$ini]) # 338
out <- old[!old %in% syn$ini]

out2 <- substring(out, 1, nchar(out)-1)
out2 <- gsub(".", "", out2, fixed=T)
out2[out2=="Chrysocoma"] <- "Chrysocoma_?"
out2[out2=="Holothrix_villosa______"] <- "Holothirx_villosa"
out2[out2=="Lampranthus_emarginatus_"] <- "Lampranthus_emarginatus"
out2[out2=="Physcia_digitata"] <- "Physcia_digitata?"
out2[out2=="Restio_quadratus_"] <- "Restio_quadratus"
out2[out2=="Thamnochortus_areanariuslucin"] <- "Thamnochortis_lucins/orenarius"
out2[out2=="Thamnochortus_areanaruislucin"] <- "Thamnochortis_lucins/orenarius"
out2[out2=="Tracheandra_tabularis_"] <- "Tracheandra_tabularus_?"
out2[!out2 %in% syn$ini]
out2

# adapt them to the correspondance list
old_corr <- old
old_corr[!old_corr %in% syn$ini] <- out
old_corr[!old_corr %in% syn$ini] <- out2
length(old_corr[old_corr %in% syn$ini]) # 354

# replace the species names by the correct ones
COR <- cbind(oldID=old, oldCORR=old_corr)
COR <- merge(COR, syn, by.x="oldCORR", by.y="ini")
head(COR)
dim(COR) # 354

COR[!(row.names(COR) %in% row.names(na.omit(COR))), ] # 0

#====================
# ABUNDANCE DATA
#====================

sp11_An <- sp11_Abun
TempNam <- names(sp11_An)
for(i in names(sp11_An)) { 
  TempNam[TempNam==i] <- COR[which(COR$oldID==i), "fin"] 
  print(paste(i, "-->", COR[which(COR$oldID==i), "fin"] ))
} 
names(sp11_An) <- TempNam
dim(sp11_An) # 131  354
sp11_An[1:10, 1:10]

# resume the duplicates
sp11_An1 <- as.data.frame(cbind(spcID=names(sp11_An), t(sp11_An)), row.names=1:length(sp11_An))
sp11_An1[1:10, 1:10]
dim(sp11_An1) #   354 132

dupli <- unique(as.character(sp11_An1$spcID[duplicated(sp11_An1$spcID)]))
for(i in dupli) { 
  t.row <- row.names(sp11_An1[which(sp11_An1==i),])
  remp <- as.numeric(apply(sp11_An1[t.row, 2:ncol(sp11_An1)], 2, max)) 
  for(j in t.row) sp11_An1[j, 2:ncol(sp11_An1)] <- remp ; print(i)
}
sp11_An1 <- unique(sp11_An1)
dim(sp11_An1) #   255  132
sp11_An1[1:10, 1:10]

row.names(sp11_An1) <- sp11_An1$spcID 
sp11_An2 <- as.data.frame(t(sp11_An1[,2:ncol(sp11_An1)]))
class(sp11_An2)
str(sp11_An2)
sort(as.numeric(as.character(unique(unlist(sp11_An2))))) # 0-75

for(i in 1:ncol(sp11_An2)) sp11_An2[,i] <- as.numeric(as.character(sp11_An2[,i]))

# Final checks
sp11_An2[1:10, 1:10]
dim(sp11_An2)  # 131 sites  255 species
range(sp11_An2) # 0  75
sort(colSums(sp11_An2))

#====================
# COVER DATA
#====================

sp11_Cn <- sp11_Cov
TempNam <- names(sp11_Cn)
for(i in names(sp11_Cn)) { 
  TempNam[TempNam==i] <- COR[which(COR$oldID==i), "fin"] 
  print(paste(i, "-->", COR[which(COR$oldID==i), "fin"] ))
} 
names(sp11_Cn) <- TempNam
dim(sp11_Cn) # 131  354
sp11_Cn[1:10, 1:10]

# resume the duplicates
sp11_Cn1 <- as.data.frame(cbind(spcID=names(sp11_Cn), t(sp11_Cn)), row.names=1:length(sp11_Cn))
sp11_Cn1[1:10, 1:10]
dim(sp11_Cn1) #   354  132

dupli <- unique(as.character(sp11_Cn1$spcID[duplicated(sp11_Cn1$spcID)]))
for(i in dupli) { 
  t.row <- row.names(sp11_Cn1[which(sp11_Cn1==i),])
  remp <- as.numeric(apply(sp11_Cn1[t.row, 2:ncol(sp11_Cn1)], 2, max)) 
  for(j in t.row) sp11_Cn1[j, 2:ncol(sp11_Cn1)] <- remp ; print(i)
}
sp11_Cn1 <- unique(sp11_Cn1)
dim(sp11_Cn1) #   255  132
sp11_Cn1[1:10, 1:10]

row.names(sp11_Cn1) <- sp11_Cn1$spcID 
sp11_Cn2 <- as.data.frame(t(sp11_Cn1[,2:ncol(sp11_Cn1)]))
class(sp11_Cn2)
str(sp11_Cn2)
sort(as.numeric(as.character(unique(unlist(sp11_Cn2))))) # 0-100

for(i in 1:ncol(sp11_Cn2)) sp11_Cn2[,i] <- as.numeric(as.character(sp11_Cn2[,i]))

# Final checks
sp11_Cn2[1:10, 1:10]
dim(sp11_Cn2)  # 131 sites  255 species
range(sp11_Cn2)  # 0 100
sort(colSums(sp11_Cn2))


#====================
# HOMOGENEIZE THE ABUNDANCE AND COVER DATA
#====================

# WARNING, SPECIES WITH 0 ABUNDANCES BUT COVER > 0 ARE SHADING THE PLOT BUT THEIR ROOT IS OUTSIDE
# WE WILL GIVE THEM AN ABUNDANCE OF 0.1

# Check species presences in each site
sp11_An3 <- data.frame(PlotID=row.names(sp11_An2), sp11_An2)
tt1 <- melt(sp11_An3, id="PlotID")
tt1 <- tt1[which(tt1$value>0),][,1:2]
tt1 <- unique(tt1[order(tt1$PlotID),])
head(tt1) ; dim(tt1) # 1268  2

sp11_Cn3 <- data.frame(PlotID=row.names(sp11_Cn2), sp11_Cn2)
tt2 <- melt(sp11_Cn3, id="PlotID")
tt2 <- tt2[which(tt2$value>0),][,1:2]
tt2 <- unique(tt2[order(tt2$PlotID),])
head(tt2) ; dim(tt2)  # 1344  2

tt1$comb <- paste(tt1[,1], tt1[,2], sep="_")
tt2$comb <- paste(tt2[,1], tt2[,2], sep="_")
head(tt1) ; head(tt2)

TT1 <- tt1[!tt1$comb %in% tt2$comb,]
TT2 <- tt2[!tt2$comb %in% tt1$comb,]
for(i in 1:3) { TT1[,i] <- as.character(TT1[,i]) ; TT2[,i] <- as.character(TT2[,i]) }

# attribute an abundance of 0.1 to missing species abundances
for(i in 1:nrow(TT2)){ sp11_An3[TT2[i,"PlotID"], TT2[i, "variable"]] <- 0.1 ; print(i) }
for(i in 1:nrow(TT1)){ sp11_Cn3[TT1[i,"PlotID"], TT1[i, "variable"]] <- 0.1 ; print(i) }

# Remove species without presences
sp11_An3 <- sp11_An3[,-grep("PlotID", names(sp11_An3))]
sp11_Cn3 <- sp11_Cn3[,-grep("PlotID", names(sp11_Cn3))]

sp11_An3 <- sp11_An3[,which(colSums(sp11_An3)>0)]
sp11_Cn3 <- sp11_Cn3[,which(colSums(sp11_Cn3)>0)]


# homogenize the rownames
row.names(sp11_An3) <- gsub(".", "_", gsub("_", "", row.names(sp11_An3)), fixed=T)
row.names(sp11_Cn3) <- gsub(".", "_", gsub("_", "", row.names(sp11_Cn3)), fixed=T)

sp11_An3[1:10, 1:10]
sp11_Cn3[1:10, 1:10]
dim(sp11_An3)  # 131 sites  254 species
dim(sp11_Cn3)  # 131 sites  254 species
hist(rowSums(sp11_An3))  
hist(rowSums(sp11_Cn3))
rowSums(sp11_An3)[rowSums(sp11_An3)==0]
rowSums(sp11_Cn3)[rowSums(sp11_Cn3)==0]

# Save it
write.table(sp11_An3, file="subPlotSpc_2011_NbIndiv_2Fev17.txt", sep="\t", quote=F)
write.table(sp11_Cn3, file="subPlotSpc_2011_PropCover_2Fev17.txt", sep="\t", quote=F)



#********************************************************************
#********************************************************************
# 2014
#********************************************************************
#********************************************************************

sp14_Abun[1:10, 1:10] 
sp14_Cov[1:10, 1:10]

dim(sp14_Abun) # 319  420
dim(sp14_Cov)  # 319  420

old <- names(sp14_Abun)
length(old) # 420
length(unique(old)) # 420

dd <- old[duplicated(old)] # 0

# Check that all species are in the list
length(old[old %in% syn$ini]) # 384
out <- old[!old %in% syn$ini]

out2 <- gsub(".", "", out, fixed=T)
out2[out2=="Moss_"] <- "Moss_spp"
out2[out2=="Passerina_paleacea_"] <- "Passerina_paleacea_dead"
out2[!out2 %in% syn$ini]
out2

# adapt them to the correspondance list
old_corr <- old
old_corr[!old_corr %in% syn$ini] <- out
old_corr[!old_corr %in% syn$ini] <- out2
length(old_corr[old_corr %in% syn$ini]) # 420

# replace the species names by the correct ones
COR <- cbind(oldID=old, oldCORR=old_corr)
COR <- merge(COR, syn, by.x="oldCORR", by.y="ini")
head(COR)
dim(COR) # 420

COR[!(row.names(COR) %in% row.names(na.omit(COR))), ] # 0

#====================
# ABUNDANCE DATA
#====================

sp14_An <- sp14_Abun
TempNam <- names(sp14_An)
for(i in names(sp14_An)) { 
  TempNam[TempNam==i] <- COR[which(COR$oldID==i), "fin"] [1]
  print(paste(i, "-->", COR[which(COR$oldID==i), "fin"] ))
} 
names(sp14_An) <- TempNam
dim(sp14_An) # 319  420
sp14_An[1:10, 1:10]

# resume the duplicates
sp14_An1 <- as.data.frame(cbind(spcID=names(sp14_An), t(sp14_An)), row.names=1:length(sp14_An))
sp14_An1[1:10, 1:10]
dim(sp14_An1) #   420 320

dupli <- unique(as.character(sp14_An1$spcID[duplicated(sp14_An1$spcID)]))
for(i in dupli) { 
  t.row <- row.names(sp14_An1[which(sp14_An1==i),])
  remp <- as.numeric(apply(sp14_An1[t.row, 2:ncol(sp14_An1)], 2, max)) 
  for(j in t.row) sp14_An1[j, 2:ncol(sp14_An1)] <- remp ; print(i)
}
sp14_An1 <- unique(sp14_An1)
dim(sp14_An1) #   345  320
sp14_An1[1:10, 1:10]

row.names(sp14_An1) <- sp14_An1$spcID 
sp14_An2 <- as.data.frame(t(sp14_An1[,2:ncol(sp14_An1)]))
class(sp14_An2)
str(sp14_An2)
sort(as.numeric(as.character(unique(unlist(sp14_An2))))) # 0-150

for(i in 1:ncol(sp14_An2)) sp14_An2[,i] <- as.numeric(as.character(sp14_An2[,i]))

# Final checks
sp14_An2[1:10, 1:10]
dim(sp14_An2)  # 319 sites  345 species
range(sp14_An2) # 0  150
sort(colSums(sp14_An2))

#====================
# COVER DATA
#====================

sp14_Cn <- sp14_Cov
TempNam <- names(sp14_Cn)
for(i in names(sp14_Cn)) { 
  TempNam[TempNam==i] <- COR[which(COR$oldID==i), "fin"] [1]
  print(paste(i, "-->", COR[which(COR$oldID==i), "fin"] ))
} 
names(sp14_Cn) <- TempNam
dim(sp14_Cn) # 319  420
sp14_Cn[1:10, 1:10]

# resume the duplicates
sp14_Cn1 <- as.data.frame(cbind(spcID=names(sp14_Cn), t(sp14_Cn)), row.names=1:length(sp14_Cn))
sp14_Cn1[1:10, 1:10]
dim(sp14_Cn1) #   420  320

dupli <- unique(as.character(sp14_Cn1$spcID[duplicated(sp14_Cn1$spcID)]))
for(i in dupli) { 
  t.row <- row.names(sp14_Cn1[which(sp14_Cn1==i),])
  remp <- as.numeric(apply(sp14_Cn1[t.row, 2:ncol(sp14_Cn1)], 2, max)) 
  for(j in t.row) sp14_Cn1[j, 2:ncol(sp14_Cn1)] <- remp ; print(i)
}
sp14_Cn1 <- unique(sp14_Cn1)
dim(sp14_Cn1) #   345  320
sp14_Cn1[1:10, 1:10]

row.names(sp14_Cn1) <- sp14_Cn1$spcID 
sp14_Cn2 <- as.data.frame(t(sp14_Cn1[,2:ncol(sp14_Cn1)]))
class(sp14_Cn2)
str(sp14_Cn2)
sort(as.numeric(as.character(unique(unlist(sp14_Cn2))))) # 0-110

for(i in 1:ncol(sp14_Cn2)) sp14_Cn2[,i] <- as.numeric(as.character(sp14_Cn2[,i]))

# Final checks
sp14_Cn2[1:10, 1:10]
dim(sp14_Cn2)  # 319 sites  345 species
range(sp14_Cn2)  # 0 110
sort(colSums(sp14_Cn2))


#====================
# HOMOGENEIZE THE ABUNDANCE AND COVER DATA
#====================

# WARNING, SPECIES WITH 0 ABUNDANCES BUT COVER > 0 ARE SHADING THE PLOT BUT THEIR ROOT IS OUTSIDE
# WE WILL GIVE THEM AN ABUNDANCE OF 0.1

# Check species presences in each site
sp14_An3 <- data.frame(PlotID=row.names(sp14_An2), sp14_An2)
tt1 <- melt(sp14_An3, id="PlotID")
tt1 <- tt1[which(tt1$value>0),][,1:2]
tt1 <- unique(tt1[order(tt1$PlotID),])
head(tt1) ; dim(tt1) # 2208  2

sp14_Cn3 <- data.frame(PlotID=row.names(sp14_Cn2), sp14_Cn2)
tt2 <- melt(sp14_Cn3, id="PlotID")
tt2 <- tt2[which(tt2$value>0),][,1:2]
tt2 <- unique(tt2[order(tt2$PlotID),])
head(tt2) ; dim(tt2)  # 2658  2

tt1$comb <- paste(tt1[,1], tt1[,2], sep="_")
tt2$comb <- paste(tt2[,1], tt2[,2], sep="_")
head(tt1) ; head(tt2)

TT1 <- tt1[!tt1$comb %in% tt2$comb,]
TT2 <- tt2[!tt2$comb %in% tt1$comb,]
for(i in 1:3) { TT1[,i] <- as.character(TT1[,i]) ; TT2[,i] <- as.character(TT2[,i]) }

# attribute an abundance of 0.1 to missing species abundances
for(i in 1:nrow(TT2)){ sp14_An3[TT2[i,"PlotID"], TT2[i, "variable"]] <- 0.1 ; print(i) }
for(i in 1:nrow(TT1)){ sp14_Cn3[TT1[i,"PlotID"], TT1[i, "variable"]] <- 0.1 ; print(i) }

# Remove species without presences
sp14_An3 <- sp14_An3[,-grep("PlotID", names(sp14_An3))]
sp14_Cn3 <- sp14_Cn3[,-grep("PlotID", names(sp14_Cn3))]

sp14_An3 <- sp14_An3[,which(colSums(sp14_An3)>0)]
sp14_Cn3 <- sp14_Cn3[,which(colSums(sp14_Cn3)>0)]

# homogenize the rownames
pc <- data.frame(num=paste(rep(1:52, each=12), paste(rep(1:6, each=2), 1:12, sep="_"), sep="_"), 
                 row.names = paste(rep(1:52, each=12), paste(rep(1:6, each=2), LETTERS[1:12], sep="."), sep="."))
row.names(pc)[row.names(pc)=="18.6.K"] <- "18.5.K"
row.names(pc)[row.names(pc)=="34.5.J"] <- "34.4.J"
row.names(sp14_An3) <- as.character(pc[row.names(sp14_An3),1])
row.names(sp14_Cn3) <- as.character(pc[row.names(sp14_Cn3),1])

sp14_An3[1:10, 1:10]
sp14_Cn3[1:10, 1:10]
dim(sp14_An3)  # 319 sites  345 species
dim(sp14_Cn3)  # 319 sites  345 species
hist(rowSums(sp14_An3))  
hist(rowSums(sp14_Cn3))
rowSums(sp14_An3)[rowSums(sp14_An3)==0]
rowSums(sp14_Cn3)[rowSums(sp14_Cn3)==0]

# Save it
write.table(sp14_An3, file="subPlotSpc_2014_NbIndiv_2Fev17.txt", sep="\t", quote=F)
write.table(sp14_Cn3, file="subPlotSpc_2014_PropCover_2Fev17.txt", sep="\t", quote=F)


