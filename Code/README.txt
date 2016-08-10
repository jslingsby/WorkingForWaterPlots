Creation:	Laure - 10 Aug 2016
Last update: 	Laure - 10 Aug 2016


##########################################################################################
##########################################################################################
############################## Description of all R scripts ##############################
##########################################################################################
##########################################################################################


##########################################################################################
############################## Data preparation scripts (prefix « P »)
##########################################################################################

- P1.getnames_tofix.R
- P2.Merge_RawTables_21June16.R
- P3.CorrectSpcNames_29July16
- P4.Make_speciesList_13July16.R
- P5.Check Phylogeny and Trait data_1August16.R
- P6.DataPrep_PlotTreatments.R


# ========================================================
# ============================  P1.getnames_tofix.R
# ======================================================== (JS)

AIM:

Collect all spc names and generate a list of all species names, synonyms, misspellings



INPUTS:

Species names were collected from: 
- Raw/plot_treatments.xls, 
- Raw/plants.xls, (i.e. trait data)
- Raw/plots.xls, 
- Raw/sup-plots.xls, 
- Raw/meta data files 2008.xls, 
- Raw/Veg data 2011.xls, 
- Raw/SAEON_2014_15_revisedAugust2015.xls



OUTPUTS:

- Raw/allnames.csv


# ========================================================
# ============================  P2.Merge_RawTables_21June16.R
# ======================================================== (LG)

AIM:

Compiling all raw « site x species » datasets into standardized and R friendly text files



INPUTS:

- allnames_old.csv

- LaurePrep/prePrep/PlotSpc_2002
- LaurePrep/prePrep/PlotSpc_2008
- LaurePrep/prePrep/PlotSpc_2011
- LaurePrep/prePrep/PlotSpc_2014

- LaurePrep/prePrep/sub-plots
- LaurePrep/prePrep/sub-plots_2008
- LaurePrep/prePrep/sub-plots_2011
- LaurePrep/prePrep/sub-plots_2014



OUTPUTS:

- LaurePrep/PlotSpc_2002_21June16
- LaurePrep/PlotSpc_2008_21June16
- LaurePrep/PlotSpc_2011_21June16
- LaurePrep/PlotSpc_2014_21June16

- LaurePrep/subPlotSpc_2002_Regeneration_21June16
- LaurePrep/subPlotSpc_2002_NbIndiv_21June16
- LaurePrep/subPlotSpc_2002_PropCover_21June16

- LaurePrep/subPlotSpc_2008_NbIndiv_21June16
- LaurePrep/subPlotSpc_2008_PropCover_21June16

- LaurePrep/subPlotSpc_2011_NbIndiv_21June16
- LaurePrep/subPlotSpc_2011_PropCover_21June16

- LaurePrep/subPlotSpc_2014_NbIndiv_21June16
- LaurePrep/subPlotSpc_2014_PropCover_21June16


# ========================================================
# ============================  P3.CorrectSpcNames_29July16
# ======================================================== (LG)

AIM:

- Correct synonymy issues & assemble duplicated names (typically by summing their abundances per sites)
- Convert abundance symbols into numbers: "." and "*" and "+" and "NA" —> 0


INPUTS:

- LaurePrep/PlotSpc_2002_21June16
- LaurePrep/PlotSpc_2008_21June16
- LaurePrep/PlotSpc_2011_21June16
- LaurePrep/PlotSpc_2014_21June16

- LaurePrep/subPlotSpc_2002_NbIndiv_21June16
- LaurePrep/subPlotSpc_2002_PropCover_21June16
- LaurePrep/subPlotSpc_2008_NbIndiv_21June16
- LaurePrep/subPlotSpc_2008_PropCover_21June16
- LaurePrep/subPlotSpc_2011_NbIndiv_21June16
- LaurePrep/subPlotSpc_2011_PropCover_21June16
- LaurePrep/subPlotSpc_2014_NbIndiv_21June16
- LaurePrep/subPlotSpc_2014_PropCover_21June16


OUTPUTS:

- LaurePrep/PlotSpc_2002_12July16
- LaurePrep/PlotSpc_2008_12July16
- LaurePrep/PlotSpc_2011_12July16
- LaurePrep/PlotSpc_2014_12July16

- LaurePrep/subPlotSpc_2002_NbIndiv_12July16
- LaurePrep/subPlotSpc_2002_PropCover_12July16
- LaurePrep/subPlotSpc_2008_NbIndiv_12July16
- LaurePrep/subPlotSpc_2008_PropCover_12July16
- LaurePrep/subPlotSpc_2011_NbIndiv_12July16
- LaurePrep/subPlotSpc_2011_PropCover_12July16
- LaurePrep/subPlotSpc_2014_NbIndiv_12July16
- LaurePrep/subPlotSpc_2014_PropCover_12July16


# ========================================================
# ============================  P4.Make_speciesList_13July16.R
# ======================================================== (LG)

AIM:

- Make native species lists
- Make invasive species lists
- Make summary table for further species name checkings


INPUTS:

- LaurePrep/CapePeninsulaList_13July16.txt
- LaurePrep/PlotSpc_2002_12July16
- LaurePrep/PlotSpc_2008_12July16
- LaurePrep/PlotSpc_2011_12July16
- LaurePrep/PlotSpc_2014_12July16
- LaurePrep/subPlotSpc_2002_NbIndiv_12July16
- LaurePrep/subPlotSpc_2008_NbIndiv_12July16
- LaurePrep/subPlotSpc_2011_NbIndiv_12July16
- LaurePrep/subPlotSpc_2014_NbIndiv_12July16

- Raw/List_Alien_SAPIA_13July16.txt
- Raw/List_Alien_CapeCom_13July16.txt
- Raw/List_Alien_iSPOT_13July16.txt


OUTPUTS:

- LaurePrep/All_species_list_CapeCom_13July16

- LaurePrep/Exotic_species_list_SA_13July16.txt  		(alien species in all South Africa)
- LaurePrep/Exotic_species_list_CapeCom_13July16.txt 	(alien species in our communities)

- LaurePrep/Spc_Plot_Year_13July16



# ========================================================
# ============================  P5.Check Phylogeny and Trait data_1August16.R
# ======================================================== (LG)

AIM:

- Compare coverage of different phylogenies (Hinchliff 2014, Zanne 2014, Forest 2007) —> choose Zanne!
- Evaluate coverage of the qualitative traits
- Evaluate coverage of the quantitative traits
- Check richness and diversity measure per plot and subplots


INPUTS:

- LaurePrep/All_species_list_CapeCom_13July16
- LaurePrep/Exotic_species_list_CapeCom_13July16.txt

- Trait_2002_1Aug16.txt				—> qualitative traits
- spp_matched_to_quantitative_trait_data.txt	-> species with quantitateive trait info

- LaurePrep/PlotSpc_2002_12July16
- LaurePrep/PlotSpc_2008_12July16
- LaurePrep/PlotSpc_2011_12July16
- LaurePrep/PlotSpc_2014_12July16
- LaurePrep/subPlotSpc_2002_NbIndiv_12July16
- LaurePrep/subPlotSpc_2008_NbIndiv_12July16
- LaurePrep/subPlotSpc_2011_NbIndiv_12July16
- LaurePrep/subPlotSpc_2014_NbIndiv_12July16


OUTPUTS:

- SpeciesWithMissingQualitativeTraits_1Aug16.txt —> the species with missing qualitative trait info
- List_Community_MPD_MFD_and_PhyTraitDistMat_14July16



# ========================================================
# ============================  P6.DataPrep_PlotTreatments.R
# ======================================================== (JS)

AIM:

INPUTS: 

OUTPUTS: 



##########################################################################################
############################### Data analysis scripts  (Prefix « A »??)
##########################################################################################








