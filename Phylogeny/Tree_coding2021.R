### SCRIPT FOR PRUNNING, DATING, AND ADDING TIPS TO PHYLOGENY

library(ape)
library(rgdal)
library(prob)
library(phytools)


rm(list=ls(all=TRUE))
### read in Phylip guidetree created in ClustalW2
setwd("/RESEARCH/2020_PDtree/TREE_2021")

########BRING IN RAXML TREE
file2 <- "/RESEARCH/2020_PDtree/TREE_2021/FINAL_positive_replace.nex"  ######### Tree based on Portik et al. (2019) with ~15 taxa added from genbank. 
tree2 <- read.nexus(file2)
plot(tree2, cex = 0.5)
nodelabels(cex=.7)

########ADD TIPS WITH NO DNA
test<-bind.tip(tree2, tip.label ="Leptodactylodon_blanci", edge.length=0.0015, where=545, position=.0001)
plotTree(test, cex=1)
test2<-bind.tip(test, tip.label ="Leptodactylodon_stevarti", edge.length=0.0015, where=545, position=.0001)
plotTree(test2, cex=1)

######### Make Ultrametric
sptree.rooted.ultra <- chronopl(test2, 0, age.min = 1, age.max = 80, node = "root", S = 1, tol = 1e-8, CV = FALSE, eval.max = 500, iter.max = 500) # Make tree ultrametric
is.ultrametric(sptree.rooted.ultra)
plot(sptree.rooted.ultra, cex = 0.3)
nodelabels(cex=0.6)
write.nexus(sptree.rooted.ultra, file="FINAL_UNDATED_TREE.nex") # Save as ultrametric tree

######## Date Complete Tree
nod.ages <- c(76.6,	72.12,	58.6,	42.5,	35.8,	32,	30.8,	28.2,	25.3,	24.4,	22.7,	19.4,	18.1,	17,	15.5,	14.56,	13.18,	12,	11.27,	9.22,	7.83,	5.84,	2.32,	7.13,	4.68,	2.06,	4.95,	1.93,	11.26,	7.89,	14.75,	12.47,	17.17,	15.15,	24.53,	20.45,	38.37,	24.9,	22.99,	19.29,	15.41,	12.56,	8.96,	3.54,	10.46,	8.95,	5.35,	45.39,	43.07,	40.44,	29.08,	24.42,	25.02,	35.31,	25.77,	24.09,	35.53,	30.76,	22.71,	16.1,	8,	51.87,	2.69)
nod <- c(301,	302,	303,	304,	305,	306,	307,	308,	309,	310,	311,	312,	313,	314,	315,	316,	317,	318,	319,	320,	321,	322,	323,	326,	327,	328,	329,	330,	331,	332,	349,	354,	363,	401,	416,	417,	459,	460,	461,	462,	463,	464,	465,	466,	472,	473,	475,	478,	479,	480,	481,	482,	501,	515,	516,	542,	554,	555,	556,	557,	558,	567,	598)


calib1b <- chronopl(sptree.rooted.ultra, lambda=1, age.min = nod.ages, 
                    age.max = NULL, node = nod)
plot(calib1b, cex = 0.3)

write.nexus(calib1b, file="FINAL_DATED_TREE.nex") # Save as ultrametric tree

######### PRUNE TREE to species present in LGF
tree <- read.nexus("FINAL_DATED_TREE_renamed.nex")  ## Read in the ultrametric Tree
plot(tree, cex =.3)


#my_data <- read.csv(file.choose("specieslist.txt")) ##List of species to keep. ##### should work but did not. So I had to make list in R (species2)
#species <- c(my_data)


### List of species present in LGF
species2 <- c("Acanthixalus_spinosus",	"Afrixalus_dorsalis",	"Afrixalus_equatorialis",	"Afrixalus_fulvovittatus",	"Afrixalus_lacteus",	"Afrixalus_laevis",	
              "Afrixalus_osorioi",	"Afrixalus_paradorsalis",	"Afrixalus_weidholzi",	"Afrixalus_wittei",	"Alexteroon_hypsiphonus",	"Alexteroon_obstetricans",	
              "Arthroleptis_adelphus",	"Arthroleptis_bioko",	"Arthroleptis_carquejai",	"Arthroleptis_lameerei",	"Arthroleptis_nlonakoensis",	"Arthroleptis_palava",	
              "Arthroleptis_perreti",	"Arthroleptis_sylvaticus",	"Arthroleptis_taeniatus",	"Arthroleptis_variabilis",	"Arthroleptis_xenochirus",	
              "Arthroleptis_poecilonotus",	"Astylosternus_batesi",	"Astylosternus_diadematus",	"Astylosternus_fallax",	"Astylosternus_laurenti",	"Astylosternus_montanus",	
              "Astylosternus_nganhanus",	"Astylosternus_perreti",	"Astylosternus_ranoides",	"Astylosternus_rheophilus",	"Cardioglossa_congolia",	"Cardioglossa_elegans",	
              "Cardioglossa_escalerae",	"Cardioglossa_gracilis",	"Cardioglossa_gratiosa",	"Cardioglossa_leucomystax",	"Cardioglossa_manengouba",	
              "Cardioglossa_melanogaster",	"Cardioglossa_nigromaculata",	"Cardioglossa_oreas",	"Cardioglossa_pulchra",	"Cardioglossa_schioetzi",	"Cardioglossa_trifasciata",	
              "Cryptothylax_greshoffii",	"Hemisus_guineensis",	"Hemisus_marmoratus",	"Hemisus_perreti",	"Hyperolius_acutirostris",	"Hyperolius_ademetzi",	
              "Hyperolius_adspersus",	"Hyperolius_balfouri",	"Hyperolius_bocagei",	"Hyperolius_bolifambae",	"Hyperolius_bopeleti",	"Hyperolius_brachiofasciatus",	
              "Hyperolius_camerunensis",	"Hyperolius_cinnamomeoventris",	"Hyperolius_concolor",	"Hyperolius_dartevellei",	"Hyperolius_dintelmanni",	"Hyperolius_endjami",	
              "Hyperolius_fusciventris",	"Hyperolius_guttulatus",	"Hyperolius_kuligae",	"Hyperolius_mosaicus",	"Hyperolius_nasutus",	"Hyperolius_ocellatus",	
              "Hyperolius_olivaceus",	"Hyperolius_parallelus",	"Hyperolius_pardalis",	"Hyperolius_phantasticus",	"Hyperolius_platyceps",	"Hyperolius_quinquevittatus",	
              "Hyperolius_raymondii",	"Hyperolius_riggenbachi",	"Hyperolius_schoutedeni",	"Hyperolius_steindachneri",	"Hyperolius_sylvaticus",	"Hyperolius_tuberculatus",	
              "Kassina_decorata",	"Kassina_maculosa",	"Kassina_senegalensis",	"Leptodactylodon_albiventris",	"Leptodactylodon_axillaris",	"Leptodactylodon_bicolor",	
              "Leptodactylodon_blanci",	"Leptodactylodon_boulengeri",	"Leptodactylodon_bueanus", "Leptodactylodon_erythrogaster",	"Leptodactylodon_mertensi",	
              "Leptodactylodon_ornatus",	"Leptodactylodon_ovatus",	"Leptodactylodon_perreti",	"Leptodactylodon_polyacanthus",	"Leptodactylodon_stevarti", 
              "Leptodactylodon_ventrimarmoratus", "Leptodactylus_wildi",	"Leptopelis_aubryi",	"Leptopelis_aubryioides",	"Leptopelis_bocagii",	"Leptopelis_boulengeri",	
              "Leptopelis_brevirostris",	"Leptopelis_bufonides",	"Leptopelis_calcaratus",	"Leptopelis_millsoni",	"Leptopelis_modestus",	"Leptopelis_nordequatorialis",	
              "Leptopelis_notatus",	"Leptopelis_ocellatus",	"Leptopelis_omissus",	"Leptopelis_rufus",	"Leptopelis_viridis",	"Leptopelis_zebra",	"Nyctibates_corrugatus",	
              "Opisthothylax_immaculatus",	"Phlyctimantis_boulengeri",	"Phlyctimantis_leonardi",	"Scotobleps_gabonicus",	"Trichobatrachus_robustus", 
              "Hyperolius_nitidulus", "Chlorolius_koehleri", "Leptodactylodon_blanci", "Leptodactylodon_stevarti")

write.csv(species2, file = "specieslist2021.csv")

### Prune tree down to species present in LGF
pruned.tree2<-drop.tip(tree, setdiff(tree$tip.label, species2));
plot(pruned.tree2, cex = .5)
nodelabels()

# Save prunned ultrametric tree
write.nexus(pruned.tree2, file="LGF_PRUNNED.nex")


#### Date tree, with calibrations from Portik et al (2019)

Stree <- read.nexus("FINAL_LGF_FINAL.nex")       # Untrametric tree we will not date. 
plot(Stree, cex =0.5)
nodelabels()

Stree
nodF <- c(115,	116,	117,	118,	119,	120,	121,	122,	123,	124,	125,	126,	
               127,	128,	129,	130,	131,	136,	137,	139,	144,	145,	146,	150,	
               151,	152,	160,	163,	164,	165,	166,	167,	168,	
               169,	170,	171,	172,	173,	174,	175,	176,	177,	178,	180,	181,	
               182,	187,	188,	189,	190,	193,	200,	205,	206,	207,	208,	211,	
               216,	217,	218,	226,	227, 161, 153)

nod.agesF <- c(72.12,	58.6,	42.5,	35.8,	32,	30.8,	24.4,	22.7,	19.4,	18.1,	17.0,	15.5,	
          13.18,	12.02,	11.27,	9.22,	5.4,	4.9,	4.0,	14.7,	4.68,	4.3,	3.59,	4.37,	
          9.7,	9.3,	9.45,	8.96,	1.76,	5.35,	45.49,	43.07,	40.44,	
          29.08,	24.42,	20.24,	9.2,	6.91,	2.21,	6.91,	4.61,	2.21,	3.57,	25.01,	21.66,	
          18.5,	12.96,	1.84,	16.43,	21.21,	12.85,	11.7,	18.92,	13.37,	12.13,	9.44,	7.14,	
          35.53,	30.76,	16.1,	4.3,	1.6, 38.37, 14.15)

calib1e <- chronopl(Stree, lambda=1, age.min = nod.agesF, 
                    age.max = NULL, node = nodF)
plot(calib1e, cex = 0.5)

write.nexus(calib1e, file="LGF_PRUNNED_DATED_FINAL.nex")

