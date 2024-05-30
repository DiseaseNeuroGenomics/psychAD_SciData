library(ggplot2)
library(UpSetR)
library(data.table)
library(RColorBrewer)
library(dplyr)
library(tidyr)
library(ggsci)

################################################################################
##### Set paths & Define helper functions ######################################
{
  setwd("./") # FIXME: SET TO THE FOLDER WITH UNCOMPRESSED REPOSITORY
  dry_lab_dir = "dry_lab/"
  wet_lab_dir = "wet_lab/"
  outDir = "output_dir"
  dir.create(outDir)
  mpdf = function(x, width=7,height=7, outDir=outDir, onefile=T)eval.parent(substitute({ pdf(paste0(outDir,"/plot_",make.names(x),".pdf"),useDingbats=F,width=width,height=height,onefile=onefile) })) #outDir must be defined as a global var

  # Load demographics & clinical & other metadata for all donors
  metadata = read.csv(file.path(wet_lab_dir, "cohort_metadata.csv"))
  rownames(metadata) = metadata$SubID
  
  # Palletes
  colorScheme = list(
    red="#B2182B",
    yellow="#E6AB02",
    green="#66A61E",
    blue="#2166AC",
    pink="#E7298A",
    purple="#7570B3")
  
  wongPalette = list(
    darkYellow="#E69F00", 
    lightBlue="#56B4E9",
    lightGreen="#009E73",
    lightYellow="#F0E442", 
    darkBlue="#0072B2",
    orange="#D55E00",
    purple="#CC79A7")
  
  # Get upper triangle of the correlation matrix
  get_upper_tri <- function(cormat){
    cormat[lower.tri(cormat)]<- NA
    return(cormat)
  }
}

################################################################################
##### DEFINITION OF DIAGNOSES ##################################################

{
  DX_DESC = list(
    "nps_MoodDysCurValue" = "Dysphoria",
    "nps_DecIntCurValue" = "Anhedonia",
    "nps_WtLossCurValue" = "Weight loss",
    "nps_DecAppCurValue" = "Decreased appetite",
    "nps_WtGainCurValue" = "Weight gain",
    "nps_EarlyInsomCurValue" = "Early insomnia",
    "nps_MidInsomCurValue" = "Middle insomnia",
    "nps_LateInsomCurValue" = "Late insomnia",
    "nps_HypersomCurValue" = "Hypersomnia",
    "nps_PsychoAgiCurValue" = "Psychomotor agitation",
    "nps_PsychoRetardCurValue" = "Psychomotor retardation",
    "nps_FatCurValue" = "Fatigue",
    "nps_WorthCurValue" = "Worthlessness",
    "nps_DelCurValue" = "Delusional worthlessness",
    "nps_RumCurValue" = "Ruminations",
    "nps_ThoughtDeathCurValue" = "Suicidal ideations",
    "nps_Last2WkCurValue" = "Depression (current to 2 weeks)",
    "nps_SixMoCurValue" = "Depression (current to 6 months)",
    "nps_LifeCurValue" = "Depression (lifetime)", 
    "AD" = "Alzheimer's disease", 
    "MCI" = "Mild cognitive impairement",
    "Dementia" = "Dementia",
    "PD" = "Parkinson's disease",
    "PD_uncertain_plus_encephalitic" = "Encephalitis, uncertain Parkinson's disase",
    "DLBD" = "Diffuse Lewy body disease", 
    "FTD" = "Frontotemporal dementia", 
    "ALS" = "Amyotrophic lateral sclerosis", 
    "NormPressHydrocephalus" = "Normal pressure hydrocephalus",
    "ArgyrophilicGrain" = "Argyrophilic grain disease",
    "CerebralAtrophy" = "Cerebral atrophy",	
    "Tauopathy" = "Tauopathy", 
    "MS" = "Multiple sclerosis",
    "PSP" = "Progressive supranuclear palsy", 
    "Epilepsy" = "Epilepsy", 
    "Seizures" = "Seizures", 
    "Tumor" = "Brain tumor", 
    "Head_Injury" = "Head injury", 
    "Vascular" = "Vascular dementia", 
    "Leucotomy" = "Leucotomy / Lobotomy", 
    "SCZ" = "Schizophrenia", 
    "MDD" = "Major depressive disorder", 
    "BD_unspecific" = "BD NOS (not otherwise specified)", 
    "BD_I" = "Bipolar disorder type I", 
    "BD_II" = "Bipolar disorder type II", 
    "PTSD" = "Post-traumatic stress disorder", 
    "ADHD" = "Attention deficit hyperactivity disorder", 
    "OCD" = "Obsessive-compulsive disorder", 
    "Tardive_Dyskinesia_Neuroleptic_induced" = "Neuroleptic-induced tardive dyskinesia", 
    "Schizoaffective_bipolar" = "Schizoaffective bipolar disorder", 
    "Schizoaffective_depressive" = "Schizoaffective depressive disorder", 
    "Anorexia" = "Anorexia nervosa", 
    "Bulimia" = "Bulimia nervosa", 
    "Binge_Purge" = "Binge eating disorder", 
    "Diabetes_mellitus_unspecified" = "Diabetes mellitus unspecified", 
    "ASHCVD" = "Atherosclerotic cardiovascular disease", 
    "TD_I" = "Type 1 diabetes", 
    "TD_II" = "Type 2 diabetes",
    "Sleep_WeightGain_Guilt_Suicide" = "Aggregated__Sleep_WeightGain_Guilt_Suicide",
    "WeightLoss_PMA" = "Aggregated__WeightLoss_PMA",
    "Depression_Mood" = "Aggregated__Depression_Mood"
  )
  
  DIAGNOSIS_CLASSIFICATION = list(
    "neurodegenerative" = c("AD", "MCI", "Dementia", "PD", "PD_uncertain_plus_encephalitic", "DLBD", "FTD", "ALS", "NormPressHydrocephalus",	"ArgyrophilicGrain",
                            "CerebralAtrophy",	"Tauopathy", "Others_Neurodegenerative"),
    "neurological" = c("MS", "PSP", "Epilepsy", "Seizures", "Tumor", "Head_Injury", "Vascular", "Others_Neurological", 
                       "Leucotomy"), 
    "neuropsychiatric" = c("SCZ", "MDD", "BD_unspecific", "BD_I", "BD_II", "PTSD", "ADHD", "OCD", "Tardive_Dyskinesia_Neuroleptic_induced", "Schizoaffective_bipolar", 
                           "Schizoaffective_depressive", "Anorexia", "Bulimia"),
    "metabolic" = c("Diabetes_mellitus_unspecified", "ASHCVD", "TD_I", "TD_II"),
    "nps" = c("nps_MoodDysCurValue", "nps_DecIntCurValue", "nps_WtLossCurValue", "nps_DecAppCurValue", "nps_WtGainCurValue", "nps_EarlyInsomCurValue", "nps_MidInsomCurValue", 
              "nps_LateInsomCurValue", "nps_HypersomCurValue", "nps_PsychoAgiCurValue", "nps_PsychoRetardCurValue", "nps_FatCurValue", "nps_WorthCurValue", "nps_DelCurValue", 
              "nps_RumCurValue", "nps_ThoughtDeathCurValue", "nps_Last2WkCurValue", "nps_SixMoCurValue", "nps_LifeCurValue", 
              "Sleep_WeightGain_Guilt_Suicide", "WeightLoss_PMA", "Depression_Mood"))
  
  all_dx = as.character(unlist(DIAGNOSIS_CLASSIFICATION))
  
  ###############
  # Summary of dx
  individual_dx = unlist(DIAGNOSIS_CLASSIFICATION)
  individual_dx2_counts = lapply(individual_dx, function(dx) {
    na.omit(sapply(1:nrow(metadata), function(i) {
      ifelse((metadata[i,dx] == 1), metadata[i,"SubID"], NA)
    }))
  })
  names(individual_dx2_counts) = individual_dx
  individual_dx2_counts = individual_dx2_counts[!names(individual_dx2_counts) %in% c("Others_Neurological", "Others_Neurodegenerative", "Others_Neuropsychiatric")]
  
  individual_dx_countsDf2 = cbind.data.frame(names(individual_dx2_counts),
                                             sapply(individual_dx2_counts, function(dxx) length(dxx)),
                                             sapply(individual_dx2_counts, function(ids) length(metadata[(metadata$SubID %in% ids) & (metadata$Brain_bank == "MSSM"),"SubID"])),
                                             sapply(individual_dx2_counts, function(ids) length(metadata[(metadata$SubID %in% ids) & (metadata$Brain_bank == "HBCC"),"SubID"])),
                                             sapply(individual_dx2_counts, function(ids) length(metadata[(metadata$SubID %in% ids) & (metadata$Brain_bank == "RUSH"),"SubID"])),
                                             sapply(individual_dx2_counts, function(ids) mean(metadata[(metadata$SubID %in% ids) & (metadata$Brain_bank == "MSSM"),"Age"])),
                                             sapply(individual_dx2_counts, function(ids) mean(metadata[(metadata$SubID %in% ids) & (metadata$Brain_bank == "HBCC"),"Age"])),
                                             sapply(individual_dx2_counts, function(ids) mean(metadata[(metadata$SubID %in% ids) & (metadata$Brain_bank == "RUSH"),"Age"])))
  colnames(individual_dx_countsDf2) = c("Diagnosis", "Freq", "count_mssm", "count_hbcc", "count_rush", "age_mssm", "age_hbcc", "age_rush")
  individual_dx_countsDf2$category = sapply(individual_dx_countsDf2$Diagnosis, function(dx) {
    names(DIAGNOSIS_CLASSIFICATION)[lapply(DIAGNOSIS_CLASSIFICATION, function(x) dx %in% x)==T]
  })
}

################################################################################
##### Figure 2 #################################################################
##### Summary of demographics and clinical data of the PsychAD cohort ##########

{
  ##########################
  # Fig. 2: "Correlations among AD-related phenotypes"
  # controls_neuropathological_clinical_mssm_manual = c("M62918",  "M133696", "M22175", "M27514", "M283783", "M651234", "M69009", "M991513", "M732693")
  controls_neuropathological_clinical_mssm_manual = c()
  SubID_group = list(
    "controls_neuropathological_clinical" = unique(na.omit(sapply(1:nrow(metadata), function(i) {
      ifelse((sum(na.omit(as.integer(metadata[i,setdiff(all_dx, c(DIAGNOSIS_CLASSIFICATION$nps, "AD", "Dementia"))]))) == 0) & 
               (((metadata[i,"CERAD"] %in% c(1)) & (metadata[i,"BRAAK_AD"] %in% c(0, 1, 2, 3)) & !is.na(metadata[i,"Dementia"])) | (metadata[i,"Brain_bank"] == "HBCC")), 
             metadata[i,"SubID"], NA)
    }))),
    "AD" = na.omit(sapply(1:nrow(metadata), function(i) {
      ifelse((sum(na.omit(as.integer(metadata[i,setdiff(all_dx, c(DIAGNOSIS_CLASSIFICATION$nps, "MCI", "Dementia", "AD"))]))) == 0) & (metadata[i,"AD"] == 1), 
             metadata[i,"SubID"], NA)
    })))
  
  corrMatrix = metadata[c(SubID_group$controls_neuropathological_clinical, SubID_group$AD), c("CERAD", "CDRScore", "BRAAK_AD", "AD", "MCI", "Plq_Mn", "Brain_bank")]
  corrMatrix = corrMatrix[corrMatrix$Brain_bank %in% c("MSSM", "RUSH"),]
  corrMatrix[corrMatrix$Brain_bank=="RUSH","CDRScore"] = sapply(which(corrMatrix$Brain_bank=="RUSH"), function(i) { if(is.na(corrMatrix[i,"AD"]) | is.na(corrMatrix[i,"MCI"]==0)) { 0 } else if(corrMatrix[i,"AD"]==0 & corrMatrix[i,"MCI"]==0) { 0 } else if(corrMatrix[i,"MCI"]) { 0.75 } else { 3 } })
  corrMatrix = corrMatrix[,c("CERAD","CDRScore", "Plq_Mn", "BRAAK_AD")]
  corrMatrixOut = data.frame(sapply(1:ncol(corrMatrix), function(x) sapply(1:ncol(corrMatrix), function(y) { cor.test(as.numeric(corrMatrix[,x]), as.numeric(corrMatrix[,y]), method="spearman")$estimate } )))
  colnames(corrMatrixOut) = c("CERAD","CDRScore", "Plq_Mn", "BRAAK_AD")
  rownames(corrMatrixOut) = colnames(corrMatrixOut)
  corrMatrixOut = get_upper_tri(corrMatrixOut)
  corrMatrixOut$ID = rownames(corrMatrixOut)
  corrMatrixOut = reshape2::melt(corrMatrixOut, id.vars="ID", factorsAsStrings=T)
  corrMatrixOut$variable = ordered(corrMatrixOut$variable,levels=c("CERAD","CDRScore", "Plq_Mn", "BRAAK_AD"))
  corrMatrixOut$ID = ordered(corrMatrixOut$ID,levels=c("CERAD","CDRScore", "Plq_Mn", "BRAAK_AD"))
  
  corr = ggplot(corrMatrixOut, aes(variable, ID, fill = value)) + geom_tile() + scale_fill_material("red") +
    coord_equal() + theme_bw() + theme(axis.title.x=element_blank(), axis.text.x=element_text(colour = "black"), axis.text.y=element_text(colour = "black"), axis.ticks.x=element_blank(),
                                       axis.title.y=element_blank(),  axis.ticks.y=element_blank())
  corr = corr + geom_text(aes(variable, ID, label = round(value, 2)), color = "black", size = 3)
  mpdf("Fig2_dxDistr_ad_phenotypes_correlation", width=4, height=4); print(corr); dev.off()
  
  ##########################
  # Fig. 2: "Dx distribution"
  dxSel = setdiff(c(all_dx, "BD_ALL", "PD_ALL", "TD_ALL", "Control"), c(DIAGNOSIS_CLASSIFICATION$nps, "Dementia", "MCI", "PD_uncertain_plus_encephalitic", "PD", "BD_I", "BD_II", "BD_unspecific", "TD_I", "TD_II", "Diabetes_mellitus_unspecified"))
  
  listInput = lapply(dxSel, function(colName) {
    metadata[which(as.integer(metadata[,colName]) == 1), "SubID"]
  })
  names(listInput) = dxSel
  minDonors = sort(unlist(lapply(listInput, length)), decreasing=T)[11]
  listInput = listInput[lapply(listInput, length) >= minDonors]
  
  # myPlot <- upset(fromList(listInput), order.by = "freq", nsets = 12)
  mpdf("Fig2_dxDistr", width=15, height=10, onefile=F); print(upset(fromList(listInput), order.by = "freq", nsets = 12)); dev.off()
  
  ##########################
  # Fig. 2d: "Sex distribution"
  distrSex = metadata %>%
    group_by(Brain_bank, Sex) %>%
    summarize(count = n()) %>%
    pivot_wider(names_from = Sex, values_from = count) %>%
    mutate(ratio_male = Male / (Male+Female)) %>%
    mutate(ratio_female = Female / (Male+Female))
  distrSexDf = melt(data.frame(distrSex[,c("Brain_bank", "ratio_male", "ratio_female")]), id.vars=c("Brain_bank"))
  # distrSexDf$Brain_bank = gsub("RUSH", "RADC", distrSexDf$Brain_bank)
  distrSexDf$Brain_bank = ordered(distrSexDf$Brain_bank, levels=c("MSSM", "HBCC", "RADC")) 
  distrSexDf$variable = ordered(gsub("ratio_", "", as.character(distrSexDf$variable)), levels=c("male", "female"))
  
  piePlot_sex = ggplot(distrSexDf, aes(x = "", y = value, fill = variable)) + geom_bar(stat = "identity", color = "white", width=1) + coord_polar("y", start = 0) + facet_wrap(~ Brain_bank) + 
    theme_void() + theme(axis.ticks=element_blank(), axis.title=element_blank(), axis.text.y = element_blank(), panel.grid  = element_blank()) + geom_text(aes(label = ""), stat = "identity") + 
    scale_fill_manual(values=c(wongPalette$darkBlue, wongPalette$orange))
  piePlot_sex
  mpdf("Fig2_sexDistr", width=5, height=5); print(piePlot_sex); dev.off()
  
  # Fig. 2: "Ethnicity distribution"
  metadata$Ethnicity_simplified = gsub("^EAS_SAS$", "Asian", gsub("^SAS$", "Asian", gsub("^SA$S", "Asian", gsub("^EAS$", "Asian", gsub("^EUR$", "European", gsub("^AFR$", "African", gsub("^AMR$", "Admixed American", metadata$Ethnicity)))))))
  metadata$Ethnicity_simplified[metadata$Ethnicity_simplified == "Unknown"] = NA
  
  distrEthnicity = metadata %>%
    group_by(Brain_bank, Ethnicity_simplified) %>%
    summarize(count = n()) %>%
    pivot_wider(names_from = Ethnicity_simplified, values_from = count) %>%
    mutate(ratio_European = European / (European + African + Asian + `Admixed American`))  %>% 
    mutate(ratio_African = African / (European + African + Asian + `Admixed American`))  %>% 
    mutate(ratio_AdmixedAmerican = `Admixed American` / (European + African + Asian + `Admixed American`))  %>% 
    mutate(ratio_Asian = Asian  / (European + African + Asian + `Admixed American`))
  distrEthnicityDf = melt(data.frame(distrEthnicity[,c("Brain_bank", "ratio_European", "ratio_African", "ratio_AdmixedAmerican", "ratio_Asian")]), id.vars=c("Brain_bank"))
  distrEthnicityDf$variable = ordered(gsub("ratio_", "", as.character(distrEthnicityDf$variable)), levels=c("European", "African", "AdmixedAmerican", "Asian"))
  # distrEthnicityDf$Brain_bank = gsub("RUSH", "RADC", distrEthnicityDf$Brain_bank)
  distrEthnicityDf$Brain_bank = ordered(distrEthnicityDf$Brain_bank, levels=c("MSSM", "HBCC", "RADC")) 
  
  piePlot_ethnicity = ggplot(distrEthnicityDf, aes(x = "", y = value, fill = variable)) + geom_bar(stat = "identity", color = "white", width=1) + coord_polar("y", start = 0) + facet_wrap(~ Brain_bank) + 
    theme_void() + theme(axis.ticks=element_blank(), axis.title=element_blank(), axis.text.y = element_blank(), panel.grid  = element_blank()) + geom_text(aes(label = ""), stat = "identity") + 
    scale_fill_manual(values=c(wongPalette$darkBlue, wongPalette$darkYellow, wongPalette$orange, wongPalette$lightBlue))
  mpdf("Fig2_pie_ethnicity", width=5, height=5); print(piePlot_ethnicity); dev.off()
  print(paste0("Donors with European ancestry constituted over ", round(distrEthnicityDf[(distrEthnicityDf$variable=="European") & (distrEthnicityDf$Brain_bank=="MSSM"),"value"]*100, 3), "% of the MSSM dataset, whereas in HBCC, it accounted for only ", round(distrEthnicityDf[(distrEthnicityDf$variable=="European") & (distrEthnicityDf$Brain_bank=="HBCC"),"value"]*100, 3), "%, and in RADC, it was ", round(distrEthnicityDf[(distrEthnicityDf$variable=="European") & (distrEthnicityDf$Brain_bank=="RADC"),"value"]*100, 3), "%"))
  
  # Fig. 2: "Age distribution"
  metadata$Aging = ifelse(metadata[,"Age"] <= 1, "Infancy", ifelse(metadata[,"Age"] <= 12, "Childhood", ifelse(metadata[,"Age"] <= 20, "Adolescence", ifelse(metadata[,"Age"] <= 40, "Young_adult", ifelse(metadata[,"Age"] <= 60, "Middle_adult", "Late_adult")))))
  distrAging = metadata %>% 
    group_by(Brain_bank, Aging) %>% 
    summarize(count = n()) %>% 
    pivot_wider(names_from = Aging, values_from = count) %>% 
    mutate(ratio_Infancy = coalesce(Infancy, 0) / (ifelse(is.na(Infancy), 0, Infancy) + ifelse(is.na(Childhood), 0, Childhood)  + ifelse(is.na(Adolescence), 0, Adolescence)  + ifelse(is.na(Young_adult), 0, Young_adult)  + ifelse(is.na(Middle_adult), 0, Middle_adult)  + ifelse(is.na(Late_adult), 0, Late_adult))) %>% 
    mutate(ratio_Childhood = coalesce(Childhood, 0) / (ifelse(is.na(Infancy), 0, Infancy) + ifelse(is.na(Childhood), 0, Childhood)  + ifelse(is.na(Adolescence), 0, Adolescence)  + ifelse(is.na(Young_adult), 0, Young_adult)  + ifelse(is.na(Middle_adult), 0, Middle_adult)  + ifelse(is.na(Late_adult), 0, Late_adult))) %>% 
    mutate(ratio_Adolescence = coalesce(Adolescence, 0) / (ifelse(is.na(Infancy), 0, Infancy) + ifelse(is.na(Childhood), 0, Childhood)  + ifelse(is.na(Adolescence), 0, Adolescence)  + ifelse(is.na(Young_adult), 0, Young_adult)  + ifelse(is.na(Middle_adult), 0, Middle_adult)  + ifelse(is.na(Late_adult), 0, Late_adult))) %>% 
    mutate(ratio_Young_adult = coalesce(Young_adult, 0) / (ifelse(is.na(Infancy), 0, Infancy) + ifelse(is.na(Childhood), 0, Childhood)  + ifelse(is.na(Adolescence), 0, Adolescence)  + ifelse(is.na(Young_adult), 0, Young_adult)  + ifelse(is.na(Middle_adult), 0, Middle_adult)  + ifelse(is.na(Late_adult), 0, Late_adult))) %>% 
    mutate(ratio_Middle_adult = coalesce(Middle_adult, 0) / (ifelse(is.na(Infancy), 0, Infancy) + ifelse(is.na(Childhood), 0, Childhood)  + ifelse(is.na(Adolescence), 0, Adolescence)  + ifelse(is.na(Young_adult), 0, Young_adult)  + ifelse(is.na(Middle_adult), 0, Middle_adult)  + ifelse(is.na(Late_adult), 0, Late_adult))) %>% 
    mutate(ratio_Late_adult = coalesce(Late_adult, 0) / (ifelse(is.na(Infancy), 0, Infancy) + ifelse(is.na(Childhood), 0, Childhood)  + ifelse(is.na(Adolescence), 0, Adolescence)  + ifelse(is.na(Young_adult), 0, Young_adult)  + ifelse(is.na(Middle_adult), 0, Middle_adult)  + ifelse(is.na(Late_adult), 0, Late_adult)))
  
  distrAgingDf = melt(data.frame(distrAging[,c("Brain_bank", "ratio_Adolescence", "ratio_Childhood", "ratio_Infancy", "ratio_Late_adult", "ratio_Middle_adult", "ratio_Young_adult")]), id.vars=c("Brain_bank"))
  distrAgingDf$variable = ordered(gsub("ratio_", "", as.character(distrAgingDf$variable)), levels=c("Infancy", "Childhood", "Adolescence", "Young_adult", "Middle_adult", "Late_adult"))
  # distrAgingDf$Brain_bank = gsub("RUSH", "RADC", distrAgingDf$Brain_bank)
  distrAgingDf$Brain_bank = ordered(distrAgingDf$Brain_bank, levels=c("MSSM", "HBCC", "RADC")) 
  
  pieplot_age = ggplot(distrAgingDf, aes(x = "", y = value, fill = variable)) + geom_bar(stat = "identity", color = "white", width=1) + coord_polar("y", start = 0) + facet_wrap(~ Brain_bank) + 
    theme_void() + theme(axis.ticks=element_blank(), axis.title=element_blank(), axis.text.y = element_blank(), panel.grid  = element_blank()) + geom_text(aes(label = ""), stat = "identity") + 
    scale_fill_manual(values=brewer.pal(6,"Reds"))
  mpdf("Fig2_pie_age", width=5, height=5); print(pieplot_age); dev.off()
  print(paste0("Regarding age distribution, ", round(distrAgingDf[(distrAgingDf$variable=="Late_adult") & (distrAgingDf$Brain_bank=="MSSM"),"value"]*100, 1), "% and ", round(distrAgingDf[(distrAgingDf$variable=="Late_adult") & (distrAgingDf$Brain_bank=="RADC"),"value"]*100, 1), "% of individuals in the MSSM and RADC subcohorts are over 60 years old, while HBCC has only ", round(distrAgingDf[(distrAgingDf$variable=="Late_adult") & (distrAgingDf$Brain_bank=="HBCC"),"value"]*100, 3), "% in this oldest age category, with ", round(sum(distrAgingDf[!(distrAgingDf$variable %in% c("Late_adult", "Middle_adult")) & (distrAgingDf$Brain_bank=="HBCC"),"value"])*100, 1), "% of donors under 40 years old"))
  
  # Fig. 2: "Distribution of dx category"
  metadata$Control = ifelse(metadata$sum_dx == 0, 1, 0)
  metadata$ControlMetabol = sapply(1:nrow(metadata), function(i) {
    ifelse(sum(na.omit(as.integer(metadata[i,setdiff(all_dx, c(DIAGNOSIS_CLASSIFICATION$metabolic, DIAGNOSIS_CLASSIFICATION$nps))]))) == 0, 1, 0)
  })
  
  distDxCat = do.call("rbind.data.frame", lapply(c("MSSM", "HBCC", "RADC"), function(bb) {
    df = metadata[metadata$Brain_bank == bb,]
    dxCat_neurodeg = sum(sapply(1:nrow(df), function(i) {
      (sum(na.omit(as.integer(df[i,setdiff(DIAGNOSIS_CLASSIFICATION$neurodegenerative, c("Anorexia", "Bulimia", "Binge_Purge"))]))) > 0) }))
    dxCat_neuropsych = sum(sapply(1:nrow(df), function(i) {
      (sum(na.omit(as.integer(df[i,setdiff(DIAGNOSIS_CLASSIFICATION$neuropsychiatric, c("Anorexia", "Bulimia", "Binge_Purge"))]))) > 0) }))
    dxCat_metabolic = sum(sapply(1:nrow(df), function(i) {
      (sum(na.omit(as.integer(df[i,setdiff(c(DIAGNOSIS_CLASSIFICATION$metabolic, "Control"), c("Anorexia", "Bulimia", "Binge_Purge"))]))) > 0) }))
    dxSum = sum(c(dxCat_neurodeg, dxCat_neuropsych, dxCat_metabolic))
    c((dxCat_neurodeg / dxSum), (dxCat_neuropsych / dxSum), (dxCat_metabolic / dxSum))
  }))
  colnames(distDxCat) = c("neurodeg", "neuropsych", "controls")
  distDxCat$Brain_bank = c("MSSM", "HBCC", "RADC")
  distDxCatDf = melt(distDxCat, id.vars=c("Brain_bank"))
  # distDxCatDf$Brain_bank = gsub("RUSH", "RADC", distDxCatDf$Brain_bank)
  distDxCatDf$Brain_bank = ordered(distDxCatDf$Brain_bank, levels=c("MSSM", "HBCC", "RADC")) 

  pieplot_dxCat = ggplot(distDxCatDf, aes(x = "", y = value, fill = variable)) + geom_bar(stat = "identity", color = "white", width=1) + coord_polar("y", start = 0) + facet_wrap(~ Brain_bank) + 
    theme_void() + theme(axis.ticks=element_blank(), axis.title=element_blank(), axis.text.y = element_blank(), panel.grid  = element_blank()) + geom_text(aes(label = ""), stat = "identity") + 
    scale_fill_manual(values=c(wongPalette$darkBlue, wongPalette$darkYellow, wongPalette$purple))
  mpdf("Fig2_pie_dxCat", width=5, height=5); print(pieplot_dxCat); dev.off()
}

################################################################################
##### Figure 3 #################################################################
################################################################################

{
  # Fig. 3: "Analysis of the counts and intersections among sources of genotyping 
  # data available for donors from the PsychAD cohort."
  # Numbers for Methods' section: DNA section preparation: Overall strategy
  df = metadata[metadata$Brain_bank=="MSSM","WGS_CommonMind"]
  mssmPreviousCoverage = (is.na(metadata[metadata$Brain_bank=="MSSM","WGS_CommonMind"]) & 
                            is.na(metadata[metadata$Brain_bank=="MSSM","ADSP_SampleId"]) & 
                            is.na(metadata[metadata$Brain_bank=="MSSM","SNParray_CommonMind"]) & 
                            is.na(metadata[metadata$Brain_bank=="MSSM","SNParray_HBBC"]))
  hbccPreviousCoverage = metadata[(!is.na(metadata$WGS_CommonMind) | 
                                   !is.na(metadata$SNParray_CommonMind)) &
                                    (metadata$Brain_bank == "HBCC"),"SubID"]
  rushPreviousCoverage = metadata[!is.na(metadata$WGS_RUSH),"SubID"]
  print(paste0("The majority of donors from the HBCC (", round(length(hbccPreviousCoverage) / nrow(metadata[(metadata$Brain_bank == "HBCC"),]) * 100, 1), "%) and RADC (", round(length(rushPreviousCoverage) / nrow(metadata[(metadata$Brain_bank == "RADC"),]) * 100, 1), "%) brain banks had previously undergone genotyping ..."))
  print(paste0("However, for the MSSM brain bank, the coverage from previous genotyping efforts was notably lower, with only ", round(sum(mssmPreviousCoverage == T) / length(mssmPreviousCoverage) * 100, 1), "% (", sum(mssmPreviousCoverage == T), " donors) ..."))

  gtCols = c("SNParray_CommonMind_with_HBCC", "WGS_RUSH", "SNParray_PsychAD", "ADSP_WGS")
  gtCols_upset = c("CommonMind", "RADC", "PsychAD-MSSM", "ADSP")
  gtAvailability = lapply(gtCols_upset, function(dsetName) {
    if (dsetName == 'RADC') {
      return(metadata[!is.na(metadata[,"WGS_RUSH"]),"SubID"])
    } else if (dsetName == 'CommonMind') {
      return(metadata[!is.na(metadata[,"SNParray_CommonMind_with_HBCC"]),"SubID"])
    } else if (dsetName == 'PsychAD-MSSM') {
      return(metadata[!is.na(metadata[,"SNParray_PsychAD"]),"SubID"])
    } else if (dsetName == 'ADSP') {
      return(metadata[!is.na(metadata[,"ADSP_WGS"]),"SubID"])
    } 
    
  })
  names(gtAvailability) = gtCols_upset
  mpdf("Fig3_genoIntersections", width=15, height=7, onefile=F); print(upset(fromList(gtAvailability), order.by = "freq", nsets = length(gtAvailability))); dev.off()
  
  # Numbers for Methods' section: DNA preparation: Overall strategy
  sapply(gtCols, function(gt) sum(!is.na(metadata[,gt])))
  moreThanOneGt = sum(sapply(1:nrow(metadata), function(i) { ifelse(length(na.omit(as.character(metadata[i,gtCols]))) > 1, T, F) }))
  print(paste0("These datasets exhibited a significant overlap, resulting in ", moreThanOneGt, " donors with one or more duplicates ..."))
  
  sapply(gtCols, function(gt) sum(!is.na(metadata[,gt])))
  wgsPresentForDuplicate = sum(sapply(1:nrow(metadata), function(i) { ifelse(length(na.omit(as.character(metadata[i,gtCols]))) > 1 & !is.na(metadata$ADSP_WGS[i]), T, F) }))
  print(paste0("First, if one sample was obtained through WGS and the other through SNP array genotyping, we retained the WGS sample. This prioritization criterion was applied to ", wgsPresentForDuplicate, " donors."))
  print(paste0("All of the remaining ", (moreThanOneGt - wgsPresentForDuplicate - 8), " donors had samples in both the ..."))
  print(paste0("As a result, the final combined genotype file encompasses ", round((sum(!is.na(metadata$primary_genotype)) / nrow(metadata))*100, 1), "% (", sum(!is.na(metadata$primary_genotype)), ") of the PsychAD donors."))
  
  #################
  # Fig. 3: F-statistic from plink’s “check-sex” function plotted by reported sex (samples with known sex chromosome aneuploidies not shown).
  
  genoSamplesNoAneuploidy = na.omit(metadata[metadata$Sex_chr_aneuploidy==T,"SNParray_PsychAD"])
  genoSamplesNoAneuploidySubID = na.omit(metadata[metadata$Sex_chr_aneuploidy==T,"SubID"])
  genoMetadataPsychAD = read.csv(file.path(dry_lab_dir, "genotype_psychad_snparray.csv"))
  genoMetadataPsychAD = genoMetadataPsychAD[(genoMetadataPsychAD$SubID %in% metadata$SubID) & !(genoMetadataPsychAD$SubID %in% as.character(genoSamplesNoAneuploidy)),]
  
  genoMetadataPsychAD$sex_clinicalMeta = metadata[match(genoMetadataPsychAD$SubID, metadata$SubID),"Sex"]
  genoSexPlot = ggplot(genoMetadataPsychAD, aes(x=imp_sex_score, fill=sex_clinicalMeta)) + geom_histogram(aes(y=after_stat(density)), position="identity", binwidth=0.04) +
    theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), aspect.ratio = 1, legend.position.inside = c(0.5, 0.85), 
   axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour = "black")) + scale_fill_manual(values=c(wongPalette$orange, wongPalette$darkBlue))
  mpdf("Fig3_genoSexCheck", width=4, height=4); print(genoSexPlot); dev.off()
  
  #################
  # Fig. 3: Ancestry check for PsychAD-MSSM SNParray
  metadata$Ethnicity_simplified = gsub("^EAS_SAS$", "Asian", gsub("^SAS$", "Asian", gsub("^SA$S", "Asian", gsub("^EAS$", "Asian", gsub("^EUR$", "European", gsub("^AFR$", "African", gsub("^AMR$", "Admixed American", metadata$Ethnicity)))))))
  metadata$Ethnicity_simplified[metadata$Ethnicity_simplified == "Unknown"] = NA
  genoMetadataPsychAD$ancestry_clinicalMeta = ordered(metadata[match(genoMetadataPsychAD$SubID, metadata$SNParray_PsychAD),"Ethnicity_simplified"], levels=c("European", "African", "Admixed American", "Asian"))
  genoAncestryPlot = ggplot(genoMetadataPsychAD, aes(x=imp_anc_PC1, y=imp_anc_PC2, color=ancestry_clinicalMeta)) + theme_bw() + geom_point() + scale_color_manual(values=c(wongPalette$darkBlue, wongPalette$darkYellow, wongPalette$orange, wongPalette$lightBlue))
  mpdf("Fig3_genoAncestrySnparray", width=6, height=4); print(genoAncestryPlot); dev.off()
  
  #################
  # Fig. 3: Ancestry check for merged genotypes file
  genoMetadataMerged = read.csv(file.path(dry_lab_dir, "genotype_psychad_merged.csv"))
  genoMetadataMerged = genoMetadataMerged[(genoMetadataMerged$SubID %in% metadata$SubID) & !(genoMetadataMerged$SubID %in% as.character(genoSamplesNoAneuploidySubID)),]
  
  metadata$Ethnicity_simplified = gsub("^EAS_SAS$", "Asian", gsub("^SAS$", "Asian", gsub("^SA$S", "Asian", gsub("^EAS$", "Asian", gsub("^EUR$", "European", gsub("^AFR$", "African", gsub("^AMR$", "Admixed American", metadata$Ethnicity)))))))
  metadata$Ethnicity_simplified[metadata$Ethnicity_simplified == "Unknown"] = NA
  genoMetadataMerged$ancestry_clinicalMeta = ordered(metadata[match(genoMetadataMerged$SubID, metadata$SubID),"Ethnicity_simplified"], levels=c("European", "African", "Admixed American", "Asian"))
  genoAncestryMergedPlot = ggplot(genoMetadataMerged, aes(x=PC1, y=PC2, color=ancestry_clinicalMeta)) + theme_bw() + geom_point() + scale_color_manual(values=c(wongPalette$darkBlue, wongPalette$darkYellow, wongPalette$orange, wongPalette$lightBlue))
  mpdf("Fig3_genoAncestryMerged", width=6, height=4); print(genoAncestryMergedPlot); dev.off()
  
  # Numbers for the text
  print(paste0("After performing all quality control steps, we observed clear and unambiguous separation of male and female samples and good concordance of inferred and reported ethnicity for all remaining ", sum(!is.na(metadata$SNParray_PsychAD)), " samples"))
  print(paste0("Similar conclusions were drawn for the combined genotype file containing ", sum(!is.na(metadata$primary_genotype))))  
}

################################################################################
##### Figure 5 #################################################################
################################################################################

# Distribution of ages stratified by the disease
df = individual_dx_countsDf2[order(individual_dx_countsDf2$Freq, decreasing=T),]
df = df[(df$category != "nps") & (df$Diagnosis %in% (setdiff(df$Diagnosis, c("Dementia", "MCI")))), c("Diagnosis", "Freq")]
df = df[order(df$Freq, decreasing=T),]
df = metadata[,c("Age", "AD", "SCZ", "BD_ALL", "Control")]
df[is.na(df)] = 0

dfMelted = reshape::melt(df, id.vars="Age")
dfMelted = dfMelted[(dfMelted$value > 0),]
dfMelted$variable = gsub("BD_ALL", "BD", dfMelted$variable)

ageDistr = ggplot(dfMelted, aes(x = Age, fill = variable, color = variable)) +  geom_density(alpha = 0.5) + 
  labs(x = "Age", y = "Density") +
  scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) + 
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) + 
  ggtitle("Looks different because public metadata needs to have Age>89 censored") + 
  theme_bw() + theme(axis.title.x=element_blank(), axis.text.x=element_text(colour = "black"), axis.text.y=element_text(colour = "black"),
                     panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.spacing = unit(0, "lines"))
mpdf("Fig5_dxAgeDist", width=10, height=4); print(ageDistr); dev.off()

################################################################################
##### Tables S1-S3 #############################################################
################################################################################

{
  ###########
  # Table S1: Summary of all diagnoses recognized within the PsychAD cohort and description of diagnosis classification criteria for each brain bank subcohort.
  metadata[,setdiff(unlist(DIAGNOSIS_CLASSIFICATION), c("MCI", "Dementia"))][is.na(setdiff(unlist(DIAGNOSIS_CLASSIFICATION), c("MCI", "Dementia")))] = 0
  write.csv(individual_dx_countsDf2[,!colnames(individual_dx_countsDf2) %in% c("combo")], row.names=F, file=file.path(outDir, "Table_S1.csv"))
  
  ##########
  # Table S2: Classification of donors by various clinical and neuropathological traits.
  braakScore = lapply(sort(unique(metadata$BRAAK_AD)), function(braak) {
    na.omit(sapply(1:nrow(metadata), function(i) {
      ifelse((metadata$BRAAK_AD[i] == braak), metadata[i,"SubID"], NA)
    }))
  })
  names(braakScore) = paste0("BRAAK_", sort(unique(metadata$BRAAK_AD)))
  
  ceradScore = lapply(sort(unique(metadata$CERAD)), function(cerad) {
    na.omit(sapply(1:nrow(metadata), function(i) {
      ifelse((metadata$CERAD[i] == cerad), metadata[i,"SubID"], NA)
    }))
  })
  names(ceradScore) = paste0("CERAD_", sort(unique(metadata$CERAD)))
  
  cdrScore = lapply(sort(unique(metadata$CDRScore)), function(cdr) {
    na.omit(sapply(1:nrow(metadata), function(i) {
      ifelse((metadata$CDRScore[i] == cdr), metadata[i,"SubID"], NA)
    }))
  })
  names(cdrScore) = paste0("CDRScore_", sort(unique(metadata$CDRScore)))
  
  plqDistr = na.omit(metadata$Plq_Mn[metadata$Plq_Mn > 0])
  quartiles = as.numeric(c(0, 10E-100, quantile(plqDistr, probs=c(0.25, 0.5, 0.75, 1))))
  quartiles = c(quartiles, quartiles[length(quartiles)])
  plaqueScore = lapply(1:(length(quartiles)-1), function(quartile_i) {
    quartile_i_val = quartiles[quartile_i]
    quartile_ii_val = quartiles[quartile_i+1]
    
    na.omit(sapply(1:nrow(metadata), function(i) {
      ifelse((metadata$Plq_Mn[i] >= quartile_i_val) & (metadata$Plq_Mn[i] < quartile_ii_val), metadata[i,"SubID"], NA)
    }))
  })
  names(plaqueScore) = paste0("PlaqueQuartile_", quartiles[1:(length(quartiles)-1)])
  
  cogdxScore = lapply(sort(unique(metadata$cogdx)), function(cogdx) {
    na.omit(sapply(1:nrow(metadata), function(i) {
      ifelse((metadata$cogdx[i] == cogdx), metadata[i,"SubID"], NA)
    }))
  })
  names(cogdxScore) = paste0("cogdx_", sort(unique(metadata$cogdx)))
  
  individual_adScore_counts = c(ceradScore, braakScore, plaqueScore,cdrScore, cogdxScore)
  names(individual_adScore_counts) = c(names(ceradScore), names(braakScore), names(plaqueScore), names(cdrScore), names(cogdxScore))
  
  adScoresDf = cbind.data.frame(names(individual_adScore_counts),
                                sapply(individual_adScore_counts, function(dxx) length(dxx)),
                                sapply(individual_adScore_counts, function(ids) length(metadata[(metadata$SubID %in% ids) & (metadata$Brain_bank == "MSSM"),"SubID"])),
                                sapply(individual_adScore_counts, function(ids) length(metadata[(metadata$SubID %in% ids) & (metadata$Brain_bank == "RUSH"),"SubID"])),
                                sapply(individual_adScore_counts, function(ids) mean(metadata[(metadata$SubID %in% ids) & (metadata$Brain_bank == "MSSM"),"Age"])),
                                sapply(individual_adScore_counts, function(ids) mean(metadata[(metadata$SubID %in% ids) & (metadata$Brain_bank == "RUSH"),"Age"])))
  colnames(adScoresDf) = c("Diagnosis", "Freq", "count_mssm", "count_rush", "age_mssm", "age_rush")
  
  write.csv(adScoresDf, row.names=F, file=file.path(outDir, "Table_2.csv"))
  
  
  ##########################
  # Table S3: Summary of the donor counts with defined neuropsychiatric symptoms.
  table3_df = do.call("rbind.data.frame", lapply(c(DIAGNOSIS_CLASSIFICATION$nps, "Sleep_WeightGain_Guilt_Suicide", "WeightLoss_PMA", "Depression_Mood"), function(nps) {
    c(nps, DX_DESC[[nps]], sum(na.omit(metadata[,nps]==T)), sum(na.omit(metadata[,nps]==F)))
  }))
  colnames(table3_df) = c("Abbreviation", "Full name", "NPS-present", "NPS-absent")
  table3_df$`NPS-present` = as.numeric(table3_df$`NPS-present`)
  table3_df$`NPS-absent` = as.numeric(table3_df$`NPS-absent`)
  
  #write.csv(individual_dx_countsDf2[,!colnames(individual_dx_countsDf2) %in% c("combo")], row.names=F, file=file.path(outDir, "table_nps.csv"))
  write.csv(table3_df, row.names=F, file=file.path(outDir, "Table_S3.csv"))
}
