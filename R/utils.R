
##################################################################################################
# Configures the workspace according to the operating system                                     #
##################################################################################################
FolderRoot = "~/Generate-Partitions-Communities"
FolderScripts = "~/Generate-Partitions-Communities/R"


##################################################################################################
# FUNCTION                                                                        #
#     Objective:                                                                                 #

#     Parameters                                                                                 #

#     Return:                                                                                    #

##################################################################################################
createDirs2 <- function(dataset_name, FolderResults){

  retorno = list()

  FolderResults = FolderResults
  if(dir.exists(FolderResults)==FALSE){
    dir.create(FolderResults)
  }

  FolderScripts = paste(FolderRoot, "/R", sep="")
  if(dir.exists(FolderScripts)==FALSE){
    dir.create(FolderScripts)
  }

  FolderUtils = paste(FolderRoot, "/utils", sep="")
  if(dir.exists(FolderUtils)==FALSE){
    dir.create(FolderUtils)
  }

  FolderDatasets = paste(FolderResults, "/Datasets", sep="")
  if(dir.exists(FolderDatasets)==FALSE){
    dir.create(FolderDatasets)
  }

  FolderDS = paste(FolderDatasets, "/", dataset_name, sep="")
  if(dir.exists(FolderDS)==FALSE){
    dir.create(FolderDS)
  }

  FolderCV = paste(FolderDS, "/CrossValidation", sep="")
  if(dir.exists(FolderCV)==FALSE){
    dir.create(FolderCV)
  }

  FolderTR = paste(FolderCV, "/Tr", sep="")
  if(dir.exists(FolderTR)==FALSE){
    dir.create(FolderTR)
  }

  FolderTS = paste(FolderCV, "/Ts", sep="")
  if(dir.exists(FolderTS)==FALSE){
    dir.create(FolderTS)
  }

  FolderVL = paste(FolderCV, "/Vl", sep="")
  if(dir.exists(FolderVL)==FALSE){
    dir.create(FolderVL)
  }

  FolderLS = paste(FolderDS, "/LabelSpace", sep="")
  if(dir.exists(FolderLS)==FALSE){
    dir.create(FolderLS)
  }

  FolderNamesLabels = paste(FolderDS, "/NamesLabels", sep="")
  if(dir.exists(FolderNamesLabels)==FALSE){
    dir.create(FolderNamesLabels)
  }

  FolderDataFrame = paste(FolderResults, "/DataFrames", sep="")
  if(dir.exists(FolderDataFrame)==FALSE){
    dir.create(FolderDataFrame)
  }

  FolderPartitions = paste(FolderResults, "/Partitions", sep="")
  if(dir.exists(FolderPartitions)==FALSE){
    dir.create(FolderPartitions)
  }

  FolderCommunities = paste(FolderResults, "/Comunidades", sep="")
  if(dir.exists(FolderCommunities)==FALSE){
    dir.create(FolderCommunities)
  }

  retorno$FolderLS = FolderLS
  retorno$FolderNamesLabels = FolderNamesLabels
  retorno$FolderResults = FolderResults
  retorno$FolderScripts = FolderScripts
  retorno$FolderUtils = FolderUtils
  retorno$FolderDatasets = FolderDatasets
  retorno$FolderDS = FolderDS
  retorno$FolderCV = FolderCV
  retorno$FolderTR = FolderTR
  retorno$FolderTS = FolderTS
  retorno$FolderVL = FolderVL
  retorno$FolderDataFrame = FolderDataFrame
  retorno$FolderCommunities = FolderCommunities
  retorno$FolderPartitions = FolderPartitions

  return(retorno)

  gc()

}



################################################################################################
# FUNCTION INFO DATA SET                                                                       #
#  Objective                                                                                   #
#     Gets the information that is in the "datasets-hpmlk.csv" file.                           #
#  Parameters                                                                                  #
#     dataset: the specific dataset                                                            #
#  Return                                                                                      #
#     Everything in the "datasets-hpmlk.csv" file.                                             #
################################################################################################
infoDataSet <- function(dataset){

  retorno = list()

  retorno$id = dataset$ID
  retorno$name = dataset$Name
  retorno$instances = dataset$Instances
  retorno$inputs = dataset$Inputs
  retorno$labels = dataset$Labels
  retorno$LabelsSets = dataset$LabelsSets
  retorno$single = dataset$Single
  retorno$maxfreq = dataset$MaxFreq
  retorno$card = dataset$Card
  retorno$dens = dataset$Dens
  retorno$mean = dataset$Mean
  retorno$scumble = dataset$Scumble
  retorno$tcs = dataset$TCS
  retorno$attStart = dataset$AttStart
  retorno$attEnd = dataset$AttEnd
  retorno$labStart = dataset$LabelStart
  retorno$labEnd = dataset$LabelEnd
  retorno$distinct = dataset$Distinct
  retorno$xn = dataset$xn
  retorno$yn = dataset$yn
  retorno$gridn = dataset$gridn

  return(retorno)

  gc()
}
