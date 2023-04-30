

##################################################################################################
# Configures the workspace according to the operating system                                     #
##################################################################################################
FolderRoot = "~/Generate-Partitions-Communities"
FolderScripts = "~/Generate-Partitions-Communities/R"



##################################################################################################
# COMMUNITIES -------------------------------------------------------------
#' COMMUNITIES
#'
#' @family
#' @param graph_builted
#' @param title
#' @param fold
#' @param FolderSplit
#' @return
#'
#' @references
#'
#' @export
#'
#' @examples
#'
communities <- function(ds, dataset_name,
                        number_dataset,
                        number_folds,
                        number_cores,
                        similarity,
                        folderResults){
  f = 1
  cmParalel <- foreach(f = 1:number_folds) %dopar% {
  # while(f <= number_folds){

    cat("\n----------->Fold ", f)

    conected = c(0)
    measure = similarity
    knn = c(0)
    trh = c(0)
    fold = f
    resume_knn = data.frame(dataset_name, fold, knn, measure, conected)
    resume_trh = data.frame(dataset_name, fold, trh, measure, conected)

    ##################################################################################################
    # Configures the workspace according to the operating system                                     #
    ##################################################################################################
    FolderRoot = "~/Generate-Partitions-Communities"
    FolderScripts = "~/Generate-Partitions-Communities/R"

    ###############################################################################
    # Load sources                                                                #
    ###############################################################################
    cat("\nCarregando os sources\n")
    setwd(FolderScripts)
    source("libraries.R")

    setwd(FolderScripts)
    source("utils.R")

    setwd(FolderScripts)
    source("functions.R")

    ####################################################################
    cat("\nAbrindo o arquivo datasets\n")
    setwd(FolderRoot)
    datasets <- data.frame(read.csv("datasets-original.csv"))
    ds = datasets[number_dataset,]
    info = infoDataSet(ds)
    dataset_name = toString(ds$Name)


    ###############################################################################
    cat("\ncriando os diretórios: ")
    folder = createDirs2(dataset_name, folderResults)

    cat("\nCriando o FOLDER split: ")
    FolderSplit = paste(folder$FolderCommunities, "/Split-", f ,sep="")
    if(dir.exists(FolderSplit)==FALSE){dir.create(FolderSplit)}


    ###############################################################################
    cat("\nAcessando o folder do data frame")
    # FolderDF = paste(folder$FolderDataFrame, "/", dataset_name,
    #                 "/Split-", f, "/", similarity, sep="")

    FolderDF = paste(folder$FolderDataFrame, "/", dataset_name,
                     "/Split-", f, sep="")

    setwd(FolderDF)
    knn_values = read.csv("sparsification-knn-values.csv")
    totalKNN = nrow(knn_values)

    i = 1
    while(i<=totalKNN){

      cat("\n\tKNN: ", i, "\n")

      df = createDF()

      FolderKnn = paste(FolderSplit, "/knn-", i ,sep="")
      if(dir.exists(FolderKnn)==FALSE){dir.create(FolderKnn)}

      setwd(FolderDF)
      grafo = data.frame(read.csv(paste(dataset_name, "-knn-", i,
                                        ".csv",sep="")))
      grafo = na.omit(grafo)
      grafo_knn = graph_from_data_frame(grafo, directed=F)
      title = paste("knn-", i, sep="")
      fold = f

      if(is.connected(grafo_knn)==TRUE){

        cat("\n===========>>>> CONECTADO")
        conected = 1
        measure = similarity
        knn = i
        resume_knn = rbind(resume_knn, data.frame(dataset_name, fold, knn, measure, conected))

        timeSG = system.time(resSG <- executeSpinGlass(ds,grafo_knn, title, fold, FolderKnn))
        timeEB = system.time(resEB <- executeEdgeBetweenness(ds,grafo_knn, title, fold, FolderKnn))
        timeLP = system.time(resLP <- executeLabelPropagation(ds,grafo_knn, title, fold, FolderKnn))
        timeWT = system.time(resWT <- executeWalkTrap(ds,grafo_knn, title, fold, FolderKnn))
        #timeLE = system.time(resLE <- executeLeadingEigenVector(grafo_knn, title, fold, FolderKnn))
        #timeOP = system.time(resOP <- executeOptimal(ds,grafo_knn, title, fold, FolderKnn))
        timeLV = system.time(resLV <- executeLouvain(ds,grafo_knn, title, fold, FolderKnn))
        timeFG = system.time(resFG <- executeFastGreedy(ds,grafo_knn, title, fold, FolderKnn))
        timeIM = system.time(resIM <- executeInfoMap(ds,grafo_knn, title, fold, FolderKnn))
        timeLD = system.time(resLD <- executeLeiden(ds,grafo_knn, title, fold, FolderKnn))

        setwd(FolderKnn)
        runtime_communities = rbind(timeSG, timeEB, timeLP, timeWT, timeLV, timeFG, timeIM, timeLD)
        write.csv(runtime_communities, paste(similarity, "-runtime-comm-knn-fold-", f , "-knn-", i, ".csv", sep=""), row.names = FALSE)

        df$communities_final = rbind(df$communities_final, resSG$communities)
        df$communities_final = rbind(df$communities_final, resEB$communities)
        df$communities_final = rbind(df$communities_final, resLP$communities)
        df$communities_final = rbind(df$communities_final, resWT$communities)
        #df$communities_final = rbind(df$communities_final, resLE$communities)
        #df$communities_final = rbind(df$communities_final, resOP$communities)
        df$communities_final = rbind(df$communities_final, resLV$communities)
        df$communities_final = rbind(df$communities_final, resFG$communities)
        df$communities_final = rbind(df$communities_final, resIM$communities)
        df$communities_final = rbind(df$communities_final, resLD$communities)

        df$infoComm_final = rbind(df$infoComm_final, resSG$infoComm)
        df$infoComm_final = rbind(df$infoComm_final, resEB$infoComm)
        df$infoComm_final = rbind(df$infoComm_final, resLP$infoComm)
        df$infoComm_final = rbind(df$infoComm_final, resWT$infoComm)
        #df$infoComm_final = rbind(df$infoComm_final, resLE$infoComm)
        #df$infoComm_final = rbind(df$infoComm_final, resOP$infoComm)
        df$infoComm_final = rbind(df$infoComm_final, resLV$infoComm)
        df$infoComm_final = rbind(df$infoComm_final, resFG$infoComm)
        df$infoComm_final = rbind(df$infoComm_final, resIM$infoComm)
        df$infoComm_final = rbind(df$infoComm_final, resLD$infoComm)

        setwd(FolderKnn)
        infoComm_knn = data.frame(filter(df$infoComm_final, split!=0))
        communities_knn = data.frame(filter(df$communities_final, split!=0))

        write.csv(infoComm_knn,
                  paste("split-", f, "-knn-", i ,"-info.csv", sep=""),
                  row.names = FALSE)

        write.csv(communities_knn,
                  paste("split-", f, "-knn-", i, "-comm.csv", sep=""),
                  row.names = FALSE)

      } else {
        cat("\n===========>>>> NÃO CONECTADO")
        conected = 0
        measure = similarity
        knn = i
        resume_knn = rbind(resume_knn, data.frame(dataset_name,
                                                  fold, knn, measure, conected))
      }

      i = i + 1
      gc()
    }

    setwd(FolderSplit)
    resume_knn = resume_knn[-1,]
    write.csv(resume_knn, paste("fold-", f, "-",
                                similarity, "-conectado-knn.csv", sep=""),
              row.names = FALSE)


    ###############################################################################
    setwd(FolderDF)
    threshold_values = read.csv("sparsification-tr-values.csv")
    totalTR = nrow(threshold_values)
    new_threshold = c()

    # do primeiro ao último threshold
    j = 0
    while(j<totalTR){

      cat("\n\tTHRESHOLD:\t", j, "\n")
      setwd(FolderDF)
      nomearquivo = paste(dataset_name, "-threshold-",j,".csv",sep="")
      grafo = data.frame(read.csv(nomearquivo))
      df2 = createDF()

      # LIMITE DO GRAFO!
      if((grafo[1,3]==0)&&(grafo[1,4]==0)){
        cat("\n\tsimilarity=0 and weights=0")

      } else {

          FolderTr = paste(FolderSplit, "/Tr-", j ,sep="")
          if(dir.exists(FolderTr)==FALSE){dir.create(FolderTr)}

          new_threshold[j+1] = as.numeric(threshold_values[j+1,2])

          grafo_tr = graph_from_data_frame(grafo, directed=F)
          title = paste("tr-", j, sep="")

          if(is.connected(grafo_tr)==TRUE){
            cat("\n===========>>>> CONECTADO")
            conected = 1
            measure = similarity
            trh = j
            resume_trh = rbind(resume_trh, data.frame(dataset_name, fold, trh, measure, conected))

            timeSG = system.time(resSG <- executeSpinGlass(ds,grafo_knn, title, fold, FolderTr))
            timeEB = system.time(resEB <- executeEdgeBetweenness(ds,grafo_knn, title, fold, FolderTr))
            timeLP = system.time(resLP <- executeLabelPropagation(ds,grafo_knn, title, fold, FolderTr))
            timeWT = system.time(resWT <- executeWalkTrap(ds,grafo_knn, title, fold, FolderTr))
            #timeLE = system.time(resLE <- executeLeadingEigenVector(grafo_knn, title, fold, FolderTr))
            #timeOP = system.time(resOP <- executeOptimal(ds,grafo_knn, title, fold, FolderTr))
            timeLV = system.time(resLV <- executeLouvain(ds,grafo_knn, title, fold, FolderTr))
            timeFG = system.time(resFG <- executeFastGreedy(ds,grafo_knn, title, fold, FolderTr))
            timeIM = system.time(resIM <- executeInfoMap(ds,grafo_knn, title, fold, FolderTr))
            timeLD = system.time(resLD <- executeLeiden(ds,grafo_knn, title, fold, FolderTr))

            setwd(FolderTr)
            runtime_communities = rbind(timeSG, timeEB, timeLP, timeWT, timeLV, timeFG, timeIM, timeLD)
            write.csv(runtime_communities,
                      paste(similarity, "-runtime-comm-knn-fold-", f,
                            "-tr-", j, ".csv"), sep="")

            df2$communities_final = rbind(df2$communities_final, resSG$communities)
            df2$communities_final = rbind(df2$communities_final, resEB$communities)
            df2$communities_final = rbind(df2$communities_final, resLP$communities)
            df2$communities_final = rbind(df2$communities_final, resWT$communities)
            #df2$communities_final = rbind(df2$communities_final, resLE$communities)
            #df2$communities_final = rbind(df2$communities_final, resOP$communities)
            df2$communities_final = rbind(df2$communities_final, resLV$communities)
            df2$communities_final = rbind(df2$communities_final, resFG$communities)
            df2$communities_final = rbind(df2$communities_final, resIM$communities)
            df2$communities_final = rbind(df2$communities_final, resLD$communities)

            df2$infoComm_final = rbind(df2$infoComm_final, resSG$infoComm)
            df2$infoComm_final = rbind(df2$infoComm_final, resEB$infoComm)
            df2$infoComm_final = rbind(df2$infoComm_final, resLP$infoComm)
            df2$infoComm_final = rbind(df2$infoComm_final, resWT$infoComm)
            #df2$infoComm_final = rbind(df2$infoComm_final, resLE$infoComm)
            #df2$infoComm_final = rbind(df2$infoComm_final, resOP$infoComm)
            df2$infoComm_final = rbind(df2$infoComm_final, resLV$infoComm)
            df2$infoComm_final = rbind(df2$infoComm_final, resFG$infoComm)
            df2$infoComm_final = rbind(df2$infoComm_final, resIM$infoComm)
            df2$infoComm_final = rbind(df2$infoComm_final, resLD$infoComm)

            setwd(FolderTr)
            infoComm_tr = data.frame(filter(df2$infoComm_final, split!=0))
            communities_tr = data.frame(filter(df2$communities_final, split!=0))

            write.csv(infoComm_tr,
                      paste("split-", f, "-tr-", j, "-info.csv", sep=""),
                      row.names = FALSE)

            write.csv(communities_tr,
                      paste("split-", f, "-tr-", j, "-comm.csv", sep=""),
                      row.names = FALSE)


          } else {
              cat("\n===========>>>> NÃO CONECTADO")
              conected = 0
              measure = similarity
              trh = j
              resume_trh = rbind(resume_trh, data.frame(dataset_name,
                                                        fold, trh, measure,
                                                        conected))
          }

        } # FIM DO ELSE

      j = j + 1
      gc()

    } # fim do while

    setwd(FolderSplit)
    resume_trh = resume_trh[-1,]
    write.csv(resume_trh, paste("fold-", f, "-", similarity,
                                "-conectado-trh.csv", sep=""),
              row.names = FALSE)

    if(length(new_threshold)==0){
      setwd(FolderDF)
      new_threshold="vazio"
      write.csv(new_threshold, "sparsification-new-threshold.csv")
    } else {
      setwd(FolderDF)
      write.csv(new_threshold, "sparsification-new-threshold.csv")
    }

    #f = f + 1
    gc()

  } # end foreach
  gc()

}
