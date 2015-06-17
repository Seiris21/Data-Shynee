

#l_filenames <- c(
# "/Users/sdurinck/src/R/LINCStest/LI4V01105_A01_04092015_MCF7_midsection_PBS_rescan1_singlecelldata_Main.txt",
# "/Users/sdurinck/src/R/LINCStest/LI4V01105_A01_04102015_MCF7_midsection_PBS_rescan2_singlecelldata_Main.txt", 
# "/Users/sdurinck/src/R/LINCStest/LI4V01105_A01_04132015_MCF7_midsection_PBS_rescan3_singlecelldata_Main.txt", 
# "/Users/sdurinck/src/R/LINCStest/LI4V01105_A02_04092015_MCF7_midsection_FGF_rescan1_singlecelldata_Main.txt", 
# "/Users/sdurinck/src/R/LINCStest/LI4V01105_A02_04102015_MCF7_midsection_FGF_rescan2_singlecelldata_Main.txt", 
# "/Users/sdurinck/src/R/LINCStest/LI4V01105_A02_04132015_MCF7_midsection_FGF_rescan3_singlecelldata_Main.txt", 
# "/Users/sdurinck/src/R/LINCStest/LI4V01105_A03_04092015_MCF7_midsection_PBS_rescan1_singlecelldata_Main.txt", 
# "/Users/sdurinck/src/R/LINCStest/LI4V01105_A03_04102015_MCF7_midsection_PBS_rescan2_singlecelldata_Main.txt", 
# "/Users/sdurinck/src/R/LINCStest/LI4V01105_A03_04132015_MCF7_midsection_PBS_rescan3_singlecelldata_Main.txt", 
# "/Users/sdurinck/src/R/LINCStest/LI4V01105_A04_04092015_MCF7_midsection_10FBS_rescan1_welldata_Main.txt", 
# "/Users/sdurinck/src/R/LINCStest/LI4V01105_A04_04102015_MCF7_midsection_10FBS_rescan2_singlecelldata_Main.txt", 
# "/Users/sdurinck/src/R/LINCStest/LI4V01105_A04_04132015_MCF7_midsection_10FBS_rescan3_singlecelldata_Main.txt")

get_Data_dic <- function(ls_source){
     #Create loop to define files being read or can hard code
     data1 <- read.table(ls_source[[1]],header = TRUE, sep="\t")
     data2 <- read.table(ls_source[[2]],header = TRUE, sep="\t")
     data3 <- read.table(ls_source[[3]],header = TRUE, sep="\t")

     lv_dataset <- c('data1','data2','data3')

     #Create variables Column/Row
     #This should remain same between rescans because experiment must be constant
     column_length <- 1:dim(get(lv_dataset[[1]]))[2]
     row_length <- 1:dim(get(lv_dataset[[1]]))[1]

     #Set up variables for data acquisition 
     #Headers among rescans should be the same because they test the same conditions
     data_dict <- vector(mode="list",3)
     data_header <- colnames(get(lv_dataset[[1]]))
     data_list <- setNames(vector(mode="list",l_column),data_header)

     #Populate data dictionary with data
     for (i in 1:length(lv_dataset)){
          for(j in 1:column_length){
               for(k in 1:row_length){
                    data_list[[j]][k] <- append(data_list[[j]][k],get(lv_dataset[[i]])[k,j])
               }
          #Put data into dic
         data_dict[[i]] <- append(data_dict[[i]], data_list) 
          #Reset List for next iteration
         data_list <- setNames(vector(mode="list",l_column),data_header)
         }
     }
     return (data_dic)
}

read_Data_table <- function(ls_source,ln_time){
     #Create loop to define files being read or can hard code
     #Loop generates variables as well
     #ls_rescan <- vector(mode="list",0)
     lt_data <- vector(mode="list",0)
     
     for (i in 1:length(ls_source)){
          #Remove file path portion
          n<-unlist(strsplit(ls_source[i],split="/",fixed=TRUE))
          #File name will always be last value in list once split
          n<-unlist(strsplit(n[length(n)],split="_",fixed=TRUE))
          #File name in format: Barcode_Well_Date_CellLine_Section_Buffer_Rescan_Type_Main
          #rescan in format: Barcode_Well_Cellline_Buffer_rescan
          rescan<-paste(n[1],n[2],n[4],n[6],n[7],sep="_")
          #ls_rescan<-append(ls_rescan,rescan)
          assign(rescan,read.table(ls_source[i],header = TRUE, sep="\t"))
          lt_data<-append(lt_data,rescan)
          
     }
     l_colnames<-make.names(colnames(get(lt_data[[1]])))
     # Don't know how to compare all the files, because I don't know the exact 
     # # of rescans (elements in lt_data) before calling intersect
     #for (w in 1:lt_data)
     same_spot <- Reduce(intersect,list(get(lt_data[[1]])$Well,get(lt_data[[2]])$Well,get(lt_data[[3]])$Well))
     diff_spot <- Reduce(setdiff,list(get(lt_data[[1]])$Well,get(lt_data[[2]])$Well,get(lt_data[[3]])$Well))
     ln_data <- vector(mode="list",0)
     #Desired columns
     selected <- c(2,3,4,5,6,10,11)
     
     for(j in lt_data){
          for (k in same_spot){
               t_data <- subset(get(j),Well==k)
               med <- colMedians(t_data,na.rm=TRUE)
               #Pastes information based on position in selected
               #Total/Mean.Intensity, Spot(Well in file), Area in that order
               for (l in selected){
                    ln_data <- append(ln_data,med[[l]])
               }
               # If rescan name should be row column comment out and simply call lt_data for rownames
               # rescan in format: Barcode_Well_Cellline_Ligand_rescan
               # Barcode
               ln_data <- append(ln_data,unlist(strsplit(j,split="_",fixed=TRUE))[1])
               # Well (A01)
               ln_data <- append(ln_data,unlist(strsplit(j,split="_",fixed=TRUE))[2])
               # Cell line
               ln_data <- append(ln_data,unlist(strsplit(j,split="_",fixed=TRUE))[3])
               # Ligand
               ln_data <- append(ln_data,unlist(strsplit(j,split="_",fixed=TRUE))[4])
               # Rescan Name --> Rescan #
               n_rescan<-as.numeric(substr(j,nchar(j),nchar(j)))
               ln_data <- append(ln_data,n_rescan)
               #Time between Rescans. Passed as list
               if (n_rescan==1){
                    ln_data <- append(ln_data,ln_time[1])
               }
               else if (n_rescan==2){
                    ln_data <- append(ln_data,ln_time[2])
               }
               else if (n_rescan==3){
                    ln_data <- append(ln_data,ln_time[3])
               }
               else{
                    print("Rescan not specified by Time list")
                    break
               }
               
          
          #Add write.table+list clear here if rescan data should be separated
          }
     }
     m_data <- matrix(as.list(ln_data), ncol=13,byrow=TRUE)
     
     #Total/Mean Intensity, Spot(Old Well), Area, barcode, Well(A01), cell line, Ligand (PBS), Rescan(#) 
     #c(2,3,4,5,6,10,11)
     l_columns <- c(l_colnames[2],l_colnames[3],l_colnames[4],l_colnames[5],l_colnames[6],"Spot",l_colnames[11],"Barcode",l_colnames[10],"Cell Line","Ligand","Rescan","Time (Hours)")
     
     colnames(m_data) <- l_columns
     #rownames(m_data) <- lt_data
     m_data <- as.table(m_data)
     #print(m_data)
     csv_filename<-paste(m_data[1,8],m_data[1,9],m_data[1,10],m_data[1,11],".txt",sep="_")
     write.table(m_data, file=csv_filename, append=FALSE, col.names=TRUE, row.names=FALSE, sep="\t")
     t_t<-read.csv(csv_filename,header=TRUE,sep="\t")
     return(t_t)
     
}

output_PDF_single<- function(t_data,l_yvalue,PDF_name){
     pdf(file=PDF_name,onefile=TRUE)
     for (i in 1:length(l_yvalue)){
          print(ggplot(t_data,aes(x=factor(Rescan)))+geom_boxplot(aes_string(y=l_yvalue[i])))
     }
     dev.off()
     
}

output_PDF_multi<-function(t_rbind_data,l_yvalue){
     PDF_name<-paste(t_rbind_data[1,8],t_rbind_data[1,9],t_rbind_data[1,10],sep="_")
     PDF_name<-paste(PDF_name,".pdf",sep="")
     pdf(file=PDF_name,onefile=TRUE)
     for (i in 1:length(l_yvalue)){
          plot_title<-paste(l_yvalue[i],"across Recans",sep=" ")
          p<-ggplot(t_rbind_data,aes(x=factor(Rescan)))+geom_boxplot(aes_string(y=l_yvalue[i]))+xlab("Rescan")+ggtitle(plot_title)
          print(p+facet_wrap(~Well,nrow=1))
     }
     dev.off()
}
output_Plot_single<-function(t_data,yvalue){
     plot_title<-paste(yvalue,"across Recans",sep=" ")
     p<-ggplot(t_data,aes(x=factor(Rescan)))+geom_boxplot(aes_string(y=yvalue))+xlab("Rescan")+ggtitle(plot_title)
     print(p+facet_wrap(~Well,nrow=1))
}

#lapply Loop apply Program
#do.call(rbind, lapply(l_t_sources, function(l_t_sources) read_Data_table(l_t_sources)))

#Can ggplot use 2 factors to choose what data to use? example: by Rescan # and by same/diff spots
#ggplot(aes(x=Same/Diff Spot,fill=factor("Rescan")))+geom_boxplot(aes_string(y=l_yvalue[i]))+xlab("same/diff?")+ggtitle(plot_title)
#Create dataframe seperately that defines, then plot

#var<-paste("s_name",i,sep="_")
#Generate variable name with assign and fill variable with data
#Can generate with paste(), or input a list of varnames in same order as source
#Update global variables? I read it was bad R practice
#assign(ls_varname,data,envir=.GlobalEnv)

#Table with columns [Median of (Total/Mean Intensity, Spot(well),Area,Rescan#)]
#assign(paste(,,sep="_"),)
#Reduce(intersect, list(a,b,c))

#get(ls_varname[[1]])

