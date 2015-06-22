library(shiny)
library(ggplot2)
library(miscTools)
source("data_read.R")

ls_source1<-c("/Users/sdurinck/src/R/LINCStest/LI4V01105_A01_04092015_MCF7_midsection_PBS_rescan1_singlecelldata_Main.txt", 
              "/Users/sdurinck/src/R/LINCStest/LI4V01105_A01_04102015_MCF7_midsection_PBS_rescan2_singlecelldata_Main.txt",
              "/Users/sdurinck/src/R/LINCStest/LI4V01105_A01_04132015_MCF7_midsection_PBS_rescan3_singlecelldata_Main.txt")
ls_source2<-c("/Users/sdurinck/src/R/LINCStest/LI4V01105_A02_04092015_MCF7_midsection_FGF_rescan1_singlecelldata_Main.txt",
              "/Users/sdurinck/src/R/LINCStest/LI4V01105_A02_04102015_MCF7_midsection_FGF_rescan2_singlecelldata_Main.txt",
              "/Users/sdurinck/src/R/LINCStest/LI4V01105_A02_04132015_MCF7_midsection_FGF_rescan3_singlecelldata_Main.txt")
ls_source3<-c("/Users/sdurinck/src/R/LINCStest/LI4V01105_A03_04092015_MCF7_midsection_PBS_rescan1_singlecelldata_Main.txt",
              "/Users/sdurinck/src/R/LINCStest/LI4V01105_A03_04102015_MCF7_midsection_PBS_rescan2_singlecelldata_Main.txt",
              "/Users/sdurinck/src/R/LINCStest/LI4V01105_A03_04132015_MCF7_midsection_PBS_rescan3_singlecelldata_Main.txt")
ls_source4<-c("/Users/sdurinck/src/R/LINCStest/LI4V01105_A04_04092015_MCF7_midsection_10FBS_rescan1_welldata_Main.txt",
              "/Users/sdurinck/src/R/LINCStest/LI4V01105_A04_04102015_MCF7_midsection_10FBS_rescan2_singlecelldata_Main.txt",
              "/Users/sdurinck/src/R/LINCStest/LI4V01105_A04_04132015_MCF7_midsection_10FBS_rescan3_singlecelldata_Main.txt")
ln_time<-c(0,23,91)
l_yvalue<-c("Total.Intensity.DAPI","Mean.Intensity.DAPI","Mean.Intensity.Alexa.488","Mean.Intensity.Alexa.647","Mean.Intensity.Alexa.555")

A01<-read_Data_table(ls_source1,ln_time)
A02<-read_Data_table(ls_source2,ln_time)
A03<-read_Data_table(ls_source3,ln_time)
A04<-read_Data_table(ls_source4,ln_time)
A0x<-rbind(A01,A02,A03,A04)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
     output$plot1<-renderPlot({
          output_Plot_single(A0x,input$xvalue,input$yvalue,input$graph_type)
     })
})

