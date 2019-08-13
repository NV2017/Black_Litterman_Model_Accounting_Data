rm(list=ls())
setwd("~/NISM/3 Portfolio Optimization/BL Project")

libraries_required = c('BLModel','treemap')

for(i in seq(libraries_required))
{
  if(!(libraries_required[i] %in% rownames(installed.packages())))
  {
    try(expr = install.packages('libraries_required[i]'), silent = T)
  }
  try(expr = library(libraries_required[i], character.only = T), silent = T)
}

Bank_Raw_Data <- read.csv('Bank Data.csv', header = T)

colnames(Bank_Raw_Data) <- c("KotakBankDate","KotakBankClose",
                             "SBIBankDate","SBIBankClose",
                             "AxisBankDate","AxisBankClose",
                             "HDFCBankDate","HDFCBankClose")

head(Bank_Raw_Data)

Kotak_Bank <- Bank_Raw_Data[,1:2]

SBI_Bank <- Bank_Raw_Data[,3:4]

Axis_Bank <- Bank_Raw_Data[,5:6]

HDFC_Bank <- Bank_Raw_Data[,7:8]


Kotak_Bank[,1] <- as.Date(Kotak_Bank[,1], format="%d/%m/%Y")
colnames(Kotak_Bank)[1] <- "Date"

SBI_Bank[,1] <- as.Date(SBI_Bank[,1], format="%d/%m/%Y")
colnames(SBI_Bank)[1] <- "Date"

Axis_Bank[,1] <- as.Date(Axis_Bank[,1], format="%Y-%m-%d")
colnames(Axis_Bank)[1] <- "Date"

HDFC_Bank[,1] <- as.Date(HDFC_Bank[,1], format="%d/%m/%Y")
colnames(HDFC_Bank)[1] <- "Date"

List_of_Individuals <- list(Kotak_Bank, SBI_Bank, Axis_Bank, HDFC_Bank)

Combined_data <- List_of_Individuals[[1]]

for(i in 2:length(List_of_Individuals)) 
{
  Combined_data <- merge(Combined_data, List_of_Individuals[[i]],  by= "Date")
}

head(Combined_data)

Returns_Data <- data.frame(na.omit(cbind( as.character(as.Date(Combined_data[,1])), 
                               c(NA,diff(log(Combined_data[,2]))),
                               c(NA,diff(log(Combined_data[,3]))),
                               c(NA,diff(log(Combined_data[,4]))),
                               c(NA,diff(log(Combined_data[,5]))) )),
                           stringsAsFactors = F)

colnames(Returns_Data) <- c("Date","Kotak_Bank_Returns","SBI_Bank_Returns",
                            "Axis_Bank_Returns","HDFC_Bank_Returns")

head(Returns_Data)

Kotak_Bank_Q_Rep <- read.csv('Kotak Mahindra Bank Q_Report_Time_Series_2019_07_13_17_40_58.csv', header = T)

SBI_Bank_Q_Rep <- read.csv('State Bank of India Q_Report_Time_Series_2019_07_13_17_41_01.csv', header = T)

Axis_Bank_Q_Rep <- read.csv('Axis Bank Q_Report_Time_Series_2019_07_13_17_21_49.csv', header = T)

HDFC_Bank_Q_Rep <- read.csv('HDFC Bank Q_Report_Time_Series_2019_07_13_17_21_53.csv', header = T)

Kotak_Bank_ROI <- scale(na.omit(Kotak_Bank_Q_Rep$Return.on.Assets..))
SBI_Bank_ROI <- scale(na.omit(SBI_Bank_Q_Rep$Return.on.Assets..))
Axis_Bank_ROI <- scale(na.omit(Axis_Bank_Q_Rep$Return.on.Assets..))
HDFC_Bank_ROI <- scale(na.omit(HDFC_Bank_Q_Rep$Return.on.Assets..))

Kotak_Bank_PL <- scale(na.omit(Kotak_Bank_Q_Rep$P.L.After.Tax.from.Ordinary.Activities))
SBI_Bank_PL <- scale(na.omit(SBI_Bank_Q_Rep$P.L.After.Tax.from.Ordinary.Activities))
Axis_Bank_PL <- scale(na.omit(Axis_Bank_Q_Rep$P.L.After.Tax.from.Ordinary.Activities))
HDFC_Bank_PL <- scale(na.omit(HDFC_Bank_Q_Rep$P.L.After.Tax.from.Ordinary.Activities))

Kotak_Bank_Outlook <- (Kotak_Bank_ROI[1] - mean(Kotak_Bank_ROI[1:4]) + Kotak_Bank_PL[1] - mean(Kotak_Bank_PL[1:4]))/2
SBI_Bank_Outlook <- (SBI_Bank_ROI[1] - mean(SBI_Bank_ROI[1:4]) + SBI_Bank_PL[1] - mean(SBI_Bank_PL[1:4]))/2
Axis_Bank_Outlook <- (Axis_Bank_ROI[1] - mean(Axis_Bank_ROI[1:4]) + Axis_Bank_PL[1] - mean(Axis_Bank_PL[1:4]))/2
HDFC_Bank_Outlook <- (HDFC_Bank_ROI[1] - mean(HDFC_Bank_ROI[1:4]) + HDFC_Bank_PL[1] - mean(HDFC_Bank_PL[1:4]))/2

dat <- cbind(Combined_data, matrix(1/dim(Combined_data)[1], dim(Combined_data)[1], 1) )

returns_freq <- 250 # Daily

SR <- 0.5

Pe <- diag(4)

qe <- c(Kotak_Bank_Outlook, SBI_Bank_Outlook, Axis_Bank_Outlook, HDFC_Bank_Outlook)
qe <- (qe+1)^(1/62)-1 # Converting to daily

tau <- 0.02

BL_Model <- BL_post_distr(dat = dat[,-1], returns_freq = returns_freq, prior_type = NULL, 
              market_portfolio = rep(1/4,4), SR = SR, P = Pe, q = 5*qe, tau = tau, 
              risk = "MAD", alpha = 0, views_distr = observ_normal, "diag", cov_matrix = NULL)

df_tree <- data.frame(Asset_Names = c('Kotak','SBI','Axis','HDFC'),
   Asset_Value = unlist(abs(head(BL_Model$post_distr,1)[,1:4])),stringsAsFactors = F)

treemap(dtf = df_tree, index = 'Asset_Names', vSize = 'Asset_Value',
        fontsize.title = 20, fontsize.labels = c(16, 16), 
        palette = 'Purples', title = 'Portfolio Allocation')
