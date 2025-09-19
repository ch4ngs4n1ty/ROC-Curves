#TRAFFIC_STATIONS_2251 -> TrafficStation_2251_0# -> Data_0#.csv

main_folder <- "TRAFFIC_STATIONS_2251" #Initial main folder containing subfolders with csv files

#R function list.files() lists all files in a directory.
#Finds files that include subtext of Data_##.csv and use recursive to look all of the subfolders
csvs <- list.files(main_folder, pattern = "Data_.*\\.csv", recursive = TRUE, full.names = TRUE)

dfs <- lapply(csvs, read.csv) #lapply() applies function for each element of vectors and returns whole list of subfolders

data_all <- do.call(rbind, dfs) #Combine all data frames into one data frame

#print(data_all) #Print the combined data frame

#3. Plan your ROC calculations
#Fix your ground truth: is_aggressive stays as-is.
#Create a loop or sapply() over thresholds 45–85.
#For each threshold:
#Predict aggressive if speed_mph >= threshold.
#Compare with is_aggressive to get TP/FP/TN/FN.
#Compute FPR and TPR.

thresholds <- 45:85 #Define thresholds from 45 to 85

all_speeds <- data_all$SPEED #Extract speed_mph column from the combined data frame

#Intent Aggresive Level = {1,2}
#Intent Non-Aggresive Level = {0}
data_all$is_aggresive <- ifelse(data_all$INTENT %in% c(1, 2), 1, 0)

truth <- data_all$is_aggresive

TPR = numeric(length(thresholds)) #True Positive Rate of (Actual(True), Suspected (True))
FPR = numeric(length(thresholds)) #False Positive Rate of (Actual (True), Suspected (False))
mistakes = integer(length(thresholds)) 

for (i in seq_along(thresholds)) {

    t <- thresholds[i] #Get the current threshold

    predicted <- ifelse(all_speeds >= t, 1, 0) #Logical vector of aggresive = 1, and non-aggresive = 0

    TP = sum(predicted == 1 & truth == 1)
    FP = sum(predicted == 1 & truth == 0)
    TN = sum(predicted == 0 & truth == 0)
    FN = sum(predicted == 0 & truth == 1)

    tpr_val = (TP / (TP + FN))

    fpr_val = (FP / (FP + TN))

    if (tpr_val > 0) {

        TPR[i] = tpr_val

    } else {

        TPR[i] = 0

    }

    if (fpr_val > 0) {

        FPR[i] = fpr_val

    } else {

        FPR[i] = 0

    }

    mistakes[i] = FP + FN
    
}

plot(FPR, TPR, 

    main = "Chart"

)
