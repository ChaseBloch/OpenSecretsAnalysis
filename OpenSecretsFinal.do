clear all

cd "C:\Users\csb257\Documents\GitHub\OpenSecretsAnalysis\"

import delimited "C:\Users\csb257\Documents\GitHub\OpenSecretsAnalysis\2X2Data_Final.csv"


***Creating Tables for Iran***

drop if adversary == 0