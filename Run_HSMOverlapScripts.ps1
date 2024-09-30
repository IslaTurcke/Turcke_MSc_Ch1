### run R scripts to calculate raster overlap metrics for HSMs ###

# BP - MP
#$processBPMP = Start-Process -FilePath "C:\Program Files\R\R-4.4.1\bin\x64\Rscript.exe" -ArgumentList "Z:\Isla_MSc_Ch1\GitHub_Repositories\Turcke_MSc_Ch1\R\Overlap_BP_MP.R" -PassThru -NoNewWindow
#Start-Sleep -Seconds 2
#$processBPMP.ProcessorAffinity = 1

# BP - RP
#$processBPRP = Start-Process -FilePath "C:\Program Files\R\R-4.4.1\bin\x64\Rscript.exe" -ArgumentList "Z:\Isla_MSc_Ch1\GitHub_Repositories\Turcke_MSc_Ch1\R\Overlap_BP_RP.R" -PassThru -NoNewWindow
#Start-Sleep -Seconds 2
#$processBPRP.ProcessorAffinity = 2

# BP - GS
#$processBPGS = Start-Process -FilePath "C:\Program Files\R\R-4.4.1\bin\x64\Rscript.exe" -ArgumentList "Z:\Isla_MSc_Ch1\GitHub_Repositories\Turcke_MSc_Ch1\R\Overlap_BP_GS.R" -PassThru -NoNewWindow
#Start-Sleep -Seconds 2
#$processBPGS.ProcessorAffinity = 4

# BP - BG
#$processBPBG = Start-Process -FilePath "C:\Program Files\R\R-4.4.1\bin\x64\Rscript.exe" -ArgumentList "Z:\Isla_MSc_Ch1\GitHub_Repositories\Turcke_MSc_Ch1\R\Overlap_BP_BG.R" -PassThru -NoNewWindow
#Start-Sleep -Seconds 2
#$processBPBG.ProcessorAffinity = 8

# # # # # 

# MP - RP
#$processMPRP = Start-Process -FilePath "C:\Program Files\R\R-4.4.1\bin\x64\Rscript.exe" -ArgumentList "Z:\Isla_MSc_Ch1\GitHub_Repositories\Turcke_MSc_Ch1\R\Overlap_MP_RP.R" -PassThru -NoNewWindow
#Start-Sleep -Seconds 2
#$processMPRP.ProcessorAffinity = 16

# MP - GS
#$processMPGS = Start-Process -FilePath "C:\Program Files\R\R-4.4.1\bin\x64\Rscript.exe" -ArgumentList "Z:\Isla_MSc_Ch1\GitHub_Repositories\Turcke_MSc_Ch1\R\Overlap_MP_GS.R" -PassThru -NoNewWindow
#Start-Sleep -Seconds 2
#$processMPGS.ProcessorAffinity = 32

# MP - BG
#$processMPBG = Start-Process -FilePath "C:\Program Files\R\R-4.4.1\bin\x64\Rscript.exe" -ArgumentList "Z:\Isla_MSc_Ch1\GitHub_Repositories\Turcke_MSc_Ch1\R\Overlap_MP_BG.R" -PassThru -NoNewWindow
#Start-Sleep -Seconds 2
#$processMPBG.ProcessorAffinity = 64

# # # # # 

# RP - GS
#$processRPGS = Start-Process -FilePath "C:\Program Files\R\R-4.4.1\bin\x64\Rscript.exe" -ArgumentList "Z:\Isla_MSc_Ch1\GitHub_Repositories\Turcke_MSc_Ch1\R\Overlap_RP_GS.R" -PassThru -NoNewWindow
#Start-Sleep -Seconds 2
#$processRPGS.ProcessorAffinity = 128

# RP - BG
#$processRPBG = Start-Process -FilePath "C:\Program Files\R\R-4.4.1\bin\x64\Rscript.exe" -ArgumentList "Z:\Isla_MSc_Ch1\GitHub_Repositories\Turcke_MSc_Ch1\R\Overlap_RP_BG.R" -PassThru -NoNewWindow
#Start-Sleep -Seconds 2
#$processRPBG.ProcessorAffinity = 256

# # # # # 

# GS - BG
$processGSBG = Start-Process -FilePath "C:\Program Files\R\R-4.4.1\bin\x64\Rscript.exe" -ArgumentList "Z:\Isla_MSc_Ch1\GitHub_Repositories\Turcke_MSc_Ch1\R\Overlap_GS_BG.R" -PassThru -NoNewWindow
Start-Sleep -Seconds 2
$processGSBG.ProcessorAffinity = 512


