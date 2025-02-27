### run R script to perform my identity test on HSMs ###

# BP - MP
$processIDtestBPMP = Start-Process -FilePath "C:\Program Files\R\R-4.4.1\bin\x64\Rscript.exe" -ArgumentList "Z:\Isla_MSc_Ch1\GitHub_Repositories\Turcke_MSc_Ch1\R\HSM_IdentityTest.R"
Start-Sleep -Seconds 2
#$processIDtestBPMP.ProcessorAffinity = 1
