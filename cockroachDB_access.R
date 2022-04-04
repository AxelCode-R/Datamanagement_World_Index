

#cockroach sql --url "postgresql://rothaxel:5vgEzpmRj6BqhEsR71NlrA@free-tier13.aws-eu-central-1.cockroachlabs.cloud:26257/defaultdb?sslmode=verify-full&options=--cluster%3Dresearch-index-db-750"

#curl https://binaries.cockroachdb.com/cockroach-v21.2.5.darwin-10.9-amd64.tgz | tar -xz; sudo cp -i cockroach-v21.2.5.darwin-10.9-amd64/cockroach /usr/local/bin/

#$ErrorActionPreference = "Stop"; [Net.ServicePointManager]::SecurityProtocol = [Net.SecurityProtocolType]::Tls12; $ProgressPreference = 'SilentlyContinue'; $null = New-Item -Type Directory -Force $env:appdata/cockroach; Invoke-WebRequest -Uri https://binaries.cockroachdb.com/cockroach-v21.2.5.windows-6.2-amd64.zip -OutFile cockroach.zip; Expand-Archive -Force -Path cockroach.zip; Copy-Item -Force cockroach/cockroach-v21.2.5.windows-6.2-amd64/cockroach.exe -Destination $env:appdata/cockroach; $Env:PATH += ";$env:appdata/cockroach"; # We recommend adding ";$env:appdata/cockroach" to the Path variable for your system environment. See https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_environment_variables#saving-changes-to-environment-variables for more information.
# mkdir -p $env:appdata\.postgresql\; Invoke-WebRequest -Uri https://cockroachlabs.cloud/clusters/a23c6fd8-b1af-445f-a033-e2c011337ae1/cert -OutFile $env:appdata\.postgresql\root.crt



# username =rothaxel
# 
# host =free-tier13.aws-eu-central-1.cockroachlabs.cloud
# 
# port =26257
# 
# database =research-index-db-750.defaultdb
# 
# password =5vgEzpmRj6BqhEsR71NlrA

# Workaround
install.packages("remotes")
# will prompt for additional packages to be upgraded, ignoring..
remotes::install_github("r-dbi/RPostgres", upgrade = c("never"))
install.packages("RPostgres")


library(DBI)
library(RPostgres)
# Connect to the default CockroachDB instance and MovR database
conn <- dbConnect(RPostgres::Postgres(), dbname = "research-index-db-750.defaultdb",
                 host = "free-tier13.aws-eu-central-1.cockroachlabs.cloud", port = 26257,
                 user = "rothaxel", password = "5vgEzpmRj6BqhEsR71NlrA")


driver <- JDBC(driverClass = "cdata.jdbc.cockroachdb.CockroachDBDriver", classPath = "MyInstallationDir\lib\cdata.jdbc.cockroachdb.jar", identifier.quote = "'") 




library(RODBC)
library(checkpoint)
checkpoint("2016-01-01")
conn <- odbcConnect("CData CockroachDB Source")



library(RJDBC)
driver <- JDBC(driverClass = "cdata.jdbc.cockroachdb.CockroachDBDriver", classPath = "MyInstallationDir\lib\cdata.jdbc.cockroachdb.jar", identifier.quote = "'")

library(googledrive)
#C:/Users/Axel/AppData/Local/gargle/gargle/Cache

drive_find(n_max = 30)


# 1wbInf7A5QLSGMFiu2LITL60mtJ3De-Cm 
drive_download("https://drive.google.com/file/d/1wbInf7A5QLSGMFiu2LITL60mtJ3De-Cm/view?usp=sharing", path = "test2.rdata", overwrite = TRUE)


download.file("https://drive.google.com/file/d/1wbInf7A5QLSGMFiu2LITL60mtJ3De-Cm/view?usp=sharing", destfile = "test2.rdata", method = "wget", mode = "wb")

drive_download("https://drive.google.com/file/d/1wbInf7A5QLSGMFiu2LITL60mtJ3De-Cm/view?usp=sharing", path = "test2.rdata", overwrite = TRUE)

