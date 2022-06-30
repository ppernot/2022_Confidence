figDir = '../Figs'
library(ErrViewLib)
gPars = ErrViewLib::setgPars(type = 'publish')
scalePoints = 0.2


# Fig_01 ####
D = read.table('../Data/SYNT01_data.csv',
               sep = ",", header = TRUE,
               check.names = FALSE,
               stringsAsFactors = FALSE)
E = D$E
uE = D$uE
Z = E/uE
vZ = ErrViewLib::varZCI(Z,method = "cho")

png(file = paste0(figDir,'/Fig_01a.png'),
    width = gPars$reso, height = gPars$reso)
ErrViewLib::plotConfidence(
  E, uE,
  oracle = TRUE,
  probref = TRUE,
  conf_probref = TRUE,
  dist_probref = 'Normal',
  rep_probref = 150,
  label = 1,
  title = paste0('SYNT01; Var(Z) = ',
                 ErrViewLib::prettyUnc(vZ$mean, vZ$sd, numDig = 1)),
  gPars = gPars
  )
dev.off()

D = read.table('../Data/SYNT02_data.csv',
               sep = ",", header = TRUE,
               check.names = FALSE,
               stringsAsFactors = FALSE)
E = D$E
uE = D$uE
Z = E/uE
vZ = ErrViewLib::varZCI(Z,method = "cho")

png(file = paste0(figDir,'/Fig_01b.png'),
    width = gPars$reso, height = gPars$reso)
ErrViewLib::plotConfidence(
  E, uE,
  ylim = c(0,1.1),
  oracle = TRUE,
  probref = TRUE,
  conf_probref = TRUE,
  dist_probref = 'Normal',
  rep_probref = 150,
  label = 2,
  title = paste0('SYNT02; Var(Z) = ',
                 ErrViewLib::prettyUnc(vZ$mean, vZ$sd, numDig = 1)),
  gPars = gPars
)
dev.off()

# Fig_02 ####
D = read.table('../Data/SYNT01_data.csv',
               sep = ",", header = TRUE,
               check.names = FALSE,
               stringsAsFactors = FALSE)
E = D$E
uE = D$uE

ftab = c('Uniform','Normp4','Laplace','T4')

for (i in seq_along(ftab)) {
  png(file = paste0(figDir,'/Fig_02',letters[i],'.png'),
      width = gPars$reso, height = gPars$reso)
  ErrViewLib::plotConfidence(
    E, uE,
    oracle = FALSE,
    probref = TRUE,
    conf_probref = TRUE,
    dist_probref = ftab[i],
    rep_probref = 150,
    label = i,
    title = paste0('SYNT01; Dist = ',ftab[i]),
    gPars = gPars
  )
  dev.off()
}

# Fig_04 ####
D = read.table('../Data/SYNT01_data.csv',
               sep = ",", header = TRUE,
               check.names = FALSE,
               stringsAsFactors = FALSE)
E = D$E
uE = D$uE

label = 1
png(file = paste0(figDir,'/Fig_04',letters[label],'.png'),
    width = gPars$reso, height = gPars$reso)
ErrViewLib::plotConfidence(
  E, uE,
  stat = sd,
  oracle = FALSE,
  probref = TRUE,
  conf_probref = TRUE,
  dist_probref = 'Normal',
  rep_probref = 150,
  ylab = paste0('RMSD / RMSD0'),
  label = label,
  title = paste0('SYNT01'),
  gPars = gPars
)
dev.off()

label = label + 1
png(file = paste0(figDir,'/Fig_04',letters[label],'.png'),
    width = gPars$reso, height = gPars$reso)
ErrViewLib::plotConfidence(
  E, uE,
  stat = ErrViewLib::q95hd,
  oracle = FALSE,
  probref = TRUE,
  conf_probref = TRUE,
  dist_probref = 'Normal',
  rep_probref = 150,
  ylab = paste0('Q95 / Q950'),
  label = label,
  title = paste0('SYNT01'),
  gPars = gPars
)
dev.off()

# Fig_03 ####

label = 0

## PRO2022 ####
label = label +1
D = read.table('../Data/PRO2022_Data.csv',
               sep = ",", header = TRUE,
               check.names = FALSE,
               stringsAsFactors = FALSE)

R  = D[,1]
C  = D[,2]
uE = D[,3]/1.96
E  = R-C
Z = E/uE
vZ = ErrViewLib::varZCI(Z,method = "cho")

png(file = paste0(figDir,'/Fig_03',letters[label],'.png'),
    width = gPars$reso, height = gPars$reso)
ErrViewLib::plotConfidence(
  E, uE,
  # ylim = c(0,1.1),
  oracle = TRUE,
  probref = TRUE,
  conf_probref = TRUE,
  dist_probref = 'Normal',
  rep_probref = 150,
  label = label,
  title = paste0('PRO2022_a; Var(Z) = ',
                 ErrViewLib::prettyUnc(vZ$mean, vZ$sd, numDig = 1)),
  gPars = gPars
)
dev.off()

label = label +1
uE = D[,4]/1.96
Z = E/uE
vZ = ErrViewLib::varZCI(Z,method = "cho")

png(file = paste0(figDir,'/Fig_03',letters[label],'.png'),
    width = gPars$reso, height = gPars$reso)
ErrViewLib::plotConfidence(
  E, uE,
  # ylim = c(0,1.1),
  oracle = TRUE,
  probref = TRUE,
  conf_probref = TRUE,
  dist_probref = 'Normal',
  rep_probref = 150,
  label = label,
  title = paste0('PRO2022_b; Var(Z) = ',
                 ErrViewLib::prettyUnc(vZ$mean, vZ$sd, numDig = 1)),
  gPars = gPars
)
dev.off()

## ZHE2022 ####
label = label + 1
D = read.table('../Data/ZHE2022_AIQM1.csv',
               sep = ",", header = TRUE,
               check.names = FALSE,
               stringsAsFactors = FALSE)
N = 8 # sampling size

E  = D[,2]
uE = D[,3] #/ sqrt(N)
Z  = E/uE
vZ = ErrViewLib::varZCI(Z,method = "cho")

png(file = paste0(figDir,'/Fig_03',letters[label],'.png'),
    width = gPars$reso, height = gPars$reso)
ErrViewLib::plotConfidence(
  E, uE,
  # ylim = c(0,1.1),
  oracle = TRUE,
  probref = TRUE,
  conf_probref = TRUE,
  dist_probref = 'Normal',
  rep_probref = 150,
  legLoc = 'topright',
  label = label,
  title = paste0('ZHE2022_AIQM1; Var(Z) = ',
                 ErrViewLib::prettyUnc(vZ$mean, vZ$sd, numDig = 1)),
  gPars = gPars
)
dev.off()

label = label + 1
D = read.table('../Data/ZHE2022_ANI-1ccx.csv',
               sep = ",", header = TRUE,
               check.names = FALSE,
               stringsAsFactors = FALSE)

E  = D[,2]
uE = D[,3]  #/ sqrt(N)
Z  = E/uE

vZ = ErrViewLib::varZCI(Z,method = "cho")

png(file = paste0(figDir,'/Fig_03',letters[label],'.png'),
    width = gPars$reso, height = gPars$reso)
ErrViewLib::plotConfidence(
  E, uE,
  # ylim = c(0,1.1),
  oracle = TRUE,
  probref = TRUE,
  conf_probref = TRUE,
  dist_probref = 'Normal',
  rep_probref = 150,
  legLoc = 'topright',
  label = label,
  title = paste0('ZHE2022_ANI-1ccx; Var(Z) = ',
                 ErrViewLib::prettyUnc(vZ$mean, vZ$sd, numDig = 1)),
  gPars = gPars
)
dev.off()
