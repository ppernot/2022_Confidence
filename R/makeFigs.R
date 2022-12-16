figDir = '../Figs'
library(ErrViewLib)
gPars = ErrViewLib::setgPars(type = 'publish')
scalePoints = 0.2
normalize = TRUE
rep_probref = 1000

plotCCMult = function(
  E, uE,
  statS = 'RMSD',
  normalize = FALSE,
  label = 0,
  title = '',
  gPars) {

  if(statS == 'RMSD') {
    stat = ErrViewLib::rmsd
    ylab = 'RMSD'
    if(normalize)
      ylab = 'RMSD / RMSD0'
  } else {
    stat = ErrViewLib::mue
    ylab = 'MAE'
    if(normalize)
      ylab = 'MAE / MAE0'

  }
  ErrViewLib::plotConfidence(
    E, uE,
    normalize = normalize,
    stat = stat,
    ylab = ylab,
    oracle = FALSE,
    probref = TRUE,
    conf_probref = FALSE,
    dist_probref = 'Uniform',
    rep_probref = 1000,
    col = 1,
    label = label,
    title = title,
    showLegend = FALSE,
    gPars = gPars
  )
  col = 1
  for(dist in c('Normp4','Normal','Laplace','T4')) {
    col = col + 1
    ErrViewLib::plotConfidence(
      E, uE,
      normalize = normalize,
      stat = stat,
      oracle = FALSE,
      probref = TRUE,
      conf_probref = FALSE,
      dist_probref = dist,
      rep_probref = 1000,
      col = col,
      add = TRUE,
      label = 0,
      title = '',
      showLegend = FALSE,
      gPars = gPars
    )
  }
  legend(
    'bottomleft', bty = 'n',
    legend = c('Data', 'Uniform prob. ref.',
               'Normp4 prob. ref.','Normal prob. ref.',
               'Laplace prob. ref.','T4 prob. ref.'),
    lty = c(1,2,2,2,2,2),
    col = gPars$cols[c(5,1:5)],
    pch = NA
  )
}

# Fig_01 ####
D = read.table('../Data/SYNT01_data.csv',
               sep = ",", header = TRUE,
               check.names = FALSE,
               stringsAsFactors = FALSE)
E  = D$E
uE = D$uE

png(file = paste0(figDir,'/Fig_01a.png'),
    width = gPars$reso, height = gPars$reso)
ErrViewLib::plotCC(
  E, uE,
  oracle = TRUE,
  dfpr   = FALSE,
  label  = 1,
  title  = paste0('SYNT01'),
  gPars = gPars
  )
dev.off()

D = read.table('../Data/SYNT02_data.csv',
               sep = ",", header = TRUE,
               check.names = FALSE,
               stringsAsFactors = FALSE)
E  = D$E
uE = D$uE

png(file = paste0(figDir,'/Fig_01b.png'),
    width = gPars$reso, height = gPars$reso)
ErrViewLib::plotCC(
  E, uE,
  ylim = c(0,0.028),
  oracle = TRUE,
  label = 2,
  dfpr   = FALSE,
  title = paste0('SYNT02'),
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

i=1
png(file = paste0(figDir,'/Fig_02',letters[i],'.png'),
    width = gPars$reso, height = gPars$reso)
plotCCMult(
  E, uE, statS = 'MAE', normalize = FALSE, label = i,
  title = 'S = MAE', gPars = gPars)
dev.off()
i=i+1
png(file = paste0(figDir,'/Fig_02',letters[i],'.png'),
    width = gPars$reso, height = gPars$reso)
plotCCMult(
  E, uE, statS = 'MAE', normalize = TRUE, label = i,
  title = 'S = MAE; Normalized', gPars = gPars)
dev.off()
i=i+1
png(file = paste0(figDir,'/Fig_02',letters[i],'.png'),
    width = gPars$reso, height = gPars$reso)
plotCCMult(
  E, uE, statS = 'RMSD', normalize = FALSE, label = i,
  title = 'S = RMSE', gPars = gPars)
dev.off()
i=i+1
png(file = paste0(figDir,'/Fig_02',letters[i],'.png'),
    width = gPars$reso, height = gPars$reso)
plotCCMult(
  E, uE, statS = 'RMSD', normalize = TRUE, label = i,
  title = 'S = RMSE; Normalized', gPars = gPars)
dev.off()


# Fig_02b ####
D = read.table(
  '../Data/SYNT01_data.csv',
  sep = ",",
  header = TRUE,
  check.names = FALSE,
  stringsAsFactors = FALSE
)
E = D$E
uE = D$uE
ftab = c('Uniform', 'Normp4', 'Laplace', 'T4')
for (i in seq_along(ftab)) {
  png(
    file = paste0(figDir, '/Fig_02b', letters[i], '.png'),
    width = gPars$reso,
    height = gPars$reso
  )
  ErrViewLib::plotCC(
    E,
    uE,
    dist_probref = ftab[i],
    label = i,
    title = paste0('SYNT01; Dist = ', ftab[i]),
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
ErrViewLib::plotCC(
  E, uE,
  label = label,
  title = paste0('SYNT01'),
  gPars = gPars
)
dev.off()

label = label + 1
png(file = paste0(figDir,'/Fig_04',letters[label],'.png'),
    width = gPars$reso, height = gPars$reso)
ErrViewLib::plotCC(
  E, uE, statS = 'Q95',
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
plotCC(
  E, uE, oracle = TRUE, legLoc = 'topright',
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
plotCC(
  E, uE, oracle = TRUE,
  label = label,   legLoc = 'topright',
  title = paste0('PRO2022_b; Var(Z) = ',
                 ErrViewLib::prettyUnc(vZ$mean, vZ$sd, numDig = 1)),
  gPars = gPars
)
dev.off()

label = label +1
png(file = paste0(figDir,'/Fig_03',letters[label],'.png'),
    width = gPars$reso, height = gPars$reso)
plotCC(
  E, uE, oracle = TRUE, normalize = TRUE,
  title = 'PRO2022_b; Normalized CC',
  legLoc = 'topright', label = label, gPars = gPars
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
plotCC(
  E, uE,
  oracle = TRUE,
  label = label,
  legLoc = 'topright',
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
plotCC(
  E, uE,
  label = label,
  oracle = TRUE,
  legLoc = 'topright',
  title = paste0('ZHE2022_ANI-1ccx; Var(Z) = ',
                 ErrViewLib::prettyUnc(vZ$mean, vZ$sd, numDig = 1)),
  gPars = gPars
)
dev.off()

label = label +1
png(file = paste0(figDir,'/Fig_03',letters[label],'.png'),
    width = gPars$reso, height = gPars$reso)
plotCC(
  E, uE, oracle = TRUE, normalize = TRUE,
  title = 'ZHE2022_ANI-1ccx; Normalized CC',
  legLoc = 'topright', label = label, gPars = gPars
)
dev.off()

# PAL2022 ####

## Fig 05 ####
slide = TRUE
nBin = 20
label = 0

case = 'Diffusion_RF'
D = read.table(
  file.path('..','Data', paste0('PAL2022_',case,'.csv')),
  sep = ",", header = TRUE,
  check.names = FALSE,
  stringsAsFactors = FALSE
)

E  = D$E
uE = D$X  #/sqrt(2) # Simple recalibration
Z  = E / uE

label = label +1
png(file = paste0(figDir,'/Fig_05',letters[label],'.png'),
    width = gPars$reso, height = gPars$reso)
ErrViewLib::plotLZV(
  uE, Z,
  logX = TRUE,
  nBin = nBin,
  method = 'cho',
  slide = slide,
  xlab = 'Uncertainty',
  col = 6,
  title = paste0(case,'; Uncal.'),
  label = label,
  gPars = gPars
)
dev.off()

label = label +1
png(file = paste0(figDir,'/Fig_05',letters[label],'.png'),
    width = gPars$reso, height = gPars$reso)
plotCC(
  E, uE,
  showUk = TRUE,
  label = label,
  gPars = gPars
)
dev.off()

uE = D$uE
Z  = E / uE

label = label +1
png(file = paste0(figDir,'/Fig_05',letters[label],'.png'),
    width = gPars$reso, height = gPars$reso)
ErrViewLib::plotLZV(
  uE, Z,
  logX = TRUE,
  nBin = nBin,
  method = 'cho',
  slide = slide,
  xlab = 'Uncertainty',
  col = 6,
  title = paste0(case,'; Cal.'),
  label = label,
  gPars = gPars
)
dev.off()

label = label +1
png(file = paste0(figDir,'/Fig_05',letters[label],'.png'),
    width = gPars$reso, height = gPars$reso)
plotCC(
  E, uE,
  showUk = TRUE,
  label = label,
  gPars = gPars
)
dev.off()

## Fig 06 ####
slide = TRUE
nBin = 20
label = 0

case = 'Diffusion_LR'
D = read.table(
  file.path('..','Data', paste0('PAL2022_',case,'.csv')),
  sep = ",", header = TRUE,
  check.names = FALSE,
  stringsAsFactors = FALSE
)

E  = D$E
uE = D$uE
Z  = E / uE

label = label +1
png(file = paste0(figDir,'/Fig_06',letters[label],'.png'),
    width = gPars$reso, height = gPars$reso)
ErrViewLib::plotLZV(
  uE, Z,
  logX = TRUE,
  nBin = nBin,
  method = 'cho',
  slide = slide,
  xlab = 'Uncertainty',
  col = 6,
  title = case,
  label = label,
  gPars = gPars
)
dev.off()

label = label +1
png(file = paste0(figDir,'/Fig_06',letters[label],'.png'),
    width = gPars$reso, height = gPars$reso)
plotCC(
  E, uE,
  showUk = TRUE,
  label = label,
  gPars = gPars
)
dev.off()

case = 'Diffusion_GPR_Bayesian'
D = read.table(
  file.path('..','Data', paste0('PAL2022_',case,'.csv')),
  sep = ",", header = TRUE,
  check.names = FALSE,
  stringsAsFactors = FALSE
)

E  = D$E
uE = D$uE
Z  = E / uE

label = label +1
png(file = paste0(figDir,'/Fig_06',letters[label],'.png'),
    width = gPars$reso, height = gPars$reso)
ErrViewLib::plotLZV(
  uE, Z,
  nBin = nBin,
  logX = TRUE,
  method = 'cho',
  slide = slide,
  xlab = 'Uncertainty',
  col = 6,
  title = case,
  label = label,
  gPars = gPars
)
dev.off()

label = label +1
png(file = paste0(figDir,'/Fig_06',letters[label],'.png'),
    width = gPars$reso, height = gPars$reso)
plotCC(
  E, uE,
  showUk = TRUE,
  label = label,
  gPars = gPars
)
dev.off()

## Fig 07 ####
label = 0
logX = TRUE
xlim = c(0.005,2)

case = 'Perovskite_GPR_Bayesian'
D = read.table(
  file.path('..','Data', paste0('PAL2022_',case,'.csv')),
  sep = ",", header = TRUE,
  check.names = FALSE,
  stringsAsFactors = FALSE
)

E  = D$E
uE = D$uE
sel = uE > 0
if (sum(!sel) > 0) {
  E  = E[sel]
  uE = uE[sel]
}
Z  = E / uE

label = label +1
png(file = paste0(figDir,'/Fig_07',letters[label],'.png'),
    width = gPars$reso, height = gPars$reso)
ErrViewLib::plotLZV(
  uE, Z,
  logX = logX,
  nBin = nBin,
  method = 'cho',
  slide = slide,
  xlab = 'Uncertainty',
  xlim = xlim,
  col = 6,
  title = case,
  label = label,
  gPars = gPars
)
dev.off()

label = label +1
png(file = paste0(figDir,'/Fig_07',letters[label],'.png'),
    width = gPars$reso, height = gPars$reso)
ErrViewLib::plotLZV(
  uE, Z,
  logX = logX,
  logBin = TRUE,
  nBin = 30,
  method = 'cho',
  slide = TRUE,
  xlab = 'Uncertainty',
  xlim = xlim,
  col = 6,
  # title = case,
  label = label,
  gPars = gPars
)
dev.off()

label = label +1
png(file = paste0(figDir,'/Fig_07',letters[label],'.png'),
    width = gPars$reso, height = gPars$reso)
plotCC(
  E, uE,
  showUk = TRUE,
  label = label,
  gPars = gPars
)
dev.off()
