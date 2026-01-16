# Best mix for depth function
#
# Arguments:
# END = Equivalent Narcotic Depth desired
# MaxppO = the maximum ppO2 desired
# MaxD = Maximum planned depth
#
# Returns:
# Best_Oxygen = Best oxygen percentage END, ppO2 and depth
# Best_Helium = Best oxygen percentage END, ppO2 and depth
# Best_Nitrogen = Best oxygen percentage END, ppO2 and depth
# MOD = Maximum Operating Depth for the returned gas mix
BestMix <- function(END, MaxppO, MaxD) {
  
  # calculate partial pressures
  ppO <- MaxppO/(MaxD/33 +1)
  ppN <- (0.79*(END/33 +1))/(MaxD/33 +1)
  ppH <- 1-ppO - ppN
  
  Best_Oxygen_Mix <- ppO*100
  Best_Helium_Mix <- ppH*100
  Best_Nitrogen_Mix <- ppN*100
  
  MOD <- ((1.4/ppO) - 1)*33
  
  Best_Mix_For_Depth <<- data.frame("Equivalent Narcotic Depth" = END,
                                    "Maximum Oxygen Partial Pressure Desired" = MaxppO,
                                    "Maximum Planned Depth" = MaxD,
                                    "Best Oxygen Percentage" = Best_Oxygen_Mix,
                                    "Best Helium Percentage" = Best_Helium_Mix,
                                    "Best Nitrogen Percentage" = Best_Nitrogen_Mix,
                                    "Maximum Operating Depth for Mix (at 1.4 ppO)" = MOD)
}
# Test run - should result in 18/40 MOD of 220' 
#BestMix(100, 1.4, 220)
#BestMix(100, 1.2, 240)


# Nitrox mixer
#
# Arguments:
# DO = Desired O2 percentage
# DP = Desired pressure
# RO = Risidual O2 in the tank
# RP = Risidual pressure in the tank
#
# Returns:
# Oxygen_Fill_Pressure = Ending pressure after topping off with pure O2
# Air_Fill_Pressure = Ending pressure after topping off with air
NitroxMix <- function(DO, DP, RO, RP) {
  pOres <- RP*RO/100
  pNres <- RP*(100 - RO)/100
  pOreq <- DP*DO/100
  pNreq <- DP*(100 - DO)/100
  dpO <- pOreq - pOres
  dpN <- pNreq - pNres
  pa <- dpN/0.79
  O <- dpO - (pa*0.21)
  
  Oxygen_Fill_Pressure <<- RP + O
  Air_Fill_Pressure <<- Oxygen_Fill_Pressure + pa
  
  Nitrox_Mix <<- data.frame('Residual Pressure' = RP,
                            'Oxygen Fill to Pressure' = Oxygen_Fill_Pressure,
                            'Air Fill to Presure' = Air_Fill_Pressure)
}
# Test run in bar - should result in 84.4 and 230
#NitroxMix(50, 230, 52, 0)

# Trimix mixer
#
# Arguments:
# DO = Desired O2 percentage
# DH = Desired H percentage
# DP = Desired pressure
# RO = Risidual O2 in the tank
# RH = Risidual H in the tank
# RP = Risidual pressure in the tank
#
# Returns:
# Helium_Fill_Pressure = Ending pressure after topping off with pure H
# Oxygen_Fill_Pressure = Ending pressure after topping off with pure O2
# Air_Fill_Pressure = Ending pressure after topping off with air
TrimixMix <- function(DO, DH, DP, RO, RH, RP) {
  pOres <- RP*RO/100
  pHres <- RP*RH/100
  pNres <- RP*(100 - RO - RH)/100
  
  pOreq <- DP*DO/100
  pHreq <- DP*DH/100
  pNreq <- DP*(100 - DO - DH)/100
  
  dpO <- pOreq - pOres
  dpH <- pHreq - pHres
  dpN <- pNreq - pNres
  
  pa <- dpN/0.79
  O <- dpO - (pa*0.21)
  
  Helium_Fill_Pressure <- RP + dpH
  Oxygen_Fill_Pressure <- dpH + O
  Air_Fill_Pressure <- Oxygen_Fill_Pressure + pa
  
  Trimix_mix <<- data.frame('Helium Fill Pressure' = Helium_Fill_Pressure,
                            'Oxygen Fill Pressure' = Oxygen_Fill_Pressure,
                            'Air Fill Pressure' = Air_Fill_Pressure)
}
# Test run in bar - should result in 57.5, 69.9 and 230
#TrimixMix(20, 25, 230, 21, 0, 0)


# Transfill calculator
#
# Arguments:
# C1p = Tank 1 pressure
# C1v = Tank 1 volume
# C2p = Tank 2 pressure
# C2v = Tank 2 volume
#
# Returns:
# Final Pressure = final pressure in the two tanks
# Note - need to expand to trimix??
TransFill <- function(C1p, C1v, C2p, C2v) {
  Final_Pressure <<- ((C1p*C1v) + (C2p*C2v))/(C1v + C2v)
}
# Testing in bar - should be 28
#TransFill(0, 27, 44, 47)

# TopOff for any gas mix
#
# Arguments:
# RO = Risidual O2 in the tank
# RH = Risidual H in the tank
# RP = Risidual pressure in the tank
# TO = O2 percentage in the top off gas
# TH = H percentage in the top off gas
# DP = Desired pressure
# MD = Maximum depth for the gas
#
# Returns:
# Helium = Percentage of H in the final mix
# Oxygen = Percentage of O2 in the final mix
# Nitrogen = Percentage of N2 in the final mix
# END = Eqiuvalent Narcotic Depth of the final mix
# ppO2 = O2 partial pressure at MD of the final mix
TopOff <- function(RO, RH, RP, TO, TH, DP, MD) {
  pOres <- RP*RO/100
  pHres <- RP*RH/100
  pNres <- RP*(100 - RO - RH)/100
  
  AP <- (DP - RP)
  
  pO <- AP*TO/100
  pH <- AP*TH/100
  pN <- AP*(100 - TO - TH)/100
  
  Oxygen <- 100*(pO + pOres)/DP
  Helium <- 100*(pH + pHres)/DP
  Nitrogen <- 100*(pN + pNres)/DP
  
  END <- (((Nitrogen/100)*((MD/33)+1)/0.79)-1)*33
  ppO2 <- (Oxygen/100)*((MD/33)+1)
  
  Top_Off <<- data.frame('Oxygen' = Oxygen,
                         'Helium' = Helium,
                         'Nitrogen' = Nitrogen,
                         'Equivalent Narcotic Depth' = END,
                         'Oxygen Partial Pressure' = ppO2)
}
# Testing in bar - should be 20.1, 7.6, 72.3, 149', 1.21
#TopOff(17, 35, 50, 21, 0, 230, 166)

# CompHalf calculator
#
# The standard mathematical form for half-time calculation, for
# the inert gas pressure in any given compartment after any given exposure time
# 
# Pcomp  = Inert gas pressure in the compartment after the exposure time ( ATM )
# Pcomp = Pbegin + [ Pgas - Pbegin ] x [ 1 - 2 ^ ( - te / tht ) ]
#  
# Arguments:
# Pbegin = Inert gas pressure in the compartment before the exposure time ( ATM )
# Depth  = Depth of the dive
# (Pgas   = Inert gas pressure in the mixture being breathed ( ATM ))
# te     = Length of the exposure time ( minutes )
# tht    = Half time of the compartment ( minutes )
#
# Returns:
# 
Compartment <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)
NHT <- c(4.0,8.0,12.5,18.5,27.0,38.3,54.3,77.0,109.0,146.0,187.0,239.0,305.0,390.0,498.0,635.0)
NaV <- c(1.2599,1.0000,0.8618,0.7562,0.6667,0.5933,0.5282,0.4701,0.4187,0.3798,0.3497,0.3223,0.2971,0.2737,0.2523,0.2327)
NbV <- c(0.5050,0.6514,0.7222,0.7725,0.8125,0.8434,0.8693,0.8910,0.9092,0.9222,0.9319,0.9403,0.9477,0.9544,0.9602,0.9653)
HHT <- c(1.5,3.0,4.7,7.0,10.2,14.5,20.5,29.1,41.1,55.1,70.6,90.2,115.1,147.2,187.9,239.6)
HaV <- c(1.7435,1.3838,1.1925,1.0465,0.9226,0.8211,0.7309,0.6506,0.5794,0.5256,0.4840,0.4460,0.4112,0.3788,0.3492,0.3220)
HbV <- c(0.1911,0.4295,0.5446,0.6265,0.6917,0.7420,0.7841,0.8195,0.8491,0.8703,0.8860,0.8997,0.9118,0.9226,0.9321,0.9404)
ZHL16 <- data.frame(Compartment, NHT, NaV, NbV, HHT, HaV, HbV)

CompHalf <- function(Pbegin, Depth, te, tht) {
  Pcomp <<- Pbegin + ((1 + Depth/33)*Pbegin - Pbegin)*(1 - 2^(-te/tht))
  Pambtol <<- (Pcomp - 2*(tht^-(1/3)))*(1.005 - (tht^-(1/2)))
}
# Testing  - should be 1.33 and 0.54
#CompHalf(0.79, 100, 10, 27)
# Testing  - should be 3.03 and 1.57
#CompHalf(0.79, 100, 50, 12.5)
# Testing  - should be 1.30 and 0.84
#CompHalf(0.79, 100, 50, 146)

