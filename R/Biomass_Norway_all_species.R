
biomass.spruce.M1988 <- function(dbh.cm, H.m){
  
  ## Marklund 1988 G20 s. 50 
  dead.branches   <- exp(-4.6351 +  3.6518*(dbh.cm/(dbh.cm+18)) +
                 0.0493*H.m  + 1.0129*(log(H.m)))
  ## Marklund 1988 G12 s. 46  Includes foliage
  living.branches  <- exp(-1.2063 + 10.9708*(dbh.cm/(dbh.cm+13)) -
                  0.0124*H.m - 0.4923*(log(H.m)))
  ## Marklund 1988 G16 s. 48 not used
  foliage   <- exp(-1.8551 +  9.7809*(dbh.cm/(dbh.cm+12))
                   - 0.4873*(log(H.m)))
  ## Marklund 1988 G8  s. 44 
  bark  <- exp(-3.4020 +  8.3089*(dbh.cm/(dbh.cm+15)) + 0.0147*H.m  +
                 0.2295*(log(H.m)))
  ## Marklund 1988 G5  s. 42  Trunc excluding bark, excluding stump 
  stem.wood  <- exp(-2.3032 +  7.2309*(dbh.cm/(dbh.cm+14)) + 0.0355*H.m  +
                 0.7030*(log(H.m)))
  ## Marklund 1988 G26 s. 54 
  stump  <- exp(-3.3645 + 10.6686*(dbh.cm/(dbh.cm+17)))
  ## Marklund 1988 G31 s. 57 
  rot1  <- exp(-2.5706 +  7.6283*(dbh.cm/(dbh.cm+12)))       
  ## Marklund 1988 G28 s. 55 
  rot2  <- exp(-6.3851 + 13.3703*(dbh.cm/(dbh.cm+8)))
  
  ## Petterson og Sthål 2006 Root limit 2 mm 
  usoil  <- (exp(((0.32308)^2) / 2 + 4.58761 + (dbh.cm*10) /
                   ((dbh.cm*10) + 138) * 10.44035))/1000
  
  total.biomass.kg <-
    living.branches + dead.branches + bark + stem.wood + usoil
  biomass.aboveground.kg <- 
    living.branches + dead.branches + bark + stem.wood + stump
  biomass.belowground.kg <-  usoil- stump
  
  return(data.frame(living.branches, dead.branches, stem.wood, stump,
                    bark,
                    usoil, rot1, rot2, foliage, biomass.aboveground.kg,
                    biomass.belowground.kg, total.biomass.kg))
  
}

biomass.pine.M1988 <- function(dbh.cm, H.m){
  dead.branches    <- exp(-5.8926 +  7.1270*(dbh.cm/(dbh.cm+10)) -
                  0.0465 * H.m + 1.1060*(log(H.m)))
  living.branches  <- exp(-2.5413 + 13.3955*(dbh.cm/(dbh.cm+10)) - 1.1955 *
                  (log(H.m))) ## includes foliage
  foliage    <- exp(-3.4781 + 12.1095*(dbh.cm/(dbh.cm+ 7)) +
                      0.0413 * H.m - 1.5650*(log(H.m)))
  bark   <- exp(-3.2765 +  7.2482*(dbh.cm/(dbh.cm+16)) + 0.4487 *
                  (log(H.m)))
  stem.wood   <- exp(-2.6864 +  7.6066*(dbh.cm/(dbh.cm+14)) + 0.0200 *
                       H.m + 0.8658*(log(H.m)))
  stump   <- exp(-3.9657 + 11.0481*(dbh.cm/(dbh.cm+15)))
  rot1   <- exp(-3.8375 +  8.8795*(dbh.cm/(dbh.cm+10)))                
  rot2   <- exp(-6.3413 + 13.2902*(dbh.cm/(dbh.cm+ 9)))
  ## Petterson og Sthål 2006 Root limit 2 mm */
  usoil  <- (exp(((0.35449)^2)  / 2 + 3.44275 + (dbh.cm*10) /
                   ((dbh.cm*10) + 113) * 11.06537))/1000
  total.biomass.kg <-
    living.branches + dead.branches + bark + stem.wood + usoil
  biomass.aboveground.kg <- 
    living.branches + dead.branches + bark + stem.wood + stump
  biomass.belowground.kg <-  usoil- stump

  
  return(data.frame(living.branches, dead.branches, stem.wood, stump, bark,
                    usoil, rot1, rot2, foliage, biomass.aboveground.kg,
                    biomass.belowground.kg, total.biomass.kg))
}

biomass.birch.M1988 <- function(dbh.cm, H.m){
  dead.branches  <- exp(-6.6237 + 11.2872*(dbh.cm/(dbh.cm+30))-0.3081*H.m+
                          2.6821*(log(H.m)))
  living.branches  <- exp(-3.3633 + 10.2806*(dbh.cm/(dbh.cm+10)))
  ##BSTEM *0.11/0.52
  foliage  <- exp(-3.3045 + 8.1184*(dbh.cm/(dbh.cm+11)) + 0.9783*(log(H.m)))*
    0.011/0.52 
  bark <- exp(-4.0778 + 8.3019*(dbh.cm/(dbh.cm+14)) + 0.7433*(log(H.m)))
  stem.wood <- exp(-3.3045 + 8.1184*(dbh.cm/(dbh.cm+11)) + 0.9783*(log(H.m)))
  stump <- exp(-3.9657 + 11.0481*(dbh.cm/(dbh.cm+15))) ##Som pine
  ##BSTEM *0.042/0.52 
  rot1 <- exp(-3.3045 + 8.1184*(dbh.cm/(dbh.cm+11)) + 0.9783*(log(H.m)))*
    0.042/0.52
  ##BSTEM *0.042/0.52
  rot2 <- exp(-3.3045 + 8.1184*(dbh.cm/(dbh.cm+11)) + 0.9783*(log(H.m)))*
    0.042/0.52
  usoil <- (exp(((0.36266)^2) / 2 + 6.1708 + (dbh.cm*10) /
                  ((dbh.cm*10) + 225) * 10.01111))/1000
  
  return(data.frame(living.branches, dead.branches, stem.wood, stump,
                    bark,
                    usoil, rot1, rot2, foliage))
  }
      
biomass.birch.S2014 <- function(dbh.cm, H.m){
  ##--Smith Whole tree biomass (above- and belowground) 
  biomass.total.kg <- 0.1009 * dbh.cm ^ 2.1821 * H.m ^ 0.3804
  biomass.aboveground.kg.S2014 <-
    0.0521 * (dbh.cm ^ 2.1372) * (H.m ^0.5570) ##Smith: Total aboveground biomass
  biomass.belowground.kg.S2014 <- 0.0558 * dbh.cm ^ 2.2887 #- Smith: Belowground biomass

  ## we calculate stump.root from the old functions
  stump <-
    biomass.birch.M1988(dbh.cm , H.m )$stump
  ## trees were cut at stump height... so we add stump
  biomass.aboveground.kg <- biomass.aboveground.kg.S2014 + stump
  biomass.belowground.kg <- biomass.belowground.kg.S2014 - stump

  dead.branches  <-  0.0031 * dbh.cm ^1.7879
  living.branches  <- 0.0276 * dbh.cm ^3.0047 * H.m ^-0.7731
  foliage  <- 0.0078 * dbh.cm ^2.1953
  bark <- 0.0137 * dbh.cm^2.3109
  stem.wood <- 0.0182 * dbh.cm^1.9083 * H.m^1.0394 ## without bark
  stump <- exp(-3.9657 + 11.0481*(dbh.cm/(dbh.cm+15))) ## stub, same as pine
  rot1 <- exp(-3.3045 + 8.1184*(dbh.cm/(dbh.cm+11)) )+
                0.9783*log(H.m) *0.042/0.52
  rot2 <- exp(-3.3045 + 8.1184*(dbh.cm/(dbh.cm+11))) +
                0.9783*log(H.m) *0.042/0.52
  usoil <- 0.0558 * dbh.cm ^2.2887
  
  return(data.frame(biomass.total.kg, 
                    biomass.aboveground.kg.S2014, 
                    biomass.belowground.kg.S2014,
                    biomass.belowground.kg,
                    biomass.aboveground.kg,
                    living.branches, dead.branches, stem.wood, stump,
                    bark,
                    usoil, rot1, rot2, foliage))
}

