# plot the static engine torque and power at full load, function of engine speed
# plot(
#   eng.NtqFullLoad,
#   eng.tqFullLoad,
#   type = "l",
#   col = "green",
#   xlab = "rpm",
#   ylab = "Nm",
#   main = "torque"
# )
# plot(
#   eng.NtqFullLoad,
#   eng.pwrFullLoad,
#   type = "l",
#   col = "green",
#   xlab = "rpm",
#   ylab = "PS",
#   main = "power"
# )

engine <-
  function(eng.NtqFullLoad,
           eng.tqFullLoad,
           engineSpeed,
           s,
           eng.minRpm,
           eng.maxRpm) {
    
    # implement filter to simulate the mechanical inertia of the engine (no understandy)
    # engineSpeed <- engineSpeed * (1/1+0.1*s)
    
    if (engineSpeed >= eng.maxRpm) {
      engineSpeed = eng.maxRpm
    }
    else if (engineSpeed <= eng.minRpm) {
      engineSpeed = eng.minRpm
    }
    
    tq <- approx(eng.NtqFullLoad, eng.tqFullLoad, engineSpeed)$y
    kW <- tq * (engineSpeed * (pi / 30)) / 1000
    return(c(tq, kW))
  }

transmission <-
  function(eng_tq,
           trn_spd,
           gear,
           gearMin,
           gearMax,
           NmaxTq,
           NmaxPwr,
           gearRat,
           fd_ratio,
           dl_efficiency) {
    shift_scheduler <-
      function(rpm,
               gear,
               gearMin,
               gearMax,
               NmaxTq,
               NmaxPwr) {
        if (rpm >= NmaxPwr) {
          gear <- min(gear + 1, gearMax)
        }
        else if (rpm <= NmaxTq * 0.7) {
          gear <- max(gear - 1, gearMin)
        }
        return(gear)
      }
    
    trn_rpm <- (30 / pi) * trn_spd
    eng_rpm <-  trn_rpm * gearRat[gear] * fd_ratio
    gear <-
      shift_scheduler(eng_rpm, gear, gearMin, gearMax, NmaxTq, NmaxPwr)
    eng_rpm <-  trn_rpm * gearRat[gear] * fd_ratio
    
    trn_tq <- gearRat[gear] * fd_ratio * eng_tq *  dl_efficiency
    return(c(trn_tq, eng_rpm, gear))
  }

vehicle <-
  function(veh.mass,
           veh.g,
           tire.miua,
           tire.load,
           road.slope,
           road.cr,
           veh.ro,
           veh.cd,
           veh.fa,
           whl_tq,
           vehSpeed,
           dT,
           tire.rwd) {
    friction_limit <- (veh.mass * veh.g * tire.miua * tire.load)
    F_slope <- (veh.mass * veh.g * sin(road.slope))
    F_roll <- (veh.mass * veh.g * road.cr * cos(road.slope))
    F_drag <- (0.5 * veh.ro * veh.cd * veh.fa * vehSpeed ^ 2)
    F_resistance_total <- sum(F_slope, F_roll, F_drag)
    whlTotTrq_N <- whl_tq / tire.rwd
    effectiveDriveTq <- min(whlTotTrq_N, friction_limit)
    netForce <- effectiveDriveTq - F_resistance_total
    vehAcc_mps2 <- netForce / veh.mass # F=ma
    vehSpeed <-
      vehSpeed + (vehAcc_mps2 * dT)  # assumes loop is dT seconds/iteration; instead of integrating
    vehSpeed_kph <- vehSpeed * 3.6
    whl_spd <- vehSpeed / tire.rwd
    return(c(whl_spd, as.numeric(vehSpeed), vehAcc_mps2))
  }

