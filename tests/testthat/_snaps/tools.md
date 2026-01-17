# t_ROS() works

    Code
      t_ROS(mcsa = 9, FSG = 6, SFC = 2, CBD = 0.1, ws = 11)
    Output
      $`Predicted rate of spread (m/min)`
      [1] 1.99
      
      $`Type of fire`
      [1] "S"
      

---

    Code
      t_ROS(mcsa = 8, FSG = 6, SFC = 2, CBD = 0.1, ws = 11)
    Output
      $`Predicted rate of spread (m/min)`
      [1] 9.69
      
      $`Type of fire`
      [1] "PC"
      

---

    Code
      t_ROS(mcsa = 8, FSG = 6, SFC = 2, CBD = 0.2, ws = 11)
    Output
      $`Predicted rate of spread (m/min)`
      [1] 19.61
      
      $`Type of fire`
      [1] "AC"
      

---

    Code
      t_ROS(mcsa = 8, CBD = 0.2, ws = 11, CF_thresh = 0.6)
    Output
      $`Predicted rate of spread (m/min)`
      [1] 2.8
      
      $`Type of fire`
      [1] "S"
      

# t_SFC_FBP() works

    Code
      t_SFC_FBP(BUI = 85, FFMC = 91, PC = 40)
    Output
      $C1
      [1] 1.42
      
      $`C2/M3/M4`
      [1] 3.12
      
      $`C3/C4`
      [1] 2.64
      
      $`C5/C6`
      [1] 2.2
      
      $C7
      [1] 3
      
      $`D1/D2`
      [1] 1.18
      
      $`M1/M2`
      [1] 1.96
      
      $S1
      [1] 7.3
      
      $S2
      [1] 12.65
      
      $S3
      [1] 25.72
      

# t_SFC_deGroot() works

    Code
      t_SFC_deGroot(BUI = 85, FFL = 3.5, FWFL = 0.3)
    Output
      $`Forest Floor Fuel Consumption`
      [1] 1.65
      
      $SFC
      [1] 1.95
      

# ladder_standing_dead() works

    Code
      ladder_standing_dead(cons = 0.2, cl = 4, FSG = 6)
    Output
      $`LFSG [m]`
      [1] 4
      
      $`Scaled SFC contribution, small snags [kg/m^2]`
      [1] 1.14
      

# ladder_midstory_saplings() works

    Code
      ladder_midstory_saplings(hs = 5, zs = 1, zp = 6, lnfl = 0.5, sapling_FMC = 120,
        actual_SFC = 2.7)
    Output
      $`Sapling crown centroid [m]`
      [1] 3
      
      $`FSG [m]`
      [1] 3
      
      $`Sapling false-SFC`
      [1] 1.41
      
      $`SFC scaled to crown centroid`
      [1] 0.95
      
      $`Total false-SFC`
      [1] 2.36
      

