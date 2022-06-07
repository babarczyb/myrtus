#---- II.1. Treatment function ----


treatment <- function (asct_elig, venvd_elig, cytrisk, #baseline patient data
                       i, response_int, response_comp, treatment_int, treatment_comp, prev_line_int, prev_line_comp, tracker_int, #cycle patient data
                       tracker_comp, cycle_track_int, cycle_track_comp, toxic_int, toxic_comp, relapse_int, relapse_comp, l1_tx_int, l1_tx_comp, #cycle patient data
                       date_adverse_l1_comp, date_adverse_2asct_comp, date_adverse_maint_comp,date_adverse_l2_comp,date_adverse_l3_comp,
                       date_adverse_l1_int,date_adverse_2asct_int,date_adverse_maint_int,date_adverse_l2_int,date_adverse_l3_int,
                       asct2_int, asct2_comp, #previous asct
                       seed_int, seed_comp, #random numbers
                       max_cycles_vector, cost_matrix, p_adverse_tx_vector, p_cr_vector, p_vgpr_vector, p_pr_vector, #model inputs
                       p_mr_vector, p_sd_vector, p_pd_vector, maint_better, scenario, dara2l,  rates_vector, dara_retr) {
  
  date_adverse_l1_comp <- date_adverse_l1_comp
  date_adverse_2asct_comp  <- date_adverse_2asct_comp
  date_adverse_maint_comp <- date_adverse_maint_comp
  date_adverse_l2_comp <- date_adverse_l2_comp
  date_adverse_l3_comp <- date_adverse_l3_comp
  date_adverse_l1_int <- date_adverse_l1_int
  date_adverse_2asct_int <- date_adverse_2asct_int
  date_adverse_maint_int <- date_adverse_maint_int
  date_adverse_l2_int <- date_adverse_l2_int
  date_adverse_l3_int <- date_adverse_l3_int
  asct2_int <- asct2_int
  asct2_comp <- asct2_comp
  
  #Previous toxicity
  toxic_int_0 <- toxic_int
  toxic_comp_0 <- toxic_comp
  
  
  if (response_int==7 & response_comp==7){ #dead on both arms, 0
    response_int <- response_comp <- 7
    treatment_int <- tracker_int <- cycle_track_int <- current_line_int <- toxic_int <- cost_int <- treatment_comp <- tracker_comp <- cycle_track_comp <- current_line_comp <- toxic_comp <- cost_comp <-  0
    l1_tx_int <- l1_tx_comp <- 0
  } else {
    #Checks similarity, sets the random number
    if (response_int==response_comp & toxic_int==toxic_comp & prev_line_int==prev_line_comp){
      
      set.seed(seed_int)
      rand_tx_int <- rand_tx_comp <- runif(1)
      rand_ae_int <- rand_ae_comp <- runif(1)
      rand_resp_int <- rand_resp_comp <- runif(1)
    } else {
      
      set.seed(seed_int)
      rand_tx_int <- runif(1)
      rand_ae_int <- runif(1)
      rand_resp_int <- runif(1)
      
      set.seed(seed_comp)
      rand_tx_comp <- runif(1)
      rand_ae_comp <- runif(1)
      rand_resp_comp <- runif(1)
    }
    ####################################################################################################################
    #################################################### Intervention ##################################################
    ####################################################################################################################
    if (response_int==7){#dead on intervention arm
      response_int <- 7
      treatment_int <- tracker_int <- cycle_track_int <- current_line_int <- toxic_int <- cost_int <- 0
    } else{ #treatment algorithm starts for intervention
      
      #Treatment change: AE or Relapse
      if(toxic_int==1 | relapse_int==1){
        #first cycle there is no previous line
        if (prev_line_int==0) {
          #ASCT
          if (asct_elig==1){
            #Intervention: Daratumumab
            if (scenario==1 | scenario==2){
              treatment_int <- 1 #DVTd
              
              l1_tx_int <- treatment_int
              tracker_int <- 1
              cycle_track_int <- 1
              current_line_int <- 1
              cost_int <- cost_matrix[tracker_int, treatment_int]
              
              toxic_int <- if (rand_ae_int < p_adverse_tx_vector[treatment_int]){1} else {0} #Adverse Event
              date_adverse_l1_int <- if (toxic_int==1) {i} else {0} #AE cycle number saved out
              induction <- treatment_int # Induction therpary saved out
            } 
            #Intervention: second/third line daratumumab
            else {
              treatment_int <- if (rand_tx_int < rates_vector[1]) {2} #VRd
              else if (rand_tx_int < rates_vector[1] + rates_vector[2]){3} #VTd
              else {4} #VCd
              
              l1_tx_int <- treatment_int
              tracker_int <- 1
              cycle_track_int <- 1
              current_line_int <- 1
              cost_int <- cost_matrix[tracker_int, treatment_int]
              
              toxic_int <- if (rand_ae_int < p_adverse_tx_vector[treatment_int]){1} else {0} #Adverse Event
              date_adverse_l1_int <- if (toxic_int==1) {i} else {0} #AE cycle number saved out
            }
          } 
          #NO ASCT
          else { 
            #Intervention: Daratumumab
            if (scenario==1 | scenario==2){
              treatment_int <- if (rand_tx_int < rates_vector[4]) {5} #DRd
              else {6} #DVMP
              
              l1_tx_int <- treatment_int
              tracker_int <- 1
              cycle_track_int <- 1
              current_line_int <- 1
              cost_int <- cost_matrix[tracker_int, treatment_int]
              
              toxic_int <- if (rand_ae_int < p_adverse_tx_vector[treatment_int]){1} else {0} #Adverse Event
              date_adverse_l1_int <- if (toxic_int==1) {i} else {0} #AE cycle number saved out
            }
            #Intervention: second/third line daratumumab 
            else { 
              treatment_int <- if (rand_tx_int < rates_vector[6]) {24} #VRd
              else if (rand_tx_int < rates_vector[6] + rates_vector[7]){20} #VMP
              else {21} #Rd
              
              l1_tx_int <- treatment_int
              tracker_int <- 1
              cycle_track_int <- 1
              current_line_int <- 1
              cost_int <- cost_matrix[tracker_int, treatment_int]
              
              toxic_int <- if (rand_ae_int < p_adverse_tx_vector[treatment_int]){1} else {0} #Adverse Event
              date_adverse_l1_int <- if (toxic_int==1) {i} else {0} #AE cycle number saved out
            }
          }
        } 
        #Patient got Relapse on first line
        else if (prev_line_int==1) {
          #Previous intervention: First line Daratumumab
          if (scenario==1 | scenario==2){
            #previous DVTd or DVMP
            if (treatment_int==1 | treatment_int==6){
              treatment_int <- if (venvd_elig==1){9} #VenVd
              else if (rand_tx_int < rates_vector[16]) {7} #KRd
              else if (rand_tx_int < rates_vector[16]+rates_vector[17]) {13} #Kd
              else if (rand_tx_int < rates_vector[16]+rates_vector[17]+rates_vector[18]) {2}#VRd
              else if (rand_tx_int < rates_vector[16]+rates_vector[17]+rates_vector[18]+rates_vector[19]) {8} #IxaRd
              else {12} #SVd
              
              tracker_int <- 1
              cycle_track_int <- 1
              current_line_int <- 3
              cost_int <- cost_matrix[tracker_int, treatment_int]
              l1_tx_int <- l1_tx_int
              
              toxic_int <- if (rand_ae_int < p_adverse_tx_vector[treatment_int]){1} else {0} #Adverse Event
              date_adverse_l2_int <- if (toxic_int==1) {i} else {0} #AE cycle number saved out
            } 
            #Previous DRd
            else if (treatment_int==5) { 
              treatment_int <- if (venvd_elig==1){9} #VenVd
              else if (rand_tx_int < rates_vector[21]) {11} #PomVd 
              else if (rand_tx_int < rates_vector[21]+rates_vector[22]) {13} #Kd
              else if (rand_tx_int < rates_vector[21]+rates_vector[22]+rates_vector[23]) {7} #KRd
              else if (rand_tx_int <rates_vector[21]+rates_vector[22]+rates_vector[23]+rates_vector[24]) {8} #IxaRd
              else {12} #SVd
              
              tracker_int <- 1
              cycle_track_int <- 1
              current_line_int <- 3
              cost_int <- cost_matrix[tracker_int, treatment_int]
              l1_tx_int <- l1_tx_int
              
              toxic_int <- if (rand_ae_int < p_adverse_tx_vector[treatment_int]){1} else {0} #Adverse Event
              date_adverse_l2_int <- if (toxic_int==1) {i} else {0} #AE cycle number saved out
            } 
            #Error in first line daratumumab treatment
            else {
              treatment_int<-tracker_int<-cycle_track_int<-current_line_int<-cost_int<-"ERROR"
            }
          }
          #3. scenario - Second/third line Dara vs no dara
          else {
            #Daratumumab on 2. line
            if (dara2l==1){ 
              treatment_int <- if (rand_tx_int < rates_vector[32]) {14} #DVd
              else if (rand_tx_int < rates_vector[32] + rates_vector[33]) {15} #DKd
              else if (rand_tx_int < rates_vector[32] + rates_vector[33] + rates_vector[34]) {23} #DRd (second line)
              else {"ERROR"}
              
              tracker_int <- 1
              cycle_track_int <- 1
              current_line_int <- 3
              cost_int <- cost_matrix[tracker_int, treatment_int]
              l1_tx_int <- l1_tx_int
              
              toxic_int <- if (rand_ae_int < p_adverse_tx_vector[treatment_int]){1} else {0} #Adverse Event
              date_adverse_l2_int <- if (toxic_int==1) {i} else {0} #AE cycle number saved out
            }
            #No daratumumab on 2. line
            else {
              #Previous line was VTd, VMP, or VCd
              if(treatment_int==3 |treatment_int==20 | treatment_int==4){
                treatment_int <- if (rand_tx_int<rates_vector[9]) {7} #KRd
                else {8} #IxaRd
                
                tracker_int <- 1
                cycle_track_int <- 1
                current_line_int <- 3
                cost_int <- cost_matrix[tracker_int, treatment_int]
                l1_tx_int <- l1_tx_int
                
                toxic_int <- if (rand_ae_int < p_adverse_tx_vector[treatment_int]){1} else {0} #Adverse Event
                date_adverse_l2_int <- if (toxic_int==1) {i} else {0} #AE cycle number saved out
              } 
              #Previous line was VRd or Rd
              else if (treatment_int==2 | treatment_int==21 | treatment_int==24) {
                treatment_int <- if (venvd_elig==1) {9} #VenVd
                else if (rand_tx_int<rates_vector[11]) {7} #KRd
                else if (rand_tx_int<rates_vector[11]+rates_vector[12]) {10} #IsaKd
                else if (rand_tx_int<rates_vector[11]+rates_vector[12]+rates_vector[13]) {11} #PomVd
                else if (rand_tx_int<rates_vector[11]+rates_vector[12]+rates_vector[13]+rates_vector[14]) {8} #IxaRd
                else {12} #SVd
                
                tracker_int <- 1
                cycle_track_int <- 1
                current_line_int <- 3
                cost_int <- cost_matrix[tracker_int, treatment_int]
                l1_tx_int <- l1_tx_int
                
                toxic_int <- if (rand_ae_int < p_adverse_tx_vector[treatment_int]){1} else {0}  #Adverse Event
                date_adverse_l2_int <- if (toxic_int==1) {i} else {0} #AE cycle number saved out
              }
            }
          }
        }
        #Previously was on maintenance
        else if(prev_line_int==2) {
          #Second ASCT eligible (Lenalidomide + 36m without relapse)
          if (treatment_int==19 & cycle_track_int>12 & asct2_int!=1) {
            #Daratumumab treatment repeat
            if (scenario==1 | scenario==2){
              treatment_int <- 1
              l1_tx_int <- treatment_int
              tracker_int <- 1
              cycle_track_int <- 1
              current_line_int <- 5 
              asct2_int <- 1 #there will be no 3. ASCT
              cost_int <- cost_matrix[tracker_int, treatment_int]
              toxic_int <- if (rand_ae_int < p_adverse_tx_vector[treatment_int]){1} else {0}  #Adverse Event
              date_adverse_2asct_int <- if (toxic_int==1) {i} else {0} #AE cycle number saved out
            } 
            #No daratumumab, first line treatment repeat
            else {
              treatment_int <- if (rand_tx_int < rates_vector[1]) {2} #VRd
              else if (rand_tx_int < rates_vector[1] + rates_vector[2]){3} #VTd 
              else {4} #VCd
              
              l1_tx_int <- treatment_int
              tracker_int <- 1
              cycle_track_int <- 1
              current_line_int <- 5
              cost_int <- cost_matrix[tracker_int, treatment_int]
              asct2_int <- 1 #there will be no 3. ASCT
              
              toxic_int <- if (rand_ae_int < p_adverse_tx_vector[treatment_int]){1} else {0} #Adverse Event
              date_adverse_2asct_int <- if (toxic_int==1) {i} else {0} #AE cycle number saved out
            }
          }
          #No second ASCT
          else {
            #Previous first line Daratumumab
            if (scenario==1 | scenario==2){
              #DVTd or DVMP
              if (l1_tx_int==1 | l1_tx_int==6){ 
                treatment_int <- if (venvd_elig==1){9} #VenVd
                else if (rand_tx_int < rates_vector[16]) {7} #KRd
                else if (rand_tx_int < rates_vector[16]+rates_vector[17]) {13} #Kd
                else if (rand_tx_int < rates_vector[16]+rates_vector[17]+rates_vector[18]) {2} #VRd
                else if (rand_tx_int < rates_vector[16]+rates_vector[17]+rates_vector[18]+rates_vector[19]) {8} #IxaRd
                else {12} #SVd
                
                tracker_int <- 1
                cycle_track_int <- 1
                current_line_int <- 3
                cost_int <- cost_matrix[tracker_int, treatment_int]
                l1_tx_int <- l1_tx_int
                toxic_int <- if (rand_ae_int < p_adverse_tx_vector[treatment_int]){1} else {0}  #Adverse Event
                date_adverse_l2_int <- if (toxic_int==1) {i} else {0} #AE cycle number saved out
              }
              #DRd
              else if (l1_tx_int==5) {
                treatment_int <- if (venvd_elig==1){9} #VenVd
                else if (rand_tx_int < rates_vector[21]) {11} #PomVd 
                else if (rand_tx_int < rates_vector[21]+rates_vector[22]) {13} #Kd
                else if (rand_tx_int < rates_vector[21]+rates_vector[22]+rates_vector[23]) {7} #KRd
                else if (rand_tx_int < rates_vector[21]+rates_vector[22]+rates_vector[23]+rates_vector[24]) {8} #IxaRd
                else {12} #SVd
                
                tracker_int <- 1
                cycle_track_int <- 1
                current_line_int <- 3
                cost_int <- cost_matrix[tracker_int, treatment_int]
                l1_tx_int <- l1_tx_int
                
                toxic_int <- if (rand_ae_int < p_adverse_tx_vector[treatment_int]){1} else {0}   #Adverse Event
                date_adverse_l2_int <- if (toxic_int==1) {i} else {0} #AE cycle number saved out
              } 
              #check
              else {
                treatment_int<-tracker_int<-cycle_track_int<-current_line_int<-cost_int<-"ERROR"
              }
            } 
            #3. scenario - Second/third line Dara vs no dara
            else {
              #Daratumumab on second line
              if (dara2l==1){
                treatment_int <- if (rand_tx_int <rates_vector[32]) {14} #DVd
                else if (rand_tx_int < rates_vector[32] + rates_vector[33]) {15} #DKd
                else if (rand_tx_int < rates_vector[32] + rates_vector[33] + rates_vector[34]) {23} #DRd (second line)
                else {"ERROR"}
                
                tracker_int <- 1
                cycle_track_int <- 1
                current_line_int <- 3
                cost_int <- cost_matrix[tracker_int, treatment_int]
                l1_tx_int <- l1_tx_int
                
                toxic_int <- if (rand_ae_int < p_adverse_tx_vector[treatment_int]){1} else {0} #Adverse Event
                date_adverse_l2_int <- if (toxic_int==1) {i} else {0} #AE cycle number saved out
                
              } 
              #No second line Dara
              else {
                #Previous line VTd, VMP, VCd
                if(l1_tx_int==3 |l1_tx_int==20 | l1_tx_int==4){
                  treatment_int <- if (rand_tx_int<rates_vector[9]) {7} #KRd
                  else {8} #IxaRd
                  
                  tracker_int <- 1
                  cycle_track_int <- 1
                  current_line_int <- 3
                  cost_int <- cost_matrix[tracker_int, treatment_int]
                  l1_tx_int <- l1_tx_int
                  
                  toxic_int <- if (rand_ae_int < p_adverse_tx_vector[treatment_int]){1} else {0} #Adverse Event
                  date_adverse_l2_int <- if (toxic_int==1) {i} else {0} #AE cycle number saved out
                  
                } 
                #Previous VRd, Rd
                else if (l1_tx_int==2 | l1_tx_int==21 | l1_tx_int==24) {
                  treatment_int <- if (venvd_elig==1) {9} #VenVd
                  else if (rand_tx_int<rates_vector[11]) {7} #KRd
                  else if (rand_tx_int<rates_vector[11]+rates_vector[12]) {10} #IsaKd
                  else if (rand_tx_int<rates_vector[11]+rates_vector[12]+rates_vector[13]) {11} #PomVd
                  else if (rand_tx_int<rates_vector[11]+rates_vector[12]+rates_vector[13]+rates_vector[14]) {8} #IxaRd
                  else {12} #SVd
                  
                  tracker_int <- 1
                  cycle_track_int <- 1
                  current_line_int <- 3
                  cost_int <- cost_matrix[tracker_int, treatment_int]
                  l1_tx_int <- l1_tx_int
                  
                  toxic_int <- if (rand_ae_int < p_adverse_tx_vector[treatment_int]){1} else {0} #Adverse Event
                  date_adverse_l2_int <- if (toxic_int==1) {i} else {0} #AE cycle number saved out
                }
              }
            }
          }
        }
        #Patient got Relapse on second line
        else if (prev_line_int==3) {
          #First line was daratumumab, second line was not
          if (scenario==1 | scenario==2){ 
            treatment_int <- if (rand_tx_int<rates_vector[26]) {16} #DPd
            else if (rand_tx_int < rates_vector[26]+rates_vector[27]){14} #DVd
            else if (rand_tx_int < rates_vector[26] + rates_vector[27] + rates_vector[28]) {15} #DKd
            else {"ERROR"}
            
            tracker_int <- 1
            cycle_track_int <- 1
            current_line_int <- 4
            cost_int <- cost_matrix[tracker_int, treatment_int]
            l1_tx_int <- l1_tx_int
            
            toxic_int <- if (rand_ae_int < p_adverse_tx_vector[treatment_int]){1} else {0} #Adverse Event
            date_adverse_l3_int <- if (toxic_int==1) {i} else {0} #AE cycle number saved out
            
          }
          #3. scenario - Second/third line Dara vs no dara
          else if (scenario==3) {
            #after 2. line daratumumab, cannot recieve mAB on 3. line
            if (dara2l==1) {
              treatment_int <- if (venvd_elig==1){9} #VenVd
              else {22} #SVd helyett dummy
              
              tracker_int <- 1
              cycle_track_int <- 1
              current_line_int <- 4
              cost_int <- cost_matrix[tracker_int, treatment_int]
              l1_tx_int <- l1_tx_int
              response_int <- 5
              
              toxic_int <- if (rand_ae_int < p_adverse_tx_vector[treatment_int]){1} else {0} #Adverse Event
              date_adverse_l3_int <- if (toxic_int==1) {i} else {0} #AE cycle number saved out
              
            }
            #no dara second line
            else {
              #2. line Isatuximab, will not receive daratumumab
              if (treatment_int==10){
                treatment_int <- if (venvd_elig==1){9} else {22} #SVd helyett dummy
                
                tracker_int <- 1
                cycle_track_int <- 1
                current_line_int <- 4
                cost_int <- cost_matrix[tracker_int, treatment_int]
                l1_tx_int <- l1_tx_int
                response_int <- 5
                
                toxic_int <- if (rand_ae_int < p_adverse_tx_vector[treatment_int]){1} else {0} #Adverse Event
                date_adverse_l3_int <- if (toxic_int==1) {i} else {0} #AE cycle number saved out
                
              } 
              #not received mAB type treatment, will receive daratumumab
              else {
                treatment_int <- if (rand_tx_int<rates_vector[26]) {16} #DPd
                else if (rand_tx_int < rates_vector[26]+rates_vector[27]){14} #DVd
                else if (rand_tx_int < rates_vector[26] + rates_vector[27] + rates_vector[28]) {15} #DKd
                else {"ERROR in 2. relapse in 3. scen treatment"}
                
                tracker_int <- 1
                cycle_track_int <- 1
                current_line_int <- 4
                cost_int <- cost_matrix[tracker_int, treatment_int]
                l1_tx_int <- l1_tx_int
                
                toxic_int <- if (rand_ae_int < p_adverse_tx_vector[treatment_int]){1} else {0} #Adverse Event
                date_adverse_l3_int <- if (toxic_int==1) {i} else {0} #AE cycle number saved out
              }
            }
          } else {
            treatment_int<-"ERROR in 2. relapse in 3. scen"
            current_line_int<-"ERROR in 2. relapse in 3. scen"
            cycle_track_int<-"ERROR in 2. relapse in 3. scen"
            tracker_int<-"ERROR in 2. relapse in 3. scen"
            cost_int <- "ERROR in 2. relapse in 3. scen"
            toxic_int <- "ERROR in 2. relapse in 3. scen"
            date_adverse_l3_int <- "ERROR in 2. relapse in 3. scen"
          }
        }
        #Patient got relapse on third line
        else if (prev_line_int==4) {
          treatment_int <- 22
          current_line_int <- prev_line_int
          cycle_track_int <- cycle_track_int + 1
          toxic_int <- 0
          tracker_int <- 1
          response_int <- 5
          cost_int <- cost_matrix[tracker_int, treatment_int]
        }
        #Patient got relapse on maintenance after second ASCT
        else if (prev_line_int==5) {
          #Previous line was daratumumab
          if (scenario==1 | scenario==2){ 
            #DVTd or DVMP
            if (treatment_int==1 | treatment_int==6){
              treatment_int <- if (venvd_elig==1){9} #VenVd
              else if (rand_tx_int < rates_vector[16]) {7} #KRd
              else if (rand_tx_int < rates_vector[16]+rates_vector[17]) {13} #Kd
              else if (rand_tx_int < rates_vector[16]+rates_vector[17]+rates_vector[18]) {2} #VRd
              else if (rand_tx_int < rates_vector[16]+rates_vector[17]+rates_vector[18]+rates_vector[19]) {8} #IxaRd
              else {12} #SVd
              
              tracker_int <- 1
              cycle_track_int <- 1
              current_line_int <- 3
              cost_int <- cost_matrix[tracker_int, treatment_int]
              l1_tx_int <- l1_tx_int
              
              toxic_int <- if (rand_ae_int < p_adverse_tx_vector[treatment_int]){1} else {0} #Adverse Event
              date_adverse_l2_int <- if (toxic_int==1) {i} else {0} #AE cycle number saved out
            } 
            #DRd
            else if (treatment_int==5) {
              treatment_int <- if (venvd_elig==1){9} #VenVd
              else if (rand_tx_int < rates_vector[21]) {11} #PomVd
              else if (rand_tx_int < rates_vector[21]+rates_vector[22]) {13} #Kd
              else if (rand_tx_int < rates_vector[21]+rates_vector[22]+rates_vector[23]) {7} #KRd
              else if (rand_tx_int < rates_vector[21]+rates_vector[22]+rates_vector[23]+rates_vector[24]) {8} #IxaRd
              else {12} #SVd
              
              tracker_int <- 1
              cycle_track_int <- 1
              current_line_int <- 3
              cost_int <- cost_matrix[tracker_int, treatment_int]
              l1_tx_int <- l1_tx_int
              
              toxic_int <- if (rand_ae_int < p_adverse_tx_vector[treatment_int]){1} else {0} #Adverse Event
              date_adverse_l2_int <- if (toxic_int==1) {i} else {0} #AE cycle number saved out
            }
            else {
              treatment_int<-tracker_int<-cycle_track_int<-current_line_int<-cost_int<-"ERROR"
            }
          }
          #3. scenario - Second/third line Dara vs no dara
          else {
            #Daratumumab on second line
            if (dara2l==1){
              treatment_int <- if (rand_tx_int < rates_vector[32]) {14} #DVd
              else if (rand_tx_int < rates_vector[33] + rates_vector[32]) {15} #DKd
              else if (rand_tx_int < rates_vector[34] + rates_vector[32] + rates_vector[33]) {23} #DRd second line
              else {"ERROR"}
              
              tracker_int <- 1
              cycle_track_int <- 1
              current_line_int <- 3
              cost_int <- cost_matrix[tracker_int, treatment_int]
              l1_tx_int <- l1_tx_int
              
              toxic_int <- if (rand_ae_int < p_adverse_tx_vector[treatment_int]){1} else {0} #Adverse Event
              date_adverse_l2_int <- if (toxic_int==1) {i} else {0} #AE cycle number saved out
            }
            #No second line Dara
            else {
              #Previos line VTD, VMP, VCD
              if(treatment_int==3 |treatment_int==20 | treatment_int==4){ 
                treatment_int <- if (rand_tx_int<rates_vector[9]) {7} #KRd
                else {8} #IxaRd
                
                tracker_int <- 1
                cycle_track_int <- 1
                current_line_int <- 3
                cost_int <- cost_matrix[tracker_int, treatment_int]
                l1_tx_int <- l1_tx_int
                
                toxic_int <- if (rand_ae_int < p_adverse_tx_vector[treatment_int]){1} else {0} #Adverse Event
                date_adverse_l2_int <- if (toxic_int==1) {i} else {0} #AE cycle number saved out
                
              } 
              #VRd, Rd
              else if (treatment_int==2 | treatment_int==21 | treatment_int==24) {
                treatment_int <- if (venvd_elig==1) {9} #VenVd
                else if (rand_tx_int<rates_vector[11]) {7} #KRd
                else if (rand_tx_int<rates_vector[11]+rates_vector[12]) {10} #IsaKd
                else if (rand_tx_int<rates_vector[11]+rates_vector[12]+rates_vector[13]) {11} #PomVd
                else if (rand_tx_int<rates_vector[11]+rates_vector[12]+rates_vector[13]+rates_vector[14]) {8} #IxaRd
                else {12} #SVd
                
                tracker_int <- 1
                cycle_track_int <- 1
                current_line_int <- 3
                cost_int <- cost_matrix[tracker_int, treatment_int]
                l1_tx_int <- l1_tx_int
                
                toxic_int <- if (rand_ae_int < p_adverse_tx_vector[treatment_int]){1} else {0} #Adverse Event
                date_adverse_l2_int <- if (toxic_int==1) {i} else {0}  #AE cycle number saved out
              }
            }
          }
        } 
        else{
          treatment_int<- "ERROR-treatment line"
          current_line_int<-"ERROR-treatment line"
          cycle_track_int<-"ERROR-treatment line"
          tracker_int<-"ERROR-treatment line"
          cost_int <- "ERROR-treatment line"
          response_int <- "ERROR-treatment line"
          toxic_int <- "ERROR-treatment line"
          date_adverse_maint_int <- "ERROR-treatment line"
          l1_tx_int <- "ERROR-treatment line"
        }
        
        
      } #end of treatment change from AE or relapse
      ###Treatment change: from induction to maintenance
      else if (tracker_int==max_cycles_vector[prev_line_int+1] ) {
        #Previous ASCT (1 or 2, count as first line)
        if ((prev_line_int==1 | prev_line_int==5) & asct_elig==1) {
          #bortezomib maintenance starts
          if (rand_tx_int<rates_vector[35]){
            treatment_int <- 18
            current_line_int <- 2
            cycle_track_int <- 1
            tracker_int <- 1
            response_int <- response_int
            cost_int <- cost_matrix[tracker_int, treatment_int]
            l1_tx_int <- l1_tx_int
            
            toxic_int <- if (rand_ae_int < p_adverse_tx_vector[treatment_int]){1} else {0} #Adverse Event
            date_adverse_maint_int <- if (toxic_int==1) {i} else {0} #AE cycle number saved out
          }
          #lenalidomide maintenance starts
          else {
            treatment_int<-19
            current_line_int<-2
            cycle_track_int<-1
            tracker_int <- 1
            cost_int <- cost_matrix[tracker_int, treatment_int]
            response_int <- response_int
            l1_tx_int <- l1_tx_int
            
            toxic_int <- if (rand_ae_int < p_adverse_tx_vector[treatment_int]){1} else {0} #Adverse Event
            date_adverse_maint_int <- if (toxic_int==1) {i} else {0} #AE cycle number saved out
          }
        }
        #Induction stops, no maintenance
        else {
          tracker_int <- 0
          cycle_track_int <- cycle_track_int+1
          treatment_int <- treatment_int
          current_line_int <- prev_line_int
          response_int <- response_int
          toxic_int <- toxic_int
          cost_int <- 0
          l1_tx_int <- l1_tx_int
        }
      }
      #NO TREATMENT CHANGE - Still no therapy
      else if (tracker_int==0)  {
        treatment_int <- treatment_int
        tracker_int <- tracker_int
        cycle_track_int <- cycle_track_int+1
        current_line_int <- prev_line_int
        toxic_int <- toxic_int
        cost_int <- 0
        l1_tx_int <- l1_tx_int
        response_int <- response_int
      } 
      #On lenalidomide maintenance response can improve
      else if (treatment_int==19){
        #PD or CR cannot improve
        if (response_int==6 | response_int==1){
          tracker_int <- tracker_int+1
          cycle_track_int <- cycle_track_int+1
          treatment_int <- treatment_int
          current_line_int <- prev_line_int
          response_int <- response_int
          toxic_int <- toxic_int
          cost_int <- cost_matrix[tracker_int, treatment_int]
          l1_tx_int <- l1_tx_int
        }
        #Response improves on lenalidomide maintenance
        else {
          if (rand_resp_int < maint_better){
            response_int <- response_int-1
            tracker_int <- tracker_int+1
            cycle_track_int <- cycle_track_int+1
            treatment_int <- treatment_int
            current_line_int <- prev_line_int
            toxic_int <- toxic_int
            cost_int <- cost_matrix[tracker_int, treatment_int]
            l1_tx_int <- l1_tx_int
          } else {
            tracker_int <- tracker_int+1
            cycle_track_int <- cycle_track_int+1
            treatment_int <- treatment_int
            current_line_int <- prev_line_int
            response_int <- response_int
            toxic_int <- toxic_int
            cost_int <- cost_matrix[tracker_int, treatment_int]
            l1_tx_int <- l1_tx_int
          }
        }
      }
      #Still on other treatment, no change
      else {
        tracker_int <- tracker_int+1
        cycle_track_int <- cycle_track_int+1
        treatment_int <- treatment_int
        current_line_int <- prev_line_int
        response_int <- response_int
        toxic_int <- toxic_int
        cost_int <- cost_matrix[tracker_int, treatment_int]
        l1_tx_int <- l1_tx_int
      }
      
      #IF patient had AE or relapse last cycle, now will recieve a response for the new therapy
      response_int <-  if ((toxic_int_0==1 | relapse_int==1) & prev_line_int!=4 & treatment_int!=22){
        if (rand_resp_int < p_cr_vector[treatment_int]) {1} #CR
        else if(rand_resp_int < p_cr_vector[treatment_int] + p_vgpr_vector[treatment_int]) {2} #VGPR
        else if(rand_resp_int < p_cr_vector[treatment_int] + p_vgpr_vector[treatment_int] + p_pr_vector[treatment_int]) {3} #PR
        else if(rand_resp_int < p_cr_vector[treatment_int] + p_vgpr_vector[treatment_int] + p_pr_vector[treatment_int] + p_mr_vector[treatment_int]) {4} #MR
        else if(rand_resp_int < p_cr_vector[treatment_int] + p_vgpr_vector[treatment_int] + p_pr_vector[treatment_int] + p_mr_vector[treatment_int] + p_sd_vector[treatment_int]) {5} #SD
        else if(rand_resp_int < p_cr_vector[treatment_int] + p_vgpr_vector[treatment_int] + p_pr_vector[treatment_int] + p_mr_vector[treatment_int] + p_sd_vector[treatment_int] + p_pd_vector[treatment_int]) {6} #PD
        else {"ERROR in response"}
      } 
      else {response_int}#no change, no event
    }
    
    ####################################################################################################################################################
    #######################################################################COMPARATOR TREATMENT#########################################################
    ####################################################################################################################################################
    
    if (response_comp==7){
      response_comp <- 7
      treatment_comp <- tracker_comp <- cycle_track_comp <- current_line_comp <- toxic_comp <- cost_comp <- 0
    }
    #treatment algorithm starts for comparator
    else { 
      #Treatment change: AE or Relapse
      if (toxic_comp==1 | relapse_comp==1){
        #first cycle there is no previous line
        if (prev_line_comp==0) {
          #ASCT
          if (asct_elig==1){
            #Comparator: no daratumumab
            if (scenario==2 | scenario==3){ 
              treatment_comp <- if (rand_tx_comp < rates_vector[1]) {2} #VRd
              else if (rand_tx_comp < rates_vector[1] + rates_vector[2]){3} #VTd
              else {4} #VCd
              
              l1_tx_comp <- treatment_comp
              tracker_comp <- 1
              cycle_track_comp <- 1
              current_line_comp <- 1
              cost_comp <- cost_matrix[tracker_comp, treatment_comp]
              
              toxic_comp <- if (rand_ae_comp < p_adverse_tx_vector[treatment_comp]){1} else {0} #Adverse Event
              date_adverse_l1_comp <- if (toxic_comp==1) {i} else {0} #AE cycle number saved out
            } 
            #Comparator: second/third line daratumumab
            else {
              treatment_comp <- if (rand_tx_comp < rates_vector[1]) {2} #VRd
              else if (rand_tx_comp < rates_vector[1] + rates_vector[2]){3} #VTd
              else {4}#VCd
              
              l1_tx_comp <- treatment_comp
              tracker_comp <- 1
              cycle_track_comp <- 1
              current_line_comp <- 1
              cost_comp <- cost_matrix[tracker_comp, treatment_comp]
              
              toxic_comp <- if (rand_ae_comp < p_adverse_tx_vector[treatment_comp]){1} else {0} #Adverse Event
              date_adverse_l1_comp <- if (toxic_comp==1) {i} else {0} #AE cycle number saved out
            }
          }
          #NO ASCT
          else {
            #Comparator: No daratumumab
            if (scenario==3 | scenario==2){
              treatment_comp <- if (rand_tx_comp < rates_vector[6]) {24} #VRd
              else if (rand_tx_comp < rates_vector[6] + rates_vector[7]){20} #VMP
              else {21} #Rd
              
              l1_tx_comp <- treatment_comp
              tracker_comp <- 1
              cycle_track_comp <- 1
              current_line_comp <- 1
              cost_comp <- cost_matrix[tracker_comp, treatment_comp]
              
              toxic_comp <- if (rand_ae_comp < p_adverse_tx_vector[treatment_comp]){1} else {0} #Adverse Event
              date_adverse_l1_comp <- if (toxic_comp==1) {i} else {0} #AE cycle number saved out
            }
            #Comparator: second/third line daratumumab
            else {
              treatment_comp <- if (rand_tx_comp < rates_vector[6]) {24} #VRd
              else if (rand_tx_comp < rates_vector[6] + rates_vector[7]){20} #VMP
              else {21} #Rd
              
              l1_tx_comp <- treatment_comp
              tracker_comp <- 1
              cycle_track_comp <- 1
              current_line_comp <- 1
              cost_comp <- cost_matrix[tracker_comp, treatment_comp]
              
              toxic_comp <- if (rand_ae_comp < p_adverse_tx_vector[treatment_comp]){1} else {0} #Adverse Event
              date_adverse_l1_comp <- if (toxic_comp==1) {i} else {0} #AE cycle number saved out
            }
          }
        }
        #Patient got AE/Relapse on first line
        else if (prev_line_comp==1) {
          #Previous no daratumumab treatment
          if (scenario==3 | scenario==2){
            #VTd, VMP, VCd
            if (treatment_comp==3 | treatment_comp==4 | treatment_comp==20) {
              treatment_comp <- if (rand_tx_comp<rates_vector[9]) {7} #KRd
              else {8} #IxaRd
              
              tracker_comp <- 1
              cycle_track_comp <- 1
              current_line_comp <- 3
              cost_comp <- cost_matrix[tracker_comp, treatment_comp]
              l1_tx_comp <- l1_tx_comp
              
              toxic_comp <- if (rand_ae_comp < p_adverse_tx_vector[treatment_comp]){1} else {0} #Adverse Event
              date_adverse_l2_comp <- if (toxic_comp==1) {i} else {0} #AE cycle number saved out
            } 
            #VRd, Rd
            else if(treatment_comp==2 | treatment_comp==21 | treatment_comp==24) {
              treatment_comp <- if (venvd_elig==1) {9} #VenVd
              else if (rand_tx_comp<rates_vector[11]) {7} #KRd
              else if (rand_tx_comp<rates_vector[11]+rates_vector[12]) {10} #IsaKd
              else if (rand_tx_comp<rates_vector[11]+rates_vector[12]+rates_vector[13]) {11} #PomVd
              else if (rand_tx_comp<rates_vector[11]+rates_vector[12]+rates_vector[13]+rates_vector[14]) {8} #IxaRd
              else {12} #SVd
              
              tracker_comp <- 1
              cycle_track_comp <- 1
              current_line_comp <- 3
              cost_comp <- cost_matrix[tracker_comp, treatment_comp]
              l1_tx_comp <- l1_tx_comp
              
              toxic_comp <- if (rand_ae_comp < p_adverse_tx_vector[treatment_comp]){1} else {0} #Adverse Event
              date_adverse_l2_comp <- if (toxic_comp==1) {i} else {0} #AE cycle number saved out
            }
            #Error in first line treatment
            else {
              treatment_comp<-tracker_comp<-cycle_track_comp<-current_line_comp<-cost_comp<-"ERROR first line no dara"
            }
          }
          #1. scenario - Second/third line Dara vs no dara
          else { 
            #Dara on 2. line
            if (dara2l==1){
              treatment_comp <- if (rand_tx_comp < rates_vector[32]) {14} #DVd
              else if (rand_tx_comp < rates_vector[33] + rates_vector[32]) {15} #DKd 
              else if (rand_tx_comp < rates_vector[34] + rates_vector[32] + rates_vector[33]) {23} #DRd
              else {"ERROR"}
              
              tracker_comp <- 1
              cycle_track_comp <- 1
              current_line_comp <- 3
              cost_comp <- cost_matrix[tracker_comp, treatment_comp]
              l1_tx_comp <- l1_tx_comp
              
              toxic_comp <- if (rand_ae_comp < p_adverse_tx_vector[treatment_comp]){1} else {0} #Adverse Event
              date_adverse_l2_comp <- if (toxic_comp==1) {i} else {0}  #AE cycle number saved out
              
            }
            #No daratumumab on 2. line
            else {
              #previous VTD, VMP, VCD first line
              if(treatment_comp==3 |treatment_comp==20 | treatment_comp==4){ 
                treatment_comp <- if (rand_tx_comp<rates_vector[9]) {7} #KRd
                else {8} #IxaRd
                
                tracker_comp <- 1
                cycle_track_comp <- 1
                current_line_comp <- 3
                cost_comp <- cost_matrix[tracker_comp, treatment_comp]
                l1_tx_comp <- l1_tx_comp
                
                toxic_comp <- if (rand_ae_comp < p_adverse_tx_vector[treatment_comp]){1} else {0} #Adverse Event
                date_adverse_l2_comp <- if (toxic_comp==1) {i} else {0} #AE cycle number saved out
              }
              #Previous line was VRd, Rd
              else if (treatment_comp==2 | treatment_comp==21 | treatment_comp==24) {
                treatment_comp <- if (venvd_elig==1) {9} #VenVd
                else if (rand_tx_comp<rates_vector[11]) {7} #KRd
                else if (rand_tx_comp<rates_vector[11]+rates_vector[12]) {10} #IsaRd
                else if (rand_tx_comp<rates_vector[11]+rates_vector[12]+rates_vector[13]) {11} #PomVd
                else if (rand_tx_comp<rates_vector[11]+rates_vector[12]+rates_vector[13]+rates_vector[14]) {8} #IxaRd
                else {12} #SVd
                
                tracker_comp <- 1
                cycle_track_comp <- 1
                current_line_comp <- 3
                cost_comp <- cost_matrix[tracker_comp, treatment_comp]
                l1_tx_comp <- l1_tx_comp
                
                toxic_comp <- if (rand_ae_comp < p_adverse_tx_vector[treatment_comp]){1} else {0} #Adverse Event
                date_adverse_l2_comp <- if (toxic_comp==1) {i} else {0} #AE cycle number saved out
              }
            }
          }
        }
        #Patient got AE/Relapse on maintenance
        else if (prev_line_comp==2) {
          #Second ASCT eligible (Lenalidomide + 36m without relapse)
          if (treatment_comp==19 & cycle_track_comp>12 & asct2_comp!=1) {
            #No daratumumab treatment repeat
            if (scenario==3 | scenario==2){
              treatment_comp <- if (rand_tx_comp < rates_vector[1]) {2} #VRd
              else if (rand_tx_comp < rates_vector[1] + rates_vector[2]){3} #VTd
              else {4} #VCd
              
              l1_tx_comp <- treatment_comp
              tracker_comp <- 1
              cycle_track_comp <- 1
              current_line_comp <- 5
              asct2_comp <- 1
              cost_comp <- cost_matrix[tracker_comp, treatment_comp]
              
              toxic_comp <- if (rand_ae_comp < p_adverse_tx_vector[treatment_comp]){1} else {0} #Adverse Event
              date_adverse_2asct_comp <- if (toxic_comp==1) {i} else {0} #AE cycle number saved out
            }
            #no dara (only second/third, this is first line repeat)
            else {
              treatment_comp <- if (rand_tx_comp < rates_vector[1]) {2} #VRd
              else if (rand_tx_comp < rates_vector[1] + rates_vector[2]){3} #VTd
              else {4} #VCd
              
              l1_tx_comp <- treatment_comp
              tracker_comp <- 1
              cycle_track_comp <- 1
              current_line_comp <- 5
              asct2_comp <- 1
              cost_comp <- cost_matrix[tracker_comp, treatment_comp]
              
              toxic_comp <- if (rand_ae_comp < p_adverse_tx_vector[treatment_comp]){1} else {0} #Adverse Event
              date_adverse_2asct_comp <- if (toxic_comp==1) {i} else {0} #AE cycle number saved out
            }
          }
          #No 2. ASCT
          else {
            #2. 3. scenario - first line was not daratumumab
            if (scenario==3 | scenario==2){
              #VTd, VMP, VCd
              if (l1_tx_comp==3 | l1_tx_comp==4 | l1_tx_comp==20) {
                treatment_comp <- if (rand_tx_comp<rates_vector[9]) {7} #KRd
                else {8} #IxaRd
                
                tracker_comp <- 1
                cycle_track_comp <- 1
                current_line_comp <- 3
                cost_comp <- cost_matrix[tracker_comp, treatment_comp]
                l1_tx_comp <- l1_tx_comp
                
                toxic_comp <- if (rand_ae_comp < p_adverse_tx_vector[treatment_comp]){1} else {0} #Adverse Event
                date_adverse_l2_comp <- if (toxic_comp==1) {i} else {0} #AE cycle number saved out
              }
              #VRd, Rd
              else if(l1_tx_comp==2 | l1_tx_comp==21 | l1_tx_comp==24) {
                treatment_comp <- if (venvd_elig==1) {9} #VenVd
                else if (rand_tx_comp<rates_vector[11]) {7} #KRd
                else if (rand_tx_comp<rates_vector[11]+rates_vector[12]) {10} #IsaKd
                else if (rand_tx_comp<rates_vector[11]+rates_vector[12]+rates_vector[13]) {11} #PomVd
                else if (rand_tx_comp<rates_vector[11]+rates_vector[12]+rates_vector[13]+rates_vector[14]) {8} #IxaRd
                else {12} #SVd
                
                tracker_comp <- 1
                cycle_track_comp <- 1
                current_line_comp <- 3
                cost_comp <- cost_matrix[tracker_comp, treatment_comp]
                l1_tx_comp <- l1_tx_comp
                
                toxic_comp <- if (rand_ae_comp < p_adverse_tx_vector[treatment_comp]){1} else {0} #Adverse Event
                date_adverse_l2_comp <- if (toxic_comp==1) {i} else {0} #AE cycle number saved out
              }
              #error if probabilities are problematic
              else {
                treatment_comp<-tracker_comp<-cycle_track_comp<-current_line_comp<-cost_comp<-l1_tx_comp <- "ERROR comp Vrd"
              }
            }
            #1. scenario - second/third line dara
            else {
              #Daratumumab on second line
              if (dara2l==1){
                treatment_comp <- if (rand_tx_comp < rates_vector[32]) {14} #DVd
                else if (rand_tx_comp < rates_vector[33] + rates_vector[32]) {15} #DKd
                else if (rand_tx_comp < rates_vector[34] + rates_vector[32] + rates_vector[33]) {23} #DRd (second)
                else {"ERROR"}
                
                tracker_comp <- 1
                cycle_track_comp <- 1
                current_line_comp <- 3
                cost_comp <- cost_matrix[tracker_comp, treatment_comp]
                l1_tx_comp <- l1_tx_comp
                
                toxic_comp <- if (rand_ae_comp < p_adverse_tx_vector[treatment_comp]){1} else {0} #Adverse Event
                date_adverse_l2_comp <- if (toxic_comp==1) {i} else {0} #AE cycle number saved out
              }
              #No second line dara
              else {
                #previous VTd, VMP, VCd
                if(l1_tx_comp==3 |l1_tx_comp==20 | l1_tx_comp==4){
                  treatment_comp <- if (rand_tx_comp<rates_vector[9]) {7} #KRd
                  else {8} # IxaRd
                  
                  tracker_comp <- 1
                  cycle_track_comp <- 1
                  current_line_comp <- 3
                  cost_comp <- cost_matrix[tracker_comp, treatment_comp]
                  l1_tx_comp <- l1_tx_comp
                  
                  toxic_comp <- if (rand_ae_comp < p_adverse_tx_vector[treatment_comp]){1} else {0} #Adverse Event
                  date_adverse_l2_comp <- if (toxic_comp==1) {i} else {0} #AE cycle number saved out
                } 
                #previous VRd, Rd
                else if (l1_tx_comp==2 | l1_tx_comp==21 | l1_tx_comp==24) {
                  treatment_comp <- if (venvd_elig==1) {9} #VenVd
                  else if (rand_tx_comp<rates_vector[11]) {7} #KRd
                  else if (rand_tx_comp<rates_vector[11]+rates_vector[12]) {10} #IsaKd
                  else if (rand_tx_comp<rates_vector[11]+rates_vector[12]+rates_vector[13]) {11} #PomVd
                  else if (rand_tx_comp<rates_vector[11]+rates_vector[12]+rates_vector[13]+rates_vector[14]) {8} #IxaRd
                  else {12} #SVd
                  
                  tracker_comp <- 1
                  cycle_track_comp <- 1
                  current_line_comp <- 3
                  cost_comp <- cost_matrix[tracker_comp, treatment_comp]
                  l1_tx_comp <- l1_tx_comp
                  
                  toxic_comp <- if (rand_ae_comp < p_adverse_tx_vector[treatment_comp]){1} else {0} #Adverse Event
                  date_adverse_l2_comp <- if (toxic_comp==1) {i} else {0} #AE cycle number saved out
                }
              }
            }
          }
        } 
        #Patient got Relapse on second line
        else if (prev_line_comp==3) {
          #No daratumumab on comparator
          if (scenario==3 | scenario==2){
            treatment_comp <- if (venvd_elig==1) {9} #VenVd
            else if (rand_tx_comp<rates_vector[29]) {22} #IsaKd helyett dummy
            else if (rand_tx_comp < rates_vector[29]+rates_vector[30]) {22} #IsaPd helyett dummy
            else {22} #SVd helyett dummy
            
            
            tracker_comp <- 1
            cycle_track_comp <- 1
            current_line_comp <- 4
            cost_comp <- cost_matrix[tracker_comp, treatment_comp]
            l1_tx_comp <- l1_tx_comp
            response_comp <- 5
            
            toxic_comp <- if (rand_ae_comp < p_adverse_tx_vector[treatment_comp]){1} else {0} #Adverse Event
            date_adverse_l3_comp <- if (toxic_comp==1) {i} else {0} #AE cycle number saved out
          }
          #Second/third line daratumumab on comparator
          else if (scenario==1) {
            #recieved daratumumab on 2. line
            if (dara2l==1) {
              treatment_comp <- if (venvd_elig==1){9} #VenVd
              else {22} #SVd helyett dummy
              
              tracker_comp <- 1
              cycle_track_comp <- 1
              current_line_comp <- 4
              cost_comp <- cost_matrix[tracker_comp, treatment_comp]
              l1_tx_comp <- l1_tx_comp
              response_comp <- 5
              
              toxic_comp <- if (rand_ae_comp < p_adverse_tx_vector[treatment_comp]){1} else {0} #Adverse Event
              date_adverse_l3_comp <- if (toxic_comp==1) {i} else {0} #AE cycle number saved out
              
            }
            #did not recieve daratumumab on 2. line
            else {
              #2. line isatuximab, cannot recieve daratumumab on 3. line
              if (treatment_comp==10){
                treatment_comp <- if (venvd_elig==1){9} #VenVd
                else {22} #SVd helyett dummy
                
                tracker_comp <- 1
                cycle_track_comp <- 1
                current_line_comp <- 4
                cost_comp <- cost_matrix[tracker_comp, treatment_comp]
                l1_tx_comp <- l1_tx_comp
                response_comp <- 5
                
                toxic_comp <- if (rand_ae_comp < p_adverse_tx_vector[treatment_comp]){1} else {0} #Adverse Event
                date_adverse_l3_comp <- if (toxic_comp==1) {i} else {0} #AE cycle number saved out
              }
              #did not recieve mAB on 2. line, will recieve daratumumab
              else{
                treatment_comp <- if (rand_tx_int<rates_vector[26]) {16} #DPd
                else if (rand_tx_comp < rates_vector[26]+rates_vector[27]){14} #DVd
                else if (rand_tx_comp < rates_vector[26] + rates_vector[27] + rates_vector[28]) {15} #DKd
                else {"ERROR"}
                
                tracker_comp <- 1
                cycle_track_comp <- 1
                current_line_comp <- 4
                cost_comp <- cost_matrix[tracker_comp, treatment_comp]
                l1_tx_comp <- l1_tx_comp
                
                toxic_comp <- if (rand_ae_comp < p_adverse_tx_vector[treatment_comp]){1} else {0} #Adverse Event
                date_adverse_l3_comp <- if (toxic_comp==1) {i} else {0} #AE cycle number saved out
              }
            }
          }
          #Error is scenario
          else {
            treatment_comp<-current_line_comp<-cycle_track_comp<-tracker_comp<-cost_comp <-toxic_comp <-l1_tx_comp <- "ERROR - 3. line no scenario was given"
          }
        }
        #Patient got relapse on third line
        else if (prev_line_comp==4) {
          treatment_comp <- 22
          tracker_comp <- 1
          current_line_comp <- prev_line_comp
          cycle_track_comp <- cycle_track_comp + 1
          toxic_comp <- 0
          response_comp <- 5
          cost_comp <- cost_matrix[tracker_comp, treatment_comp]
          l1_tx_comp <- l1_tx_comp
        }
        #Patient got relapse on maintenance after second ASCT
        else if (prev_line_comp==5) {
          #PRevious line was not daratumumab
          if (scenario==3 | scenario==2){
            #VTd, VMP, VCd
            if (treatment_comp==3 | treatment_comp==4 | treatment_comp==20) {
              treatment_comp <- if (rand_tx_comp<rates_vector[9]) {7} #KRd
              else {8} #IxaRd
              
              tracker_comp <- 1
              cycle_track_comp <- 1
              current_line_comp <- 3
              cost_comp <- cost_matrix[tracker_comp, treatment_comp]
              l1_tx_comp <- l1_tx_comp
              
              toxic_comp <- if (rand_ae_comp < p_adverse_tx_vector[treatment_comp]){1} else {0} #Adverse Event
              date_adverse_l2_comp <- if (toxic_comp==1) {i} else {0} #AE cycle number saved out
            } 
            #VRd, Rd
            else if(treatment_comp==2 | treatment_comp==21 | treatment_comp==24) {
              treatment_comp <- if (venvd_elig==1) {9} #VenVd
              else if (rand_tx_comp<rates_vector[11]) {7} #KRd
              else if (rand_tx_comp<rates_vector[11]+rates_vector[12]) {10} #IsaKd
              else if (rand_tx_comp<rates_vector[11]+rates_vector[12]+rates_vector[13]) {11} #PomVd
              else if (rand_tx_comp<rates_vector[11]+rates_vector[12]+rates_vector[13]+rates_vector[14]) {8} #IxaRd
              else {12} #SVd
              
              tracker_comp <- 1
              cycle_track_comp <- 1
              current_line_comp <- 3
              cost_comp <- cost_matrix[tracker_comp, treatment_comp]
              l1_tx_comp <- l1_tx_comp
              
              toxic_comp <- if (rand_ae_comp < p_adverse_tx_vector[treatment_comp]){1} else {0} #Adverse Event
              date_adverse_l2_comp <- if (toxic_comp==1) {i} else {0} #AE cycle number saved out
            }
            #error in treatment regiment
            else {
              treatment_comp<-tracker_comp<-cycle_track_comp<-current_line_comp<-cost_comp<-"ERROR"
            }
          }
          #1. scenario, second/third line daratumumab comparator
          else {
            #daratumumab on 2. line
            if (dara2l==1){
              treatment_comp <- if (rand_tx_comp < rates_vector[32]) {14} #DVd
              else if (rand_tx_comp < rates_vector[33] + rates_vector[32]) {15} #DKd 
              else if (rand_tx_comp < rates_vector[34] + rates_vector[32] + rates_vector[33]) {23} #DRd second line
              else {"ERROR"}
              
              tracker_comp <- 1
              cycle_track_comp <- 1
              current_line_comp <- 3
              cost_comp <- cost_matrix[tracker_comp, treatment_comp]
              l1_tx_comp <- l1_tx_comp
              
              toxic_comp <- if (rand_ae_comp < p_adverse_tx_vector[treatment_comp]){1} else {0} #Adverse Event
              date_adverse_l2_comp <- if (toxic_comp==1) {i} else {0} #AE cycle number saved out
            } 
            #no daratumumab on 2. line
            else {
              #previous line VTd, VMP, VCd 
              if(treatment_comp==3 |treatment_comp==20 | treatment_comp==4){
                treatment_comp <- if (rand_tx_comp<rates_vector[9]) {7} #KRd
                else {8} #IxaRd
                
                tracker_comp <- 1
                cycle_track_comp <- 1
                current_line_comp <- 3
                cost_comp <- cost_matrix[tracker_comp, treatment_comp]
                l1_tx_comp <- l1_tx_comp
                
                toxic_comp <- if (rand_ae_comp < p_adverse_tx_vector[treatment_comp]){1} else {0} #Adverse Event
                date_adverse_l2_comp <- if (toxic_comp==1) {i} else {0} #AE cycle number saved out
              } 
              #previous VRd, Rd
              else if (treatment_comp==2 | treatment_comp==21 | treatment_comp==24) {
                treatment_comp <- if (venvd_elig==1) {9} #VenVd
                else if (rand_tx_comp<rates_vector[11]) {7} #KRd
                else if (rand_tx_comp<rates_vector[11]+rates_vector[12]) {10} #IsaKd
                else if (rand_tx_comp<rates_vector[11]+rates_vector[12]+rates_vector[13]) {11} #PomVd
                else if (rand_tx_comp<rates_vector[11]+rates_vector[12]+rates_vector[13]+rates_vector[14]) {8} #IxaRd
                else {12} #SVd
                
                tracker_comp <- 1
                cycle_track_comp <- 1
                current_line_comp <- 3
                cost_comp <- cost_matrix[tracker_comp, treatment_comp]
                l1_tx_comp <- l1_tx_comp
                
                toxic_comp <- if (rand_ae_comp < p_adverse_tx_vector[treatment_comp]){1} else {0} #Adverse Event
                date_adverse_l2_comp <- if (toxic_comp==1) {i} else {0} #AE cycle number saved out
              }
            }
          }
        } 
        #error in prev_line
        else {
          treatment_comp<-current_line_comp<-cycle_track_comp<-tracker_comp<-cost_comp <-response_comp <-toxic_comp <-date_adverse_maint_comp<-l1_tx_comp <- "ERROR-treatment line"
        }
      }
      #Treatment change: end of induction
      else if (tracker_comp==max_cycles_vector[prev_line_comp+1]) {
        #Previous ASCT (1 or 2, count as first line)
        if ((prev_line_comp==1 | prev_line_comp==5) & asct_elig==1) {
          #bortezomib maintenance
          if (rand_tx_comp<rates_vector[35]){
            treatment_comp<-18
            current_line_comp<-2
            cycle_track_comp<-1
            tracker_comp<-1
            response_comp<-response_comp
            cost_comp <- cost_matrix[tracker_comp, treatment_comp]
            l1_tx_comp <- l1_tx_comp
            
            toxic_comp <- if (rand_ae_comp < p_adverse_tx_vector[treatment_comp]){1} else {0} #Adverse Event
            date_adverse_maint_comp <- if (toxic_comp==1) {i} else {0} #AE cycle number saved out
          }
          #lenalidomide maintenance
          else {
            treatment_comp<-19
            current_line_comp<-2
            cycle_track_comp<-1
            tracker_comp<-1
            response_comp<-response_comp
            cost_comp <- cost_matrix[tracker_comp, treatment_comp]
            l1_tx_comp <- l1_tx_comp
            
            toxic_comp <- if (rand_ae_comp < p_adverse_tx_vector[treatment_comp]){1} else {0} #Adverse Event
            date_adverse_maint_comp <- if (toxic_comp==1) {i} else {0} #AE cycle number saved out
          }
        } 
        #no maintenance after induction ends
        else {
          tracker_comp <- 0
          cycle_track_comp <- cycle_track_comp+1
          treatment_comp <- treatment_comp
          current_line_comp <- prev_line_comp
          response_comp <- response_comp
          toxic_comp <- toxic_comp
          cost_comp <- 0
          l1_tx_comp <- l1_tx_comp
        }
      }
      #NO TREATMENT CHANGE - Still no therapy
      else if (tracker_comp==0) {
        treatment_comp <- treatment_comp
        tracker_comp <- tracker_comp
        cycle_track_comp <- cycle_track_comp+1
        current_line_comp <- prev_line_comp
        response_comp <- response_comp
        toxic_comp <- toxic_comp
        cost_comp <- 0
        response_comp <- response_comp  
        l1_tx_comp <- l1_tx_comp
      }
      #On lenalidomide maintenance response can improve
      else if (treatment_comp==19){
        #PD or CR cannot improve
        if (response_comp==6 | response_comp==1){
          tracker_comp <- tracker_comp+1
          cycle_track_comp <- cycle_track_comp+1
          treatment_comp <- treatment_comp
          current_line_comp <- prev_line_comp
          response_comp <- response_comp
          toxic_comp <- toxic_comp
          cost_comp <- cost_matrix[tracker_comp, treatment_comp]
          l1_tx_comp <- l1_tx_comp
        }
        #other response
        else {
          #Response improves on lenalidomide maintenance
          if (rand_resp_comp < maint_better){
            response_comp <- response_comp-1
            tracker_comp <- tracker_comp+1
            cycle_track_comp <- cycle_track_comp+1
            treatment_comp <- treatment_comp
            current_line_comp <- prev_line_comp
            toxic_comp <- toxic_comp
            cost_comp <- cost_matrix[tracker_comp, treatment_comp]
            l1_tx_comp <- l1_tx_comp
          }
          #no change on lenalidomide
          else {
            tracker_comp <- tracker_comp+1
            cycle_track_comp <- cycle_track_comp+1
            treatment_comp <- treatment_comp
            current_line_comp <- prev_line_comp
            response_comp <- response_comp
            toxic_comp <- toxic_comp
            cost_comp <- cost_matrix[tracker_comp, treatment_comp]
            l1_tx_comp <- l1_tx_comp
          }
        }
      }
      #NO treatment change - Still on treatment
      else {
        tracker_comp <- tracker_comp+1
        cycle_track_comp <- cycle_track_comp+1
        treatment_comp <- treatment_comp
        current_line_comp <- prev_line_comp
        response_comp <- response_comp
        toxic_comp <- toxic_comp
        cost_comp <- cost_matrix[tracker_comp, treatment_comp]
        l1_tx_comp <- l1_tx_comp
      }
      
      #IF patient had AE or relapse last cycle, now will recieve a response for the new therapy
      response_comp <- if ((toxic_comp_0==1 | relapse_comp==1) & prev_line_comp!=4 & treatment_comp!=22){
        if (rand_resp_comp < p_cr_vector[treatment_comp]) {1} #CR
        else if(rand_resp_comp < p_cr_vector[treatment_comp] + p_vgpr_vector[treatment_comp]) {2} #VGPR
        else if(rand_resp_comp < p_cr_vector[treatment_comp] + p_vgpr_vector[treatment_comp] + p_pr_vector[treatment_comp]) {3} #PR
        else if(rand_resp_comp < p_cr_vector[treatment_comp] + p_vgpr_vector[treatment_comp] + p_pr_vector[treatment_comp] + p_mr_vector[treatment_comp]) {4} #MR
        else if(rand_resp_comp < p_cr_vector[treatment_comp] + p_vgpr_vector[treatment_comp] + p_pr_vector[treatment_comp] + p_mr_vector[treatment_comp] + p_sd_vector[treatment_comp]) {5} #SD
        else if(rand_resp_comp < p_cr_vector[treatment_comp] + p_vgpr_vector[treatment_comp] + p_pr_vector[treatment_comp] + p_mr_vector[treatment_comp] + p_sd_vector[treatment_comp] + p_pd_vector[treatment_comp]) {6} #PD
        else {"ERROR response"}
      }
      else {response_comp} #no change, no event
    }
  }
  
  return(c(treatment_int, tracker_int, cycle_track_int, current_line_int, response_int, toxic_int, cost_int, 
           treatment_comp, tracker_comp, cycle_track_comp, current_line_comp, response_comp, toxic_comp, cost_comp, 
           l1_tx_int, l1_tx_comp, 
           date_adverse_l1_comp, date_adverse_2asct_comp, date_adverse_maint_comp, date_adverse_l2_comp, date_adverse_l3_comp, 
           date_adverse_l1_int, date_adverse_2asct_int, date_adverse_maint_int, date_adverse_l2_int, date_adverse_l3_int,
           asct2_int, asct2_comp))
}

#---- II.2. Outcome function -----

outcome <- function (i, current_line_int, current_line_comp, treatment_vector_line_int, treatment_vector_line_comp, response_int, response_comp, cytrisk, #patient data (either previous cycle or constant)
                     pfs_matrix, os_matrix,
                     seed_int_2, seed_comp_2){
  if (response_int==7 & response_comp==7){ #dead on both arm, 0
    response_int <- response_int
    response_comp <- response_comp
    relapse_int <- 0
    relapse_comp <- 0
    
    date_death_int <- date_prog_l1_int <- date_prog_2asct_int <- date_prog_l2_int <- date_prog_l3_int <- date_relapse_l1_int <- date_relapse_2asct_int <- date_relapse_l2_int <- date_relapse_l3_int <-  date_death_comp <- date_prog_l1_comp <- date_prog_2asct_comp <- date_prog_l2_comp <- date_prog_l3_comp <- date_relapse_l1_comp <- date_relapse_2asct_comp <- date_relapse_l2_comp <- date_relapse_l3_comp <- 0
  } 
  #patient is alive at least on 1 arm
  else {
    #check similarity and arrange random numbers
    if (response_int==response_comp) {
      set.seed(seed_int_2)
      rand_pfs_int <- rand_pfs_comp <- runif(1)
      rand_os_int <- rand_os_comp <- runif(1)
    }
    else {
      set.seed(seed_comp_2)
      rand_os_comp <-  runif(1)
      rand_pfs_comp <-  runif(1)
      
      set.seed(seed_int_2)
      rand_pfs_int <- runif(1)
      rand_os_int <- runif(1)
    }
    
    #Dead on intervention
    if (response_int==7) {
      response_int <- response_int
      relapse_int <- 0
      
      date_death_int <- date_prog_l1_int <- date_prog_2asct_int <- date_prog_l2_int <- date_prog_l3_int <- date_relapse_l1_int <- date_relapse_2asct_int <- date_relapse_l2_int <- date_relapse_l3_int <- 0
      
    }
    ######################################################################Intervention outcomes###################################################################################
    else{
      #Patient dies
      if (rand_os_int < os_matrix[response_int,current_line_int]){
        response_int <- 7
        relapse_int <- 0
        date_death_int <- i
        date_prog_l1_int <- date_prog_2asct_int <- date_prog_l2_int <- date_prog_l3_int <- date_relapse_l1_int <- date_relapse_2asct_int <- date_relapse_l2_int <- date_relapse_l3_int <- 0
      }
      #Patient lives
      else {
        #Patient progression
        if (rand_pfs_int < pfs_matrix[response_int,current_line_int]){
          #Was on PD, Relapse
          if (response_int==6){
            response_int <- response_int
            relapse_int <- 1
            date_death_int <- date_prog_l1_int <- date_prog_l2_int <- date_prog_l3_int <- date_prog_2asct_int <- 0
            #Relapse on first line
            if(treatment_vector_line_int==1) {
              date_relapse_l1_int <- i
              date_relapse_l2_int <- date_relapse_l3_int <- date_relapse_2asct_int <- 0 
            }
            #Relapse on maintenance
            else if(treatment_vector_line_int==2){
              date_relapse_l1_int <- i
              date_relapse_l2_int <- date_relapse_l3_int <- date_relapse_2asct_int <- 0
            }
            #Relapse on second line
            else if(treatment_vector_line_int==3){
              date_relapse_l2_int <- i
              date_relapse_l1_int <- date_relapse_l3_int <- date_relapse_2asct_int <- 0
            }
            #Relapse on third line
            else if(treatment_vector_line_int==4){
              date_relapse_l3_int <- i
              date_relapse_l1_int <- date_relapse_l2_int <- date_relapse_2asct_int <- 0
            }
            #Relapse after 2. ASCT (count as first line)
            else if(treatment_vector_line_int==5){
              date_relapse_2asct_int <- i
              date_relapse_l1_int <- date_relapse_l2_int <- date_relapse_l3_int <- 0
            }
            #error in treatment line
            else {date_relapse_l1_int <- date_relapse_2asct_int <- date_relapse_l2_int <- date_relapse_l3_int <- "ERROR"}
          }
          #High cytogenic risk - progression is relapse in these cases
          else if (cytrisk==1) {
            response_int <- 6
            relapse_int <- 1
            date_death_int <- 0
            #Progression/relapse on first line
            if(treatment_vector_line_int==1) {
              date_prog_l1_int <- i
              date_prog_l2_int <- date_prog_l3_int <- date_prog_2asct_int <- 0
              date_relapse_l1_int <- i
              date_relapse_l2_int <- date_relapse_l3_int <- date_relapse_2asct_int <- 0 
            }
            #Progression/relapse on maintenance
            else if(treatment_vector_line_int==2){
              date_prog_l1_int <- i
              date_prog_l2_int <- date_prog_l3_int <- date_prog_2asct_int <- 0
              date_relapse_l1_int <- i
              date_relapse_l2_int <- date_relapse_l3_int <- date_relapse_2asct_int <- 0
            }
            #Progression/relapse on second line
            else if(treatment_vector_line_int==3){
              date_prog_l2_int <- i
              date_prog_l1_int <- date_prog_l3_int <- date_prog_2asct_int <- 0
              date_relapse_l2_int <- i
              date_relapse_l1_int <- date_relapse_l3_int <- date_relapse_2asct_int <- 0
            }
            #Progression/relapse on third line
            else if(treatment_vector_line_int==4){
              date_prog_l3_int <- i
              date_prog_l1_int <- date_prog_l2_int <- date_prog_2asct_int <- 0
              date_relapse_l3_int <- i
              date_relapse_l1_int <- date_relapse_l2_int <- date_relapse_2asct_int <- 0
            }
            #Progression/relapse after 2. ASCT (count as first line)
            else if(treatment_vector_line_int==5){
              date_prog_2asct_int <- i
              date_prog_l1_int <- date_prog_l2_int <- date_prog_l3_int <- 0
              date_relapse_2asct_int <- i
              date_relapse_l1_int <- date_relapse_l2_int <- date_relapse_l3_int <- 0
            }
            #error in treatment line
            else {date_relapse_l1_int <- date_relapse_2asct_int <- date_relapse_l2_int <- date_relapse_l3_int <- "ERROR"}
          }
          #Progression to PD
          else {
            response_int <- 6
            relapse_int <- 0
            date_death_int <- date_relapse_l1_int <- date_relapse_l2_int <- date_relapse_l3_int <- date_relapse_2asct_int <- 0
            #PD on first line
            if(treatment_vector_line_int==1) {
              date_prog_l1_int <- i
              date_prog_l2_int <- date_prog_l3_int <- date_prog_2asct_int <- 0
            }
            #PD on maintenance
            else if(treatment_vector_line_int==2){
              date_prog_l1_int <- i
              date_prog_l2_int <- date_prog_l3_int <- date_prog_2asct_int <- 0
            }
            #PD on second line
            else if(treatment_vector_line_int==3){
              date_prog_l2_int <- i
              date_prog_l1_int <- date_prog_l3_int <- date_prog_2asct_int <- 0
            }
            #PD on third line
            else if(treatment_vector_line_int==4){
              date_prog_l3_int <- i
              date_prog_l1_int <- date_prog_l2_int <- date_prog_2asct_int <- 0
            }
            #PD on 2. ASCT (count as first line)
            else if(treatment_vector_line_int==5){
              date_prog_2asct_int <- i
              date_prog_l1_int <- date_prog_l2_int <- date_prog_l3_int <- 0
            }
            #error in treatment line
            else {date_prog_l1_int <- date_prog_2asct_int <- date_prog_l2_int <- date_prog_l3_int <- "ERROR"}
          }
        }
        #NO PROGRESSION
        else {
          response_int <- response_int
          relapse_int <- 0
          date_death_int <- date_prog_l1_int <- date_prog_2asct_int <- date_prog_l2_int <- date_prog_l3_int <- date_relapse_l1_int <- date_relapse_2asct_int <- date_relapse_l2_int <- date_relapse_l3_int <- 0
        }
      }  
    }
    
    #Dead on comparator
    if (response_comp==7){
      response_comp <- response_comp
      relapse_comp <- 0
      date_death_comp <- date_prog_l1_comp <- date_prog_2asct_comp <- date_prog_l2_comp <- date_prog_l3_comp <- date_relapse_l1_comp <- date_relapse_2asct_comp <- date_relapse_l2_comp <- date_relapse_l3_comp <- 0
    }
    ##############################################################################Comparator outcomes####################################################################
    else {
      #Patient dies on comparator
      if (rand_os_comp < os_matrix[response_comp,current_line_comp]){
        response_comp <- 7
        relapse_comp <- 0
        date_death_comp <- i
        date_prog_l1_comp <- date_prog_2asct_comp <- date_prog_l2_comp <- date_prog_l3_comp <- date_relapse_l1_comp <- date_relapse_2asct_comp <- date_relapse_l2_comp <- date_relapse_l3_comp <- 0
      }
      #Patient lives
      else {
        #Patient progression or Relapse
        if (rand_pfs_comp < pfs_matrix[response_comp,current_line_comp]){
          #Relapse
          if (response_comp==6){
            response_comp <- response_comp
            relapse_comp <- 1
            date_death_comp <- date_prog_l1_comp <- date_prog_l2_comp <- date_prog_l3_comp <- date_prog_2asct_comp <- 0
            #Relapse on first line
            if(treatment_vector_line_comp==1) {
              date_relapse_l1_comp <- i
              date_relapse_l2_comp <- date_relapse_l3_comp <- date_relapse_2asct_comp <- 0 
            }
            #Relapse on maintenance
            else if(treatment_vector_line_comp==2){
              date_relapse_l1_comp <- i
              date_relapse_l2_comp <- date_relapse_l3_comp <- date_relapse_2asct_comp <- 0
            }
            #Relapse on second line
            else if(treatment_vector_line_comp==3){
              date_relapse_l2_comp <- i
              date_relapse_l1_comp <- date_relapse_l3_comp <- date_relapse_2asct_comp <- 0
            }
            #Relapse on third line
            else if(treatment_vector_line_comp==4){
              date_relapse_l3_comp <- i
              date_relapse_l1_comp <- date_relapse_l2_comp <- date_relapse_2asct_comp <- 0
            }
            #Relapse on 2. ASCT (count as first line)
            else if(treatment_vector_line_comp==5){
              date_relapse_2asct_comp <- i
              date_relapse_l1_comp <- date_relapse_l2_comp <- date_relapse_l3_comp <- 0
            }
            #error in treatment line
            else {date_relapse_l1_comp <- date_relapse_l2_comp <- date_relapse_l3_comp <- date_relapse_2asct_comp <- "ERROR"}
          }
          #High cytogenic risk - progression is relapse in these cases
          else if (cytrisk==1) {
            response_comp <- 6
            relapse_comp <- 1
            date_death_comp <- 0
            #Progression/relapse on first line
            if(treatment_vector_line_comp==1) {
              date_prog_l1_comp <- i
              date_prog_l2_comp <- date_prog_l3_comp <- date_prog_2asct_comp <- 0
              date_relapse_l1_comp <- i
              date_relapse_l2_comp <- date_relapse_l3_comp <- date_relapse_2asct_comp <- 0 
            }
            #Progression/relapse on maintenance
            else if(treatment_vector_line_comp==2){
              date_prog_l1_comp <- i
              date_prog_l2_comp <- date_prog_l3_comp <- date_prog_2asct_comp <- 0
              date_relapse_l1_comp <- i
              date_relapse_l2_comp <- date_relapse_l3_comp <- date_relapse_2asct_comp <- 0
            }
            #Progression/relapse on second line
            else if(treatment_vector_line_comp==3){
              date_prog_l2_comp <- i
              date_prog_l1_comp <- date_prog_l3_comp <- date_prog_2asct_comp <- 0
              date_relapse_l2_comp <- i
              date_relapse_l1_comp <- date_relapse_l3_comp <- date_relapse_2asct_comp <- 0
            }
            #Progression/relapse on third line
            else if(treatment_vector_line_comp==4){
              date_prog_l3_comp <- i
              date_prog_l1_comp <- date_prog_l2_comp <- date_prog_2asct_comp <- 0
              date_relapse_l3_comp <- i
              date_relapse_l1_comp <- date_relapse_l2_comp <- date_relapse_2asct_comp <- 0
            }
            #Progression/relapse on 2. ASCT (count as first line)
            else if(treatment_vector_line_comp==5){
              date_prog_2asct_comp <- i
              date_prog_l1_comp <- date_prog_l2_comp <- date_prog_l3_comp <- 0
              date_relapse_2asct_comp <- i
              date_relapse_l1_comp <- date_relapse_l2_comp <- date_relapse_l3_comp <- 0
            }
            #error in treatment line
            else {date_prog_l1_comp <- date_prog_2asct_comp <- date_prog_l2_comp <- date_prog_l3_comp <- date_relapse_l1_comp <- date_relapse_2asct_comp <- date_relapse_l2_comp <- date_relapse_l3_comp <- "ERROR"}
          }
          #Progression to PD
          else {
            response_comp <- 6
            relapse_comp <- 0
            date_death_comp <- date_relapse_l1_comp <- date_relapse_l2_comp <- date_relapse_l3_comp <- date_relapse_2asct_comp <- 0
            #PD on first line
            if(treatment_vector_line_comp==1) {
              date_prog_l1_comp <- i
              date_prog_l2_comp <- date_prog_l3_comp <- date_prog_2asct_comp <- 0
            }
            #PD on maintenance
            else if(treatment_vector_line_comp==2){
              date_prog_l1_comp <- i
              date_prog_l2_comp <- date_prog_l3_comp <- date_prog_2asct_comp <- 0
            }
            #PD on second line
            else if(treatment_vector_line_comp==3){
              date_prog_l2_comp <- i
              date_prog_l1_comp <- date_prog_l3_comp <- date_prog_2asct_comp <- 0
            }
            #PD on third line
            else if(treatment_vector_line_comp==4){
              date_prog_l3_comp <- i
              date_prog_l1_comp <- date_prog_l2_comp <- date_prog_2asct_comp <- 0
            }
            #PD on 2. ASCT (count as first line)
            else if(treatment_vector_line_comp==5){
              date_prog_2asct_comp <- i
              date_prog_l1_comp <- date_prog_l2_comp <- date_prog_l3_comp <- 0
            }
            #error in treatment line
            else {date_prog_l1_comp <- date_prog_2asct_comp <- date_prog_l2_comp <- date_prog_l3_comp <- "ERROR"}
          }
        } 
        #No progression
        else {
          response_comp <- response_comp
          relapse_comp <- 0
          date_death_comp <- date_prog_l1_comp <- date_prog_2asct_comp <- date_prog_l2_comp <- date_prog_l3_comp <- date_relapse_l1_comp <- date_relapse_2asct_comp <- date_relapse_l2_comp <- date_relapse_l3_comp <- 0
        }
      }
    } 
  }
  
  return(c(response_int, response_comp, relapse_int, relapse_comp, 
           date_death_int, date_prog_l1_int, date_prog_2asct_int, date_prog_l2_int, date_prog_l3_int, date_relapse_l1_int, date_relapse_2asct_int, date_relapse_l2_int, date_relapse_l3_int, 
           date_death_comp, date_prog_l1_comp, date_prog_2asct_comp, date_prog_l2_comp, date_prog_l3_comp, date_relapse_l1_comp, date_relapse_2asct_comp, date_relapse_l2_comp, date_relapse_l3_comp))
  
}