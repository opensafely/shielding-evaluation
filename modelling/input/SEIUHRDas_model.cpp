//##############################################################################################
//# SEIUHRD age-structured, stratified-contact compartment model, age profiles: u, y, h, m
		 
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List SEIUHRD(List pars){ 

    // array sizes
    const int nt = as<int>(pars["nt"]); 
    const int na = as<int>(pars["na"]);
    const int nw = as<int>(pars["nw"]);
    // vector indices for start of week
    const IntegerVector iw = as<IntegerVector>(pars["iw1"]) - 1;
    
    // Contact matrices
    //const NumericVector cm   = as<NumericVector>(pars["cm"]);  //check if use of cm nec
    const NumericVector cm_0 = as<NumericVector>(pars["cm_0"]);
    const NumericVector cm_1 = as<NumericVector>(pars["cm_1"]);
    const int    cmdim1 = as<int>(pars["cmdim1"]);
    const int    cmdim2 = as<int>(pars["cmdim2"]);
    const double ocmdim = std::pow(cmdim1*cmdim2,-1);
    
    // States(age,time) = 0 - need initialise S(na,0) etc
    NumericMatrix S_0(na,nt);
    NumericMatrix S_1(na,nt);
    NumericMatrix E_0(na,nt);
    NumericMatrix E_1(na,nt);
    NumericMatrix I_0(na,nt);
    NumericMatrix I_1(na,nt);
    NumericMatrix U_0(na,nt);
    NumericMatrix U_1(na,nt);
    NumericMatrix C1_0(na,nt);
    NumericMatrix C1_1(na,nt);
    NumericMatrix C2_0(na,nt);
    NumericMatrix C2_1(na,nt);
    NumericMatrix C3_0(na,nt);
    NumericMatrix C3_1(na,nt);
    NumericMatrix C4_0(na,nt);
    NumericMatrix C4_1(na,nt);
    NumericMatrix C5_0(na,nt);
    NumericMatrix C5_1(na,nt);
    NumericMatrix H1_0(na,nt);
    NumericMatrix H1_1(na,nt);
    NumericMatrix H2_0(na,nt);
    NumericMatrix H2_1(na,nt);
    NumericMatrix H3_0(na,nt);
    NumericMatrix H3_1(na,nt);
    NumericMatrix H4_0(na,nt);
    NumericMatrix H4_1(na,nt);
    NumericMatrix H5_0(na,nt);
    NumericMatrix H5_1(na,nt);
    NumericMatrix O1_0(na,nt);
    NumericMatrix O1_1(na,nt);
    NumericMatrix O2_0(na,nt);
    NumericMatrix O2_1(na,nt);
    NumericMatrix O3_0(na,nt);
    NumericMatrix O3_1(na,nt);
    NumericMatrix O4_0(na,nt);
    NumericMatrix O4_1(na,nt);
    NumericMatrix O5_0(na,nt);
    NumericMatrix O5_1(na,nt);
    NumericMatrix R_0(na,nt);
    NumericMatrix R_1(na,nt);
    NumericMatrix D_0(na,nt);	
    NumericMatrix D_1(na,nt);	
    NumericMatrix N_0(na,nt);
    NumericMatrix N_1(na,nt);
    
    // initial states at time[0] - age-vector - evenly split between ps and 1-ps
    NumericVector Sa0 = as<NumericVector>(pars["Sa0"]);
    NumericVector Ea0 = as<NumericVector>(pars["Ea0"]);
    NumericVector Ia0 = as<NumericVector>(pars["Ia0"]);
    NumericVector Ua0 = as<NumericVector>(pars["Ua0"]);
    NumericVector Ha0 = as<NumericVector>(pars["Ha0"]);
    NumericVector Oa0 = as<NumericVector>(pars["Oa0"]);
    NumericVector Ra0 = as<NumericVector>(pars["Ra0"]);
    NumericVector Da0 = as<NumericVector>(pars["Da0"]);
    NumericVector Na0 = as<NumericVector>(pars["Na0"]);

    // States(time) = 0 - need initialise St[0] etc, time[0]
    NumericVector St_0(nt);
    NumericVector St_1(nt);
    NumericVector Et_0(nt);
    NumericVector Et_1(nt);
    NumericVector It_0(nt);
    NumericVector It_1(nt);
    NumericVector Ut_0(nt);
    NumericVector Ut_1(nt);
    NumericVector Ct_0(nt);
    NumericVector Ct_1(nt);
    NumericVector Ht_0(nt);
    NumericVector Ht_1(nt);
    NumericVector Ot_0(nt);
    NumericVector Ot_1(nt);
    NumericVector Rt_0(nt);
    NumericVector Rt_1(nt);
    NumericVector Dt_0(nt); //DHt(nt), DOt(nt) - only need weekly incidence
    NumericVector Dt_1(nt); //DHt(nt), DOt(nt) - only need weekly incidence
    NumericVector Nt_0(nt);
    NumericVector Nt_1(nt);
    NumericVector Hw_0(nw);
    NumericVector Hw_1(nw);
    NumericVector Dw_0(nw);
    NumericVector Dw_1(nw);
    NumericVector DHw_0(nw);
    NumericVector DHw_1(nw);
    NumericVector DOw_0(nw);
    NumericVector DOw_1(nw);
    NumericVector time(nt);
    //NumericVector cmdtmean(nt);
    NumericVector cmdtmean_0(nt);
    NumericVector cmdtmean_1(nt);

    //weekly incidence H and D
    NumericVector Hapw_0(na); //overall
    NumericVector Hapw_1(na);
    NumericVector Dapw_0(na); //overall DH and DO
    NumericVector Dapw_1(na);
    NumericVector DHapw_0(na);
    NumericVector DHapw_1(na);
    NumericVector DOapw_0(na);
    NumericVector DOapw_1(na);
    NumericVector Ha1w_0(nw);  //by age
    NumericVector Ha1w_1(nw);
    NumericVector Ha2w_0(nw);
    NumericVector Ha2w_1(nw);
    NumericVector Ha3w_0(nw);
    NumericVector Ha3w_1(nw);
    NumericVector Ha4w_0(nw);
    NumericVector Ha4w_1(nw);
    NumericVector Ha5w_0(nw);
    NumericVector Ha5w_1(nw);
    NumericVector Ha6w_0(nw);
    NumericVector Ha6w_1(nw);
    NumericVector Ha7w_0(nw);
    NumericVector Ha7w_1(nw);
    NumericVector Ha8w_0(nw);
    NumericVector Ha8w_1(nw);
    NumericVector Ha9w_0(nw);
    NumericVector Ha9w_1(nw);
    NumericVector Da1w_0(nw);
    NumericVector Da1w_1(nw);
    NumericVector Da2w_0(nw);
    NumericVector Da2w_1(nw);
    NumericVector Da3w_0(nw);
    NumericVector Da3w_1(nw);
    NumericVector Da4w_0(nw);
    NumericVector Da4w_1(nw);
    NumericVector Da5w_0(nw);
    NumericVector Da5w_1(nw);
    NumericVector Da6w_0(nw);
    NumericVector Da6w_1(nw);
    NumericVector Da7w_0(nw);
    NumericVector Da7w_1(nw);
    NumericVector Da8w_0(nw);
    NumericVector Da8w_1(nw);
    NumericVector Da9w_0(nw);
    NumericVector Da9w_1(nw);
    NumericVector DHa1w_0(nw);
    NumericVector DHa1w_1(nw);
    NumericVector DHa2w_0(nw);
    NumericVector DHa2w_1(nw);
    NumericVector DHa3w_0(nw);
    NumericVector DHa3w_1(nw);
    NumericVector DHa4w_0(nw);
    NumericVector DHa4w_1(nw);
    NumericVector DHa5w_0(nw);
    NumericVector DHa5w_1(nw);
    NumericVector DHa6w_0(nw);
    NumericVector DHa6w_1(nw);
    NumericVector DHa7w_0(nw);
    NumericVector DHa7w_1(nw);
    NumericVector DHa8w_0(nw);
    NumericVector DHa8w_1(nw);
    NumericVector DHa9w_0(nw);
    NumericVector DHa9w_1(nw);
    NumericVector DOa1w_0(nw);
    NumericVector DOa1w_1(nw);
    NumericVector DOa2w_0(nw);
    NumericVector DOa2w_1(nw);
    NumericVector DOa3w_0(nw);
    NumericVector DOa3w_1(nw);
    NumericVector DOa4w_0(nw);
    NumericVector DOa4w_1(nw);
    NumericVector DOa5w_0(nw);
    NumericVector DOa5w_1(nw);
    NumericVector DOa6w_0(nw);
    NumericVector DOa6w_1(nw);
    NumericVector DOa7w_0(nw);
    NumericVector DOa7w_1(nw);
    NumericVector DOa8w_0(nw);
    NumericVector DOa8w_1(nw);
    NumericVector DOa9w_0(nw);    
    NumericVector DOa9w_1(nw);    
    // read parameters
    const NumericVector u   = as<NumericVector>(pars["u"]); 
    const NumericVector y_0 = as<NumericVector>(pars["y_0"]); 
    const NumericVector y_1 = as<NumericVector>(pars["y_1"]); 
    const NumericVector h_0 = as<NumericVector>(pars["h_0"]); 
    const NumericVector h_1 = as<NumericVector>(pars["h_1"]); 
    const NumericVector m_a_0= as<NumericVector>(pars["ma_0"]); 
    const NumericVector m_b_0= as<NumericVector>(pars["mb_0"]); 
    const NumericVector m_a_1= as<NumericVector>(pars["ma_1"]); 
    const NumericVector m_b_1= as<NumericVector>(pars["mb_1"]); 
    const NumericVector d_0 = as<NumericVector>(pars["d_0"]);  //change for da_, db_
    //const NumericVector d_0 = as<NumericVector>(pars["db_0"]); 
    const NumericVector d_1 = as<NumericVector>(pars["d_1"]);  //change for da_, db_
    //const NumericVector d_1 = as<NumericVector>(pars["db_1"]);
    const NumericVector psage_1 = as<NumericVector>(pars["psagecoh"]); //dim(psagecoh)=na
    const NumericVector rseed   = as<NumericVector>(pars["rseed"]);    //per age group
    
    //const double ps   = pars["pscoh"];
    const double beta_infectivity = pars["beta"];
    const double fu   = pars["fu"];
    const double ad_0 = pars["ad_0"];
    const double ad_1 = pars["ad_1"];
    const double rEI  = pars["rEI"];
    const double rEU  = pars["rEU"];
    const double rIR  = pars["rIR"];
    const double rIO  = pars["rIO"];
    const double rOD  = pars["rOD"];  //change  
    //const double rODa = pars["rODa"];  //change  
    //const double rODb = pars["rODb"];  //change  
    const double rUR  = pars["rUR"];
    const double rIH  = pars["rIH"];
    const double rHRa = pars["rHRa"];
    const double rHRb = pars["rHRb"];
    const double rHDa = pars["rHDa"];
    const double rHDb = pars["rHDb"];
    const double rRS  = pars["rRS"];
    const double rC   = pars["rC"];
    const double rCi  = 5*rC;
    //const double rHi  = 5*rHD;
    const double rOi  = 5*rOD; //change
    const double dt   = pars["dt"];

    // age group and population states initialised
    for (int ia = 0; ia < na; ia++) {
        const double pa_1 = psage_1[ia], pa_0 = 1-psage_1[ia];
        const double IUa0 = Ia0[ia]+Ua0[ia];
        S_0(ia,0)  = Sa0[ia]*pa_0;        St_0[0] += S_0(ia,0);
        S_1(ia,0)  = Sa0[ia]*pa_1;        St_1[0] += S_1(ia,0);
        E_0(ia,0)  = Ea0[ia]*pa_0;        Et_0[0] += E_0(ia,0);
        E_1(ia,0)  = Ea0[ia]*pa_1;        Et_1[0] += E_1(ia,0);
        I_0(ia,0)  = Ia0[ia]*pa_0;        It_0[0] += I_0(ia,0);
        I_1(ia,0)  = Ia0[ia]*pa_1;        It_1[0] += I_1(ia,0);
        U_0(ia,0)  = Ua0[ia]*pa_0;        Ut_0[0] += U_0(ia,0);
        U_1(ia,0)  = Ua0[ia]*pa_1;        Ut_1[0] += U_1(ia,0);
        C1_0(ia,0) = IUa0*pa_0;           Ct_0[0] += C1_0(ia,0);
        C1_1(ia,0) = IUa0*pa_1;           Ct_1[0] += C1_1(ia,0);
        C2_0(ia,0) = 0;  
        C2_1(ia,0) = 0;  
        C3_0(ia,0) = 0;   
        C3_1(ia,0) = 0;   
        C4_0(ia,0) = 0;   
        C4_1(ia,0) = 0;   
        C5_0(ia,0) = 0;
        C5_1(ia,0) = 0;
        H1_0(ia,0) = Ha0[ia]*pa_0;        Ht_0[0] += H1_0(ia,0);   Hw_0[0] += 0;
        H1_1(ia,0) = Ha0[ia]*pa_1;        Ht_1[0] += H1_1(ia,0);   Hw_1[0] += 0;
        H2_0(ia,0) = 0;
        H2_1(ia,0) = 0;
        H3_0(ia,0) = 0;
        H3_1(ia,0) = 0;
        H4_0(ia,0) = 0;
        H4_1(ia,0) = 0;
        H5_0(ia,0) = 0;
        H5_1(ia,0) = 0;
        O1_0(ia,0) = Ua0[ia]*pa_0;        Ot_0[0] += O1_0(ia,0);
        O1_1(ia,0) = Ua0[ia]*pa_1;        Ot_1[0] += O1_1(ia,0);
        O2_0(ia,0) = 0;
        O2_1(ia,0) = 0;
        O3_0(ia,0) = 0;
        O3_1(ia,0) = 0;
        O4_0(ia,0) = 0;
        O4_1(ia,0) = 0;
        O5_0(ia,0) = 0;
        O5_1(ia,0) = 0;
        R_0(ia,0)  = Ra0[ia]*pa_0;        Rt_0[0] += R_0(ia,0);
        R_1(ia,0)  = Ra0[ia]*pa_1;        Rt_1[0] += R_1(ia,0);
        D_0(ia,0)  = Da0[ia]*pa_0;        Dt_0[0] += D_0(ia,0);   Dw_0[0] += 0; DHw_0[0] += 0; DOw_0[0] += 0;
        D_1(ia,0)  = Da0[ia]*pa_1;        Dt_1[0] += D_1(ia,0);   Dw_1[0] += 0; DHw_1[0] += 0; DOw_1[0] += 0;
        N_0(ia,0)  = Na0[ia]*pa_0;        Nt_0[0] += N_0(ia,0);
        N_1(ia,0)  = Na0[ia]*pa_1;        Nt_1[0] += N_1(ia,0);
    }


    // Dynamics of state variables - Euler integration
    time[0] = 0;
    int  week  = 1;
    int  week0 = 1;
    double seedon = 0;
    double  Hpw_0 = 0,   Hpw_1 = 0;
    double  Dpw_0 = 0,   Dpw_1 = 0;
    double DHpw_0 = 0,  DHpw_1 = 0;
    double DOpw_0 = 0,  DOpw_1 = 0;
    double rHR = rHRa;
    double rHi = 5*rHDa;
    double qma = 1;
    for (int it = 0; it < (nt-1); it++) {	//Crucial: nt-1
        week0 = week;
        week  = 1 + (int) time[it]/7;
        if (week <= 4) {seedon = 1;}; //floor(1/ceil(week/4));
        if (week > 22) {rHR = rHRb;  rHi = 5*rHDb;  qma=0;}; //change: rOD = rODb;};
        cmdtmean_0[it] = 0;
        cmdtmean_1[it] = 0;
        
        for (int ia = 0; ia < na; ia++) {
          
        // current matrix cells
        double Sat_0 = S_0(ia,it);
        double Sat_1 = S_1(ia,it);
        double Eat_0 = E_0(ia,it);
        double Eat_1 = E_1(ia,it);
        double Iat_0 = I_0(ia,it);
        double Iat_1 = I_1(ia,it);
        double Uat_0 = U_0(ia,it);
        double Uat_1 = U_1(ia,it);
        double C1at_0 = C1_0(ia,it);
        double C1at_1 = C1_1(ia,it);
        double C2at_0 = C2_0(ia,it);
        double C2at_1 = C2_1(ia,it);
        double C3at_0 = C3_0(ia,it);
        double C3at_1 = C3_1(ia,it);
        double C4at_0 = C4_0(ia,it);
        double C4at_1 = C4_1(ia,it);
        double C5at_0 = C5_0(ia,it);
        double C5at_1 = C5_1(ia,it);
        double H1at_0 = H1_0(ia,it);
        double H1at_1 = H1_1(ia,it);
        double H2at_0 = H2_0(ia,it);
        double H2at_1 = H2_1(ia,it);
        double H3at_0 = H3_0(ia,it);
        double H3at_1 = H3_1(ia,it);
        double H4at_0 = H4_0(ia,it);
        double H4at_1 = H4_1(ia,it);
        double H5at_0 = H5_0(ia,it);
        double H5at_1 = H5_1(ia,it);
        double O1at_0 = O1_0(ia,it);
        double O1at_1 = O1_1(ia,it);
        double O2at_0 = O2_0(ia,it);
        double O2at_1 = O2_1(ia,it);
        double O3at_0 = O3_0(ia,it);
        double O3at_1 = O3_1(ia,it);
        double O4at_0 = O4_0(ia,it);
        double O4at_1 = O4_1(ia,it);
        double O5at_0 = O5_0(ia,it);
        double O5at_1 = O5_1(ia,it);
        double Rat_0 = R_0(ia,it);
        double Rat_1 = R_1(ia,it);
        double Dat_0 = D_0(ia,it);
        double Dat_1 = D_1(ia,it);
        double Nat_0 = N_0(ia,it);
        double Nat_1 = N_1(ia,it);
        
        // force of infection on group ia
        double lambda_0 = 0, lambda_1 = 0;
        double beta_ua  = beta_infectivity*u[ia];
        double ya_0 = y_0[ia];
        double ya_1 = y_1[ia];
        double ha_0 = h_0[ia];
        double ha_1 = h_1[ia];
        double ma_0 = m_a_0[ia]*qma + m_b_0[ia]*(1-qma);
        double ma_1 = m_a_1[ia]*qma + m_b_1[ia]*(1-qma);
        double da_0 = d_0[ia]*ad_0;
        double da_1 = d_1[ia]*ad_1;
        double rseeda = seedon*rseed[ia];
        
        for (int ib = 0; ib < na; ib++) {
            int    icm = (week-1)*cmdim1*cmdim2 + ib*cmdim1 + ia;
            double cmi_0 = cm_0[icm];
            double cmi_1 = cm_1[icm];
            double O_sum_0   = O1_0(ib,it) + O2_0(ib,it) + O3_0(ib,it) + O4_0(ib,it) + O5_0(ib,it);
            double O_sum_1   = O1_1(ib,it) + O2_1(ib,it) + O3_1(ib,it) + O4_1(ib,it) + O5_1(ib,it);
            double inffrom_0 =(1-psage_1[ib])*(I_0(ib,it)  + fu*O_sum_0  + fu*U_0(ib,it) )/N_0(ib,it);
            double inffrom_1 =   psage_1[ib]*( I_1(ib,it)  + fu*O_sum_1  + fu*U_1(ib,it) )/N_1(ib,it);
            //Outside hospital delay to death (assume as infectious as U, i.e. partial isolation)
            //Force of infection on each stratum       
            lambda_0      += beta_ua*cmi_0*( inffrom_0 + inffrom_1 ); //check:   prob(contact=s)xprob(s infected)
            lambda_1      += beta_ua*cmi_1*( inffrom_0 + inffrom_1 ); //check: + prob(contact=n)xprob(n infected)
            cmdtmean_0[it]+= cmi_0*ocmdim;
            cmdtmean_1[it]+= cmi_1*ocmdim;
        } //ib

        //Infection update for next timestep
        double dS_0  = dt*(-lambda_0*Sat_0       + rRS*Rat_0 - rseeda); //need rseeda*psa
        double dS_1  = dt*(-lambda_1*Sat_1       + rRS*Rat_1 - rseeda); //need rseeda*(1-psa)
        double dE_0  = dt*( lambda_0*Sat_0       - (rEU*(1-ya_0) + rEI*ya_0)*Eat_0 +  rseeda); //early seeding, no effect later
        double dE_1  = dt*( lambda_1*Sat_1       - (rEU*(1-ya_1) + rEI*ya_1)*Eat_1 +  rseeda); //early seeding, no effect later
        double dI_0  = dt*( rEI*ya_0*Eat_0       - (rIR*(1-ha_0-da_0) + rIH*ha_0 + rIO*da_0)*Iat_0 );
        double dI_1  = dt*( rEI*ya_1*Eat_1       - (rIR*(1-ha_1-da_1) + rIH*ha_1 + rIO*da_1)*Iat_1 );
        double dU_0  = dt*( rEU*(1-ya_0)*Eat_0   - rUR*Uat_0 );
        double dU_1  = dt*( rEU*(1-ya_1)*Eat_1   - rUR*Uat_1 );

        //Marker of longer duration of positivity in tests compared to duration of E (infection becoming I or U)
        double dC1_0 = dt*( lambda_0*Sat_0 - rCi*C1at_0);
        double dC1_1 = dt*( lambda_1*Sat_1 - rCi*C1at_1);
        double dC2_0 = dt*( rCi*C1at_0     - rCi*C2at_0);
        double dC2_1 = dt*( rCi*C1at_1     - rCi*C2at_1);
        double dC3_0 = dt*( rCi*C2at_0     - rCi*C3at_0);
        double dC3_1 = dt*( rCi*C2at_1     - rCi*C3at_1);
        double dC4_0 = dt*( rCi*C3at_0     - rCi*C4at_0);
        double dC4_1 = dt*( rCi*C3at_1     - rCi*C4at_1);
        double dC5_0 = dt*( rCi*C4at_0     - rCi*C5at_0); //decays but doesn't flow to another state
        double dC5_1 = dt*( rCi*C4at_1     - rCi*C5at_1);

        //DO - post-I delay in death outside hospital
        double dOin_0= dt*rIO*da_0*Iat_0;
        double dOin_1= dt*rIO*da_1*Iat_1;
        double dO1_0 = dOin_0       - dt*( rOi*O1at_0);
        double dO1_1 = dOin_1       - dt*( rOi*O1at_1);
        double dO2_0 = dt*( rOi*O1at_0   - rOi*O2at_0);
        double dO2_1 = dt*( rOi*O1at_1   - rOi*O2at_1);
        double dO3_0 = dt*( rOi*O2at_0   - rOi*O3at_0);
        double dO3_1 = dt*( rOi*O2at_1   - rOi*O3at_1);
        double dO4_0 = dt*( rOi*O3at_0   - rOi*O4at_0);
        double dO4_1 = dt*( rOi*O3at_1   - rOi*O4at_1);
        double dO5_0 = dt*( rOi*O4at_0   - rOi*O5at_0); 
        double dO5_1 = dt*( rOi*O4at_1   - rOi*O5at_1);

        //H, DH  - post-I delay in death or recovery in hospital         
        double dHin_0= dt*rIH*ha_0*Iat_0;  //hospitalisation incidence
        double dHin_1= dt*rIH*ha_1*Iat_1;
        double dH1_0 = dHin_0       - dt*( rHi*H1at_0);
        double dH1_1 = dHin_1       - dt*( rHi*H1at_1);
        double dH2_0 = dt*( rHi*H1at_0   - rHi*H2at_0);
        double dH2_1 = dt*( rHi*H1at_1   - rHi*H2at_1);
        double dH3_0 = dt*( rHi*H2at_0   - rHi*H3at_0);
        double dH3_1 = dt*( rHi*H2at_1   - rHi*H3at_1);
        double dH4_0 = dt*( rHi*H3at_0   - rHi*H4at_0);
        double dH4_1 = dt*( rHi*H3at_1   - rHi*H4at_1);
        double dH5_0 = dt*( rHi*H4at_0   - ((rHR*5)*(1-ma_0)+rHi*ma_0)*H5at_0); //rHi=5*rHD //Recovery time def could be different
        double dH5_1 = dt*( rHi*H4at_1   - ((rHR*5)*(1-ma_1)+rHi*ma_1)*H5at_1);

        //R  - recovery from all infections inside or outside hospital         
        double dR_0  = dt*( rUR*Uat_0 + rIR*(1-ha_0-da_0)*Iat_0 + (rHR*5)*(1-ma_0)*H5at_0 - rRS*Rat_0);
        double dR_1  = dt*( rUR*Uat_1 + rIR*(1-ha_1-da_1)*Iat_1 + (rHR*5)*(1-ma_1)*H5at_1 - rRS*Rat_1);
        //D - deaths from I infections inside or outside hospital  
        double dDin_0= dt*( rHi*ma_0*H5at_0 + rOi*O5at_0); //death incidence
        double dDin_1= dt*( rHi*ma_1*H5at_1 + rOi*O5at_1);
        double dDH_0 = dt*( rHi*ma_0*H5at_0 );             //death incidence inside hospital
        double dDH_1 = dt*( rHi*ma_1*H5at_1 );
        double dDO_0 = dt*( rOi*O5at_0    );               //death incidence outside hospital
        double dDO_1 = dt*( rOi*O5at_1    );
        double dD_0  = dDH_0 + dDO_0;                      //death change = incidence
        double dD_1  = dDH_1 + dDO_1;
        double dN_0  = dS_0 + dE_0 + dU_0 + dI_0 + (dH1_0 + dH2_0 + dH3_0 + dH4_0 + dH5_0) + (dO1_0 + dO2_0 + dO3_0 + dO4_0 + dO5_0) + dR_0 + dD_0;
        double dN_1  = dS_1 + dE_1 + dU_1 + dI_1 + (dH1_1 + dH2_1 + dH3_1 + dH4_1 + dH5_1) + (dO1_1 + dO2_1 + dO3_1 + dO4_1 + dO5_1) + dR_1 + dD_1;

        S_0(ia,it+1)  = Sat_0  + dS_0;
        S_1(ia,it+1)  = Sat_1  + dS_1;
        E_0(ia,it+1)  = Eat_0  + dE_0;
        E_1(ia,it+1)  = Eat_1  + dE_1;
        I_0(ia,it+1)  = Iat_0  + dI_0;
        I_1(ia,it+1)  = Iat_1  + dI_1;
        U_0(ia,it+1)  = Uat_0  + dU_0;
        U_1(ia,it+1)  = Uat_1  + dU_1;
        C1_0(ia,it+1) = C1at_0 + dC1_0;
        C1_1(ia,it+1) = C1at_1 + dC1_1;
        C2_0(ia,it+1) = C2at_0 + dC2_0;
        C2_1(ia,it+1) = C2at_1 + dC2_1;
        C3_0(ia,it+1) = C3at_0 + dC3_0;
        C3_1(ia,it+1) = C3at_1 + dC3_1;
        C4_0(ia,it+1) = C4at_0 + dC4_0;
        C4_1(ia,it+1) = C4at_1 + dC4_1;
        C5_0(ia,it+1) = C5at_0 + dC5_0;
        C5_1(ia,it+1) = C5at_1 + dC5_1;
        H1_0(ia,it+1) = H1at_0 + dH1_0;
        H1_1(ia,it+1) = H1at_1 + dH1_1;
        H2_0(ia,it+1) = H2at_0 + dH2_0;
        H2_1(ia,it+1) = H2at_1 + dH2_1;
        H3_0(ia,it+1) = H3at_0 + dH3_0;
        H3_1(ia,it+1) = H3at_1 + dH3_1;
        H4_0(ia,it+1) = H4at_0 + dH4_0;
        H4_1(ia,it+1) = H4at_1 + dH4_1;
        H5_0(ia,it+1) = H5at_0 + dH5_0;
        H5_1(ia,it+1) = H5at_1 + dH5_1;
        O1_0(ia,it+1) = O1at_0 + dO1_0;
        O1_1(ia,it+1) = O1at_1 + dO1_1;
        O2_0(ia,it+1) = O2at_0 + dO2_0;
        O2_1(ia,it+1) = O2at_1 + dO2_1;
        O3_0(ia,it+1) = O3at_0 + dO3_0;
        O3_1(ia,it+1) = O3at_1 + dO3_1;
        O4_0(ia,it+1) = O4at_0 + dO4_0;
        O4_1(ia,it+1) = O4at_1 + dO4_1;
        O5_0(ia,it+1) = O5at_0 + dO5_0;
        O5_1(ia,it+1) = O5at_1 + dO5_1;
        R_0(ia,it+1)  = Rat_0  + dR_0;
        R_1(ia,it+1)  = Rat_1  + dR_1;
        D_0(ia,it+1)  = Dat_0  + dD_0; //DH(ia,it+1) = DHat + dDH; //DO(ia,it+1) = DOat + dDO; //not yet
        D_1(ia,it+1)  = Dat_1  + dD_1; //DH(ia,it+1) = DHat + dDH; //DO(ia,it+1) = DOat + dDO; //not yet
        N_0(ia,it+1)  = Nat_0  + dN_0;
        N_1(ia,it+1)  = Nat_1  + dN_1;
        // time update (units of parameters)
        time[it+1] = (it+1)*dt;
        // population states evaluated (t>0)
        St_0[it+1]  += Sat_0 + dS_0;
        St_1[it+1]  += Sat_1 + dS_1;
        Et_0[it+1]  += Eat_0 + dE_0;
        Et_1[it+1]  += Eat_1 + dE_1;
        It_0[it+1]  += Iat_0 + dI_0;
        It_1[it+1]  += Iat_1 + dI_1;
        Ut_0[it+1]  += Uat_0 + dU_0;
        Ut_1[it+1]  += Uat_1 + dU_1;
        Ct_0[it+1]  += C1at_0 + C2at_0 + C3at_0 + C4at_0 + C5at_0 + dC1_0 + dC2_0 + dC3_0 + dC4_0 + dC5_0;
        Ct_1[it+1]  += C1at_1 + C2at_1 + C3at_1 + C4at_1 + C5at_1 + dC1_1 + dC2_1 + dC3_1 + dC4_1 + dC5_1;
        Ht_0[it+1]  += H1at_0 + H2at_0 + H3at_0 + H4at_0 + H5at_0 + dH1_0 + dH2_0 + dH3_0 + dH4_0 + dH5_0;
        Ht_1[it+1]  += H1at_1 + H2at_1 + H3at_1 + H4at_1 + H5at_1 + dH1_1 + dH2_1 + dH3_1 + dH4_1 + dH5_1;
        Ot_0[it+1]  += O1at_0 + O2at_0 + O3at_0 + O4at_0 + O5at_0 + dO1_0 + dO2_0 + dO3_0 + dO4_0 + dO5_0;
        Ot_1[it+1]  += O1at_1 + O2at_1 + O3at_1 + O4at_1 + O5at_1 + dO1_1 + dO2_1 + dO3_1 + dO4_1 + dO5_1;
        Rt_0[it+1]  += Rat_0 + dR_0;
        Rt_1[it+1]  += Rat_1 + dR_1;
        Dt_0[it+1]  += Dat_0 + dD_0;
        Dt_1[it+1]  += Dat_1 + dD_1;
        Nt_0[it+1]  += Nat_0 + dN_0;
        Nt_1[it+1]  += Nat_1 + dN_1;
        // week incidence (pw) 
        Hpw_0       += dHin_0;
   	    Hpw_1       += dHin_1;
   	    Dpw_0       += dDin_0; //dDH + dDO - contributions from all ia
   	    Dpw_1       += dDin_1;
   	    DHpw_0      += dDH_0;
   	    DHpw_1      += dDH_1;
   	    DOpw_0      += dDO_0;
   	    DOpw_1      += dDO_1;
   	    // week incidence (pw) by age
   	    Hapw_0[ia]  += dHin_0;
   	    Hapw_1[ia]  += dHin_1;
   	    Dapw_0[ia]  += dDin_0; //dDH + dDO - contributions from each ia only
   	    Dapw_1[ia]  += dDin_1;
   	    DHapw_0[ia] += dDH_0; 
   	    DHapw_1[ia] += dDH_1; 
   	    DOapw_0[ia] += dDO_0; 
   	    DOapw_1[ia] += dDO_1; 
  }; //ia

        if (week - week0 == 1) { //week incidence - update at the end of each week
          Hw_0[week-1] = Hpw_0;           Hpw_0=0;
          Hw_1[week-1] = Hpw_1;           Hpw_1=0;
          Dw_0[week-1] = Dpw_0;           Dpw_0=0;
          Dw_1[week-1] = Dpw_1;           Dpw_1=0;
          DHw_0[week-1]= DHpw_0;          DHpw_0=0;
          DHw_1[week-1]= DHpw_1;          DHpw_1=0;
          DOw_0[week-1]= DOpw_0;          DOpw_0=0;
          DOw_1[week-1]= DOpw_1;          DOpw_1=0;
          // by age
          Ha1w_0[week-1]  = Hapw_0[0];    Hapw_0[0]=0; 
          Ha1w_1[week-1]  = Hapw_1[0];    Hapw_1[0]=0; 
          Ha2w_0[week-1]  = Hapw_0[1];    Hapw_0[1]=0;
          Ha2w_1[week-1]  = Hapw_1[1];    Hapw_1[1]=0;
          Ha3w_0[week-1]  = Hapw_0[2];    Hapw_0[2]=0;
          Ha3w_1[week-1]  = Hapw_1[2];    Hapw_1[2]=0;
          Ha4w_0[week-1]  = Hapw_0[3];    Hapw_0[3]=0;
          Ha4w_1[week-1]  = Hapw_1[3];    Hapw_1[3]=0;
          Ha5w_0[week-1]  = Hapw_0[4];    Hapw_0[4]=0;
          Ha5w_1[week-1]  = Hapw_1[4];    Hapw_1[4]=0;
          Ha6w_1[week-1]  = Hapw_1[5];    Hapw_1[5]=0;
          Ha6w_0[week-1]  = Hapw_0[5];    Hapw_0[5]=0;
          Ha7w_1[week-1]  = Hapw_1[6];    Hapw_1[6]=0;
          Ha7w_0[week-1]  = Hapw_0[6];    Hapw_0[6]=0;
          Ha8w_1[week-1]  = Hapw_1[7];    Hapw_1[7]=0;
          Ha8w_0[week-1]  = Hapw_0[7];    Hapw_0[7]=0;
          Ha9w_0[week-1]  = Hapw_0[8];    Hapw_0[8]=0;
          Ha9w_1[week-1]  = Hapw_1[8];    Hapw_1[8]=0;
          
          Da1w_0[week-1]  = Dapw_0[0];    Dapw_0[0]=0;
          Da1w_1[week-1]  = Dapw_1[0];    Dapw_1[0]=0;
          Da2w_0[week-1]  = Dapw_0[1];    Dapw_0[1]=0;
          Da2w_1[week-1]  = Dapw_1[1];    Dapw_1[1]=0;
          Da3w_0[week-1]  = Dapw_0[2];    Dapw_0[2]=0;
          Da3w_1[week-1]  = Dapw_1[2];    Dapw_1[2]=0;
          Da4w_0[week-1]  = Dapw_0[3];    Dapw_0[3]=0;
          Da4w_1[week-1]  = Dapw_1[3];    Dapw_1[3]=0;
          Da5w_0[week-1]  = Dapw_0[4];    Dapw_0[4]=0;
          Da5w_1[week-1]  = Dapw_1[4];    Dapw_1[4]=0;
          Da6w_0[week-1]  = Dapw_0[5];    Dapw_0[5]=0;
          Da6w_1[week-1]  = Dapw_1[5];    Dapw_1[5]=0;
          Da7w_0[week-1]  = Dapw_0[6];    Dapw_0[6]=0;
          Da7w_1[week-1]  = Dapw_1[6];    Dapw_1[6]=0;
          Da8w_0[week-1]  = Dapw_0[7];    Dapw_0[7]=0;
          Da8w_1[week-1]  = Dapw_1[7];    Dapw_1[7]=0;
          Da9w_0[week-1]  = Dapw_0[8];    Dapw_0[8]=0;
          Da9w_1[week-1]  = Dapw_1[8];    Dapw_1[8]=0;
          
          DHa1w_0[week-1] = DHapw_0[0];   DHapw_0[0]=0;
          DHa1w_1[week-1] = DHapw_1[0];   DHapw_1[0]=0;
          DHa2w_0[week-1] = DHapw_0[1];   DHapw_0[1]=0;
          DHa2w_1[week-1] = DHapw_1[1];   DHapw_1[1]=0;
          DHa3w_0[week-1] = DHapw_0[2];   DHapw_0[2]=0;
          DHa3w_1[week-1] = DHapw_1[2];   DHapw_1[2]=0;
          DHa4w_0[week-1] = DHapw_0[3];   DHapw_0[3]=0;
          DHa4w_1[week-1] = DHapw_1[3];   DHapw_1[3]=0;
          DHa5w_0[week-1] = DHapw_0[4];   DHapw_0[4]=0;
          DHa5w_1[week-1] = DHapw_1[4];   DHapw_1[4]=0;
          DHa6w_0[week-1] = DHapw_0[5];   DHapw_0[5]=0;
          DHa6w_1[week-1] = DHapw_1[5];   DHapw_1[5]=0;
          DHa7w_0[week-1] = DHapw_0[6];   DHapw_0[6]=0;
          DHa7w_1[week-1] = DHapw_1[6];   DHapw_1[6]=0;
          DHa8w_0[week-1] = DHapw_0[7];   DHapw_0[7]=0;
          DHa8w_1[week-1] = DHapw_1[7];   DHapw_1[7]=0;
          DHa9w_0[week-1] = DHapw_0[8];   DHapw_0[8]=0;
          DHa9w_1[week-1] = DHapw_1[8];   DHapw_1[8]=0;
          
          DOa1w_0[week-1] = DOapw_0[0];   DOapw_0[0]=0;
          DOa1w_1[week-1] = DOapw_1[0];   DOapw_1[0]=0;
          DOa2w_0[week-1] = DOapw_0[1];   DOapw_0[1]=0;
          DOa2w_1[week-1] = DOapw_1[1];   DOapw_1[1]=0;
          DOa3w_0[week-1] = DOapw_0[2];   DOapw_0[2]=0;
          DOa3w_1[week-1] = DOapw_1[2];   DOapw_1[2]=0;
          DOa4w_0[week-1] = DOapw_0[3];   DOapw_0[3]=0;
          DOa4w_1[week-1] = DOapw_1[3];   DOapw_1[3]=0;
          DOa5w_0[week-1] = DOapw_0[4];   DOapw_0[4]=0;
          DOa5w_1[week-1] = DOapw_1[4];   DOapw_1[4]=0;
          DOa6w_0[week-1] = DOapw_0[5];   DOapw_0[5]=0;
          DOa6w_1[week-1] = DOapw_1[5];   DOapw_1[5]=0;
          DOa7w_0[week-1] = DOapw_0[6];   DOapw_0[6]=0;
          DOa7w_1[week-1] = DOapw_1[6];   DOapw_1[6]=0;
          DOa8w_0[week-1] = DOapw_0[7];   DOapw_0[7]=0;
          DOa8w_1[week-1] = DOapw_1[7];   DOapw_1[7]=0;
          DOa9w_0[week-1] = DOapw_0[8];   DOapw_0[8]=0;
          DOa9w_1[week-1] = DOapw_1[8];   DOapw_1[8]=0;
      }
	}; //it
	
    Rcpp::DataFrame byw_0 = Rcpp::DataFrame::create(
        Named("time") = time[iw],
        Named("cmdtmean_0") = cmdtmean_0[iw],
        Named("cmdtmean_1") = cmdtmean_1[iw],
        // weekly totals
        Named("St")   = St_0[iw],
        Named("Et")   = Et_0[iw],
        Named("It")   = It_0[iw],
        Named("Ut")   = Ut_0[iw],
        Named("Ct")   = Ct_0[iw],
        Named("Ht")   = Ht_0[iw],
        Named("Ot")   = Ot_0[iw],
        Named("Rt")   = Rt_0[iw],
        Named("Dt")   = Dt_0[iw],
        Named("Nt")   = Nt_0[iw],
        // weekly incidence - fitted to incidence data
        Named("Hw")   = Hw_0,
        Named("Dw")   = Dw_0,
        Named("DHw")  = DHw_0,
        Named("DOw")  = DOw_0);
    Rcpp::DataFrame byw_1 = Rcpp::DataFrame::create(
        // weekly totals
        Named("St")   = St_1[iw],
        Named("Et")   = Et_1[iw],
        Named("It")   = It_1[iw],
        Named("Ut")   = Ut_1[iw],
        Named("Ct")   = Ct_1[iw],
        Named("Ht")   = Ht_1[iw],
        Named("Ot")   = Ot_1[iw],
        Named("Rt")   = Rt_1[iw],
        Named("Dt")   = Dt_1[iw],
        Named("Nt")   = Nt_1[iw],
        //weekly incidence - fitted to incidence data
        Named("Hw")   = Hw_1,
        Named("Dw")   = Dw_1,
        Named("DHw")  = DHw_1,
        Named("DOw")  = DOw_1);
    
    // weekly incidence by age - fitted to incidence data
    Rcpp::DataFrame byw_ageH_0 = Rcpp::DataFrame::create(
        Named("H1w") = Ha1w_0,
        Named("H2w") = Ha2w_0,
        Named("H3w") = Ha3w_0,
        Named("H4w") = Ha4w_0,
        Named("H5w") = Ha5w_0,
        Named("H6w") = Ha6w_0,
        Named("H7w") = Ha7w_0,
        Named("H8w") = Ha8w_0,
        Named("H9w") = Ha9w_0);
    Rcpp::DataFrame byw_ageH_1 = Rcpp::DataFrame::create(
        Named("H1w") = Ha1w_1,
        Named("H2w") = Ha2w_1,
        Named("H3w") = Ha3w_1,
        Named("H4w") = Ha4w_1,
        Named("H5w") = Ha5w_1,
        Named("H6w") = Ha6w_1,
        Named("H7w") = Ha7w_1,
        Named("H8w") = Ha8w_1,
        Named("H9w") = Ha9w_1);
    
    // weekly incidence by age - fitted to incidence data
    Rcpp::DataFrame byw_ageD_0 = Rcpp::DataFrame::create(
        Named("DH1w") = DHa1w_0,
        Named("DH2w") = DHa2w_0,
        Named("DH3w") = DHa3w_0,
        Named("DH4w") = DHa4w_0,
        Named("DH5w") = DHa5w_0,
        Named("DH6w") = DHa6w_0,
        Named("DH7w") = DHa7w_0,
        Named("DH8w") = DHa8w_0,
        Named("DH9w") = DHa9w_0,
        Named("DO1w") = DOa1w_0,
        Named("DO2w") = DOa2w_0,
        Named("DO3w") = DOa3w_0,
        Named("DO4w") = DOa4w_0,
        Named("DO5w") = DOa5w_0,
        Named("DO6w") = DOa6w_0,
        Named("DO7w") = DOa7w_0,
        Named("DO8w") = DOa8w_0,
        Named("DO9w") = DOa9w_0);

    // weekly incidence by age - fitted to incidence data
    Rcpp::DataFrame byw_ageD_1 = Rcpp::DataFrame::create(
        Named("DH1w") = DHa1w_1,
        Named("DH2w") = DHa2w_1,
        Named("DH3w") = DHa3w_1,
        Named("DH4w") = DHa4w_1,
        Named("DH5w") = DHa5w_1,
        Named("DH6w") = DHa6w_1,
        Named("DH7w") = DHa7w_1,
        Named("DH8w") = DHa8w_1,
        Named("DH9w") = DHa9w_1,
        Named("DO1w") = DOa1w_1,
        Named("DO2w") = DOa2w_1,
        Named("DO3w") = DOa3w_1,
        Named("DO4w") = DOa4w_1,
        Named("DO5w") = DOa5w_1,
        Named("DO6w") = DOa6w_1,
        Named("DO7w") = DOa7w_1,
        Named("DO8w") = DOa8w_1,
        Named("DO9w") = DOa9w_1);    
    

    return Rcpp::List::create(Rcpp::Named("byw_0") = byw_0,
                              Rcpp::Named("byw_1") = byw_1,
                              Rcpp::Named("byw_ageH_0") = byw_ageH_0,
                              Rcpp::Named("byw_ageH_1") = byw_ageH_1,
                              Rcpp::Named("byw_ageD_0") = byw_ageD_0,
                              Rcpp::Named("byw_ageD_1") = byw_ageD_1);
}
