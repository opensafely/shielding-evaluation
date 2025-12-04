//##############################################################################################
//# SEIUHRD age-structured, stratified-contact compartment model, age profiles: u, y, h, m
		 
#include <Rcpp.h>
using namespace Rcpp;
//using namespace std;
#include <array>
// [[Rcpp::export]]
List SEIUHRD(List pars){ 

    // array sizes
    const int nt(pars["nt"]); 
    const int na(pars["na"]);
    const int nw(pars["nw"]);
	
	//it index for start of each week
    IntegerVector iW(nw); // required, can't def vector<int>
	
    // Contact matrices
    const std::vector<double> cm_0(pars["cm_0"]);
    const std::vector<double> cm_1(pars["cm_1"]);
    const int                cmdim1(pars["cmdim1"]);
    const int                cmdim2(pars["cmdim2"]);
    
    // States(age,time) = 0 - need initialise S(na) etc
    std::vector<double> S_0(na);
    std::vector<double> S_1(na);
    std::vector<double> E_0(na);
    std::vector<double> E_1(na);
    std::vector<double> I_0(na);
    std::vector<double> I_1(na);
    std::vector<double> U_0(na);
    std::vector<double> U_1(na);
    std::vector<double> C1_0(na);
    std::vector<double> C1_1(na);
    std::vector<double> C2_0(na);
    std::vector<double> C2_1(na);
    std::vector<double> C3_0(na);
    std::vector<double> C3_1(na);
    std::vector<double> C4_0(na);
    std::vector<double> C4_1(na);
    std::vector<double> C5_0(na);
    std::vector<double> C5_1(na);
    std::vector<double> H1_0(na);
    std::vector<double> H1_1(na);
    std::vector<double> H2_0(na);
    std::vector<double> H2_1(na);
    std::vector<double> H3_0(na);
    std::vector<double> H3_1(na);
    std::vector<double> H4_0(na);
    std::vector<double> H4_1(na);
    std::vector<double> H5_0(na);
    std::vector<double> H5_1(na);
    std::vector<double> O1_0(na);
    std::vector<double> O1_1(na);
    std::vector<double> O2_0(na);
    std::vector<double> O2_1(na);
    std::vector<double> O3_0(na);
    std::vector<double> O3_1(na);
    std::vector<double> O4_0(na);
    std::vector<double> O4_1(na);
    std::vector<double> O5_0(na);
    std::vector<double> O5_1(na);
    std::vector<double> R_0(na);
    std::vector<double> R_1(na);
    std::vector<double> D_0(na);	
    std::vector<double> D_1(na);	
    std::vector<double> N_0(na);
    std::vector<double> N_1(na);
    
    // initial states at time[0] - age-vector - split below between ps and 1-ps
    std::vector<double> Sa0(pars["Sa0"]);
    std::vector<double> Ea0(pars["Ea0"]);
    std::vector<double> Ia0(pars["Ia0"]);
    std::vector<double> Ua0(pars["Ua0"]);
    std::vector<double> Ha0(pars["Ha0"]);
    std::vector<double> Oa0(pars["Oa0"]);
    std::vector<double> Ra0(pars["Ra0"]);
    std::vector<double> Da0(pars["Da0"]);
    std::vector<double> Na0(pars["Na0"]);

    // States(time) = 0 - need initialise St[0] etc, time[0]
    double St_0;
    double St_1;
    double Et_0;
    double Et_1;
    double It_0=0; 
    double It_1=0;
    double Ut_0=0; 
    double Ut_1=0; 
    double Ct_0=0;
    double Ct_1=0; 
    double Ht_0;
    double Ht_1;
    double Ot_0;
    double Ot_1;
    double Rt_0;
    double Rt_1;
    double Dt_0;
    double Dt_1; 
    double Nt_0;
    double Nt_1;
	
    std::vector<double> Hw_0(nw);
    std::vector<double> Hw_1(nw);
    std::vector<double> Dw_0(nw);
    std::vector<double> Dw_1(nw);
    std::vector<double> DHw_0(nw);
    std::vector<double> DHw_1(nw);
    std::vector<double> DOw_0(nw);
    std::vector<double> DOw_1(nw);
	std::vector<double> Cw_0(nw);
    std::vector<double> Cw_1(nw);
	std::vector<double> Iw_0(nw);
    std::vector<double> Iw_1(nw);
	std::vector<double> Uw_0(nw);
    std::vector<double> Uw_1(nw);
	
    NumericVector time(nt);       //required w IntegerVector index iW (latter required rather than vector<int>)

    //weekly incidence H and D - by age
    std::vector<double> Hapw_0(na);
    std::vector<double> Hapw_1(na);
    std::vector<double> Dapw_0(na);
    std::vector<double> Dapw_1(na);
    std::vector<double> DHapw_0(na);
    std::vector<double> DHapw_1(na);
    std::vector<double> DOapw_0(na);
    std::vector<double> DOapw_1(na);
    std::vector<double> Ha1w_0(nw);
    std::vector<double> Ha1w_1(nw);
    std::vector<double> Ha2w_0(nw);
    std::vector<double> Ha2w_1(nw);
    std::vector<double> Ha3w_0(nw);
    std::vector<double> Ha3w_1(nw);
    std::vector<double> Ha4w_0(nw);
    std::vector<double> Ha4w_1(nw);
    std::vector<double> Ha5w_0(nw);
    std::vector<double> Ha5w_1(nw);
    std::vector<double> Ha6w_0(nw);
    std::vector<double> Ha6w_1(nw);
    std::vector<double> Ha7w_0(nw);
    std::vector<double> Ha7w_1(nw);
    std::vector<double> Ha8w_0(nw);
    std::vector<double> Ha8w_1(nw);
    std::vector<double> Ha9w_0(nw);
    std::vector<double> Ha9w_1(nw);
    std::vector<double> Da1w_0(nw);
    std::vector<double> Da1w_1(nw);
    std::vector<double> Da2w_0(nw);
    std::vector<double> Da2w_1(nw);
    std::vector<double> Da3w_0(nw);
    std::vector<double> Da3w_1(nw);
    std::vector<double> Da4w_0(nw);
    std::vector<double> Da4w_1(nw);
    std::vector<double> Da5w_0(nw);
    std::vector<double> Da5w_1(nw);
    std::vector<double> Da6w_0(nw);
    std::vector<double> Da6w_1(nw);
    std::vector<double> Da7w_0(nw);
    std::vector<double> Da7w_1(nw);
    std::vector<double> Da8w_0(nw);
    std::vector<double> Da8w_1(nw);
    std::vector<double> Da9w_0(nw);
    std::vector<double> Da9w_1(nw);
    std::vector<double> DHa1w_0(nw);
    std::vector<double> DHa1w_1(nw);
    std::vector<double> DHa2w_0(nw);
    std::vector<double> DHa2w_1(nw);
    std::vector<double> DHa3w_0(nw);
    std::vector<double> DHa3w_1(nw);
    std::vector<double> DHa4w_0(nw);
    std::vector<double> DHa4w_1(nw);
    std::vector<double> DHa5w_0(nw);
    std::vector<double> DHa5w_1(nw);
    std::vector<double> DHa6w_0(nw);
    std::vector<double> DHa6w_1(nw);
    std::vector<double> DHa7w_0(nw);
    std::vector<double> DHa7w_1(nw);
    std::vector<double> DHa8w_0(nw);
    std::vector<double> DHa8w_1(nw);
    std::vector<double> DHa9w_0(nw);
    std::vector<double> DHa9w_1(nw);
    std::vector<double> DOa1w_0(nw);
    std::vector<double> DOa1w_1(nw);
    std::vector<double> DOa2w_0(nw);
    std::vector<double> DOa2w_1(nw);
    std::vector<double> DOa3w_0(nw);
    std::vector<double> DOa3w_1(nw);
    std::vector<double> DOa4w_0(nw);
    std::vector<double> DOa4w_1(nw);
    std::vector<double> DOa5w_0(nw);
    std::vector<double> DOa5w_1(nw);
    std::vector<double> DOa6w_0(nw);
    std::vector<double> DOa6w_1(nw);
    std::vector<double> DOa7w_0(nw);
    std::vector<double> DOa7w_1(nw);
    std::vector<double> DOa8w_0(nw);
    std::vector<double> DOa8w_1(nw);
    std::vector<double> DOa9w_0(nw);    
    std::vector<double> DOa9w_1(nw);    
    // read parameters
    const std::vector<double> u(pars["u"]); 
    const std::vector<double> y_0(pars["y_0"]); 
    const std::vector<double> y_1(pars["y_1"]); 
    const std::vector<double> h_0(pars["h_0"]); 
    const std::vector<double> h_1(pars["h_1"]); 
    const std::vector<double> m_a_0(pars["ma_0"]); 
    const std::vector<double> m_b_0(pars["mb_0"]); 
    const std::vector<double> m_a_1(pars["ma_1"]); 
    const std::vector<double> m_b_1(pars["mb_1"]); 
    const std::vector<double> d_0(pars["d_0"]); 
    const std::vector<double> d_1(pars["d_1"]); 
    const std::vector<double> psage_1(pars["psagecoh"]); 
    const std::vector<double> rseed(pars["rseed"]);
    
    const double beta_infectivity = pars["beta"];
    const double fu   = pars["fu"];
    const double fuO  = pars["fuO"];
    const double ad_0 = pars["ad_0"];
    const double ad_1 = pars["ad_1"];
    const double rEI  = pars["rEI"];
    const double rEU  = pars["rEU"];
    const double rIR  = pars["rIR"];
    const double rIO  = pars["rIO"];
    const double rOD  = pars["rOD"];  
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
    const double rOi  = 5*rOD;
    const double dt   = pars["dt"];

    double pa_0, pa_1;
	double Sa0i, Ea0i, Ia0i, Ua0i, IUa0i, Ha0i, Ra0i, Da0i, Na0i;

    // age group and population states initialised
    for (int ia = 0; ia < na; ia++) {
        pa_1 = psage_1[ia], pa_0 = 1-psage_1[ia];
		Sa0i = Sa0[ia], Ea0i = Ea0[ia], Ia0i = Ia0[ia], Ua0i = Ua0[ia]; 
		Ha0i = Ha0[ia], Ra0i = Ra0[ia], Da0i = Da0[ia], Na0i = Na0[ia];
		IUa0i = Ea0i;  //Ia0i+Ua0i; //
        S_0[ia]  = Sa0i*pa_0;        St_0 += Sa0i*pa_0;
        S_1[ia]  = Sa0i*pa_1;        St_1 += Sa0i*pa_1;
        E_0[ia]  = Ea0i*pa_0;        Et_0 += Ea0i*pa_0;
        E_1[ia]  = Ea0i*pa_1;        Et_1 += Ea0i*pa_1;
        I_0[ia]  = Ia0i*pa_0;        It_0 += Ia0i*pa_0;
        I_1[ia]  = Ia0i*pa_1;        It_1 += Ia0i*pa_1;
        U_0[ia]  = Ua0i*pa_0;        Ut_0 += Ua0i*pa_0;
        U_1[ia]  = Ua0i*pa_1;        Ut_1 += Ua0i*pa_1;
        C1_0[ia] = IUa0i*pa_0;       Ct_0 += IUa0i*pa_0;
        C1_1[ia] = IUa0i*pa_1;       Ct_1 += IUa0i*pa_1;
        C2_0[ia] = 0;  
        C2_1[ia] = 0;  
        C3_0[ia] = 0;   
        C3_1[ia] = 0;   
        C4_0[ia] = 0;   
        C4_1[ia] = 0;   
        C5_0[ia] = 0;
        C5_1[ia] = 0;
        H1_0[ia] = Ha0i*pa_0;        Ht_0 += Ha0i*pa_0;   Hw_0[0] += 0;
        H1_1[ia] = Ha0i*pa_1;        Ht_1 += Ha0i*pa_1;   Hw_1[0] += 0;
        H2_0[ia] = 0;
        H2_1[ia] = 0;
        H3_0[ia] = 0;
        H3_1[ia] = 0;
        H4_0[ia] = 0;
        H4_1[ia] = 0;
        H5_0[ia] = 0;
        H5_1[ia] = 0;
        O1_0[ia] = Ua0i*pa_0;           Ot_0 += Ua0i*pa_0;
        O1_1[ia] = Ua0i*pa_1;           Ot_1 += Ua0i*pa_1;
        O2_0[ia] = 0;
        O2_1[ia] = 0;
        O3_0[ia] = 0;
        O3_1[ia] = 0;
        O4_0[ia] = 0;
        O4_1[ia] = 0;
        O5_0[ia] = 0;
        O5_1[ia] = 0;
        R_0[ia]  = Ra0i*pa_0;        Rt_0 += Ra0i*pa_0;
        R_1[ia]  = Ra0i*pa_1;        Rt_1 += Ra0i*pa_1;
        D_0[ia]  = Da0i*pa_0;        Dt_0 += Da0i*pa_0;   Dw_0[0] += 0; DHw_0[0] += 0; DOw_0[0] += 0;
        D_1[ia]  = Da0i*pa_1;        Dt_1 += Da0i*pa_1;   Dw_1[0] += 0; DHw_1[0] += 0; DOw_1[0] += 0;
        N_0[ia]  = Na0i*pa_0;        Nt_0 += Na0i*pa_0;
        N_1[ia]  = Na0i*pa_1;        Nt_1 += Na0i*pa_1;
    }

    // Variables in loops
	double Sat_0, Sat_1, Eat_0, Eat_1, Iat_0, Iat_1, Uat_0, Uat_1, 
	       C1at_0, C2at_0, C3at_0, C4at_0, C5at_0,
	       C1at_1, C2at_1, C3at_1, C4at_1, C5at_1,
	       H1at_0, H2at_0, H3at_0, H4at_0, H5at_0,
	       H1at_1, H2at_1, H3at_1, H4at_1, H5at_1,
		     O1at_0, O2at_0, O3at_0, O4at_0, O5at_0,
	       O1at_1, O2at_1, O3at_1, O4at_1, O5at_1,
	       Rat_0, Rat_1, Dat_0, Dat_1, Nat_0, Nat_1;
	double lambda_0, lambda_1, beta_ua, ya_0, ya_1, ha_0, ha_1, ma_0, ma_1, da_0, da_1, rseeda_0, rseeda_1;
	double cmi_0, cmi_1, O_sum_0, O_sum_1, inffrom_0, inffrom_1;
	int icm;
	double dS_0, dS_1, dE_0, dE_1, dI_0, dI_1, dU_0, dU_1, 
	       dC1_0, dC2_0, dC3_0, dC4_0, dC5_0, dC1_1, dC2_1, dC3_1, dC4_1, dC5_1,
	       dH1_0, dH2_0, dH3_0, dH4_0, dH5_0, dH1_1, dH2_1, dH3_1, dH4_1, dH5_1,
	       dO1_0, dO2_0, dO3_0, dO4_0, dO5_0, dO1_1, dO2_1, dO3_1, dO4_1, dO5_1;
    double dOin_0, dOin_1, dHin_0, dHin_1, dR_0, dR_1, dDin_0, dDin_1, 
	       dDH_0, dDH_1, dDO_0, dDO_1, dD_0, dD_1, dN_0, dN_1;

    // Dynamics of state variables - Euler integration
    time[0] = 0;
	iW[0]   = 0;
    int  week  = 1;
    int  week0 = 1;
    double seedon = 0;
	Cw_0[0] = Ct_0;
	Cw_1[0] = Ct_1;
    Iw_0[0] = It_0;
	Iw_1[0] = It_1;
    Uw_0[0] = Ut_0;
	Uw_1[0] = Ut_1;
    double  Hpw_0 = 0,   Hpw_1 = 0; //overall
    double  Dpw_0 = 0,   Dpw_1 = 0;
    double DHpw_0 = 0,  DHpw_1 = 0;
    double DOpw_0 = 0,  DOpw_1 = 0;
    double rHR = rHRa;
    double rHi = 5*rHDa;
    double qma = 1;
    for (int it = 0; it < (nt-1); it++) {	//Crucial: nt-1
        week0 = week;
        week  = 1 + (int) time[it]/7;
        if (week <= 4) {seedon = 1;};
        if (week > 22) {rHR = rHRb;  rHi = 5*rHDb;  qma=0;};
		
		St_0 = 0;
        St_1 = 0;
        Et_0 = 0;
        Et_1 = 0;
        It_0 = 0;
        It_1 = 0;
        Ut_0 = 0;
        Ut_1 = 0;
        Ct_0 = 0;
        Ct_1 = 0;
        Ht_0 = 0;
        Ht_1 = 0;
        Ot_0 = 0;
        Ot_1 = 0;
        Rt_0 = 0;
        Rt_1 = 0;
        Dt_0 = 0;
        Dt_1 = 0;
        Nt_0 = 0;
        Nt_1 = 0;
		//Only output Ct, It, Ut - only really need to reset these
		
        for (int ia = 0; ia < na; ia++) {
          
        // current matrix cells		   
        Sat_0 = S_0[ia];
        Sat_1 = S_1[ia];
        Eat_0 = E_0[ia];
        Eat_1 = E_1[ia];
        Iat_0 = I_0[ia];
        Iat_1 = I_1[ia];
        Uat_0 = U_0[ia];
        Uat_1 = U_1[ia];
        C1at_0 = C1_0[ia];
        C1at_1 = C1_1[ia];
        C2at_0 = C2_0[ia];
        C2at_1 = C2_1[ia];
        C3at_0 = C3_0[ia];
        C3at_1 = C3_1[ia];
        C4at_0 = C4_0[ia];
        C4at_1 = C4_1[ia];
        C5at_0 = C5_0[ia];
        C5at_1 = C5_1[ia];
        H1at_0 = H1_0[ia];
        H1at_1 = H1_1[ia];
        H2at_0 = H2_0[ia];
        H2at_1 = H2_1[ia];
        H3at_0 = H3_0[ia];
        H3at_1 = H3_1[ia];
        H4at_0 = H4_0[ia];
        H4at_1 = H4_1[ia];
        H5at_0 = H5_0[ia];
        H5at_1 = H5_1[ia];
        O1at_0 = O1_0[ia];
        O1at_1 = O1_1[ia];
        O2at_0 = O2_0[ia];
        O2at_1 = O2_1[ia];
        O3at_0 = O3_0[ia];
        O3at_1 = O3_1[ia];
        O4at_0 = O4_0[ia];
        O4at_1 = O4_1[ia];
        O5at_0 = O5_0[ia];
        O5at_1 = O5_1[ia];
        Rat_0 = R_0[ia];
        Rat_1 = R_1[ia];
        Dat_0 = D_0[ia];
        Dat_1 = D_1[ia];
        Nat_0 = N_0[ia];
        Nat_1 = N_1[ia];
        
        // force of infection on group ia	
        lambda_0 = 0;
		lambda_1 = 0;
        beta_ua  = beta_infectivity*u[ia];
        ya_0 = y_0[ia];
        ya_1 = y_1[ia];
        ha_0 = h_0[ia];
        ha_1 = h_1[ia];
        ma_0 = m_a_0[ia]*qma + m_b_0[ia]*(1-qma);
        ma_1 = m_a_1[ia]*qma + m_b_1[ia]*(1-qma);
        da_0 = d_0[ia]*ad_0;
        da_1 = d_1[ia]*ad_1;
        rseeda_0 = seedon*rseed[ia]*(1-psage_1[ia]);
        rseeda_1 = seedon*rseed[ia]*psage_1[ia];

        
        for (int ib = 0; ib < na; ib++) {
            icm = (week-1)*cmdim1*cmdim2 + ib*cmdim1 + ia;
            cmi_0 = cm_0[icm];
            cmi_1 = cm_1[icm];
            O_sum_0   = O1_0[ib] + O2_0[ib] + O3_0[ib] + O4_0[ib] + O5_0[ib];
            O_sum_1   = O1_1[ib] + O2_1[ib] + O3_1[ib] + O4_1[ib] + O5_1[ib];
            inffrom_0 =(1-psage_1[ib])*(I_0[ib]  + fuO*O_sum_0  + fu*U_0[ib] )/N_0[ib];
            inffrom_1 =   psage_1[ib]*( I_1[ib]  + fuO*O_sum_1  + fu*U_1[ib] )/N_1[ib];
            //Outside hospital delay to death (assume  effective infectivity fuO=0)
            //Force of infection on each stratum       
            lambda_0      += beta_ua*cmi_0*( inffrom_0 + inffrom_1 ); //  prob(contact=s)xprob(s infected)
            lambda_1      += beta_ua*cmi_1*( inffrom_0 + inffrom_1 ); //+ prob(contact=n)xprob(n infected)         
        } //ib

        //Infection update for next timestep	
        dS_0  = dt*(-lambda_0*Sat_0       + rRS*Rat_0 - rseeda_0);
        dS_1  = dt*(-lambda_1*Sat_1       + rRS*Rat_1 - rseeda_1); 
        dE_0  = dt*( lambda_0*Sat_0       - (rEU*(1-ya_0) + rEI*ya_0)*Eat_0 +  rseeda_0); //early seeding, no effect later
        dE_1  = dt*( lambda_1*Sat_1       - (rEU*(1-ya_1) + rEI*ya_1)*Eat_1 +  rseeda_1); //early seeding, no effect later
        dI_0  = dt*( rEI*ya_0*Eat_0       - (rIR*(1-ha_0-da_0) + rIH*ha_0 + rIO*da_0)*Iat_0 );
        dI_1  = dt*( rEI*ya_1*Eat_1       - (rIR*(1-ha_1-da_1) + rIH*ha_1 + rIO*da_1)*Iat_1 );
        dU_0  = dt*( rEU*(1-ya_0)*Eat_0   - rUR*Uat_0 );
        dU_1  = dt*( rEU*(1-ya_1)*Eat_1   - rUR*Uat_1 );

        //Marker of longer duration of positivity in tests compared to duration of E (infection becoming I or U)
        dC1_0 = dt*( lambda_0*Sat_0 - rCi*C1at_0);
        dC1_1 = dt*( lambda_1*Sat_1 - rCi*C1at_1);
        dC2_0 = dt*( rCi*C1at_0     - rCi*C2at_0);
        dC2_1 = dt*( rCi*C1at_1     - rCi*C2at_1);
        dC3_0 = dt*( rCi*C2at_0     - rCi*C3at_0);
        dC3_1 = dt*( rCi*C2at_1     - rCi*C3at_1);
        dC4_0 = dt*( rCi*C3at_0     - rCi*C4at_0);
        dC4_1 = dt*( rCi*C3at_1     - rCi*C4at_1);
        dC5_0 = dt*( rCi*C4at_0     - rCi*C5at_0); //decays but doesn't flow to another state
        dC5_1 = dt*( rCi*C4at_1     - rCi*C5at_1);

        //DO - post-I delay in death outside hospital	
        dOin_0= dt*rIO*da_0*Iat_0;
        dOin_1= dt*rIO*da_1*Iat_1;
        dO1_0 = dOin_0       - dt*( rOi*O1at_0);
        dO1_1 = dOin_1       - dt*( rOi*O1at_1);
        dO2_0 = dt*( rOi*O1at_0   - rOi*O2at_0);
        dO2_1 = dt*( rOi*O1at_1   - rOi*O2at_1);
        dO3_0 = dt*( rOi*O2at_0   - rOi*O3at_0);
        dO3_1 = dt*( rOi*O2at_1   - rOi*O3at_1);
        dO4_0 = dt*( rOi*O3at_0   - rOi*O4at_0);
        dO4_1 = dt*( rOi*O3at_1   - rOi*O4at_1);
        dO5_0 = dt*( rOi*O4at_0   - rOi*O5at_0); 
        dO5_1 = dt*( rOi*O4at_1   - rOi*O5at_1);

        //H, DH  - post-I delay in death or recovery in hospital    
        dHin_0= dt*rIH*ha_0*Iat_0;  //hospitalisation incidence
        dHin_1= dt*rIH*ha_1*Iat_1;
        dH1_0 = dHin_0       - dt*( rHi*H1at_0);
        dH1_1 = dHin_1       - dt*( rHi*H1at_1);
        dH2_0 = dt*( rHi*H1at_0   - rHi*H2at_0);
        dH2_1 = dt*( rHi*H1at_1   - rHi*H2at_1);
        dH3_0 = dt*( rHi*H2at_0   - rHi*H3at_0);
        dH3_1 = dt*( rHi*H2at_1   - rHi*H3at_1);
        dH4_0 = dt*( rHi*H3at_0   - rHi*H4at_0);
        dH4_1 = dt*( rHi*H3at_1   - rHi*H4at_1);
        dH5_0 = dt*( rHi*H4at_0   - ((rHR*5)*(1-ma_0)+rHi*ma_0)*H5at_0); //rHi=5*rHD //Recovery time def could be different
        dH5_1 = dt*( rHi*H4at_1   - ((rHR*5)*(1-ma_1)+rHi*ma_1)*H5at_1);

        //R  - recovery from all infections inside or outside hospital         
        dR_0  = dt*( rUR*Uat_0 + rIR*(1-ha_0-da_0)*Iat_0 + (rHR*5)*(1-ma_0)*H5at_0 - rRS*Rat_0);
        dR_1  = dt*( rUR*Uat_1 + rIR*(1-ha_1-da_1)*Iat_1 + (rHR*5)*(1-ma_1)*H5at_1 - rRS*Rat_1);
        //D - deaths from I infections inside or outside hospital  
        dDin_0= dt*( rHi*ma_0*H5at_0 + rOi*O5at_0); //death incidence
        dDin_1= dt*( rHi*ma_1*H5at_1 + rOi*O5at_1);
        dDH_0 = dt*( rHi*ma_0*H5at_0 );             //death incidence inside hospital
        dDH_1 = dt*( rHi*ma_1*H5at_1 );
        dDO_0 = dt*( rOi*O5at_0    );               //death incidence outside hospital
        dDO_1 = dt*( rOi*O5at_1    );
        dD_0  = dDH_0 + dDO_0;                      //death change = incidence
        dD_1  = dDH_1 + dDO_1;
        dN_0  = dS_0 + dE_0 + dU_0 + dI_0 + (dH1_0 + dH2_0 + dH3_0 + dH4_0 + dH5_0) + (dO1_0 + dO2_0 + dO3_0 + dO4_0 + dO5_0) + dR_0 + dD_0;
        dN_1  = dS_1 + dE_1 + dU_1 + dI_1 + (dH1_1 + dH2_1 + dH3_1 + dH4_1 + dH5_1) + (dO1_1 + dO2_1 + dO3_1 + dO4_1 + dO5_1) + dR_1 + dD_1;

        S_0[ia]  = Sat_0  + dS_0;
        S_1[ia]  = Sat_1  + dS_1;
        E_0[ia]  = Eat_0  + dE_0;
        E_1[ia]  = Eat_1  + dE_1;
        I_0[ia]  = Iat_0  + dI_0;
        I_1[ia]  = Iat_1  + dI_1;
        U_0[ia]  = Uat_0  + dU_0;
        U_1[ia]  = Uat_1  + dU_1;
        C1_0[ia] = C1at_0 + dC1_0;
        C1_1[ia] = C1at_1 + dC1_1;
        C2_0[ia] = C2at_0 + dC2_0;
        C2_1[ia] = C2at_1 + dC2_1;
        C3_0[ia] = C3at_0 + dC3_0;
        C3_1[ia] = C3at_1 + dC3_1;
        C4_0[ia] = C4at_0 + dC4_0;
        C4_1[ia] = C4at_1 + dC4_1;
        C5_0[ia] = C5at_0 + dC5_0;
        C5_1[ia] = C5at_1 + dC5_1;
        H1_0[ia] = H1at_0 + dH1_0;
        H1_1[ia] = H1at_1 + dH1_1;
        H2_0[ia] = H2at_0 + dH2_0;
        H2_1[ia] = H2at_1 + dH2_1;
        H3_0[ia] = H3at_0 + dH3_0;
        H3_1[ia] = H3at_1 + dH3_1;
        H4_0[ia] = H4at_0 + dH4_0;
        H4_1[ia] = H4at_1 + dH4_1;
        H5_0[ia] = H5at_0 + dH5_0;
        H5_1[ia] = H5at_1 + dH5_1;
        O1_0[ia] = O1at_0 + dO1_0;
        O1_1[ia] = O1at_1 + dO1_1;
        O2_0[ia] = O2at_0 + dO2_0;
        O2_1[ia] = O2at_1 + dO2_1;
        O3_0[ia] = O3at_0 + dO3_0;
        O3_1[ia] = O3at_1 + dO3_1;
        O4_0[ia] = O4at_0 + dO4_0;
        O4_1[ia] = O4at_1 + dO4_1;
        O5_0[ia] = O5at_0 + dO5_0;
        O5_1[ia] = O5at_1 + dO5_1;
        R_0[ia]  = Rat_0  + dR_0;
        R_1[ia]  = Rat_1  + dR_1;
        D_0[ia]  = Dat_0  + dD_0; 
        D_1[ia]  = Dat_1  + dD_1; 
        N_0[ia]  = Nat_0  + dN_0;
        N_1[ia]  = Nat_1  + dN_1;
        // time update (units of parameters)
        time[it+1] = (it+1)*dt;
        // population states evaluated (t>0)
        St_0  += Sat_0 + dS_0;
        St_1  += Sat_1 + dS_1;
        Et_0  += Eat_0 + dE_0;
        Et_1  += Eat_1 + dE_1;
        It_0  += Iat_0 + dI_0;
        It_1  += Iat_1 + dI_1;
        Ut_0  += Uat_0 + dU_0;
        Ut_1  += Uat_1 + dU_1;
        Ct_0  += C1at_0 + C2at_0 + C3at_0 + C4at_0 + C5at_0 + dC1_0 + dC2_0 + dC3_0 + dC4_0 + dC5_0;
        Ct_1  += C1at_1 + C2at_1 + C3at_1 + C4at_1 + C5at_1 + dC1_1 + dC2_1 + dC3_1 + dC4_1 + dC5_1;
        Ht_0  += H1at_0 + H2at_0 + H3at_0 + H4at_0 + H5at_0 + dH1_0 + dH2_0 + dH3_0 + dH4_0 + dH5_0;
        Ht_1  += H1at_1 + H2at_1 + H3at_1 + H4at_1 + H5at_1 + dH1_1 + dH2_1 + dH3_1 + dH4_1 + dH5_1;
        Ot_0  += O1at_0 + O2at_0 + O3at_0 + O4at_0 + O5at_0 + dO1_0 + dO2_0 + dO3_0 + dO4_0 + dO5_0;
        Ot_1  += O1at_1 + O2at_1 + O3at_1 + O4at_1 + O5at_1 + dO1_1 + dO2_1 + dO3_1 + dO4_1 + dO5_1;
        Rt_0  += Rat_0 + dR_0;
        Rt_1  += Rat_1 + dR_1;
        Dt_0  += Dat_0 + dD_0;
        Dt_1  += Dat_1 + dD_1;
        Nt_0  += Nat_0 + dN_0;
        Nt_1  += Nat_1 + dN_1;
		
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
		  iW[week-1]   = it;
		  //positive and infection prevalence - current population level, not incidences
		  Iw_0[week-1] = It_0;
		  Iw_1[week-1] = It_1;
		  Uw_0[week-1] = Ut_0;
		  Uw_1[week-1] = Ut_1;
		  Cw_0[week-1] = Ct_0;
		  Cw_1[week-1] = Ct_1;
		  //incidences
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
	    Named("iW") = iW,
        Named("time") = time[iW],
        // weekly totals
        Named("It")   = Iw_0,
        Named("Ut")   = Uw_0,
        Named("Ct")   = Cw_0,
        // weekly incidence - fitted to incidence data
        Named("Hw")   = Hw_0,
        Named("Dw")   = Dw_0,
        Named("DHw")  = DHw_0,
        Named("DOw")  = DOw_0);
    Rcpp::DataFrame byw_1 = Rcpp::DataFrame::create(
        // weekly totals
        Named("It")   = Iw_1,
        Named("Ut")   = Uw_1,
        Named("Ct")   = Cw_1,
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
