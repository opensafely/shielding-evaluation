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
    
    // contact matrix
    const NumericVector cm = as<NumericVector>(pars["cm"]);
    const int cmdim1 = as<int>(pars["cmdim1"]);
    const int cmdim2 = as<int>(pars["cmdim2"]);
    const double ocmdim = std::pow(cmdim1*cmdim2,-1);
    
    // age-group states = 0, need initialise S(na,0) etc
    NumericMatrix S(na,nt);
    NumericMatrix E(na,nt);
    NumericMatrix I(na,nt);
    NumericMatrix U(na,nt);
    NumericMatrix C1(na,nt);
    NumericMatrix C2(na,nt);
    NumericMatrix C3(na,nt);
    NumericMatrix C4(na,nt);
    NumericMatrix C5(na,nt);
    NumericMatrix H1(na,nt);
    NumericMatrix H2(na,nt);
    NumericMatrix H3(na,nt);
    NumericMatrix H4(na,nt);
    NumericMatrix H5(na,nt);
    NumericMatrix O1(na,nt);
    NumericMatrix O2(na,nt);
    NumericMatrix O3(na,nt);
    NumericMatrix O4(na,nt);
    NumericMatrix O5(na,nt);
    NumericMatrix R(na,nt);
    NumericMatrix D(na,nt);	
    NumericMatrix N(na,nt);
    
    // age-group initial states at time[0]
    NumericVector Sa0 = as<NumericVector>(pars["Sa0"]);
    NumericVector Ea0 = as<NumericVector>(pars["Ea0"]);
    NumericVector Ia0 = as<NumericVector>(pars["Ia0"]);
    NumericVector Ua0 = as<NumericVector>(pars["Ua0"]);
    NumericVector Ha0 = as<NumericVector>(pars["Ha0"]);
    NumericVector Oa0 = as<NumericVector>(pars["Oa0"]);
    NumericVector Ra0 = as<NumericVector>(pars["Ra0"]);
    NumericVector Da0 = as<NumericVector>(pars["Da0"]);
    NumericVector Na0 = as<NumericVector>(pars["Na0"]);

    // population states = 0, need initialise St[0] etc, time[0]
    NumericVector St(nt);
    NumericVector Et(nt);
    NumericVector It(nt);
    NumericVector Ut(nt);
    NumericVector Ct(nt);
    NumericVector Ht(nt);
    NumericVector Ot(nt);
    NumericVector Rt(nt);
    NumericVector Dt(nt); //DHt(nt), DO(nt) not yet
    NumericVector Nt(nt);
    NumericVector Hw(nw);
    NumericVector Dw(nw);
    NumericVector DHw(nw);
    NumericVector DOw(nw);
    NumericVector time(nt);
    NumericVector cmdtmean(nt);
    
    //weekly H and D, DH, DO by age
    NumericVector Hapw(na);
    NumericVector Dapw(na);
    NumericVector DHapw(na);
    NumericVector DOapw(na);
    NumericVector Ha1w(nw);
    NumericVector Ha2w(nw);
    NumericVector Ha3w(nw);
    NumericVector Ha4w(nw);
    NumericVector Ha5w(nw);
    NumericVector Ha6w(nw);
    NumericVector Ha7w(nw);
    NumericVector Ha8w(nw);
    NumericVector Ha9w(nw);
    NumericVector Da1w(nw);
    NumericVector Da2w(nw);
    NumericVector Da3w(nw);
    NumericVector Da4w(nw);
    NumericVector Da5w(nw);
    NumericVector Da6w(nw);
    NumericVector Da7w(nw);
    NumericVector Da8w(nw);
    NumericVector Da9w(nw);
    NumericVector DHa1w(nw);
    NumericVector DHa2w(nw);
    NumericVector DHa3w(nw);
    NumericVector DHa4w(nw);
    NumericVector DHa5w(nw);
    NumericVector DHa6w(nw);
    NumericVector DHa7w(nw);
    NumericVector DHa8w(nw);
    NumericVector DHa9w(nw);
    NumericVector DOa1w(nw);
    NumericVector DOa2w(nw);
    NumericVector DOa3w(nw);
    NumericVector DOa4w(nw);
    NumericVector DOa5w(nw);
    NumericVector DOa6w(nw);
    NumericVector DOa7w(nw);
    NumericVector DOa8w(nw);
    NumericVector DOa9w(nw);    
    // read parameters
    const NumericVector u = as<NumericVector>(pars["u"]); 
    const NumericVector y = as<NumericVector>(pars["y"]); 
    const NumericVector h = as<NumericVector>(pars["h"]); 
    const NumericVector m = as<NumericVector>(pars["m"]); 
    const NumericVector d = as<NumericVector>(pars["d"]);
    const NumericVector rseed = as<NumericVector>(pars["rseed"]); //per age group

    const double beta_infectivity = pars["beta"];
    const double phm  = pars["phm"];
    const double fu   = pars["fu"];
    const double ad   = pars["ad"];
    const double rEI  = pars["rEI"];
    const double rEU  = pars["rEU"];
    const double rIR  = pars["rIR"];
    const double rID  = pars["rID"];
    const double rUR  = pars["rUR"];
    const double rIH  = pars["rIH"];
    const double rHR  = pars["rHR"];
    const double rHD  = pars["rHD"];
    const double rRS  = pars["rRS"];
    const double rC   = pars["rC"];
    const double rCi  = 5*rC;
    const double rHi  = 5*rHD;
    const double rOi  = 6*rID;
    const double dt   = pars["dt"];
    
    // age group and population states initialised
    for (int ia = 0; ia < na; ia++) {
        S(ia,0)  = Sa0[ia];   St[0] += S(ia,0);
        E(ia,0)  = Ea0[ia];   Et[0] += E(ia,0);
        I(ia,0)  = Ia0[ia];   It[0] += I(ia,0);
        U(ia,0)  = Ua0[ia];   Ut[0] += U(ia,0);
        C1(ia,0) = Ia0[ia]+Ua0[ia];   
        C2(ia,0) = 0;  
        C3(ia,0) = 0;   
        C4(ia,0) = 0;   
        C5(ia,0) = 0;         Ct[0] += C1(ia,0);
        H1(ia,0) = Ha0[ia];
        H2(ia,0) = 0;
        H3(ia,0) = 0;
        H4(ia,0) = 0;
        H5(ia,0) = 0;         Ht[0] += Ha0[ia];   Hw[0] += 0;
        O1(ia,0) = Oa0[ia];
        O2(ia,0) = 0;
        O3(ia,0) = 0;
        O4(ia,0) = 0;
        O5(ia,0) = 0;         Ot[0] += Oa0[ia];
        R(ia,0)  = Ra0[ia];   Rt[0] += R(ia,0);
        D(ia,0)  = Da0[ia];   Dt[0] += D(ia,0);   Dw[0] += 0; DHw[0] += 0; DOw[0] += 0;
        N(ia,0)  = Na0[ia];   Nt[0] += N(ia,0);
    }


    // Dynamics of state variables - Euler integration
    time[0] = 0;
    int  week  = 1;
    int  week0 = 1;
    double seedon = 0;
    double Hpw = 0;
    double Dpw = 0;
    double DHpw = 0;
    double DOpw = 0;
    for (int it = 0; it < (nt-1); it++) {	//Crucial: nt-1
        week0 = week;
        week  = 1 + (int) time[it]/7;
        if (week <= 4) {seedon = 1;}; //floor(1/ceil(week/4));
        cmdtmean[it] = 0;
         
        for (int ia = 0; ia < na; ia++) {
          
 	 	    // current matrix cells
        double Sat = S(ia,it);
        double Eat = E(ia,it);
        double Iat = I(ia,it);
        double Uat = U(ia,it);
        double C1at = C1(ia,it);
        double C2at = C2(ia,it);
        double C3at = C3(ia,it);
        double C4at = C4(ia,it);
        double C5at = C5(ia,it);
        double H1at = H1(ia,it);
        double H2at = H2(ia,it);
        double H3at = H3(ia,it);
        double H4at = H4(ia,it);
        double H5at = H5(ia,it);
        double O1at = O1(ia,it);
        double O2at = O2(ia,it);
        double O3at = O3(ia,it);
        double O4at = O4(ia,it);
        double O5at = O5(ia,it);
        double Rat = R(ia,it);
        double Dat = D(ia,it);
        double Nat = N(ia,it);

        // force of infection on group ia
        double lambda  = 0;
        double ua = u[ia];
        double ya = y[ia];
        double ha = h[ia];
        double ma = m[ia]*phm;
        double da = d[ia]*ad;
        double rseeda = seedon*rseed[ia];
          
        for (int ib = 0; ib < na; ib++) {
            int    icm = (week-1)*cmdim1*cmdim2 + ib*cmdim1 + ia;
            double cmi = cm[icm];
            double O_sum   = O1(ib,it) + O2(ib,it) + O3(ib,it) + O4(ib,it) + O5(ib,it);
            lambda        += beta_infectivity*ua*cmi*( I(ib,it) + fu*O_sum + fu*U(ib,it) )/N(ib,it);
            //Outside hospital delay to death (assume as infectious as U, i.e. partial isolation)
            cmdtmean[it]  += cmi*ocmdim;
        } //ib

        // state update for next timestep
        double dS  = dt*(-lambda*Sat       + rRS*Rat - rseeda);
        double dE  = dt*( lambda*Sat       - (rEU*(1-ya)+rEI*ya)*Eat  +  rseeda); //early seeding, no effect later
        double dI  = dt*( rEI*ya*Eat       - (rIR*(1-ha-da)+rIH*ha+rOi*da)*Iat);
        double dU  = dt*( rEU*(1-ya)*Eat   - rUR*Uat);

        double dC1 = dt*( lambda*Sat - rCi*C1at);
        double dC2 = dt*( rCi*C1at   - rCi*C2at);
        double dC3 = dt*( rCi*C2at   - rCi*C3at);
        double dC4 = dt*( rCi*C3at   - rCi*C4at);
        double dC5 = dt*( rCi*C4at   - rCi*C5at);
//DO
//Outside hospital delay to death
        double dOin= dt*rOi*da*Iat;
        double dO1 = dOin       - dt*( rOi*O1at);
        double dO2 = dt*( rOi*O1at   - rOi*O2at);
        double dO3 = dt*( rOi*O2at   - rOi*O3at);
        double dO4 = dt*( rOi*O3at   - rOi*O4at);
        double dO5 = dt*( rOi*O4at   - rOi*O5at); 
//H, DH        
        double dHin= dt*rIH*ha*Iat;
        double dH1 = dHin       - dt*( rHi*H1at);
        double dH2 = dt*( rHi*H1at   - rHi*H2at);
        double dH3 = dt*( rHi*H2at   - rHi*H3at);
        double dH4 = dt*( rHi*H3at   - rHi*H4at);
        double dH5 = dt*( rHi*H4at   - ((rHR*5)*(1-ma)+rHi*ma)*H5at); //Recovery time def could be different

        double dR  = dt*( rUR*Uat + rIR*(1-ha-da)*Iat + (rHR*5)*(1-ma)*H5at - rRS*Rat);
        double dDin= dt*( rHi*ma*H5at + rOi*O5at); //rID*da*Iat);
        double dDH = dt*( rHi*ma*H5at );//death incidence inside hospital
        double dDO = dt*( rOi*O5at    ); //rID*da*Iat ); //death incidence outside hospital
        double dD  = dDH + dDO;
        double dN  = dS + dE + dU + dI + (dH1 + dH2 + dH3 + dH4 + dH5) + (dO1 + dO2 + dO3 + dO4 + dO5) + dR + dD;

        S(ia,it+1)  = Sat  + dS;
        E(ia,it+1)  = Eat  + dE;
        I(ia,it+1)  = Iat  + dI;
        U(ia,it+1)  = Uat  + dU;
        C1(ia,it+1) = C1at + dC1;
        C2(ia,it+1) = C2at + dC2;
        C3(ia,it+1) = C3at + dC3;
        C4(ia,it+1) = C4at + dC4;
        C5(ia,it+1) = C5at + dC5;
        H1(ia,it+1) = H1at + dH1;
        H2(ia,it+1) = H2at + dH2;
        H3(ia,it+1) = H3at + dH3;
        H4(ia,it+1) = H4at + dH4;
        H5(ia,it+1) = H5at + dH5;
        O1(ia,it+1) = O1at + dO1;
        O2(ia,it+1) = O2at + dO2;
        O3(ia,it+1) = O3at + dO3;
        O4(ia,it+1) = O4at + dO4;
        O5(ia,it+1) = O5at + dO5;
        R(ia,it+1)  = Rat  + dR;
        D(ia,it+1)  = Dat  + dD; //DH(ia,it+1) = DHat + dDH; //DO(ia,it+1) = DOat + dDO; //not yet
        N(ia,it+1)  = Nat  + dN;
        // time update (units of parameters)
        time[it+1] = (it+1)*dt;
        // population states evaluated (t>0)
        St[it+1]  += Sat + dS;
        Et[it+1]  += Eat + dE;
        It[it+1]  += Iat + dI;
        Ut[it+1]  += Uat + dU;
        Ct[it+1]  += C1at + C2at + C3at + C4at + C5at + dC1 + dC2 + dC3 + dC4 + dC5;
        Ht[it+1]  += H1at + H2at + H3at + H4at + H5at + dH1 + dH2 + dH3 + dH4 + dH5;
        Ot[it+1]  += O1at + O2at + O3at + O4at + O5at + dO1 + dO2 + dO3 + dO4 + dO5;
        Rt[it+1]  += Rat + dR;
        Dt[it+1]  += Dat + dD;
   	    Nt[it+1]  += Nat + dN;
   	    Hpw       += dHin;
   	    Dpw       += dDin;
   	    DHpw      += dDH;
   	    DOpw      += dDO;
   	    Hapw[ia]  += dHin; // = dHin;
   	    Dapw[ia]  += dDin; // = dDin;
   	    DHapw[ia] += dDH; 
   	    DOapw[ia] += dDO; 
  }; //ia
        if (week - week0 == 1) {
          Hw[week-1] = Hpw;
          Hpw=0;
          Dw[week-1] = Dpw;
          Dpw=0;
          DHw[week-1]= DHpw;
          DHpw=0;
          DOw[week-1]= DOpw;
          DOpw=0;
          Ha1w[week-1]  = Hapw[0];  Hapw[0]=0; 
          Ha2w[week-1]  = Hapw[1];  Hapw[1]=0;
          Ha3w[week-1]  = Hapw[2];  Hapw[2]=0;
          Ha4w[week-1]  = Hapw[3];  Hapw[3]=0;
          Ha5w[week-1]  = Hapw[4];  Hapw[4]=0;
          Ha6w[week-1]  = Hapw[5];  Hapw[5]=0;
          Ha7w[week-1]  = Hapw[6];  Hapw[6]=0;
          Ha8w[week-1]  = Hapw[7];  Hapw[7]=0;
          Ha9w[week-1]  = Hapw[8];  Hapw[8]=0;
          Da1w[week-1]  = Dapw[0];  Dapw[0]=0;
          Da2w[week-1]  = Dapw[1];  Dapw[1]=0;
          Da3w[week-1]  = Dapw[2];  Dapw[2]=0;
          Da4w[week-1]  = Dapw[3];  Dapw[3]=0;
          Da5w[week-1]  = Dapw[4];  Dapw[4]=0;
          Da6w[week-1]  = Dapw[5];  Dapw[5]=0;
          Da7w[week-1]  = Dapw[6];  Dapw[6]=0;
          Da8w[week-1]  = Dapw[7];  Dapw[7]=0;
          Da9w[week-1]  = Dapw[8];  Dapw[8]=0;
          DHa1w[week-1] = DHapw[0]; DHapw[0]=0;
          DHa2w[week-1] = DHapw[1]; DHapw[1]=0;
          DHa3w[week-1] = DHapw[2]; DHapw[2]=0;
          DHa4w[week-1] = DHapw[3]; DHapw[3]=0;
          DHa5w[week-1] = DHapw[4]; DHapw[4]=0;
          DHa6w[week-1] = DHapw[5]; DHapw[5]=0;
          DHa7w[week-1] = DHapw[6]; DHapw[6]=0;
          DHa8w[week-1] = DHapw[7]; DHapw[7]=0;
          DHa9w[week-1] = DHapw[8]; DHapw[8]=0;
          DOa1w[week-1] = DOapw[0]; DOapw[0]=0;
          DOa2w[week-1] = DOapw[1]; DOapw[1]=0;
          DOa3w[week-1] = DOapw[2]; DOapw[2]=0;
          DOa4w[week-1] = DOapw[3]; DOapw[3]=0;
          DOa5w[week-1] = DOapw[4]; DOapw[4]=0;
          DOa6w[week-1] = DOapw[5]; DOapw[5]=0;
          DOa7w[week-1] = DOapw[6]; DOapw[6]=0;
          DOa8w[week-1] = DOapw[7]; DOapw[7]=0;
          DOa9w[week-1] = DOapw[8]; DOapw[8]=0;
      }
	}; //it
	
    // TODO: output by AGE
    Rcpp::DataFrame byw = Rcpp::DataFrame::create(
        Named("time") = time[iw],
        Named("St")   = St[iw],
        Named("Et")   = Et[iw],
        Named("It")   = It[iw],
        Named("Ut")   = Ut[iw],
        Named("Ct")   = Ct[iw],
        Named("Ht")   = Ht[iw],
        Named("Ot")   = Ot[iw],
        Named("Rt")   = Rt[iw],
        Named("Dt")   = Dt[iw],
        Named("Nt")   = Nt[iw],
        Named("Hw")   = Hw,
        Named("Dw")   = Dw,
        Named("DHw")  = DHw,
        Named("DOw")  = DOw,
        Named("cmdtmean") = cmdtmean[iw]);
    
    Rcpp::DataFrame byw_age = Rcpp::DataFrame::create(
      Named("H1w") = Ha1w,
      Named("H2w") = Ha2w,
      Named("H3w") = Ha3w,
      Named("H4w") = Ha4w,
      Named("H5w") = Ha5w,
      Named("H6w") = Ha6w,
      Named("H7w") = Ha7w,
      Named("H8w") = Ha8w,
      Named("H9w") = Ha9w,
      Named("D1w") = Da1w,
      Named("D2w") = Da2w,
      Named("D3w") = Da3w,
      Named("D4w") = Da4w,
      Named("D5w") = Da5w,
      Named("D6w") = Da6w,
      Named("D7w") = Da7w,
      Named("D8w") = Da8w,
      Named("D9w") = Da9w);

    Rcpp::DataFrame byw_aHO = Rcpp::DataFrame::create(
      Named("DH1w") = DHa1w,
      Named("DH2w") = DHa2w,
      Named("DH3w") = DHa3w,
      Named("DH4w") = DHa4w,
      Named("DH5w") = DHa5w,
      Named("DH6w") = DHa6w,
      Named("DH7w") = DHa7w,
      Named("DH8w") = DHa8w,
      Named("DH9w") = DHa9w,
      Named("DO1w") = DOa1w,
      Named("DO2w") = DOa2w,
      Named("DO3w") = DOa3w,
      Named("DO4w") = DOa4w,
      Named("DO5w") = DOa5w,
      Named("DO6w") = DOa6w,
      Named("DO7w") = DOa7w,
      Named("DO8w") = DOa8w,
      Named("DO9w") = DOa9w);
    
    return Rcpp::List::create(Rcpp::Named("byw") = byw,
                              Rcpp::Named("byw_age") = byw_age,
                              Rcpp::Named("byw_aHO") = byw_aHO);
}
