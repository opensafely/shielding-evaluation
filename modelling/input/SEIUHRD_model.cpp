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
    NumericMatrix R(na,nt);
    NumericMatrix D(na,nt);	
    NumericMatrix N(na,nt);
    
    // age-group initial states at time[0]
    NumericVector Sa0 = as<NumericVector>(pars["Sa0"]);
    NumericVector Ea0 = as<NumericVector>(pars["Ea0"]);
    NumericVector Ia0 = as<NumericVector>(pars["Ia0"]);
    NumericVector Ua0 = as<NumericVector>(pars["Ua0"]);
    NumericVector Ha0 = as<NumericVector>(pars["Ha0"]);
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
    NumericVector Rt(nt);
    NumericVector Dt(nt);
    NumericVector Nt(nt);
    NumericVector Hw(nw);
    NumericVector Dw(nw);
    NumericVector time(nt);
    NumericVector cmdtmean(nt);

    // read parameters
    const NumericVector u = as<NumericVector>(pars["u"]); 
    const NumericVector y = as<NumericVector>(pars["y"]); 
    const NumericVector h = as<NumericVector>(pars["h"]); 
    const NumericVector m = as<NumericVector>(pars["m"]); 
    
    const double beta_infectivity = pars["beta"];
    const double phm  = pars["phm"];
    const double fu   = pars["fu"];
    const double rEI  = pars["rEI"];
    const double rEU  = pars["rEU"];
    const double rIR  = pars["rIR"];
    const double rUR  = pars["rUR"];
    const double rIH  = pars["rIH"];
    const double rHR  = pars["rHR"];
    const double rHD  = pars["rHD"];
    const double rRS  = pars["rRS"];
    const double rC   = pars["rC"];
    const double rCi  = 5*rC;
    const double rHi  = 5*rHD;
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
        R(ia,0)  = Ra0[ia];   Rt[0] += R(ia,0);
        D(ia,0)  = Da0[ia];   Dt[0] += D(ia,0);   Dw[0] += 0;
        N(ia,0)  = Na0[ia];   Nt[0] += N(ia,0);
    }


    // Dynamics of state variables - Euler integration
    time[0] = 0;
    int  week  = 1;
    int  week0 = 1;
    double Hpw = 0;
    double Dpw = 0;
    for (int it = 0; it < (nt-1); it++) {	//Crucial: nt-1
        week0 = week;
        week  = 1 + (int) time[it]/7;
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
        double Rat = R(ia,it);
        double Dat = D(ia,it);
        double Nat = N(ia,it);
        // force of infection on group ia
        double lambda  = 0;
        double ua = u[ia];
        double ya = y[ia];
        double ha = h[ia];
        double ma = m[ia]*phm;

        for (int ib = 0; ib < na; ib++) {
            int    icm = (week-1)*cmdim1*cmdim2 + ib*cmdim1 + ia;
            double cmi = cm[icm];
            lambda        += beta_infectivity*ua*cmi*( I(ib,it) + fu*U(ib,it) )/N(ib,it);
            cmdtmean[it]  += cmi*ocmdim;
        } //ib

        // state update for next timestep
        double dS  = dt*(-lambda*Sat       + rRS*Rat);
        double dE  = dt*( lambda*Sat       - (rEU*(1-ya)+rEI*ya)*Eat);
        double dI  = dt*( rEI*ya*Eat       - (rIR*(1-ha)+rIH*ha)*Iat);
        double dU  = dt*( rEU*(1-ya)*Eat   - rUR*Uat);

        double dC1 = dt*( lambda*Sat - rCi*C1at);
        double dC2 = dt*( rCi*C1at   - rCi*C2at);
        double dC3 = dt*( rCi*C2at   - rCi*C3at);
        double dC4 = dt*( rCi*C3at   - rCi*C4at);
        double dC5 = dt*( rCi*C4at   - rCi*C5at);

        double dHin= dt*rIH*ha*Iat;
        double dH1 = dHin       - dt*( rHi*H1at);
        double dH2 = dt*( rHi*H1at   - rHi*H2at);
        double dH3 = dt*( rHi*H2at   - rHi*H3at);
        double dH4 = dt*( rHi*H3at   - rHi*H4at);
        double dH5 = dt*( rHi*H4at   - ((rHR*5)*(1-ma)+rHi*ma)*H5at); //Recovery time def could be different

        double dR  = dt*( rUR*Uat + rIR*(1-ha)*Iat + (rHR*5)*(1-ma)*H5at - rRS*Rat);
        double dDin= dt*( rHi*ma*H5at );
        double dD  = dDin;
        double dN  = dS + dE + dU + dI + dH1 + dH2 + dH3 + dH4 + dH5 + dR + dD;

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
        R(ia,it+1)  = Rat  + dR;
        D(ia,it+1)  = Dat  + dD;
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
        Rt[it+1]  += Rat + dR;
        Dt[it+1]  += Dat + dD;
   	    Nt[it+1]  += Nat + dN;
   	    Hpw       += dHin;
   	    Dpw       += dDin;
   	    
  }; //ia
        if (week - week0 == 1) {
          Hw[week-1] = Hpw;
          Hpw=0;
          Dw[week-1] = Dpw;
          Dpw=0;
        }
	}; //it
	
    // TODO: output by AGE
  DataFrame simt = DataFrame::create(
        Named("time") = time[iw],
        Named("St")   = St[iw],
        Named("Et")   = Et[iw],
        Named("It")   = It[iw],
        Named("Ut")   = Ut[iw],
        Named("Ct")   = Ct[iw],
        Named("Ht")   = Ht[iw],
        Named("Rt")   = Rt[iw],
        Named("Dt")   = Dt[iw],
        Named("Nt")   = Nt[iw],
        Named("Hw")   = Hw,
        Named("Dw")   = Dw,
        Named("cmdtmean") = cmdtmean[iw]);
	return simt;
};

