//##############################################################################################
//# SEIR age-structured, stratified-contact compartment model, susceptibility u

#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List SEIR(List pars){ 

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
    NumericMatrix R(na,nt);
    NumericMatrix N(na,nt);
    
    // age-group initial states at time[0]
    NumericVector Sa0 = as<NumericVector>(pars["Sa0"]);
    NumericVector Ea0 = as<NumericVector>(pars["Ea0"]);
    NumericVector Ia0 = as<NumericVector>(pars["Ia0"]);
    NumericVector Ra0 = as<NumericVector>(pars["Ra0"]);
    NumericVector Na0 = as<NumericVector>(pars["Na0"]);

    // population states = 0, need initialise St[0] etc, time[0]
    NumericVector St(nt);
    NumericVector Et(nt);
    NumericVector It(nt);
    NumericVector Rt(nt);
    NumericVector Nt(nt);
    NumericVector Iw(nw);
    NumericVector Rw(nw);
    NumericVector time(nt);
    NumericVector cmdtmean(nt);

    // age group and population states initialised
    for (int ia = 0; ia < na; ia++) {
        S(ia,0) = Sa0[ia];   St[0] += S(ia,0);
        E(ia,0) = Ea0[ia];   Et[0] += E(ia,0);
        I(ia,0) = Ia0[ia];   It[0] += I(ia,0);
        R(ia,0) = Ra0[ia];   Rt[0] += R(ia,0);
        N(ia,0) = Na0[ia];   Nt[0] += N(ia,0);
    }

    // read parameters
    const NumericVector u = as<NumericVector>(pars["u"]); 
    const double d    = pars["d"];
    const double b    = pars["b"];
    const double beta_infectivity = pars["beta"];
    const double rEI  = pars["rEI"];
    const double rIR  = pars["rIR"];
    const double rRS  = pars["rRS"];
    const double dt   = pars["dt"];


    // Dynamics of state variables - Euler integration
    time[0] = 0;
    int  week  = 1;
    int  week0 = 1;
    double Ipw = 0;
    double Rpw = 0;
    for (int it = 0; it < (nt-1); it++) {	//Crucial: nt-1
        week0 = week;
        week  = 1 + (int) time[it]/7;
        cmdtmean[it] = 0;

        for (int ia = 0; ia < na; ia++) {
		    // current matrix cells
        double Sat = S(ia,it);
        double Eat = E(ia,it);
        double Iat = I(ia,it);
        double Rat = R(ia,it);
        double Nat = N(ia,it);
        // force of infection on group ia
        double lambda  = 0;
        double ua = u[ia];
        for (int ib = 0; ib < na; ib++) {
            int    icm = (week-1)*cmdim1*cmdim2 + ib*cmdim1 + ia;
            double cmi = cm[icm];
            lambda        += beta_infectivity*ua*cmi*I(ib,it)/N(ib,it);
            cmdtmean[it]  += cmi*ocmdim;
        } //ib

        // state update for next timestep
        double dS = dt*(-lambda*Sat + rRS*Rat  - d*Sat + b*Nat);
        double dE = dt*( lambda*Sat - rEI*Eat  - d*Eat );
        double dIin = dt*rEI*Eat;
        double dI = dIin - dt*( rIR*Iat + d*Iat );
        double dRin = dt*rIR*Iat;
        double dR = dRin - dt*( rRS*Rat + d*Rat );
        S(ia,it+1) = Sat + dS;
        E(ia,it+1) = Eat + dE;
        I(ia,it+1) = Iat + dI;
        R(ia,it+1) = Rat + dR;
        N(ia,it+1) = Nat + dS + dE + dI + dR;
        // time update (units of parameters)
        time[it+1] = (it+1)*dt;
        // population states evaluated (t>0)
        St[it+1]  += Sat + dS;
        Et[it+1]  += Eat + dE;
        It[it+1]  += Iat + dI;
   	    Rt[it+1]  += Rat + dR;
   	    Nt[it+1]  += Nat + dS + dE + dI + dR; // = dt*(b-d)*Nat
   	    Ipw       += dIin;
   	    Rpw       += dRin;
   	    
	}; //ia
        if (week - week0 == 1) {
          Iw[week-1] = Ipw;
          Ipw=0;
          Rw[week-1] = Rpw;
          Rpw=0;
        }
	}; //it
	
    // TODO: output by AGE
	DataFrame simt = DataFrame::create(
        Named("time") = time[iw],
        Named("St")   = St[iw],
        Named("Et")   = Et[iw],
        Named("It")   = It[iw],
        Named("Rt")   = Rt[iw],
        Named("Nt")   = Nt[iw],
        Named("Iw")   = Iw,
        Named("Rw")   = Rw,
        Named("cmdtmean") = cmdtmean[iw] );
	return simt;
};

