
(* Last modified: July 18, 2008 at 21:50 at home in Boulder *)

(* l_sG.m *)

(* sine-Gordon lattice *)
(* Equation (2) in paper by Quispel, Capel, Papageorgiou, and Nijhoff *)
(* Physica A, vol. 173, pp. 243-266 (1991) *)

(* lattice[origin_,right_,up_,across_,parameterright_,parameterup_] *)

name = sG;

(* if term = 1 then sG as in equation (2) in paper Quispel et al. 1991 *)
(* if term = 0 then sG without constant term *)

(* term = 0; *)
(* sine-Gordon lattice in paper Quispel et al in Physica A, 1991 *)
(* 
lattice[x_,x1_,x2_,x12_,p_,q_] := 
        x*x1*x2*x12-p*q*(x*x12-x1*x2)-1*term == 0;  
*)

(* More general sine-Gordon lattice in Project Thesis Tran: *)
(* equation (2.10) on page 13 *)

fun1[p_,q_] := 1;     
fun2[p_,q_] := p*q; 
fun3[p_,q_] := 1;    

lattice[x_,x1_,x2_,x12_,p_,q_] := 
   fun1[p,q]*(x*x1*x2*x12)-fun2[p,q]*(x*x12-x1*x2)-fun3[p,q] == 0;

(* choice 1 -- do not forget to set byhand to True and detmethod to False *)
(* choice in project thesis Tran *)
(* 
tchoice = 1; 
schoice = 1/(k^2*p^2*q^2-1); 
*)

(* choice 2 -- do not forget to set byhand to True and detmethod to False *)
(* choice in paper Quispel et al Physica A 1991 *)
(*
tchoice = 1/(p - k); 
schoice = 1/(q - (1/k)); 
*)

(* L and M must be given in the format for testing the compatibility *)
(* condition L2.M - M1.L = 0 on lattice and computation of matrix N *)
(* so that L2.M - M1.L = (lattice)*N *)

(* choice 1 -- do not forget to set LaxPairGiven to True *)
(* and testconsistencyoncube to False for skipping the consistency test *)
(* Choice in project thesis Tran: *)
(* WORKS FINE! *)

(* 
candidateL = 
  {
  {1,           -x1 },
  {-(k^2)/x,   (x1)/x }
  };

candidateM = (1/( (k*p*q)^2 - 1) )*
  {
  { -k^2*p*q*(x2/x),       1/x},
  { k^2*x2,           -k^2*p*q}
  };
*)

(* choice 2 -- do not forget to set LaxPairGiven to True *)
(* and testconsistencyoncube to False, if you want to skip the test *)

(* Choice given in paper Quispel et al Physica A 1991 *)
(* WORKS FINE *)
(* 
candidateL = (1/(p-k))*
  {
  {p,                -x1  },
  {-(k^2)/(x),   (p*x1)/x }
  };

candidateM = (1/( q-(1/k) ))*
  {
  { (q*x2)/x,       -1/((k^2)*x)},
  { -x2,                       q}
  };
*)

(* choice 3 -- do not forget to set LaxPairGiven to True *)
(* based on result in Tran's project thesis, but without the factor *)
(* 1/( (k*p*q)^2 - 1) in front of M *)
(* WORKS FINE *)

(* 
candidateL = 
  {
  { 1,        -x1 },
  { -(k^2)/x, x1/x }
  };

candidateM =
  {
  { -(k^2*p*q)*(x2/x),     1/x },
  { k^2*x2,           -k^2*p*q }
  };
*)

(* choice 4 -- do not forget to set LaxPairGiven to True *)
(* based on result in Quispel et al Physica A 1991 *)
(* but without the factor 1/(q - 1/k) in front of M *)
(* WORKS FINE *)

(* 
candidateL =
  {
  { p,               -x1 },
  { -(k^2)/x,   p*(x1/x) }
  };

candidateM = 
  {
  { q*(x2/x),   -(1/k^2)*(1/x) },
  { -x2,                    q  }
  };
*)

(* choice 5 -- do not forget to set LaxPairGiven to True *)
(* based on result in Quispel et al Physica A 1991 *)
(* without the factor 1/(q - 1/k) in front of M and *)
(* where L and M are independent of the spectral parameter k *)
(* WORKS FINE *)

(*
candidateL =
  {
  { p,               -x1 },
  { -1/x,   p*(x1/x) }
  };

candidateM = 
  {
  { q*(x2/x),   -(1/x) },
  { -x2,                    q  }
  };
*)

(* choice 6 -- do not forget to set LaxPairGiven to True *)
(* based on result in Quispel et al Physica A 1991 *)
(* but without the factor 1/(q - 1/k) in front of M and where *)
(* factors k and 1/k in L and M are put in ``symmetric" places *)
(* WORKS FINE *)

(* *)
candidateL =
  {
  { p,         -k*x1 },
  { -k*(1/x),   p*(x1/x) }
  };

candidateM = 
  {
  { q*(x2/x),    -(1/k)*(1/x) },
  { -(1/k)*x2,           q  }
  };
(* *)

(* **************************************************************** *)

(* controls for computing t and s or given t and s in data file *)
(* Set either byhand or detmethod to True, the other one must be set False *)

byhand = True;
detmethod = False;

(* controls for the computation of the Lax pair: *)
(* either the Lax pair is given in the data file or computed with the *)
(* algorithm, the latter only works for lattices consistent around the cube *)

LaxPairGiven = True; 
(* LaxPairGiven = False; *)

(* control for testing the consistency around the cube *)

testconsistencyoncube = True; 
(* testconsistencyoncube = False; *)

(* ****************************************************************** *)

(* Control of powerexpand with controlpowerexpand *)
(* Control my simplification rules for roots with controlrootrules *)

controlpowerexpand    = False;
(* controlpowerexpand = True; *)

controlrootrules    = True;
(* controlrootrules = False; *)

(* Control for testing the compatibility conditions *)
(* set to True or False *)

controlcompatibilitytesting = True; 
(* controlcompatibitytesting = False; *)

(* Control for computation of matrix N with flag controlcomputationmatrixN *)
(* set to True or False *)

(* controlcomputationmatrixN = False; *)
controlcomputationmatrixN = True; 

(* Control of applying the gauge transformation set to True or False *)

controlgaugetf = False; 
(* controlgaugetf = True; *)

(* l_sg.m *)

(* ################################################################ *)

