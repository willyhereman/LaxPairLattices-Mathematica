
(* Last modified: July 18, 2008 at 21:50 at home in Boulder *)

(* l_gsG.m *)

(* General sine-Gordon lattice equation (2.10) in Thesis Tran *)
(* General case of equation (2) in paper by Quispel, Capel, Papageorgiou, *)
(* and Nijhoff, Physica A, vol. 173, pp. 243-266 (1991) *)

(* lattice[origin_,right_,up_,across_,parameterright_,parameterup_] *)

name = gsG;

(* if term = 1 then sg as in equation (2) in paper Quispel et al. 1991 *)
(* if term = 0 then sg without constant term *)

(* term = 0; *)
(* sine-Gordon lattice in paper Quispel et al in Physica A, 1991 *)
(* lattice[x_,x1_,x2_,x12_,p_,q_] := 
        x*x1*x2*x12-p*q*(x*x12-x1*x2)-1*term == 0;  *)

(* More general sine-Gordon lattice in Project Thesis Tran: *)
(* equation (2.10) on page 13 *)

(* 
fun1[p_,q_] := p*q; 
fun2[p_,q_] := 1;     
fun3[p_,q_] := 1; 
*)

(* 
fun1[p_,q_] := -alpha1; 
fun2[p_,q_] := alpha2;     
fun3[p_,q_] := alpha3; 
*) 

lattice[x_,x1_,x2_,x12_,p_,q_] := 
   alpha2*(x*x1*x2*x12) + alpha1*(x*x12-x1*x2) - alpha3 == 0;

(* L and M operators are converted so that they work for the *)
(* consistency test in format L2.M - M1.L = (lattice)*N *)

(* choice 1 -- do not forget to set LaxPairGiven to True *)
(* and testconsistencyoncube to False, if you want to skip it *)
(* *)
candidateL = 
  {
  { 1,        -x1 },
  { -(k^2)/x, (x1)/x }
  };
(* *)

(* *)
candidateM = 1/(alpha1^2*k^2-alpha2*alpha3)*
  {
  { (alpha1*k^2*x2)/x, (alpha3)/x },
  { (alpha2*k^2)*x2,   alpha1*k^2 }
  };
(* *)

(* **************************************************************** *)
(* CONTROL FLAGS *)

(* Computation of t and s with determinant method or by hand, that is *) 
(* t and s must be giving in this data file. *)
(* Set either byhand or detmethod to True, the other one must be set False *)
(* Computation will only be carried out if LaxPairGiven is False ! *)

(* irrelevant for this case because Lax pair must be given *)
byhand      = False;
detmethod   = True;

LaxPairGiven = True;
(* LaxPairGiven = False; *)

(* applying or skipping the consistency test around the cube *)

testconsistencyoncube = True; 
(* testconsistencyoncube = False; *)

(* ****************************************************************** *)

(* use of powerexpand and simplification rules for roots *)

controlpowerexpand    = False;
(* controlpowerexpand = True; *)

controlrootrules    = True;
(* controlrootrules = False; *)

(* test of compatibility conditions *)

controlcompatibilitytesting = True; 
(* controlcompatibitytesting = False; *)

(* control over the computation of matrix N *)

controlcomputationmatrixN = True;
(* controlcomputationmatrixN = False; *)

(* computation of gauge transformation *)

controlgaugetf = False; 
(* controlgaugetf = True; *)

(* l_gsG.m *)

(* ################################################################ *)

