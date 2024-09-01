
(* Last modified: July 15, 2008 at 21:50 at home in Boulder *)

(* l_gcabeq.m *)

(* (alpha-beta)-equation (4.12) in Project Thesis of Dinh Tran, *)
(* but with linear terms added but no longer 1 (or removed). *)
(* Equation (4.12) on page 34 *)

(* set term = 0 to remove the linear terms and *)
(* set action = moebiustransformation to apply moebius transformation *)
(* gives (4.14) *)

(* set term = 1 to add the linear terms and *)
(* set action = translation perform the translation *)
(* gives (4.13) *)

(* lattice[origin_,right_,up_,across_,parameterright_,parameterup_] *)

name = cabeq;

(* action = moebiustransformation; *)
(* action = translation; *)

(* was term = 1 before *)

term = r;

lattice[x_,x1_,x2_,x12_,p_,q_]:= 
((p-alpha)*x-(p+beta)*x1+1*term)*((p-beta)*x2-(p+alpha)*x12+1*term)- 
((q-alpha)*x-(q+beta)*x2+1*term)*((q-beta)*x1-(q+alpha)*x12+1*term) == 0;

(* data for abeq itself: *)
(* choice 1 -- do not forget to set byhand to True and detmethod to False *)
(* 
tchoice = 1/((alpha-p)*x+(beta+p)*x1); 
schoice = 1/((alpha-q)*x+(beta+q)*x2); 
*)

(* data for abeq itself: *)
(* choice 2 -- do not forget to set byhand to True and detmethod to False *)
(* 
tchoice = 1/((beta-p)*x+(alpha+p)*x1); 
schoice = 1/((beta-q)*x+(alpha+q)*x2); 
*)

(* L and M must be given in a form that is suitable for the *)
(* compatibility equation:  L2.M - M1.L = 0 on lattice *)
(* and computation of matrix N so that L2.M - M1.L = (lattice)*N *)

(* data for abeq itself: *)
(* choice 1 -- do not forget to set LaxPairGiven to True *)
(* 
candidateL = 1/((alpha-p)*x+(beta+p)*x1)*
 { 
 { (p-alpha)*(p-beta)*x+(k^2-p^2)*x1, -(k-alpha)*(k-beta)*x*x1 },
 { (k+alpha)*(k+beta),                -((p+alpha)*(p+beta)*x1+(k^2-p^2)*x) }
 };

candidateM = 1/((alpha-q)*x+(beta+q)*x2)*
 { 
 { (q-alpha)*(q-beta)*x+(k^2-q^2)*x2, -(k-alpha)*(k-beta)*x*x2 },
 { (k+alpha)*(k+beta),                -((q+alpha)*(q+beta)*x2+(k^2-q^2)*x) }
 };
*)

(* data for abeq itself: *)
(* choice 2 -- do not forget to set LaxPairGiven to True *)
(* 
candidateL = 1/((beta-p)*x+(alpha+p)*x1)*
 { 
 { (p-alpha)*(p-beta)*x+(k^2-p^2)*x1, -(k-alpha)*(k-beta)*x*x1 },
 { (k+alpha)*(k+beta),                -((p+alpha)*(p+beta)*x1+(k^2-p^2)*x) }
 };

candidateM = 1/((beta-q)*x+(alpha+q)*x2)*
 { 
 { (q-alpha)*(q-beta)*x+(k^2-q^2)*x2, -(k-alpha)*(k-beta)*x*x2 },
 { (k+alpha)*(k+beta),                -((q+alpha)*(q+beta)*x2+(k^2-q^2)*x) }
 };
*)

(* data for abeq itself: *)
(* choice 3 -- do not forget to set LaxPairGiven to True *)
(* 
candidateL = 1/Sqrt[((beta-p)*x+(alpha+p)*x1)*((alpha-p)*x+(beta+p)*x1)]*
 { 
 { (p-alpha)*(p-beta)*x+(k^2-p^2)*x1, -(k-alpha)*(k-beta)*x*x1 },
 { (k+alpha)*(k+beta),                -((p+alpha)*(p+beta)*x1+(k^2-p^2)*x) }
 };

candidateM = 1/Sqrt[((beta-q)*x+(alpha+q)*x2)*((alpha-q)*x+(beta+q)*x2)]*
 { 
 { (q-alpha)*(q-beta)*x+(k^2-q^2)*x2, -(k-alpha)*(k-beta)*x*x2 },
 { (k+alpha)*(k+beta),                -((q+alpha)*(q+beta)*x2+(k^2-q^2)*x) }
 };
*)

(* **************************************************************** *)
(* CONTROL FLAGS *)

(* Computation of t and s with determinant method or by hand, in which *) 
(* case the t and s must be given in this data file *)
(* Set either byhand or detmethod to True, the other one must be set False *)
(* Computation will only be carried out if LaxPairGiven is False ! *)

byhand      = False;
detmethod   = True;

LaxPairGiven = False;
(* LaxPairGiven = True; *)

(* applying or skipping the consistency test around the cube *)

testconsistencyoncube = True; 
(* testconsistencyoncube = False; *)

(* ****************************************************************** *)

(* use of powerexpand and simplification rules for roots *)
(* They should have opposite Boolean values *)

controlpowerexpand    = False;
(* controlpowerexpand = True; *)

controlrootrules    = True;
(* controlrootrules = False; *)

(* control over the test of compatibility conditions *)

controlcompatibilitytesting = True; 
(* controlcompatibitytesting = False; *)

(* control over the computation of matrix N *)

controlcomputationmatrixN = True;
(* controlcomputationmatrixN = False; *)

(* computation of gauge transformation *)

controlgaugetf = False; 
(* controlgaugetf = True; *)

(* l_gcabeq.m *)

(* ################################################################ *)

