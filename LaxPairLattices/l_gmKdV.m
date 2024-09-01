
(* Last modified: July 18, 2008 at 21:50 at home in Boulder *)

(* l_gmKdV.m *)

(* general modified KdV lattice *)
(* Introducing functional coefficients of p and q instead of original *)
(* p and -q itself *)

(* Equation (A.2) in paper by Quispel, Capel, Papageorgiou, and Nijhoff *)
(* Physica A, vol. 173, pp. 243-266 (1991) *)
(* Special case of equation (2.13) in Project Thesis of Dinh Tran *)

(* Form of mKdV equation but with arbitrary functions of p and q *)
(* See Project Thesis of Dinh Tran: equation (4.1), *)
(* Note that Tran takes constants not functions of p and q *)

(* lattice[origin_,right_,up_,across_,parameterright_,parameterup_] *)

name = gmKdV;

(* for original mKdV equation *)
(* fun1[p_,q_] :=  p *)
(* fun2[p_,q_] := -q *)
(* fun3[p_,q_] := -q  *)   

(* lattice[x_,x1_,x2_,x12_,p_,q_] := p*(x*x2-x1*x12)-q*x*x1+q*x2*x12 == 0; *)

(* for first run start with arbitrary function fun1[p,q], fun2[p,q] and *)
(* fun3[p,q], but do not assume the separated functional forms yet *)

(* you will probably get conditions like in gkdv case *)
(* fun1[pp_,qq_] := f[pp]-f[qq]; *)
(* fun2[pp_,qq_] := f[pp]+f[qq]; *)
(* fun3[pp_,qq_] := g[pp]-g[qq]; *)

(* 
lattice[x_,x1_,x2_,x12_,p_,q_] := 
   fun1[p,q]*(x*x2-x1*x12)+fun2[p,q]*x*x1-fun3[p,q]*x2*x12 == 0;  
*)

lattice[x_,x1_,x2_,x12_,p_,q_] := 
   beta1*(x*x2-x1*x12)+beta2*x*x1-beta3*x2*x12 == 0;  

name = gmKdV;

(* irrelevant here because Lax pair must be given !!! *)
(* choice 1 -- do not forget to set byhand to True *)
(* tchoice = 1; *)
(* schoice = 1; *)

(* L and M must be given for testing the compatibility equation in *)
(* the form L2.M - M1.L = (lattice)*N *)
(* choice 1 -- do not forget to set LaxPairGiven to True *)
(* and testconsistencyoncube to False, if you want to skip it *)
(* *)
candidateL = 
  {
  { 1,           -x1 },
  { -(k^2)/x, (x1)/x }
  };

candidateM = 1/(beta2*beta3-beta1^2*k^2)*
  {
  { beta2,          beta1*x2 },
  { (beta1*k^2)/x,  (beta3*x2)/x }
  };
(* *)

(* **************************************************************** *)
(* CONTROL FLAGS *)

(* Computation of t and s with determinant method or by hand, the latter *) 
(* requires that t and s are given in the data file. *)
(* Set either byhand or detmethod to True, the other one must be set False *)
(* Computation will only be carried out if LaxPairGiven is False !!! *)

byhand      = False;
detmethod   = True;

(* True is Lax pair is given in the data file, False otherwise *)

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

(* control over test of compatibility conditions *)

controlcompatibilitytesting = True; 
(* controlcompatibitytesting = False; *)

(* control over computation of matrix N *)

controlcomputationmatrixN = True;
(* controlcomputationmatrixN = False; *)

(* computation of gauge transformation *)

controlgaugetf = False; 
(* controlgaugetf = True; *)

(* l_gmKdV.m *)

(* ################################################################ *)

