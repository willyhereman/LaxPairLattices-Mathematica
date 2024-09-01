
(* Last modified: July 18, 2008 at 21:50 at home in Boulder *)

(* l_sGtran.m *)

(* sine-Gordon lattice *)
(* Equation (2) in paper by Quispel, Capel, Papageorgiou, and Nijhoff *)
(* Physica A, vol. 173, pp. 243-266 (1991) *)

(* lattice[origin_,right_,up_,across_,parameterright_,parameterup_] *)

name = sGtran;

(* if term = 1 then sg as in equation (2) in paper Quispel et al. 1991 *)
(* if term = 0 then sg without constant term *)

(* term = 0; *)
(* sine-Gordon lattice in paper Quispel et al in Physica A, 1991 *)
(* lattice[x_,x1_,x2_,x12_,p_,q_] := 
        x*x1*x2*x12-p*q*(x*x12-x1*x2)-1*term == 0;  *)

(* More general sine-Gordon lattice in Project Thesis Tran: *)
(* equation (2.10) on page 13 *)

fun1[p_,q_] := p*q; 
fun2[p_,q_] := 1;     
fun3[p_,q_] := 1;    

lattice[x_,x1_,x2_,x12_,p_,q_] := 
   fun2[p,q]*x*x1*x2*x12-fun1[p,q]*(x*x12-x1*x2)-fun3[p,q] == 0;

(*
AAA = 1;
BBB = k^2;
CCC = 1/k^2;
DDD = 1;
*)

(* choice 1 -- do not forget to set LaxPairGiven to True *)
(* *)
candidateL = 
  {
  {p,            -AAA*x1  },
  {-BBB*(1/x),   p*(x1/x) }
  };

candidateM = 
  {
  {q*(x2/x),    -CCC*(1/x) },
  {-DDD*x2,              q }
  };
(* *)

(* **************************************************************** *)

(* Control of either byhand (giving tcondition and scondition) with *)
(* byhand set to True or computing it with detmethod with detmethod by *)
(* setting detmethod to True *)

byhand      = False;
detmethod   = True;

(* Lax pair given in this data file or not *)

LaxPairGiven = True;
(* LaxPairGiven = False; *)

(* testing the consistency around the cube, or skipping the test *)

testconsistencyoncube = True; 
(* testconsistencyoncube = False; *)

(* ****************************************************************** *)

(* Control of powerexpand with controlpowerexpand *)
(* Control my simplification rules for roots with controlrootrules *)

controlpowerexpand    = False;
(* controlpowerexpand = False; *)

controlrootrules    = True;
(* controlrootrules = False; *)

(* Control for computation of matrix N with flag controlcomputationmatrixN *)
(* set to True or False *)

(* controlcomputationmatrixN = True; *)
controlcomputationmatrixN = False; 

(* Control if testing the compatibility conditions *)
(* set to True or False *)

controlcompatibilitytesting = True; 
(* controlcompatibitytesting = False; *)

(* Control of applying the gauge transformation set to True or False *)

controlgaugetf = False; 
(* controlgaugetf = True; *)

(* l_sGtran.m *)

(* ################################################################ *)

