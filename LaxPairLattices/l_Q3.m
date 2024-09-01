
(* Last modified: July 18, 2008 at 21:50 at home in Boulder *)

(* l_Q3.m *)

(* (Q.3) in paper on ``Classification of integrable equations on *)
(* quadgraphs" by V.E. Adler, A.I. Bobenko, Yu. B. Suris *)
(* Comm. Math. Phys. vol. 233, pp. 513-543, 2003 *)

(* lattice[origin_,right_,up_,across_,parameterright_,parameterup_] *)

name = Q3;

(* If delta = 0 ---> Q3zero case, also in separate file *)
 
If[delta===0, 
 lattice[x_,x1_,x2_,x12_,p_,q_]:= 
 (q^2-p^2)*(x*x12+x1*x2)+q*(p^2-1)*(x*x1+x2*x12)-p*(q^2-1)*(x*x2+x1*x12) == 0
];

If[delta=!=0, 
 lattice[x_,x1_,x2_,x12_,p_,q_]:= 
 (4*p*q)*(
 (q^2-p^2)*(x*x12+x1*x2)+q*(p^2-1)*(x*x1+x2*x12)-p*(q^2-1)*(x*x2+x1*x12))-
 delta^2*(p^2-q^2)*(p^2-1)*(q^2-1) == 0
];

(* choice 1 -- do not forget to set byhand to True and detmethod to False *)
(* 
tchoice = 1/Sqrt[delta^2-2*delta^2*p^2+delta^2*p^4+4*p^2*x^2-4*p*x*x1-
          4*p^3*x*x1+4*p^2*x1^2]; 

schoice = 1/Sqrt[delta^2-2*delta^2*q^2+delta^2*q^4+4*q^2*x^2-4*q*x*x2-
          4*q^3*x*x2+4*q^2*x2^2]; 
*)

(* L and M must be given in a format that allows one to test the *)
(* compatibility condition in the form:  L2.M - M1.L = 0 on lattice *)
(* and computation of matrix N so that L2.M - M1.L = (lattice)*N *)

(* choice 1 -- do not forget to set LaxPairGiven to True *)
(* and testconsistencyoncube to False, if you want to skip it *)
(* 
candidateL = 1/(Sqrt[delta^2-2*delta^2*p^2+delta^2*p^4+4*p^2*x^2-4*p*x*x1-
          4*p^3*x*x1+4*p^2*x1^2])*
{
{ -4*k*p*(p*(k^2-1)*x+(p^2-k^2)*x1),  
-(p^2-1)*(delta^2*k^2-delta^2*k^4-delta^2*p^2+delta^2*k^2*p^2-4*k^2*p*x*x1) },
{ -4*k^2*p*(p^2-1),                         4*k*p*(p*(k^2-1)*x1+(p^2-k^2)*x) }
};

candidateM = 1/(Sqrt[delta^2-2*delta^2*q^2+delta^2*q^4+4*q^2*x^2-4*q*x*x2-
          4*q^3*x*x2+4*q^2*x2^2])*
{
{ -4*k*q*(q*(k^2-1)*x+(q^2-k^2)*x2),  
-(q^2-1)*(delta^2*k^2-delta^2*k^4-delta^2*q^2+delta^2*k^2*q^2-4*k^2*q*x*x2) },
{ -4*k^2*q*(q^2-1),                         4*k*q*(q*(k^2-1)*x2+(q^2-k^2)*x) }
};
*)

(* **************************************************************** *)
(* CONTROL FLAGS *)

(* Computation of t and s with determinant method or by hand, in the latter *) 
(* case the t and s must be given in this data file. *)
(* Set either byhand or detmethod to True, the other one must be set False *)
(* Computation will only be carried out if LaxPairGiven is set to False !!! *)

byhand      = False;
detmethod   = True;

(* Lax pair given in data file or computed with the algorithm *)

(* LaxPairGiven = True; *)
LaxPairGiven = False; 

(* applying or skipping the consistency test around the cube *)

testconsistencyoncube = True; 
(* testconsistencyoncube = False; *)

(* ****************************************************************** *)

(* use of powerexpand and simplification rules for roots *)
(* they should have opposite Boolean values *)

controlpowerexpand    = True;
(* controlpowerexpand = False; *)

controlrootrules    = True;
(* controlrootrules = False; *)

(* control over testing compatibility conditions *)

controlcompatibilitytesting = True; 
(* controlcompatibitytesting = False; *)

(* Does not work for complicated lattices like Q3 with delta not zero *)
(* Piece of code still under development *)
(* Control of computation of matrix N *)

controlcomputationmatrixN = False;
(* controlcomputationmatrixN = False; *)

(* control over computation of gauge transformation *)

controlgaugetf = False; 
(* controlgaugetf = True; *)

(* l_Q3.m *)

(* ################################################################ *)

