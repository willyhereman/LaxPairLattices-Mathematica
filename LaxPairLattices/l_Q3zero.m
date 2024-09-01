
(* Last modified: July 18, 2008 at 21:50 at home in Boulder *)

(* l_Q3zero.m *)

(* (Q.3) in paper on ``Classification of integrable equations on *)
(* quadgraphs" by V.E. Adler, A.I. Bobenko, Yu. B. Suris *)
(* Comm. Math. Phys. vol. 233, pp. 513-543, 2003 *)

(* lattice[origin_,right_,up_,across_,parameterright_,parameterup_] *)

name = Q3zero;

(* if delta = 0 then the equation without fractional term *)

delta = 0;

If[delta=!=0, 
 lattice[x_,x1_,x2_,x12_,p_,q_]:= 
 (4*p*q)*(
 (q^2-p^2)*(x*x12+x1*x2)+q*(p^2-1)*(x*x1+x2*x12)-p*(q^2-1)*(x*x2+x1*x12))-
 delta^2*(p^2-q^2)*(p^2-1)*(q^2-1) == 0
];

If[delta===0, 
 lattice[x_,x1_,x2_,x12_,p_,q_]:= 
 (q^2-p^2)*(x*x12+x1*x2)+q*(p^2-1)*(x*x1+x2*x12)-p*(q^2-1)*(x*x2+x1*x12) == 0
];

(* choice 1 -- do not forget to set byhand to True and detmethod to False *)
(* 
tchoice = 1/(p*x-x1); 
schoice = 1/(q*x-x2); 
*)

(* choice 2 -- do not forget to set byhand to True and detmethod to False *)
(* *)
tchoice = 1/(p*x1-x); 
schoice = 1/(q*x2-x); 
(* *)

(* choice 3 -- do not forget to set byhand to True and detmethod to False *)
(* 
tchoice = 1/Sqrt[(p*x-x1)*(p*x1-x)]; 
schoice = 1/Sqrt[(q*x-x2)*(q*x2-x)]; 
*)

(* L and M must be given in a format that allows one to test the *)
(* compatibility condition in the form:  L2.M - M1.L = 0 on lattice *)
(* and computation of matrix N so that L2.M - M1.L = (lattice)*N *)

(* choice 1 -- do not forget to set LaxPairGiven to True *)
(* and testconsistencyoncube to False, if you want to skip it *)
(* 
candidateL = 1/(p*x-x1)*
  {
  {(p^2-k^2)*x1+p*(k^2-1)*x,              -k*(p^2-1)*x*x1 },
  {(p^2-1)*k,                  -( (p^2-k^2)*x+p*(k^2-1)*x1 ) }
  };

candidateM = 1/(q*x-x2)*
  {
  {(q^2-k^2)*x2+q*(k^2-1)*x,              -k*(q^2-1)*x*x2 },
  {(q^2-1)*k,                  -( (q^2-k^2)*x+q*(k^2-1)*x2 ) }
  };
*)

(* choice 2 -- do not forget to set LaxPairGiven to True *)
(* and testconsistencyoncube to False, if you want to skip it *)
(* 
candidateL = 1/(p*x1-x)*
  {
  {(p^2-k^2)*x1+p*(k^2-1)*x,              -k*(p^2-1)*x*x1 },
  {(p^2-1)*k,                  -( (p^2-k^2)*x+p*(k^2-1)*x1 ) }
  };

candidateM = 1/(q*x2-x)*
  {
  {(q^2-k^2)*x2+q*(k^2-1)*x,              -k*(q^2-1)*x*x2 },
  {(q^2-1)*k,                  -( (q^2-k^2)*x+q*(k^2-1)*x2 ) }
  };
*)

(* choice 3 -- do not forget to set LaxPairGiven to True *)
(* and testconsistencyoncube to False, if you want to skip it *)
(* 
candidateL = (1/Sqrt[(p*x-x1)*(p*x1-x)])*
  {
  {(p^2-k^2)*x1+p*(k^2-1)*x,              -k*(p^2-1)*x*x1 },
  {(p^2-1)*k,                  -( (p^2-k^2)*x+p*(k^2-1)*x1 ) }
  };

candidateM = (1/Sqrt[(q*x-x2)*(q*x2-x)])*
  {
  {(q^2-k^2)*x2+q*(k^2-1)*x,              -k*(q^2-1)*x*x2 },
  {(q^2-1)*k,                  -( (q^2-k^2)*x+q*(k^2-1)*x2 ) }
  };
*)

(* **************************************************************** *)
(* CONTROL FLAGS *)

(* Computation of t and s with determinant method or given by hand *)
(* in this data file. *)
(* Set either byhand or detmethod to True, the other one must be set False *)
(* Computation will only be carried out if LaxPairGiven is False !!! *)

byhand      = True;
detmethod   = False;

(* Lax pair is either given in this data file or will be computed *)

LaxPairGiven = False;
(* LaxPairGiven = True; *)

(* applying or skipping the consistency test around the cube *)

testconsistencyoncube = True; 
(* testconsistencyoncube = False; *)

(* ****************************************************************** *)

(* use of powerexpand and simplification rules for roots *)
(* they should have opposite Boolean values *)

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

(* control for computation of gauge transformation *)

controlgaugetf = False; 
(* controlgaugetf = True; *)

(* l_Q3zero.m *)

(* ################################################################ *)

