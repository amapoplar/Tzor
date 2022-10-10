(* ::Package:: *)

fourierTransform::usage = "caclulate the Fourier transform from x-space to p-sapce"
fourierTransformConvolve::usage = "Fourier Transform Convole"


Begin["`Private`"]


(*\:8f85\:52a9\:51fd\:6570*)
giveIndice[fourVector[a_,index_],a_]:=index
giveIndice[a_,x_]:=Nothing
giveIndice[a_ b_,x_]:=Flatten[{giveIndice[a,x],giveIndice[b,x]}]

(*\:5085\:91cc\:53f6\:4e3b\:79ef\:5206\:51fd\:6570*)
fourierMasterFunction[s_,q_]:=-I 2^(dim-2s) \[Pi]^(dim/2) (-q^2)^(s-dim/2) Gamma[dim/2-s]/Gamma[s]
(*\:5085\:91cc\:53f6\:53d8\:6362*)
fourierTransform[a_?NumericQ,x0_,p0_]:=a (2\[Pi])^4 delta[p0]
fourierTransform[func1_+func2_,x0_,p0_]:=fourierTransform[func1,x0,p0]+fourierTransform[func2,x0,p0]
fourierTransform[func0_,x0_,p0_]:=Module[{fun = func0,s=-Exponent[func0,x0]/2,x=x0,p=p0,slashQ = MemberQ[func0,slash[x0],Infinity],slashindex = Nothing,nLorenz =giveIndice[func0,x0] ,tempresult=0},
tempresult= fourierMasterFunction[s,p]//Expand;
fun = fun/.{fourVector[x,q_]:>1,x->1};
tempresult = If[Length[nLorenz]>0,Fold[ Expand[-I DLorenz[#1,p,#2]]&,tempresult*fun,nLorenz ],tempresult*fun];
If[slashQ,slashindex = ToExpression["\[Alpha]"<>Fold[StringDelete,DateString[],{" ",":"}]];
Expand[-I DLorenz[tempresult,p,slashindex]]/.{fourVector[p,slashindex]->1,slash[1]-> slash[p],metric[index_,slashindex]:>  gamma[index]},tempresult ]
]


(*\:5085\:91cc\:53f6\:5377\:79ef*)
momentum[FeynAmpD[a_?ListQ]]:=First[a]
momentum[FeynAmpD[a_]]:=a
momentum[FeynAmpD[a_,b__]]:=momentum[FeynAmpD[a]]+momentum[FeynAmpD[b]]
momentum[a_ b_]:=momentum[a]+momentum[b]
momentum[a_]:=0
fourierTransformConvolve[exp_,x_,p_]:= Module[{exp0=exp,j = 0, kinternal = momentum[exp],\[Nu]=0},
exp0 = exp0/.scalarP[x,k_]:> fourVector[x,indexNew["\[Mu]"<>ToString[j=j+1]]]fourVector[k,indexNew["\[Mu]"<>ToString[j]]];
exp0 = fourierTransform[exp0,x,p];
exp0 = exp0/.{(-p^2)^n_:> (-1)^n FeynAmpD[{p,0,n}],(-p^2):> (-1)FeynAmpD[p]};
exp0 = exp0//.{fourVector[k_,n_]fourVector[l_,n_]:>scalarP[k,l], metric[a_,b_]fourVector[l_,a_]fourVector[k_,b_]:>scalarP[l,k]};
exp0/.{p->p-kinternal}
]


End[]
