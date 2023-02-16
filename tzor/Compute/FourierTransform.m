(* ::Package:: *)

fourierTransform::usage = "caclulate the Fourier transform from x-space to p-sapce"
fourierTransformConvolve::usage = "Fourier Transform Convole"


Begin["`Private`"]


(*\:8f85\:52a9\:51fd\:6570*)
giveIndice[fourVector[a_,index_],a_]:=index
giveIndice[a_,x_]:=Nothing
giveIndice[a_ b_,x_]:=Flatten[{giveIndice[a,x],giveIndice[b,x]}]


(*\:5085\:91cc\:53f6\:4e3b\:79ef\:5206\:51fd\:6570*)
fourierMasterFunction[s_,q_]:=-I 2^(dim-2s) \[Pi]^(dim/2) (-scalarP[q,q])^(s-dim/2) Gamma[dim/2-s]/Gamma[s]
(*\:5085\:91cc\:53f6\:53d8\:6362*)
fourierTransform[a_?NumericQ,x0_,p0_]:=a (2\[Pi])^4 delta[p0]
fourierTransform[func1_+func2_,x0_,p0_]:=fourierTransform[func1,x0,p0]+fourierTransform[func2,x0,p0]

fourierTransform[func0_,x0_,p0_]:=Module[{fun = func0,s=0,x=x0,p=p0,slashindex = Nothing,nLorenz ={},tempresult=0},
s = -Exponent[fun,scalarP[x,x]];
tempresult= fourierMasterFunction[s,p]//Expand;
fun =fun/.{scalarP[x,x]->1,scalarP[x,k_?(#=!=x&)]:>fourVector[x,indexNew["\[Mu]"]]fourVector[k,indexNew["\[Mu]","EndQ"->True]],slash[x]:> fourVector[x,indexNew["\[Mu]"]]gamma[indexNew["\[Mu]","EndQ"->True]]};
nLorenz = giveIndice[fun,x];
fun = fun/.{fourVector[x,_]:>1};
tempresult = If[Length[nLorenz]>0,Fold[ Expand[-I DLorenz[#1,p,#2]]&,tempresult*fun,nLorenz ],tempresult*fun]//.{metric[\[Mu]_,\[Nu]_]fourVector[p1_,\[Mu]_]:>fourVector[p1,\[Nu]],fourVector[k1_,\[Mu]_]fourVector[k2_,\[Mu]_]:>scalarP[k1,k2],gamma[\[Mu]_]fourVector[p1_,\[Mu]_]:>slash[p1]}
]



(*\:5085\:91cc\:53f6\:5377\:79ef*)
fourierTransformConvolve[func1_+func2_,x0_,p0_]:=fourierTransform[func1,x0,p0]+fourierTransformConvolve[func2,x0,p0]
fourierTransformConvolve[exp_,x_,p0_]:= Module[{exp0=exp,j = 0, kinternal = momentum[exp]},
exp0 = fourierTransform[exp0,x,p0];
exp0/.{(-scalarP[p0,p0])^n_:>(-1)^n FeynAmpD[{p0,0,-n}],scalarP[p0,p0]:>FeynAmpD[{p0,0,1}]}/.{diracDelta[a__]:>diracDelta[p0,a]}
]






End[]
