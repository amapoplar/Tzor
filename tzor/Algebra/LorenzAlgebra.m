(* ::Package:: *)

(* Wolfram Language package *)
$indexli = {li}
$TensorAndVector = {fourVector,metric,scalarP}
dim::usage = "dim is space dimension D";


Begin["`Private`"]


(*\:56db\:77e2\:548c\:5ea6\:89c4\:7684\:683c\:5f0f*)
Format[dim,TraditionalForm]:=DisplayForm[ToExpression["D"]]
Format[fourVector[v_,index_],TraditionalForm]:=DisplayForm[Subscript[v,index]]
Format[metric[index1_,index2_],TraditionalForm]:=DisplayForm[Subscript["g",RowBox[{index1,index2}]]]

SetAttributes[metric,Orderless]


fourVector[-p_,\[Mu]_]:=-fourVector[p,\[Mu]]
fourVector[x_+y_,\[Mu]_]:=fourVector[x,\[Mu]]+fourVector[y,\[Mu]]


Format[scalarP[x_,p_],TraditionalForm]:=DisplayForm[RowBox[{x,"\[CenterDot]",p}]]
scalarP[x1_+x2_,x_]:=scalarP[x1,x]+scalarP[x2,x]
scalarP[-x1_,x2_]:=-scalarP[x1,x2]
Unprotect[Dot]
scalarP[k1_,k2_].scalarP[k3_,k4_]:= scalarP[k1,k2]scalarP[k3,k4]
Protect[Dot]
SetAttributes[scalarP,Orderless]


End[]
