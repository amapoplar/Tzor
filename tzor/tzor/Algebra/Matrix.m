(* ::Package:: *)

Track::usage = "Track is trace of matrices";
Trans::usage = "Trans is Transpose of something";


Begin["`Private`"]


MakeBoxes[(h_)[si[{a_, b_}]], TraditionalForm] := 
 SubscriptBox[ToBoxes[h,TraditionalForm], RowBox[{ToString[a], "", ToString[b]}]]

MakeBoxes[(h_)[ci[{a_, b_}]], TraditionalForm] := 
 SuperscriptBox[ToBoxes[h,TraditionalForm], RowBox[{ToString[a], "", ToString[b]}]]


Format[Track[func_], 
  TraditionalForm] := 
 DisplayForm[RowBox[{"Tr","(",func,")"}]]
Format[Trans[p_],TraditionalForm]:= DisplayForm[
  SuperscriptBox[p,"T"]]


Track[a___,b_?NumericQ c_,d___]:=b Track[a,c,d];
Track[a___,b_?NumericQ,d___]:=b Track[a,d];
CQ[expr_]:=(Head[expr]=!=gamma)\[And](Head[expr]=!=sigma)\[And](Head[expr]=!=slash);
CQ[Trans[expr_]]:=CQ[expr]
CQ[CJM]:= False;
CQ[a_ b_]:=CQ[a]\[And]CQ[b];
CQ[a_+b_]:=CQ[a]\[And]CQ[b];
CQ[a_ .b_]:=CQ[a]\[And]CQ[b];
Trans[Trans[a_]]:= a
Trans[a_+b_]:= Trans[a]+Trans[b]
Trans[a_ b_?CQ]:=b Trans[a]
Trans[1]:= 1
Trans[mass[q_]]:= mass[q]
Trans[scalorP[q1_,q2_]]:= scalorP[q1,q2]
Trans[a_?NumberQ]:= a
Track[a_?CQ b_]:= a Track[b]


End[]
