(* ::Package:: *)

(* Wolfram Language package *)

$IndicesLists = {si,ci}

TzorDeclareHeader[FileNameJoin[{$TzorDirectory,"Algebra/LorenzAlgebra.m"}]]
QField::usage = "QuarkQField[f,a,\[Alpha],x] is a flavor f quark filed at postion x with color a and Lonrtez index \[Alpha]";
QFieldB::usage= "QuarkQQFieldB[f,a,\[Alpha],x] is a flavor f anti quark filed at postion x with color a and Lonrtez index \[Alpha]";
GField::usage = "GluonQField[n,si[{\[Mu],\[Nu]}],x]is a gluon tensor QField at postion x with color n, \[Mu] and \[Nu] are Lonrtez indices";
DE::usage = "DE is a quark progrator";
GE::usage = "GE is a gluon progrator";
gs::usage = "gs is strong copusing contants number";


Begin["`Private`"]
Format[QField[u_, a_, \[Alpha]_, x_], TraditionalForm] := DisplayForm[RowBox[{SubsuperscriptBox[u, \[Alpha], a], "(", x, ")"}]]
Format[QFieldB[u_, a_, \[Alpha]_, x_], TraditionalForm] := DisplayForm[RowBox[{SubsuperscriptBox[OverBar[u], \[Alpha], a], "(", x, ")"}]] 
Format[GField[n_,x_],TraditionalForm]:= DisplayForm[RowBox[{SuperscriptBox["G", n], "(", x, ")"}]]
Format[QField[u_, a_, x_], TraditionalForm] := DisplayForm[RowBox[{SuperscriptBox[u, a], "(", x, ")"}]]
Format[QFieldB[u_, a_, x_], TraditionalForm] :=DisplayForm[RowBox[{SuperscriptBox[OverBar[u],  a], "(", x, ")"}]] 
Format[gs,TraditionalForm]:= DisplayForm[SubscriptBox["g", "s"]]
Format[GField[n_,x_][li[{\[Mu]_,\[Nu]_}]],TraditionalForm]:=DisplayForm[RowBox[{SubsuperscriptBox["G",RowBox[{\[Mu],\[Nu]}],n], "(", x, ")"}]]
Format[GField[n_][li[{\[Mu]_,\[Nu]_}]],TraditionalForm]:=DisplayForm[SubsuperscriptBox["G",RowBox[{\[Mu],\[Nu]}],n]]
Format[DE[{ferm_, ferm_}, {x_ , y_}], TraditionalForm] := DisplayForm[RowBox[{SuperscriptBox["S",ferm], "(", y-x, ")"}]]
Format[DE[{ferm_,ferm_},{x_,y_}][si[{\[Mu]_,\[Nu]_}]],TraditionalForm]:=DisplayForm[RowBox[{SubsuperscriptBox["S",RowBox[{\[Mu],\[Nu]}],ferm], "(", y-x, ")"}]]
Format[DE[{ferm_,ferm_},{x_,y_}][ci[{\[Mu]_,\[Nu]_}]],TraditionalForm]:=DisplayForm[RowBox[{SubsuperscriptBox["S",RowBox[{\[Mu],\[Nu]}],ferm], "(", y-x, ")"}]]
Format[DE[{ferm_,ferm_},{x_,y_}][ci[{a_,b_}],si[{\[Mu]_,\[Nu]_}]],TraditionalForm]:= DisplayForm[RowBox[{SubsuperscriptBox["S", RowBox[{\[Mu], \[Nu]}], RowBox[{ferm, ",", RowBox[{a, b}]}]], "(",y-x, ")"}]]
Format[GE[GField[n1_,x_][li[{\[Mu]1_,\[Nu]1_}]],GField[n2_,y_][li[{\[Mu]2_,\[Nu]2_}]]],TraditionalForm]:= DisplayForm[RowBox[{"\[LeftAngleBracket]",SubsuperscriptBox["G",RowBox[{\[Mu]1,\[Nu]1}],n1],"(", x, ")",SubsuperscriptBox["G",RowBox[{\[Mu]2,\[Nu]2}],n2],"(", y, ")", "\[RightAngleBracket]"}]]



Unprotect[NonCommutativeMultiply]
Format[NonCommutativeMultiply[list__],TraditionalForm]:=DisplayForm[ RowBox[{list}]]
NonCommutativeMultiply[a___,-b_,c___]:=-NonCommutativeMultiply[a,b,c]
NonCommutativeMultiply[a___,n_?NumberQ b_,c___]:=n NonCommutativeMultiply[a,b,c]
Protect[NonCommutativeMultiply]


End[]
