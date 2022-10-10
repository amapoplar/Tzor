(* ::Package:: *)

$consedates = {mass,chiral,hybrid,doubleG,tripleG}


dimension::usage = "dimension of consedate.";


Begin["`Private`"]


Format[mass[q_], TraditionalForm] := DisplayForm[SubscriptBox["m", RowBox[{q}]]]
Format[chiral[q_],TraditionalForm]:=DisplayForm[RowBox[{"\[LeftAngleBracket]",OverscriptBox[q,"_"],q,"\[RightAngleBracket]"}]]
Format[hybrid[q_],TraditionalForm]:=DisplayForm[RowBox[{"\[LeftAngleBracket]",SubscriptBox["g","s"],OverscriptBox[q,"_"],"\[Sigma]","\[CenterDot]","G",q,"\[RightAngleBracket]"}]]
Format[doubleG,TraditionalForm]:=DisplayForm[RowBox[{"\[LeftAngleBracket]",SuperscriptBox["g","2"],SuperscriptBox["G","2"],"\[RightAngleBracket]"}]]
Format[tripleG,TraditionalForm]:=DisplayForm[RowBox[{"\[LeftAngleBracket]","f",SuperscriptBox["G","3"],"\[RightAngleBracket]"}]]


dimension[mass[q_]]:=1
dimension[chiral[q_]]:=3
dimension[hybrid[q_]]:=5
dimension[doubleG]:=4
dimension[tripleG]:=6
dimension[a_ b_]:=dimension[a]+dimension[b]
dimension[a_^n_]:=n dimension[a]
dimension[a_]:=0


Trans[mass[q_]]:= mass[q]


End[]
