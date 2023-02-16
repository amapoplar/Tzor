(* ::Package:: *)

DLorenz::usage = "DLorenz will do D-dimesions divgence."


Begin["`Piravte`"]


(*\:56db\:7ef4\:504f\:5bfc\:6027\:8d28\:548c\:5b9a\:4e49*)
DLorenz[fourVector[x_,index1_],x_,index2_] := metric[index1,index2]
DLorenz[metric[index1_,index2_],x_,index3_] := 0
DLorenz[fun1_ fun2_,x0_,index_]:=DLorenz[fun1,x0,index]fun2+fun1 DLorenz[fun2,x0,index]
DLorenz[fun_^n_,x_,index_]:= n fun^(n-1) DLorenz[fun,x,index]
DLorenz[fun1_ +fun2_,x0_,index_]:=DLorenz[fun1,x0,index]+DLorenz[fun2,x0,index]
DLorenz[x0_^n_,x0_,index_]:=
	Module[{},2(D[(x0^n)/.{x0->Sqrt[x0]},x0]/.{x0->x0^2})fourVector[x0,index]]
	DLorenz[a_?NumberQ,q_,\[Mu]_]:=0
DLorenz[k_,q_,\[Mu]_]:=0
DLorenz[slash[x_],x_,\[Mu]_]:= gamma[\[Mu]]
DLorenz[scalarP[x_,y_],q_,\[Mu]_]:=(DLorenz[fourVector[x,indexNew["sp"]],q,\[Mu]]fourVector[y,indexNew["sp"]]+DLorenz[fourVector[y,indexNew["sp"]],q,\[Mu]]fourVector[x,indexNew["sp","EndQ"->True]])/.{metric[\[Lambda]_,\[Nu]_]fourVector[p_,\[Lambda]_]:>fourVector[p,\[Nu]]}
DLorenz[Dot[fun1_, fun2___],x0_,index_]:=(Dot[ DLorenz[fun1,x0,index],fun2]+Dot[fun1, DLorenz[Dot[fun2],x0,index]])/.{Dot[___,0,___]:>0,Dot[a___,g_+c_,h___]:>Dot[a,g,h]+Dot[a,c,h]}
DLorenz[FeynAmpD[a___,{k_,m_,n_},b___],q_,\[Mu]_]:=FeynAmpD[a,{k,m,n+1},b]DLorenz[scalarP[k,k],q,\[Mu]]+FeynAmpD[{k,m,n}]DLorenz[FeynAmpD[a,b],q,\[Mu]]//Expand


End[]
