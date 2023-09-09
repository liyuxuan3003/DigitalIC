#!/usr/bin/env wolframscript
(* ::Package:: *)

Plot[(Exp[VD/(0.026)]-1),{VD,-1,1},Frame->True,PlotRange->{-10^13,10^14}]


Plot[Evaluate@Table[1/(1-VD/0.7)^m,{m,{1/2,1/3}}],{VD,-4,1},Frame->True,PlotRange->{0,2}]


Plot[
	0.45+0.4(Sqrt[Abs[2*(-0.3)-VSB]]-Sqrt[Abs[2*(-0.3)]]),
	{VSB,0,2.5},
	Frame->True,
	PlotPoints->20000,
	GridLines->Automatic,
	PlotRange->{0.4,0.9}]






xic=2
mun=1.3 10^11
Plot[
	Piecewise[{{(mun xi)/(1+(xi/xic)),xi<=xic},{(mun xic)/2,xi>=xic}}],{xi,0,4},
	Frame->True,GridLines->Automatic,
	PlotRange->{-0.2 10^11,1.4 10^11}
]
D[(mun xi)/(1+xi/xic),{xi}]/.{xi->xic}


Plot[
	Evaluate@Table[1/(1+(V/(xic L))),{L,0.25,1,0.25}],{V,0,5},
	Frame->True,GridLines->Automatic,PlotRange->{0,1}
]
Plot[
	Evaluate@Table[V/(1+(V/(xic L))),{L,0.25,1,0.25}],{V,0,5},
	Frame->True,GridLines->Automatic,PlotRange->{0,1}
]


kn=115 10^(-6)1.5
VT=0.43
l=0.06
ID[VGS_,VDS_]:=Piecewise[{{0,VGS<=VT},{((VGS-VT)VDS-VDS^2/2),VDS<=VGS-VT},{((VGS-VT)^2/2),VDS>=VGS-VT}}]
Plot[
	Evaluate@Append[Table[ID[VGS,VDS],{VGS,{1.0,1.5,2.0,2.5,3.0}}],ID[VDS,VDS]],
	{VDS,0,3.5},
	Frame->True,
	PlotRange->{-0.5,4},
	GridLines->Automatic]
Plot[
	ID[VGS,VDS]/.{VDS->5},
	{VGS,0,3.5},
	Frame->True,
	PlotRange->{-0.5,4},
	GridLines->Automatic]


kappa[V_]:=1/(1+(V/1.25))
IDx[VGS_,VDS_]:=Piecewise[{
	{0,
	VGS<=VT},
	{((VGS-VT)VDS-VDS^2/2)kappa[VDS],
	VDS<=kappa[VGS-VT](VGS-VT)},
	{((VGS-VT)(kappa[VGS-VT](VGS-VT))-(kappa[VGS-VT](VGS-VT))^2/2)kappa[kappa[VGS-VT](VGS-VT)],
	VDS>kappa[VGS-VT](VGS-VT)}}]
s=Solve[kappa[VGS-VT](VGS-VT)==VDS && VDS<=1, VGS]
Plot[
	Evaluate@Join[Table[IDx[VGS,VDS],{VGS,{1.0,1.5,2.0,2.5,3.0}}],{IDx[(VGS/.s)[[1]],VDS],IDx[VDS,VDS]}],
	{VDS,0,3.5},
	Frame->True,
	GridLines->Automatic,
	PlotPoints->1000,
	PlotRange->{-0.1,1.2}
]
Plot[
	{IDx[VGS,VDS]/.{VDS->5}},
	{VGS,0,3.5},
	Frame->True,
	GridLines->Automatic
]


IDSub[VGS_,VDS_]:=Piecewise[{{Exp[VGS/1](1--VDS/1),VGS<VT}}]
Plot[IDSub[VGS,VDS]/.{VDS->1},{VGS,0,3}]
