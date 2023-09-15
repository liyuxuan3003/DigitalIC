#!/usr/bin/env wolframscript
(* ::Package:: *)

SetDirectory[NotebookDirectory[]];
Needs["MaTeX`"];


nbFileName="Chapter03B_01.fig.wls";
nbPDFName="build/"<>StringReplace[nbFileName,"wls"->"pdf"];
nbPDFNameList=(StringReplace[nbPDFName,".fig"->#<>".fig"]&)/@Alphabet[];
Export[nbPDFName,""]


xic=1 (* \:901f\:5ea6\:9971\:548c\:4e34\:754c\:7535\:573a *)
kappa[V_]:=1/(1+(V/1.25)) (* \:901f\:5ea6\:9971\:548ckappa\:51fd\:6570 *)
s=Solve[kappa[VGS-VT](VGS-VT)==VDS && VDS<=1, VGS] (* \:901f\:5ea6\:9971\:548c\:66f2\:7ebf\:7684\:89e3 *)
 
ListL=Reverse[{0.25,0.50,1.00,2.00,4.00,8.00}] (* \:6c9f\:9053\:957f\:5ea6\:5217\:8868 *)

VDSAT=0.63  (* \:901f\:5ea6\:9971\:548c\:7535\:538b *)
VT=0.43     (* \:9608\:503c\:7535\:538b *)
l=0.06      (* \:6c9f\:9053\:957f\:5ea6\:8c03\:5236Lambda *)

RBColorList=Table[Blend[{Blue,Red},x],{x,0.2,0.8,0.15}] (* \:7ea2\:84dd\:989c\:8272\:5217\:8868 *)
RBColorList2=Table[Blend[{Blue,Red},x],{x,0.20,0.80,0.05}] (* \:7ea2\:84dd\:989c\:8272\:5217\:88682 *)


(* \:5939\:65ad\:9971\:548cID *)
ID[VGS_,VDS_]:=Piecewise[{
	{0,VGS<=VT},
	{((VGS-VT)VDS-VDS^2/2),VDS<=VGS-VT},
	{((VGS-VT)^2/2),VDS>=VGS-VT}
}]


(* \:901f\:5ea6\:9971\:548cID *)
IDx[VGS_,VDS_]:=Piecewise[{
	{0,VGS<=VT},
	{((VGS-VT)VDS-VDS^2/2)kappa[VDS],VDS<=kappa[VGS-VT](VGS-VT)},
	{((VGS-VT)(kappa[VGS-VT](VGS-VT))-(kappa[VGS-VT](VGS-VT))^2/2)kappa[kappa[VGS-VT](VGS-VT)],VDS>kappa[VGS-VT](VGS-VT)}
}]


(* \:8c03\:548c\:624b\:5de5\:5206\:6790\:6a21\:578bID *)
IDFinal[VGS_,VDS_]:=Piecewise[{
	{0,VGS-VT<=0},
	{(VGS-VT)Min[VGS-VT,VDS,VDSAT]-(Min[VGS-VT,VDS,VDSAT]^2)/2,VGS-VT>=0}
}]


figSettings={Frame->True,GridLines->Automatic,ImageSize->Automatic->300};
frameLabel[t1_,t2_]:=
{
	MaTeX[t1,Magnification->0.8,"Preamble"->{"\\usepackage{siunitx}"}],
	MaTeX[t2,Magnification->0.8,"Preamble"->{"\\usepackage{siunitx}"}]
};


fig3DSettings=
{
	ViewPoint->{-100,-80,60},
	PlotPoints -> 100,
	MaxRecursion -> 7,
	ExclusionsStyle->{None,Black},
	ColorFunction->ColorData["DarkRainbow"]
};

fig3DAxesLabel[t1_,t2_,t3_]:=
{
	MaTeX[t1,Magnification->0.8,"Preamble"->{"\\usepackage{siunitx}"}],
	MaTeX[t2,Magnification->0.8,"Preamble"->{"\\usepackage{siunitx}"}],
	MaTeX[t3,Magnification->0.8,"Preamble"->{"\\usepackage{siunitx}"}]
};


fig01Settings=
{
	Evaluate@figSettings,
	PlotStyle->{Black},
	FrameTicks->
	{
		Table[{x,MaTeX[ToString[PaddedForm[x,{3,2}]],Magnification->0.5]},{x,0.0,2.5,0.25}],
		Table[{x,MaTeX[ToString[PaddedForm[x,{2,2}]],Magnification->0.5]},{x,0.4,0.9,0.05}]
	},
	FrameLabel->frameLabel["V_{BS}~(\\si{V})","V_T (\\si{V})"]
};

fig01=Plot[0.45+0.4(Sqrt[Abs[2*(-0.3)-VSB]]-Sqrt[Abs[2*(-0.3)]]),
	{VSB,0,2.5},
	PlotRange->{0.4,0.9},
	Evaluate@fig01Settings
]


fig02Settings=
{
	Evaluate@figSettings,
	PlotStyle->{Black},
	FrameTicks->
	{
		Table[{x,MaTeX[ToString[PaddedForm[x,{2,1}]],Magnification->0.5]},{x,0.0,2,0.2}],
		Table[{x,MaTeX[ToString[PaddedForm[x,{2,1}]],Magnification->0.5]},{x,0.0,1.0,0.1}]
	},
	FrameLabel->frameLabel["|\\xi|/\\xi_c","v/v_{sat}"]
};

fig02=Plot[
	Piecewise[{{(xi)/(1+(xi/xic))/(xic/2),xi<=xic},{1,xi>=xic}}],{xi,0,2},
	PlotRange->{-0.1,1.1},
	Evaluate@fig02Settings
]


fig03Settings[legendsPos_]:=
{
	Evaluate@figSettings,
	PlotStyle->ColorData[10],
	PlotLegends->Placed[
		LineLegend[
			(MaTeX["L="<>ToString[PaddedForm[#,{3,2}]]<>"\\si{um}","Preamble"->{"\\usepackage{siunitx}"},Magnification->0.65]&)/@ ListL,
			LegendFunction->"Panel",
			LegendLayout -> {"ReversedColumn",3}
		],
		legendsPos
	]
};

fig03ASettings=
{
	Evaluate@fig03Settings[{Left,Bottom}],
	FrameTicks->
	{
		Table[{x,MaTeX[ToString[PaddedForm[x,{2,1}]],Magnification->0.5]},{x,0,5,0.5}],
		Table[{x,MaTeX[ToString[PaddedForm[x,{2,1}]],Magnification->0.5]},{x,0.0,1.0,0.1}]
	},
	FrameLabel->frameLabel["V_{DS}~(\\si{V})","\\kappa(V_{DS})"]
};

fig03BSettings=
{
	Evaluate@fig03Settings[{Left,Top}],
	FrameTicks->
	{
		Table[{x,MaTeX[ToString[PaddedForm[x,{2,1}]],Magnification->0.5]},{x,0,5,0.5}],
		Table[{x,MaTeX[ToString[PaddedForm[x,{2,1}]],Magnification->0.5]},{x,0.0,5.0,0.5}]
	},
	FrameLabel->frameLabel["V_{GT}~(\\si{V})","\\kappa(V_{GT})V_{GT}"]
};

fig03A=Plot[
	Evaluate@Table[1/(1+(V/(5 L))),{L,ListL}],{V,0,5},
	PlotRange->{-0.1,1.05},
	Evaluate@fig03ASettings
]

fig03B=Plot[
	Evaluate@Table[V/(1+(V/(5 L))),{L,ListL}],{V,0,5},
	PlotRange->{-0.5,5.25},
	Evaluate@fig03BSettings
]


fig04ASettings=
{
	Evaluate@figSettings,
	PlotPoints->1500,
	PlotStyle->Join[{{Gray,Thickness[0.002]},{Black,Thickness[0.002]}},RBColorList],
	Epilog -> 
	{
		Text[MaTeX["V_{GS}=1.0\\si{V}",Magnification->0.6,"Preamble"->{"\\usepackage{siunitx}"}],{3.25,0.05}],
		Text[MaTeX["V_{GS}=1.5\\si{V}",Magnification->0.6,"Preamble"->{"\\usepackage{siunitx}"}],{3.25,0.25}],
		Text[MaTeX["V_{GS}=2.0\\si{V}",Magnification->0.6,"Preamble"->{"\\usepackage{siunitx}"}],{3.25,0.49}],
		Text[MaTeX["V_{GS}=2.5\\si{V}",Magnification->0.6,"Preamble"->{"\\usepackage{siunitx}"}],{3.25,0.75}],
		Text[MaTeX["V_{GS}=3.0\\si{V}",Magnification->0.6,"Preamble"->{"\\usepackage{siunitx}"}],{3.25,1.03}],
		Text["\:5939\:65ad\:9971\:548c",{2.13,0.65}],
		Text["\:901f\:5ea6\:9971\:548c",{1.05,0.65}]
	},
	FrameTicks->
	{
		Table[{x,MaTeX[ToString[PaddedForm[x,{3,2}]],Magnification->0.5]},{x,0,3.5,0.25}],
		Table[{x,MaTeX[ToString[PaddedForm[x,{2,1}]],Magnification->0.5]},{x,0,1.2,0.1}]
	},
	FrameLabel->frameLabel["V_{DS}~(\\si{V})","I_D/k_n"]
};

fig04BSettings=
{
	Evaluate@figSettings,
	PlotPoints->1500,
	PlotStyle->Join[{{Black,Thickness[0.002]}},RBColorList],
	Epilog -> 
	{
		Text[MaTeX["V_{GS}=1.0\\si{V}",Magnification->0.6,"Preamble"->{"\\usepackage{siunitx}"}],{3.25,0.02}],
		Text[MaTeX["V_{GS}=1.5\\si{V}",Magnification->0.6,"Preamble"->{"\\usepackage{siunitx}"}],{3.25,0.40}],
		Text[MaTeX["V_{GS}=2.0\\si{V}",Magnification->0.6,"Preamble"->{"\\usepackage{siunitx}"}],{3.25,1.05}],
		Text[MaTeX["V_{GS}=2.5\\si{V}",Magnification->0.6,"Preamble"->{"\\usepackage{siunitx}"}],{3.25,1.95}],
		Text[MaTeX["V_{GS}=3.0\\si{V}",Magnification->0.6,"Preamble"->{"\\usepackage{siunitx}"}],{3.25,3.15}],
		Text["\:5939\:65ad\:9971\:548c",{2.0,2.75}]
	},
	FrameTicks->
	{
		Table[{x,MaTeX[ToString[PaddedForm[x,{3,2}]],Magnification->0.5]},{x,0,3.5,0.25}],
		Table[{x,MaTeX[ToString[PaddedForm[x,{2,1}]],Magnification->0.5]},{x,0,3.5,0.5}]
	},
	FrameLabel->frameLabel["V_{DS}~(\\si{V})","I_D/k_n"]
};

fig04CSettings=
{
	Evaluate@figSettings,
	PlotPoints->1500,
	PlotStyle->Join[({Thickness[0.0025],#}&)/@RBColorList2,{{Black,Thickness[0.004],Dashed},{Black,Thickness[0.004],Dashed}}],
	Epilog->
	{
		Thickness[0.004],
		Dashed,
		Line[{{VDSAT,IDFinal[VDSAT+VT,VDSAT]},{VDSAT,1.4}}],
		Text[Style["\:7ebf\:6027\:533a",{Small}],{0.2,0.8},Background->White],
		Text[Style["\:901f\:5ea6\:9971\:548c\:533a",{Small}],{1.01,0.8},Background->White],
		Text[Style["\:5939\:65ad\:9971\:548c\:533a",{Small}],{1.0,0.1},Background->White],
		Text[MaTeX["V_{DS}=V_{GT}",Magnification->0.8],{0.45,-0.09},Background->White],
		Text[MaTeX["V_{DS}=V_{DSAT}",Magnification->0.8],{0.7,1.5},Background->White],
		Text[MaTeX["V_{GT}=V_{DSAT}",Magnification->0.8],{3.0,0.3},Background->White]
	},
	FrameTicks->
	{
		Table[{x,MaTeX[ToString[PaddedForm[x,{3,2}]],Magnification->0.5]},{x,0,3.5,0.25}],
		Table[{x,MaTeX[ToString[PaddedForm[x,{2,1}]],Magnification->0.5]},{x,0,1.5,0.1}]
	},
	FrameLabel->frameLabel["V_{DS}~(\\si{V})","I_D/k_n"]
};

fig04A=Plot[Evaluate@Join[{IDx[(VGS/.s)[[1]],VDS],IDx[VDS+VT,VDS]},Table[IDx[VGS,VDS],{VGS,{1.0,1.5,2.0,2.5,3.0}}]],
	{VDS,0,3.5},
	PlotRange->{-0.1,1.3},
	Evaluate@fig04ASettings
]

fig04B=Plot[Evaluate@Join[{ID[VDS+VT,VDS]},Table[ID[VGS,VDS],{VGS,{1.0,1.5,2.0,2.5,3.0}}]],
	{VDS,0,3.5},
	PlotRange->{-0.291,3.5+0.291},
	Evaluate@fig04BSettings
]

fig04C=Plot[Evaluate@Join[Table[IDFinal[VGS,VDS],{VGS,0.6,3.0,0.2}],{
		Piecewise[{{IDFinal[VDSAT+VT,VDS],VDS>VDSAT},{-1,VDS<VDSAT}}],
		Piecewise[{{IDFinal[VDS+VT,VDS],VDS<VDSAT},{-1,VDS>VDSAT}}]
	}],
	{VDS,0,3.5},
	PlotRange->{-0.2,1.6},
	Evaluate@fig04CSettings
]


fig05ASettings=
{
	Evaluate@figSettings,
	PlotStyle->Black,
	Epilog->{Text["\:5e73\:65b9\:5173\:7cfb",{0.75,0.18}],Text["\:7ebf\:6027\:5173\:7cfb",{2.60,1.07}]},
	FrameTicks->
	{
		Table[{x,MaTeX[ToString[PaddedForm[x,{3,2}]],Magnification->0.5]},{x,0,3.5,0.25}],
		Table[{x,MaTeX[ToString[PaddedForm[x,{2,1}]],Magnification->0.5]},{x,0,1.2,0.1}]
	},
	FrameLabel->frameLabel["V_{GS}~(\\si{V})","I_D/k_n"]
};

fig05BSettings=
{
	Evaluate@figSettings,
	PlotStyle->Black,
	Epilog->{Text["\:5e73\:65b9\:5173\:7cfb",{2.30,1.07}]},
	FrameTicks->
	{
		Table[{x,MaTeX[ToString[PaddedForm[x,{3,2}]],Magnification->0.5]},{x,0,3.5,0.25}],
		Table[{x,MaTeX[ToString[PaddedForm[x,{2,1}]],Magnification->0.5]},{x,0,3.5,0.5}]
	},
	FrameLabel->frameLabel["V_{GS}~(\\si{V})","I_D/k_n"]
};

fig05A=Plot[{IDx[VGS,VDS]/.{VDS->5}},
	{VGS,0,3.5},
	PlotRange->{-0.1,1.3},
	Evaluate@fig05ASettings
]

fig05B=Plot[{ID[VGS,VDS]/.{VDS->5}},
	{VGS,0,3.5},
	PlotRange->{-0.291,3.5+0.291},
	Evaluate@fig05BSettings
]


fig06ASettings=
{
	Evaluate@fig3DSettings,
	Exclusions->{VDS==VGS-VT,kappa[VGS-VT](VGS-VT)==VDS},
	Ticks->
	{
		Table[{x,MaTeX[ToString[PaddedForm[x,{3,2}]],Magnification->0.5]},{x,0,3.5,0.5}],
		Table[{x,MaTeX[ToString[PaddedForm[x,{3,2}]],Magnification->0.5]},{x,0,3.5,0.5}],
		Table[{x,MaTeX[ToString[PaddedForm[x,{3,2}]],Magnification->0.5]},{x,0,1.2,0.2}]
	},
	AxesLabel->fig3DAxesLabel["V_{DS}~(\\si{V})","V_{GS}~(\\si{V})","I_D/k_n"]
};

fig06BSettings=
{
	Evaluate@fig3DSettings,
	Exclusions->{VDS==VGS-VT},
	Ticks->
	{
		Table[{x,MaTeX[ToString[PaddedForm[x,{3,2}]],Magnification->0.5]},{x,0,3.5,0.5}],
		Table[{x,MaTeX[ToString[PaddedForm[x,{3,2}]],Magnification->0.5]},{x,0,3.5,0.5}],
		Table[{x,MaTeX[ToString[PaddedForm[x,{3,2}]],Magnification->0.5]},{x,0,4.5,0.5}]
	},
	AxesLabel->fig3DAxesLabel["V_{DS}~(\\si{V})","V_{GS}~(\\si{V})","I_D/k_n"]
};

fig06CSettings=
{
	Evaluate@fig3DSettings,
	Exclusions->{{VDS==VGS-VT,VDS<VDSAT},{VDS==VDSAT,VGS-VT>VDSAT},{VDSAT==VGS-VT,VDS>VDSAT}},
	Ticks->
	{
		Table[{x,MaTeX[ToString[PaddedForm[x,{3,2}]],Magnification->0.5]},{x,0,3.5,0.5}],
		Table[{x,MaTeX[ToString[PaddedForm[x,{3,2}]],Magnification->0.5]},{x,0,3.5,0.5}],
		Table[{x,MaTeX[ToString[PaddedForm[x,{3,2}]],Magnification->0.5]},{x,0,1.5,0.5}]
	},
	AxesLabel->fig3DAxesLabel["V_{DS}~(\\si{V})","V_{GS}~(\\si{V})","I_D/k_n"]
};

fig06A=Plot3D[IDx[VGS,VDS],{VDS,0.0,3.5},{VGS,0.0,3.5},Evaluate@fig06ASettings]
fig06B=Plot3D[ID[VGS,VDS], {VDS,0.0,3.5},{VGS,0.0,3.5},Evaluate@fig06BSettings]
fig06C=Plot3D[IDFinal[VGS,VDS],{VDS,0,3.5},{VGS,0,3.5},Evaluate@fig06CSettings]


fig={fig01,fig02,fig03A,fig03B,fig04A,fig04B,fig04C,fig05A,fig05B,fig06A,fig06B,fig06C}


figWithName=Transpose[{fig,nbPDFNameList[[1;;Length[fig]]]}];
ReleaseHold[(Hold[Export[#[[2]],#[[1]],ImageResolution->2000]]&)/@figWithName]


(*-*)
