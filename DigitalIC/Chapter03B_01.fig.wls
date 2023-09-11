#!/usr/bin/env wolframscript
(* ::Package:: *)

SetDirectory[NotebookDirectory[]];
Needs["MaTeX`"];


nbFileName="Chapter03B_01.fig.wls";
nbPDFName="build/"<>StringReplace[nbFileName,"wls"->"pdf"];
nbPDFNameList=(StringReplace[nbPDFName,".fig"->#<>".fig"]&)/@Alphabet[];
Export[nbPDFName,""]


xic=1
kappa[V_]:=1/(1+(V/1.25))
ListL=Reverse[{0.25,0.50,1.00,2.00,4.00,8.00}]
s=Solve[kappa[VGS-VT](VGS-VT)==VDS && VDS<=1, VGS]
VT=0.43
l=0.06
RBColorList=Table[Blend[{Blue,Red},x],{x,0.2,0.8,0.15}]
ID[VGS_,VDS_]:=Piecewise[{
	{0,VGS<=VT},
	{((VGS-VT)VDS-VDS^2/2),VDS<=VGS-VT},
	{((VGS-VT)^2/2),VDS>=VGS-VT}
}]
IDx[VGS_,VDS_]:=Piecewise[{
	{0,VGS<=VT},
	{((VGS-VT)VDS-VDS^2/2)kappa[VDS],VDS<=kappa[VGS-VT](VGS-VT)},
	{((VGS-VT)(kappa[VGS-VT](VGS-VT))-(kappa[VGS-VT](VGS-VT))^2/2)kappa[kappa[VGS-VT](VGS-VT)],VDS>kappa[VGS-VT](VGS-VT)}
}]
fig=
{
	Plot[0.45+0.4(Sqrt[Abs[2*(-0.3)-VSB]]-Sqrt[Abs[2*(-0.3)]]),
		{VSB,0,2.5},
		Frame->True,
		PlotPoints->1000,
		GridLines->Automatic,
		PlotRange->{0.4,0.9},
		ImageSize->Automatic->300,
		PlotStyle->{Black},
		FrameTicks->{
			Table[{x,MaTeX[ToString[PaddedForm[x,{3,2}]],Magnification->0.5]},{x,0.0,2.5,0.25}],
			Table[{x,MaTeX[ToString[PaddedForm[x,{2,2}]],Magnification->0.5]},{x,0.4,0.9,0.05}]},
		FrameLabel->{
			MaTeX["V_{BS}~(\\si{V})",Magnification->0.8,"Preamble"->{"\\usepackage{siunitx}"}],
			MaTeX["V_T (\\si{V})",Magnification->0.8,"Preamble"->{"\\usepackage{siunitx}"}]}
	],
	Plot[
		Piecewise[{{(xi)/(1+(xi/xic))/(xic/2),xi<=xic},{1,xi>=xic}}],{xi,0,2},
		PlotRange->{-0.1,1.1},
		PlotPoints->1000,
		Frame->True,
		GridLines->Automatic,
		ImageSize->Automatic->300,
		PlotStyle->{Black},
		FrameTicks->{
			Table[{x,MaTeX[ToString[PaddedForm[x,{2,1}]],Magnification->0.5]},{x,0.0,2,0.2}],
			Table[{x,MaTeX[ToString[PaddedForm[x,{2,1}]],Magnification->0.5]},{x,0.0,1.0,0.1}]},
		FrameLabel->{
			MaTeX["\\xi/\\xi_c",Magnification->0.8,"Preamble"->{"\\usepackage{siunitx}"}],
			MaTeX["v/v_{sat}",Magnification->0.8,"Preamble"->{"\\usepackage{siunitx}"}]}
	],
	Plot[
		Evaluate@Table[1/(1+(V/(5 L))),{L,ListL}],{V,0,5},
		PlotRange->{-0.1,1.05},
		PlotPoints->1000,
		Frame->True,
		GridLines->Automatic,
		ImageSize->Automatic->300,
		PlotStyle->ColorData[10],
		FrameTicks->{
			Table[{x,MaTeX[ToString[PaddedForm[x,{2,1}]],Magnification->0.5]},{x,0,5,0.5}],
			Table[{x,MaTeX[ToString[PaddedForm[x,{2,1}]],Magnification->0.5]},{x,0.0,1.0,0.1}]},
		FrameLabel->{
			MaTeX["V_{DS}~(\\si{V})",Magnification->0.8,"Preamble"->{"\\usepackage{siunitx}"}],
			MaTeX["\\kappa(V_{DS})",Magnification->0.8,"Preamble"->{"\\usepackage{siunitx}"}]},
		PlotLegends->Placed[
			LineLegend[
				(MaTeX["L="<>ToString[PaddedForm[#,{3,2}]]<>"\\si{um}",
					"Preamble"->{"\\usepackage{siunitx}"},
					Magnification->0.65]&)/@ ListL,
				LegendFunction->"Panel",
				LegendLayout -> {"ReversedColumn",3}
			],{Left,Bottom}]
	],
	Plot[
		Evaluate@Table[V/(1+(V/(5 L))),{L,ListL}],{V,0,5},
		PlotRange->{-0.5,5.25},
		PlotPoints->1000,
		Frame->True,
		GridLines->Automatic,
		ImageSize->Automatic->300,
		PlotStyle->ColorData[10],
		FrameTicks->{
			Table[{x,MaTeX[ToString[PaddedForm[x,{2,1}]],Magnification->0.5]},{x,0,5,0.5}],
			Table[{x,MaTeX[ToString[PaddedForm[x,{2,1}]],Magnification->0.5]},{x,0.0,5.0,0.5}]},
		FrameLabel->{
			MaTeX["V_{GS}-V_{T}~(\\si{V})",Magnification->0.8,"Preamble"->{"\\usepackage{siunitx}"}],
			MaTeX["\\kappa(V_{GS}-V_T)(V_{GS}-V_T)",Magnification->0.8,"Preamble"->{"\\usepackage{siunitx}"}]},
		PlotLegends->Placed[
			LineLegend[
				(MaTeX["L="<>ToString[PaddedForm[#,{3,2}]]<>"\\si{um}",
					"Preamble"->{"\\usepackage{siunitx}"},
					Magnification->0.65]&)/@ ListL,
				LegendFunction->"Panel",
				LegendLayout -> {"ReversedColumn",3}
			],{Left,Top}]
	],
	Plot[
		Evaluate@Join[{IDx[(VGS/.s)[[1]],VDS],IDx[VDS,VDS]},Table[IDx[VGS,VDS],{VGS,{1.0,1.5,2.0,2.5,3.0}}]],
		{VDS,0,3.5},
		PlotRange->{-0.1,1.3},
		Frame->True,
		PlotPoints->1500,
		GridLines->Automatic,
		ImageSize->Automatic->300,
		PlotStyle->Join[{{Gray,Thickness[0.002]},{Black,Thickness[0.002]}},RBColorList],
		FrameTicks->{
			Table[{x,MaTeX[ToString[PaddedForm[x,{3,2}]],Magnification->0.5]},{x,0,3.5,0.25}],
			Table[{x,MaTeX[ToString[PaddedForm[x,{2,1}]],Magnification->0.5]},{x,0,1.2,0.1}]},
		FrameLabel->{
			MaTeX["V_{DS}~(\\si{V})",Magnification->0.8,"Preamble"->{"\\usepackage{siunitx}"}],
			MaTeX["I_D/k_n",Magnification->0.8,"Preamble"->{"\\usepackage{siunitx}"}]},
		Epilog -> {
			Text[MaTeX["V_{GS}=1.0\\si{V}",Magnification->0.6,"Preamble"->{"\\usepackage{siunitx}"}],{3.25,0.05}],
			Text[MaTeX["V_{GS}=1.5\\si{V}",Magnification->0.6,"Preamble"->{"\\usepackage{siunitx}"}],{3.25,0.25}],
			Text[MaTeX["V_{GS}=2.0\\si{V}",Magnification->0.6,"Preamble"->{"\\usepackage{siunitx}"}],{3.25,0.49}],
			Text[MaTeX["V_{GS}=2.5\\si{V}",Magnification->0.6,"Preamble"->{"\\usepackage{siunitx}"}],{3.25,0.75}],
			Text[MaTeX["V_{GS}=3.0\\si{V}",Magnification->0.6,"Preamble"->{"\\usepackage{siunitx}"}],{3.25,1.03}],
			Text["\:5939\:65ad\:9971\:548c",{2.63,0.65}],
			Text["\:901f\:5ea6\:9971\:548c",{1.10,0.65}]
		}
	],
	Plot[
		{IDx[VGS,VDS]/.{VDS->5}},
		{VGS,0,3.5},
		PlotRange->{-0.1,1.3},
		Frame->True,
		PlotPoints->2000,
		GridLines->Automatic,
		ImageSize->Automatic->300,
		PlotStyle->Black,
		FrameTicks->{
			Table[{x,MaTeX[ToString[PaddedForm[x,{3,2}]],Magnification->0.5]},{x,0,3.5,0.25}],
			Table[{x,MaTeX[ToString[PaddedForm[x,{2,1}]],Magnification->0.5]},{x,0,1.2,0.1}]},
		FrameLabel->{
			MaTeX["V_{GS}~(\\si{V})",Magnification->0.8,"Preamble"->{"\\usepackage{siunitx}"}],
			MaTeX["I_D/k_n",Magnification->0.8,"Preamble"->{"\\usepackage{siunitx}"}]},
		Epilog->{
			Text["\:5e73\:65b9\:5173\:7cfb",{0.75,0.18}],
			Text["\:7ebf\:6027\:5173\:7cfb",{2.60,1.07}]
		}
	],
	Plot[
		Evaluate@Join[{ID[VDS,VDS]},Table[ID[VGS,VDS],{VGS,{1.0,1.5,2.0,2.5,3.0}}]],
		{VDS,0,3.5},
		PlotRange->{-0.291,3.5+0.291},
		Frame->True,
		PlotPoints->1500,
		GridLines->Automatic,
		ImageSize->Automatic->300,
		PlotStyle->Join[{{Black,Thickness[0.002]}},RBColorList],
		FrameTicks->{
			Table[{x,MaTeX[ToString[PaddedForm[x,{3,2}]],Magnification->0.5]},{x,0,3.5,0.25}],
			Table[{x,MaTeX[ToString[PaddedForm[x,{2,1}]],Magnification->0.5]},{x,0,3.5,0.5}]},
		FrameLabel->{
			MaTeX["V_{DS}~(\\si{V})",Magnification->0.8,"Preamble"->{"\\usepackage{siunitx}"}],
			MaTeX["I_D/k_n",Magnification->0.8,"Preamble"->{"\\usepackage{siunitx}"}]},
		Epilog -> {
			Text[MaTeX["V_{GS}=1.0\\si{V}",Magnification->0.6,"Preamble"->{"\\usepackage{siunitx}"}],{3.25,0.02}],
			Text[MaTeX["V_{GS}=1.5\\si{V}",Magnification->0.6,"Preamble"->{"\\usepackage{siunitx}"}],{3.25,0.40}],
			Text[MaTeX["V_{GS}=2.0\\si{V}",Magnification->0.6,"Preamble"->{"\\usepackage{siunitx}"}],{3.25,1.05}],
			Text[MaTeX["V_{GS}=2.5\\si{V}",Magnification->0.6,"Preamble"->{"\\usepackage{siunitx}"}],{3.25,1.95}],
			Text[MaTeX["V_{GS}=3.0\\si{V}",Magnification->0.6,"Preamble"->{"\\usepackage{siunitx}"}],{3.25,3.15}],
			Text["\:5939\:65ad\:9971\:548c",{2.4,2.75}]
		}
	],
	Plot[
		{ID[VGS,VDS]/.{VDS->5}},
		{VGS,0,3.5},
		PlotRange->{-0.291,3.5+0.291},
		Frame->True,
		PlotPoints->2000,
		GridLines->Automatic,
		ImageSize->Automatic->300,
		PlotStyle->Black,
		FrameTicks->{
			Table[{x,MaTeX[ToString[PaddedForm[x,{3,2}]],Magnification->0.5]},{x,0,3.5,0.25}],
			Table[{x,MaTeX[ToString[PaddedForm[x,{3,2}]],Magnification->0.5]},{x,0,3.5,0.50}]},
		FrameLabel->{
			MaTeX["V_{GS}~(\\si{V})",Magnification->0.8,"Preamble"->{"\\usepackage{siunitx}"}],
			MaTeX["I_D/k_n",Magnification->0.8,"Preamble"->{"\\usepackage{siunitx}"}]},
		Epilog->{
			Text["\:5e73\:65b9\:5173\:7cfb",{2.30,1.07}]
		}
	],
	Plot3D[
		IDx[VGS,VDS],{VDS,0.0,3.5},{VGS,0.0,3.5},
		ViewPoint->{-100,-80,60},
		PlotPoints -> 50,
		MaxRecursion -> 5,
		Exclusions->{VDS==VGS,kappa[VGS-VT](VGS-VT)==VDS},
		ExclusionsStyle->{None,Black},
		ColorFunction->ColorData["DarkRainbow"],
		Ticks->{
			Table[{x,MaTeX[ToString[PaddedForm[x,{3,2}]],Magnification->0.5]},{x,0,3.5,0.5}],
			Table[{x,MaTeX[ToString[PaddedForm[x,{3,2}]],Magnification->0.5]},{x,0,3.5,0.5}],
			Table[{x,MaTeX[ToString[PaddedForm[x,{3,2}]],Magnification->0.5]},{x,0,1.2,0.2}]
		},
		AxesLabel->{
			MaTeX["V_{GS}~(\\si{V})",Magnification->0.8,"Preamble"->{"\\usepackage{siunitx}"}],
			MaTeX["V_{DS}~(\\si{V})",Magnification->0.8,"Preamble"->{"\\usepackage{siunitx}"}],
			MaTeX["I_D/k_n",Magnification->0.8,"Preamble"->{"\\usepackage{siunitx}"}]
		}
	],
	Plot3D[
		ID[VGS,VDS],{VDS,0.0,3.5},{VGS,0.0,3.5},
		ViewPoint->{-100,-80,60},
		PlotPoints -> 50,
		MaxRecursion -> 5,
		Exclusions->{VDS==VGS},
		ExclusionsStyle->{None,Black},
		ColorFunction->ColorData["DarkRainbow"],
		Ticks->{
			Table[{x,MaTeX[ToString[PaddedForm[x,{3,2}]],Magnification->0.5]},{x,0,3.5,0.5}],
			Table[{x,MaTeX[ToString[PaddedForm[x,{3,2}]],Magnification->0.5]},{x,0,3.5,0.5}],
			Table[{x,MaTeX[ToString[PaddedForm[x,{3,2}]],Magnification->0.5]},{x,0,4.5,0.5}]
		},
		AxesLabel->{
			MaTeX["V_{GS}~(\\si{V})",Magnification->0.8,"Preamble"->{"\\usepackage{siunitx}"}],
			MaTeX["V_{DS}~(\\si{V})",Magnification->0.8,"Preamble"->{"\\usepackage{siunitx}"}],
			MaTeX["I_D/k_n",Magnification->0.8,"Preamble"->{"\\usepackage{siunitx}"}]
		}
	]
}


figWithName=Transpose[{fig,nbPDFNameList[[1;;Length[fig]]]}];
ReleaseHold[(Hold[Export[#[[2]],#[[1]],ImageResolution->2000]]&)/@figWithName]


(*-*)
