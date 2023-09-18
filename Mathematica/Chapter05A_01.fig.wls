#!/usr/bin/env wolframscript
(* ::Package:: *)

ClearAll["Global`*"]


SetDirectory[NotebookDirectory[]];
Needs["MaTeX`"];


nbFileName="Chapter05A_01.fig.wls";
nbPDFName="build/"<>StringReplace[nbFileName,"wls"->"pdf"];
nbPDFNameList=(StringReplace[nbPDFName,".fig"->#<>".fig"]&)/@Alphabet[];
Export[nbPDFName,""]


xLaTeX=(MaTeX[
	#1,Magnification->#2,
	"Preamble"->{"\\usepackage{siunitx}","\\usepackage{physics}","\\usepackage{upgreek}"}])&;


xFigConfig={
	Frame->True,GridLines->Automatic,ImageSize->Automatic->300,
	FrameStyle->BlackFrame,PlotStyle->ColorData[10],BaseStyle->{FontFamily->"Latin Modern Roman",FontSize->8}};
xFigLabel=(FrameLabel->{xLaTeX[#1,0.8],xLaTeX[#2,0.8]})&;


xFig3DConfig={
	ViewPoint->{-100,-80,60},PlotPoints -> 50,MaxRecursion -> 5,
	ExclusionsStyle->{None,Black},ColorFunction->ColorData["DarkRainbow"],
	BaseStyle->{FontFamily->"Latin Modern Roman",FontSize->8}};
xFig3DLabel=(AxesLabel->{xLaTeX[#1,0.8],xLaTeX[#2,0.8],xLaTeX[#3,0.8]})&;


xVNT0=+0.43;
xVPT0=-0.40;

xVNDSAT=+0.63;
xVPDSAT=-1.00;

xNlambda=+0.06;
xPlambda=-0.10;

argNMOS={VT->xVNT0,VDSAT->xVNDSAT,lambda->0};
argPMOS={VT->-xVNT0,VDSAT->-xVNDSAT,lambda->0};
(*argPMOS={VT->xVPT0,VDSAT->xVPDSAT,lambda->xPlambda};*)

xVGT=Function[{VGS},VGS-VT];
xVMin=Function[{VDS,VGS},Min[VDS,VGT,VDSAT]/.{VGT->xVGT[VGS]}];
xVMax=Function[{VDS,VGS},Max[VDS,VGT,VDSAT]/.{VGT->xVGT[VGS]}];

xID=Function[{VDS,VGS},(VGT Vm-Vm^2/2)(1+lambda VDS)/.{VGT->xVGT[VGS]}];
xIDNMOS=Function[{VDS,VGS},Piecewise[{{0,VGT<=0},{0,VGT>0 && VDS<=0},{+xID[VDS,VGS],VGT>0 && VDS>0}}]/.{Vm->xVMin[VDS,VGS],VGT->xVGT[VGS]}/.argNMOS];
xIDPMOS=Function[{VDS,VGS},Piecewise[{{0,VGT>=0},{0,VGT<0 && VDS>=0},{-xID[VDS,VGS],VGT<0 && VDS<0}}]/.{Vm->xVMax[VDS,VGS],VGT->xVGT[VGS]}/.argPMOS];


xVDD=2.5;
xGND=0.0;

transNMOS={VDS->(Vout-xGND),VGS->(Vin-xGND)}
transPMOS={VDS->(Vout-xVDD),VGS->(Vin-xVDD)}

xIoutNMOS=Function[{Vout,Vin},Iout/.{Iout->+xIDNMOS[VDS,VGS]}/.transNMOS];
xIoutPMOS=Function[{Vout,Vin},Iout/.{Iout->-xIDPMOS[VDS,VGS]}/.transPMOS];


DensityPlot[ID/.{ID->xIoutNMOS[Vout,Vin]},{Vout,0,xVDD},{Vin,0,xVDD},
	Exclusions->({{VGT==0},{VDS==VDSAT,VGT>=VDSAT},{VDS==VGT,VDS<=VDSAT},{VGT==VDSAT,VDS>=VDSAT}}/.{VGT->xVGT[VGS]}/.transNMOS/.argNMOS),
	Evaluate@xFigLabel["V_{out}","V_{in}"],
	MeshFunctions->{#3&,#3&}]


Plot3D[ID/.{ID->xIoutNMOS[Vout,Vin]},{Vout,0,xVDD},{Vin,0,xVDD},
	Exclusions->({{VGT==0},{VDS==VDSAT,VGT>=VDSAT},{VDS==VGT,VDS<=VDSAT},{VGT==VDSAT,VDS>=VDSAT}}/.{VGT->xVGT[VGS]}/.transNMOS/.argNMOS),
	Evaluate@xFig3DConfig,Evaluate@xFig3DLabel["V_{out}","V_{in}","I_D/k_n"]]
	
Plot3D[ID/.{ID->xIoutPMOS[Vout,Vin]},{Vout,0,xVDD},{Vin,0,xVDD},
	Exclusions->({{VGT==0},{VDS==VDSAT,VGT<VDSAT}}/.{VGT->xVGT[VGS]}/.transPMOS/.argPMOS),
	Evaluate@xFig3DConfig,Evaluate@xFig3DLabel["V_{out}","V_{in}","I_D/k_n"]]



