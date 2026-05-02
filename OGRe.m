(* ::Package:: *)

(*
OGRe: An (O)bject-Oriented (G)eneral (Re)lativity Package for Mathematica
By Barak Shoshany (baraksh@gmail.com) (baraksh.com)
GitHub repository: https://github.com/bshoshany/OGRe
Copyright (c) 2021 Barak Shoshany. Licensed under the MIT license.

If you use this package in published research, please cite it as follows:

* Shoshany, B., (2021). OGRe: An Object-Oriented General Relativity Package for Mathematica. Journal of Open Source Software, 6(65), 3416, https://doi.org/10.21105/joss.03416

You can also use the following BibTeX entry:

@article{Shoshany2021_OGRe,
    author    = {Barak Shoshany},
    doi       = {10.21105/joss.03416},
    journal   = {Journal of Open Source Software},
    number    = {65},
    pages     = {3416},
    publisher = {The Open Journal},
    title     = {OGRe: An Object-Oriented General Relativity Package for Mathematica},
    url       = {https://doi.org/10.21105/joss.03416},
    volume    = {6},
    year      = {2021}
}
*)

BeginPackage["OGRe`"];

(* Check if the package has already been loaded, in case Get was used instead of Needs. *)

If[ValueQ[OGRe`Private`AlreadyLoaded],
    (* Unprotect and clear all symbols, so they can be redefined. Useful for debugging, or to reload the package after updating. *)
    Unprotect["OGRe`*"];
    Unprotect["OGRe`Private`*"];
    (* Keep the tensor objects and settings created so far during the session, so they don't get deleted when the package is reloaded. *)
    OGReTemp`TensorData = OGRe`Private`TensorData;
    ClearAll["OGRe`*"];
    ClearAll["OGRe`Private`*"];
    OGRe`Private`TensorData = OGReTemp`TensorData;
    Remove["OGReTemp`*"];
    OGRe`Private`AlreadyLoaded = True
    ,
    OGRe`Private`AlreadyLoaded = True;
    (* Initialize the symbol TensorData, which is used to store the data for the tensor objects, as well as user settings. This is done only on first load. *)
    OGRe`Private`TensorData = Association[];
];

(* A dirty trick to make the package's public modules globally visible without defining their usage messages in advance. I prefer to define each usage message at the same time as the module itself, so it can also serve as documentation for the code. *)

Null[{$CurveParam, TAddCoordTransformation, TCalc, TChangeDefaultCoords, TChangeDefaultIndices, TChangeID, TChangeSymbol, TCheckForUpdates, TChristoffel, TCite, TCovariantD, TDelete, TDim, TDocs, TEinstein, TExport, TExportAll, TGeodesicFromChristoffel, TGeodesicFromLagrangian, TGeodesicWithTimeParameter, TGetComponents, TImport, TImportAll, TInfo, TKretschmann, TLagrangian, TLineElement, TList, TMessage, TMetricDeterminant, TNewCoordinates, TNewMetric, TNewTensor, TNormSquared, TPartialD, TRank, TRicci, TRicciScalar, TRiemann, TSetAllowOverwrite, TSetAssumptions, TSetAutoUpdates, TSetCurveParameter, TSetExactSignChecks, TSetIndexLetters, TSetParallelization, TSetReservedSymbols, TSetSimplifyFunc, TShow, TSimplify, TTeXList, TTeXShow, TVolumeElement, TWeyl}];

Begin["`Private`"];

(* DO NOT change the format of the next line. It was used by TCheckForUpdates to detect the version of this file in pre-v2.0.0 versions. Changing it will break the automatic update mechanism. Only change the version number and date. *)

OGReVersion = "v2.0.0 (2026-05-02)";

(* If the global options LocalSymbol has been previously set, then it will have the Head Association. Otherwise it will have the Head LocalSymbol, and we create it now for later use. *)

If[Head[LocalSymbol["OGReGlobalOptions"]] =!= Association,
    LocalSymbol["OGReGlobalOptions"] = Association[];
];

(* Load the global options from the persistent storage. *)

OGReGlobalOptions = LocalSymbol["OGReGlobalOptions"];

(* Set the "OGReVersion" key to the current version of the package. *)

OGReGlobalOptions["OGReVersion"] = OGReVersion;

(* If the option "AutoUpdates" has not been set, or is not a boolean value, we set it now to the default value of True. *)

If[!KeyExistsQ[OGReGlobalOptions, "AutoUpdates"] || !BooleanQ[OGReGlobalOptions["AutoUpdates"]],
    OGReGlobalOptions["AutoUpdates"] = True;
];

(* If the option "AllowOverwrite" has not been set, or is not a boolean value, we set it now to the default value of False. *)

If[!KeyExistsQ[OGReGlobalOptions, "AllowOverwrite"] || !BooleanQ[OGReGlobalOptions["AllowOverwrite"]],
    OGReGlobalOptions["AllowOverwrite"] = False;
];

(* Save the global options to the persistent storage. *)

LocalSymbol["OGReGlobalOptions"] = OGReGlobalOptions;

(* Define a function to create a nicely-formatted usage message. *)

CreateUsageMessage[f_, msg_String : {}] :=
    Evaluate[f::usage] = ToString[TextCell[Row[Flatten[{List @@ StringReplace[msg, {"`" ~~ (x : Shortest[__]) ~~ "`" :> Style[x, Bold]}]}]]], StandardForm];

CreateUsageMessage[$CurveParam, "$CurveParam is used internally by OGRe as a placeholder for the curve parameter in Lagrangians and geodesic equations. It is not intended to be invoked by the user."]

(* Define defaults. *)

DefaultCurveParameter = "Global`\[Lambda]";

DefaultIndexLetters = "\[Mu]\[Nu]\[Rho]\[Sigma]\[Kappa]\[Lambda]\[Alpha]\[Beta]\[Gamma]\[Delta]\[CurlyEpsilon]\[Zeta]\[Eta]\[Theta]\[Iota]\[Xi]\[Pi]\[Tau]\[Phi]\[Chi]\[Psi]\[Omega]";

DefaultResultID = "Result";

DefaultSymbol = "\[DottedSquare]";

ReferenceOperator = "->";

(* ===================================================
   Public modules (accessible to the user) start here.
   =================================================== *)

CreateUsageMessage[TAddCoordTransformation, "TAddCoordTransformation[`sourceID`, `targetID`, `rules`] adds a transformation from the coordinate system `sourceID` to the coordinate system `targetID`.
`rules` must be a list of transformation rules. For example, {x \[Rule] r Sin[\[Theta]] Cos[\[Phi]], y \[Rule] r Sin[\[Theta]] Sin[\[Phi]], z \[Rule] r Cos[\[Theta]]} is a transformation from Cartesian to spherical coordinates."];

SyntaxInformation[TAddCoordTransformation] = {"ArgumentsPattern" -> {_, _, _}};

TAddCoordTransformation::ErrorRulesForm = "The transformation rules must be a list of rules of the form x \[Rule] y.";

TAddCoordTransformation::ErrorDifferentCoords = "The source and target coordinate systems must be different.";

TAddCoordTransformation::ErrorNotSameDim = "The source and target coordinate systems must be of the same dimension.";

TAddCoordTransformation::ErrorNotInvertible = "The coordinate transformation must be invertible.";

TAddCoordTransformation[sourceID_String, targetID_String, rules_List] :=
    Module[
        {allJacobians, dim, inverseJacobian, jacobian, newCoordSymbols, oldCoordSymbols}
        ,
        (* Check that the tensor object sourceID exists. *)
        CheckIfTensorExists[sourceID];
        (* Check that the rules are of the correct form. *)
        If[!AllTrue[rules, MatchQ[#, _ -> _]&],
            Message[TAddCoordTransformation::ErrorRulesForm];
            Abort[];
        ];
        (* Check that both tensor objects represents coordinate systems. *)
        CheckIfCoordinates[sourceID];
        CheckIfCoordinates[targetID];
        (* Check that the source and target coordinate systems are different. *)
        If[sourceID === targetID,
            Message[TAddCoordTransformation::ErrorDifferentCoords];
            Abort[];
        ];
        (* Check that the source and target coordinate systems are of the same dimension. *)
        If[Length[GetTensorData[sourceID]["Components"][{{1}, sourceID}]] != Length[GetTensorData[targetID]["Components"][{{1}, targetID}]],
            Message[TAddCoordTransformation::ErrorNotSameDim];
            Abort[];
        ];
        (* Calculate the Jacobian and inverse Jacobian, and store them in the tensor object of the source coordinates, to be used later whenever a coordinate transformation is performed. *)
        oldCoordSymbols = GetTensorData[sourceID]["Components"][{{1}, sourceID}];
        newCoordSymbols = GetTensorData[targetID]["Components"][{{1}, targetID}];
        dim = Length[oldCoordSymbols];
        jacobian = TensorSimplify[Table[D[oldCoordSymbols[[i]] /. rules, newCoordSymbols[[j]]], {i, 1, dim}, {j, 1, dim}]];
        inverseJacobian = Quiet[Check[Inverse[jacobian], "Error"]];
        If[inverseJacobian === "Error",
            Message[TAddCoordTransformation::ErrorNotInvertible];
            Abort[];
        ];
        inverseJacobian = TensorSimplify[inverseJacobian];
        allJacobians = Association["Jacobian" -> jacobian, "InverseJacobian" -> inverseJacobian];
        (* Add the transformation to the CoordTransformations key of the source object, or create it if it doesn't already exist. *)
        If[KeyExistsQ[GetTensorData[sourceID], "CoordTransformations"] && AssociationQ[GetTensorData[sourceID]["CoordTransformations"]],
            ChangeTensorKey[sourceID, "CoordTransformations", Append[GetTensorData[sourceID]["CoordTransformations"], targetID -> rules]]
            ,
            ChangeTensorKey[sourceID, "CoordTransformations", Association[targetID -> rules]];
        ];
        (* Add both Jacobians to the Jacobians key of the source object, or create it if it doesn't already exist. *)
        If[KeyExistsQ[GetTensorData[sourceID], "Jacobians"] && AssociationQ[GetTensorData[sourceID]["Jacobians"]],
            ChangeTensorKey[sourceID, "Jacobians", Append[GetTensorData[sourceID]["Jacobians"], targetID -> allJacobians]]
            ,
            ChangeTensorKey[sourceID, "Jacobians", Association[targetID -> allJacobians]];
        ];
        Return[sourceID];
    ];

CreateUsageMessage[TCalc, "TCalc[`formula`] calculates a tensor `formula`, which may involve any number of tensors in the format `ID`[`indices`], where `ID` is a tensor object and `indices` is a string representing the order of indices, along with any combination of the following operations:
\[Bullet] Addition: For example, \"A\"[\"\[Mu]\[Nu]\"] + \"B\"[\"\[Mu]\[Nu]\"].
\[Bullet] Contraction: For example, \"A\"[\"\[Mu]\[Lambda]\"] . \"B\"[\"\[Lambda]\[Nu]\"].
\[Bullet] Multiplication by scalar: For example, 2 * \"A\"[\"\[Mu]\[Nu]\"].
TCalc[`targetID`[`targetIndices`], `formula`, `symbol`] calculates a tensor `formula` and stores the result in a new tensor object.
`targetID` specifies the ID of the tensor object in which to store the result. If omitted, the ID \"" <> DefaultResultID <> "\" will be used.
`targetIndices` specifies the order of indices of the resulting tensor. The indices must be a permutation of the free indices of `formula`. If omitted, the indices are assumed to be in the same order as they appear in `formula`.
`symbol` specifies the symbol to use for the resulting tensor. If omitted, the placeholder symbol " <> DefaultSymbol <> " will be used."];

SyntaxInformation[TCalc] = {"ArgumentsPattern" -> {_, _., _.}};

TCalc::ErrorIndices = "The LHS index specification \"`1`\" and the RHS index specification \"`2`\" must be the same up to permutation.";

TCalc::ErrorCoordinates = "Coordinate systems cannot be used as tensors in TCalc.";

TCalc::ErrorResult = "Invalid tensor expression obtained: `1`. Please check that the tensor expression you entered contains only tensor references of the form \"ID\"[\"indices\"] combined using addition, contraction (dot product), or multiplication by scalar."

TCalc[rhsExpression_, symbol_String:DefaultSymbol] :=
    TCalc[DefaultResultID[""], rhsExpression, symbol];

TCalc[lhsTensorID_String, rhsExpression_, symbol_String:DefaultSymbol] :=
    TCalc[lhsTensorID[""], rhsExpression, symbol];

TCalc[lhsTensorID_String[lhsIndices_String], rhsExpression_, symbol_String:DefaultSymbol] :=
    Module[
        {allVars, components, lhsVars, newComponents, newIndices, result, resultID, resultIndices, rhsVars, rules, useCoords, useIndices}
        ,
        (* Check that the tensor lhsTensorID doesn't already exist, but only if it's not the default ID. *)
        If[lhsTensorID =!= DefaultResultID,
            CheckIDHasNoReference[lhsTensorID];
            CheckIfOverwriting[lhsTensorID];
        ];
        (* Check that the tensor expression does not use coordinate systems. *)
        Scan[
            If[TensorIDExistsQ[#] && GetTensorData[#]["Role"] === "Coordinates",
                Message[TCalc::ErrorCoordinates];
                Abort[];
            ]&
            ,
            DeleteDuplicates[Cases[rhsExpression, tensorID_String[_String] :> tensorID, {0, Infinity}]]
        ];
        (* Define the rules for computing tensor formulas. *)
        rules = {tensorID_String[indices_String /; !DuplicateFreeQ[Characters[indices]]] :> TensorTrace[tensorID[indices]], firstID_String[firstIndices_String] + secondID_String[secondIndices_String] :> AddTensors[TensorTrace[firstID[firstIndices]], TensorTrace[secondID[secondIndices]]], scalar_ * tensorID_String[indices_String] :> TensorByScalar[TensorTrace[tensorID[indices]], scalar], firstID_String[firstIndices_String] . secondID_String[secondIndices_String] :> ContractTensors[TensorTrace[firstID[firstIndices]], TensorTrace[secondID[secondIndices]]], TPartialD[derivativeIndex_String] . tensorID_String[tensorIndices_String] :> DivOrGrad[derivativeIndex, TensorTrace[tensorID[tensorIndices]]], TCovariantD[derivativeIndex_String] . tensorID_String[tensorIndices_String] :> CovariantDivOrGrad[derivativeIndex, TensorTrace[tensorID[tensorIndices]]]};
        (* Repeatedly replace tensor operations with their results until we reach a fixed point. *)
        result = ReplaceRepeated[rhsExpression, rules];
        (* Check that the result is valid, i.e. of the form "tensorID"["indices"]. *)
        If[!MatchQ[result, _String[_String]],
            Message[TCalc::ErrorResult, result];
            (* Clear the temporary tensors that were created for the purpose of the calculation. *)
            ClearTemp[];
            Abort[];
        ];
        resultID = result[[0]];
        resultIndices = result[[1]];
        (* Get the indices, coordinates, and components of the result. *)
        useIndices = GetTensorData[resultID]["DefaultIndices"];
        useCoords = GetTensorData[resultID]["DefaultCoords"];
        components = GetTensorData[resultID]["Components"][{useIndices, useCoords}];
        (* Simplify the components. *)
        components = TensorSimplify[components];
        If[lhsIndices === "",
            (* Either a scalar, or no rearranging of indices is desired. Store the result directly in a new tensor object. *)
            SetTensorID[lhsTensorID, Association["Components" -> Association[{useIndices, useCoords} -> components], "DefaultCoords" -> useCoords, "DefaultIndices" -> useIndices, "Metric" -> GetTensorData[resultID]["Metric"], "Role" -> "Calculated", "Symbol" -> symbol]]
            ,
            (* Check that the LHS and RHS index specifications are the same up to permutation. *)
            If[Sort[Characters[lhsIndices]] != Sort[Characters[resultIndices]],
                Message[TCalc::ErrorIndices, lhsIndices, resultIndices];
                (* Clear the temporary tensors that were created for the purpose of the calculation. *)
                ClearTemp[];
                Abort[];
            ];
            (* Collect the variables to be used for rearranging the indices. Both lhsVars and rhsVars will be the same set of variables, but potentially in a different order. *)
            allVars = Association[];
            Scan[(allVars[#] = Unique["var"])&, Characters[lhsIndices]];
            lhsVars = allVars[#]& /@ Characters[lhsIndices];
            rhsVars = allVars[#]& /@ Characters[resultIndices];
            (* Rearrange the components and indices to allow for a LHS with a different index order than the RHS. *)
            newComponents = Table[components[[Sequence @@ rhsVars]], Evaluate[Sequence @@ ({#, 1, Length[components]}& /@ lhsVars)]];
            newIndices = Table[useIndices[[StringPosition[resultIndices, Characters[lhsIndices][[n]]][[1, 1]]]], {n, 1, StringLength[lhsIndices]}];
            (* Store the result in a new tensor object. *)
            SetTensorID[lhsTensorID, Association["Components" -> Association[{newIndices, useCoords} -> newComponents], "DefaultCoords" -> useCoords, "DefaultIndices" -> newIndices, "Metric" -> GetTensorData[resultID]["Metric"], "Role" -> "Calculated", "Symbol" -> symbol]];
        ];
        (* Clear the temporary tensors that were created for the purpose of the calculation. *)
        ClearTemp[];
        (* Update all code completions. *)
        UpdateCodeCompletions[];
        Return[lhsTensorID];
    ];

CreateUsageMessage[TChristoffel, "TChristoffel[`metricID`] returns a reference to the Christoffel symbols (the coefficients of the Levi-Civita connection) calculated from the metric `metricID`. If the Christoffel symbols have not been calculated yet, they will be calculated and cached."];

SyntaxInformation[TChristoffel] = {"ArgumentsPattern" -> {_}};

TChristoffel[metricID_String] :=
    EnsureMetricCache[metricID, "Christoffel"];

CreateUsageMessage[TRiemann, "TRiemann[`metricID`] returns a reference to the Riemann tensor calculated from the metric `metricID`. If the Riemann tensor has not been calculated yet, it will be calculated and cached."];

SyntaxInformation[TRiemann] = {"ArgumentsPattern" -> {_}};

TRiemann[metricID_String] :=
    EnsureMetricCache[metricID, "Riemann"];

CreateUsageMessage[TWeyl, "TWeyl[`metricID`] returns a reference to the Weyl tensor calculated from the metric `metricID`. If the Weyl tensor has not been calculated yet, it will be calculated and cached."];

SyntaxInformation[TWeyl] = {"ArgumentsPattern" -> {_}};

TWeyl[metricID_String] :=
    EnsureMetricCache[metricID, "Weyl"];

CreateUsageMessage[TRicci, "TRicci[`metricID`] returns a reference to the Ricci tensor calculated from the metric `metricID`. If the Ricci tensor has not been calculated yet, it will be calculated and cached."];

SyntaxInformation[TRicci] = {"ArgumentsPattern" -> {_}};

TRicci[metricID_String] :=
    EnsureMetricCache[metricID, "Ricci"];

CreateUsageMessage[TRicciScalar, "TRicciScalar[`metricID`] returns a reference to the Ricci scalar calculated from the metric `metricID`. If the Ricci scalar has not been calculated yet, it will be calculated and cached."];

SyntaxInformation[TRicciScalar] = {"ArgumentsPattern" -> {_}};

TRicciScalar[metricID_String] :=
    EnsureMetricCache[metricID, "RicciScalar"];

CreateUsageMessage[TKretschmann, "TKretschmann[`metricID`] returns a reference to the Kretschmann scalar calculated from the metric `metricID`. If the Kretschmann scalar has not been calculated yet, it will be calculated and cached."];

SyntaxInformation[TKretschmann] = {"ArgumentsPattern" -> {_}};

TKretschmann[metricID_String] :=
    EnsureMetricCache[metricID, "Kretschmann"];

CreateUsageMessage[TEinstein, "TEinstein[`metricID`] returns a reference to the Einstein tensor calculated from the metric `metricID`. If the Einstein tensor has not been calculated yet, it will be calculated and cached."];

SyntaxInformation[TEinstein] = {"ArgumentsPattern" -> {_}};

TEinstein[metricID_String] :=
    EnsureMetricCache[metricID, "Einstein"];

CreateUsageMessage[TLagrangian, "TLagrangian[`metricID`] returns a reference to the curve Lagrangian of the metric `metricID`, defined as the norm-squared of the tangent to the curve. If the Lagrangian has not been calculated yet, it will be calculated and cached.
Taking the square root of (the absolute value of) the Lagrangian yields the integrand of the curve length functional. Varying the Lagrangian using the Euler-Lagrange equations yields the geodesic equations.
The result will be given in terms of the coordinate symbols as functions of the curve parameter and their derivatives with respect to the curve parameter. The curve parameter can be selected using TSetCurveParameter[]."];

SyntaxInformation[TLagrangian] = {"ArgumentsPattern" -> {_}};

TLagrangian[metricID_String] :=
    EnsureMetricCache[metricID, "Lagrangian"];

CreateUsageMessage[TGeodesicFromChristoffel, "TGeodesicFromChristoffel[`metricID`] returns a reference to the geodesic equations obtained using the Christoffel symbols of the metric `metricID`. Equating the components to zero will yield the full system of geodesic equations.
The result will be given in terms of the coordinate symbols as functions of the curve parameter and their derivatives with respect to the curve parameter. The curve parameter can be selected using TSetCurveParameter[]."];

SyntaxInformation[TGeodesicFromChristoffel] = {"ArgumentsPattern" -> {_}};

TGeodesicFromChristoffel[metricID_String] :=
    EnsureMetricCache[metricID, "GeodesicFromChristoffel"];

CreateUsageMessage[TGeodesicFromLagrangian, "TGeodesicFromLagrangian[`metricID`] returns a reference to the geodesic equations obtained using the curve Lagrangian of the metric `metricID`. Equating the components to zero will yield the full system of geodesic equations.
Derivatives with respect to the curve parameter in the Euler-Lagrange equation will be left unevaluated using Inactive[], which can sometimes help solve the geodesic equations by inspection. Use Activate[] to evaluate the derivatives.
The result will be given in terms of the coordinate symbols as functions of the curve parameter and their derivatives with respect to the curve parameter. The curve parameter can be selected using TSetCurveParameter[]."];

SyntaxInformation[TGeodesicFromLagrangian] = {"ArgumentsPattern" -> {_}};

TGeodesicFromLagrangian[metricID_String] :=
    EnsureMetricCache[metricID, "GeodesicFromLagrangian"];

CreateUsageMessage[TGeodesicWithTimeParameter, "TGeodesicWithTimeParameter[`metricID`] returns a reference to the geodesic equations obtained using the first coordinate as the parameter. Equating the components to zero will yield the full system of geodesic equations.
The result will be given in terms of the spatial coordinate symbols as functions of the time coordinate and their derivatives with respect to time. The first coordinate of the coordinate system will be assumed to be the time coordinate, even if its symbol is not t."];

SyntaxInformation[TGeodesicWithTimeParameter] = {"ArgumentsPattern" -> {_}};

TGeodesicWithTimeParameter[metricID_String] :=
    EnsureMetricCache[metricID, "GeodesicWithTimeParameter"];

CreateUsageMessage[TNormSquared, "TNormSquared[`tensorID`] calculates the norm-squared of the tensor `tensorID` with respect to its metric, that is, the tensor contracted with itself in all indices. For example, for a vector v^a the norm-squared will be v^a v_a and for a rank-2 tensor T^ab the result will be T^ab T_ab.
TNormSquared[`tensorID`, `coordinatesID`] calculates the norm-squared in the coordinate system `coordinatesID`."];

SyntaxInformation[TNormSquared] = {"ArgumentsPattern" -> {_, _.}};

TNormSquared[tensorID_String, coordinatesID_String:"_UseDefault_"] :=
    Module[
        {components, normSquared, rank, secondComponents, useCoords, useIndices, vars}
        ,
        (* Check that tensorID exists. *)
        CheckIfTensorExists[tensorID];
        (* Determine which coordinate system to use. *)
        If[coordinatesID === "_UseDefault_",
            useCoords = GetTensorData[tensorID]["DefaultCoords"]
            ,
            CheckIfTensorExists[coordinatesID];
            CheckIfCoordinates[coordinatesID];
            useCoords = coordinatesID;
        ];
        (* Get the tensor's rank and components. *)
        useIndices = GetTensorData[tensorID]["DefaultIndices"];
        rank = Length[useIndices];
        components = AddRepresentation[tensorID, useIndices, useCoords];
        If[rank == 0,
            normSquared = components[[1]] ^ 2
            ,
            secondComponents = AddRepresentation[tensorID, -useIndices, useCoords];
            vars = Unique[Table["var", {rank}]];
            normSquared = Sum[components[[Sequence @@ vars]] * secondComponents[[Sequence @@ vars]], Evaluate[Sequence @@ ({#, 1, Length[components]}& /@ vars)]];
        ];
        (* If this is a tensor that uses a curve parameter, replace the placeholder with the selected curve parameter. *)
        If[CurveParameterTensorQ[tensorID],
            normSquared = ReplaceAll[normSquared, $CurveParam -> Symbol[TensorData[Options]["CurveParameter"]]];
        ];
        Return[TensorSimplify[normSquared]];
    ];

CreateUsageMessage[TRank, "TRank[`ID`] returns the rank of the tensor object `ID`, that is, the number of indices."];

SyntaxInformation[TRank] = {"ArgumentsPattern" -> {_}};

TRank[tensorID_String] :=
    (
        CheckIfTensorExists[tensorID];
        Return[Length[GetTensorData[tensorID]["DefaultIndices"]]];
    );

CreateUsageMessage[TChangeDefaultCoords, "TChangeDefaultCoords[`tensorID`, `coordinatesID`] changes the default coordinate system of the tensor object `tensorID` to `coordinatesID`."];

SyntaxInformation[TChangeDefaultCoords] = {"ArgumentsPattern" -> {_, _}};

TChangeDefaultCoords::ErrorCoordTensor = "Cannot change the default coordinate system for a tensor object representing a coordinate system."

TChangeDefaultCoords[tensorID_String, coordinatesID_String] :=
    (
        CheckIfTensorExists[tensorID];
        CheckIfUserTensor[tensorID];
        CheckIfTensorExists[coordinatesID];
        (* Check that the tensor object tensorID does not itself represents a coordinate system. *)
        If[GetTensorData[tensorID]["Role"] === "Coordinates",
            Message[TChangeDefaultCoords::ErrorCoordTensor];
            Abort[];
        ];
        (* Check that the tensor object coordinatesID represents a coordinate system. *)
        CheckIfCoordinates[coordinatesID];
        (* Add a representation to the tensor in the new coordinate system with the default indices, if it doesn't already exist. *)
        AddRepresentation[tensorID, GetTensorData[tensorID]["DefaultIndices"], coordinatesID];
        (* Change the DefaultCoords key. *)
        ChangeTensorKey[tensorID, "DefaultCoords", coordinatesID];
        Return[tensorID];
    );

CreateUsageMessage[TChangeDefaultIndices, "TChangeDefaultIndices[`ID`, `indices`] changes the default index configuration of the tensor object `ID` to `indices`.
`indices` must be a list of the form {\[PlusMinus]1, \[PlusMinus]1, ...}, where +1 corresponds to an upper index and -1 corresponds to a lower index."];

SyntaxInformation[TChangeDefaultIndices] = {"ArgumentsPattern" -> {_, _}};

TChangeDefaultIndices::ErrorCoords = "Cannot change the default index configuration for a tensor object representing a coordinate system."

TChangeDefaultIndices::ErrorMetric = "Cannot change the default index configuration for a tensor object representing a metric."

TChangeDefaultIndices[tensorID_String, indices_List] :=
    (
        CheckIfTensorExists[tensorID];
        CheckIfUserTensor[tensorID];
        (* Check that the tensor object does not represent a coordinate system. *)
        If[GetTensorData[tensorID]["Role"] === "Coordinates",
            Message[TChangeDefaultIndices::ErrorCoords];
            Abort[];
        ];
        (* Check that the tensor object does not represent a metric. *)
        If[GetTensorData[tensorID]["Role"] === "Metric",
            Message[TChangeDefaultIndices::ErrorMetric];
            Abort[];
        ];
        (* Check that the list of indices is of the correct form and has the correct rank. *)
        CheckIndicesForm[indices];
        CheckIndicesRank[indices, tensorID];
        (* Add a representation to the tensor with the new indices in the default coordinate system, if it doesn't already exist. *)
        AddRepresentation[tensorID, indices, GetTensorData[tensorID]["DefaultCoords"]];
        (* Change the DefaultIndices key. *)
        ChangeTensorKey[tensorID, "DefaultIndices", indices];
        Return[tensorID];
    );

CreateUsageMessage[TChangeID, "TChangeID[`oldID`, `newID`] changes the ID of the tensor object `oldID` to `newID`.
If the tensor is a metric or a coordinate system, all currently defined tensors will be scanned, and any references to `oldID` will be replaced with `newID`."];

SyntaxInformation[TChangeID] = {"ArgumentsPattern" -> {_, _}};

TChangeID[oldID_String, newID_String] :=
    Module[{oldData},
        CheckIfTensorExists[oldID];
        CheckIfUserTensor[oldID];
        CheckIDHasNoReference[newID];
        (* Check that the tensor object newID doesn't already exist. *)
        CheckIfOverwriting[newID];
        oldData = GetTensorData[oldID];
        (* Copy the old tensor data to the new ID and then remove the old ID. *)
        SetTensorID[newID, oldData];
        RemoveTensorID[oldID];
        (* If the tensor is a metric, replace all references to it. *)
        If[oldData["Role"] === "Metric",
            Scan[
                If[GetTensorData[#]["Metric"] === oldID,
                    ChangeTensorKey[#, "Metric", newID]
                ]&
                ,
                AllTensorIDs[]
            ];
        ];
        (* If the tensor is a coordinate system, replace all references to it. *)
        If[oldData["Role"] === "Coordinates",
            Scan[ChangeCoordinateID[#, oldID, newID]&, AllTensorIDs[]];
        ];
        (* Update all code completions. *)
        UpdateCodeCompletions[];
        Return[newID];
    ];

CreateUsageMessage[TChangeSymbol, "TChangeSymbol[`ID`, `symbol`] changes the symbol of the tensor object `ID` to `symbol`."];

SyntaxInformation[TChangeSymbol] = {"ArgumentsPattern" -> {_, _}};

TChangeSymbol[tensorID_String, symbol_String] :=
    (
        CheckIfTensorExists[tensorID];
        CheckIfUserTensor[tensorID];
        ChangeTensorKey[tensorID, "Symbol", symbol];
        Return[tensorID];
    );

CreateUsageMessage[TCheckForUpdates, "TCheckForUpdates[] checks the GitHub repository for new versions of this package. If a new version is available, the user will be given the option to download or install it."];

SyntaxInformation[TCheckForUpdates] = {"ArgumentsPattern" -> {}};

TCheckForUpdates[] :=
    Module[{errorMessage, latestRelease, newVersion, newVersionQ, releaseFileURL, releaseNotes, releaseURL, toPrint},
        errorMessage = Row[{"Error: Failed to check for updates. Please visit ", Hyperlink["https://github.com/bshoshany/OGRe/releases"], " to check manually."}];
        OGRePrint["Checking GitHub repository for updates..."];
        latestRelease = LatestReleaseData[];
        If[latestRelease === $Failed,
            OGRePrint[errorMessage];
            Abort[];
        ];
        newVersion = latestRelease["tag_name"];
        releaseNotes = latestRelease["body"];
        releaseURL = latestRelease["html_url"];
        newVersionQ = NewVersionQ[newVersion];
        If[newVersionQ === $Failed,
            OGRePrint[errorMessage];
            Abort[];
        ];
        If[!newVersionQ,
            OGRePrint["You have the latest version of the package."]
            ,
            releaseFileURL = "https://raw.githubusercontent.com/bshoshany/OGRe/" <> newVersion <> "/OGRe.m";
            toPrint = {Row[{"A new version of the package is available: ", Style[newVersion, Bold]}]};
            AppendTo[toPrint, Row[{"\[Bullet] ", Hyperlink["View release notes on GitHub.", releaseURL]}]];
            AppendTo[
                toPrint
                ,
                Row[
                    {
                        "\[Bullet] "
                        ,
                        With[{notes = releaseNotes},
                            CreateButton["View release notes in this notebook.", ShowReleaseNotes[notes]]
                        ]
                    }
                ]
            ];
            AppendTo[
                toPrint
                ,
                Row[
                    {
                        "\[Bullet] "
                        ,
                        With[{url = releaseFileURL},
                            CreateButton["Reload new version directly from GitHub without downloading it.", Get[url]]
                        ]
                    }
                ]
            ];
            (* If the notebook is an Untitled notebook, meaning it is not an actual file in the file system, then NotebookDirectory[] will return $Failed and issue the error message NotebookDirectory::nosv. Otherwise, show an option to download the notebook to the current notebook directory. *)
            Off[NotebookDirectory::nosv];
            If[NotebookDirectory[] =!= $Failed,
                AppendTo[
                    toPrint
                    ,
                    Row[
                        {
                            "\[Bullet] "
                            ,
                            With[{url = releaseFileURL, path = FileNameJoin[{NotebookDirectory[], "OGRe.m"}]},
                                CreateButton[
                                    "Download new version to " <> path <> " and reload the package."
                                    ,
                                    URLDownload[url, path];
                                    OGRePrint["Downloaded! Reloading..."];
                                    Get[path]
                                ]
                            ]
                        }
                    ]
                ];
            ];
            On[NotebookDirectory::nosv];
            AppendTo[
                toPrint
                ,
                Row[
                    {
                        "\[Bullet] "
                        ,
                        With[{url = releaseFileURL, path = FileNameJoin[{$UserBaseDirectory, "Applications", "OGRe.m"}]},
                            CreateButton[
                                "Install new version to " <> path <> " and reload the package."
                                ,
                                URLDownload[url, path];
                                OGRePrint["Installed! Reloading..."];
                                Get[path]
                            ]
                        ]
                    }
                ]
            ];
            OGRePrint[Column[toPrint]];
        ];
    ];

CreateUsageMessage[TCite, "TCite[] displays information on how to cite this package in published research. Thank you for citing my work! :)"];

SyntaxInformation[TCite] = {"ArgumentsPattern" -> {}};

TCite[] :=
    CellPrint[{Cell["If you use this package in published research, please cite it as follows:", "Text"], Cell["Shoshany, B., (2021). OGRe: An Object-Oriented General Relativity Package for Mathematica. Journal of Open Source Software, 6(65), 3416, https://doi.org/10.21105/joss.03416", "Item"], Cell["You can also use the following BibTeX entry:", "Text"], Cell["@article{Shoshany2021_OGRe,
    author    = {Barak Shoshany},
    doi       = {10.21105/joss.03416},
    journal   = {Journal of Open Source Software},
    number    = {65},
    pages     = {3416},
    publisher = {The Open Journal},
    title     = {{OGRe}: An Object-Oriented General Relativity Package for Mathematica},
    url       = {https://doi.org/10.21105/joss.03416},
    volume    = {6},
    year      = {2021}
}", "Program"], Cell["Thank you for citing my work! :)", "Text"]}];

CreateUsageMessage[TDelete, "TDelete[`ID`] permanently deletes the tensor object `ID`. If the tensor is a metric or coordinate system, it cannot be deleted unless all tensors referring to it have been deleted first."];

SyntaxInformation[TDelete] = {"ArgumentsPattern" -> {_}};

TDelete::ErrorMetric = "The metric \"`1`\" cannot be deleted, as it has been used to define the tensor \"`2`\". To delete the metric, first delete \"`2`\" and any other tensors defined using this metric.";

TDelete::ErrorCoords = "The coordinate system \"`1`\" cannot be deleted, as it is the default coordinate system of the tensor \"`2`\". To delete the coordinate system, first change the default coordinate system of \"`2`\" and any other relevant tensors.";

TDelete[tensorID_String] :=
    (
        CheckIfTensorExists[tensorID];
        (* If this tensor represents a metric, check that no tensors are defined using this metric. *)
        If[GetTensorData[tensorID]["Role"] === "Metric",
            Scan[
                If[GetTensorData[#]["Metric"] === tensorID && # =!= tensorID,
                    Message[TDelete::ErrorMetric, DisplayTensorID[tensorID], DisplayTensorID[#]];
                    Abort[];
                ]&
                ,
                TopLevelTensorIDs[]
            ];
        ];
        (* If this tensor represents a coordinate system, check that no tensors are using it as their default coordinate system. *)
        If[GetTensorData[tensorID]["Role"] === "Coordinates",
            Scan[
                If[GetTensorData[#]["DefaultCoords"] === tensorID && # =!= tensorID,
                    Message[TDelete::ErrorCoords, DisplayTensorID[tensorID], DisplayTensorID[#]];
                    Abort[];
                ]&
                ,
                TopLevelTensorIDs[]
            ];
        ];
        RemoveTensorID[tensorID];
        (* Update all code completions. *)
        UpdateCodeCompletions[];
    );

CreateUsageMessage[TDim, "TDim[`ID`] returns the number of dimensions in the manifold that the tensor object `ID` is defined in, that is, the number of coordinates."];

SyntaxInformation[TDim] = {"ArgumentsPattern" -> {_}};

TDim[tensorID_String] :=
    Module[{coordinatesID},
        CheckIfTensorExists[tensorID];
        coordinatesID = GetTensorData[tensorID]["DefaultCoords"];
        Return[Length[GetTensorData[coordinatesID]["Components"][{{1}, coordinatesID}]]];
    ];

CreateUsageMessage[TDocs, "TDocs[] opens the Mathematica notebook OGRe_Documentation.nb from the GitHub repository, which contains the full documentation for the package."];

SyntaxInformation[TDocs] = {"ArgumentsPattern" -> {}};

TDocs[] :=
    Module[{documentationURL, versionTag},
        versionTag = VersionNumberFromString[OGReVersion]["Tag"];
        documentationURL = "https://raw.githubusercontent.com/bshoshany/OGRe/" <> versionTag <> "/OGRe_Documentation.nb";
        If[NotebookOpen[documentationURL] === $Failed,
            OGRePrint[Row[{"Error: Failed to load the documentation. Please visit ", Hyperlink[ReleaseURLFromTag[versionTag]], " to download it manually."}]]
            ,
            OGRePrint["Successfully loaded the documentation from GitHub."];
        ];
    ];

CreateUsageMessage[TExport, "TExport[`ID`] exports the raw tensor data for the tensor object `ID` as an Association."];

SyntaxInformation[TExport] = {"ArgumentsPattern" -> {_}};

TExport[tensorID_String] :=
    (
        CheckIfTensorExists[tensorID];
        Return[Association[tensorID -> Append[GetTensorData[tensorID], "OGReVersion" -> OGReVersion]]];
    );

CreateUsageMessage[TExportAll, "TExportAll[] exports the raw tensor data for all tensors defined in the current session as an Association.
TExportAll[`filename`] exports the data to `filename`. If a file extension is not provided, the extension `.m` will be appended, unless the file name explicitly ends with a dot. If a full path is not given, the file will be created in the current working directory, as given by Directory[]. This directory can be changed using SetDirectory[]. Note that the file will be overwritten if it already exists."];

SyntaxInformation[TExportAll] = {"ArgumentsPattern" -> {_.}};

TExportAll::ErrorFileWrite = "The file `1` cannot be opened for writing.";

TExportAll[] :=
    ExportTensorData[];

TExportAll[filename_String] :=
    Module[{path, stream},
        If[FileExtension[filename] === "" && StringTake[filename, -1] =!= ".",
            path = filename <> ".m";
            ,
            path = filename;
        ];
        stream = OpenWrite[path];
        If[stream === $Failed,
            Message[TExportAll::ErrorFileWrite, path];
            Abort[];
        ];
        Write[stream, ExportTensorData[]];
        Close[stream];
        OGRePrint[
            "Exported all tensor data to "
            ,
            With[{p = AbsoluteFileName[path]},
                CreateButton[p, NotebookOpen[p]]
            ]
            ,
            "."
        ];
    ];

CreateUsageMessage[TGetComponents, "TGetComponents[`ID`] returns the components of the tensor object `ID` in its default index configuration and coordinate system.
TGetComponents[`ID`, `indices`] returns the components in the index configuration `indices`, which should be a list of the form {\[PlusMinus]1, \[PlusMinus]1, ...}, where +1 corresponds to an upper index and -1 corresponds to a lower index.
TGetComponents[`ID`, `coordinatesID`] returns the components in the coordinate system `coordinatesID`.
TGetComponents[`ID`, `rules`] applies ReplaceAll[`rules`] to each of the tensor's elements, and then automatically simplifies them, before they are returned. `rules` can be either a single rule of the form `lhs \[Rule] rhs` or a list of rules.
TGetComponents[`ID`, `function`] maps `function` to each of the tensor's elements, and then automatically simplifies them, before they are returned.
TGetComponents[`ID`, `indices`, `coordinatesID`, `rules`, `function`] does all of the above; any of the arguments can be omitted. If both `rules` and `function` are given, the rules will be applied before the function.
If `indices` and/or `coordinatesID` are omitted, the default values are used, and a message will let the user know which representation the components are given in, to avoid confusion."];

SyntaxInformation[TGetComponents] = {"ArgumentsPattern" -> {_, _., _., _., _.}};

TGetComponents::UsingDefault = "Using `1`.";

TGetComponents[tensorID_String, indices : {(1 | -1)...} : {"_UseDefault_"}, coordinatesID_String:"_UseDefault_", replace : _Rule | {_Rule..} : {}, function_:Identity] /; (!ListQ[function] && !StringQ[function]) :=
    Module[
        {components, useCoords, useIndices, usingMessage = ""}
        ,
        (* Check that the tensor object tensorID exists. *)
        CheckIfTensorExists[tensorID];
        (* If specific indices are not given, use the tensor's default indices. *)
        If[indices === {"_UseDefault_"},
            useIndices = GetTensorData[tensorID]["DefaultIndices"];
            (* Make sure the user knows which representation they got. *)
            usingMessage = "the default index configuration " <> ToString[useIndices]
            ,
            (* Check that the list of indices is of the correct form and has the correct rank. *)
            CheckIndicesForm[indices];
            CheckIndicesRank[indices, tensorID];
            useIndices = indices;
        ];
        (* If a specific coordinate system is not given, use the tensor's default coordinate system. *)
        If[coordinatesID === "_UseDefault_",
            useCoords = GetTensorData[tensorID]["DefaultCoords"];
            (* Make sure the user knows which representation they got. *)
            If[usingMessage =!= "",
                usingMessage = usingMessage <> " and "
            ];
            usingMessage = usingMessage <> "the default coordinate system \"" <> useCoords <> "\""
            ,
            (* Check that the tensor object coordinatesID exists and represents a coordinate system. *)
            CheckIfTensorExists[coordinatesID];
            CheckIfCoordinates[coordinatesID];
            useCoords = coordinatesID;
        ];
        If[usingMessage =!= "",
            Message[TGetComponents::UsingDefault, usingMessage];
        ];
        (* Get the components of the tensor in the desired representation. They will be calculated if the representation is not yet stored in the tensor object. *)
        components = AddRepresentation[tensorID, useIndices, useCoords];
        (* If this is a tensor that uses a curve parameter, replace any instance of the curve parameter placeholder with the selected curve parameter. *)
        If[CurveParameterTensorQ[tensorID],
            components = ReplaceAll[components, $CurveParam -> Symbol[TensorData[Options]["CurveParameter"]]];
        ];
        (* If desired, map a function to the components and/or apply replacement rules to them, then simplify. *)
        If[function =!= Identity || replace =!= {},
            (* Apply the optional replacement rules to the components. *)
            If[replace =!= {},
                components = ReplaceAll[components, replace];
            ];
            (* Map the optional function to the components. *)
            If[function =!= Identity,
                components = Map[function, components, {ArrayDepth[components]}];
            ];
            (* Simplify the components in the end. *)
            components = TensorSimplify[components];
        ];
        Return[components];
    ];

CreateUsageMessage[TImport, "TImport[`data`] imports a tensor that has been exported using TExport[]."];

SyntaxInformation[TImport] = {"ArgumentsPattern" -> {_}};

TImport::ErrorMissingCoordinates = "Cannot import the tensor \"`1`\", as it uses the coordinate system \"`2`\", which does not exist.";

TImport::ErrorMissingMetric = "Cannot import the tensor \"`1`\", as it uses the metric \"`2`\", which does not exist.";

TImport::ErrorNotCoordinates = "Cannot import the tensor \"`1`\", as it uses the tensor \"`2`\" as a coordinate system, but that tensor does not represent a coordinate system.";

TImport::ErrorNotMetric = "Cannot import the tensor \"`1`\", as it uses the tensor \"`2`\" as a metric, but that tensor does not represent a metric.";

TImport::ErrorCoordinatesDimension = "Cannot import the tensor \"`1`\", as it requires the coordinate system \"`2`\" to have `3` dimensions, but the existing coordinate system has `4` dimensions.";

TImport::ErrorMetricDimension = "Cannot import the tensor \"`1`\", as it requires the metric \"`2`\" to have `3` dimensions, but the existing metric has `4` dimensions.";

TImport[data_Association] :=
    Module[{newData, newID},
        newID = Keys[data][[1]];
        If[!KeyExistsQ[data[[1]], "OGReVersion"] || data[[1]]["OGReVersion"] != OGReVersion,
            OGRePrint["Warning: The imported tensor was created in a different version of OGRe. Compatibility issues may occur."];
        ];
        (* Check that the imported tensor object doesn't already exist. *)
        CheckIfOverwriting[newID];
        If[ReferenceIDQ[newID],
            CheckIfTensorExists[ReferenceParts[newID][[1]]]
            ,
            CheckIDHasNoReference[newID]
        ];
        newData = KeyDrop[Values[data][[1]], "OGReVersion"];
        CheckImportedTensorReferences[newID, newData];
        SetTensorID[newID, newData];
        (* Update all code completions. *)
        UpdateCodeCompletions[];
        Return[newID];
    ];

CreateUsageMessage[TImportAll, "TImportAll[`source`] imports tensor data that has been exported using TExportAll[]. If `source` is an Association, imports the data directly. If `source` is a file name, imports the data from that file. If a file extension is not provided, the extension `.m` will be appended, unless the file name explicitly ends with a dot. If a full path is not given, the file is assumed to be located in the current working directory, as given by Directory[]. This directory can be changed using SetDirectory[].
`WARNING: This will irreversibly delete ALL of the tensors already defined in the current session.`"];

SyntaxInformation[TImportAll] = {"ArgumentsPattern" -> {_}};

TImportAll::ErrorFileMissing = "The file `1` does not exist.";

TImportAll::ErrorFileRead = "The file `1` cannot be opened for reading.";

TImportAll[data_Association] :=
    ImportTensorData[data];

TImportAll[filename_String] :=
    Module[{data, path, stream},
        If[FileExtension[filename] === "" && StringTake[filename, -1] =!= ".",
            path = filename <> ".m";
            ,
            path = filename;
        ];
        If[!FileExistsQ[path],
            Message[TImportAll::ErrorFileMissing, path];
            Abort[];
        ];
        stream = OpenRead[path];
        If[stream === $Failed,
            Message[TImportAll::ErrorFileRead, path];
            Abort[];
        ];
        data = Read[stream];
        ImportTensorData[data];
        Close[stream];
        (* Update all code completions. *)
        UpdateCodeCompletions[];
        OGRePrint[
            "Imported all tensor data from "
            ,
            With[{p = AbsoluteFileName[path]},
                CreateButton[p, NotebookOpen[p]]
            ]
            ,
            "."
        ];
    ];

CreateUsageMessage[TInfo, "TInfo[] lists all the tensors created so far in this session: coordinate systems, metrics, and the tensors associated with each metric.
TInfo[`ID`] displays information about the tensor object `ID`, including its symbol, role, rank, dimensions, associated metric, and default coordinates and indices, in human-readable form.
If `ID` represents a coordinate system, displays a list of all tensors using it as their default coordinate system.
If `ID` represents a metric, displays a list of all tensors using it as their associated metric."];

SyntaxInformation[TInfo] = {"ArgumentsPattern" -> {_.}};

TInfo[tensorID_String] :=
    Module[
        {info}
        ,
        (* Check that the tensor object tensorID exists. *)
        CheckIfTensorExists[tensorID];
        (* Collect information about the object. *)
        info = {Row[{Style["ID: ", Bold], DisplayTensorID[tensorID]}]};
        AppendTo[info, Row[{Style["Symbol: ", Bold], GetTensorData[tensorID]["Symbol"]}]];
        AppendTo[info, Row[{Style["Role: ", Bold], GetTensorData[tensorID]["Role"]}]];
        AppendTo[info, Row[{Style["Rank: ", Bold], TRank[tensorID]}]];
        AppendTo[info, Row[{Style["Dimensions: ", Bold], TDim[tensorID]}]];
        If[GetTensorData[tensorID]["Role"] =!= "Coordinates" && GetTensorData[tensorID]["Role"] =!= "Metric",
            AppendTo[info, Row[{Style["Metric: ", Bold], InfoButton[GetTensorData[tensorID]["Metric"]]}]]
        ];
        If[GetTensorData[tensorID]["Role"] =!= "Coordinates",
            AppendTo[info, Row[{Style["Default Coordinates: ", Bold], InfoButton[GetTensorData[tensorID]["DefaultCoords"]]}]]
        ];
        AppendTo[info, Row[{Style["Default Indices: ", Bold], GetTensorData[tensorID]["DefaultIndices"]}]];
        (* If tensorID represents a coordinate system, display a list of all tensors using it as their default coordinate system. *)
        If[GetTensorData[tensorID]["Role"] === "Coordinates",
            AppendTo[info, Row[{Style["Default Coordinates For: ", Bold], Row[InfoButton /@ Select[AllTensorIDs[], GetTensorData[#]["DefaultCoords"] === tensorID && # =!= tensorID&], ", "]}]]
        ];
        (* If tensorID represents a metric, display a list of all tensors using it as their associated metric. *)
        If[GetTensorData[tensorID]["Role"] === "Metric",
            AppendTo[info, Row[{Style["Tensors Using This Metric: ", Bold], MetricTensorButtons[tensorID]}]]
        ];
        (* Provide links to print out the components of the tensor using TList or TShow. *)
        AppendTo[info, Row[{Style["Components: ", Bold], CreateButton["TList", TList[tensorID]], " | ", CreateButton["TShow", TShow[tensorID]]}]];
        (* Print out the collected information. *)
        OGRePrint[Column[info]];
    ];

TInfo[] :=
    Module[
        {allTensors, tensorList, countCoord = 1, countMetric = 1}
        ,
        (* List all tensors except for Options (which is not actually a tensor) and DefaultResultID. *)
        tensorList = PublicTensorIDs[];
        allTensors = DeleteCases[AllTensorIDs[], DefaultResultID];
        (* Print a list of the coordinate systems, metrics, and the tensors associated with each metric. *)
        OGRePrint[Column[{Row[{Style["Total tensors created: ", Bold], Length[allTensors]}], Style["Coordinate Systems:", Bold], Column[Table[Row[{countCoord++, ". ", ID}], {ID, InfoButton /@ Select[tensorList, GetTensorData[#]["Role"] == "Coordinates"&]}]], Style["Metrics:", Bold], Column[Table[Row[{countMetric++, ". ", Style[InfoButton[ID], Bold], " \[Rule] ", MetricTensorButtons[ID]}], {ID, Select[tensorList, GetTensorData[#]["Role"] == "Metric"&]}]]}]];
    ];

CreateUsageMessage[TLineElement, "TLineElement[`metricID`] returns the line element of the metric `metricID` in its default coordinate system.
TLineElement[`metricID`, `coordinatesID`] returns the line element in the coordinate system `coordinatesID`."];

SyntaxInformation[TLineElement] = {"ArgumentsPattern" -> {_, _.}};

TLineElement::ErrorNotMetric = "The line element can only be calculated for a metric.";

TLineElement[metricID_String, coordinatesID_String:"_UseDefault_"] :=
    Module[{components, coordSymbols, dim, useCoords},
        CheckIfTensorExists[metricID];
        (* Check that metricID is indeed a metric. *)
        If[GetTensorData[metricID]["Role"] =!= "Metric",
            Message[TLineElement::ErrorNotMetric];
            Abort[];
        ];
        (* If a specific coordinate system is not given, use the metric's default coordinate system. *)
        If[coordinatesID === "_UseDefault_",
            useCoords = GetTensorData[metricID]["DefaultCoords"]
            ,
            (* Check that the tensor object coordinatesID exists and represents a coordinate system. *)
            CheckIfTensorExists[coordinatesID];
            CheckIfCoordinates[coordinatesID];
            useCoords = coordinatesID;
        ];
        (* Get the coordinate symbols to be used in the line element. *)
        coordSymbols = GetTensorData[useCoords]["Components"][{{1}, useCoords}];
        (* The dimension is the number of coordinates. *)
        dim = Length[coordSymbols];
        (* Get the metric's components in the desired representation, adding it if it does not already exist. *)
        components = AddRepresentation[metricID, {-1, -1}, useCoords];
        (* Return the line element. *)
        Return[TensorSimplify[Sum[components[[m, n]] * Symbol["\[DoubleStruckD]" <> ToString[coordSymbols[[m]]]] * Symbol["\[DoubleStruckD]" <> ToString[coordSymbols[[n]]]], {m, 1, dim}, {n, 1, dim}]]];
    ];

CreateUsageMessage[TList, "TList[`ID`] lists the unique, non-zero components of the tensor object `ID` in its default index configuration and coordinate system.
TList[`ID`, `indices`] lists the components in the index configuration `indices`, which should be a list of the form {\[PlusMinus]1, \[PlusMinus]1, ...}, where +1 corresponds to an upper index and -1 corresponds to a lower index.
TList[`ID`, `coordinatesID`] lists the components in the coordinate system `coordinatesID`.
TList[`ID`, `rules`] applies ReplaceAll[`rules`] to each of the tensor's elements, and then automatically simplifies them, before they are displayed. `rules` can be either a single rule of the form `lhs \[Rule] rhs` or a list of rules.
TList[`ID`, `function`] maps `function` to each of the tensor's elements, and then automatically simplifies them, before they are displayed.
TList[`ID`, `indices`, `coordinatesID`, `rules`, `function`] does all of the above; any of the arguments can be omitted. If both `rules` and `function` are given, the rules will be applied before the function.
Use TSetExactSignChecks to enable or disable exact sign checks. When exact sign checks are enabled (the default), if TList cannot detect equality of two components up to sign by direct comparison, it checks whether the sum of the components can be proven to equal zero, which may require simplification. For tensors with complicated components, this may take a long time, in which case this option can be disabled; however, TList will then only be able to detect when two components are the same up to sign in simple cases."];

SyntaxInformation[TList] = {"ArgumentsPattern" -> {_, _., _., _., _.}};

TList[tensorID_String, indices : {(1 | -1)...} : {"_UseDefault_"}, coordinatesID_String:"_UseDefault_", replace : _Rule | {_Rule..} : {}, function_:Identity] /; (!ListQ[function] && !StringQ[function]) :=
    ShowList["List", "Print", tensorID, indices, coordinatesID, replace, function];

CreateUsageMessage[TTeXList, "TTeXList[`ID`] returns a TeX string for the unique, non-zero components of the tensor object `ID` in its default index configuration and coordinate system.
TTeXList[`ID`, `indices`] returns the components in the index configuration `indices`, which should be a list of the form {\[PlusMinus]1, \[PlusMinus]1, ...}, where +1 corresponds to an upper index and -1 corresponds to a lower index.
TTeXList[`ID`, `coordinatesID`] returns the components in the coordinate system `coordinatesID`.
TTeXList[`ID`, `rules`] applies ReplaceAll[`rules`] to each of the tensor's elements, and then automatically simplifies them, before they are converted to TeX. `rules` can be either a single rule of the form `lhs \[Rule] rhs` or a list of rules.
TTeXList[`ID`, `function`] maps `function` to each of the tensor's elements, and then automatically simplifies them, before they are converted to TeX.
TTeXList[`ID`, `indices`, `coordinatesID`, `rules`, `function`] does all of the above; any of the arguments can be omitted. If both `rules` and `function` are given, the rules will be applied before the function.
Use TSetExactSignChecks to enable or disable exact sign checks. When exact sign checks are enabled (the default), if TTeXList cannot detect equality of two components up to sign by direct comparison, it checks whether the sum of the components can be proven to equal zero, which may require simplification. For tensors with complicated components, this may take a long time, in which case this option can be disabled; however, TTeXList will then only be able to detect when two components are the same up to sign in simple cases.
Note: It is recommended to use CopyToClipboard@TTeXList[...] to copy the resulting TeX string to the clipboard directly."];

SyntaxInformation[TTeXList] = {"ArgumentsPattern" -> {_, _., _., _., _.}};

TTeXList[tensorID_String, indices : {(1 | -1)...} : {"_UseDefault_"}, coordinatesID_String:"_UseDefault_", replace : _Rule | {_Rule..} : {}, function_:Identity] /; (!ListQ[function] && !StringQ[function]) :=
    ShowList["List", "TeX", tensorID, indices, coordinatesID, replace, function];

CreateUsageMessage[TMessage, "TMessage is a placeholder symbol to which messages not associated with any specific OGRe module are attached."];

SyntaxInformation[TMessage] = {"ArgumentsPattern" -> {}};

CreateUsageMessage[TNewCoordinates, "TNewCoordinates[`coordinatesID`, `symbols`], creates a new tensor object representing a coordinate system.
`coordinatesID` is a string that will be used to identify the new object, and must be unique.
`symbols` are the coordinate symbols, e.g. {t, x, y, z}. They will automatically be cleared and protected against future changes using TSetReservedSymbols[]."];

SyntaxInformation[TNewCoordinates] = {"ArgumentsPattern" -> {_, _}};

TNewCoordinates::ErrorEmptyList = "The coordinate symbols cannot be an empty list. At least one coordinate symbol must be specified.";

TNewCoordinates[coordinatesID_String, coordinates_List?VectorQ] :=
    (
        CheckIDHasNoReference[coordinatesID];
        CheckIfOverwriting[coordinatesID];
        (* Check that the coordinates are not an empty list. *)
        If[coordinates == {},
            Message[TNewCoordinates::ErrorEmptyList];
            Abort[]
        ];
        (* Clear any definitions previously used for the coordinate symbols and protect them against future changes. *)
        TSetReservedSymbols[Unevaluated[coordinates]];
        (* Create a new tensor object for the coordinates with the desired ID. *)
        SetTensorID[coordinatesID, Association["Components" -> Association[{{1}, coordinatesID} -> coordinates], "DefaultCoords" -> coordinatesID, "DefaultIndices" -> {1}, "Role" -> "Coordinates", "Symbol" -> "x"]];
        (* Update all code completions. *)
        UpdateCodeCompletions[];
        Return[coordinatesID];
    );

Attributes[TNewCoordinates] = HoldRest;

CreateUsageMessage[TNewMetric, "TNewMetric[`metricID`, `coordinatesID`, `components`, `symbol`] creates a new tensor object representing a metric.
`metricID` is a string that will be used to identify the new object, and must be unique.
`coordinatesID` is the unique ID of a tensor object representing a coordinate system, created using TNewCoordinates[].
`components` is a square, symmetric, and invertible matrix representing the metric with two lower indices in that coordinate system.
`symbol` will be used to represent the metric in formulas. If not given, \"g\" will be used."];

SyntaxInformation[TNewMetric] = {"ArgumentsPattern" -> {_, _, _, _.}};

TNewMetric::ErrorIncorrectDim = "The metric components must have the same dimension as the coordinates.";

TNewMetric::ErrorNotInvertible = "The metric must be invertible.";

TNewMetric::ErrorNotSymmetric = "The metric must be symmetric.";

TNewMetric::ErrorNotSquare = "The components of the metric must be a square matrix.";

TNewMetric::WarningOverwrite = "All built-in derived tensors previously calculated and cached in the metric being overwritten have been deleted.";

TNewMetric[metricID_String, coordinatesID_String, components_List, symbol_String:"g"] :=
    Module[{dim = Length[components], inverse, overwriting = False, simplified},
        CheckIDHasNoReference[metricID];
        (* Check that the target tensor object doesn't already exist. *)
        CheckIfOverwriting[metricID];
        If[TensorIDExistsQ[metricID],
            overwriting = True
        ];
        (* Check that the tensor object coordinatesID exists and represents a coordinate system. *)
        CheckIfTensorExists[coordinatesID];
        CheckIfCoordinates[coordinatesID];
        (* Simplify the components. *)
        simplified = TensorSimplify[components];
        (* Check that the matrix is square. *)
        If[!SquareMatrixQ[simplified],
            Message[TNewMetric::ErrorNotSquare];
            Abort[];
        ];
        (* Check that the metric components have the same dimension as the coordinates. *)
        If[dim != Length[GetTensorData[coordinatesID]["Components"][{{1}, coordinatesID}]],
            Message[TNewMetric::ErrorIncorrectDim];
            Abort[];
        ];
        (* Check that the matrix is symmetric. *)
        If[!SymmetricMatrixQ[simplified],
            Message[TNewMetric::ErrorNotSymmetric];
            Abort[];
        ];
        (* Invert the components, and return an error if the matrix is singular or cannot be inverted for any reason. *)
        inverse = Quiet[Check[Inverse[simplified], "Error"]];
        If[inverse === "Error",
            Message[TNewMetric::ErrorNotInvertible];
            Abort[];
        ];
        inverse = TensorSimplify[inverse];
        (* Create a new tensor object for the metric with the desired ID. The components of the matrix in every possible index configuration will be calculated in advance in the default coordinate system, to improve performance. *)
        SetTensorID[metricID, Association["Components" -> Association[{{-1, -1}, coordinatesID} -> simplified, {{1, 1}, coordinatesID} -> inverse, {{1, -1}, coordinatesID} -> IdentityMatrix[dim], {{-1, 1}, coordinatesID} -> IdentityMatrix[dim]], "DefaultCoords" -> coordinatesID, "DefaultIndices" -> {-1, -1}, "Metric" -> metricID, "Role" -> "Metric", "Symbol" -> symbol]];
        (* If we are overwriting an existing metric, all cached tensors stored in it have been deleted. *)
        If[overwriting,
            Message[TNewMetric::WarningOverwrite];
        ];
        (* Update all code completions. *)
        UpdateCodeCompletions[];
        Return[metricID];
    ];

CreateUsageMessage[TNewTensor, "TNewTensor[`tensorID`, `metricID`, `coordinatesID`, `indices`, `components`, `symbol`] creates a new tensor object.
`tensorID` is a string that will be used to identify the new object, and must be unique.
`metricID` is the unique ID of a tensor object representing a metric, created using TNewMetric[]. The metric will be used to raise and lower indices for the new tensor.
`coordinatesID` is the unique ID of a tensor object representing a coordinate system, created using TNewCoordinates[]. This coordinate system will be used to specify the components of the new tensor. If omitted, the default coordinate system of the metric `metricID` will be used.
`indices` must be a list of the form {\[PlusMinus]1, \[PlusMinus]1, ...}, where +1 corresponds to an upper index and -1 corresponds to a lower index.
`components` is a list specifying the representation of the tensor with the index configuration `indices` and in the coordinate system `coordinatesID`.
`symbol` will be used to represent the tensor in formulas. If not given, the placeholder " <> DefaultSymbol <> " will be used."];

SyntaxInformation[TNewTensor] = {"ArgumentsPattern" -> {_, _, _., _, _, _.}};

TNewTensor::ErrorDimension = "The components must have the same dimension as the coordinate system.";

TNewTensor::ErrorRank = "The number of indices must match the rank of the components.";

TNewTensor[tensorID_String, metricID_String, coordinatesID_String:"_UseDefault_", indices_List, components_List, symbol_String:DefaultSymbol] :=
    Module[{useCoords},
        CheckIDHasNoReference[tensorID];
        (* Check that the target tensor object doesn't already exist. *)
        CheckIfOverwriting[tensorID];
        (* Check that the tensor object metricID exists and represents a metric. *)
        CheckIfTensorExists[metricID];
        CheckIfMetric[metricID];
        (* Check that the list of indices is of the correct form. *)
        CheckIndicesForm[indices];
        (* Determine which coordinate system the components for the new tensor are given in. *)
        If[coordinatesID === "_UseDefault_",
            useCoords = GetTensorData[metricID]["DefaultCoords"]
            ,
            (* Check that the tensor object coordinatesID exists and represents a coordinate system. *)
            CheckIfTensorExists[coordinatesID];
            CheckIfCoordinates[coordinatesID];
            useCoords = coordinatesID
        ];
        (* Validate the input. This is done differently depending on whether the tensor is a scalar or not. *)
        If[Length[components] * ArrayDepth[components] != 1,
            (* Not a scalar. Check that the dimension of the given components matches the dimension of the coordinates being used. *)
            If[Length[components] != Length[GetTensorData[useCoords]["Components"][{{1}, useCoords}]],
                Message[TNewTensor::ErrorDimension];
                Abort[];
            ];
            (* Check that the rank of the given components matches the number of given indices. *)
            If[Length[indices] != ArrayDepth[components],
                Message[TNewTensor::ErrorRank];
                Abort[];
            ]
            ,
            (* Is a scalar. Check that the number of indices for the scalar is zero. *)
            If[Length[indices] != 0,
                Message[TNewTensor::ErrorRank];
                Abort[];
            ];
        ];
        (* Create a new tensor object for the tensor with the desired ID. *)
        SetTensorID[tensorID, Association["Components" -> Association[{indices, useCoords} -> TensorSimplify[components]], "DefaultCoords" -> useCoords, "DefaultIndices" -> indices, "Metric" -> metricID, "Role" -> "Tensor", "Symbol" -> symbol]];
        (* Update all code completions. *)
        UpdateCodeCompletions[];
        Return[tensorID];
    ];

CreateUsageMessage[TSetAllowOverwrite, "TSetAllowOverwrite[`True`] allows overwriting tensors. If the user creates a new tensor with the same ID as an existing tensor, the latter will be overwritten. Note that this can result in loss of data. TSetAllowOverwrite[`False`] disallows overwriting, which is the default setting. TSetAllowOverwrite[] returns the current setting. Note that this setting is persistent between sessions."];

SyntaxInformation[TSetAllowOverwrite] = {"ArgumentsPattern" -> {_.}};

TSetAllowOverwrite::Notify = "Overwriting tensors turned `1`.";

TSetAllowOverwrite[bool_?BooleanQ] :=
    (
        Unprotect[OGReGlobalOptions];
        OGReGlobalOptions["AllowOverwrite"] = bool;
        Message[
            TSetAllowOverwrite::Notify
            ,
            If[bool,
                "on"
                ,
                "off"
            ]
        ];
        LocalSymbol["OGReGlobalOptions"] = OGReGlobalOptions;
        Protect[OGReGlobalOptions];
    );

TSetAllowOverwrite[] :=
    OGReGlobalOptions["AllowOverwrite"];

CreateUsageMessage[TSetAssumptions, "TSetAssumptions[] shows the assumptions to be used when simplifying expressions.
TSetAssumptions[`assumptions`] appends new assumptions to the previously added assumptions.
TSetAssumptions[None] clears all previously added assumptions.
TSetAssumptions[!Reals] disables the default assumption that all variables are real, which secretly adds the assumption Element[_, Reals] to the list of assumptions. TSetAssumptions[Reals] re-enables this assumption.
The output of this module is always an Association indicating whether variables are assumed to be real and listing the user-defined assumptions."];

SyntaxInformation[TSetAssumptions] = {"ArgumentsPattern" -> {_.}};

TSetAssumptions[] :=
    TensorData[Options]["SimplifyAssumptions"];

TSetAssumptions[assumptions_] :=
    (
        Unprotect[TensorData];
        Switch[assumptions,
            None,
                TensorData[Options]["SimplifyAssumptions"]["User"] = None
            ,
            Reals,
                TensorData[Options]["SimplifyAssumptions"]["AssumeReal"] = True
            ,
            !Reals,
                TensorData[Options]["SimplifyAssumptions"]["AssumeReal"] = False
            ,
            _,
                If[TensorData[Options]["SimplifyAssumptions"]["User"] === None,
                    TensorData[Options]["SimplifyAssumptions"]["User"] = {assumptions}
                    ,
                    If[!MemberQ[TensorData[Options]["SimplifyAssumptions"]["User"], assumptions],
                        AppendTo[TensorData[Options]["SimplifyAssumptions"]["User"], assumptions];
                    ];
                ];
        ];
        Protect[TensorData];
        Return[TensorData[Options]["SimplifyAssumptions"]];
    );

Attributes[TSetAssumptions] = HoldAll;

CreateUsageMessage[TSetAutoUpdates, "TSetAutoUpdates[`False`] turns off automatic checks for updates at startup. TSetAutoUpdates[`True`] turns them back on, which is the default setting. TSetAutoUpdates[] returns the current setting. Note that this setting is persistent between sessions."];

SyntaxInformation[TSetAutoUpdates] = {"ArgumentsPattern" -> {_.}};

TSetAutoUpdates[bool_?BooleanQ] :=
    (
        Unprotect[OGReGlobalOptions];
        OGReGlobalOptions["AutoUpdates"] = bool;
        OGRePrint[
            "Auto updates turned "
            ,
            If[bool,
                "on"
                ,
                "off"
            ]
            ,
            "."
        ];
        LocalSymbol["OGReGlobalOptions"] = OGReGlobalOptions;
        Protect[OGReGlobalOptions];
    );

TSetAutoUpdates[] :=
    OGReGlobalOptions["AutoUpdates"];

CreateUsageMessage[TSetCurveParameter, "TSetCurveParameter[] shows the curve parameter used for calculating Lagrangians and geodesics.
TSetCurveParameter[`parameter`] changes the curve parameter. The new parameter will be cleared and protected, and the old parameter will be unprotected. `parameter` can be given either as a symbol name or a string representing a symbol name.
TSetCurveParameter[Automatic] resets the curve parameter to the default: " <> StringReplace[DefaultCurveParameter, "Global`" -> ""] <> "."];

SyntaxInformation[TSetCurveParameter] = {"ArgumentsPattern" -> {_.}};

TSetCurveParameter::InvalidSymbol = "\"`1`\" is not a valid symbol name.";

TSetCurveParameter[] :=
    StringReplace[TensorData[Options]["CurveParameter"], StartOfString ~~ "Global`" -> ""];

TSetCurveParameter[newParameter_String] :=
    Module[{newParameterName, newParameterSymbol, oldParameterSymbol, useParameter},
        newParameterName = StringReplace[StringTrim[newParameter], StartOfString ~~ "Global`" -> ""];
        useParameter = "Global`" <> newParameterName;
        newParameterSymbol = Quiet[Check[Symbol[useParameter], None]];
        (* Validate that Symbol accepted the parameter, and that it has no context mark after removing an optional Global` prefix. *)
        If[newParameterSymbol === None || StringContainsQ[newParameterName, "`"],
            Message[TSetCurveParameter::InvalidSymbol, newParameter];
            Abort[];
        ];
        oldParameterSymbol = Quiet[Check[Symbol[TensorData[Options]["CurveParameter"]], None]];
        (* Unprotect the old parameter. *)
        If[oldParameterSymbol =!= None,
            Unprotect[Evaluate[oldParameterSymbol]];
        ];
        (* Clear and protect the new parameter. *)
        Unprotect[Evaluate[newParameterSymbol]];
        ClearAll[Evaluate[newParameterSymbol]];
        Protect[Evaluate[newParameterSymbol]];
        (* Store the new parameter in the Options key. We store it as a string because strings are simpler to handle if the desired parameter already has a value. We store the Global` context explicitly because otherwise it would be evaluated in the OGRe`Private` context at startup. *)
        Unprotect[TensorData];
        TensorData[Options]["CurveParameter"] = useParameter;
        Protect[TensorData];
        Return[newParameterName];
    ];

TSetCurveParameter[parameter_Symbol] :=
    TSetCurveParameter[Evaluate[ToString[Unevaluated[parameter]]]];

TSetCurveParameter[Automatic] :=
    TSetCurveParameter[Evaluate[DefaultCurveParameter]];

Attributes[TSetCurveParameter] = HoldAll;

CreateUsageMessage[TSetExactSignChecks, "TSetExactSignChecks[] shows whether exact sign checks are enabled.
TSetExactSignChecks[`True`] enables exact sign checks, and TSetExactSignChecks[`False`] disables them.
TSetExactSignChecks[Automatic] resets exact sign checks to the default setting, which is `True`.
When exact sign checks are enabled, if TList cannot detect equality of two components up to sign by direct comparison, it checks whether the sum of the components can be proven to equal zero. It first tries a fast exact algebraic zero test, and if that fails, it falls back to the simplification function configured with TSetSimplifyFunc. For tensors with complicated components, this may take a long time, in which case this option can be disabled; however, TList will then only be able to detect when two components are the same up to sign in simple cases."];

SyntaxInformation[TSetExactSignChecks] = {"ArgumentsPattern" -> {_.}};

TSetExactSignChecks[chk_?BooleanQ] :=
    (
        Unprotect[TensorData];
        TensorData[Options]["ExactSignChecks"] = chk;
        Protect[TensorData];
        If[chk,
            OGRePrint["Exact sign checks enabled."];
            ,
            OGRePrint["Exact sign checks disabled."];
        ];
    );

TSetExactSignChecks[] :=
    TensorData[Options]["ExactSignChecks"];

TSetExactSignChecks[Automatic] :=
    TSetExactSignChecks[True];

CreateUsageMessage[TSetIndexLetters, "TSetIndexLetters[] shows the index letters used when displaying indices.
TSetIndexLetters[`letters`] changes the index letters.
TSetIndexLetters[Automatic] resets the index letters to the default: \"" <> DefaultIndexLetters <> "\"."];

SyntaxInformation[TSetIndexLetters] = {"ArgumentsPattern" -> {_.}};

TSetIndexLetters[] :=
    TensorData[Options]["IndexLetters"];

TSetIndexLetters[letters_String] :=
    (
        Unprotect[TensorData];
        TensorData[Options]["IndexLetters"] = letters;
        Protect[TensorData];
        Return[letters];
    );

TSetIndexLetters[Automatic] :=
    TSetIndexLetters[DefaultIndexLetters];

CreateUsageMessage[TSetParallelization, "TSetParallelization[`True`] enables the parallelization of tensor simplifications, and TSetParallelization[`False`] disables it. The default value is `False`. TSetParallelization[] returns the current value. If simplifications take less than a few seconds, then you should leave parallelization off, as it has a small overhead and may actually impede performance. However, if simplifications are taking more than a few seconds, then it is highly recommended to enable parallelization for a significant performance boost."];

SyntaxInformation[TSetParallelization] = {"ArgumentsPattern" -> {_.}};

TSetParallelization[par_?BooleanQ] :=
    (
        Unprotect[TensorData];
        TensorData[Options]["Parallelize"] = par;
        Protect[TensorData];
        If[par,
            OGRePrint["Parallelization enabled."];
            (* Launch all of the kernels for parallelization if they have not been launched yet, or if fewer than the maximum available number of kernels have been launched. Better do it now than cause a delay later. *)
            If[$KernelCount < $MaxLicenseSubprocesses,
                LaunchKernels[$MaxLicenseSubprocesses - $KernelCount];
                OGRePrint[$KernelCount, " parallel kernels launched. CPU has ", $ProcessorCount, " cores."];
            ]
            ,
            OGRePrint["Parallelization disabled."];
            OGRePrint["Closing ", $KernelCount, " kernels."];
            CloseKernels[];
        ];
    );

TSetParallelization[] :=
    TensorData[Options]["Parallelize"];

CreateUsageMessage[TSetSimplifyFunc, "TSetSimplifyFunc[] shows the function used when simplifying expressions.
TSetSimplifyFunc[`function`] changes the simplification function. The function will be called as `function`[`expression`, `assumptions`], where `assumptions` are the user-defined simplification assumptions set using TSetAssumptions[].
TSetSimplifyFunc[None] disables simplification completely.
TSetSimplifyFunc[Automatic] resets the simplification function to the default: FullSimplify."];

SyntaxInformation[TSetSimplifyFunc] = {"ArgumentsPattern" -> {_.}};

TSetSimplifyFunc[] :=
    TensorData[Options]["SimplifyFunc"];

TSetSimplifyFunc[Automatic] :=
    TSetSimplifyFunc[FullSimplify];

TSetSimplifyFunc[func_] :=
    (
        Unprotect[TensorData];
        TensorData[Options]["SimplifyFunc"] = func;
        Protect[TensorData];
        Return[func];
    );

CreateUsageMessage[TSetReservedSymbols, "TSetReservedSymbols[`symbol`] clears any definitions previously used for `symbol` and protects it against future changes. `symbol` can be either a symbol name or a string representing a symbol name. After completion, the module will return a list of the currently reserved symbols.
Useful for making sure coordinate variables, parameters, and abstract functions used in tensors remain abstract symbols and do not accidentally obtain values and break the code.
If the reserved symbol is a function, this function will be displayed without arguments when using TList[] and TShow[], for improved readability. The reserved symbols will be exported when using TExportAll[] so they can later be imported using TImportAll[].
TSetReservedSymbols[{`symbol1`, `symbol2`, ...}] reserves all of the given symbols. Each of the symbols can be either a symbol name or a string representing a symbol name.
TSetReservedSymbols[] returns the currently reserved symbols."];

SyntaxInformation[TSetReservedSymbols] = {"ArgumentsPattern" -> {_.}};

TSetReservedSymbols::ErrorNotGlobal = "Cannot reserve \"`1`\", as it is in the context `2`. Only symbols in the context Global`.` can be reserved.";

TSetReservedSymbols::ErrorNotSymbol = "Cannot reserve \"`1`\", as it is not a symbol.";

TSetReservedSymbols[symbol_Symbol] :=
    (
        If[Context[symbol] =!= "Global`",
            Message[TSetReservedSymbols::ErrorNotGlobal, Unevaluated[symbol], Context[Unevaluated[symbol]]]
            ,
            Unprotect[Unevaluated[symbol]];
            ClearAll[Unevaluated[symbol]];
            Protect[Unevaluated[symbol]];
            Unprotect[TensorData];
            AppendTo[TensorData[Options]["ReservedSymbols"], ToString[symbol]];
            TensorData[Options]["ReservedSymbols"] = DeleteDuplicates[TensorData[Options]["ReservedSymbols"]];
            Protect[TensorData];
            Return[TensorData[Options]["ReservedSymbols"]];
        ];
    );

TSetReservedSymbols[symbol_String] :=
    (
        Unprotect[symbol];
        ClearAll[symbol];
        Quiet[
            Check[
                Protect @@ ToExpression[symbol, StandardForm, Hold]
                ,
                Message[TSetReservedSymbols::ErrorNotSymbol, symbol];
                Abort[]
                ,
                Protect::pssl
            ]
            ,
            Protect::pssl
        ];
        Unprotect[TensorData];
        AppendTo[TensorData[Options]["ReservedSymbols"], symbol];
        TensorData[Options]["ReservedSymbols"] = DeleteDuplicates[TensorData[Options]["ReservedSymbols"]];
        Protect[TensorData];
        Return[TensorData[Options]["ReservedSymbols"]];
    );

TSetReservedSymbols[symbols_List] :=
    (
        Scan[TSetReservedSymbols, Unevaluated[symbols]];
        Return[TensorData[Options]["ReservedSymbols"]];
    )

TSetReservedSymbols[] :=
    TensorData[Options]["ReservedSymbols"];

Attributes[TSetReservedSymbols] = HoldAll;

CreateUsageMessage[TShow, "TShow[`ID`] shows the components of the tensor object `ID` in its default index configuration and coordinate system.
TShow[`ID`, `indices`] shows the components in the index configuration `indices`, which should be a list of the form {\[PlusMinus]1, \[PlusMinus]1, ...}, where +1 corresponds to an upper index and -1 corresponds to a lower index.
TShow[`ID`, `coordinatesID`] shows the components in the coordinate system `coordinatesID`.
TShow[`ID`, `rules`] applies ReplaceAll[`rules`] to each of the tensor's elements, and then automatically simplifies them, before they are displayed. `rules` can be either a single rule of the form `lhs \[Rule] rhs` or a list of rules.
TShow[`ID`, `function`] maps `function` to each of the tensor's elements, and then automatically simplifies them, before they are displayed.
TShow[`ID`, `indices`, `coordinatesID`, `rules`, `function`] does all of the above; any of the arguments can be omitted. If both `rules` and `function` are given, the rules will be applied before the function."];

SyntaxInformation[TShow] = {"ArgumentsPattern" -> {_, _., _., _., _.}};

TShow[tensorID_String, indices : {(1 | -1)...} : {"_UseDefault_"}, coordinatesID_String:"_UseDefault_", replace : _Rule | {_Rule..} : {}, function_:Identity] /; (!ListQ[function] && !StringQ[function]) :=
    ShowList["Show", "Print", tensorID, indices, coordinatesID, replace, function];

CreateUsageMessage[TTeXShow, "TTeXShow[`ID`] returns a TeX string for the components of the tensor object `ID` in its default index configuration and coordinate system.
TTeXShow[`ID`, `indices`] returns the components in the index configuration `indices`, which should be a list of the form {\[PlusMinus]1, \[PlusMinus]1, ...}, where +1 corresponds to an upper index and -1 corresponds to a lower index.
TTeXShow[`ID`, `coordinatesID`] returns the components in the coordinate system `coordinatesID`.
TTeXShow[`ID`, `rules`] applies ReplaceAll[`rules`] to each of the tensor's elements, and then automatically simplifies them, before they are converted to TeX. `rules` can be either a single rule of the form `lhs \[Rule] rhs` or a list of rules.
TTeXShow[`ID`, `function`] maps `function` to each of the tensor's elements, and then automatically simplifies them, before they are converted to TeX.
TTeXShow[`ID`, `indices`, `coordinatesID`, `rules`, `function`] does all of the above; any of the arguments can be omitted. If both `rules` and `function` are given, the rules will be applied before the function.
Note: It is recommended to use CopyToClipboard@TTeXShow[...] to copy the resulting TeX string to the clipboard directly."];

SyntaxInformation[TTeXShow] = {"ArgumentsPattern" -> {_, _., _., _., _.}};

TTeXShow[tensorID_String, indices : {(1 | -1)...} : {"_UseDefault_"}, coordinatesID_String:"_UseDefault_", replace : _Rule | {_Rule..} : {}, function_:Identity] /; (!ListQ[function] && !StringQ[function]) :=
    ShowList["Show", "TeX", tensorID, indices, coordinatesID, replace, function];

CreateUsageMessage[TSimplify, "TSimplify[`ID`] simplifies all previously-calculated representations of the tensor object `ID` based on the user-defined simplification assumptions set using TSetAssumptions[]. To be used if the assumptions have changed after the components have already been calculated.
TSimplify[`expression`] simplifies `expression` based on the user-defined simplification assumptions. If `expression` is a list, the components will be simplified in parallel."];

SyntaxInformation[TSimplify] = {"ArgumentsPattern" -> {_}};

TSimplify[tensorID_String] :=
    (
        CheckIfTensorExists[tensorID];
        ChangeTensorKey[tensorID, "Components", TensorSimplify /@ GetTensorData[tensorID]["Components"]];
        Return[tensorID];
    );

TSimplify[expression_] :=
    TensorSimplify[expression];

CreateUsageMessage[TMetricDeterminant, "TMetricDeterminant[`metricID`] returns the determinant of the metric `metricID` in its default coordinate system.
TMetricDeterminant[`metricID`, `coordinatesID`] returns the determinant in the coordinate system `coordinatesID`."];

SyntaxInformation[TMetricDeterminant] = {"ArgumentsPattern" -> {_, _.}};

TMetricDeterminant::ErrorNotMetric = "The determinant can only be calculated for a metric.";

TMetricDeterminant[metricID_String, coordinatesID_String:"_UseDefault_"] :=
    Module[{components, useCoords},
        CheckIfTensorExists[metricID];
        (* Check that metricID is indeed a metric. *)
        If[GetTensorData[metricID]["Role"] =!= "Metric",
            Message[TMetricDeterminant::ErrorNotMetric];
            Abort[];
        ];
        (* If a specific coordinate system is not given, use the metric's default coordinate system. *)
        If[coordinatesID === "_UseDefault_",
            useCoords = GetTensorData[metricID]["DefaultCoords"]
            ,
            (* Check that the tensor object coordinatesID exists and represents a coordinate system. *)
            CheckIfTensorExists[coordinatesID];
            CheckIfCoordinates[coordinatesID];
            useCoords = coordinatesID;
        ];
        (* Get the metric's components in the desired representation, adding it if it does not already exist. *)
        components = AddRepresentation[metricID, {-1, -1}, useCoords];
        (* Return the metric determinant. *)
        Return[TensorSimplify[Det[components]]];
    ];

CreateUsageMessage[TVolumeElement, "TVolumeElement[`metricID`] returns the volume element of the metric `metricID` in its default coordinate system.
TVolumeElement[`metricID`, `coordinatesID`] returns the volume element in the coordinate system `coordinatesID`."];

SyntaxInformation[TVolumeElement] = {"ArgumentsPattern" -> {_, _.}};

TVolumeElement::ErrorNotMetric = "The volume element can only be calculated for a metric.";

TVolumeElement[metricID_String, coordinatesID_String:"_UseDefault_"] :=
    Module[{components, coordDifferentials, coordForm, coordSymbols, dim, useCoords, volumeFactor},
        CheckIfTensorExists[metricID];
        (* Check that metricID is indeed a metric. *)
        If[GetTensorData[metricID]["Role"] =!= "Metric",
            Message[TVolumeElement::ErrorNotMetric];
            Abort[];
        ];
        (* If a specific coordinate system is not given, use the metric's default coordinate system. *)
        If[coordinatesID === "_UseDefault_",
            useCoords = GetTensorData[metricID]["DefaultCoords"]
            ,
            (* Check that the tensor object coordinatesID exists and represents a coordinate system. *)
            CheckIfTensorExists[coordinatesID];
            CheckIfCoordinates[coordinatesID];
            useCoords = coordinatesID;
        ];
        (* Get the coordinate symbols to be used in the volume element. *)
        coordSymbols = GetTensorData[useCoords]["Components"][{{1}, useCoords}];
        (* The dimension is the number of coordinates. *)
        dim = Length[coordSymbols];
        (* Get the metric's components in the desired representation, adding it if it does not already exist. *)
        components = AddRepresentation[metricID, {-1, -1}, useCoords];
        (* The volume factor is the square root of the absolute value of the determinant of the metric components. *)
        volumeFactor = TensorSimplify[Sqrt[Abs[Det[components]]]];
        (* Create symbols for the coordinate differentials. *)
        coordDifferentials = Symbol["\[DoubleStruckD]" <> ToString[#]]& /@ coordSymbols;
        (* Construct the coordinate n-form. *)
        coordForm =
            If[dim == 1,
                First[coordDifferentials]
                ,
                Inactive[Wedge] @@ coordDifferentials
            ];
        (* Return the volume element. *)
        Return[volumeFactor * coordForm];
    ];

(* =================================================== *)

(* Private modules (for internal use only) start here. *)

(* =================================================== *)

(* The URL of the latest release API endpoint on GitHub. *)

OGReLatestReleaseURL = "https://api.github.com/repos/bshoshany/OGRe/releases/latest";

(* Construct the GitHub URL for a release tag. *)

ReleaseURLFromTag[versionTag_String] :=
    "https://github.com/bshoshany/OGRe/releases/tag/" <> versionTag;

(* Construct the GitHub API URL for a release tag. *)

ReleaseAPIURLFromTag[versionTag_String] :=
    "https://api.github.com/repos/bshoshany/OGRe/releases/tags/" <> versionTag;

(* Get release data from GitHub. *)

ReleaseData[releaseAPIURL_String] :=
    Module[{releaseData},
        releaseData = Quiet[Import[releaseAPIURL, "RawJSON"]];
        If[AssociationQ[releaseData] && KeyExistsQ[releaseData, "tag_name"] && KeyExistsQ[releaseData, "html_url"] && KeyExistsQ[releaseData, "body"] && StringQ[releaseData["tag_name"]] && StringQ[releaseData["html_url"]] && StringQ[releaseData["body"]],
            Return[releaseData]
            ,
            Return[$Failed]
        ];
    ];

(* Get the latest release data from GitHub. *)

LatestReleaseData[] :=
    ReleaseData[OGReLatestReleaseURL];

(* Show Markdown release notes as Mathematica notebook cells. *)

ShowReleaseNotes[releaseNotes_String] :=
    CellPrint[ImportString[releaseNotes, {"Markdown", "Notebook"}][[1]]];

(* Show release notes for a release tag. *)

ShowReleaseNotesForTag[versionTag_String] :=
    Module[{releaseData},
        releaseData = ReleaseData[ReleaseAPIURLFromTag[versionTag]];
        If[releaseData === $Failed,
            OGRePrint[Row[{"Error: Failed to load the release notes. Please visit ", Hyperlink[ReleaseURLFromTag[versionTag]], " to view them manually."}]]
            ,
            ShowReleaseNotes[releaseData["body"]];
        ];
    ];

(* Extract the version tag and version numbers from a string of the form v<major>.<minor>.<patch>. *)

VersionNumberFromString[version_String] :=
    Module[{versionLookup},
        versionLookup = StringCases[version, StartOfString ~~ "v" ~~ major : DigitCharacter.. ~~ "." ~~ minor : DigitCharacter.. ~~ "." ~~ patch : DigitCharacter.. ~~ ___ :> Association["Tag" -> "v" <> major <> "." <> minor <> "." <> patch, "Numbers" -> ToExpression[{major, minor, patch}]]];
        If[Length[versionLookup] == 1,
            Return[versionLookup[[1]]]
            ,
            Return[$Failed]
        ];
    ];

(* Check whether a release version is newer than the current package version. *)

NewVersionQ[newVersion_String] :=
    Module[{currentVersion, newVersionData, versionComparison},
        currentVersion = VersionNumberFromString[OGReVersion];
        newVersionData = VersionNumberFromString[newVersion];
        If[currentVersion === $Failed || newVersionData === $Failed,
            Return[$Failed];
        ];
        versionComparison = Select[Transpose[{newVersionData["Numbers"], currentVersion["Numbers"]}], #[[1]] != #[[2]]&];
        Return[Length[versionComparison] > 0 && versionComparison[[1, 1]] > versionComparison[[1, 2]]];
    ];

(* Return True if the tensor ID is a reference ID. *)

ReferenceIDQ[tensorID_String] :=
    StringContainsQ[tensorID, ReferenceOperator];

(* Split a reference ID into the metric and role it refers to. *)

TMessage::ErrorInvalidReference = "The tensor ID \"`1`\" is an invalid reference. A reference ID must have the form \"Metric" <> ReferenceOperator <> "Role\".";

ReferenceParts[tensorID_String] :=
    Module[{count},
        count = StringCount[tensorID, ReferenceOperator];
        If[count =!= 1 || StringStartsQ[tensorID, ReferenceOperator] || StringEndsQ[tensorID, ReferenceOperator],
            Message[TMessage::ErrorInvalidReference, tensorID];
            Abort[];
        ];
        Return[StringSplit[tensorID, ReferenceOperator]];
    ];

(* Check that a new user-created tensor ID is not a reference ID. *)

TMessage::ErrorReferenceID = "User-defined tensor IDs cannot contain \"" <> ReferenceOperator <> "\".";

CheckIDHasNoReference[tensorID_String] :=
    If[ReferenceIDQ[tensorID],
        Message[TMessage::ErrorReferenceID];
        Abort[];
    ];

(* Check that a tensor ID refers to a user-created tensor rather than a cached tensor. *)

TMessage::ErrorNotUserTensor = "The tensor \"`1`\" is a cached tensor. This module can only modify user-created tensor objects.";

CheckIfUserTensor[tensorID_String] :=
    If[ReferenceIDQ[tensorID],
        Message[TMessage::ErrorNotUserTensor, DisplayTensorID[tensorID]];
        Abort[];
    ];

(* Create a reference ID from a metric ID and a cache role. *)

ReferenceID[metricID_String, role_String] :=
    metricID <> ReferenceOperator <> role;

(* Return True if the tensor ID exists, including cached tensors referenced using ReferenceOperator. *)

TensorIDExistsQ[tensorID_String] :=
    Module[{metricID, role},
        If[ReferenceIDQ[tensorID],
            {metricID, role} = ReferenceParts[tensorID];
            Return[KeyExistsQ[TensorData, metricID] && AssociationQ[TensorData[metricID]] && KeyExistsQ[TensorData[metricID], "Derived"] && AssociationQ[TensorData[metricID]["Derived"]] && KeyExistsQ[TensorData[metricID]["Derived"], role]];
        ];
        Return[KeyExistsQ[TensorData, tensorID]];
    ];

(* Get the data for a tensor ID, including cached tensors referenced using ReferenceOperator. *)

GetTensorData[tensorID_String] :=
    Module[{metricID, role},
        If[ReferenceIDQ[tensorID],
            {metricID, role} = ReferenceParts[tensorID];
            Return[TensorData[metricID]["Derived"][role]];
        ];
        Return[TensorData[tensorID]];
    ];

(* Return the raw tensor data association. *)

ExportTensorData[] :=
    TensorData;

(* List tensor IDs stored directly in TensorData. *)

TopLevelTensorIDs[] :=
    Sort[DeleteCases[Keys[TensorData], Options]];

(* List user-facing tensor IDs stored directly in TensorData. *)

PublicTensorIDs[] :=
    DeleteCases[TopLevelTensorIDs[], DefaultResultID];

(* List cache roles stored inside a metric. *)

CachedTensorRoles[metricID_String] :=
    Module[{derivedData, metricData = GetTensorData[metricID]},
        If[KeyExistsQ[metricData, "Role"] && metricData["Role"] === "Metric",
            If[KeyExistsQ[metricData, "Derived"] && AssociationQ[metricData["Derived"]],
                derivedData = metricData["Derived"];
                Return[Sort[Select[Keys[derivedData], StringQ[#] && AssociationQ[derivedData[#]] && KeyExistsQ[derivedData[#], "Role"] && KeyExistsQ[derivedData[#], "Components"]&]]];
            ];
        ];
        Return[{}];
    ];

(* List cache reference IDs. *)

CachedTensorIDs[] :=
    Sort[Flatten[Table[ReferenceID[metricID, #]& /@ CachedTensorRoles[metricID], {metricID, Select[PublicTensorIDs[], GetTensorData[#]["Role"] === "Metric"&]}]]];

(* List all tensor IDs, including cache references. *)

AllTensorIDs[] :=
    Sort[Join[TopLevelTensorIDs[], CachedTensorIDs[]]];

(* Display a tensor ID in user-facing output. *)

DisplayTensorID[tensorID_String] :=
    tensorID;

(* Ensure that a built-in cache tensor exists inside the given metric, and return its reference ID. *)

EnsureMetricCache[metricID_String, role_String] :=
    Module[
        {cacheID, components, useCoords}
        ,
        (* Check that metricID exists and represents a metric. *)
        CheckIfTensorExists[metricID];
        CheckIfMetric[metricID];
        cacheID = ReferenceID[metricID, role];
        If[!TensorIDExistsQ[cacheID],
            useCoords = GetTensorData[metricID]["DefaultCoords"];
            components = BuiltInRoleInfo[role]["Calculation"][metricID, useCoords];
            SetTensorID[cacheID, Association["Components" -> Association[{BuiltInRoleInfo[role]["DefaultIndices"], useCoords} -> components], "DefaultCoords" -> useCoords, "DefaultIndices" -> BuiltInRoleInfo[role]["DefaultIndices"], "Metric" -> metricID, "Role" -> role, "Symbol" -> BuiltInRoleInfo[role]["Symbol"]]];
        ];
        Return[cacheID];
    ];

(* Check if a tensor is of a role that uses the curve parameter. *)

CurveParameterTensorQ[tensorID_String] :=
    MemberQ[{"Lagrangian", "GeodesicFromChristoffel", "GeodesicFromLagrangian"}, GetTensorData[tensorID]["Role"]];

(* Check if a tensor is of a role that should be recalculated directly when changing coordinates, rather than transformed as a tensor. *)

RecalculateCoordinatesTensorQ[tensorID_String] :=
    MemberQ[{"Christoffel", "Riemann", "Weyl", "Lagrangian", "GeodesicFromChristoffel", "GeodesicFromLagrangian", "GeodesicWithTimeParameter"}, GetTensorData[tensorID]["Role"]];

(* Calculate the Christoffel symbol components directly in the given coordinate system. *)

CalcChristoffelComponents[metricID_String, coordinatesID_String] :=
    Module[
        {coordSymbols, dim, inverseMetricComponents, metricComponents}
        ,
        (* Get the metric components and coordinate symbols in the given coordinate system. *)
        metricComponents = AddRepresentation[metricID, {-1, -1}, coordinatesID];
        inverseMetricComponents = AddRepresentation[metricID, {1, 1}, coordinatesID];
        coordSymbols = GetTensorData[coordinatesID]["Components"][{{1}, coordinatesID}];
        dim = Length[coordSymbols];
        (* Calculate the Christoffel symbols. *)
        Return[TensorSimplify[Table[1/2 Sum[inverseMetricComponents[[\[Lambda], \[Sigma]]] * (D[metricComponents[[\[Nu], \[Sigma]]], coordSymbols[[\[Mu]]]] + D[metricComponents[[\[Sigma], \[Mu]]], coordSymbols[[\[Nu]]]] - D[metricComponents[[\[Mu], \[Nu]]], coordSymbols[[\[Sigma]]]]), {\[Sigma], 1, dim}], {\[Lambda], 1, dim}, {\[Mu], 1, dim}, {\[Nu], 1, dim}]]];
    ];

(* Calculate the Riemann tensor components directly in the given coordinate system. *)

CalcRiemannComponents[metricID_String, coordinatesID_String] :=
    Module[
        {christComponents, coordSymbols, dim}
        ,
        (* Get the Christoffel symbols and coordinate symbols in the given coordinate system. *)
        christComponents = AddRepresentation[TChristoffel[metricID], {1, -1, -1}, coordinatesID];
        coordSymbols = GetTensorData[coordinatesID]["Components"][{{1}, coordinatesID}];
        dim = Length[coordSymbols];
        (* Calculate the Riemann tensor. *)
        Return[TensorSimplify[Table[D[christComponents[[\[Rho], \[Nu], \[Sigma]]], coordSymbols[[\[Mu]]]] - D[christComponents[[\[Rho], \[Mu], \[Sigma]]], coordSymbols[[\[Nu]]]] + Sum[christComponents[[\[Rho], \[Mu], \[Lambda]]] * christComponents[[\[Lambda], \[Nu], \[Sigma]]] - christComponents[[\[Rho], \[Nu], \[Lambda]]] * christComponents[[\[Lambda], \[Mu], \[Sigma]]], {\[Lambda], 1, dim}], {\[Rho], 1, dim}, {\[Sigma], 1, dim}, {\[Mu], 1, dim}, {\[Nu], 1, dim}]]];
    ];

(* Calculate the Weyl tensor components directly in the given coordinate system. *)

CalcWeylComponents[metricID_String, coordinatesID_String] :=
    Module[
        {dim, metricComponents, ricciComponents, ricciScalar, ricciMixedComponents, riemannComponents}
        ,
        (* Calculate the Weyl tensor from the Riemann tensor, Ricci tensor, and Ricci scalar. *)
        riemannComponents = AddRepresentation[TRiemann[metricID], {1, -1, -1, -1}, coordinatesID];
        dim = Length[riemannComponents];
        If[dim <= 3,
            Return[Table[0, {\[Rho], 1, dim}, {\[Sigma], 1, dim}, {\[Mu], 1, dim}, {\[Nu], 1, dim}]]
        ];
        metricComponents = AddRepresentation[metricID, {-1, -1}, coordinatesID];
        ricciComponents = AddRepresentation[TRicci[metricID], {-1, -1}, coordinatesID];
        ricciMixedComponents = AddRepresentation[TRicci[metricID], {1, -1}, coordinatesID];
        ricciScalar = AddRepresentation[TRicciScalar[metricID], {}, coordinatesID][[1]];
        Return[TensorSimplify[Table[riemannComponents[[\[Rho], \[Sigma], \[Mu], \[Nu]]] - (KroneckerDelta[\[Rho], \[Mu]] * ricciComponents[[\[Nu], \[Sigma]]] - KroneckerDelta[\[Rho], \[Nu]] * ricciComponents[[\[Mu], \[Sigma]]] - metricComponents[[\[Sigma], \[Mu]]] * ricciMixedComponents[[\[Rho], \[Nu]]] + metricComponents[[\[Sigma], \[Nu]]] * ricciMixedComponents[[\[Rho], \[Mu]]]) / (dim - 2) + ricciScalar * (KroneckerDelta[\[Rho], \[Mu]] * metricComponents[[\[Nu], \[Sigma]]] - KroneckerDelta[\[Rho], \[Nu]] * metricComponents[[\[Mu], \[Sigma]]]) / ((dim - 1) * (dim - 2)), {\[Rho], 1, dim}, {\[Sigma], 1, dim}, {\[Mu], 1, dim}, {\[Nu], 1, dim}]]];
    ];

(* Calculate the Ricci tensor components directly in the given coordinate system. *)

CalcRicciComponents[metricID_String, coordinatesID_String] :=
    Module[
        {dim, riemannComponents}
        ,
        (* Take the trace of the Riemann tensor. *)
        riemannComponents = AddRepresentation[TRiemann[metricID], {1, -1, -1, -1}, coordinatesID];
        dim = Length[riemannComponents];
        Return[TensorSimplify[Table[Sum[riemannComponents[[\[Lambda], \[Mu], \[Lambda], \[Nu]]], {\[Lambda], 1, dim}], {\[Mu], 1, dim}, {\[Nu], 1, dim}]]];
    ];

(* Calculate the Ricci scalar components directly in the given coordinate system. *)

CalcRicciScalarComponents[metricID_String, coordinatesID_String] :=
    Module[
        {dim, inverseMetricComponents, ricciComponents}
        ,
        (* Take the trace of the Ricci tensor. *)
        inverseMetricComponents = AddRepresentation[metricID, {1, 1}, coordinatesID];
        ricciComponents = AddRepresentation[TRicci[metricID], {-1, -1}, coordinatesID];
        dim = Length[ricciComponents];
        Return[TensorSimplify[{Sum[inverseMetricComponents[[\[Mu], \[Nu]]] * ricciComponents[[\[Mu], \[Nu]]], {\[Mu], 1, dim}, {\[Nu], 1, dim}]}]];
    ];

(* Calculate the Kretschmann scalar components directly in the given coordinate system. *)

CalcKretschmannComponents[metricID_String, coordinatesID_String] :=
    Module[
        {dim, riemannComponents, secondRiemannComponents}
        ,
        (* Contract the Riemann tensor with itself. *)
        riemannComponents = AddRepresentation[TRiemann[metricID], {1, -1, -1, -1}, coordinatesID];
        secondRiemannComponents = AddRepresentation[TRiemann[metricID], {-1, 1, 1, 1}, coordinatesID];
        dim = Length[riemannComponents];
        Return[TensorSimplify[{Sum[riemannComponents[[\[Rho], \[Sigma], \[Mu], \[Nu]]] * secondRiemannComponents[[\[Rho], \[Sigma], \[Mu], \[Nu]]], {\[Rho], 1, dim}, {\[Sigma], 1, dim}, {\[Mu], 1, dim}, {\[Nu], 1, dim}]}]];
    ];

(* Calculate the Einstein tensor components directly in the given coordinate system. *)

CalcEinsteinComponents[metricID_String, coordinatesID_String] :=
    Module[
        {metricComponents, ricciComponents, ricciScalar}
        ,
        (* Calculate the Einstein tensor from the Ricci tensor and scalar. *)
        metricComponents = AddRepresentation[metricID, {-1, -1}, coordinatesID];
        ricciComponents = AddRepresentation[TRicci[metricID], {-1, -1}, coordinatesID];
        ricciScalar = AddRepresentation[TRicciScalar[metricID], {}, coordinatesID][[1]];
        Return[TensorSimplify[ricciComponents - 1/2 metricComponents * ricciScalar]];
    ];

(* Calculate the curve Lagrangian components directly in the given coordinate system. *)

CalcLagrangianComponents[metricID_String, coordinatesID_String] :=
    Module[
        {coordSymbols, dim, metricComponents, tangentComponents}
        ,
        (* Get the metric components and coordinate symbols in the given coordinate system. *)
        metricComponents = AddRepresentation[metricID, {-1, -1}, coordinatesID];
        coordSymbols = GetTensorData[coordinatesID]["Components"][{{1}, coordinatesID}];
        dim = Length[coordSymbols];
        (* Define metric components with any instance of the coordinate symbols replaced with coordinate functions in terms of the curve parameter. For example, with coordinates {x, y, z}, the expression z * f[x, y] will be replaced with z[c] * f[x[c], y[c]] where c is the curve parameter. *)
        metricComponents = metricComponents /. (# -> #[$CurveParam]& /@ coordSymbols);
        (* Define the components of the tangent vector to the curve. The vector consists of the derivatives of the coordinates with respect to the curve parameter. *)
        tangentComponents = coordSymbols /. ((# -> #'[$CurveParam])& /@ coordSymbols);
        (* Calculate the Lagrangian. *)
        Return[TensorSimplify[{Sum[metricComponents[[\[Mu], \[Nu]]] * tangentComponents[[\[Mu]]] * tangentComponents[[\[Nu]]], {\[Mu], 1, dim}, {\[Nu], 1, dim}]}]];
    ];

(* Calculate the geodesic equation components from the Christoffel symbols in the given coordinate system. *)

CalcGeodesicFromChristoffelComponents[metricID_String, coordinatesID_String] :=
    Module[
        {accelComponents, christComponents, coordSymbols, dim, tangentComponents}
        ,
        (* Get the Christoffel symbols and coordinate symbols in the given coordinate system. *)
        christComponents = AddRepresentation[TChristoffel[metricID], {1, -1, -1}, coordinatesID];
        coordSymbols = GetTensorData[coordinatesID]["Components"][{{1}, coordinatesID}];
        dim = Length[coordSymbols];
        (* Replace the coordinate symbols in the Christoffel symbols with coordinate functions in terms of the curve parameter. *)
        christComponents = christComponents /. (# -> #[$CurveParam]& /@ coordSymbols);
        (* Define the components of the tangent vector to the curve. The vector consists of the derivatives of the coordinates with respect to the curve parameter. *)
        tangentComponents = coordSymbols /. ((# -> #'[$CurveParam])& /@ coordSymbols);
        (* Define the components of the acceleration vector. The vector consists of the second derivatives of the coordinates with respect to the curve parameter. *)
        accelComponents = coordSymbols /. ((# -> #''[$CurveParam])& /@ coordSymbols);
        (* Calculate the geodesic equation vector. *)
        Return[TensorSimplify[Table[accelComponents[[\[Sigma]]] + Sum[christComponents[[\[Sigma], \[Mu], \[Nu]]] * tangentComponents[[\[Mu]]] * tangentComponents[[\[Nu]]], {\[Mu], 1, dim}, {\[Nu], 1, dim}], {\[Sigma], 1, dim}]]];
    ];

(* Calculate the geodesic equation components with the first coordinate as the parameter. *)

CalcGeodesicWithTimeParameterComponents[metricID_String, coordinatesID_String] :=
    Module[
        {accelComponents, christComponents, coordSymbols, coordSymbolsWithoutTime, dim, parameter, tangentComponents}
        ,
        (* Get the Christoffel symbols and coordinate symbols in the given coordinate system. *)
        christComponents = AddRepresentation[TChristoffel[metricID], {1, -1, -1}, coordinatesID];
        coordSymbols = GetTensorData[coordinatesID]["Components"][{{1}, coordinatesID}];
        dim = Length[coordSymbols];
        (* Use the first coordinate, which we assume to be time, as the curve parameter. *)
        parameter = coordSymbols[[1]];
        (* For the replacements below we only want to replace the non-time coordinates. *)
        coordSymbolsWithoutTime = coordSymbols[[2 ;; ]];
        (* Replace the non-time coordinate symbols in the Christoffel symbols with coordinate functions of the time parameter. *)
        christComponents = christComponents /. (# -> #[parameter]& /@ coordSymbolsWithoutTime);
        (* Define the components of the tangent vector to the curve. The vector consists of the derivatives of the coordinates with respect to the time parameter, thus the first component will be equal to 1. *)
        tangentComponents = coordSymbols /. ((# -> #'[parameter])& /@ coordSymbolsWithoutTime);
        tangentComponents[[1]] = 1;
        (* Define the components of the acceleration vector. The vector consists of the second derivatives of the coordinates with respect to the time parameter, thus the first component will be equal to 0. *)
        accelComponents = coordSymbols /. ((# -> #''[parameter])& /@ coordSymbolsWithoutTime);
        accelComponents[[1]] = 0;
        (* Calculate the geodesic equation vector. *)
        Return[TensorSimplify[Table[accelComponents[[\[Sigma]]] + Sum[(christComponents[[\[Sigma], \[Mu], \[Nu]]] - christComponents[[1, \[Mu], \[Nu]]] * tangentComponents[[\[Sigma]]]) * tangentComponents[[\[Mu]]] * tangentComponents[[\[Nu]]], {\[Mu], 1, dim}, {\[Nu], 1, dim}], {\[Sigma], 1, dim}]]];
    ];

(* Calculate the geodesic equation components from the curve Lagrangian in the given coordinate system. *)

CalcGeodesicFromLagrangianComponents[metricID_String, coordinatesID_String] :=
    Module[
        {coordSymbols, eulerLagrangeComponents, lagrangianComponents}
        ,
        (* Get the Lagrangian components and coordinate symbols in the given coordinate system. We divide the Lagrangian by 2 since geodesics calculated in this way will inevitably get additional factors of 2 from taking the derivatives of squares. *)
        lagrangianComponents = AddRepresentation[TLagrangian[metricID], {}, coordinatesID][[1]] / 2;
        coordSymbols = GetTensorData[coordinatesID]["Components"][{{1}, coordinatesID}];
        (* Calculate the geodesic equation vector from the Lagrangian by calculating the Euler-Lagrange equation for each variable. We leave the derivative with respect to the curve parameter implicit using Inactive. *)
        eulerLagrangeComponents = Table[Inactive[D][D[lagrangianComponents, coordSymbols[[n]]'[$CurveParam]], $CurveParam] - D[lagrangianComponents, coordSymbols[[n]][$CurveParam]], {n, 1, Length[coordSymbols]}];
        Return[TensorSimplify[eulerLagrangeComponents]];
    ];

(* Store the default index configuration, calculation function, and symbol for built-in roles. *)

BuiltInRoleInfo = Association["Christoffel" -> Association["DefaultIndices" -> {1, -1, -1}, "Calculation" -> CalcChristoffelComponents, "Symbol" -> "\[CapitalGamma]"], "Einstein" -> Association["DefaultIndices" -> {-1, -1}, "Calculation" -> CalcEinsteinComponents, "Symbol" -> "G"], "GeodesicFromChristoffel" -> Association["DefaultIndices" -> {1}, "Calculation" -> CalcGeodesicFromChristoffelComponents, "Symbol" -> "0"], "GeodesicFromLagrangian" -> Association["DefaultIndices" -> {-1}, "Calculation" -> CalcGeodesicFromLagrangianComponents, "Symbol" -> "0"], "GeodesicWithTimeParameter" -> Association["DefaultIndices" -> {1}, "Calculation" -> CalcGeodesicWithTimeParameterComponents, "Symbol" -> "0"], "Kretschmann" -> Association["DefaultIndices" -> {}, "Calculation" -> CalcKretschmannComponents, "Symbol" -> "K"], "Lagrangian" -> Association["DefaultIndices" -> {}, "Calculation" -> CalcLagrangianComponents, "Symbol" -> "L"], "Ricci" -> Association["DefaultIndices" -> {-1, -1}, "Calculation" -> CalcRicciComponents, "Symbol" -> "R"], "RicciScalar" -> Association["DefaultIndices" -> {}, "Calculation" -> CalcRicciScalarComponents, "Symbol" -> "R"], "Riemann" -> Association["DefaultIndices" -> {1, -1, -1, -1}, "Calculation" -> CalcRiemannComponents, "Symbol" -> "R"], "Weyl" -> Association["DefaultIndices" -> {1, -1, -1, -1}, "Calculation" -> CalcWeylComponents, "Symbol" -> "C"]];

(* Recalculate a built-in tensor in a new coordinate system and store the components. We do this when the tensor was already calculated and is requested in a different coordinate system, instead of transforming the existing components. *)

RecalculateCoordinates[tensorID_String, coordinatesID_String] :=
    Module[
        {allComponents, calculatedComponents, metricID = GetTensorData[tensorID]["Metric"], role = GetTensorData[tensorID]["Role"]}
        ,
        (* Calculate the components in the new coordinate system. *)
        calculatedComponents = BuiltInRoleInfo[role]["Calculation"][metricID, coordinatesID];
        (* Store the new coordinate representation. *)
        allComponents = GetTensorData[tensorID]["Components"];
        allComponents[{BuiltInRoleInfo[role]["DefaultIndices"], coordinatesID}] = calculatedComponents;
        ChangeTensorKey[tensorID, "Components", allComponents];
        Return[calculatedComponents];
    ];

(* Add a new representation with a specific index configuration and coordinate system to a tensor object, if it does not already exist. Returns the components of the representation in any case. *)

TMessage::ErrorCoordinatesCoord = "Cannot transform coordinates for a tensor object representing a coordinate system.";

TMessage::ErrorCoordinatesIndex = "Cannot lower index for a tensor object representing a coordinate system.";

TMessage::ErrorGeodesicTimeIndices = "Index transformations are not supported for geodesic equations in terms of the time coordinate, since they do not transform as a tensor.";

AddRepresentation[tensorID_String, indices_List, coordinatesID_String] :=
    Module[
        {defCoords = GetTensorData[tensorID]["DefaultCoords"], defIndices = GetTensorData[tensorID]["DefaultIndices"], oldIndices}
        ,
        (* Geodesic equations written in terms of the time coordinate do not transform as tensors, so their index configuration cannot be changed. *)
        If[GetTensorData[tensorID]["Role"] === "GeodesicWithTimeParameter" && indices =!= defIndices,
            Message[TMessage::ErrorGeodesicTimeIndices];
            Abort[];
        ];
        (* If the representation has already been calculated, simply return the components. *)
        If[KeyExistsQ[GetTensorData[tensorID]["Components"], {indices, coordinatesID}],
            Return[GetTensorData[tensorID]["Components"][{indices, coordinatesID}]]
        ];
        (* Check that we are not trying to represent a coordinate tensor in a different coordinate system. *)
        If[GetTensorData[tensorID]["Role"] === "Coordinates" && coordinatesID =!= tensorID,
            Message[TMessage::ErrorCoordinatesCoord];
            Abort[];
        ];
        (* Check that we are not trying to represent a coordinate tensor with a lower index. *)
        If[GetTensorData[tensorID]["Role"] === "Coordinates" && indices != {1},
            Message[TMessage::ErrorCoordinatesIndex];
            Abort[];
        ];
        (* Transform the tensor to different coordinates if required. Some built-in tensors are recalculated directly in the new coordinates instead of transformed. *)
        If[RecalculateCoordinatesTensorQ[tensorID] && coordinatesID =!= defCoords,
            RecalculateCoordinates[tensorID, coordinatesID]
            ,
            TransformCoordinates[tensorID, defIndices, defCoords, coordinatesID];
        ];
        (* Raise or lower indices if required, one by one. We only need to do this if the tensor is not a metric, since metric tensors already have their components pre-calculated with all possible index configurations. *)
        If[GetTensorData[tensorID]["Role"] =!= "Metric",
            oldIndices = defIndices;
            Do[oldIndices = RaiseLower[tensorID, coordinatesID, oldIndices, i, indices[[i]]], {i, 1, Length[defIndices]}];
        ];
        (* Return the new components. *)
        Return[GetTensorData[tensorID]["Components"][{indices, coordinatesID}]];
    ];

(* Add two tensor objects. *)

TCalc::AddErrorCoords = "The tensor \"`1`\" cannot be added to another tensor, as it represents a coordinate system.";

TCalc::AddErrorIndicesSame = "The tensors \"`1`\"[\"`2`\"] and \"`3`\"[\"`4`\"] cannot be added, as their index specifications must be the same up to permutation."

TCalc::AddErrorMetricsMatch = "The tensors \"`1`\" and \"`2`\" cannot be added, as they are associated with different metrics, \"`3`\" and \"`4`\" respectively.";

TCalc::AddErrorRanksMatch = "The tensors \"`1`\" and \"`2`\" cannot be added, as they have different ranks.";

AddTensors[firstID_String[firstIndices_String], secondID_String[secondIndices_String]] :=
    Module[
        {allVars, firstComponents, firstVars, newID, secondComponents, secondVars, sumComponents, useCoords, useIndices, useSecondIndices}
        ,
        (* Check that both tensors exist. *)
        CheckIfTensorExists[firstID];
        CheckIfTensorExists[secondID];
        (* Check that neither of the tensors is a coordinate system. *)
        Scan[
            If[GetTensorData[#]["Role"] === "Coordinates",
                Message[TCalc::AddErrorCoords, DisplayTensorID[#]];
                Abort[];
            ]&
            ,
            {firstID, secondID}
        ];
        (* Check that both tensors are associated with the same metric. *)
        If[GetTensorData[firstID]["Metric"] =!= GetTensorData[secondID]["Metric"],
            Message[TCalc::AddErrorMetricsMatch, DisplayTensorID[firstID], DisplayTensorID[secondID], GetTensorData[firstID]["Metric"], GetTensorData[secondID]["Metric"]];
            Abort[];
        ];
        (* Check that both tensors have the same rank. *)
        If[Length[GetTensorData[firstID]["DefaultIndices"]] =!= Length[GetTensorData[secondID]["DefaultIndices"]],
            Message[TCalc::AddErrorRanksMatch, DisplayTensorID[firstID], DisplayTensorID[secondID]];
            Abort[];
        ];
        (* Check that the index strings match the ranks of the tensors. *)
        CheckIndicesRank[firstIndices, firstID];
        CheckIndicesRank[secondIndices, secondID];
        (* Check that the index strings of both tensors are the same up to permutation. *)
        If[Sort[Characters[firstIndices]] != Sort[Characters[secondIndices]],
            Message[TCalc::AddErrorIndicesSame, DisplayTensorID[firstID], firstIndices, DisplayTensorID[secondID], secondIndices];
            Abort[];
        ];
        (* The components that will be added are the ones corresponding to the default representation of the first tensor. *)
        useIndices = GetTensorData[firstID]["DefaultIndices"];
        useCoords = GetTensorData[firstID]["DefaultCoords"];
        firstComponents = GetTensorData[firstID]["Components"][{useIndices, useCoords}];
        (* The index configuration for the second tensor will be rearranged to correctly calculate expressions like T^a_b + T_b^a. *)
        useSecondIndices = Table[useIndices[[StringPosition[firstIndices, Characters[secondIndices][[i]]][[1, 1]]]], {i, 1, StringLength[firstIndices]}];
        (* Add the appropriate index configuration to the second tensor if it does not already exist. *)
        secondComponents = AddRepresentation[secondID, useSecondIndices, useCoords];
        (* Collect the variables to be used in the addition. Both firstVars and secondVars will be the same set of variables, but potentially in a different order. *)
        allVars = Association[];
        Scan[(allVars[#] = Unique["var"])&, Characters[firstIndices]];
        firstVars = allVars[#]& /@ Characters[firstIndices];
        secondVars = allVars[#]& /@ Characters[secondIndices];
        (* Add the two tensors by summing over their components using the appropriate variables. (We do this instead of just adding the two lists to account for cases where the indices of both tensors are not in the same order.) *)
        sumComponents = Table[firstComponents[[Sequence @@ firstVars]] + secondComponents[[Sequence @@ secondVars]], Evaluate[Sequence @@ ({#, 1, Length[firstComponents]}& /@ firstVars)]];
        (* Store the result in a new temporary tensor, which will be deleted once the recursive calculation in TCalc is complete. *)
        newID = NewTempID[];
        SetTensorID[newID, Association["Components" -> Association[{useIndices, useCoords} -> sumComponents], "DefaultCoords" -> useCoords, "DefaultIndices" -> useIndices, "Metric" -> GetTensorData[firstID]["Metric"], "Role" -> "Temporary", "Symbol" -> DefaultSymbol]];
        Return[newID[firstIndices]];
    ];

(* Go over the component definitions of a given tensor and change every reference to a coordinate system to a different name. Note that this also means changing the reference in the default coordinate system of a tensor and in any coordinate transformation rules. Used by TChangeID. *)

ChangeCoordinateID[tensorID_String, oldCoordsID_String, newCoordsID_String] :=
    Module[
        {newComponents = Association[], oldComponents = GetTensorData[tensorID]["Components"], transf}
        ,
        (* For each key of the old components, if oldCoordsID is the coordinate system, convert it to newCoordsID. *)
        Scan[
            If[#[[2]] === oldCoordsID,
                newComponents[{#[[1]], newCoordsID}] = oldComponents[#]
                ,
                newComponents[#] = oldComponents[#];
            ]&
            ,
            Keys[oldComponents]
        ];
        (* Store the new components. *)
        ChangeTensorKey[tensorID, "Components", newComponents];
        (* If the default coordinate system is oldCoordsID, change it to newCoordsID. *)
        If[GetTensorData[tensorID]["DefaultCoords"] === oldCoordsID,
            ChangeTensorKey[tensorID, "DefaultCoords", newCoordsID];
        ];
        (* If the tensor object is itself a coordinate system, change oldCoordsID to newCoordsID in any coordinate transformations stored inside it. *)
        If[GetTensorData[tensorID]["Role"] === "Coordinates" && KeyExistsQ[GetTensorData[tensorID], "CoordTransformations"],
            transf = GetTensorData[tensorID]["CoordTransformations"];
            If[KeyExistsQ[transf, oldCoordsID],
                transf[newCoordsID] = transf[oldCoordsID];
                KeyDropFrom[transf, oldCoordsID];
                ChangeTensorKey[tensorID, "CoordTransformations", transf];
            ]
        ];
    ];

(* Change a particular key for the tensor object with the given ID. *)

ChangeTensorKey[tensorID_String, key_String, value_] :=
    Module[{metricID, role},
        Unprotect[TensorData];
        If[ReferenceIDQ[tensorID],
            {metricID, role} = ReferenceParts[tensorID];
            TensorData[metricID]["Derived"][role][key] = value;
            ,
            TensorData[tensorID][key] = value;
        ];
        Protect[TensorData];
    ];

(* Check if the tensor with the given ID represents a coordinate system. *)

TMessage::ErrorNotCoordinates = "The tensor \"`1`\" does not represent a coordinate system.";

CheckIfCoordinates[tensorID_String] :=
    If[GetTensorData[tensorID]["Role"] =!= "Coordinates",
        Message[TMessage::ErrorNotCoordinates, DisplayTensorID[tensorID]];
        Abort[];
    ];

(* Check if the tensor with the given ID represents a metric. *)

TMessage::ErrorNotMetric = "The tensor \"`1`\" does not represent a metric.";

CheckIfMetric[tensorID_String] :=
    If[GetTensorData[tensorID]["Role"] =!= "Metric",
        Message[TMessage::ErrorNotMetric, DisplayTensorID[tensorID]];
        Abort[];
    ];

(* Check if a tensor with the given ID exists. If overwriting is allowed, just print a warning. If it's not allowed, print an error message and abort. *)

TMessage::ErrorOverwrite = "A tensor with the ID \"`1`\" already exists. Please rename it using TChangeID[] or delete it using TDelete[] first. Type TSetAllowOverwrite[True] to allow overwriting tensors.";

TMessage::WarningOverwrite = "Overwriting the tensor \"`1`\"."

CheckIfOverwriting[tensorID_String] :=
    If[TensorIDExistsQ[tensorID],
        If[!OGReGlobalOptions["AllowOverwrite"],
            Message[TMessage::ErrorOverwrite, DisplayTensorID[tensorID]];
            Abort[]
            ,
            Message[TMessage::WarningOverwrite, DisplayTensorID[tensorID]];
        ];
    ];

(* Check if a tensor with the given ID exists, and abort if it doesn't. *)

TMessage::ErrorDoesNotExist = "The tensor \"`1`\" does not exist.";

CheckIfTensorExists[tensorID_String] :=
    If[!TensorIDExistsQ[tensorID],
        Message[TMessage::ErrorDoesNotExist, DisplayTensorID[tensorID]];
        Abort[];
    ];

(* Return the dimension of a coordinate system. *)

CoordinatesDimension[coordinatesID_String] :=
    Length[GetTensorData[coordinatesID]["Components"][{{1}, coordinatesID}]];

(* Return the dimension of an imported tensor, or None for a scalar. *)

ImportedTensorDimension[importedData_Association] :=
    If[importedData["DefaultIndices"] === {},
        None
        ,
        Length[importedData["Components"][{importedData["DefaultIndices"], importedData["DefaultCoords"]}]]
    ];

(* Check that a coordinate system used by an imported tensor already exists in the current session and has the correct dimension. A coordinate system can refer to itself before it is imported. *)

CheckImportedCoordinates[importedID_String, importedData_Association, coordinatesID_String, expectedDimension_] :=
    Module[{currentDimension},
        If[importedData["Role"] === "Coordinates" && coordinatesID === importedID,
            Return[];
        ];
        If[!TensorIDExistsQ[coordinatesID],
            Message[TImport::ErrorMissingCoordinates, DisplayTensorID[importedID], DisplayTensorID[coordinatesID]];
            Abort[];
        ];
        If[GetTensorData[coordinatesID]["Role"] =!= "Coordinates",
            Message[TImport::ErrorNotCoordinates, DisplayTensorID[importedID], DisplayTensorID[coordinatesID]];
            Abort[];
        ];
        currentDimension = CoordinatesDimension[coordinatesID];
        If[expectedDimension =!= None && currentDimension != expectedDimension,
            Message[TImport::ErrorCoordinatesDimension, DisplayTensorID[importedID], DisplayTensorID[coordinatesID], expectedDimension, currentDimension];
            Abort[];
        ];
    ];

(* Check that a metric used by an imported tensor already exists in the current session and has the correct dimension. A metric can refer to itself before it is imported. *)

CheckImportedMetric[importedID_String, importedData_Association, metricID_String, expectedDimension_] :=
    Module[{currentDimension},
        If[importedData["Role"] === "Metric" && metricID === importedID,
            Return[];
        ];
        If[!TensorIDExistsQ[metricID],
            Message[TImport::ErrorMissingMetric, DisplayTensorID[importedID], DisplayTensorID[metricID]];
            Abort[];
        ];
        If[GetTensorData[metricID]["Role"] =!= "Metric",
            Message[TImport::ErrorNotMetric, DisplayTensorID[importedID], DisplayTensorID[metricID]];
            Abort[];
        ];
        currentDimension = CoordinatesDimension[GetTensorData[metricID]["DefaultCoords"]];
        If[expectedDimension =!= None && currentDimension != expectedDimension,
            Message[TImport::ErrorMetricDimension, DisplayTensorID[importedID], DisplayTensorID[metricID], expectedDimension, currentDimension];
            Abort[];
        ];
    ];

(* Check that all coordinate systems and metrics used by an imported tensor already exist in the current session. *)

CheckImportedTensorReferences[importedID_String, importedData_Association] :=
    Module[{coordinatesIDs, dimension},
        dimension = ImportedTensorDimension[importedData];
        coordinatesIDs = DeleteDuplicates[Join[{importedData["DefaultCoords"]}, Last /@ Keys[importedData["Components"]]]];
        If[KeyExistsQ[importedData, "CoordTransformations"],
            coordinatesIDs = Join[coordinatesIDs, Keys[importedData["CoordTransformations"]]];
        ];
        If[KeyExistsQ[importedData, "Jacobians"],
            coordinatesIDs = Join[coordinatesIDs, Keys[importedData["Jacobians"]]];
        ];
        coordinatesIDs = DeleteDuplicates[coordinatesIDs];
        Scan[CheckImportedCoordinates[importedID, importedData, #, dimension]&, coordinatesIDs];
        If[KeyExistsQ[importedData, "Metric"],
            CheckImportedMetric[importedID, importedData, importedData["Metric"], dimension];
        ];
    ];

(* Check that an index list has been entered correctly: a one-dimensional list with all its components either plus or minus 1. *)

TMessage::ErrorIndexForm = "The indices must be a list of the form {\[PlusMinus]1, \[PlusMinus]1, ...}, where +1 corresponds to an upper index and -1 corresponds to a lower index.";

CheckIndicesForm[indices_List] :=
    If[!VectorQ[indices] || !AllTrue[indices, (#^2 == 1)&],
        Message[TMessage::ErrorIndexForm];
        Abort[];
    ];

(* Check that an index list matches the tensor's rank. *)

TMessage::ErrorIndexRank = "The index configuration `1` does not match the rank of the tensor \"`2`\". The number of indices should be `3`.";

CheckIndicesRank[indices_List, tensorID_String] :=
    If[Length[indices] != Length[GetTensorData[tensorID]["DefaultIndices"]],
        Message[TMessage::ErrorIndexRank, indices, DisplayTensorID[tensorID], Length[GetTensorData[tensorID]["DefaultIndices"]]];
        Abort[];
    ];

CheckIndicesRank[indices_String, tensorID_String] :=
    If[StringLength[indices] != Length[GetTensorData[tensorID]["DefaultIndices"]],
        Message[TMessage::ErrorIndexRank, "\"" <> indices <> "\"", DisplayTensorID[tensorID], Length[GetTensorData[tensorID]["DefaultIndices"]]];
        Abort[];
    ];

(* Clear all temporary tensors created by TCalc, and reset the counter. *)

ClearTemp[] :=
    (
        Scan[
            If[GetTensorData[#]["Role"] === "Temporary",
                RemoveTensorID[#];
            ]&
            ,
            TopLevelTensorIDs[]
        ];
        Unprotect[TempID];
        TempID = 0;
        Protect[TempID];
    );

(* Contract two tensors. *)

TCalc::ContractErrorMetricsMatch = "The tensors \"`1`\" and \"`2`\" cannot be contracted, as they are associated with different metrics, \"`3`\" and \"`4`\" respectively.";

ContractTensors[firstID_String[firstIndices_String], secondID_String[secondIndices_String]] :=
    Module[
        {allVars, firstComponents, firstVars, newComponents, newID, newIndices, outIndices, outVars, removeIndices, secondComponents, secondVars, sumVars, useCoords, useFirstIndices, useSecondIndices}
        ,
        (* Check that both tensors exist. *)
        CheckIfTensorExists[firstID];
        CheckIfTensorExists[secondID];
        (* Check that both tensors are associated with the same metric. *)
        If[GetTensorData[firstID]["Metric"] =!= GetTensorData[secondID]["Metric"],
            Message[TCalc::ContractErrorMetricsMatch, DisplayTensorID[firstID], DisplayTensorID[secondID], GetTensorData[firstID]["Metric"], GetTensorData[secondID]["Metric"]];
            Abort[];
        ];
        (* Check that the index strings match the ranks of the tensors. *)
        CheckIndicesRank[firstIndices, firstID];
        CheckIndicesRank[secondIndices, secondID];
        (* We perform the calculation in the default coordinate system of the first tensor. *)
        useCoords = GetTensorData[firstID]["DefaultCoords"];
        (* We start with the default index configuration of both tensors, but then rearrange the indices on the second tensor so that any contracted indices will be one upper, one lower. *)
        useFirstIndices = GetTensorData[firstID]["DefaultIndices"];
        useSecondIndices = GetTensorData[secondID]["DefaultIndices"];
        (* If either of the tensors is a scalar, simply do multiplication of tensor by scalar. *)
        If[Length[useFirstIndices] == 0,
            Return[TensorByScalar[secondID[secondIndices], GetTensorData[firstID]["Components"][{useFirstIndices, useCoords}][[1]]]]
        ];
        If[Length[useSecondIndices] == 0,
            Return[TensorByScalar[firstID[firstIndices], GetTensorData[secondID]["Components"][{useSecondIndices, useCoords}][[1]]]]
        ];
        (* newIndices will be the index configuration of the newly created tensor. We start from the default indices of the first tensor, remove any indices that were contracted, and add the remaining free indices of the second tensor. *)
        newIndices = useFirstIndices;
        removeIndices = {};
        Do[
            If[# =!= {},
                    useSecondIndices[[i]] = -useFirstIndices[[#[[1, 1]]]];
                    AppendTo[removeIndices, {#[[1, 1]]}]
                    ,
                    AppendTo[newIndices, useSecondIndices[[i]]];
                ]& @ StringPosition[firstIndices, Characters[secondIndices][[i]]]
            ,
            {i, 1, Length[useSecondIndices]}
        ];
        newIndices = Delete[newIndices, removeIndices];
        (* The components of the first tensor will be taken in the default index representation. *)
        firstComponents = GetTensorData[firstID]["Components"][{useFirstIndices, useCoords}];
        (* The components of the second tensor will be taken in the index representation that matches the contraction, with indices raised or lowered to match the corresponding indices in the first tensor. *)
        secondComponents = AddRepresentation[secondID, useSecondIndices, useCoords];
        (* Collect the variables to be used for contracting the indices.*)
        allVars = Association[];
        (* allVars will contain all of the variables for both the first and the second tensor. *)
        Scan[(allVars[#] = Unique["var"])&, Characters[firstIndices <> secondIndices]];
        (* firstVars will contain the variables to be used as indices for the first tensor. *)
        firstVars = allVars[#]& /@ Characters[firstIndices];
        (* secondVars will contain the variables to be used as indices for the second tensor. *)
        secondVars = allVars[#]& /@ Characters[secondIndices];
        (* sumVars will contain the variables to be used in the sum. *)
        sumVars = {};
        (* outVars will contain the variables to be used in the output tensor. *)
        outVars = {};
        (* outIndices will be the index string of the output tensor. *)
        outIndices = "";
        If[Length[Cases[Characters[firstIndices <> secondIndices], #]] == 2,
                AppendTo[sumVars, allVars[#]]
                ,
                AppendTo[outVars, allVars[#]];
                outIndices = outIndices <> #;
            ]& /@ Keys[allVars];
        (* Calculate the components of the new tensor by summing over the contracted variables. *)
        newComponents =
            Table[
                If[Length[sumVars] > 0,
                    Sum[firstComponents[[Sequence @@ firstVars]] * secondComponents[[Sequence @@ secondVars]], Evaluate[Sequence @@ ({#, 1, Length[firstComponents]}& /@ sumVars)]]
                    ,
                    firstComponents[[Sequence @@ firstVars]] * secondComponents[[Sequence @@ secondVars]]
                ]
                ,
                Evaluate[
                    If[Length[outVars] > 0,
                        Sequence @@ ({#, 1, Length[firstComponents]}& /@ outVars)
                        ,
                        1
                    ]
                ]
            ];
        (* Store the result in a new temporary tensor, which will be deleted once the recursive calculation in TCalc is complete. *)
        newID = NewTempID[];
        SetTensorID[newID, Association["Components" -> Association[{newIndices, useCoords} -> newComponents], "DefaultCoords" -> useCoords, "DefaultIndices" -> newIndices, "Metric" -> GetTensorData[firstID]["Metric"], "Role" -> "Temporary", "Symbol" -> DefaultSymbol]];
        Return[newID[outIndices]];
    ];

(* Take the covariant derivative of a tensor. *)

CreateUsageMessage[TCovariantD, "TCovariantD[`index`] represents the covariant derivative when used in a tensor expression given to TCalc[]. The Levi-Civita connection associated with the tensor's metric will be obtained using TChristoffel[]."];

SyntaxInformation[TCovariantD] = {"ArgumentsPattern" -> {_}};

CovariantDivOrGrad[derivativeIndex_String, tensorID_String[tensorIndices_String]] :=
    Module[
        {indexChars, myChristoffel, out, pos, replacedIndices, summationIndex = "\|040200", term, useIndices}
        ,
        (* If the tensor is a scalar, then the covariant derivative reduces to the partial derivative. *)
        If[GetTensorData[tensorID]["DefaultIndices"] === {},
            Return[DivOrGrad[derivativeIndex, tensorID[tensorIndices]]];
        ];
        (* The first term is just the partial derivative. *)
        out = DivOrGrad[derivativeIndex, tensorID[tensorIndices]];
        (* Get the Christoffel symbols associated with the tensor's metric. *)
        myChristoffel = TChristoffel[GetTensorData[tensorID]["Metric"]];
        (* Use the same index configuration as DivOrGrad: for a divergence, raise the contracted tensor index to match the lower derivative index. *)
        useIndices = GetTensorData[tensorID]["DefaultIndices"];
        pos = StringPosition[tensorIndices, derivativeIndex];
        If[pos =!= {},
            useIndices[[pos[[1, 1]]]] = 1;
        ];
        indexChars = Characters[tensorIndices];
        (* Add one Christoffel symbol per index, with a plus sign for upper indices and a minus sign for lower indices. The Unicode symbol \|040200 is used as the summation index, to prevent collisions with indices given by the user. *)
        Do[
            replacedIndices = StringReplace[tensorIndices, indexChars[[i]] -> summationIndex];
            If[useIndices[[i]] == 1,
                term = ContractTensors[TensorTrace[myChristoffel[indexChars[[i]] <> derivativeIndex <> summationIndex]], tensorID[replacedIndices]]
                ,
                term = TensorByScalar[ContractTensors[TensorTrace[myChristoffel[summationIndex <> derivativeIndex <> indexChars[[i]]]], tensorID[replacedIndices]], -1]
            ];
            out = AddTensors[out, term];
            ,
            {i, 1, Length[useIndices]}
        ];
        Return[out];
    ];

(* Create a clickable button that looks and behaves like a hyperlink. *)

CreateButton[label_, action_] :=
    Button[MouseAppearance[Mouseover[Style[label, "Hyperlink"], Style[label, "HyperlinkActive"]], "LinkHand"], action, Appearance -> "Frameless", BaseStyle -> "Hyperlink"];

Attributes[CreateButton] = HoldRest;

(* Take the divergence or gradient of a tensor. *)

CreateUsageMessage[TPartialD, "TPartialD[`index`] represents the partial derivative when used in a tensor expression given to TCalc[]."];

SyntaxInformation[TPartialD] = {"ArgumentsPattern" -> {_}};

TCalc::DerivativeErrorOneIndex = "The index specification of the partial derivative must be a string with exactly one character. If the character matches a character in the index specification of the tensor, it will be contracted with it to produce a divergence. Otherwise, the gradient will be calculated.";

DivOrGrad[derivativeIndex_String, tensorID_String[tensorIndices_String]] :=
    Module[
        {allVars, components, coordinateSymbols, firstVars, newComponents, newID, newIndices, outIndices, outVars, pos, secondVars, sumVars, useCoords, useIndices}
        ,
        (* Check that the tensor exists. *)
        CheckIfTensorExists[tensorID];
        (* Check that the derivative has exactly one index. *)
        If[StringLength[derivativeIndex] != 1,
            Message[TCalc::DerivativeErrorOneIndex];
            Abort[];
        ];
        (* Check that the index string matches the rank of the tensor. *)
        CheckIndicesRank[tensorIndices, tensorID];
        useIndices = GetTensorData[tensorID]["DefaultIndices"];
        useCoords = GetTensorData[tensorID]["DefaultCoords"];
        (* If the derivative's index matches one of the tensor's indices, we are calculating a divergence, so raise that index on the tensor (useIndices) to match the lower index on the derivative, and remove that index from the result (newIndices) since we are contracting it. Otherwise, we are calculating a gradient, so prepend a lower index to the result (newIndices) to account for the derivative's index. *)
        newIndices = useIndices;
        pos = StringPosition[tensorIndices, derivativeIndex];
        If[pos =!= {},
            useIndices[[pos[[1, 1]]]] = 1;
            newIndices = Delete[newIndices, pos[[1, 1]]]
            ,
            PrependTo[newIndices, -1];
        ];
        (* Get the tensor's components in the desired representation, adding it if it does not already exist. *)
        components = AddRepresentation[tensorID, useIndices, useCoords];
        (* Collect the variables to be used for contracting the indices. See ContractTensors for more details on how this part works. *)
        allVars = Association[];
        Scan[(allVars[#] = Unique["var"])&, Characters[derivativeIndex <> tensorIndices]];
        firstVars = allVars[#]& /@ Characters[derivativeIndex];
        secondVars = allVars[#]& /@ Characters[tensorIndices];
        sumVars = {};
        outVars = {};
        outIndices = "";
        If[Length[Cases[Characters[derivativeIndex <> tensorIndices], #]] == 2,
                AppendTo[sumVars, allVars[#]]
                ,
                AppendTo[outVars, allVars[#]];
                outIndices = outIndices <> #;
            ]& /@ Keys[allVars];
        (* Collect the coordinate symbols, since we are taking derivatives with respect to them. *)
        coordinateSymbols = GetTensorData[useCoords]["Components"][{{1}, useCoords}];
        (* Calculate the components of the new tensor by summing over the contracted variables and taking the derivatives of the tensor's components. *)
        (* If the tensor is a scalar, then the components will be a list with 1 item. This will then lead to the resulting vector being a list of lists. Replacing the components with the single component itself solves that issue. *)
        If[GetTensorData[tensorID]["DefaultIndices"] === {},
            components = components[[1]];
        ];
        newComponents =
            Table[
                If[Length[sumVars] > 0,
                    Sum[D[components[[Sequence @@ secondVars]], coordinateSymbols[[firstVars[[1]]]]], Evaluate[Sequence @@ ({#, 1, Length[coordinateSymbols]}& /@ sumVars)]]
                    ,
                    D[components[[Sequence @@ secondVars]], coordinateSymbols[[firstVars[[1]]]]]
                ]
                ,
                Evaluate[
                    If[Length[outVars] > 0,
                        Sequence @@ ({#, 1, Length[coordinateSymbols]}& /@ outVars)
                        ,
                        1
                    ]
                ]
            ];
        (* Store the result in a new temporary tensor, which will be deleted once the recursive calculation in TCalc is complete. *)
        newID = NewTempID[];
        SetTensorID[newID, Association["Components" -> Association[{newIndices, useCoords} -> newComponents], "DefaultCoords" -> useCoords, "DefaultIndices" -> newIndices, "Metric" -> GetTensorData[tensorID]["Metric"], "Role" -> "Temporary", "Symbol" -> DefaultSymbol]];
        Return[newID[outIndices]];
    ];

(* Replace TensorData with the given Association. All previously defined tensor objects will be erased. The Options key will be created if it does not already exist. *)

ImportTensorData[data_Association] :=
    (
        If[!KeyExistsQ[data, Options] || !KeyExistsQ[data[Options], "OGReVersion"] || data[Options]["OGReVersion"] != OGReVersion,
            OGRePrint["Warning: The imported tensors were created in a different version of OGRe. Compatibility issues may occur."];
        ];
        Unprotect[TensorData];
        TensorData = data;
        PopulateOptions[];
        Protect[TensorData];
    );

(* Create a spacer with the same width as an index letter. *)

IndexSpacerBaseWidth = First[ImageDimensions[Rasterize[Style["T", "DisplayFormula"]]]];

IndexSpacerUnitWidth = First[ImageDimensions[Rasterize[Style[Spacer[1], "DisplayFormula"]]]];

IndexSpacerWidth[letter_String] :=
    Module[{width},
        width = (First[ImageDimensions[Rasterize[Style[Superscript["T", letter], "DisplayFormula"]]]] - IndexSpacerBaseWidth) / IndexSpacerUnitWidth;
        Unprotect[IndexSpacerWidth];
        IndexSpacerWidth[letter] = width;
        Protect[IndexSpacerWidth];
        Return[width];
    ];

IndexSpacer[letter_String] :=
    Spacer[IndexSpacerWidth[letter]];

MakeBoxes[IndexSpace[letter_String], TeXForm] :=
    "";

MakeBoxes[IndexSpace[letter_String], form_] :=
    ToBoxes[IndexSpacer[letter], form];

(* Convert an index configuration, that is, a List with +1 for an upper index and -1 for a lower index, into labels. Used for displaying tensors using either letters or coordinates as indices. If the first argument is +1, returns a row with the upper indices, and spacers to account for the lower indices. If the first argument is -1, do the same with the lower indices. *)

IndicesToLabels[lookFor_Integer, upperLower_List, labels_List] :=
    Module[{labelStrings = ToString /@ labels},
        Row[
            MapIndexed[
                If[#1 == lookFor,
                    labels[[First[#2]]]
                    ,
                    IndexSpace[labelStrings[[First[#2]]]]
                ]&
                ,
                upperLower
            ]
        ]
    ];

(* Add upper and/or lower indices to a tensor symbol. *)

SymbolWithIndices[symbol_, upperLower_List, labels_List] :=
    Which[
        upperLower === {},
            symbol
        ,
        FreeQ[upperLower, -1],
            Superscript[symbol, Row[Take[labels, Length[upperLower]]]]
        ,
        FreeQ[upperLower, 1],
            Subscript[symbol, Row[Take[labels, Length[upperLower]]]]
        ,
        True,
            Subsuperscript[symbol, IndicesToLabels[-1, upperLower, labels], IndicesToLabels[1, upperLower, labels]]
    ];

SymbolWithIndices[symbol_, upperLower_List, letters_String] :=
    SymbolWithIndices[symbol, upperLower, Take[Characters[letters], Length[upperLower]]];

(* Create a link that will execute TInfo for a specific tensor when clicked. *)

InfoButton[tensorID_String] :=
    CreateButton[DisplayTensorID[tensorID], TInfo[tensorID]];

(* Create a link that will execute TInfo for a specific cached tensor when clicked. *)

CachedInfoButton[metricID_String, role_String] :=
    With[{tensorID = ReferenceID[metricID, role]},
        CreateButton[role, TInfo[tensorID]]
    ];

(* Create a list of tensors associated with a metric, with cached tensors grouped separately. *)

MetricTensorButtons[metricID_String] :=
    Module[{cachedRoles, entries, userTensors},
        userTensors = Select[PublicTensorIDs[], KeyExistsQ[GetTensorData[#], "Metric"] && GetTensorData[#]["Metric"] === metricID && # =!= metricID && # =!= DefaultResultID&];
        cachedRoles = CachedTensorRoles[metricID];
        entries = InfoButton /@ userTensors;
        If[cachedRoles =!= {},
            AppendTo[entries, Row[{"(Cached: ", Row[CachedInfoButton[metricID, #]& /@ cachedRoles, "|"], ")"}]];
        ];
        Return[Row[entries, "|"]];
    ];

(* Ensure that the tensor IDs given to temporary tensors are unique. *)

TempID = 0;

NewTempID[] :=
    (
        Unprotect[TempID];
        TempID++;
        Protect[TempID];
        Return["_TCalcTemp" <> ToString[TempID] <> "_"];
    );

(* Display an expression using the DisplayFormula style. The user must define what this style means manually in the notebook style sheet. *)

Nice[expression_] :=
    Style[expression, "DisplayFormula"];

(* Convert formatted expressions and tensor labels to TeX. *)

TeXStripText[tex_String] :=
    StringReplace[tex, StartOfString ~~ "\\text{" ~~ text__ ~~ "}" ~~ EndOfString :> text];

TeXAtom[expression_] :=
    TeXStripText[ToString[TeXForm[expression]]];

TeXIndexGroup[group_List] :=
    If[group[[1, 1]] === "Up",
            "^"
            ,
            "_"
        ] <> "{" <> StringRiffle[group[[All, 2]], " "] <> "}";

TeXSlotString[slots_List] :=
    Module[{groups = SplitBy[slots, First]},
        If[Length[groups] == 0,
            Return[""]
            ,
            Return[StringJoin[Riffle[TeXIndexGroup /@ groups, "{}"]]]
        ];
    ];

TeXSymbolWithIndices[symbol_, upperLower_List, letters_List] :=
    Module[{slots},
        slots =
            MapThread[
                If[#1 === 1,
                    {"Up", TeXAtom[#2]}
                    ,
                    {"Down", TeXAtom[#2]}
                ]&
                ,
                {upperLower, letters}
            ];
        Return[TeXAtom[symbol] <> TeXSlotString[slots]];
    ];

TeXSymbolWithIndices[symbol_, upperLower_List, letters_String] :=
    TeXSymbolWithIndices[symbol, upperLower, Take[Characters[letters], Length[upperLower]]];

TeXString[expression_] :=
    ToString[TeXForm[Nice[expression]]];

TeXListLabel[label_String] :=
    label;

TeXListLabel[-label_String] :=
    "-" <> label;

TeXAlignedList[positiveElements_List, negativeElements_List, values_List] :=
    "\\begin{aligned}\n" <> StringRiffle[Table[StringRiffle[TeXListLabel /@ Join[positiveElements[[i]], -negativeElements[[i]]], " = "] <> " &= " <> TeXString[values[[i]]], {i, 1, Length[values]}], " \\\\\n"] <> "\n\\end{aligned}";

(* Print an expression in an uneditable cell with the label OGRe. *)

OGRePrint[expression_] :=
    CellPrint[ExpressionCell[expression, "Output", Editable -> False, CellLabel -> "OGRe:", CellLabelStyle -> Directive["CellLabel", Smaller, Blue]]];

OGRePrint[expressions__] :=
    OGRePrint[Row[{expressions}]];

(* A special key in TensorData, Options, is used to store information about the current session, for the purpose of exporting and importing between sessions using TExportAll and TImportAll. Since this key is not a string, it cannot be accidentally overwritten by a tensor definition. *)

PopulateOptions[] :=
    Module[{useCurveParameter, useExactSignChecks, useIndexLetters, useParallelize, useReservedSymbols, useSimplifyAssumptions, useSimplifyFunc},
        If[!KeyExistsQ[TensorData, Options],
            (* If the Options key doesn't exist, which can happen when the package first loads or when importing from an old version of OGRe, create it with the default values. *)
            TensorData[Options] = Association["CurveParameter" -> DefaultCurveParameter, "ExactSignChecks" -> True, "IndexLetters" -> DefaultIndexLetters, "OGReVersion" -> OGReVersion, "Parallelize" -> False, "ReservedSymbols" -> {}, "SimplifyAssumptions" -> Association["AssumeReal" -> True, "User" -> None], "SimplifyFunc" -> FullSimplify];
            useCurveParameter = DefaultCurveParameter;
            useReservedSymbols = {}
            ,
            (* If the Options key does exist, populate it with the imported values, but substitute the default values if any keys are missing, which can happen when importing from a different version of OGRe. *)
            useCurveParameter = Lookup[TensorData[Options], "CurveParameter", DefaultCurveParameter];
            useExactSignChecks = Lookup[TensorData[Options], "ExactSignChecks", True];
            useIndexLetters = Lookup[TensorData[Options], "IndexLetters", DefaultIndexLetters];
            useParallelize = Lookup[TensorData[Options], "Parallelize", False];
            If[KeyExistsQ[TensorData[Options], "SimplifyAssumptions"],
                useSimplifyAssumptions = Association["AssumeReal" -> Lookup[TensorData[Options]["SimplifyAssumptions"], "AssumeReal", True], "User" -> Lookup[TensorData[Options]["SimplifyAssumptions"], "User", None]]
                ,
                useSimplifyAssumptions = Association["AssumeReal" -> True, "User" -> None]
            ];
            useSimplifyFunc = Lookup[TensorData[Options], "SimplifyFunc", FullSimplify];
            useReservedSymbols = Lookup[TensorData[Options], "ReservedSymbols", {}];
            TensorData[Options] = Association["CurveParameter" -> useCurveParameter, "ExactSignChecks" -> useExactSignChecks, "IndexLetters" -> useIndexLetters, "OGReVersion" -> OGReVersion, "Parallelize" -> useParallelize, "ReservedSymbols" -> {}, "SimplifyAssumptions" -> useSimplifyAssumptions, "SimplifyFunc" -> useSimplifyFunc];
        ];
        (* Reserve the symbols that should be reserved. This will also populate the key "ReservedSymbols" automatically, if the symbols are valid. *)
        TSetReservedSymbols[Evaluate[useReservedSymbols]];
        (* Clear and protect the curve parameter. *)
        Unprotect[Evaluate[useCurveParameter]];
        ClearAll[Evaluate[useCurveParameter]];
        (* Protect won't protect a symbol that hasn't been used yet if the argument is a string pattern, so we convert the string to a symbol first. *)
        useCurveParameter = Symbol[useCurveParameter];
        Protect[Evaluate[useCurveParameter]];
    ];

PopulateOptions[];

(* Change one of a tensor's indices to the desired position (upper = +1 or lower = -1), starting from a specific index configuration (assumed to be already calculated). The new components are then saved as a separate representation within the tensor object. Returns the new index configuration. *)

RaiseLower[tensorID_String, coordinatesID_String, oldIndices_List, indexPos_Integer, upperLower_Integer] :=
    Module[{allComponents, components = GetTensorData[tensorID]["Components"][{oldIndices, coordinatesID}], coordSymbols, dim, metricID = GetTensorData[tensorID]["Metric"], newComponents, newIndices, newVars, oldVars, raiseVar, sumVar, useMetric},
        dim = Length[components];
        (* If lowering the index, use the metric (with two lower indices). If raising the index, use the inverse metric (with two upper indices). If the index is not being raised or lowered, do nothing. *)
        Which[
            oldIndices[[indexPos]] == 1 && upperLower == -1,
                useMetric = AddRepresentation[metricID, {-1, -1}, coordinatesID]
            ,
            oldIndices[[indexPos]] == -1 && upperLower == 1,
                useMetric = AddRepresentation[metricID, {1, 1}, coordinatesID]
            ,
            True,
                Return[oldIndices]
        ];
        (* Geodesic equations are defined along a parameterized curve, so index transformations must use metric components evaluated on that curve. *)
        If[CurveParameterTensorQ[tensorID],
            coordSymbols = GetTensorData[coordinatesID]["Components"][{{1}, coordinatesID}];
            useMetric = useMetric /. (# -> #[$CurveParam]& /@ coordSymbols);
        ];
        (* The new indices are the same as the old indices, except for the one index being raised or lowered. *)
        newIndices = ReplacePart[oldIndices, indexPos -> upperLower];
        (* If a representation with the desired index configuration already exists, do nothing. *)
        If[KeyExistsQ[GetTensorData[tensorID]["Components"], {newIndices, coordinatesID}],
            Return[newIndices]
        ];
        (* Define the variables to be used in the calculation. As an example, say we want to lower the last index of T^abc. Then we have T^ab_c = g_cd T^abd. In this case newVars = {a, b, c}, oldVars = {a, b, d}, raiseVar = c, and sumVar = d. *)
        newVars = Unique[Table["var", {Length[newIndices]}]];
        oldVars = ReplacePart[newVars, indexPos -> sumVar];
        raiseVar = newVars[[indexPos]];
        (* Create the new components using a Table with newVars as iterators. Each component will be calculated using a Sum with sumVar as the summation variable. *)
        newComponents = (Table[Sum[useMetric[[raiseVar, sumVar]] * components[[Sequence @@ oldVars]], {sumVar, 1, dim}], ##]&) @@ ({#, 1, dim}&) /@ newVars;
        (* Store the new representation in the tensor object. *)
        allComponents = GetTensorData[tensorID]["Components"];
        allComponents[{newIndices, coordinatesID}] = TensorSimplify[newComponents];
        ChangeTensorKey[tensorID, "Components", allComponents];
        (* Return the new index configuration. *)
        Return[newIndices];
    ];

(* Remove the tensor object with the given ID. *)

RemoveTensorID[tensorID_String] :=
    Module[{metricID, role},
        Unprotect[TensorData];
        If[ReferenceIDQ[tensorID],
            {metricID, role} = ReferenceParts[tensorID];
            If[KeyExistsQ[TensorData, metricID] && AssociationQ[TensorData[metricID]] && KeyExistsQ[TensorData[metricID], "Derived"] && AssociationQ[TensorData[metricID]["Derived"]],
                KeyDropFrom[TensorData[metricID]["Derived"], role];
                If[Length[TensorData[metricID]["Derived"]] == 0,
                    KeyDropFrom[TensorData[metricID], "Derived"];
                ];
            ];
            ,
            KeyDropFrom[TensorData, tensorID];
        ];
        Protect[TensorData];
    ];

(* Create a new tensor object with the given ID, or assign it new data if it already exists. *)

SetTensorID[tensorID_String, data_Association] :=
    Module[{metricID, role},
        Unprotect[TensorData];
        If[ReferenceIDQ[tensorID],
            {metricID, role} = ReferenceParts[tensorID];
            If[!KeyExistsQ[TensorData, metricID],
                Protect[TensorData];
                Message[TMessage::ErrorDoesNotExist, metricID];
                Abort[];
            ];
            If[!KeyExistsQ[TensorData[metricID], "Derived"],
                TensorData[metricID]["Derived"] = Association[];
            ];
            TensorData[metricID]["Derived"][role] = data;
            ,
            TensorData[tensorID] = data;
        ];
        Protect[TensorData];
    ];

(* Show or list a tensor's components. Called by TShow, TList, TTeXShow, and TTeXList. *)

ShowList[showOrList_String, outputMode_String, tensorID_String, indices_List, coordinatesID_String, replace_, function_] :=
    Module[
        {allElements, components, coordSymbols, curveParameter = Symbol[TensorData[Options]["CurveParameter"]], exactCompareOrNot, exactZeroQ, grid, labels, match, negativeElements, nonZeroGroups, output, positiveElements, progress, row, showComponents, showCoords, showLabel, uniqueVal, uniqueValuesSign, uniqueValuesSignClean, useCoords, useExactSignChecks, useIndices}
        ,
        (* Check that the tensor object tensorID exists. *)
        CheckIfTensorExists[tensorID];
        (* If specific indices are not given, use the tensor's default indices. *)
        If[indices === {"_UseDefault_"},
            useIndices = GetTensorData[tensorID]["DefaultIndices"]
            ,
            (* Check that the list of indices is of the correct form and has the correct rank. *)
            CheckIndicesForm[indices];
            CheckIndicesRank[indices, tensorID];
            useIndices = indices;
        ];
        (* If a specific coordinate system is not given, use the tensor's default coordinate system. *)
        If[coordinatesID === "_UseDefault_",
            useCoords = GetTensorData[tensorID]["DefaultCoords"]
            ,
            (* Check that the tensor object coordinatesID exists and represents a coordinate system. *)
            CheckIfTensorExists[coordinatesID];
            CheckIfCoordinates[coordinatesID];
            useCoords = coordinatesID;
        ];
        (* Get the components of the tensor in the given representation (indices and coordinate system), or calculate it if it does not already exist. *)
        components = AddRepresentation[tensorID, useIndices, useCoords];
        (* If this is a tensor that uses a curve parameter, replace any instance of the curve parameter placeholder with the selected curve parameter. *)
        If[CurveParameterTensorQ[tensorID],
            components = ReplaceAll[components, $CurveParam -> Symbol[TensorData[Options]["CurveParameter"]]];
        ];
        (* If desired, map a function to the components and/or apply replacement rules to them, then simplify. *)
        If[function =!= Identity || replace =!= {},
            (* Apply the optional replacement rules to the components. *)
            If[replace =!= {},
                components = ReplaceAll[components, replace];
            ];
            (* Map the optional function to the components. *)
            If[function =!= Identity,
                components = Map[function, components, {ArrayDepth[components]}];
            ];
            (* Simplify the components in the end. *)
            components = TensorSimplify[components];
        ];
        coordSymbols = GetTensorData[useCoords]["Components"][{{1}, useCoords}];
        (* Execute the following if called by TShow. *)
        If[showOrList === "Show",
            (* Clean up the notation before printing the components. *)
            components = CleanupNotation[components, tensorID, curveParameter, coordSymbols];
            If[outputMode === "Print",
                (* Print the tensor's ID followed by a colon. *)
                row = {DisplayTensorID[tensorID], ":   "};
                (* Print the tensor's symbol along with the desired upper and lower indices. The index letters will be taken from TensorData[Options]["IndexLetters"]. *)
                row = Append[row, SymbolWithIndices[GetTensorData[tensorID]["Symbol"], useIndices, TensorData[Options]["IndexLetters"]]];
                (* Print the tensor's coordinates as function arguments, e.g. (t, x, y, z), but only if the tensor does not itself represent coordinates. *)
                If[GetTensorData[tensorID]["Role"] =!= "Coordinates",
                    row = Join[row, {"(", Row[GetTensorData[useCoords]["Components"][{{1}, useCoords}], ","], ")"}]
                ];
                (* Print an equal sign. *)
                row = Append[row, " = "];
                (* Print the components of the tensor. Use MatrixForm, unless it is a scalar. *)
                If[Length[components] * ArrayDepth[components] != 1,
                    row = Append[row, MatrixForm[components]]
                    ,
                    row = Join[row, components]
                ];
                (* Create the row to print. *)
                output = Row[row];
                ,
                (* Return TeX for the symbol, coordinates, and components, without the tensor ID. *)
                showLabel = TeXSymbolWithIndices[GetTensorData[tensorID]["Symbol"], useIndices, TensorData[Options]["IndexLetters"]];
                If[GetTensorData[tensorID]["Role"] =!= "Coordinates",
                    showCoords = "(" <> StringRiffle[TeXAtom /@ GetTensorData[useCoords]["Components"][{{1}, useCoords}], ","] <> ")"
                    ,
                    showCoords = "";
                ];
                showComponents =
                    If[Length[components] * ArrayDepth[components] != 1,
                        TeXString[MatrixForm[components]]
                        ,
                        TeXString[First[components]]
                    ];
                output = showLabel <> showCoords <> " = " <> showComponents;
            ];
        ];
        (* Execute the following if called by TList. *)
        If[showOrList === "List",
            If[AllTrue[Flatten[components], # === 0&],
                If[outputMode === "Print",
                    grid = "No non-zero elements."
                    ,
                    output = "\\begin{aligned}\n\\text{No non-zero elements.}\n\\end{aligned}";
                ];
                ,
                (* Create an array of elements of the form {value, label}, where label is the label of the specific component (with the coordinates as indices, e.g. g_xy is the component with x for the first index and y for the second index) and value is its value. *)
                allElements =
                    Flatten[
                        MapIndexed[
                            {
                                #1
                                ,
                                (* The coordinate symbols (e.g. t, x, y, z) will be used in place of the indices. *)
                                If[outputMode === "Print",
                                    SymbolWithIndices[GetTensorData[tensorID]["Symbol"], useIndices, coordSymbols[[#2]]]
                                    ,
                                    TeXSymbolWithIndices[GetTensorData[tensorID]["Symbol"], useIndices, coordSymbols[[#2]]]
                                ]
                            }&
                            ,
                            components
                            ,
                            {ArrayDepth[components]}
                        ]
                        ,
                        ArrayDepth[components] - 1
                    ];
                (* Group tensor elements by exact value. *)
                nonZeroGroups = Select[GatherBy[allElements, First], #[[1, 1]] =!= 0&];
                (* List only the tensor elements that are unique up to sign. *)
                uniqueValuesSign = {};
                positiveElements = {};
                negativeElements = {};
                useExactSignChecks = TensorData[Options]["ExactSignChecks"];
                progress = 0;
                If[useExactSignChecks && Length[nonZeroGroups] > 1,
                    PrintTemporary["Exact sign checks progress: ", ProgressIndicator[Dynamic[progress], {0, Length[nonZeroGroups]}]];
                ];
                exactZeroQ[expression_] := expression === 0 || Quiet[Check[Cancel[Together[expression]] === 0, False]] || TensorSimplify[expression] === 0;
                exactCompareOrNot[a_, b_] := exactCompareOrNot[a, b] =
                    exactCompareOrNot[b, a] =
                        If[a === b,
                            True
                            ,
                            useExactSignChecks && exactZeroQ[a - b]
                        ];
                Scan[
                    Function[valueGroup,
                        uniqueVal = valueGroup[[1, 1]];
                        labels = valueGroup[[All, 2]];
                        match = FirstPosition[uniqueValuesSign, testVal_ /; exactCompareOrNot[uniqueVal, -testVal], None, {1}, Heads -> False];
                        If[match === None,
                            AppendTo[uniqueValuesSign, uniqueVal];
                            AppendTo[positiveElements, labels];
                            AppendTo[negativeElements, {}]
                            ,
                            negativeElements[[match[[1]]]] = Join[negativeElements[[match[[1]]]], labels]
                        ];
                        progress++;
                    ]
                    ,
                    nonZeroGroups
                ];
                (* Clean up the notation before printing the components. We do this now because we wanted to compare the actual expressions, not the cleaned up ones, for the purpose of determining elements with the same value - in case the cleanup makes two different elements the same for some reason (although this shouldn't happen). *)
                uniqueValuesSignClean = CleanupNotation[uniqueValuesSign, tensorID, curveParameter, coordSymbols];
                (* Create a grid of the unique tensor elements. In each row we print all components with the same exact value, followed by components equal to minus that value, followed by the value. *)
                If[outputMode === "Print",
                    grid = Grid[Table[{Row[Join[positiveElements[[i]], -negativeElements[[i]]], "="], "=", uniqueValuesSignClean[[i]]}, {i, 1, Length[uniqueValuesSign]}], Alignment -> {Left, Baseline}]
                    ,
                    output = TeXAlignedList[positiveElements, negativeElements, uniqueValuesSignClean];
                ];
            ];
            If[outputMode === "Print",
                (* Create the grid preceded by the tensor's ID. *)
                output = Column[{Row[{DisplayTensorID[tensorID], ":"}], grid}, Alignment -> {Center, Baseline}];
            ];
        ];
        (* Print the output formatted using Nice, or return it as a TeX string. *)
        Which[
            outputMode === "Print",
                OGRePrint[Nice[output]]
            ,
            outputMode === "TeX",
                Return[output]
        ];
    ];

(* Create a nicely formatted partial derivative operator, for use by CleanupNotation. *)

PartialDerivativeOperator[arg_, order_] :=
    If[order === 1,
        Subscript["\[PartialD]", arg]
        ,
        Subsuperscript["\[PartialD]", arg, order]
    ];

(* Create a nicely formatted partial derivative expression, potentially with multiple derivative operators, for use by CleanupNotation. *)

PartialDerivativeExpression[orders_List, f_, args_List] :=
    Row[Append[PartialDerivativeOperator @@@ DeleteCases[Transpose[{args, orders}], {_, 0}], Apply[f, args]]];

(* Convert the components of a tensor to a cleaner form, for use by ShowList. *)

CleanupNotation[oldComponents_, tensorID_, curveParameter_, coordSymbols_] :=
    Module[
        {components = oldComponents}
        ,
        (* If the tensor is a curve Lagrangian or a geodesic vector, nicely format the functions and derivatives to produce a clean expression without explicitly displaying the curve parameter or the coordinates. *)
        If[CurveParameterTensorQ[tensorID],
            components = components /. Flatten[{#[curveParameter] -> #, #'[curveParameter] -> OverDot[#], #''[curveParameter] -> OverDot[#, 2]}& /@ coordSymbols];
        ];
        (* If the tensor is a geodesic vector with time as a parameter, do the same as above, with the first coordinate instead of the curve parameter. *)
        If[GetTensorData[tensorID]["Role"] === "GeodesicWithTimeParameter",
            components = components /. Flatten[{#[coordSymbols[[1]]] -> #, #'[coordSymbols[[1]]] -> OverDot[#], #''[coordSymbols[[1]]] -> OverDot[#, 2]}& /@ coordSymbols[[2 ;; ]]];
        ];
        (* Display partial derivatives in a nicer way. For example, Derivative[1, 1, 2][f][x, y, z] will be displayed as \[PartialD]_x \[PartialD]_y \[PartialD]_z^2 f[x, y, z]. If the whole derivative is raised to a power, use parentheses so the power does not look like it applies to the function itself. *)
        components = components /. {Power[Derivative[orders__][f_][args__], power_] :> Power[Row[{"(", PartialDerivativeExpression[{orders}, f, {args}], ")"}], power], Derivative[orders__][f_][args__] :> PartialDerivativeExpression[{orders}, f, {args}]};
        (* If a function is a reserved symbol and has only the coordinates as its argument(s), remove the arguments. *)
        components = components /. (f_ ? (MemberQ[Symbol /@ TensorData[Options]["ReservedSymbols"], #]&))[args__ ? (SubsetQ[coordSymbols, List[#]]&)] :> f;
        Return[components];
    ];

(* Check for updates at startup. *)

StartupCheckForUpdates[] :=
    Module[{errorMessage, latestRelease, newVersion, newVersionQ},
        errorMessage = Row[{"Could not check for updates automatically. Please visit ", Hyperlink["https://github.com/bshoshany/OGRe/releases"], " to check manually."}];
        latestRelease = LatestReleaseData[];
        Unprotect[UpdateMessage];
        If[latestRelease === $Failed,
            UpdateMessage = errorMessage
            ,
            newVersion = latestRelease["tag_name"];
            newVersionQ = NewVersionQ[newVersion];
            If[newVersionQ === $Failed,
                UpdateMessage = errorMessage
                ,
                If[!newVersionQ,
                    UpdateMessage = "You have the latest version of the package."
                    ,
                    UpdateMessage = Row[{"A new version of the package is available: ", Style[newVersion, Bold], ". For more information, type ", CreateButton["TCheckForUpdates[]", TCheckForUpdates[]], "."}];
                ];
            ];
        ];
        Protect[UpdateMessage];
    ];

(* Multiply a tensor object by a scalar. *)

TCalc::ScalarMulErrorCoords = "The tensor \"`1`\" cannot be multiplied by a scalar, as it represents a coordinate system.";

TensorByScalar[tensorID_String[indices_String], scalar_] :=
    Module[
        {newComponents, newID, useCoords, useIndices}
        ,
        (* Check that the tensor exists. *)
        CheckIfTensorExists[tensorID];
        (* Check that the tensor does not represent a coordinate system. *)
        If[GetTensorData[tensorID]["Role"] === "Coordinates",
            Message[TCalc::ScalarMulErrorCoords, DisplayTensorID[tensorID]];
            Abort[];
        ];
        (* Check that the index string matches the rank of the tensor. *)
        CheckIndicesRank[indices, tensorID];
        (* The components that will be multiplied are the ones corresponding to the default representation of the tensor. *)
        useIndices = GetTensorData[tensorID]["DefaultIndices"];
        useCoords = GetTensorData[tensorID]["DefaultCoords"];
        (* Calculate the product of the scalar with the tensor. *)
        newComponents = scalar * GetTensorData[tensorID]["Components"][{useIndices, useCoords}];
        (* Store the result in a new temporary tensor, which will be deleted once the recursive calculation in TCalc is complete. *)
        newID = NewTempID[];
        SetTensorID[newID, Association["Components" -> Association[{useIndices, useCoords} -> newComponents], "DefaultCoords" -> useCoords, "DefaultIndices" -> useIndices, "Metric" -> GetTensorData[tensorID]["Metric"], "Role" -> "Temporary", "Symbol" -> DefaultSymbol]];
        Return[newID[indices]];
    ];

(* Simplify an expression with optional user-defined assumptions. *)

TensorSimplify[expression_] :=
    Module[{assumptions, progress, result, simplifyFunc, tasks},
        assumptions =
            Join[
                {
                    If[TensorData[Options]["SimplifyAssumptions"]["AssumeReal"],
                        Element[_, Reals]
                        ,
                        None
                    ]
                }
                ,
                Flatten[{TensorData[Options]["SimplifyAssumptions"]["User"]}]
            ];
        simplifyFunc = TensorData[Options]["SimplifyFunc"];
        If[simplifyFunc === None,
            Return[expression];
        ];
        (* If the expression is not a list, or it's a list with just one component, then we don't need parallelization. *)
        If[Head[expression] =!= List || Length[expression] * ArrayDepth[expression] == 1,
            result = simplifyFunc[expression, assumptions]
            ,
            (* Print a dynamic progress indicator. The progress parameter will be shared between all kernels so they can update it. *)
            progress = 0;
            (* Share the progress parameter, simplification assumptions, and simplification function with all of the kernels. *)
            SetSharedVariable[progress, assumptions, simplifyFunc];
            PrintTemporary["Simplification progress: ", ProgressIndicator[Dynamic[progress], {0, Times @@ Dimensions[expression]}]];
            If[TensorData[Options]["Parallelize"],
                (* Submit the simplification of each element in the tensor as an individual task. Whenever a kernel becomes available, it will pick up the next available task. This results in better performance than ParallelMap. Using Block instead of Module here for maximum performance. *)
                tasks =
                    Map[
                        Function[element,
                            ParallelSubmit[
                                Block[{simplified = simplifyFunc[element, assumptions]},
                                    progress++;
                                    simplified
                                ]
                            ]
                        ]
                        ,
                        expression
                        ,
                        {ArrayDepth[expression]}
                    ];
                (* Wait for all tasks to be completed. *)
                result = WaitAll[tasks]
                ,
                (* Do the same without parallelization. *)
                result =
                    Map[
                        Function[element,
                            Block[{simplified = simplifyFunc[element, assumptions]},
                                progress++;
                                simplified
                            ]
                        ]
                        ,
                        expression
                        ,
                        {ArrayDepth[expression]}
                    ];
            ];
            UnsetShared[progress];
        ];
        Return[result];
    ];

(* Take the trace of a tensor. *)

TCalc::TraceErrorMoreThanTwo = "The index specification \"`1`\" is invalid, as it contains more than two instances of the index \"`2`\".";

TensorTrace[tensorID_String[indices_String]] :=
    Module[
        {allowedLetters, chars = Characters[indices], count, out, pos, tally, toContract}
        ,
        (* Check that the tensor exists. *)
        CheckIfTensorExists[tensorID];
        (* Tally the indices, i.e. count the multiplicities of all distinct indices. *)
        tally = Tally[chars];
        (* If no index appears twice, we do not need to take the trace. Just return the tensor as is. *)
        If[Max[tally[[All, 2]]] < 2,
            Return[tensorID[indices]];
        ];
        (* We want to replace any repeated indices by contractions with the metric, so for each contraction, we will need an arbitrary index letter to sum upon. To ensure that there are no collisions with the indices given by the user, we use characters from an unused Unicode plane. *)
        allowedLetters = CharacterRange["\|040000", "\|040100"];
        (* The list toContract will be populated with the index letters we are contracting. *)
        toContract = {};
        count = 0;
        (* Check that no indices appear more than twice. For each index that appears exactly twice, add it to the list of indices to contract, and replace it with a summation index. *)
        Scan[
            (
                If[#[[2]] > 2,
                    Message[TCalc::TraceErrorMoreThanTwo, indices, #[[1]]];
                    Abort[];
                ];
                If[#[[2]] == 2,
                    count++;
                    pos = Position[chars, #[[1]]][[2, 1]];
                    (* Each entry in toContract will be of the form {contracted letter, summation letter}. These will be the two indices used in the metric for each contracted index. *)
                    AppendTo[toContract, {chars[[pos]], allowedLetters[[count]]}];
                    chars[[pos]] = allowedLetters[[count]];
                ];
            )&
            ,
            tally
        ];
        (* Start with the tensor we are taking the trace of, with the contracted indices replaced. *)
        out = tensorID[StringJoin[chars]];
        (* Contract the metric with each traced index of this tensor. *)
        Scan[(out = ContractTensors[GetTensorData[tensorID]["Metric"][StringJoin @@ #], out])&, toContract];
        Return[out];
    ];

(* Transform the components of a tensor, with the specified index configuration, from one coordinate system to another. Returns the new components as output. *)

TMessage::ErrorNoCoordRules = "Rules for transforming coordinates from \"`1`\" to \"`2`\" have not been defined."

TransformCoordinates[tensorID_String, indices_List, sourceID_String, targetID_String] :=
    Module[
        {allComponents, dim, inverseJacobian, jacobian, newComponents, newCoordSymbols, newVars, oldComponents, oldCoordSymbols, oldVars, rank, transRules}
        ,
        (* Get the components of all existing representations. *)
        allComponents = GetTensorData[tensorID]["Components"];
        (* If a representation in the desired coordinate system already exists, do nothing. *)
        If[KeyExistsQ[allComponents, {indices, targetID}],
            Return[allComponents[{indices, targetID}]]
        ];
        (* Get the components in the source coordinate system. *)
        oldComponents = allComponents[{indices, sourceID}];
        (* Check that rules to transform from the source to the target coordinate system have been defined. *)
        If[KeyExistsQ[GetTensorData[sourceID], "CoordTransformations"] && KeyExistsQ[GetTensorData[sourceID]["CoordTransformations"], targetID],
            transRules = GetTensorData[sourceID]["CoordTransformations"][targetID]
            ,
            Message[TMessage::ErrorNoCoordRules, sourceID, targetID];
            Abort[];
        ];
        If[indices == {},
            (* If the tensor is a scalar, simply transform its one component. *)
            newComponents = TensorSimplify[oldComponents /. transRules]
            ,
            (* If the tensor is not a scalar, transform its components using contractions with the Jacobian. *)
            (* Get the rank of the tensor, corresponding to the number of variables to use. *)
            rank = ArrayDepth[oldComponents];
            (* Get the symbols (e.g. x, y, z) of the old and new coordinates, in terms of which the transformation is defined. *)
            oldCoordSymbols = GetTensorData[sourceID]["Components"][{{1}, sourceID}];
            newCoordSymbols = GetTensorData[targetID]["Components"][{{1}, targetID}];
            (* Get the dimension of the coordinates. *)
            dim = Length[oldCoordSymbols];
            (* Define the variables in terms of which to calculate the result. *)
            oldVars = Unique[Table["old", {rank}]];
            newVars = Unique[Table["new", {rank}]];
            (* If the Jacobians for this transformation have not already been calculated for some reason, calculate them now. *)
            If[!KeyExistsQ[GetTensorData[sourceID], "Jacobians"] || !AssociationQ[GetTensorData[sourceID]["Jacobians"]] || !KeyExistsQ[GetTensorData[sourceID]["Jacobians"], targetID],
                TAddCoordTransformation[sourceID, targetID, transRules];
            ];
            (* Collect the Jacobians from the object's data. *)
            jacobian = GetTensorData[sourceID]["Jacobians"][targetID]["Jacobian"];
            inverseJacobian = GetTensorData[sourceID]["Jacobians"][targetID]["InverseJacobian"];
            (* Calculate the new components by contracting each lower index with the Jacobian and each upper index with the inverse Jacobian. *)
            newComponents =
                (
                            Table[
                                (
                                        Sum[
                                            Product[
                                                    Switch[indices[[k]],
                                                        -1,
                                                            jacobian[[oldVars[[k]], newVars[[k]]]]
                                                        ,
                                                        1,
                                                            inverseJacobian[[newVars[[k]], oldVars[[k]]]]
                                                    ]
                                                    ,
                                                    {k, 1, rank}
                                                ] * (oldComponents[[##]]&) @@ oldVars
                                            ,
                                            ##
                                        ]&
                                    ) @@ ({#, 1, dim}&) /@ oldVars
                                ,
                                ##
                            ]&
                        ) @@ ({#, 1, dim}&) /@ newVars /. transRules;
            (* Simplify the result. *)
            newComponents = TensorSimplify[newComponents];
            (* If the tensor is a metric, store the inverse metric and identity matrix for future use. *)
            If[GetTensorData[tensorID]["Role"] === "Metric",
                If[indices == {-1, -1} && !KeyExistsQ[allComponents, {{1, 1}, targetID}],
                    allComponents[{{1, 1}, targetID}] = TensorSimplify[Inverse[newComponents]]
                ];
                If[indices == {1, 1} && !KeyExistsQ[allComponents, {{-1, -1}, targetID}],
                    allComponents[{{-1, -1}, targetID}] = TensorSimplify[Inverse[newComponents]]
                ];
                allComponents[{{1, -1}, targetID}] = IdentityMatrix[dim];
                allComponents[{{-1, 1}, targetID}] = IdentityMatrix[dim];
            ];
        ];
        (* Store the new components. *)
        allComponents[{indices, targetID}] = newComponents;
        ChangeTensorKey[tensorID, "Components", allComponents];
        Return[newComponents];
    ];

(* Add code completion for a given function. *)

AddCodeCompletion[function_String][args___] :=
    Module[{processed},
        processed = {args} /. {None -> 0, "AbsoluteFileName" -> 2, "RelativeFileName" -> 3, "Color" -> 4, "PackageName" -> 7, "DirectoryName" -> 8, "InterpreterType" -> 9};
        (FE`Evaluate[FEPrivate`AddSpecialArgCompletion[#1]]&)[function -> processed]
    ]

(* Update code completions for all public modules. *)

UpdateCodeCompletions[] :=
    Module[
        {allCoords, allIndexChangeableTensors, allIndexLetters, allMetrics, allNonCoords, allTensors}
        ,
        (* List all tensors except for Options (which is not actually a tensor) and DefaultResultID. *)
        allTensors = PublicTensorIDs[];
        (* List all coordinates and metrics. *)
        allCoords = Select[allTensors, GetTensorData[#]["Role"] == "Coordinates"&];
        allMetrics = Select[allTensors, GetTensorData[#]["Role"] == "Metric"&];
        (* List all tensors except coordinate systems. *)
        allNonCoords = Select[allTensors, GetTensorData[#]["Role"] =!= "Coordinates"&];
        (* List all tensors whose default index configuration can be changed. *)
        allIndexChangeableTensors = Select[allNonCoords, GetTensorData[#]["Role"] =!= "Metric"&];
        (* List all currently preferred index letters. *)
        allIndexLetters = Characters[TensorData[Options]["IndexLetters"]];
        (* AddCodeCompletion has a bug where it crashes when given an empty list. Therefore, we change empty lists to None. *)
        If[Length[allCoords] == 0,
            allCoords = None
        ];
        If[Length[allMetrics] == 0,
            allMetrics = None
        ];
        If[Length[allNonCoords] == 0,
            allNonCoords = None
        ];
        If[Length[allIndexChangeableTensors] == 0,
            allIndexChangeableTensors = None
        ];
        If[Length[allIndexLetters] == 0,
            allIndexLetters = None
        ];
        If[Length[allTensors] == 0,
            allTensors = None
        ];
        (* Add code completions for all relevant public modules. *)
        AddCodeCompletion["TAddCoordTransformation"][allCoords, allCoords, None];
        AddCodeCompletion["TChangeDefaultCoords"][allNonCoords, allCoords];
        AddCodeCompletion["TChangeDefaultIndices"][allIndexChangeableTensors, None];
        AddCodeCompletion["TChangeID"][allTensors, None];
        AddCodeCompletion["TChangeSymbol"][allTensors, None];
        AddCodeCompletion["TChristoffel"][allMetrics];
        AddCodeCompletion["TCovariantD"][allIndexLetters];
        AddCodeCompletion["TDelete"][allTensors];
        AddCodeCompletion["TDim"][allTensors];
        AddCodeCompletion["TEinstein"][allMetrics];
        AddCodeCompletion["TExport"][allTensors];
        AddCodeCompletion["TExportAll"]["AbsoluteFileName"];
        AddCodeCompletion["TGeodesicFromChristoffel"][allMetrics];
        AddCodeCompletion["TGeodesicFromLagrangian"][allMetrics];
        AddCodeCompletion["TGeodesicWithTimeParameter"][allMetrics];
        AddCodeCompletion["TGetComponents"][allTensors, allCoords, allCoords, None, None];
        AddCodeCompletion["TImportAll"]["AbsoluteFileName"];
        AddCodeCompletion["TInfo"][allTensors];
        AddCodeCompletion["TKretschmann"][allMetrics];
        AddCodeCompletion["TLagrangian"][allMetrics];
        AddCodeCompletion["TLineElement"][allMetrics, allCoords];
        AddCodeCompletion["TList"][allTensors, allCoords, allCoords, None, None];
        AddCodeCompletion["TMetricDeterminant"][allMetrics, allCoords];
        AddCodeCompletion["TNewCoordinates"][];
        AddCodeCompletion["TNewMetric"][None, allCoords, None, {"g", "h", "q", "\[Gamma]", "\[Eta]"}];
        AddCodeCompletion["TNewTensor"][None, allMetrics, allCoords, None, None, None];
        AddCodeCompletion["TNormSquared"][allNonCoords, allCoords];
        AddCodeCompletion["TPartialD"][allIndexLetters];
        AddCodeCompletion["TRank"][allTensors];
        AddCodeCompletion["TRicci"][allMetrics];
        AddCodeCompletion["TRicciScalar"][allMetrics];
        AddCodeCompletion["TRiemann"][allMetrics];
        AddCodeCompletion["TSetCurveParameter"][{"\[Lambda]", "\[Sigma]", "\[Tau]", "s"}];
        AddCodeCompletion["TSetIndexLetters"][{DefaultIndexLetters, "abcdefghijklmnopqrstuvwxyz"}];
        AddCodeCompletion["TShow"][allTensors, allCoords, allCoords, None, None];
        AddCodeCompletion["TSimplify"][allTensors];
        AddCodeCompletion["TTeXList"][allTensors, allCoords, allCoords, None, None];
        AddCodeCompletion["TTeXShow"][allTensors, allCoords, allCoords, None, None];
        AddCodeCompletion["TVolumeElement"][allMetrics, allCoords];
        AddCodeCompletion["TWeyl"][allMetrics];
    ]

(* Update the code completions on startup. *)

UpdateCodeCompletions[];

(* Check for updates at startup, if the AutoUpdates setting is turned on. *)

If[OGReGlobalOptions["AutoUpdates"],
    UpdateMessage = "Checking for updates...";
    UpdateCheck = Row[{Dynamic[UpdateMessage], " To disable automatic checks for updates at startup, type ", CreateButton["TSetAutoUpdates[False]", TSetAutoUpdates[False]], "."}];
    SessionSubmit[StartupCheckForUpdates[]]
    ,
    UpdateCheck = Row[{"To check for updates, type ", CreateButton["TCheckForUpdates[]", TCheckForUpdates[]], ". ", "To enable automatic checks for updates at startup, type ", CreateButton["TSetAutoUpdates[True]", TSetAutoUpdates[True]], "."}];
]

(* Print a welcome message at startup. *)

With[{versionTag = VersionNumberFromString[OGReVersion]["Tag"]},
    OGRePrint[Column[{Style[Row[{"OGRe: An ", Style["O", Underlined], "bject-Oriented ", Style["G", Underlined], "eneral ", Style["Re", Underlined], "lativity Package for Mathematica"}], Bold, Larger], Style[Row[{"By Barak Shoshany (", Hyperlink["baraksh@gmail.com", "mailto:baraksh@gmail.com"], ") (", Hyperlink["baraksh.com", "https://baraksh.com/"], ")"}], Bold], Style[Row[{OGReVersion}], Bold], Style[Row[{"GitHub repository: ", Hyperlink["https://github.com/bshoshany/OGRe"]}], Bold], Style[Row[{"Release notes: ", Hyperlink["view on GitHub", ReleaseURLFromTag[versionTag]], " | ", CreateButton["view in this notebook", ShowReleaseNotesForTag[versionTag]]}], Bold], Row[{"\[Bullet] To view the full documentation for the package, type ", CreateButton["TDocs[]", TDocs[]], "."}], Row[{"\[Bullet] To list all available modules, type ", CreateButton["?OGRe`*", OGRePrint[Information["OGRe`*"]]], "."}], Row[{"\[Bullet] To get help on a particular module, type ", Style["?", "Input"], " followed by the module name."}], Row[{"\[Bullet] To enable parallelization, type ", CreateButton["TSetParallelization[True]", TSetParallelization[True]], "."}], Row[{"\[Bullet] ", UpdateCheck}]}]];
];

End[];

(* Protect all OGRe symbols so they will not be accidentally overwritten elsewhere. *)

Protect["OGRe`*"];

Protect["OGRe`Private`*"];

EndPackage[]; (* OGRe *)
