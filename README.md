<!-- remove-before-compile -->
[![Author: Barak Shoshany](https://img.shields.io/badge/author-Barak_Shoshany-009933)](https://baraksh.com/)
[![DOI: 10.21105/joss.03416](https://joss.theoj.org/papers/10.21105/joss.03416/status.svg)](https://doi.org/10.21105/joss.03416)
[![arXiv:2109.04193](https://img.shields.io/badge/arXiv-2109.04193-b31b1b.svg)](https://arxiv.org/abs/2109.04193)
[![License: MIT](https://img.shields.io/github/license/bshoshany/OGRe)](https://github.com/bshoshany/OGRe/blob/master/LICENSE.txt)
![Language: Mathematica 14](https://img.shields.io/badge/Language-Mathematica%2014-yellow)
[![GitHub stars](https://img.shields.io/github/stars/bshoshany/OGRe?style=flat&color=009999)](https://github.com/bshoshany/OGRe/stargazers)
[![GitHub forks](https://img.shields.io/github/forks/bshoshany/OGRe?style=flat&color=009999)](https://github.com/bshoshany/OGRe/forks)
[![GitHub release](https://img.shields.io/github/v/release/bshoshany/OGRe?color=660099)](https://github.com/bshoshany/OGRe/releases)
[![Open in Visual Studio Code](https://img.shields.io/badge/Open_in_Visual_Studio_Code-007acc)](https://vscode.dev/github/bshoshany/OGRe)
<!-- /remove-before-compile -->

# OGRe: An **O**bject-Oriented **G**eneral **Re**lativity Package for Mathematica

By **Barak Shoshany**\
Email: <baraksh@gmail.com>\
Website: <https://baraksh.com/>\
GitHub: <https://github.com/bshoshany>

Project GitHub repository: [https://github.com/bshoshany/OGRe](https://github.com/bshoshany/OGRe)\
This is the complete documentation for **v2.0.0** of the package, released on **2026-05-02**.

<!-- remove-before-compile -->
<div style="color: red">

**Note: While this Markdown document can be read on its own, it is meant to be compiled into a Mathematica notebook so that the output of the executed statements will be shown. Some parts will not make sense without seeing the output. The files [`OGRe_Documentation.nb`](https://github.com/bshoshany/OGRe/blob/master/OGRe_Documentation.nb) and [`OGRe_Documentation.pdf`](https://github.com/bshoshany/OGRe/blob/master/OGRe_Documentation.pdf) are the compiled notebook versions of this Markdown documentation, including all cell outputs.**

</div>
<!-- /remove-before-compile -->

<!-- remove-before-compile -->
* [Introduction](#introduction)
    * [Overview](#overview)
    * [Features](#features)
    * [The object-oriented design philosophy](#the-object-oriented-design-philosophy)
* [Installing and loading the package](#installing-and-loading-the-package)
* [Getting started](#getting-started)
* [Creating and displaying tensor objects](#creating-and-displaying-tensor-objects)
    * [Defining coordinates](#defining-coordinates)
    * [Defining metrics](#defining-metrics)
    * [Displaying tensors](#displaying-tensors)
    * [Line and volume elements](#line-and-volume-elements)
    * [Choosing index letters](#choosing-index-letters)
    * [Creating tensors in a given manifold](#creating-tensors-in-a-given-manifold)
    * [Tensor ID completion](#tensor-id-completion)
* [Operations on single tensors](#operations-on-single-tensors)
    * [Changing the symbol or ID of a tensor](#changing-the-symbol-or-id-of-a-tensor)
    * [Deleting and overwriting tensors](#deleting-and-overwriting-tensors)
    * [Raising and lowering indices](#raising-and-lowering-indices)
    * [Coordinate transformations](#coordinate-transformations)
    * [Applying replacement rules or a function to the tensor components](#applying-replacement-rules-or-a-function-to-the-tensor-components)
    * [Customizing the simplification function](#customizing-the-simplification-function)
    * [Setting simplification assumptions](#setting-simplification-assumptions)
    * [Getting the components of a tensor](#getting-the-components-of-a-tensor)
    * [Getting the dimension and rank of a tensor](#getting-the-dimension-and-rank-of-a-tensor)
    * [Getting information about tensors](#getting-information-about-tensors)
* [Calculations with tensors](#calculations-with-tensors)
    * [The `TCalc` module](#the-tcalc-module)
    * [Addition of tensors](#addition-of-tensors)
    * [Multiplication of a tensor by a scalar](#multiplication-of-a-tensor-by-a-scalar)
    * [Taking traces and contracting tensors](#taking-traces-and-contracting-tensors)
    * [Calculating the norm squared](#calculating-the-norm-squared)
* [Derivatives and curvature tensors](#derivatives-and-curvature-tensors)
    * [The Christoffel symbols](#the-christoffel-symbols)
    * [The Riemann tensor](#the-riemann-tensor)
    * [The Ricci tensor and scalar](#the-ricci-tensor-and-scalar)
    * [The Kretschmann scalar](#the-kretschmann-scalar)
    * [The Einstein tensor](#the-einstein-tensor)
    * [The Weyl tensor](#the-weyl-tensor)
    * [Covariant derivatives](#covariant-derivatives)
    * [Overwriting metrics](#overwriting-metrics)
* [Curves and geodesics](#curves-and-geodesics)
    * [The curve Lagrangian](#the-curve-lagrangian)
    * [Geodesic equations from the Lagrangian](#geodesic-equations-from-the-lagrangian)
    * [Geodesic equations from the Christoffel symbols](#geodesic-equations-from-the-christoffel-symbols)
    * [Changing the curve parameter](#changing-the-curve-parameter)
    * [Geodesic equations in terms of the time coordinate](#geodesic-equations-in-terms-of-the-time-coordinate)
* [Importing and exporting tensors](#importing-and-exporting-tensors)
* [Parallelization](#parallelization)
    * [Improving performance with parallelization](#improving-performance-with-parallelization)
    * [Making use of OGRe's parallelized simplifications for other Mathematica expressions](#making-use-of-ogres-parallelized-simplifications-for-other-mathematica-expressions)
* [About the project](#about-the-project)
    * [Bug reports and feature requests](#bug-reports-and-feature-requests)
    * [Contribution and pull request policy](#contribution-and-pull-request-policy)
    * [Starring the repository](#starring-the-repository)
    * [Acknowledgements](#acknowledgements)
    * [Copyright and citing](#copyright-and-citing)
* [Other projects to check out](#other-projects-to-check-out)
<!-- /remove-before-compile -->

## Introduction

### Overview

OGRe is a modern Mathematica package for differential geometry and tensor calculus, designed to be both powerful and user-friendly. It can be used in a variety of contexts where tensor calculations are needed, in both mathematics and physics, but it is especially suitable for general relativity.

Tensors are abstract objects, which can be represented as multi-dimensional arrays once a choice of index configuration and coordinate system is made. OGRe stays true to this definition, but takes away the complexities that come with combining tensors in different representations. This is done using an object-oriented programming approach, taking advantage of principles such as encapsulation and class invariants to eliminate the possibility of user error.

The user initially defines each tensor in OGRe using its explicit components in any single representation. Operations on this tensor are then done abstractly, without needing to specify which representation to use. Possible operations include addition of tensors, multiplication of a tensor by a scalar, trace, contraction, and partial and covariant derivatives.

OGRe will automatically choose which representation to use for each tensor based on how the tensors are combined. For example, if two tensors are added, then OGRe will automatically use the same index configuration for both. Similarly, if two tensors are contracted, then OGRe will automatically ensure that the contracted indices are one upper (contravariant) and one lower (covariant). OGRe will also automatically transform all tensors being operated on to the same coordinate system.

Transformations between representations are done behind the scenes; all the user has to do is specify which metric to use for raising and lowering indices, and how to transform between the coordinate systems being used. This also means that there is no room for user error. The user cannot mistakenly perform "illegal" operations such as $2{A}^{\mu\nu}+{B}_{\mu\lambda} {C}_{\lambda\nu}$. Instead, the user simply inputs the names of the tensors, the order (but **not** the configuration) of indices for each, and the operations to perform - and the correct combination $2{A}^{\mu\nu} {{B}^{\mu}}_{\lambda} {C}^{\lambda\nu}$ will be automatically deduced.

I initially created OGRe for use in my own research, so I made it as flexible and powerful as possible. I also wanted my students to be able to use it easily and efficiently, even if they only have minimal experience with Mathematica and/or general relativity, so I made it simple to learn and easy to use. As a result, this package is equally suitable for both experienced and novice researchers.

### Features

* Define coordinate systems and the transformation rules between them. Tensor components are then transformed automatically between coordinates behind the scenes as needed.
* Each tensor is associated with a specific metric. Tensor components are then transformed automatically between different index configurations, raising and lowering indices behind the scenes as needed.
* Get the components of any tensor in any index configuration and coordinate system. Display or generate TeX code for the components, either in vector/matrix form or as a list of all unique non-zero elements. Metrics can also be displayed as a line element. Arbitrary functions or replacement rules can easily be applied to the components before they are displayed.
* Automatically simplify tensor components, optionally with user-defined simplification functions and/or assumptions. Simplifications can be parallelized for a significant performance boost. The user may utilize the package's simplification algorithm for any Mathematica expression, not just tensors.
* Export tensors to a Mathematica notebook or to a file, so they can later be imported into another Mathematica session without having to redefine them from scratch.
* Highly customizable. User settings are exported and imported along with the tensors. Some settings are persistent between sessions.
* Easily calculate arbitrary tensor formulas using any combination of addition, multiplication by a scalar, trace, contraction, partial derivative, and covariant derivative.
* Built-in modules for calculating the Christoffel symbols (Levi-Civita connection), Riemann tensor, Ricci tensor and scalar, Kretschmann scalar, Einstein tensor, Weyl tensor, curve Lagrangian, and volume element from a metric.
* Calculate the norm-squared of tensors of any rank, and the determinant and volume element of any metric.
* Calculate the geodesic equations in terms of a user-defined affine curve parameter, in two different ways: from the Christoffel symbols or from the curve Lagrangian. For spacetime metrics, the geodesic equations can be calculated in terms of the time coordinate.
* Designed with speed and performance in mind, using optimized algorithms designed specifically for this package. Derived tensors are cached on first use to avoid recalculating.
* Tensor IDs are auto-completed in the notebook interface when used with any suitable OGRe module, making the interface more user-friendly and easy to use.
* Fully portable. Can be imported directly from the web into any Mathematica notebook, without downloading or installing anything. Integrates seamlessly with the Wolfram Cloud.
* Clear and detailed documentation, with many examples, in both [Mathematica notebook](https://github.com/bshoshany/OGRe/blob/master/OGRe_Documentation.nb) and [PDF](https://github.com/bshoshany/OGRe/blob/master/OGRe_Documentation.pdf) format. The notebook `OGRe_Documentation.nb` can also be opened with the free [Wolfram Player](https://www.wolfram.com/player/). Detailed usage messages are also provided.
* Open source. The code is extensively documented; please feel free to fork and modify it as you see fit.
* Under continuous and active development. Bug reports and feature requests are welcome, and should be made via [GitHub issues](https://github.com/bshoshany/OGRe/issues).
* Thoroughly tested. A comprehensive internal test suite thoroughly tests all OGRe modules and verifies that it accurately reproduces a wide variety of known results from differential geometry and general relativity, ensuring users can fully trust OGRe as a reliable tool for research and pedagogy.

### The object-oriented design philosophy

**Object-oriented programming** refers to a paradigm where a program's code is organized around objects. An **object** belongs to a user-defined type, called a **class**. The class defines the **data** that the object stores, as well as **methods** or **member functions** that read or manipulate that data. One of the fundamental principles of object-oriented programming is **encapsulation**, which means that the user may only access an object's data using the methods defined by the class, and is unable to access the object's data directly.

Importantly, encapsulation allows for the preservation of **class invariants**. An invariant is a condition of validity that can always be assumed to be satisfied by the data stored in each object. If the methods make sure to preserve the invariant whenever they store or manipulate the data, and the user is prevented from changing the data manually and thus potentially violating the invariant, then the implementation of the class can be greatly simplified, and performance can be improved, because we will not need to verify that the data is valid every time we perform an operation.

The main idea behind OGRe is to simplify the use of tensors by encoding all the information about a tensor in a single, self-contained object. As we mentioned above, a tensor is an abstract object. We can find components which represent this abstract entity in a particular coordinate system and index configuration, but the tensor is **not** its components. In OGRe, a tensor object is initially defined (or **constructed**) by providing the components of the tensor in a particular representation - but once this is done, the user does not need to worry about coordinates or indices anymore, or even remember which coordinates and indices were initially used. The abstract tensor object will automatically transform the initial data to a different coordinate system or index configuration as needed, based on the context in which it was used.

As a tensor object holds the components of the same tensor in many different representations, the most important class invariant is the assumption that the different components indeed represent the same tensor. This is achieved using encapsulation; the object's data can only be modified by private methods that preserve the invariant, and thus the user cannot accidentally cause a violation of the invariant by assigning components to one representation that are not related to the components of all other representations by the appropriate coordinate and/or index transformation.

Unfortunately, Mathematica does not have built-in support for object-oriented programming. However, version 10.0 of Mathematica, released in 2014, introduced the `Association` symbol. An `Association` is an **associative array**; it is similar to a `List`, except that instead of being just an array of values, an `Association` is a list of keys with a value associated to each key. This allows us to easily implement a rudimentary form of object-oriented programming, storing the properties of each object in the keys of a corresponding `Association`.

Of course, as Mathematica is not truly object-oriented, there is no actual "tensor class" defined anywhere in the package. Instead, the tensor class exists only **implicitly**, as a design paradigm. Furthermore, the functions that process the data stored in the tensor objects are not methods of a class, they are simply modules that take tensor objects as input and/or produce tensor objects as outputs. (In earlier versions, I tried using a syntax that resembled method syntax in languages such as C++ or Python, but eventually decided against it, as it was too cumbersome.) Still, designing the package with object-oriented programming in mind allows us to reap many of this paradigm's benefits, as explained above - and it simply makes sense for tensors, due to their abstract and multifaceted nature.

## Installing and loading the package

This package is compatible with Mathematica 14.0 or later. It consists of only one file, `OGRe.m`. There are several different ways to load the package:

* **Run from local file with installation:** This is the recommended option, as it allows you to permanently use the package offline from any Mathematica notebook. Download the file `OGRe.m` from [the GitHub repository](https://github.com/bshoshany/OGRe) and copy it to the directory given by `FileNameJoin[{$UserBaseDirectory,"Applications"}]`. The package may now be loaded from any notebook by writing ``Needs["OGRe`"]`` (note the backtick `` ` `` following the word OGRe).
* **Run from local file without installation:** This option allows you to use the package in a portable fashion, without installing it in the `Applications` directory. Download the file `OGRe.m` from [the GitHub repository](https://github.com/bshoshany/OGRe), place it in the same directory as the notebook you would like to use, and use the command `Get["OGRe.m",Path->NotebookDirectory[]]` to load the package.
* **Run from web with installation:** This option allows you to automatically download and install the package on any computer. Simply run the command `URLDownload["https://raw.githubusercontent.com/bshoshany/OGRe/master/OGRe.m",FileNameJoin[{$UserBaseDirectory,"Applications","OGRe.m"}]]` from any Mathematica notebook to permanently install the package. Then use ``Needs["OGRe`"]`` from any notebook to load it.
* **Run from web without installation:** This option allows you to use the package from any Mathematica notebook on any computer, without manually downloading or installing it, as long as you have a working Internet connection. It also ensures that you always use the latest version of the package, but be aware that updates may sometimes not be fully backwards compatible. Simply write `Get["https://raw.githubusercontent.com/bshoshany/OGRe/master/OGRe.m"]` in any Mathematica notebook to load the package.

Note: In the "run from web" options, if you wish to load a specific version for compatibility reasons, replace `master` in the URL with the full version number including the `v` prefix, e.g. `https://raw.githubusercontent.com/bshoshany/OGRe/v2.0.0/OGRe.m` for v2.0.0.

To **uninstall** the package, just delete the file from the `Applications` directory, which can be done from within Mathematica using the command `DeleteFile[FileNameJoin[{$UserBaseDirectory,"Applications","OGRe.m"}]]`.

## Getting started

For the purposes of this documentation, I will use the "run from local file without installation" option, since you most likely downloaded both the documentation and the package together:

```wl
Get["OGRe.m",Path->NotebookDirectory[]]
```

The package displays a welcome message upon loading, which provides some information on how to get started. As stated in the welcome message, to list all of the modules available in this package, you may use the following command:

```wl
?OGRe`*
```

Clicking on the name of any module in this list will show its usage message. Notice that all OGRe modules start with the letter `T`, to help distinguish them from other modules, whether built-in or from other packages.

When the package loads, it will automatically check the GitHub repository for updates. This can also be done manually using the module `TCheckForUpdates`. If a new version of the package has been released, you will be given the option to download or install it in one click. If you are running the package directly from the web, you will always be using the latest version.

```wl
?TCheckForUpdates
```

You can also disable automatic checks for updates at startup in all future sessions by calling `TSetAutoUpdates[False]`:

```wl
?TSetAutoUpdates
```

If at any point you wish to view this documentation, simply type `TDocs[]`. The file [`OGRe_Documentation.nb`](https://github.com/bshoshany/OGRe/blob/master/OGRe_Documentation.nb) will then be automatically opened in Mathematica.

```wl
?TDocs
```

## Creating and displaying tensor objects

### Defining coordinates

To define tensors, we first need to define the manifold on which they reside. Since we are focusing on general relativity, we will use 4-dimensional spacetime manifolds in the following examples, but this package works equally well with manifolds that are purely spatial and/or have a different number of dimensions.

The first step is to define the coordinate system. In OGRe, coordinates are represented as a special tensor object: a vector ${x}^{\mu}$ (a tensor of rank 1) representing a point. To define the coordinates, we use the module `TNewCoordinates`:

```wl
?TNewCoordinates
```

For example, to define the **Cartesian coordinate system**, we use the following syntax:

```wl
TNewCoordinates["Cartesian",{t,x,y,z}]
```

The first argument is the new tensor object's **unique ID**. This is the string that we will use to refer to this tensor from now on, and it will also be displayed whenever we print the tensor. The ID string is case-sensitive, can include any characters, and can be of any length, but it is recommended to keep it short and simple. Once a tensor object is created, you cannot create another tensor object with the same ID unless you rename or delete it first (see below). Note that the ID string is also the return value of the module; generally, all modules that operate on a tensor object will return its ID string as output. This allows us to compose different modules together, as we will see below.

The second argument is the list of coordinates. Note that the order of coordinates matters, as it will determine the order of components in the tensors defined on this manifold. The symbols used also matter, as tensor components will usually be functions of these symbols. We can similarly define the spherical coordinate system:

```wl
TNewCoordinates["Spherical",{t,r,\[Theta],\[Phi]}]
```

It is crucial that the coordinate symbols, in this case `t`, `r`, `\[Theta]`, `\[Phi]`, remain as **undefined** symbols throughout the calculation, otherwise errors may occur. For example, if our metric contains functions of `r`, and at some point in the notebook we set `r = 1`, then Mathematica will replace every instance of `r` with `1`, which means those abstract functions will be replaced with their values evaluated at `r = 1`. Furthermore, if we, for example, want to take the derivative with respect to `r` (e.g. for the purpose of calculating various curvature tensors), this will not be possible, since one cannot take a derivative with respect to a number.

To prevent such errors, `TNewCoordinates` automatically clears any previous definitions of the given symbols, and also protects them against future changes. This is done using the `TSetReservedSymbols` module:

```wl
?TSetReservedSymbols
```

Indeed, if we now try to give `r` the value `1`, we will get an error:

```wl
r=1
```

You can also use `TSetReservedSymbols` manually for any constants or parameters used in the tensor, as we will demonstrate in the next section. Note that it is always possible to replace reserved symbols with numbers or other expressions using `/.` or `ReplaceAll`, so giving a value to a reserved symbol directly is not something you will ever need to do.

### Defining metrics

To finish defining a manifold, we need to define its metric tensor. For this, we use the module `TNewMetric`:

```wl
?TNewMetric
```

Again, we must input a unique ID for the new tensor object corresponding to the metric. We also input the unique ID of a coordinate system created using `TNewCoordinates`. This coordinate system is the one in which the components of the metric are initially given, but they will be automatically transformed to other coordinate systems later as needed. Note that the components are assumed to be the representation of the metric with two lower indices, since that is how metrics are usually defined; one upper and one lower index is just the identity matrix, and two upper indices is the inverse metric. Optionally, we can also specify a symbol to be used for representing the metric.

Let us use this module to create a tensor object for the **Minkowski metric**, specifying the components in Cartesian coordinates:

```wl
TNewMetric["Minkowski","Cartesian",DiagonalMatrix[{-1,1,1,1}],"\[Eta]"]
```

As with `TNewCoordinates`, we received the ID string for the new tensor object as output.

Similarly, let us define the **Schwarzschild metric**, this time specifying the components in spherical coordinates. However, before we can safely do so, we should take one additional step. When we defined the coordinates using `TNewCoordinates`, the symbols used were automatically reserved, that is, cleared and protected from future changes, using `TSetReservedSymbols`. However, `TNewMetric` does not automatically reserve any symbols it finds in the metric, since you might not want to reserve some of them. Since the Schwarzschild metric has a free parameter `M`, the mass of the black hole, we must reserve this symbol manually:

```wl
TSetReservedSymbols[M]
```

Note that `TSetReservedSymbols` returned the full list of symbols we reserved so far: the coordinate symbols and the black hole's mass.

Now we can define the Schwarzschild metric:

```wl
TNewMetric["Schwarzschild","Spherical",DiagonalMatrix[{-(1-(2*M)/r),1/(1-(2*M)/r),r^2,r^2*Sin[\[Theta]]^2}]]
```

Note that since we did not specify a symbol, the symbol `g` will be used by default, as demonstrated below.

### Displaying tensors

OGRe contains two modules for displaying the contents of tensors. The first one is `TShow`, which shows the ID, symbol, indices, coordinates, and components in those indices and coordinates, in vector or matrix form when applicable:

```wl
?TShow
```

Coordinates are also tensor objects, so we can use `TShow` to show the two coordinate tensors we defined above:

```wl
TShow["Cartesian"]
```

```wl
TShow["Spherical"]
```

Note that coordinate tensors always have the symbol $x$.

We can also show the two metrics we created using these coordinates:

```wl
TShow["Minkowski"]
```

```wl
TShow["Schwarzschild"]
```

The other module available in OGRe for displaying the contents of tensors is `TList`, which lists all of the **unique (up to sign), non-zero** components of the tensor. It is usually the best option for higher-rank tensors, which cannot be displayed in vector or matrix form, such as the Christoffel symbols or Riemann tensor (see below). Its syntax is:

```wl
?TList
```

For example:

```wl
TList["Minkowski"]
```

```wl
TList["Schwarzschild"]
```

Note that both `TShow` and `TList` display their outputs using the `DisplayFormula` Notebook style. It's up to the user to decide how to define this style; in this notebook, I used a font size of 20, aligned to center. The style may be easily changed by clicking on the "Format" menu in Mathematica and selecting "Edit Stylesheet". Then, choose the `DisplayFormula` style, select that cell, and modify its format using the "Format" menu.

If, as in the examples above, no additional arguments are given to `TShow` and `TList`, they display the tensors in their **default indices** and **default coordinates**, which are the ones first used to define the tensor (unless you change them later). So, for example, the default indices of the Minkowski metric are two lower indices, and its default coordinates are Cartesian. We will show later how to change these defaults, and how to display any tensor in any index configuration and coordinate system.

By default, `TList` uses **exact sign checks** to detect when two components are the same up to a sign. This is controlled by `TSetExactSignChecks[]`:

```wl
?TSetExactSignChecks
```

When exact sign checks are enabled, if `TList` cannot detect equality of two components up to sign by direct comparison, it checks whether the sum of the components can be proven to equal zero. It first tries a fast exact algebraic zero test, and if that fails, it falls back to the configured simplification function (see below). For tensors with complicated components, this may take a long time, in which case this option can be disabled using `TSetExactSignChecks[False]`; however, `TList` will then only be able to detect when two components are the same up to sign in simple cases.

Often, we might want to copy the output of `TShow` or `TList` into a LaTeX document. Copying the output cells directly in Mathematica, or even using "Copy As > LaTeX", usually won't produce good results, due to the way these modules structure the notebook output so it is displayed nicely. Instead, the modules `TTeXShow` and `TTeXList` accept the same arguments as `TShow` and `TList`, respectively, but return the formatted output as a TeX string instead of displaying it in the notebook. For example:

```wl
?TTeXShow
```

```wl
?TTeXList
```

Note that `TTeXShow` and `TTeXList` return the TeX output as a string. If we run them in an input cell, the string will be placed in an output cell, and if we just select that string and copy it, Mathematica will usually mess up the formatting, resulting in invalid TeX code. Instead, the best way to copy the TeX output is to use `CopyToClipboard` to copy it directly to the clipboard, for example:

```wl
CopyToClipboard@TTeXShow["Schwarzschild"]
```

### Line and volume elements

The **line element** for a metric ${g}_{\mu\nu}$ is defined as

$$
{\mathrm{d}s}^{2} = {g}_{\mu\nu} {\mathrm{d}x}^{\mu} {\mathrm{d}x}^{\nu}.
$$

In OGRe, we can calculate the line element using the module `TLineElement`:

```wl
?TLineElement
```

For example:

```wl
TLineElement["Minkowski"]
```

```wl
TLineElement["Schwarzschild"]
```

Note that these are standard Mathematica expressions, so they can be manipulated like any other expressions, including operations such as simplifying or factoring. As an example of a more interesting (non-diagonal) line element, consider the **Alcubierre warp drive metric**:

```wl
TSetReservedSymbols[{v,f}];
TNewMetric["Alcubierre","Cartesian",{{-1+v[t]^2*f[t,x,y,z]^2,0,0,-v[t]*f[t,x,y,z]},{0,1,0,0},{0,0,1,0},{-v[t]*f[t,x,y,z],0,0,1}}];
```

This is what the metric looks like in matrix form:

```wl
TShow["Alcubierre"]
```

$f$ is a **form function** which is equal to 1 inside a "warp bubble" of finite radius and 0 outside it, and $v$ is the velocity of the bubble, which can be faster than the speed of light ($v > 1$). Note that we reserved both $v$ and $f$, since they are parameters used in the metric. Here we see another advantage of reserving symbols: since `v` and `f` are reserved symbols, and they are functions of the coordinates only, their arguments are not shown when using `TShow` and `TList`, for improved readability.

It is easy to see that the metric is flat where $f = 0$, that is, outside the bubble. Its line element is:

```wl
lineElement=TLineElement["Alcubierre"]
```

We can simplify it by doing some clever factorization:

```wl
lineElement=lineElement//Expand//(#[[1]]+#[[2]]+#[[3]]+Factor[#[[4]]+#[[5]]+#[[6]]])&
```

In this form, it is immediately clear that the metric is flat outside the warp bubble (where $f$ is 0), and inside the warp bubble (when $f$ is 1) it is a flat metric translated by an amount $v(t)\,\mathrm{d}t$ in the $z$ direction:

```wl
lineElement/.f->(0&)
```

```wl
lineElement/.f->(1&)
```

Another thing we can do with a metric is calculate its **volume element**, defined as

$$
\mathrm{d}V = \sqrt{|\det(g)|}\,{\mathrm{d}x}^{0}\wedge \ldots \wedge {\mathrm{d}x}^{n-1},
$$

where $n$ is the number of dimensions. This can be done using the module `TVolumeElement`:

```wl
?TVolumeElement
```

For example:

```wl
TVolumeElement["Minkowski"]
```

```wl
TVolumeElement["Schwarzschild"]
```

```wl
TVolumeElement["Alcubierre"]
```

Note that the volume element involves the square root of the absolute value of the determinant of the metric. If we want the determinant itself, we can use `TMetricDeterminant`:

```wl
?TMetricDeterminant
```

For example:

```wl
TMetricDeterminant["Schwarzschild"]
```

### Choosing index letters

By default, the `TShow` module uses Greek letters for the indices, in a specific order. The letters can be displayed or changed using the `TSetIndexLetters` module:

```wl
?TSetIndexLetters
```

The default letters are:

```wl
TSetIndexLetters[]
```

This means that the letter $\mu$ will be used for the first index, $\nu$ for the second, and so on. However, sometimes we want to use different letters. For example, let us change the indices to lowercase English letters:

```wl
TSetIndexLetters["abcdefghijklmnopqrstuvwxyz"]
```

`TShow` will now use these letters - in this particular order - when displaying tensors:

```wl
TShow["Minkowski"]
```

Finally, let us reset the letters to the default:

```wl
TSetIndexLetters[Automatic]
```

Note that `TList` always uses the coordinate symbols themselves for the indices (e.g. ${\eta}_{tt}$, ${\eta}_{xx}$, etc.), so it is not affected by `TSetIndexLetters`.

### Creating tensors in a given manifold

Any tensors other than coordinates and metrics are created using the module `TNewTensor`:

```wl
?TNewTensor
```

In OGRe, all tensor objects must have an **associated metric** - except coordinate objects, and the metric tensors themselves. This is because OGRe automatically raises and lowers indices as appropriate for various operations such as adding and contracting tensors, and it cannot do so without knowing which metric to use. Even scalars, which have no indices, should still be associated to a specific metric - since they can multiply other tensors, and you cannot multiply tensors from different manifolds together.

The index configuration of the tensor is a 1-dimensional `List`. The number of indices is the rank of the tensor. Each element in the `List` corresponds to one index, with `+1` specifying an upper index and `-1` specifying a lower index. For example, `{-1,-1}` corresponds to a tensor such as the metric ${g}_{\mu\nu}$, which has two lower indices, while `{1,-1,-1,-1}` corresponds to a tensor such as the Riemann tensor ${R}^{\rho}{}_{\sigma\mu\nu}$, which has one upper index followed by three lower indices.

The components of the tensor must also be a `List`. The components are the representation of the new tensor in the given index configuration and coordinate system. If a coordinate system is not specified, the default coordinate system of the associated metric will be used - but it is recommended to always specify the coordinate system explicitly, to avoid accidentally defining the tensor with the wrong components. The components will be automatically converted to different indices or coordinates later as needed, as we will demonstrate below.

To create a **scalar**, or a tensor of rank 0 (with no indices), we must input an empty list `{}` for the indices, and a list with one item for the components. For example, let us define the **Kretschmann scalar** in the Schwarzschild spacetime (below we will show how to calculate it directly from the metric):

```wl
TNewTensor["Kretschmann","Schwarzschild","Spherical",{},{(48*M^2)/r^6},"K"]
```

Again, the output is the unique ID of the tensor object that was created. Let us show the tensor:

```wl
TShow["Kretschmann"]
```

Notice that the output of `TNewTensor` is also the input of `TShow`, so in fact, we could **compose** them together using `@`. We will do so from now on.

Similarly, we can create a **vector**, or a tensor of rank 1 (with one index). For example, let us create a vector for the 4-velocity of a particle moving at 3-velocity $v$ along the $x$ direction in Minkowski space. (We do not need to reserve the symbol `v`, since we already reserved it for the Alcubierre metric above.)

Since the 4-velocity has an upper index by definition, we make sure to define the components in the representation of the tensor with an upper index by specifying the index configuration as `{1}`:

```wl
TShow@TNewTensor["FourVelocity","Minkowski","Cartesian",{1},({1,v,0,0})/(Sqrt[1-v^2])]
```

Again, the output of `TNewTensor` was the ID of the tensor, `"FourVelocity"`, but that is also the input we want to pass to `TShow`, so we **composed** the two modules together. Note also that since we did not specify a symbol for this tensor, its symbol is just a placeholder $\square$. We will give it a proper symbol below.

Finally, as an example of a tensor of rank 2 (with two indices), let us define the stress-energy tensor ${T}^{\mu\nu}$ for a perfect fluid. First, let us reserve the symbols `\[Rho]` (for the energy density) and `p` (for the pressure):

```wl
TSetReservedSymbols[{\[Rho],p}]
```

Next we create the tensor, using its matrix representation with two upper indices by specifying the index configuration `{1,1}`:

```wl
TShow@TNewTensor["PerfectFluid","Minkowski","Cartesian",{1,1},DiagonalMatrix[{\[Rho],p,p,p}],"T"]
```

In a similar manner, we could also define tensors of rank 3 and above. However, such tensors are most often derived by operating on lower-rank tensors, rather than defined manually via their components. We will see an example of such a derivation when we derive the Christoffel symbols and Riemann tensor from the metric below.

### Tensor ID completion

Now is a great time to introduce a feature that makes OGRe very user-friendly and easy to use: **tensor ID completion**. All OGRe modules that take existing tensor IDs as arguments will show a dropdown menu with a list of relevant IDs - coordinate systems, metrics, and/or tensors - as soon as you type the opening double quotes for a string in a relevant argument position. Some other strings are also auto-completed, such as common symbols. For the modules we used so far, completions work as follows:

* For `TLineElement` and `TVolumeElement`, the first argument is the ID of an existing metric, so if you start typing `TLineElement["` or `TVolumeElement["` in a Mathematica input cell, as soon as you type the `"` to start a string, a list of all existing metric IDs will be shown for you to select from. Currently, this will show `"Minkowski"`, `"Schwarzschild"`, and `"Alcubierre"`.
* For `TShow` and `TList`, the first argument is the ID of an existing tensor, so if you start typing `TShow["` or `TList["`, a list of all existing tensor IDs (including coordinate systems and metrics) will be shown. Currently, this will show the coordinate systems `"Cartesian"` and `"Spherical"`, the metrics `"Minkowski"`, `"Schwarzschild"`, and `"Alcubierre"`, and the tensors `"Kretschmann"`, `"FourVelocity"`, and `"PerfectFluid"`.
* For `TNewMetric`, the first argument is the new ID you create, so it is not completed for you, but the second argument is the ID of an existing coordinate system. Therefore, if you start typing `TNewMetric["MyMetric","`, a list of all existing coordinate system IDs will be shown. Currently, this will show `"Cartesian"` and `"Spherical"`. Additionally, for the fourth argument, a list of common metric symbols will be shown.
* For `TNewTensor`, the first argument is the new ID you create, so it is not completed for you, but the second argument is the ID of an existing metric. Therefore, if you start typing `TNewTensor["MyTensor","`, a list of all existing metric IDs will be shown. Similarly, the third argument is the ID of an existing coordinate system, so if you start typing `TNewTensor["MyTensor","MyMetric","`, a list of all existing coordinate system IDs will be shown.

Most of the modules we will introduce in the rest of this documentation will utilize tensor ID completion as well. It is highly recommended to try it for yourself now, so you can see how it works.

## Operations on single tensors

### Changing the symbol or ID of a tensor

If we ever want to change the symbol used to display a tensor, we can simply use the module `TChangeSymbol`:

```wl
?TChangeSymbol
```

For example, let us give the symbol $u$ to the four-velocity, and then show it:

```wl
TShow@TChangeSymbol["FourVelocity","u"]
```

Similarly, we can also change the ID of the tensor using the `TChangeID` module:

```wl
?TChangeID
```

For example, let us change the ID of the 4-velocity tensor from `"FourVelocity"` to `"4-Velocity"`:

```wl
TChangeID["FourVelocity","4-Velocity"]
```

The old ID no longer represents any tensor object, so we get an error if we try using it:

```wl
TShow["FourVelocity"]
```

Note that this error is associated with the symbol `TMessage`. Any message not associated with a specific OGRe module will be associated with this symbol.

We can access the tensor using the new ID:

```wl
TShow["4-Velocity"]
```

Note that when we define a tensor using a metric and a coordinate system, OGRe doesn't store the actual metric components or coordinates inside the tensor object - it only stores **references** to the relevant objects, using their IDs. This both improves performance and avoids redundancies that can lead to inconsistencies. For this reason, if the tensor to be renamed represents a metric or a coordinate system, OGRe will automatically update the references in the definitions of all of the tensors that have been defined so far in the session using that metric or coordinate system. This guarantees that there are never any broken references.

### Deleting and overwriting tensors

If we want to delete a tensor we have created, we can use the `TDelete` module:

```wl
?TDelete
```

To prevent breaking references, `TDelete` will not delete a tensor object representing a metric or coordinate system if it is referred to by any other tensor. For example, if we try to delete the coordinate system `"Cartesian"`, we will get an error message, since it is used as the default coordinate system for some tensors:

```wl
TDelete["Cartesian"]
```

Similarly, we cannot delete the metric `"Minkowski"` since it was used to define some tensors:

```wl
TDelete["Minkowski"]
```

There is no module to change the components of a tensor after it has already been defined, as this may break class invariants (in other words, introduce inconsistencies in the data). Instead, you must create a new tensor with the same ID using `TNewTensor`. By default, OGRe does not allow overwriting tensors, to prevent loss of data. Users who want to be able to create new tensors with the same ID as an existing tensor without deleting the old tensor first, and are confident that they will not accidentally lose any data by doing so, may enable overwriting tensors using `TSetAllowOverwrite`:

```wl
?TSetAllowOverwrite
```

First let us save the current state of `TSetAllowOverwrite`:

```wl
currentOverwrite=TSetAllowOverwrite[];
```

Next, we turn it off:

```wl
TSetAllowOverwrite[False]
```

Overwriting the tensor is now forbidden:

```wl
TNewTensor["4-Velocity","Minkowski","Cartesian",{1},({1,v,0,0})/(Sqrt[1-v^2]),"v"]
```

Let us now turn it on:

```wl
TSetAllowOverwrite[True]
```

We can now overwrite the tensor "4-Velocity" by creating a new tensor with the same ID:

```wl
TNewTensor["4-Velocity","Minkowski","Cartesian",{1},({1,v,0,0})/(Sqrt[1-v^2]),"v"]
```

Like any other message, the message warning us that we are overwriting a tensor can be turned off using `Off`. In this case, the command to turn it off would be `Off[TMessage::WarningOverwrite]`.

Note that the state of `TSetAllowOverwrite` is persistent between sessions - if you turn overwriting on, then it will remain on permanently, even in other Mathematica sessions, until you turn it back off. For this reason, the documentation notebook will now restore the previous setting, to avoid messing up the preferences of users who evaluate it:

```wl
Off[TSetAllowOverwrite::Notify]; TSetAllowOverwrite[currentOverwrite]; On[TSetAllowOverwrite::Notify]
```

### Raising and lowering indices

Raising and lowering indices is one of the most basic tensor operations. For example, if we have a vector represented with one upper index, ${v}^{\nu}$, we can turn it into a covector, which is represented with one lower index, by **contracting** it with the metric:

$$
{v}_{\mu} = {g}_{\mu\nu} {v}^{\nu}.
$$

This is called "lowering an index". Here and in the rest of this documentation, we will be using the **Einstein summation convention**, where the same index repeated **exactly twice**, once as an upper index and once as a lower index, implies summation over that index. In this case, the implied summation is over $\nu \in \{0,1,2,3\}$:

$$
{v}_{\mu} = {\sum}_{\nu=0}^{3} {g}_{\mu\nu} {v}^{\nu} = {g}_{\mu0} {v}^{0} + {g}_{\mu1} {v}^{1} + {g}_{\mu2} {v}^{2} + {g}_{\mu3} {v}^{3}.
$$

Such a sum over an index is called a contraction, and it is a generalization of the inner product, as we will describe in more detail below. Conversely, if we have a covector ${w}_{\mu}$, we can raise its index by contracting it with the inverse metric:

$$
{w}^{\mu} = {g}^{\mu\nu} {w}_{\nu}.
$$

This works the same for indices of higher-rank tensors. For example, if we have a tensor of rank 2 represented with two upper indices, ${T}^{\mu\lambda}$, we can lower either one or both of its indices:

$$
{T}_{\nu}^{\mu} = {g}_{\nu\lambda} {T}^{\mu\lambda}, \quad {T}_{\mu\nu} = {g}_{\mu\rho} {g}_{\nu\lambda} {T}^{\rho\lambda}.
$$

In OGRe, since tensor objects are **abstract tensors**, independent of any specific index configuration, **there is no notion of raising or lowering the indices of a tensor object**. Instead, one simply requests to **display** the components of the tensor with the desired index configuration, without modifying the object itself. This works with both the `TShow` and `TList` modules, by simply adding as a second argument the list of indices, as when we created a new tensor.

As an example, let us show the vector `"4-Velocity"` with a lower index, that is, with index configuration `{-1}`:

```wl
TShow["4-Velocity",{-1}]
```

OGRe automatically knows to use the Minkowski metric to lower the index, which means that a minus sign has been added to the first component, as expected. Similarly, let us lower just the **second** index on `PerfectFluid`:

```wl
TList["PerfectFluid",{1,-1}]
```

The components of the representation of the metric with two upper indices are the components of the inverse metric, since

$$
{g}_{\mu\lambda} {g}^{\lambda\nu}={\delta}_{\mu}^{\nu}.
$$

Therefore, a quick way to show the components of the inverse metric is:

```wl
TShow["Schwarzschild",{1,1}]
```

For the same reason, the metric with one upper and one lower index is just the identity matrix:

```wl
TList["Schwarzschild",{1,-1}]
```

As explained above, if the modules `TShow` or `TList` are called with only the tensor ID, the tensor is displayed in its **default index configuration**, which is the one first used to define the tensor. So the 4-velocity has one upper index by default, and the stress tensor has two upper indices by default, because that is how we initially defined them. However, the default indices can be changed using the module `TChangeDefaultIndices`:

```wl
?TChangeDefaultIndices
```

For example, let us change the default indices of the perfect fluid stress tensor to two lower indices:

```wl
TChangeDefaultIndices["PerfectFluid",{-1,-1}]
```

Now, when we display the tensor using `TShow` with only the tensor ID, this is the index configuration that will be used:

```wl
TShow["PerfectFluid"]
```

Note that the default indices can only be changed for user-defined tensors; they cannot be changed for coordinates, metrics, or built-in tensors such as curvature tensors (which we will introduce later).

### Coordinate transformations

The components of any tensor may be transformed from one coordinate system ${x}^{\mu}$ to another coordinate system ${x}^{{\mu}^{\prime}}$ using the following prescription:

* For every lower index $\mu$, add a factor of $\partial {x}^{\mu}$/$\partial {x}^{{\mu}^{\prime}}$ (i.e. the derivative of the old coordinates with respect to the new, or the **Jacobian**).

* For every upper index $\mu$, add a factor of $\partial {x}^{{\mu}^{\prime}}$/$\partial {x}^{\mu}$ (i.e. the derivative of the new coordinates with respect to the old, or the inverse of the Jacobian).

For example, given a tensor with components ${T}_{\alpha\beta}$ in a coordinate system ${x}^{\mu}$, we can transform to components ${T}_{{\alpha}^{\prime} {\beta}^{\prime}}$ in another coordinate system ${x}^{{\mu}^{\prime}}$ as follows:

$$
{T}_{{\alpha}^{\prime} {\beta}^{\prime}}({x}^{{\mu}^{\prime}}) = \frac{\partial {x}^{\alpha}} {\partial {x}^{{\alpha}^{\prime}}}\frac{\partial {x}^{\beta}} {\partial {x}^{{\beta}^{\prime}}} {T}_{\alpha\beta}({x}^{\mu}).
$$

For a general rank $(p,q)$ tensor with $p$ upper indices ${\alpha}_{1},\ldots,{\alpha}_{p}$ and $q$ lower indices ${\beta}_{1},\ldots,{\beta}_{q}$, the transformation takes the form

$$
{T}_{{\beta}_{1}^{\prime} \cdots {\beta}_{q}^{\prime}}^{{\alpha}_{1}^{\prime} \cdots {\alpha}_{p}^{\prime}}({x}^{{\mu}^{\prime}}) = \left(\frac{\partial {x}^{{\alpha}_{1}^{\prime}}}{\partial {x}^{{\alpha}_{1}}} \cdots \frac{\partial {x}^{{\alpha}_{p}^{\prime}}}{\partial {x}^{{\alpha}_{p}}}\right) \left(\frac{\partial {x}^{{\beta}_{1}}}{\partial {x}^{{\beta}_{1}^{\prime}}} \cdots \frac{\partial {x}^{{\beta}_{q}}}{\partial {x}^{{\beta}_{q}^{\prime}}}\right) {T}_{{\beta}_{1} \cdots {\beta}_{q}}^{{\alpha}_{1} \cdots {\alpha}_{p}}({x}^{\mu}).
$$

As a mnemonic for this formula, recall that two indices may only be contracted if one of them is an upper index and the other is a lower index. If an index is in the denominator of a derivative, then its role is reversed (upper $\leftrightarrow$ lower). Thus the old (non-primed) and new (primed) indices can only be in places that allow properly contracting the Jacobian or inverse Jacobian with the tensor. For example, ${\alpha}_{1}$ is an upper index in $T$ and therefore must be contracted with a lower index. Thus, $\partial {x}^{{\alpha}_{1}}$ must be in the denominator, to lower its index and allow it to be contracted with the tensor.

As we saw above, OGRe automatically knows how to raise or lower indices as needed using the appropriate metric. Similarly, any operation that requires transforming to another coordinate system will perform the transformation automatically behind the scenes. However, for this to happen, OGRe needs to know the appropriate **transformation rules**. These are defined between the tensor objects representing the coordinates, which were generated using the module `TNewCoordinates`. The rules for transforming from a source coordinate system to a target coordinate system are stored within the tensor object representing the source. This is done using the module `TAddCoordTransformation`:

```wl
?TAddCoordTransformation
```

Let us add the rules to transform from Cartesian to spherical coordinates:

```wl
TAddCoordTransformation["Cartesian","Spherical",{x->r Sin[\[Theta]] Cos[\[Phi]],y->r Sin[\[Theta]] Sin[\[Phi]],z->r Cos[\[Theta]]}];
```

These will be stored in the data of the object `"Cartesian"`. Note that we did not have to input a rule for $t$, since in this case, it stays the same. Conversely, let us add the rules to transform from spherical to Cartesian coordinates:

```wl
TAddCoordTransformation["Spherical","Cartesian",{r->Sqrt[x^2+y^2+z^2],\[Theta]->ArcCos[z/Sqrt[x^2+y^2+z^2]],\[Phi]->ArcTan[x,y]}];
```

These will be stored in the data of the object `"Spherical"`. Now OGRe knows how to convert back and forth between these two coordinate systems - and this will happen automatically whenever required. We just needed to provide these rules once and for all, and any tensor initially defined in one coordinate system can now be automatically converted to the other.

As in the case of raising and lowering indices, displaying a tensor in a different coordinate system is a simple matter of calling the modules `TShow` or `TList` with an additional argument specifying the coordinate system to use. For example, let us show the Minkowski metric in spherical coordinates:

```wl
TShow["Minkowski","Spherical"]
```

We can also ask to see a tensor in a specific index configuration **and** a specific coordinate system; in this case, the indices must come before the coordinates in the argument list:

```wl
TShow["PerfectFluid",{1,1},"Spherical"]
```

The module `TList` works in exactly the same way, for example:

```wl
TList["Kretschmann","Cartesian"]
```

Just as with default indices, every tensor has a **default coordinate system**, which is, initially, the one we used to create it. We can change it using the module `TChangeDefaultCoords`, and then whenever we display the tensor, it will be displayed in that coordinate system if no other coordinate system is specified:

```wl
?TChangeDefaultCoords
```

For example, let's change the default coordinates of the perfect fluid stress tensor to spherical coordinates:

```wl
TChangeDefaultCoords["PerfectFluid","Spherical"]
```

Now, when we display the tensor using `TList` with only the tensor ID (or with just a choice of indices), this is the coordinate system that will be used:

```wl
TList["PerfectFluid"]
```

### Applying replacement rules or a function to the tensor components

`TList` and `TShow` accept a rule or a list of rules as an argument. If provided, the replacement will be applied to each of the tensor's components, the components will be simplified, and the tensor will be displayed with the new components. Note that this only applies to displaying the components; the tensor data itself will not change.

This can be used to replace a particular symbol in the tensor with another symbol or numerical value. For example, perhaps we would like to display the value of the Kretschmann scalar for a particular choice of $M$ and $r$:

```wl
TShow["Kretschmann",{M->1,r->10}]
```

Or maybe we would like to display the perfect fluid stress tensor with $p$ equal to $\rho$:

```wl
TList["PerfectFluid",p->\[Rho]]
```

The replacement rules can, of course, also be combined with a choice of indices and/or coordinates, as long as the rules come after the indices and/or coordinates in the argument list:

```wl
TList["PerfectFluid",{1,1},"Cartesian",p->\[Rho]]
```

Additionally, a function can be provided to `TList` and `TShow` as their last argument. If provided, the function will be applied to each of the tensor's components, the components will be simplified, and the tensor will be displayed with the new components. Providing a list of rules is equivalent to applying the function `ReplaceAll` with those rules.

As an example, let's square the components of the 4-velocity:

```wl
TShow["4-Velocity",#1^2&]
```

Indices, coordinates, replacement rules, and functions can all be combined together. The full syntax is:

* `TList[ID, indices, coordinatesID, rules, function]`
* `TShow[ID, indices, coordinatesID, rules, function]`

The arguments must be provided in this exact order, but any of them can be omitted. So we can, for example, provide just `indices` and `rules`, or `coordinatesID`, `rules`, and `function`, and so on; any combination in the correct order will work. If both `rules` and `function` are given, the rules will be applied before the function.

### Customizing the simplification function

Whenever OGRe performs an operation that creates or modifies tensor components, such as converting between index representations or coordinate systems, it automatically simplifies the result. The simplification function can be specified using the module `TSetSimplifyFunc`:

```wl
?TSetSimplifyFunc
```

By default, OGRe uses `FullSimplify`. However, one possible use of `TSetSimplifyFunc` is to replace it with `Simplify`, which is a much faster simplification function that still does a good job in most cases. This might be necessary if simplification is taking too long or getting stuck.

You may also use your own custom-made simplification function, combining various Mathematica functions or your own code. In that case, your function should take two arguments: the expression to be simplified, and a list of assumptions. A simple example is using `FullSimplify` with specific options. For example, we can use `FullSimplify` with the option `TimeConstraint->5` to limit the time spent on simplification to 5 seconds:

```wl
TSetSimplifyFunc[FullSimplify[#1,#2,TimeConstraint->5]&]
```

In extreme situations, you may even want to cancel simplification altogether, which can be achieved using `TSetSimplifyFunc[None]`. To restore the simplification function to the default, `FullSimplify`, you can use `TSetSimplifyFunc[Automatic]`:

```wl
TSetSimplifyFunc[Automatic]
```

Note that changing the simplification function will **not** automatically apply it to any existing tensors. The reason is that when OGRe calculates the components of a tensor in a particular representation, it calculates them **once and for all**, and then saves them in the object's data to be reused later. This is done to improve performance, so that the components don't have to be recalculated every time they are needed. However, we can force re-simplification of the stored components of a specific tensor using the module `TSimplify`:

```wl
?TSimplify
```

We will demonstrate how to use `TSimplify` in the next section.

### Setting simplification assumptions

Often, coordinate transformations are only invertible for a specific range of coordinates. For example, let us define a new scalar in Minkowski space, which is equal to the spatial distance from the origin:

```wl
TShow@TNewTensor["SpatialDistance","Minkowski","Cartesian",{},{Sqrt[x^2+y^2+z^2]},"d"]
```

When we convert this scalar to spherical coordinates, we expect to get $r$, but instead we get the absolute value of $r$:

```wl
TShow["SpatialDistance","Spherical"]
```

As usual in Mathematica, such issues can be easily fixed by simplifying with the correct **assumptions**. By default, OGRe uses the simplification assumption that all variables (coordinates, parameters, etc.) are **real**. Therefore, Mathematica knows to simplify $\sqrt{{r}^{2}}$ to $|r|$. However, Mathematica doesn't automatically know that $r$ is non-negative.

The user may specify which assumptions to pass to the simplification function using the module `TSetAssumptions`:

```wl
?TSetAssumptions
```

Note that these assumptions will be globally applied to **all** tensor calculations, which is usually the desired behavior, since for example the assumption $r \ge 0$ should apply to all tensors that use spherical coordinates. Let us set this assumption now:

```wl
TSetAssumptions[r>=0]
```

In fact, it is good practice to set any assumptions regarding the coordinates **as soon as they are defined**, so we should have set this assumption already when we defined the spherical coordinates in the beginning of this documentation. From now on, this assumption will automatically be used by modules that perform any kind of calculations on tensors. However, if we now try to show the scalar again using `TShow`, we still get the same (non-simplified) result:

```wl
TShow["SpatialDistance","Spherical"]
```

The reason is that, as explained above, OGRe does not automatically recalculate components it already calculated previously. In this case, since we already calculated the spatial distance in spherical coordinates when we showed it above - **before** we set the new simplification assumptions - that value has been saved, and will not be recalculated automatically, even though we now have new assumptions.

However, we can use `TSimplify` to force re-simplification and get the expected result:

```wl
TShow[TSimplify["SpatialDistance"],"Spherical"]
```

Here, again, note that `TSimplify` returns the ID of the tensor it simplifies, so we can compose it with `TShow` if we want to show that same tensor. However, since we are now using `TShow` with a second argument, we put `TSimplify["SpatialDistance"]` as the first argument, instead of composing it directly with `@`.

Finally, we note that if you are using non-real variables, you can disable the assumption that all variables are real using `TSetAssumptions[!Reals]`. If you later want to turn it back on, use `TSetAssumptions[Reals]`. The value of the key `"AssumeReal"` in the output of `TSetAssumptions` indicates whether this assumption is turned on.

### Getting the components of a tensor

Sometimes you may want to extract the components of a tensor in a specific representation as a `List` so you can use them outside of this package, as regular Mathematica expressions rather than tensor objects. This is done using `TGetComponents`:

```wl
?TGetComponents
```

For example:

```wl
InverseSchwarzschild=TGetComponents["Schwarzschild",{1,1},"Spherical"]
```

We can now treat `InverseSchwarzschild` as any other `List` in Mathematica - for example, extract the element at a particular position:

```wl
InverseSchwarzschild[[1,1]]
```

If the desired index configuration and/or coordinate system are not specified, the default ones will be used. However, it is important to always know exactly which representation the components are in, to avoid confusion. Thus, you will be notified which representation was used:

```wl
TGetComponents["Schwarzschild"]
```

`TGetComponents` can take replacement rules and/or a function to map to the components, in the exact same syntax as `TList` and `TShow` (see above). For example, here are the components of the Schwarzschild metric on the hypersurface with $\theta = \pi$/2:

```wl
TGetComponents["Schwarzschild",\[Theta]->\[Pi]/2]
```

### Getting the dimension and rank of a tensor

The `TDim` module can be used to get the dimension, or number of coordinates, of the manifold a tensor is defined in.

```wl
?TDim
```

For example:

```wl
TDim["Minkowski"]
```

The `TRank` module can be used to get the rank of a tensor. The rank is the number of indices in the representations of the tensor. A rank-0 tensor is a scalar, a rank-1 tensor is a vector, a rank-2 tensor is a matrix, and so on.

```wl
?TRank
```

```wl
TRank["PerfectFluid"]
```

### Getting information about tensors

The `TInfo` module can be used to display information about any tensor object, including its symbol, role, rank, dimensions, associated metric, and default coordinates and indices:

```wl
?TInfo
```

`TInfo` will also tell you which other tensor objects use this tensor as their metric or default coordinate system, if applicable. Here are some examples:

```wl
TInfo["Cartesian"]
```

```wl
TInfo["Minkowski"]
```

```wl
TInfo["PerfectFluid"]
```

You can click on the links to `TList` and `TShow` at the bottom to display the components of the tensor being inspected. You can also click on any tensor name in the output (e.g. `Minkowski`, or `4-Velocity` in `TInfo["Cartesian"]`) to execute `TInfo` for that tensor.

In addition, calling `TInfo[]` with no parameters lists all the tensors created so far in this session - coordinate systems, metrics, and the tensors associated with each metric:

```wl
TInfo[]
```

We see that we created 9 tensors in total so far: 2 coordinate systems, 3 metrics, 3 tensors associated with the Minkowski metric, and 1 tensor associated with the Schwarzschild metric.

## Calculations with tensors

### The `TCalc` module

Now that we have all the bookkeeping of tensors out of the way, we can finally discuss how to use those tensors in calculations. In OGRe, all tensor calculations are performed using the `TCalc` module:

```wl
?TCalc
```

Any use of `TCalc` should be thought of as invoking a tensor equation of the form

$$
{L}_{{\beta}_{1} {\cdots \beta}_{q}}^{{\alpha}_{1} {\cdots \alpha}_{p}}={R}_{{\beta}_{1} {\cdots \beta}_{q}}^{{\alpha}_{1} {\cdots \alpha}_{p}},
$$

where both the left-hand side and the right-hand side are tensors of the same rank and with the same **free indices** (that is, indices that are not being contracted). ${L}_{{\beta}_{1} {\cdots \beta}_{q}}^{{\alpha}_{1} {\cdots \alpha}_{p}}$ is the tensor that will be used to store the result, while ${R}_{{\beta}_{1} {\cdots \beta}_{q}}^{{\alpha}_{1} {\cdots \alpha}_{p}}$ is (the final result of) a general tensor calculation which contains any combination of addition, multiplication by a scalar, trace, contraction, partial derivative, and covariant derivative. Let us now go over these operations one by one, and give some examples.

### Addition of tensors

Addition of tensors in OGRe is represented by a sum of the form `"ID1"["indices1"] + "ID2"["indices2"]`, where `"ID1"` and `"ID2"` are the IDs of the tensor objects to be added, and `"indices1"` and `"indices2"` are the **index specifications** for each tensor, given as a string of letters. Note that you do **not** specify the position (upper or lower) of the indices. Furthermore, just like in any tensor equation, **the index letters themselves have no meaning**; they are just placeholders. Therefore, `"\[Alpha]\[Beta]\[Gamma]"`, `"abc"`, and `"ABC"` are all completely equivalent. The only requirement is that the **indices are consistent**; in the case of addition, this means that both tensors being added must have **the same indices up to permutation**.

The following constraints apply to addition of tensors:

* You may not add a tensor representing a coordinate system to any other tensor, since coordinates do not transform like tensors.

* You may not add two tensors associated with different metrics, since their sum would have undefined transformation properties.

* You may not add two tensors with different ranks, since that is not a well-defined operation.

* As stated above, both tensors must have the same indices up to permutation. ${A}^{\mu\nu}+{B}^{\mu\nu}$ and ${A}^{\mu\nu}+{B}^{\nu\mu}$ are both okay, but ${A}^{\mu\nu}+{B}^{\alpha\beta}$ doesn't make sense, as it has more free indices than the rank of the result.

As an example, let us add the Minkowski metric ${\eta}_{\mu\nu}$ and the perfect fluid stress tensor ${T}_{\mu\nu}$:

```wl
TShow@TCalc["Minkowski"["\[Mu]\[Nu]"]+"PerfectFluid"["\[Mu]\[Nu]"]]
```

Notice that the result was stored in a tensor with ID `"Result"`, and has no symbol. We can add a symbol to use as an additional argument:

```wl
TShow@TCalc["Minkowski"["\[Mu]\[Nu]"]+"PerfectFluid"["\[Mu]\[Nu]"],"S"]
```

With this symbol, the tensor equation we are calculating becomes:

$$
{S}_{\mu\nu}={\eta}_{\mu\nu}+{T}_{\mu\nu}.
$$

We can also use a different ID for the result by giving it as the first argument, with or without a symbol:

```wl
TShow@TCalc["SumResult","Minkowski"["\[Mu]\[Nu]"]+"PerfectFluid"["\[Mu]\[Nu]"],"S"]
TDelete["SumResult"]
```

(We deleted the result to avoid cluttering memory with unused objects.) In the following examples, we will not specify a symbol, to keep the code cleaner.

Sometimes it is also helpful to specify indices for the result. To give an example, let us define the following non-symmetric tensor:

```wl
TShow@TNewTensor["NonSymmetric","Minkowski","Cartesian",{-1,-1},{{0,0,0,1},{0,0,0,0},{0,0,0,0},{0,0,0,0}},"N"]
```

If we add it to the Minkowski metric, we get:

$$
{\square}_{\mu\nu}={\eta}_{\mu\nu}+{N}_{\mu\nu,}
$$

```wl
TShow@TCalc["Minkowski"["\[Mu]\[Nu]"]+"NonSymmetric"["\[Mu]\[Nu]"]]
```

However, if we flip its index string from `"\[Mu]\[Nu]"` to `"\[Nu]\[Mu]"`, then we instead get:

$$
{\square}_{\mu\nu}={\eta}_{\mu\nu}+{N}_{\nu\mu,}
$$

```wl
TShow@TCalc["Minkowski"["\[Mu]\[Nu]"]+"NonSymmetric"["\[Nu]\[Mu]"]]
```

Since the order of indices now matters, we can also define an index string for the left-hand side, to indicate the order of indices we want in the result. If that string is also `"\[Nu]\[Mu]"`, then we get back to the original result:

$$
{\square}_{\nu\mu}={\eta}_{\mu\nu}+{N}_{\nu\mu,}
$$

```wl
TShow@TCalc["Result"["\[Nu]\[Mu]"],"Minkowski"["\[Mu]\[Nu]"]+"NonSymmetric"["\[Nu]\[Mu]"]]
```

We see that explicitly specifying the indices in `TCalc` allows it to have **a 1-to-1 correspondence with any tensor equation**. Importantly, note that there is no difference between `"NonSymmetric"["\[Mu]\[Nu]"]` and `"NonSymmetric"["\[Nu]\[Mu]"]` on its own, as **the index labels themselves are meaningless** unless there is some context in which they obtain meaning - as is always the case for tensor expressions. However, there is a big difference between, for example, `"Minkowski"["\[Mu]\[Nu]"] + "NonSymmetric"["\[Mu]\[Nu]"]` and `"Minkowski"["\[Mu]\[Nu]"] + "NonSymmetric"["\[Nu]\[Mu]"]`, as the indices have a different order, and thus the two expressions refer to adding different components.

Of course, any number of tensors can be added, not just two - and the same tensor can be added multiple times, with different index specifications each time. For example, we can calculate:

$$
{\square}_{\mu\nu}={\eta}_{\mu\nu}+{T}_{\mu\nu}+{N}_{\mu\nu}+{N}_{\nu\mu,}
$$

```wl
TShow@TCalc["Minkowski"["\[Mu]\[Nu]"]+"PerfectFluid"["\[Mu]\[Nu]"]+"NonSymmetric"["\[Mu]\[Nu]"]+"NonSymmetric"["\[Nu]\[Mu]"]]
```

### Multiplication of a tensor by a scalar

Multiplication of a tensor by a scalar in OGRe is represented by a product of the form `scalar * "ID"["indices"]`, where `"ID"` is the ID of the tensor object to be multiplied, `"indices"` is an index specification as for addition, and `scalar` is the scalar to multiply by. Note that `scalar` should be a normal Mathematica symbol, such as a number or a variable, and **not** a tensor object of rank 0. To multiply a tensor by a tensor of rank 0, use contraction instead, as detailed in the next section.

As an example, let us multiply the Minkowski metric ${\eta}_{\mu\nu}$ by 2. The tensor equation we will be calculating is:

$$
{\square}_{\mu\nu}=2{\eta}_{\mu\nu},
$$

and the OGRe expression to calculate it (and show the result) is:

```wl
TShow@TCalc[2 "Minkowski"["\[Mu]\[Nu]"]]
```

While in this example the indices seem redundant, they are necessary because in most non-trivial situations we would like to combine multiplication with other operations, such as addition or contraction, in which the order of indices matters. For example, consider:

$$
{\square}_{\mu\nu}=2{t\eta}_{\mu\nu}-3{xT}_{\mu\nu}+4{yN}_{\mu\nu}-5{zN}_{\nu\mu,}
$$

```wl
TShow@TCalc[2 t "Minkowski"["\[Mu]\[Nu]"]-3 x "PerfectFluid"["\[Mu]\[Nu]"]+4 y "NonSymmetric"["\[Mu]\[Nu]"]-5 z "NonSymmetric"["\[Nu]\[Mu]"]]
```

### Taking traces and contracting tensors

The most complicated tensor operation is **contraction**, a generalization of the vector inner product. This is done by summing over one or more disjoint pairs of indices, with each pair containing exactly one upper index and one lower index. Raising and lowering indices is one example of contraction: the metric (or its inverse) is contracted with a tensor. Coordinate transformations are another example, where we contract the Jacobian (or its inverse) with a tensor.

The simplest example of contraction is the **vector inner product**, which is defined as the contraction of a vector (one upper index) with a covector (one lower index):

$$
{v}^{\mu} {w}_{\mu}={g}_{\mu\nu} {v}^{\mu} {w}^{\nu}=g(v,w).
$$

The middle part of this equality comes from the fact that, as explained above, when we lower an index on ${w}^{\nu}$, we use the metric:

$$
{w}_{\mu}={g}_{\mu\nu} {w}^{\nu}.
$$

This, in turn, justifies the notation $g(v,w)$ on the right-hand side, as this is, in fact, an inner product of two vectors using the metric $g$ (in index-free notation).

Contraction of indices in higher-rank tensors is simply a generalization of the inner product, for example:

$$
{A}^{\mu\alpha} {B}_{\alpha\nu}={g}_{\alpha\beta} {A}^{\mu\alpha} {B}^{\beta}{}_{\nu}.
$$

We can also contract more than one index:

$$
{A}^{\mu\nu} {B}_{\mu\nu}={g}_{\mu\alpha} {g}_{\nu\beta} {A}^{\mu\nu} {B}^{\alpha\beta}.
$$

This simply amounts to the fact that lowering both indices of ${B}^{\alpha\beta}$ involves contracting each index with the metric. We can even contract two indices **of the same tensor**:

$$
{A}^{\mu}{}_{\mu}={g}_{\mu\nu} {A}^{\mu\nu}.
$$

This is called **taking the trace**. Furthermore, it is also possible to contract pairs of indices from more than two tensors at the same time:

$$
{A}^{\mu\nu} {B}_{\nu\rho} {C}^{\rho\sigma}={g}_{\nu\alpha} {g}_{\rho\beta} {A}^{\mu\nu} {B}^{\alpha\beta} {C}^{\rho\sigma}.
$$

However, such operations can always be broken down into individual contractions of pairs of tensors. For example, in this case, one could first contract ${B}_{\nu\rho}$ with ${C}^{\rho\sigma}$ and then contract the result with ${A}^{\mu\nu}$ - which is indeed how this kind of contraction will be performed in OGRe in practice:

$$
{A}^{\mu\nu} {B}_{\nu\rho} {C}^{\rho\sigma}={A}^{\mu\nu}({B}_{\nu\rho} {C}^{\rho\sigma}).
$$

In a contraction, there are two types of indices: **contracted indices**, which are summed upon, and **free indices**, which are not summed upon. The rank of the tensor that results from the contraction is the number of free indices. So for example, in the expression ${A}^{\mu\alpha} {B}_{\alpha\nu}$ we have one contracted index, $\alpha$, and two free indices, $\mu$ and $\nu$. Therefore, the resulting tensor is of rank two: ${T}_{\nu}^{\mu}={A}^{\mu\alpha} {B}_{\alpha\nu}$.

Contraction of tensors in OGRe is represented by an expression of the form `"ID1"["indices1"] . "ID2"["indices2"]`, where `"ID1"` and `"ID2"` are the IDs of the tensor objects to be contracted, and `"indices1"` and `"indices2"` are the index strings for each tensor. Any **matching indices** in both index strings will be contracted. This means that, for example, ${v}^{\mu} {w}_{\mu}$ is calculated using `"v"["\[Mu]"] . "w"["\[Mu]"]` and ${A}^{\mu\nu} {B}_{\nu\rho} {C}^{\rho\sigma}$ is calculated using `"A"["\[Mu]\[Nu]"] . "B"["\[Nu]\[Rho]"] . "C"["\[Rho]\[Sigma]"]`. Note that the user doesn't need to worry about the contracted indices being one upper and one lower, which is a common source of errors when contracting tensors by hand; the order of the indices, and whether the same index repeats twice, is all that matters.

As a first example, let us create the stress-energy tensor for a perfect fluid with a 4-velocity ${u}^{\mu}$. This is defined as follows:

$$
{T}^{\mu\nu}=(\rho+p){u}^{\mu} {u}^{\nu}+{pg}^{\mu\nu}.
$$

Even though this does not involve any contractions, it still counts as a "trivial" contraction, since two tensors (the 4-velocities) are juxtaposed next to each other to create another tensor. This is also known as an **outer product**. Therefore, it uses the same dot product syntax as any other contraction, except that there are **no matching indices**. Note that this expression involves not just contraction (in the first term), but also multiplication by a scalar (in both terms), and addition of the two terms together. Again, OGRe takes care of everything behind the scenes, so this just works:

```wl
TShow@TCalc["PerfectFluidFromVelocity",(\[Rho]+p) "4-Velocity"["\[Mu]"]."4-Velocity"["\[Nu]"]+p "Minkowski"["\[Mu]\[Nu]"],"T"]
```

Indeed, for $v=0$ we get the previously defined stress tensor:

```wl
TShow["PerfectFluidFromVelocity",v->0]
```

Note that here, the second argument is a replacement rule to apply to the tensor elements, as explained above under "applying replacement rules or a function to the tensor components".

Multiplying a tensor by a scalar tensor (i.e. a tensor of rank 0) is also done using a "trivial" contraction with no contracted indices. For example:

```wl
TShow[TCalc["SpatialDistance"[""]."Minkowski"["\[Mu]\[Nu]"]],"Spherical"]
```

Note the empty index string `[""]`, which is mandatory in order for OGRe to recognize that the scalar is a tensor object. We can also multiply a scalar by another scalar:

```wl
TShow[TCalc["SpatialDistance"[""]."SpatialDistance"[""]],"Spherical"]
```

Now let us demonstrate some non-trivial contractions. First, we have the inner product of vectors - in this case, we get the norm (squared) of the 4-velocity, since we are contracting it with itself:

```wl
TShow@TCalc["4-Velocity"["\[Mu]"]."4-Velocity"["\[Mu]"]]
```

Note that this can be done more easily with `TNormSquared` (see below). We can also contract several tensors together, with **two** matching pairs of indices:

```wl
TShow@TCalc["4-Velocity"["\[Mu]"]."PerfectFluidFromVelocity"["\[Mu]\[Nu]"]."NonSymmetric"["\[Nu]\[Rho]"]]
```

Finally, to take the trace of a tensor, we simply match pairs of indices in that tensor's index string:

```wl
TShow@TCalc["Minkowski"["\[Mu]\[Mu]"]]
```

```wl
TShow@TCalc["PerfectFluidFromVelocity"["\[Mu]\[Mu]"]]
```

Of course, this also works for tensors with more than two indices, as we will see below. Any combination of indices can be used, with no limit on the number of traces taken for each tensor.

### Calculating the norm squared

The norm squared of a tensor of any rank can be calculated with `TNormSquared`:

```wl
?TNormSquared
```

For any tensor, the norm squared is defined as the tensor contracted with itself in all indices:

$$
\left|v\right|^{2}\equiv {v}^{\mu} {v}_{\mu},
$$

$$
\left|T\right|^{2}\equiv {T}^{\mu\nu} {T}_{\mu\nu},
$$

and so on. The result is always a scalar expression. For example, the norm of any 4-velocity is -1:

```wl
TNormSquared["4-Velocity"]
```

The norm of any metric is the number of dimensions in that metric:

```wl
TNormSquared["Minkowski"]
```

## Derivatives and curvature tensors

The **partial derivative** ${\partial}_{\mu}$ is represented in OGRe using the symbol `TPartialD`. It can be contracted with other tensors using the usual OGRe contraction notation - including an appropriate index string - to calculate gradients and divergences.

```wl
?TPartialD
```

The **gradient** of a tensor is the partial derivative ${\partial}_{\mu}$ acting on the tensor with a free index, e.g. ${\partial}_{\mu}\phi$, ${\partial}_{\mu} {v}^{\nu}$, or ${\partial}_{\mu} {T}^{\nu\lambda}$, resulting in a tensor of **one rank higher**. For example, we can calculate the gradient ${\partial}_{\mu}K$ of the Kretschmann scalar as follows:

```wl
TShow@TCalc[TPartialD["\[Mu]"]."Kretschmann"[""]]
```

The **divergence** of a tensor is the contraction of the partial derivative ${\partial}_{\mu}$ with one of the tensor's indices, e.g. ${\partial}_{\mu} {v}^{\mu}$ or ${\partial}_{\mu} {T}^{\mu\nu}$, resulting in a tensor of **one rank lower**. For example, here is the divergence ${\partial}_{\mu} {x}^{\mu}$ of the Cartesian position vector:

```wl
TShow@TNewTensor["Position","Minkowski","Cartesian",{1},{t,x,y,z},"x"]
```

```wl
TShow@TCalc[TPartialD["\[Mu]"]."Position"["\[Mu]"]]
```

As you can see, the syntax for both the gradient and divergence is the same; if the index specification of `TPartialD["\[Mu]"]` matches one of the indices of the tensor to its right, then the divergence will be calculated, otherwise the gradient will be calculated.

**WARNING: When applying partial derivatives to tensors, the result generally does not transform like a tensor under a coordinate transformation.** For this reason, in general relativity we normally use the **covariant derivative** (see below) instead of a partial derivative. However, there are three important exceptions, where partial derivatives must be used: in the covariant derivative itself, the **Levi-Civita connection**, and the **Riemann tensor**, all of which will be discussed below.

Of these three special cases, the covariant derivative and the Riemann tensor turn out to nonetheless transform like tensors under coordinate transformations, due to cancellations. However, the Levi-Civita connection, whose components are called the **Christoffel symbols**, does not transform like a tensor; it has a special transformation rule with an extra term, as we will see below.

In all other cases, if the user creates an arbitrary tensor using partial derivatives, the result will generally **transform incorrectly** under a coordinate transformation in OGRe. Therefore, it is highly recommended to avoid using partial derivatives with `TCalc` unless you really know what you're doing.

### The Christoffel symbols

The **Christoffel symbols** are a very important tensor-like object in differential geometry. They are the components of the **Levi-Civita connection,** which is the unique torsion-free connection that preserves the metric. The Christoffel symbols are defined as follows:

$$
{\Gamma}_{\mu\nu}^{\lambda} = \frac{1}{2} {g}^{\lambda\sigma} ({\partial}_{\mu} {g}_{\nu\sigma}+{\partial}_{\nu} {g}_{\sigma\mu}-{\partial}_{\sigma} {g}_{\mu\nu}).
$$

Each of the terms inside the parentheses is a gradient of the metric, with different indices. For example, the first term ${\partial}_{\mu} {g}_{\nu\sigma}$ is represented in OGRe as `TPartialD["\[Mu]"]."Metric"["\[Nu]\[Sigma]"]` where `"Metric"` is the tensor object representing the metric. Since contraction, addition, and multiplication by a scalar can be combined arbitrarily when using `TCalc`, we can calculate the Christoffel symbols in a straightforward way as follows:

```wl
TList@TChangeDefaultIndices[TCalc[1/2"Schwarzschild"["\[Lambda]\[Sigma]"].(TPartialD["\[Mu]"]."Schwarzschild"["\[Nu]\[Sigma]"]+TPartialD["\[Nu]"]."Schwarzschild"["\[Sigma]\[Mu]"]-TPartialD["\[Sigma]"]."Schwarzschild"["\[Mu]\[Nu]"]),"\[CapitalGamma]"],{1,-1,-1}]
```

Note that here we used `TChangeDefaultIndices` to ensure the result has the correct index placement (one upper, two lower).

However, there is a problem; as we mentioned above, **the Christoffel symbols are not the components of a tensor**, meaning that the Levi-Civita connection does not transform as a tensor does under a coordinate transformation. Indeed, by transforming the metric in the definition, one can show that

$$
{\Gamma}_{{\mu}^{\prime} {\nu}^{\prime}}^{{\lambda}^{\prime}} = \frac{\partial {x}^{{\lambda}^{\prime}}} {\partial {x}^{\lambda}}\frac{\partial {x}^{\mu}} {\partial {x}^{{\mu}^{\prime}}}\frac{\partial {x}^{\nu}} {\partial {x}^{{\nu}^{\prime}}} {\Gamma}_{\mu\nu}^{\lambda}+\frac{\partial {x}^{{\lambda}^{\prime}}} {\partial {x}^{\lambda}}\frac{{\partial}^{2} {x}^{\lambda}} {\partial {x}^{{\mu}^{\prime}}\partial {x}^{{\nu}^{\prime}}}.
$$

The first term is the familiar transformation rule for a tensor, with one factor of the Jacobian per index as usual. However, there is also an extra second term, meaning that the Christoffel symbols do not transform like a tensor.

(Similarly, you are also not supposed to raise or lower indices in the Christoffel symbols, but in practice, you can do that as long as you make it clear that it's just an abuse of notation - you are only adding factors of the metric, not creating a new tensor representation with different transformation properties.)

Due to the extra transformation term, the tensor object we calculated manually above using `TCalc` **must not be used**. Instead, we should use the built-in module `TChristoffel`, which not only performs the calculation automatically for us, but also knows that it has special transformation properties:

```wl
?TChristoffel
```

Let us, then, calculate the Christoffel symbols for the Schwarzschild metric properly, using `TChristoffel`:

```wl
TList@TChristoffel["Schwarzschild"]
```

These are the same components we got before, but now they will transform properly. The calculated Christoffel symbols are **cached** internally inside the metric object on first use, and are subsequently reused whenever `TChristoffel["Schwarzschild"]` is called; this module does not create a separate tensor object with its own ID. This means that whenever we want to do any operations on the Christoffel symbols, including using them in arbitrary calculations with `TCalc`, we call `TChristoffel["Schwarzschild"]` again instead of using a specific ID string. The same is true for all other built-in tensors derived from the metric, which we will discuss below.

For maximal clarity, let us demonstrate the discrepancy in the coordinate transformation with a simple test metric:

```wl
TShow@TNewMetric["SimpleMetric","Cartesian",DiagonalMatrix[{-x,1,1,1}]]
```

We calculate its Christoffel symbols in two ways. First manually, as we did above for the Schwarzschild metric:

```wl
TList@TChangeDefaultIndices[TCalc["SimpleMetricManualChristoffel",1/2"SimpleMetric"["\[Lambda]\[Sigma]"].(TPartialD["\[Mu]"]."SimpleMetric"["\[Nu]\[Sigma]"]+TPartialD["\[Nu]"]."SimpleMetric"["\[Sigma]\[Mu]"]-TPartialD["\[Sigma]"]."SimpleMetric"["\[Mu]\[Nu]"]),"\[CapitalGamma]"],{1,-1,-1}]
```

Then, with the built-in module `TChristoffel`:

```wl
TList@TChristoffel["SimpleMetric"]
```

The two results have the same components, as expected. But now, let us transform them to spherical coordinates. First, we transform the tensor object obtained using `TChristoffel`:

```wl
TList[TChristoffel["SimpleMetric"],"Spherical"]
```

This is the **correct** representation of the Christoffel symbols in spherical coordinates, as it was recalculated directly from the metric in that coordinate system (OGRe does this instead of using the special transformation rule, as it turns out to be faster to just recalculate from scratch). However, if we transform the Christoffel symbols we obtained manually using `TCalc`, we get:

```wl
TList["SimpleMetricManualChristoffel","Spherical"]
```

This is **not** the correct result, since a tensor created manually with `TCalc` always transforms as an ordinary tensor, which is not the correct way to transform the Christoffel symbols. To verify that the former result is indeed the correct one, let us change the default coordinate system of `SimpleMetric` to spherical:

```wl
TChangeDefaultCoords["SimpleMetric","Spherical"];
```

Now, when we calculate the Christoffel symbols manually from this metric, we will get their correct representation in spherical coordinates. This is because `TCalc` always performs the calculations internally in the default coordinates of the first tensor, so the result was calculated **from scratch** in spherical coordinates, instead of being calculated first in Cartesian coordinates and then transformed:

```wl
TList@TChangeDefaultIndices[TCalc["SimpleMetricManualChristoffelSpherical",1/2"SimpleMetric"["\[Lambda]\[Sigma]"].(TPartialD["\[Mu]"]."SimpleMetric"["\[Nu]\[Sigma]"]+TPartialD["\[Nu]"]."SimpleMetric"["\[Sigma]\[Mu]"]-TPartialD["\[Sigma]"]."SimpleMetric"["\[Mu]\[Nu]"]),"\[CapitalGamma]"],{1,-1,-1}]
```

Indeed, this is the exact same result we got when we transformed `TChristoffel["SimpleMetric"]` to spherical coordinates. We have learned an important lesson: since the Christoffel symbols do not transform like a tensor, we should always use the built-in module `TChristoffel` to calculate them, which ensures that they transform properly. (Of course, this method is also much more convenient than writing the explicit definition...)

For future use, let us define the **Friedmann-Lemaitre-Robertson-Walker (FLRW) metric**, which describes an expanding universe:

```wl
TSetReservedSymbols[{a,k}];
TShow[TNewMetric["FLRW","Spherical",DiagonalMatrix[{-1,a[t]^2/(1-k r^2),a[t]^2 r^2,a[t]^2 r^2 Sin[\[Theta]]^2}]]]
```

This metric has the line element:

```wl
TLineElement["FLRW"]
```

and the volume element:

```wl
TVolumeElement["FLRW"]
```

Here, $a(t)$ is the **scale factor** and $k$ is the curvature of the spatial surfaces, with $k=+1,0,-1$ corresponding to positively curved, flat, or negatively curved respectively. Its Christoffel symbols can be easily calculated using `TChristoffel`:

```wl
TList@TChristoffel["FLRW"]
```

Notice how the derivatives of the function `a` are shown by `TList` in the notation ${\partial}_{t}a$ instead of a'[t]. This may not seem like much of an improvement in this case, but in the case of partial derivatives of functions of many arguments, it can greatly improve readability. Here is a simple example:

```wl
TShow@TNewTensor["PartialDerivatives","FLRW","Cartesian",{1},{D[f[x,y,z],x],D[f[x,y,z],x,y],D[f[x,y,z],x,y,z],D[f[x,y,z],x,y,{z,2}]}]
```

Compare this with the raw components, where the order of the derivative with respect to each variable is determined by position, and the `[x,y,z]` function argument is shown explicitly:

```wl
TGetComponents["PartialDerivatives"]
```

### The Riemann tensor

The **Riemann curvature tensor** ${R}^{\rho}{}_{\sigma\mu\nu}$ can be calculated from the Christoffel symbols using the definition:

$$
{R}^{\rho}{}_{\sigma\mu\nu} = {\partial}_{\mu} {\Gamma}_{\nu\sigma}^{\rho}-{\partial}_{\nu} {\Gamma}_{\mu\sigma}^{\rho}+{\Gamma}_{\mu\lambda}^{\rho} {\Gamma}_{\nu\sigma}^{\lambda}-{\Gamma}_{\nu\lambda}^{\rho} {\Gamma}_{\mu\sigma}^{\lambda}.
$$

Even though it contains partial derivatives, it nonetheless transforms like a tensor under a change of coordinates, because the extra transformation terms exactly cancel each other. To calculate this tensor, we can simply write down the formula in `TCalc` with the correct indices contracted. Note that this time we specified the LHS indices explicitly, since they are not in the same order as the RHS indices in our definition:

```wl
TList@TChangeDefaultIndices[TCalc["SchwarzschildManualRiemann"["\[Rho]\[Sigma]\[Mu]\[Nu]"],TPartialD["\[Mu]"].TChristoffel["Schwarzschild"]["\[Rho]\[Nu]\[Sigma]"]-TPartialD["\[Nu]"].TChristoffel["Schwarzschild"]["\[Rho]\[Mu]\[Sigma]"]+TChristoffel["Schwarzschild"]["\[Rho]\[Mu]\[Lambda]"].TChristoffel["Schwarzschild"]["\[Lambda]\[Nu]\[Sigma]"]-TChristoffel["Schwarzschild"]["\[Rho]\[Nu]\[Lambda]"].TChristoffel["Schwarzschild"]["\[Lambda]\[Mu]\[Sigma]"],"R"],{1,-1,-1,-1}]
```

The Riemann tensor with all its indices lowered satisfies the following symmetry and anti-symmetry relations:

$$
{R}_{\rho\sigma\mu\nu}=-{R}_{\sigma\rho\mu\nu}=-{R}_{\rho\sigma\nu\mu}={R}_{\mu\nu\rho\sigma}.
$$

We can verify this for the Schwarzschild Riemann tensor using `TList`, as it automatically detects components that are the same up to sign:

```wl
TList[TRiemann["Schwarzschild"],{-1,-1,-1,-1}]
```

Don't worry - you don't need to write the explicit definition of the Riemann tensor every time you want to calculate it. Instead, OGRe offers the built-in module `TRiemann`:

```wl
?TRiemann
```

For example, for the FLRW metric we get:

```wl
TList@TRiemann["FLRW"]
```

Like `TChristoffel`, and like all the other built-in modules for calculating derived tensors we will discuss below, `TRiemann` calculates and caches the Riemann tensor the first time it is called, and then reuses the cached version on subsequent calls.

Using `TRiemann` also has the advantage that it takes a metric as an input, and will automatically calculate and cache the Christoffel symbols of the metric (as if using `TChristoffel`) if they have not already been calculated. The same principle also applies to the other built-in modules for calculating curvature tensors, which we will present below; they always take a metric as input, and will calculate any intermediate tensors in their definitions automatically as needed.

### The Ricci tensor and scalar

The **Ricci tensor** ${R}_{\mu\nu}$ is the trace of the first and third indices of the Riemann tensor:

$$
{R}_{\mu\nu}={R}^{\lambda}{}_{\mu\lambda\nu}.
$$

Therefore, we can calculate it by taking the trace, with the usual `TCalc` syntax. For the Schwarzschild metric, the Ricci tensor vanishes:

```wl
TList@TCalc[TRiemann["Schwarzschild"]["\[Lambda]\[Mu]\[Lambda]\[Nu]"],"R"]
```

We can also use the shorthand module `TRicci`:

```wl
?TRicci
```

Here is the Ricci tensor for the FLRW metric:

```wl
TList@TRicci["FLRW"]
```

The **Ricci scalar** is the trace of the Ricci tensor:

$$
R={R}^{\lambda}{}_{\lambda}={g}^{\mu\nu} {R}_{\mu\nu}.
$$

We can calculate it from the Ricci tensor by taking the trace:

```wl
TShow@TCalc[TRicci["FLRW"]["\[Mu]\[Mu]"],"R"]
```

Or, as usual, we can simply use the shorthand module `TRicciScalar` to calculate it directly from the metric:

```wl
?TRicciScalar
```

```wl
TList@TRicciScalar["FLRW"]
```

### The Kretschmann scalar

The **Kretschmann scalar** is the norm-squared of the Riemann tensor:

$$
K={R}_{\rho\sigma\mu\nu} {R}^{\rho\sigma\mu\nu}.
$$

It can be easily calculated in OGRe for the Schwarzschild metric using `TCalc` as follows:

```wl
TShow@TCalc[TRiemann["Schwarzschild"]["\[Rho]\[Sigma]\[Mu]\[Nu]"].TRiemann["Schwarzschild"]["\[Rho]\[Sigma]\[Mu]\[Nu]"],"K"]
```

Even more easily, we can use the `TNormSquared` module on the Riemann tensor (but the result will be the scalar itself, not a new tensor object):

```wl
TNormSquared[TRiemann["Schwarzschild"]]
```

However, the easiest way to calculate it is using the built-in module `TKretschmann`:

```wl
?TKretschmann
```

```wl
TShow@TKretschmann["Schwarzschild"]
```

For the FLRW metric, we get:

```wl
TShow@TKretschmann["FLRW"]
```

### The Einstein tensor

The **Einstein tensor** ${G}_{\mu\nu}$ is given by:

$$
{G}_{\mu\nu} = {R}_{\mu\nu} - \frac{1}{2} {g}_{\mu\nu} R.
$$

As with all other curvature tensors, we can calculate it by combining the previously calculated tensors with the usual syntax:

```wl
TList@TCalc[TRicci["FLRW"]["\[Mu]\[Nu]"]-1/2"FLRW"["\[Mu]\[Nu]"].TRicciScalar["FLRW"][""],"G"]
```

Or we can simply use the built-in module `TEinstein`:

```wl
?TEinstein
```

```wl
TList@TEinstein["FLRW"]
```

### The Weyl tensor

The **Weyl tensor** ${C}^{\rho}{}_{\sigma\mu\nu}$ is the trace-free part of the Riemann tensor, given by:

$$
{C}^{\rho}{}_{\sigma\mu\nu} = {R}^{\rho}{}_{\sigma\mu\nu} - \frac{1}{n-2} \left( {\delta}^{\rho}{}_{\mu} {R}_{\nu\sigma} - {\delta}^{\rho}{}_{\nu} {R}_{\mu\sigma} - {g}_{\sigma\mu} {R}^{\rho}{}_{\nu} + {g}_{\sigma\nu} {R}^{\rho}{}_{\mu} \right) + \frac{R}{(n-1)(n-2)} \left( {\delta}^{\rho}{}_{\mu} {g}_{\nu\sigma} - {\delta}^{\rho}{}_{\nu} {g}_{\mu\sigma} \right),
$$

where $n$ is the dimension. In dimensions $n \leq 3$, the Weyl tensor vanishes identically. We can calculate it in OGRe using the built-in module `TWeyl`:

```wl
?TWeyl
```

The FLRW metric is conformally flat, so its Weyl tensor vanishes:

```wl
TList@TWeyl["FLRW"]
```

Since the Schwarzschild metric is Ricci-flat, the Weyl tensor is equal to the Riemann tensor, as we can verify by taking the difference of the two tensors:

```wl
TList@TCalc[TWeyl["Schwarzschild"]["\[Rho]\[Sigma]\[Mu]\[Nu]"]-TRiemann["Schwarzschild"]["\[Rho]\[Sigma]\[Mu]\[Nu]"]]
```

For an example where the Weyl tensor is non-zero but differs from the Riemann tensor, let us define the **Schwarzschild-de Sitter metric**:

```wl
TSetReservedSymbols[\[CapitalLambda]];
TShow@TNewMetric["SchwarzschildDeSitter","Spherical",DiagonalMatrix[{-(1-(2*M)/r-(\[CapitalLambda] r^2)/3),1/(1-(2*M)/r-(\[CapitalLambda] r^2)/3),r^2,r^2 Sin[\[Theta]]^2}]]
```

Its line element is:

```wl
TLineElement["SchwarzschildDeSitter"]
```

Calculating the Weyl tensor, we find:

```wl
TList@TWeyl["SchwarzschildDeSitter"]
```

To see explicitly how it differs from the Riemann tensor, compare the Kretschmann scalar:

```wl
TShow@TKretschmann["SchwarzschildDeSitter"]
```

With the norm-squared of the Weyl tensor:

```wl
TNormSquared[TWeyl["SchwarzschildDeSitter"]]
```

The Kretschmann scalar contains both the Schwarzschild mass term and the cosmological constant term, while the norm-squared of the Weyl tensor contains only the mass term.

### Covariant derivatives

The partial derivative has limited use in general relativity, as **it does not transform like a tensor**. Therefore, it is only used in special cases, such as calculating the Christoffel symbols and the Riemann tensor. The **covariant derivative** ${\nabla}_{\mu}$ is a generalization of the partial derivative, which does transform like a tensor (as long as it acts on a proper tensor). It is defined as follows:

* On a scalar $\Phi$, the covariant derivative acts as ${\nabla}_{\mu}\Phi\equiv {\partial}_{\mu}\Phi$.

* On a vector ${v}^{\mu}$, the covariant derivative acts as ${\nabla}_{\mu} {v}^{\nu}\equiv {\partial}_{\mu} {v}^{\nu}+{\Gamma}_{\mu\lambda}^{\nu} {v}^{\lambda}$.

* On a covector ${w}_{\mu}$, the covariant derivative acts as ${\nabla}_{\mu} {w}_{\nu}\equiv {\partial}_{\mu} {w}_{\nu}-{\Gamma}_{\mu\nu}^{\lambda} {w}_{\lambda}$.

More generally, on a rank $(p,q)$ tensor with components ${T}_{{\sigma}_{1} {\cdots \sigma}_{q}}^{{\nu}_{1} {\cdots \nu}_{p}}$, the covariant derivative ${\nabla}_{\mu} {T}_{{\sigma}_{1} {\cdots \sigma}_{q}}^{{\nu}_{1} {\cdots \nu}_{p}}$ is defined as follows:

* The first term will be ${\partial}_{\mu} {T}_{{\sigma}_{1} {\cdots \sigma}_{q}}^{{\nu}_{1} {\cdots \nu}_{p}}$.

* We **add** one term ${\Gamma}_{\mu\lambda}^{{\nu}_{i}} {T}_{{\sigma}_{1} {\cdots \sigma}_{q}}^{{\nu}_{1} {\cdots \lambda\cdots \nu}_{p}}$ for each upper index ${\nu}_{i}$.

* We **subtract** one term ${\Gamma}_{{\mu\sigma}_{i}}^{\lambda} {T}_{{\sigma}_{1} {\cdots \lambda\cdots \sigma}_{q}}^{{\nu}_{1} {\cdots \nu}_{p}}$ for each lower index ${\sigma}_{i}$.

Note that even though the covariant derivative is made from ingredients that do not transform like tensors - the partial derivative and the Christoffel symbols - the unwanted terms in the transformations of these ingredients cancel each other exactly, so that in the end, the entire sum does transform like a tensor.

As usual, we can, of course, write down the covariant derivative manually. For example, the covariant divergence of the metric is:

$$
{\nabla}_{\mu} {g}_{\alpha\beta}={\partial}_{\mu} {g}_{\alpha\beta}-{\Gamma}_{\mu\alpha}^{\lambda} {g}_{\lambda\beta}-{\Gamma}_{\mu\beta}^{\lambda} {g}_{\alpha\lambda}.
$$

It should vanish, by definition, for any metric; this is what we meant when we said the Levi-Civita connection **preserves** the metric. Indeed, we have for the Schwarzschild metric:

```wl
TList@TCalc[TPartialD["\[Mu]"]."Schwarzschild"["\[Alpha]\[Beta]"]-TChristoffel["Schwarzschild"]["\[Lambda]\[Mu]\[Alpha]"]."Schwarzschild"["\[Lambda]\[Beta]"]-TChristoffel["Schwarzschild"]["\[Lambda]\[Mu]\[Beta]"]."Schwarzschild"["\[Alpha]\[Lambda]"]]
```

Much more conveniently, the covariant derivative is represented in OGRe as `TCovariantD`. It will automatically add the correct terms, as detailed above, for each of the tensor's indices. To use it, simply contract it with any tensor, just like `TPartialD`:

```wl
?TCovariantD
```

For example, we can check that the covariant derivative of the FLRW metric also vanishes:

```wl
TList@TCalc[TCovariantD["\[Mu]"]."FLRW"["\[Alpha]\[Beta]"]]
```

The covariant divergence of the Einstein tensor is:

$$
{\nabla}_{\mu} {G}^{\mu\nu}={\partial}_{\mu} {G}^{\mu\nu}+{\Gamma}_{\mu\lambda}^{\mu} {G}^{\lambda\nu}+{\Gamma}_{\mu\lambda}^{\nu} {G}^{\mu\lambda}.
$$

Note that it involves a contraction in the index $\mu$, which becomes a trace in the first Christoffel symbol. This expression vanishes because of the **Bianchi identity**:

$$
{\nabla}_{\mu} {R}^{\mu\nu}=\frac{1} {2} {\nabla}^{\nu}R \quad \Rightarrow \quad {\nabla}_{\mu} {G}^{\mu\nu}=0.
$$

To calculate it in OGRe, we simply write:

```wl
TList@TCalc[TCovariantD["\[Mu]"].TEinstein["FLRW"]["\[Mu]\[Nu]"]]
```

Finally, for a non-trivial result, let us recall that the stress-energy tensor should be **conserved**:

$$
{\nabla}_{\mu} {T}^{\mu\nu}={\partial}_{\mu} {T}^{\mu\nu}+{\Gamma}_{\mu\lambda}^{\mu} {T}^{\lambda\nu}+{\Gamma}_{\mu\lambda}^{\nu} {T}^{\mu\lambda}=0.
$$

This follows from the fact that ${\nabla}_{\mu} {G}^{\mu\nu}=0$, combined with the **Einstein equation**:

$$
{G}_{\mu\nu}=\kappa{T}_{\mu\nu},
$$

where $\kappa=1$ or $\kappa=8\pi$ depending on your preferred units. However, unlike ${\nabla}_{\mu} {G}^{\mu\nu}=0$, the relation ${\nabla}_{\mu} {T}^{\mu\nu}=0$ is **not** an identity; it is an **energy-momentum conservation equation**. To derive the equation for the FLRW metric, let us first define the rest-frame fluid 4-velocity in this spacetime:

```wl
TShow@TNewTensor["RestVelocity","FLRW","Spherical",{1},{1,0,0,0},"u"]
```

Using the 4-velocity and the metric, we redefine the perfect fluid stress tensor in the FLRW spacetime using the formula ${T}^{\mu\nu}=(\rho+p){u}^{\mu} {u}^{\nu}+{pg}^{\mu\nu}$, and give $\rho$ and $p$ spacetime dependence:

```wl
TCalc["PerfectFluidFLRW",(\[Rho][t,r,\[Theta],\[Phi]]+p[t,r,\[Theta],\[Phi]])"RestVelocity"["\[Mu]"]."RestVelocity"["\[Nu]"]+p[t,r,\[Theta],\[Phi]]"FLRW"["\[Mu]\[Nu]"],"T"];
TChangeDefaultIndices["PerfectFluidFLRW",{1,1}];
TShow["PerfectFluidFLRW"]
```

Finally, we take the covariant derivative of the stress tensor:

```wl
TList@TCalc["FLRWConservation",TCovariantD["\[Mu]"]."PerfectFluidFLRW"["\[Mu]\[Nu]"]]
```

From demanding that the $t$ component vanishes, we get the following equation:

$$
\dot{\rho}=-3(\rho+p)\frac{\dot{a}} {a}.
$$

We see that in an expanding universe, energy is not conserved, but rather, the energy density changes with time in a way that depends on the scale factor. If the universe is not expanding, that is, $\dot{a}=0$, then energy will be conserved.

### Overwriting metrics

If overwriting tensors has been allowed using `TSetAllowOverwrite[True]`, and we overwrite a metric tensor, then all derived tensors cached inside that metric will be automatically deleted, since the corresponding quantities for the new metric will generally be different. For example, let us overwrite the FLRW metric with a similar metric where *k* is equal to 0:

```wl
currentOverwrite=TSetAllowOverwrite[];
TSetAllowOverwrite[True];
TShow@TNewMetric["FLRW","Spherical",DiagonalMatrix[{-1,a[t]^2,a[t]^2 r^2,a[t]^2 r^2 Sin[\[Theta]]^2}]]
```

If we now try to access, for example, `TChristoffel["FLRW"]`, OGRe will recalculate it automatically from the new metric:

```wl
TList@TChristoffel["FLRW"]
```

The same applies to all other tensors derived from the metric using built-in modules. However, user-defined tensors created with `TNewTensor` that are associated to the metric are not automatically deleted, since they do not depend on the metric itself.

Finally, let us reset `TSetAllowOverwrite` back to its previous setting:

```wl
Off[TSetAllowOverwrite::Notify]; TSetAllowOverwrite[currentOverwrite]; On[TSetAllowOverwrite::Notify]
```

## Curves and geodesics

### The curve Lagrangian

Consider a **curve**, which is a function ${x}^{\mu}(\lambda)$ on the manifold where $\lambda$ is called the **curve parameter**. The **curve Lagrangian** of a metric is defined as the norm-squared of the tangent to the curve:

$$
L={g}_{\mu\nu} {\dot{x}}^{\mu} {\dot{x}}^{\nu},
$$

where ${\dot{x}}^{\mu}$ is the first derivative of ${x}^{\mu}$ with respect to the curve parameter (in Newton dot notation). We can calculate it using the module `TLagrangian`:

```wl
?TLagrangian
```

For example:

```wl
TList@TLagrangian["Minkowski"]
```

```wl
TList@TLagrangian["Schwarzschild"]
```

```wl
TList@TLagrangian["FLRW"]
```

```wl
TList@TLagrangian["Alcubierre"]
```

Notice how `TList` (and `TShow`) use Newton dot notation for the derivatives of the coordinate functions, for improved readability. To get the full expressions with the explicit derivatives, we can use `TGetComponents`. For example:

```wl
TGetComponents@TLagrangian["Minkowski"]
```

### Geodesic equations from the Lagrangian

By applying the **Euler-Lagrange equations** to the curve Lagrangian:

$$
\frac{\mathrm{d}}{\mathrm{d}\lambda} \left(\frac{\partial L} {\partial {\dot{x}}^{\mu}}\right)-\frac{\partial L}{\partial {x}^{\mu}}=0,
$$

we can obtain the geodesic equations for our spacetime. This is done using the module `TGeodesicFromLagrangian`:

```wl
?TGeodesicFromLagrangian
```

For the Minkowski metric, the geodesic equations are:

```wl
TList@TGeodesicFromLagrangian["Minkowski"]
```

Note that this module only calculates the left-hand side of the Euler-Lagrange equations; if we equate the result to zero, we will get the actual geodesic equations. This is hinted at visually by setting the resulting tensor's symbol to 0, so that you actually see the equations when using `TList`. It is trivial to see that the solution to these equations is simply a curve with a constant velocity; in a flat Minkowski spacetime, particles experience no gravitational force, and thus no acceleration (unless some other force acts on them, of course).

The derivatives with respect to the curve parameter $\lambda$ are kept unevaluated, using the Mathematica function `Inactive`. This simplifies the equations, and can sometimes help solve them by inspection. If we want to activate the derivatives, we simply need to use `Activate`. Recall that `TList` and `TShow` can apply a function to the tensor's components before displaying them, so we just need to pass `Activate` as the last argument:

```wl
TList[TGeodesicFromLagrangian["Minkowski"],Activate]
```

Now the derivatives have been activated; note that all the components have become second derivatives of the coordinates.

As with the Lagrangian itself, the geodesic equations are displayed in compact notation when using `TList`. If we want the full expressions with the explicit derivatives, for example in order to pass them to `DSolve` and actually solve the equations, we can use `TGetComponents`:

```wl
TGetComponents[TGeodesicFromLagrangian["Minkowski"]]
```

Again, if we wish to activate the derivatives, we need to use `Activate` (recall that a function passed as the last argument to `TGetComponents` is applied to the components before returning them):

```wl
TGetComponents[TGeodesicFromLagrangian["Minkowski"],Activate]
```

We can similarly find the geodesic equations of other metrics:

```wl
TList@TGeodesicFromLagrangian["Schwarzschild"]
```

```wl
TList@TGeodesicFromLagrangian["FLRW"]
```

```wl
TList@TGeodesicFromLagrangian["Alcubierre"]
```

The last example, the geodesic equations of the Alcubierre metric, is a good example of how we can solve the geodesic equations by inspection. Indeed, it is easy to see that

$$
{\dot{x}}^{\mu}=(1,0,0,vf)
$$

is a solution to this system of equations, since then we have $\dot{x}=\dot{y}=0$ and $(f v \dot{t}-\dot{z})=0$, and both terms in each equation vanish (the last term in the first equation will reduce to ${\partial}_{\lambda}(-1)$, which is of course zero). We can verify this solution by replacing the coordinate functions with their solutions; since we will be left with ${\partial}_{\lambda}(-1)$ in the first equation, we must also activate the derivative:

```wl
TList[TGeodesicFromLagrangian["Alcubierre"],(Activate@ReplaceAll[#,{t'[\[Lambda]]->1,x'[\[Lambda]]->0,y'[\[Lambda]]->0,z'[\[Lambda]]->v[t[\[Lambda]]]f[t[\[Lambda]],x[\[Lambda]],y[\[Lambda]],z[\[Lambda]]]}])&]
```

Notice how we had to write the coordinates explicitly as **functions of the curve parameter**, even when they are arguments of a function; for example, `v[t]` became `v[t[\[Lambda]]]`. This solution indicates that we are traveling with velocity $v$ in the $z$ direction; the warp bubble (inside which, as you recall, $f = 1$) moves whatever is inside it, such as a spaceship, through space at the velocity $v$, but there is no limit on $v$ - it can even be faster than light!

### Geodesic equations from the Christoffel symbols

Another way of obtaining the geodesic equations is using the covariant derivative, and thus the Christoffel symbols:

$$
{\dot{x}}^{\rho} {\nabla}_{\rho} {\dot{x}}^{\sigma}=0 \quad \Rightarrow \quad {\ddot{x}}^{\sigma}+{\Gamma}_{\mu\nu}^{\sigma} {\dot{x}}^{\mu} {\dot{x}}^{\nu}=0.
$$

In OGRe, we can calculate the left-hand side of this equation using `TGeodesicFromChristoffel`:

```wl
?TGeodesicFromChristoffel
```

For example:

```wl
TList@TGeodesicFromChristoffel["Minkowski"]
```

```wl
TList@TGeodesicFromChristoffel["Schwarzschild"]
```

```wl
TList@TGeodesicFromChristoffel["FLRW"]
```

```wl
TList@TGeodesicFromChristoffel["Alcubierre"]
```

Often, you will find that the Lagrangian method produces simpler equations, which can even be solved by inspection, as we did for the Alcubierre metric. This is due to the possibility of leaving the $\lambda$ derivative unevaluated. However, in other cases, the Christoffel method might produce simpler equations. The best thing to do is to try both methods and see which one produces simpler or nicer results for the specific metric in question. Note that the system of equations obtained using `TGeodesicFromLagrangian` will often be different from the one obtained using `TGeodesicFromChristoffel`, but both systems will always have the same solutions.

### Changing the curve parameter

By default, the curve parameter is $\lambda$. However, sometimes we want to use another parameter - for example $\tau$ for proper time. To change the parameter, we use `TSetCurveParameter`:

```wl
?TSetCurveParameter
```

Let us change it to $\tau$:

```wl
TSetCurveParameter[\[Tau]];
```

If we get the components of any Lagrangian or affine-parameter geodesic equation we previously calculated, the parameter will now be shown as $\tau$ instead of $\lambda$:

```wl
TGetComponents@TLagrangian["Minkowski"]
```

```wl
TList@TGeodesicFromLagrangian["Minkowski"]
```

### Geodesic equations in terms of the time coordinate

If the metric is a spacetime metric, it is often convenient to obtain the geodesic equations in terms of the time parameter, instead of an affine curve parameter. It can be shown that the geodesic equations in terms of the time coordinate are given by

$$
\frac{\mathrm{d}^{2} {x}^{\sigma}}{{\mathrm{d}t}^{2}}+\left({\Gamma}_{\mu\nu}^{\sigma}-{\Gamma}_{\mu\nu}^{0}\frac{{\mathrm{d}x}^{\sigma}} {\mathrm{d}t}\right) \frac{{\mathrm{d}x}^{\mu}} {\mathrm{d}t}\frac{{\mathrm{d}x}^{\nu}}{\mathrm{d}t}=0,
$$

where we are assuming the time coordinate is $t$ and it is the first (zero) coordinate. These equations can be obtained using `TGeodesicWithTimeParameter`:

```wl
?TGeodesicWithTimeParameter
```

Note that `TGeodesicWithTimeParameter` assumes time is the first coordinate, but the coordinate does not need to have the symbol $t$. As an example, the equations for the FLRW metric in terms of a curve parameter are

```wl
TList[TGeodesicFromChristoffel["FLRW"],"Cartesian"]
```

but in terms of $t$, we only need 3 equations:

```wl
TList[TGeodesicWithTimeParameter["FLRW"],"Cartesian"]
```

These equations are easier to solve. For simplicity, assume that we are only moving along the $x$ coordinate. Then we only have one equation to solve:

```wl
FLRWEq=TGetComponents[TGeodesicWithTimeParameter["FLRW"],"Cartesian",y'[t]^2+z'[t]^2->0][[2]]
```

The solution can be obtained in terms of an integral over $a(t)$:

```wl
FLRWSol=DSolve[FLRWEq==0,x,t]
```

From this we can get the coordinate velocity $\dot{x}$ along x:

```wl
x'[t]==FullSimplify[a[t]D[x[t]/.FLRWSol[[2]],t],a[t]>0]/a[t]
```

## Importing and exporting tensors

In a single Mathematica session, one can spend considerable time and effort defining tensors and doing various operations on them. However, as the tensors are only stored in memory, once the session is over and the kernel is stopped, all that information will be lost. Due to the non-linear nature of Mathematica notebooks, even if you saved the entire notebook, it can be hard or even impossible to retrace your steps and get the exact same tensors again from the information in the notebook.

Instead of defining all the tensors from scratch, OGRe allows the user to export tensors and then import them in another session to continue working with them later. The tensors are stored internally as an `Association`, and exporting a tensor essentially amounts to outputting the corresponding `Association`. **Warning: Do not change the exported data manually, as that might break the class invariants and cause errors after importing it back!**

To export a single tensor, use the `TExport` module:

```wl
?TExport
```

For example, here is how the 4-velocity is stored internally:

```wl
TExport["4-Velocity"]
```

This is a nested `Association`. The upper level has just one key: `"4-Velocity"`, which is the ID of the tensor. Its value is another `Association`, which has the following keys:

* `"Components"`: An `Association` containing the components of the tensor in different representations, each with a specific index configuration and coordinate system. The components are only generated when a particular combination of indices and coordinates is requested for the first time, so for example, here the components with both an upper and a lower index in Minkowski coordinates have been stored, but no components in spherical coordinates, since we have not tried to access them so far.
* `"DefaultCoords"`: The default coordinate system to use when displaying the tensor.
* `"DefaultIndices"`: The default index configuration to use when displaying the tensor.
* `"Metric"`: The unique ID of the metric that will be used to raise and lower the tensor's indices. Note that this is only a reference, so a tensor object with this ID must exist. If a tensor is exported, its metric must be exported separately as well for raising and lowering of indices to work.
* `"Role"`: The role of the tensor, which depends on the module that created it. Will be `"Coordinates"` if the tensor was created using `TNewCoordinates`, `"Metric"` if the tensor was created using `TNewMetric`, or `"Tensor"` if the tensor was created using `TNewTensor`. Other modules that we will discuss below, such as `TCalc`, `TChristoffel`, and `TRiemann`, have corresponding roles as well. Additional roles are only used internally by OGRe, such as `"Temporary"` for a temporary tensor created as an intermediate step in a calculation.
* `"Symbol"`: The symbol used to represent the tensor when displaying it.
* `"OGReVersion"`: The version of the package used to create the tensor. (Note that this key is not stored internally, it is added by `TExport`.)

Other keys also exist in special cases. For example, for tensor objects representing coordinate systems, the keys `"CoordTransformations"` and `"Jacobians"` are used to store the details of coordinate transformations defined using `TAddCoordTransformation`, as can be seen by exporting `"Cartesian"`:

```wl
TExport["Cartesian"]
```

Built-in tensors derived from a metric, such as `TRicci["FLRW"]`, are cached inside the metric, not stored as top-level tensor objects with a unique ID. They can still be exported individually:

```wl
TExport[TRicci["FLRW"]]
```

In this case, the exported key will have the form `"FLRW->Ricci"`; this is not a tensor ID, but a reference to a cached built-in tensor. The `->` reference operator is reserved for these built-in tensors, and cannot be used in tensor IDs created by the user. Since the built-in tensors are cached inside the metric object, exporting the metric will also export any cached built-in tensors derived from it.

To import a tensor back after it has been exported, use the `TImport` module:

```wl
?TImport
```

To export **all** of the tensors defined in the current session, we may use the `TExportAll` module:

```wl
?TExportAll
```

The output will be an `Association` as above, where the keys are the names of all the tensors defined so far in the current session, and the value of each key is the data of that tensor. We will not show the complete output here, since it is very long, but let us just demonstrate that the keys of the `Association` are all of the tensors we defined so far in this session:

```wl
Keys@TExportAll[]
```

`TExportAll` exports not only the tensors, but also a special key called `Options`, which contains the current version of the package (for compatibility purposes, because the storage format may change between versions) and any options set by the user during the session. Note that keys associated with tensor objects, such as `"Cartesian"`, `"Spherical"`, and so on, are always strings, but `Options` is not a string; this is to ensure it doesn't get accidentally interpreted as a tensor object. We can see the options configured in this session by reading the value of the `Options` key:

```wl
TExportAll[][Options]
```

Note that the settings we have discussed, such as the index letters, reserved symbols, and simplification assumptions, are included. This means that these settings will be imported when you import the tensor data in another Mathematica session - you can start right where you left off, no need to reconfigure anything.

The output of `TExportAll` can be saved in a Mathematica notebook, and imported using the module `TImportAll`:

```wl
?TImportAll
```

Note that `TImportAll` will delete any tensors already defined in the current session, whether or not they have the same ID as an imported tensor. To keep them, first export them into an `Association`, `Join` it with the `Association` you wish to import, and then use `TImportAll` on the result - or, alternatively, import the tensors one by one using `TImport`. Similarly, any settings configured during the session will be replaced with the imported settings. `TImport` and `TImportAll` will issue a warning when importing data exported by a different OGRe version, and the tensors or settings may not be imported correctly in that case; when transitioning between versions, it is best to re-define all the tensors from scratch.

If a file name is given to `TExportAll`, the output will be saved to that file. If a file extension is not provided, the extension `.m` will be appended, unless the file name explicitly ends with a dot. If only the name of the file is given, and not a full path - e.g. `TExportAll["OGReTensors.m"]` - then the file will be saved in the current working directory, as given by `Directory[]`. To change the working directory, use `SetDirectory[]` before exporting the file. To import from the file, pass the file name to `TImportAll`, e.g. `TImportAll["OGReTensors.m"]`.

Note that if for some reason you would like to delete all the tensors defined so far in the current session, you can simply import an empty `Association` as follows: `TImportAll[<||>]`. Be careful, as this action is **irreversible**!

Lagrangians and affine-parameter geodesic equations are stored internally using the placeholder `$CurveParam` for the curve parameter. The placeholder is replaced with the user-selected curve parameter only when they are displayed or the components are retrieved. When exporting tensors using `TExport` or `TExportAll`, the tensors will be exported with the placeholder, so that when they are imported later, they will be imported correctly regardless of which curve parameter is currently selected:

```wl
TExport[TLagrangian["Minkowski"]]
```

`TExportAll` also exports the choice of user-facing curve parameter separately as part of the `Options` key, and it is imported when using `TImportAll`, so your parameter of choice can persist between sessions.

## Parallelization

### Improving performance with parallelization

The calculations we have demonstrated so far in this documentation have been quite simple, and should not take more than a second to perform on a decent computer. However, when doing research, calculations can be much more involved, and thus also take more time to complete. Typically, the most time-consuming part of any tensor calculation is not the tensor operations themselves, but rather the **simplification** of the final result using `FullSimplify`.

If simplification is taking more than a few seconds, it is highly recommended to turn on the **parallelization** feature, which simplifies the components of the tensors in parallel instead of one after the other. This can provide a significant performance boost, proportional to the number of parallel kernels that can be launched. Note that this number is determined by your Mathematica license, and it may be less than the number of cores in your CPU.

Since we would like to measure execution time in this section, let us use the command `ClearSystemCache` to clear Mathematica's cache - since otherwise it will remember the results of previous simplifications, which may artificially speed up the calculation:

```wl
ClearSystemCache[]
```

To demonstrate the benefits of parallelization, let us consider the following somewhat complicated non-diagonal metric, which depends on an abstract function $f$:

```wl
TShow[TNewMetric["ParallelizationTest","Cartesian",Table[f[a b t],{a,1,4},{b,1,4}]]]
```

We first calculate the Christoffel symbols for this metric **without** parallelization, and use `AbsoluteTiming` to measure how long it takes:

```wl
notParallelChristoffel=AbsoluteTiming[TChristoffel["ParallelizationTest"]][[1]]
```

This is the number of seconds this calculation took on my computer. On your computer this duration may be shorter or longer, but the calculation will invariably take a non-trivial amount of time on any personal computer. The vast majority of that time is spent not on calculating the tensor itself, but on **simplifying** the result.

To turn on parallelization, we use the module `TSetParallelization`:

```wl
?TSetParallelization
```

Let us turn it on now:

```wl
TSetParallelization[True]
```

When parallelization is first turned on, all available parallel kernels are automatically launched. As you can see, my system has a 24-core CPU, but only 16 parallel kernels can be launched, since that is what my Mathematica license allows.

We will now repeat the calculation of the Christoffel symbols, in order to see how its performance improves with parallelization. However, before we can do that, we must again use the command `ClearSystemCache` to clear Mathematica's cache:

```wl
ClearSystemCache[]
```

(If you don't trust that `ClearSystemCache` is enough to make a reliable benchmark, you can exit the kernel using `Quit`, and then reload the package and redefine the Cartesian coordinates and the test metric.)

We will also delete the tensor that we calculated previously, so that we can calculate it again (if overwriting is not turned on):

```wl
TDelete[TChristoffel["ParallelizationTest"]];
```

Now we can accurately measure the execution time for `TChristoffel` with parallelization:

```wl
parallelChristoffel=AbsoluteTiming[TChristoffel["ParallelizationTest"]][[1]]
```

We see that the calculation now took less time, with an improvement by a factor of:

```wl
notParallelChristoffel/parallelChristoffel
```

With longer calculations, the improvement will be even more significant; I intentionally chose an example that doesn't take too long to calculate, because I evaluate this notebook on a regular basis as part of the package's development. Increasing the number of kernels (if it was allowed by my license) would provide an additional speedup.

As a rule of thumb, if simplifications are taking less than a few seconds, then you should leave parallelization off, as it has a small overhead and may actually impede performance in that case. However, if simplifications are taking more than a few seconds, then it is highly recommended to enable parallelization for a significant performance boost.

### Making use of OGRe's parallelized simplifications for other Mathematica expressions

When `TSimplify` is applied to any expression which is not a tensor object, the object is simplified based on the user-defined simplification assumptions set using `TSetAssumptions`:

```wl
TSetAssumptions[]
```

```wl
TSimplify[Sqrt[r^2]]
```

If the expression is a `List`, the components will be simplified in parallel. The user can thus make use of OGRe's optimized simplification process to simplify any Mathematica expression.

## About the project

### Bug reports and feature requests

This package is under continuous and active development. If you encounter any bugs, or if you would like to request any additional features, please feel free to [open a new issue on GitHub](https://github.com/bshoshany/OGRe/issues) and I will look into it as soon as I can.

### Contribution and pull request policy

Contributions are always welcome. However, I release my projects in cumulative updates after editing and testing them locally on my system, so **my policy is to never accept any pull requests**. If you open a pull request, and I decide to incorporate your suggestion into the project, I will first modify your code to comply with the project's coding conventions (formatting, syntax, naming, comments, programming practices, etc.), and perform some tests to ensure that the change doesn't break anything. I will then merge it into the next release of the project, possibly together with some other changes. The new release will also include a note in `CHANGELOG.md` with a link to your pull request, and modifications to the documentation in `README.md` as needed.

### Starring the repository

If you found this project useful, please consider [starring it on GitHub](https://github.com/bshoshany/OGRe/stargazers)! This allows me to see how many people are using my code, and motivates me to keep working to improve it.

### Acknowledgements

A major portion of the code for this package was written while I was a postdoctoral researcher in Niayesh Afshordi's group at Perimeter Institute for Theoretical Physics in Waterloo, Ontario, Canada. I would like to thank Niayesh Afshordi and Perimeter Institute for their support during that time. Research at Perimeter Institute is supported in part by the Government of Canada through the Department of Innovation, Science, and Economic Development Canada and by the Province of Ontario through the Ministry of Colleges and Universities.

Further work on this package was done as an assistant professor at Brock University. I acknowledge the support of the Natural Sciences and Engineering Research Council of Canada (NSERC), RGPIN-2024-04063.

### Copyright and citing

Copyright (c) 2021-2026 [Barak Shoshany](https://baraksh.com/). Licensed under the [MIT license](https://github.com/bshoshany/OGRe/blob/master/LICENSE.txt).

If you use this package in software of any kind, please provide a link to [the GitHub repository](https://github.com/bshoshany/OGRe) in the source code and documentation.

If you use this package in published research, please cite it as follows:

* Shoshany, B., (2021). OGRe: An Object-Oriented General Relativity Package for Mathematica. Journal of Open Source Software, 6(65), 3416, <https://doi.org/10.21105/joss.03416>

You can also use the following BibTeX entry:

```none
@article{Shoshany2021_OGRe,
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
}
```

Citing information can always be obtained by executing the command `TCite[]`:

```wl
?TCite
```

## Other projects to check out

This package has a Python port, [OGRePy: An Object-Oriented General Relativity Package for Python](https://github.com/bshoshany/OGRePy). If you are interested in doing OGRe-style tensor calculations in Python, please check it out!

If you are interested in high-performance scientific computing with C++, and want to get the maximum performance possible from multi-core CPUs, please check out [`BS::thread_pool`: a fast, lightweight, modern, and easy-to-use C++17 / C++20 / C++23 thread pool library](https://github.com/bshoshany/thread-pool).
