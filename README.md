# OGRe: An **O**bject-Oriented **G**eneral **Re**lativity Package for Mathematica

<!-- TOC depthFrom:2 -->

- [Summary](#summary)
- [Features](#features)
- [Installing and loading the package](#installing-and-loading-the-package)
- [Documentation](#documentation)
- [Version history](#version-history)
- [Citing](#citing)
- [Copyright and license](#copyright-and-license)

<!-- /TOC -->

<a id="markdown-summary" name="summary"></a>
## Summary

OGRe is a modern Mathematica package for tensor calculus, designed to be both powerful and user-friendly. It can be used in a variety of contexts where tensor calculations are needed, in both mathematics and physics, but it is especially suitable for general relativity.

Tensors are abstract objects, which can be represented as multi-dimensional arrays once a choice of index configuration and coordinate system is made. OGRe stays true to this definition, but takes away the complexities that come with combining tensors in different representations. This is done using an object-oriented programming approach, as detailed in the documentation.

The user initially defines each tensor in OGRe using its explicit components in any single representation. Operations on this tensor are then done abstractly, without needing to specify which representation to use. Possible operations include addition of tensors, multiplication of tensor by scalar, trace, contraction, and partial and covariant derivatives.

OGRe will automatically choose which representation to use for each tensor based on how the tensors are combined. For example, if two tensors are added, then OGRe will automatically use the same index configuration for both. Similarly, if two tensors are contracted, then OGRe will automatically ensure that the contracted indices are one upper (contravariant) and one lower (covariant). OGRe will also automatically transform all tensors being operated on to the same coordinate system.

Transformations between representations are done behind the scenes; all the user has to do is specify which metric to use for raising and lowering indices, and how to transform between the coordinate systems being used. This also means that there is no room for user error. The user cannot mistakenly perform "illegal" operations such as 2A<sup>&mu;&nu;</sup>+B<sub>&mu;&lambda;</sub>C<sub>&lambda;&nu;</sub>. Instead, the user simply inputs the names of the tensors, the order (but **not** the configuration) of indices for each, and the operations to perform - and the correct combination 2A<sup>&mu;&nu;</sup>+B<sup>&mu;</sup><sub>&lambda;</sub>C<sup>&lambda;&nu;</sup> will be automatically deduced.

I initially created OGRe for use in my own research, so I made it as flexible and powerful as possible. I also wanted my students to be able to use it easily and efficiently, even if they only have minimal experience with Mathematica and/or general relativity, so I made it simple to learn and easy to use. As a result, this package is equally suitable for both experienced and novice researchers.

<a id="markdown-features" name="features"></a>
## Features

* Define coordinate systems and the transformation rules between them. Tensor components are then transformed automatically between coordinates behind the scenes as needed.
* Each tensor is associated with a specific metric. Tensor components are then transformed automatically between different index configurations, raising and lowering indices behind the scenes as needed.
* Display any tensor in any index configuration and coordinate system, either in vector/matrix form or as a list of all unique non-zero elements.
* Automatically simplify tensor components, optionally with user-defined simplification assumptions. Simplifications can be parallelized for a significant performance boost.
* Export tensors to a Mathematica notebook or to a file, so they can later be imported into another Mathematica session without having to redefine them from scratch.
* Easily calculate arbitrary tensor formulas using any combination of addition, multiplication by scalar, trace, contraction, partial derivative, and covariant derivative.
* Built-in modules for calculating the Christoffel symbols (Levi-Civita connection), Riemann tensor, Ricci tensor and scalar, and Einstein tensor. More will be added in future versions.
* Built with speed and performance in mind, using optimized algorithms designed specifically for this package.
* Fully portable. Can be imported directly from the web into any Mathematica notebook, without downloading or installing anything.
* Clear and detailed documentation, with many examples, in both [Mathematica notebook](OGRe_Documentation.nb) and [PDF](OGRe_Documentation.pdf) format. Detailed usage messages are also provided.
* Open source. The code is extensively documented; please feel free to fork and modify it as you see fit.
* Under active development. Please see the "future plans" section of the documentation for more information. Bug reports and feature requests are welcome, and should be made via GitHub issues.

<a id="markdown-installing-and-loading-the-package" name="installing-and-loading-the-package"></a>
## Installing and loading the package

The package consists of only one file, `OGRe.m`. There are several different ways to load the package:

* **Run from local file with installation:** This is the recommended option, as it allows you to permanently use the package offline from any Mathematica notebook. Download the file `OGRe.m` from [the GitHub repository](https://github.com/bshoshany/OGRe) and copy it to the directory given by `FileNameJoin[{$UserBaseDirectory, "Applications"}]`. The package may now be loaded from any notebook by writing ``Needs["OGRe`"]`` (note the backtick <code>&#96;</code> following the word OGRe).

* **Run from local file without installation:** This option allows you to use the package in a portable fashion, without installing it in the `Applications` directory. Download the file `OGRe.m` from [the GitHub repository](https://github.com/bshoshany/OGRe), place it in the same directory as the notebook you would like to use, and use the command `Get["OGRe.m", Path->NotebookDirectory[]]` to load the package.

* **Run from web with installation:** This option allows you to automatically download and install the package on any computer. Simply run the command `URLDownload["https://raw.githubusercontent.com/bshoshany/OGRe/master/OGRe.m", FileNameJoin[{$UserBaseDirectory, "Applications", "OGRe.m"}]]` from any Mathematica notebook to permanently install the package. Then use ``Needs["OGRe`"]`` from any notebook to load it.

* **Run from web without installation:** This option allows you to use the package from any Mathematica notebook on any computer, without manually downloading or installing it, as long as you have a working Internet connection. It also ensures that you always use the latest version of the package, but be aware that updates may sometimes not be fully backwards compatible. Simply write `Get["https://raw.githubusercontent.com/bshoshany/OGRe/master/OGRe.m"]` in any Mathematica notebook to load the package.

To uninstall the package, just delete the file from the `Applications` directory, which can be done from within Mathematica using the command `DeleteFile[FileNameJoin[{$UserBaseDirectory, "Applications", "OGRe.m"}]]`.

<a id="markdown-documentation" name="documentation"></a>
## Documentation

The full and detailed documentation for this package may be found in the following repository files:

* `OGRe_Documentation.nb`: An interactive Mathematica notebook. Requires Mathematica to open.
* `OGRe_Documentation.pdf`: A PDF version of the notebook. Can be viewed with any PDF reader.

Once the package is loaded, the documentation can be easily accessed by executing the command `TDocs[]`, which automatically downloads the file `OGRe_Documentation.nb` from GitHub and opens it in Mathematica.

<a id="markdown-version-history" name="version-history"></a>
## Version history

* v1.2 (2021-04-28)
    * New modules:
        * `TDocs`: Opens the Mathematica notebook `OGRe_Documentation.nb` directly from the GitHub repository. This allows users to instantly access the latest documentation from any Mathematica session, whether the package itself was loaded locally or from GitHub.
        * `TParallelize`:
            * Enables or disable the parallelization of tensor simplifications (see below). It is disabled by default.
            * As a rule of thumb, if simplifications are taking less than a few seconds, then you should leave parallelization off, as it has a small overhead and may actually impede performance in that case. However, if simplifications are taking more than a few seconds, then it is highly recommended to enable parallelization for a significant performance boost.
            * This setting will be exported when using `TExportAll`, so it will be imported automatically with `TImportAll` in future sessions.
            * Aspects of the package other than simplification are not parallelized, since they are typically short calculations with few elements, and would not benefit from parallelization.
    * Changes to existing modules:
        * `TCalc` now displays a progress bar (see below).
        * `TCheckForUpdates`:
            * Fixed a bug where this module falsely reported that a new version is available.
            * Fixed a bug where this module issued an error when giving the option to download `OGRe.m` to the current notebook directory if the notebook is an Untitled notebook, meaning it is not an actual file in the file system and thus does not have a notebook directory.
            * This module now gives the user the option to reload the new version directly from GitHub without saving it locally.
        * `TList` and `TShow`:
            * The output of these modules is now uneditable. I made this change after I noticed that some users were editing the output, thinking that they are editing the tensor itself. (An option to actually edit the elements of a tensor in `MatrixForm` may be added in a future version.)
            * These modules now take a fourth optional argument: a function to map to each of the tensor's elements before they are displayed. Typically this would be `ReplaceAll[rules]` to apply the `rules` to the elements, but any function can be used. Note that applying `ReplaceAll` to the output of `TShow` or `TList` directly won't work, since the output is formatted.
        * `TNewMetric` now verifies that the metric components are an invertible, symmetric, and square matrix.
    * Other changes:
        * The performance of simplifications has been improved considerably!
            * Simplifications are now performed automatically less often, to prevent simplifying the same expression multiple times.
            * In addition, simplifications can now be parallelized. This is enabled using the new module `TParallelize` (see above). The parallelization is implemented using a thread pool model (with `ParallelSubmit` and `WaitAll`), which in my benchmarks was found to provide better performance than the simpler `Parallelize` or `ParallelMap`.
            * Parallelization can potentially provide a speedup proportional to the number of parallel kernels. Note that this number is determined by your Mathematica license, and it may be less than the number of cores in your CPU.
        * Calculations with `TCalc` and simplifications now both display progress bars, so you can keep track of long calculations. This was a popular feature request, especially for simplifications, since they are usually the most time-consuming operations.
        * Improved the appearance of the welcome message.
        * Messages from OGRe are now displayed with a special cell label.
        * Information on how to load the package is now displayed in the `README.md` file.
* v1.1 (2021-04-15)
    * New modules:
        * `TCheckForUpdates`: Automatically checks the GitHub repository for updates. If a new version of the package is available, the module will offer an option to download or install the new version and reload the package.
        * `TGetComponents`: Extracts the components of a tensor object in a specific representation as a `List`.
        * `TInfo`: Displays information about a tensor object, and any other objects associated with it, in human-readable form.
    * Changes to existing modules:
        * `TAddCoordTransformation` now also calculates the Jacobian of the transformation when it is executed, and stores the result for future use within the tensor object of the source coordinates. This improves performance by using the pre-calculated Jacobian whenever a coordinate transformation is performed, instead of calculating it from scratch every time, as was the case in v1.0.
        * `TChristoffel` now explicitly marks the resulting tensor object as having special transformation properties. The Levi-Civita connection, whose components are the Christoffel symbols, does not transform like a tensor under a coordinate transformation, and OGRe now automatically knows to use the correct transformation rule. **Please note that tensors created with `TChristoffel` in v1.0 will not transform correctly, so they should be recalculated after updating.**
        * `TExport` now adds a key named `"OGReVersion"` which records the version of the package used to create the tensor being exported. This will be used in future versions to ensure backwards compatibility.
        * `TExportAll` now exports, in addition to all the tensors defined so far, a special key, `Options`, containing information about the current session. Currently, this key stores the version of the package, the index letters to use, and the simplification assumptions set by the user. When importing the data in another session using `TImportAll`, the version number will be used to ensure backwards compatibility, and the other options will be used to restore any user-defined index letters and simplification assumptions made during the session.
        * `TNewTensor` now allows defining the components of the new tensor in any coordinate system. In v1.0, the components had to be defined in the default coordinate system of the associated metric. This is still the default behavior if a coordinate system is not specified, for compatibility with v1.0, but it is recommended to always specify the coordinate system explicitly, to avoid accidentally defining the tensor with the wrong components.
        * `TSimplifyAssumptions` now appends new simplification assumptions to the list of previously added  assumptions, instead of replacing it. Also, OGRe now automatically assumes that all variables are real, which helps simplify certain expressions. If you are using more exotic variables, use `TSimplifyAssumptions[!Reals]` to disable this assumption.
    * Other changes:
        * The `"Role"` key of each tensor object now indicates how that object was created. In v1.0, tensors created with `TNewCoordinates` had the role `"Coordinates"` and tensors created with `TNewMetric` had the role `"Metric"`, but all other tensors had the role `"General"`. Now tensors will have the roles `"Tensor"`, `"Calculated"`, `"Christoffel"`, `"Riemann"`, `"Ricci Tensor"`, `"Ricci Scalar"`, or `"Einstein"` if they were created using `TNewTensor`, `TCalc`, `TChristoffel`, `TRiemannTensor`, `TRicciTensor`, `TRicciScalar`, or `TEinstein` respectively. This is currently just for bookkeeping, but may have other uses in future versions. Note that tensors imported from v1.0 will still have the role `"General"`.
        * Improved the formatting of the usage messages for all OGRe modules. They no longer break in the middle of words.
        * Debug mode has been removed. If the package is loaded more than once in a single session, it will redefine all symbols but keep any previously defined tensors intact. This is useful both for debugging and for reloading the package after an update.
    * Bug fixes:
        * Fixed a bug where the partial and covariant derivatives of a scalar were not calculated correctly.
* v1.0 (2021-02-10)
    * Initial release.

<a id="markdown-citing" name="citing"></a>
## Citing

If you use this package in your research, please cite it as follows:

* Barak Shoshany, OGRe: An Object-oriented General Relativity Package for Mathematica, [https://github.com/bshoshany/OGRe](https://github.com/bshoshany/OGRe) (2021).

(This citation will be replaced with a journal reference once a paper is published.)

<a id="markdown-copyright-and-license" name="copyright-and-license"></a>
## Copyright and license

Copyright (c) 2021 [Barak Shoshany](http://baraksh.com) (baraksh@gmail.com). Licensed under the [MIT license](LICENSE.txt).
