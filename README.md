[![DOI:10.5281/zenodo.4742935](https://zenodo.org/badge/DOI/10.5281/zenodo.4742935.svg)](https://doi.org/10.5281/zenodo.4742935)
[![License: MIT](https://img.shields.io/github/license/bshoshany/thread-pool)](https://github.com/bshoshany/OGRe/blob/master/LICENSE.txt)
![Language: Mathematica 12](https://img.shields.io/badge/Language-Mathematica%2012-yellow)
![File size in bytes](https://img.shields.io/github/size/bshoshany/OGRe/OGRe.m)
![GitHub last commit](https://img.shields.io/github/last-commit/bshoshany/OGRe)
[![GitHub repo stars](https://img.shields.io/github/stars/bshoshany/OGRe?style=social)](https://github.com/bshoshany/OGRe)
[![Twitter @BarakShoshany](https://img.shields.io/twitter/follow/BarakShoshany?style=social)](https://twitter.com/BarakShoshany)

# OGRe: An Object-Oriented General Relativity Package for Mathematica

**Barak Shoshany**\
Department of Physics, Brock University,\
1812 Sir Isaac Brock Way, St. Catharines, Ontario, L2S 3A1, Canada\
[bshoshany@brocku.ca](mailto:bshoshany@brocku.ca) | [https://baraksh.com/](https://baraksh.com/)\
DOI: [doi:10.5281/zenodo.4742935](https://doi.org/10.5281/zenodo.4742935)

<!-- TOC depthFrom:2 -->

- [Summary](#summary)
- [Features](#features)
- [Installing and loading the package](#installing-and-loading-the-package)
- [Documentation](#documentation)
- [Copyright and citing](#copyright-and-citing)

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
* Under continuous and active development. Bug reports and feature requests are welcome, and should be made via [GitHub issues](https://github.com/bshoshany/OGRe/issues).

<a id="markdown-installing-and-loading-the-package" name="installing-and-loading-the-package"></a>
## Installing and loading the package

This package is compatible with Mathematica 12.0 or newer. It consists of only one file, `OGRe.m`. There are several different ways to load the package:

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

For version history, please see the file `CHANGELOG.md`.

<a id="markdown-copyright-and-citing" name="copyright-and-citing"></a>
## Copyright and citing

Copyright (c) 2021 [Barak Shoshany](http://baraksh.com). Licensed under the [MIT license](LICENSE.txt).

If you use this package in published research, please cite it as follows:

* Barak Shoshany, "OGRe: An Object-Oriented General Relativity Package for Mathematica", [doi:10.5281/zenodo.4742935](https://doi.org/10.5281/zenodo.4742935) (May 2021)

You can use the following BibTeX entry:

```none
@article{Shoshany2021_OGRe,
    author = {Barak Shoshany},
    doi    = {10.5281/zenodo.4742935},
    month  = {May},
    title  = {{OGRe: An Object-Oriented General Relativity Package for Mathematica}},
    year   = {2021}
}
```
