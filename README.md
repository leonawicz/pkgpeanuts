
<!-- README.md is generated from README.Rmd. Please edit that file -->
pkgpeanuts <a href="man/figures/logo.png" _target="blank"><img src="man/figures/logo.png" style="margin-left:10px;margin-bottom:5px;" width="120" align="right"></a>
====================================================================================================================================================================

[![Travis-CI Build Status](https://travis-ci.org/leonawicz/pkgpeanuts.svg?branch=master)](https://travis-ci.org/leonawicz/pkgpeanuts) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/leonawicz/pkgpeanuts?branch=master&svg=true)](https://ci.appveyor.com/project/leonawicz/pkgpeanuts) [![Coverage Status](https://img.shields.io/codecov/c/github/leonawicz/pkgpeanuts/master.svg)](https://codecov.io/github/leonawicz/pkgpeanuts?branch=master)

The intent of `pkgpeanuts` is to automate building out robust R package scaffolding for a newly created package. Create a new R package project in RStudio. Make an initial commit and link up the local repository with your empty remote repository. Then let `pkgpeanuts` take care of the rest.

This is not intended for existing packages.

Installation
------------

You can install the development version of `pkgpeanuts` from GitHub with:

``` r
# install.packages("remotes")
remotes::install_github("leonawicz/pkgpeanuts")
```

Package setup
-------------

After creating a new package in RStudio, make sure the new, empty repository exists on GitHub (or BitBucket) as well. Make an initial commit, e.g. commit the `.Rproj` file, so that you can add and push to the remote repository. If this has not been done, `pkgpeanuts` will fail. Once this minimal setup is complete, run something like the following from the package root directory:

``` r
pkgpeanuts::pour("github_username")
```

`pkgpeanuts` has many arguments. See the help documentation for details. And again, this is not intended for existing packages.

Motivation
----------

At each of my last two workplaces I have created workplace context-specific renditions of this package. This has enabled me to spin up new packages at work quickly and still have them conforming to professional standards and looking polished, despite limited time. By avoiding manually fussing with extraneous details I'd prefer to automate, I can focus on the fun part: making the actual package. `pkgpeanuts` represents a generalization of my previous packages, stripping away code related to specific contexts. By design it must try to achieve less than my other packages did, but if generalized appropriately, could be useful to the R community.

This is largely a wrapper around a plethora of `usethis` functions, plus additional functions from `pkgpeanuts`. It helps you slingshot past making many individual calls to various package setup functions, particularly when you leverage a lot of them and make a lot of packages. My goal is to make this package general enough that it can be useful to others, while allowing it to still handle as much as it possible so that users can really feel the boost.

Reference
---------

[Complete package reference and function documentation](https://leonawicz.github.io/pkgpeanuts/)
