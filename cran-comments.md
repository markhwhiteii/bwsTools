## Test environments
* local macOS Mojave 10.14 install, R 4.0.0
* Windows Sever 08, via devtools::check_win_oldrelease(), 
    devtools::check_win_devel(), and devtools::check_win_release()
* Ubuntu Linux and Fedora Linux, via devtools::check_rhub()
* Linux Xenial on R oldrelease, release, devel, via Travis CI

## R CMD check results
There were no ERRORs or WARNINGs.

There is one NOTE:
    checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: 'Mark White <markhwhiteii@gmail.com>'

## Downstream dependencies
There are no packages depending on this package, verified via devtools::revdep()
