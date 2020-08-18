## Test environments

* local macOS Catalina 10.15.6 install, R 4.0.2
* Windows Server 2008 R2 SP1, R-devel 32/64 bit via rhub::check()
* Ubuntu Linux 16.04 LTS, R-release, GCC, via devtools::check_rhub()
* Fedora Linux, R-devel, clang, gfortran, via devtools::check_rhub()
* Windows, via devtools::check_win_oldrelease(), 
    devtools::check_win_devel(), and devtools::check_win_release()
* Linux Xenial on R oldrel, release, devel, via Travis CI

## R CMD check results
There were no ERRORs or WARNINGs.

There is one NOTE:
    checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: 'Mark White <markhwhiteii@gmail.com>'

## Downstream dependencies
There are no packages depending on this package, verified via devtools::revdep()
