# CRAN submission for canaper 1.0.1

2023-05-05

This is a bug fix to address [failing tests related to comparison of NA values](https://github.com/ropensci/canaper/issues/22).

## Reverse dependencies

Revdeps were checked with revdepcheck::revdep_check(). canaper currently has no revdeps.

## Test environments

* local OS X install, R 4.2.0 (devtools::check())
* Ubuntu Linux 20.04.1 LTS, R-release, GCC (rhub)
* Fedora Linux, R-devel, clang, gfortran (rhub)
* Windows Server 2022, R-devel, 64 bit (rhub)
* Windows Server 2022, x86_64-w64-mingw32 (64-bit) (win-builder)

### NOTEs by platform

* This NOTE was found on  x86_64-w64-mingw32 (64-bit):

```
Found the following (possibly) invalid URLs:
  URL: http://ropensci.r-universe.dev/ui/#package:canaper
    From: README.md
    Status: Error
    Message: Empty reply from server
  URL: https://app.codecov.io/gh/ropensci/canaper?branch=main
    From: README.md
    Status: Error
    Message: schannel: failed to receive handshake, SSL/TLS connection failed
  URL: https://docs.ropensci.org/canaper/articles/how-many-rand.html
    From: inst/doc/canape.html
    Status: Error
    Message: schannel: failed to receive handshake, SSL/TLS connection failed
  URL: https://phylodiversity.net/phylocom/
    From: README.md
    Status: Error
    Message: schannel: failed to receive handshake, SSL/TLS connection failed
```

I have manually checked, and all of these URLs are valid.

* The following NOTEs was found on Windows Server 2022, R-devel, 64 bit:

```
* checking for non-standard things in the check directory ... NOTE
Found the following files/directories:
  ''NULL''

* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'
```

There is no file or folder called 'NULL'.

As previously noted in [R-hub issue #503](https://github.com/r-hub/rhub/issues/503), `'lastMiKTeXException'` has been flagged as a bug in MiKTeK and probably can be safely ignored.

* The following NOTE was found on Fedora Linux, R-devel, clang, gfortran and  	Ubuntu Linux 20.04.1 LTS, R-release, GCC:

```
* checking HTML version of manual ... NOTE
Skipping checking HTML validation: no command 'tidy' found
```

The NOTE about 'tidy' is a [known issue](https://github.com/r-hub/rhub/issues/548) with R CMD check run by rhub
