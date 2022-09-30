# CRAN submission for canaper 1.0.0

2022-09-30

This is the first submission of canaper to CRAN, so it has no reverse dependencies.

Thank you for reviewing this submission.

## Test environments

* local OS X install, R 4.2.0 (devtools::check())
* Ubuntu Linux 20.04.1 LTS, R-release, GCC (rhub)
* Fedora Linux, R-devel, clang, gfortran (rhub)
* Windows Server 2022, R-devel, 64 bit (rhub)
* Windows Server 2022, x86_64-w64-mingw32 (64-bit) (win-builder)

## R CMD check results

Ubuntu Linux 20.04.1 LTS, R-release, GCC (rhub) resulted in a PREPERROR. However, the [build log](https://builder.r-hub.io/status/canaper_1.0.0.tar.gz-e885f4fe4a044b68ae4b19e7ebe21e62) shows `Status: success`. This is [a known problem with rhub](https://github.com/r-hub/rhub/issues/448), which seems to be due to overly long logs.

### NOTEs by platform

* The following NOTE was found on all builds:

```
* checking CRAN incoming feasibility ... [4s/51s] NOTE
Maintainer: ‘Joel H. Nitta <joelnitta@gmail.com>’

New submission

Possibly misspelled words in DESCRIPTION:
  Endemism (2:47)
  endemism (26:83)
  neo (26:68)
```

These words are not misspelled.

* Fedora Linux, R-devel, clang, gfortran (rhub):

```
* checking examples ... [24s/24s] NOTE
Examples with CPU (user + system) or elapsed time > 5s
                     user system elapsed
cpr_classify_endem 11.246  0.328  11.586

* checking HTML version of manual ... NOTE
Skipping checking HTML validation: no command 'tidy' found
```

I am not sure why the cpr_classify_endem() example took > 5s on this platform; on my computer it runs in < 2s.

It seems `Skipping checking HTML validation: no command 'tidy' found` could be suppressed by setting `_R_CHECK_RD_VALIDATE_RD2HTML_` to false, but [apparently that just turns off HTML validation](https://developer.r-project.org/blosxom.cgi/R-devel/2022/04/28), which happens anyways.

* Windows Server 2022, R-devel, 64 bit (rhub)

```
* checking CRAN incoming feasibility ... [40s] NOTE
Maintainer: 'Joel H. Nitta <joelnitta@gmail.com>'

New submission

Possibly misspelled words in DESCRIPTION:
  Endemism (2:47)
  endemism (26:83)
  neo (26:68)

Found the following (possibly) invalid URLs:
  URL: https://doi.org/10.1002/ajb2.1848
    From: README.md
    Status: 503
    Message: Service Unavailable
  URL: https://doi.org/10.1002/ecy.2043
    From: inst/doc/canape.html
          inst/doc/how-many-rand.html
    Status: 503
    Message: Service Unavailable
  URL: https://doi.org/10.1111/ecog.01814
    From: README.md
    Status: 503
    Message: Service Unavailable
  URL: https://doi.org/10.1890/0012-9658(2003)084[0532:sainma]2.0.co;2
    From: inst/doc/how-many-rand.html
    Status: 503
    Message: Service Unavailable

* checking examples ... [19s] NOTE
Examples with CPU (user + system) or elapsed time > 5s
                   user system elapsed
cpr_classify_endem 7.64   0.26    7.92

* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'Rscript1058267d0c10' 'Rscriptbd4267d0c10' 'lastMiKTeXException'
```

I checked these URLs and none are broken.

As previously noted in [R-hub issue #503](https://github.com/r-hub/rhub/issues/503), `'lastMiKTeXException'` has been flagged as a bug in MiKTeK and probably can be safely ignored.

`'Rscript18a82568a7f6'` and `'Rscriptffc2568a815'` seem to be left behind by parallel processes (https://stat.ethz.ch/pipermail/r-devel/2021-June/080830.html)

* Windows Server 2022, x86_64-w64-mingw32 (64-bit) (win-builder)

```
Found the following (possibly) invalid file URIs:
  URI: .github/CONTRIBUTING.md
    From: README.md
  URI: LICENSE.md
    From: README.md
```

`.github/CONTRIBUTING.md` is included in the package repo.
