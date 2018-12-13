# Revision history for sensu-run

## 0.6.1 -- 2018-12-13

* Fix the broken --timeout option
* Add support for GHC 8.6

## 0.6.0.3 -- 2018-11-06

* Relax upper version bound for base

## 0.6.0.2 -- 2018-09-12

* Add source-repository stanza in the cabal file

## 0.6.0.1 -- 2018-09-11

* Relax upper version bound for network

## 0.6.0 -- 2018-07-09

* Lock a file to prevent multiple instances of the same name from running
    * Use --no-lock option to disable this behavior

## 0.5.0.3 -- 2018-07-07

* Relax upper version bound for lens
* Update Stackage LTS to 11.16

## 0.5.0.2 -- 2018-06-11

* Relax upper version bound for aeson

## 0.5.0.1 -- 2018-04-30

* Relax upper version bound for network and fix a warning

## 0.5.0 -- 2018-04-20

* Add support for HTTPS (#18)
* Do not append new line to the output when the command was successful
* Add --redirect option (#12)
* Relax upper version bound for temporary
* Tighten upper version bound for unix

## 0.4.0.5 -- 2018-04-09

* Relax upper version bounds for base and aeson

## 0.4.0.4 -- 2018-01-30

* Relax upper version bounds for http-types, lens and time

## 0.4.0.3 -- 2017-11-07

* Relax upper version bound for http-types

## 0.4.0.2 -- 2017-11-07

* Relax upper version bound for unix-compat

## 0.4.0.1 -- 2017-10-26

* Relax upper version bound for http-types

## 0.4.0 -- 2017-08-03

* Include command output in response even on timeout/failure (#14)

## 0.3.0 -- 2017-07-25

* Add user field to result JSON (#7)
* Handle missing executable properly (#10)

## 0.2.0 -- 2017-07-12

* Relax upper version bound for base
* Kill child process(es) when timeout occurs

## 0.1.1.3 -- 2017-06-11

* Relax upper version bound for optparse-applicative
* Binary releases

## 0.1.1 -- 2017-06-09

* Add --version option

## 0.1.0 -- 2017-06-07

* Fix a bug that ignores --handle option by accident
* Small documentation fixes
* Enable AppVeyor builds

## 0.0.0  -- 2017-05-16

* Initial release
