# Changelog for collect-errors

## v0.1.2.0

* Add CanTakeErrors type class and liftTakeErrors function

* Add instance of deepseq's NFData

## v0.1.1.0

* Add removeValue functions

## v0.1.0.0

* Initial port of CollectErrors and CN from mixed-types-num-0.4.1

* Simplify the code by abandoning EnsureCE and related type functions and utilities

* NumErrors within CN are now a set to prevent multiple occurrences of the same error

* Add CollectError or CN instances for various Prelude type classes, including Num and Floating, checking for domain violations
