# Changelog for collect-errors

## v0.1.0.0

* Initial port of CollectErrors and CN from mixed-types-num-0.4.2

* Simplify the code by abandoning EnsureCE and related type functions and utilities

* NumErrors within CN are now a set to prevent multiple occurrences of the same error

* Add CollectError or CN instances for various Prelude type classes, including Num and Floating, checking for domain violations
