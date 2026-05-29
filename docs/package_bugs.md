# Real bugs found in user packages

This file tracks **real type bugs in package source code** that the
checker found during stage-5 corpus runs — distinct from checker
limitations / false positives, which live in `checker_limitations.md`.

A bug counts as "real" if the package's own code would fail at runtime
under the inputs it accepts, OR if it has a non-typecheckable usage
pattern that the maintainer would want to fix.

Empty so far. Each entry should include:
- Package name + version
- Source file + line
- The diagnostic the checker produced
- Why it's a real bug (vs. checker noise)
- Suggested fix
