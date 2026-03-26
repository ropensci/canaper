## Plan: MVP parquet randomization I/O

Add two exported helper functions to persist randomization outputs to parquet
and load them later, without changing existing analysis workflows yet. Keep
schema simple and stable: one row per community per randomization replicate,
with metric columns and metadata needed for reproducible downstream use.

**Steps**
1. Discover current randomization output shape and naming conventions
   - Confirm the object returned by `cpr_rand_test()` and internal
     randomization structures in [R/cpr_rand_test.R](R/cpr_rand_test.R) and
     [R/calc_biodiv_random.R](R/calc_biodiv_random.R).
   - Identify minimal columns needed for round-trip re-use (community id,
     replicate index, metric values, null model settings).
2. Define parquet schema and function contracts (*blocks 3–5*)
   - New writer: `cpr_write_randomizations_parquet(x, path, ...,
     compression = "zstd")`
   - New reader: `cpr_read_randomizations_parquet(path, ...,
     as = c("data.frame","tibble"))`
   - Enforce deterministic column names and types (character ids, integer
     replicate, numeric metrics).
3. Implement writer function (*depends on 2*)
   - Add [R/cpr_write_randomizations_parquet.R](R/cpr_write_randomizations_parquet.R)
     with input validation using existing style (e.g., `stopifnot`/assert
     helpers used in package).
   - Convert accepted input (`data.frame`/`tibble`) to Arrow Table and write
     parquet via `arrow::write_parquet`.
   - Store lightweight metadata (package version, timestamp, optional user
     metadata) in file metadata if available through Arrow API.
4. Implement reader function (*depends on 2*)
   - Add [R/cpr_read_randomizations_parquet.R](R/cpr_read_randomizations_parquet.R).
   - Read via `arrow::read_parquet`, validate required columns, return
     requested class.
   - Provide clear error messages for missing required columns or incompatible
     schema.
5. Expose and document API (*parallel with 3–4 once names fixed*)
   - Export functions in [NAMESPACE](NAMESPACE) via roxygen tags.
   - Add roxygen docs and examples; generate/update man pages in [man/](man/).
   - Update package docs mention in [README.Rmd](README.Rmd) or
     [NEWS.md](NEWS.md) (minimal note for MVP).
6. Add focused tests (*depends on 3–4*)
   - New tests in [tests/testthat/](tests/testthat/) for:
     - write→read round trip equality for representative randomization table
     - required column/type validation
     - overwrite behavior and path handling
   - Use temp files and skip-if-no-arrow pattern if Arrow is optional.
7. Dependency and CRAN considerations (*depends on 3–6*)
   - Decide dependency policy:
     - Option A (recommended MVP): `Suggests: arrow` + runtime checks + test
       skips.
     - Option B: `Imports: arrow` if feature is core and always expected.
   - Update [DESCRIPTION](DESCRIPTION) and conditionally guard functions if
     Arrow absent.

**Relevant files**
- [R/cpr_rand_test.R](R/cpr_rand_test.R) — source of randomization results to
  serialize
- [R/calc_biodiv_random.R](R/calc_biodiv_random.R) — metric calculation
  shape/columns
- [R/utils.R](R/utils.R) — reuse input-validation helpers/patterns
- [DESCRIPTION](DESCRIPTION) — Arrow dependency policy
- [NAMESPACE](NAMESPACE) — exports
- [tests/testthat/](tests/testthat/) — parquet round-trip and validation tests
- [NEWS.md](NEWS.md) — short MVP changelog note

**Verification**
1. Run targeted tests for new files: `testthat::test_file()` on parquet I/O
   tests.
2. Run full test suite: `devtools::test()`.
3. Run package checks enough for docs/namespace integrity: `devtools::check()`
   (or at least `document()` + tests).
4. Manual smoke check: write randomization table to temp parquet, restart
   session, read back, verify dimensions/types and key summary values.

**Decisions**
- Included in MVP:
  - Two standalone functions for parquet write/read.
  - Schema validation and round-trip tests.
- Excluded from MVP:
  - Integrating parquet directly into `cpr_rand_test()` execution loop.
  - Partitioned datasets, chunked append writes, or cloud/object-store
    backends.
  - Backward compatibility with multiple schema versions beyond v1.

**Further Considerations**
1. Input object for writer: should MVP accept only a "long randomizations
   table" or also raw `cpr_rand_test` objects? Recommendation: start with
   table-only for simplicity.
2. Required metadata columns: include `null_model`, `n_iterations`, and `seed`
   now, or keep only replicate/community + metrics? Recommendation: include
   them for provenance.
3. Dependency policy: keep Arrow optional to reduce install friction.
   Recommendation: `Suggests` + explicit error message if functions are used
   without Arrow.
