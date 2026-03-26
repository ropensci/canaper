# Parquet I/O for Randomizations - Usage Example

This document demonstrates how to use the new `cpr_write_rand_parquet()` and
`cpr_read_rand_parquet()` functions to save and load randomization results.

## Background

Randomization tests in `cpr_rand_test()` can be computationally expensive,
especially for large datasets with many replicates. The parquet I/O functions
allow you to:

1. Save randomization results to disk after initial computation
2. Read them back in later sessions without re-computing
3. Share randomization results across different analyses

## Basic Workflow

### Step 1: Note - Current Limitation

**Important**: The current MVP implementation provides low-level functions for
writing and reading randomization data. These functions work with the internal
randomization structure and are designed for future integration into
`cpr_rand_test()`.

For now, these functions are primarily useful for:
- Advanced users who want to manually save/load intermediate results
- Future development of integrated workflows

### Step 2: Potential Future Integration

In a future version, you might be able to use these functions like this:

```r
library(canaper)
library(arrow)  # Required for parquet functionality

# Load example data
data(phylocom)

# Run randomization test and save results
results <- cpr_rand_test(
  phylocom$comm,
  phylocom$phy,
  null_model = "swap",
  n_reps = 1000,
  save_rand = "my_randomizations.parquet"  # Future feature
)

# Later, in a new session, resume analysis:
results <- cpr_rand_test(
  phylocom$comm,
  phylocom$phy,
  null_model = "swap",
  load_rand = "my_randomizations.parquet"  # Future feature
)
```

### Current Usage (Advanced)

Currently, these functions can be used to work with the internal randomization
structure if you need to save/load intermediate results:

```r
# The functions are available but require working with internal structures
# See function documentation for details:
?cpr_write_rand_parquet
?cpr_read_rand_parquet
```

## Data Format

The parquet files store randomization results in a long-format table with:

- `site`: Character column with site/community identifiers
- `replicate`: Integer column with replicate numbers (1 to n_reps)
- Metric columns: Numeric columns for each metric (pd, pe, rpd, rpe, etc.)

Example structure:

```
  site    replicate    pd      pe      rpd     rpe
  site1   1            0.45    0.32    0.89    1.12
  site1   2            0.48    0.35    0.91    1.08
  site2   1            0.52    0.41    0.95    1.15
  site2   2            0.50    0.38    0.93    1.10
  ...
```

## Advantages of Parquet Format

- **Efficient storage**: Compressed format saves disk space
- **Fast I/O**: Optimized for reading/writing tabular data
- **Cross-platform**: Works on Windows, Mac, Linux
- **Language-agnostic**: Can be read by Python, R, Julia, etc.
- **Columnar format**: Efficient for analytical queries

## Installation Note

The parquet functionality requires the `arrow` package:

```r
install.packages("arrow")
```

If `arrow` is not installed, the functions will provide a clear error message.

## Future Enhancements

Potential future improvements could include:

1. Direct integration with `cpr_rand_test()` via `save_rand` and `load_rand`
   parameters
2. Support for partitioned datasets (by site or replicate)
3. Metadata storage (null_model, n_iterations, seed, timestamp)
4. Progress indicators for large files
5. Compression level options
6. Cloud storage backend support (S3, GCS, etc.)
