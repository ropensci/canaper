# Parquet I/O for Randomizations - Usage Example

This document demonstrates how to use parquet I/O functionality to save and
load randomization results in `cpr_rand_test()`.

## Background

Randomization tests in `cpr_rand_test()` can be computationally expensive,
especially for large datasets with many replicates. The parquet I/O functions
allow you to:

1. Save randomization results to disk after initial computation
2. Read them back in later sessions without re-computing
3. Share randomization results across different analyses

## Basic Workflow

### Integrated Usage (Recommended)

The easiest way to use parquet I/O is through the `save_rand` and `load_rand`
parameters in `cpr_rand_test()`:

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
  save_rand = "my_randomizations.parquet"
)

# Later, in a new session, load previously saved randomizations
results <- cpr_rand_test(
  phylocom$comm,
  phylocom$phy,
  null_model = "swap",
  load_rand = "my_randomizations.parquet"
)
```

**Note**: When using `load_rand`, the `null_model` and `n_reps` parameters
are ignored since the randomizations are loaded from the file. However,
`null_model` is still required syntactically.

### Advanced Usage (Low-level Functions)

For advanced users who need more control, the low-level functions
`cpr_write_rand_parquet()` and `cpr_read_rand_parquet()` are also available
for working with the internal randomization structure:

```r
# The functions are available for working with internal structures
# See function documentation for details:
?cpr_write_rand_parquet
?cpr_read_rand_parquet
```

## Validation and Error Handling

The integration includes comprehensive validation:

- **Mutual exclusivity**: Cannot specify both `save_rand` and `load_rand`
- **File existence**: `load_rand` checks that the specified file exists
- **Site name matching**: Loaded randomizations must match the sites in `comm`
- **Metric availability**: Loaded file must contain all requested metrics
- **Arrow package**: Clear error message if `arrow` is not installed

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

- **Efficient storage**: Compressed format saves disk space (zstd compression)
- **Fast I/O**: Optimized for reading/writing tabular data
- **Cross-platform**: Works on Windows, Mac, Linux
- **Language-agnostic**: Can be read by Python, R, Julia, etc.
- **Columnar format**: Efficient for analytical queries

## Installation Note

The parquet functionality requires the `arrow` package:

```r
install.packages("arrow")
```

If `arrow` is not installed, the functions will provide a clear error message
with installation instructions.

## Future Enhancements

Potential future improvements could include:

1. Support for partitioned datasets (by site or replicate)
2. Metadata storage (null_model, n_iterations, seed, timestamp)
3. Progress indicators for large files
4. Compression level options
5. Cloud storage backend support (S3, GCS, etc.)
