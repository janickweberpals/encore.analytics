# Input types and model arguments of the pooling functions

Shared documentation of
[`km_pooling`](https://janickweberpals.github.io/encore.analytics/reference/km_pooling.md)
and
[`cox_pooling`](https://janickweberpals.github.io/encore.analytics/reference/cox_pooling.md).
Both functions fit one model per imputed dataset and then pool the
results with Rubin's rules. This page describes which inputs they accept
and which arguments are passed on to the per-dataset model.

## Arguments

- x:

  The imputed datasets to analyze. One of:

  - a list of data frames (one per imputed dataset), e.g., the output of
    [`raking_weights`](https://janickweberpals.github.io/encore.analytics/reference/raking_weights.md)
    or of `MatchThem::complete(..., action = "all")`

  - a weighted (`wimids`) or matched (`mimids`) object created with
    MatchThem

  - an imputed (`mids`) object created with mice

  See the section "Input types and default model arguments" for how each
  input is handled.

- ...:

  Additional arguments passed on to the per-dataset model call
  ([`survfit2`](http://www.danieldsjoberg.com/ggsurvfit/reference/survfit2.md)
  for
  [`km_pooling()`](https://janickweberpals.github.io/encore.analytics/reference/km_pooling.md),
  [`coxph`](https://rdrr.io/pkg/survival/man/coxph.html) for
  [`cox_pooling()`](https://janickweberpals.github.io/encore.analytics/reference/cox_pooling.md)).
  They are used in addition to, and take precedence over, the default
  arguments of the input type (see the section "Input types and default
  model arguments"). Only `formula` and `data` are set internally and
  cannot be supplied here. Column names are written without quotes and
  are looked up in each dataset, e.g. `weights = w` or `id = patientid`.

## Input types and default model arguments

The datasets are first brought into the form of a list of data frames.
Depending on the type of `x`, default model arguments are set:

|  |  |  |  |
|----|----|----|----|
| Input `x` | How the datasets are obtained | `weights` | `cluster` |
| `wimids` | `MatchThem::complete(x, action = "all", all = FALSE, include = FALSE)` | `weights` | none |
| `mimids` | same as `wimids` | `weights` | `subclass` |
| `mids` | `mice::complete(x, action = "all", include = FALSE)` | none (unweighted) | none |
| list of data frames | used as supplied | none | none |

**Lists of data frames are never interpreted automatically.** A list can
come from many sources (weighting, matching, raking, your own code) so
the function cannot know which columns are meant as weights or clusters.
You state this explicitly through `...`, for example:

- weighted data: `weights = weights`

- matched data: `weights = weights, cluster = subclass`

- weights stored in a column with another name: `weights = w`

If the datasets contain a column named `weights` or `subclass` that is
not used because no `weights` or `cluster` (or `id`) argument was
specified, a warning is raised that names the argument to add. Specify
`weights = NULL` or `cluster = NULL` to confirm that the column should
deliberately be ignored.

**Important**: when you build the list from a `mimids` or `wimids`
object yourself, use
`MatchThem::complete(x, action = "all", all = FALSE, include = FALSE)`.
With `all = TRUE`, unmatched patients and patients with zero weight are
included in the models.

## Precedence of arguments in `...`

Default arguments of the input type are applied first, then the
arguments you supply via `...`. If both contain the same argument,
**your argument wins**. For example, for a `mimids` object
`cluster = NULL` fits the model without clustering on the matched sets
and `cluster = pair_id` clusters on another column. `formula` and `data`
are the only reserved arguments.

`robust` is set automatically: unless you specify it, `robust = TRUE` is
used whenever weights, a cluster or an id are part of the model (the
model-based variance is not valid in these cases), otherwise the default
of the model function is kept. An explicitly supplied `robust` always
takes precedence.

## Transparency

Every call prints a message that lists how the datasets were obtained
and exactly which arguments were used for the models, including where
each argument comes from (default for the input type, derived, or
specified by you). Use
[`suppressMessages()`](https://rdrr.io/r/base/message.html) to silence
it.
