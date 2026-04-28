# Research Ideas

This file records the highest-value conceptual directions for `wordfeatures` so
they survive day-to-day package work.

## 1. Cross-Model Cognitive Alignment Benchmark

Core claim: lexical norm datasets can serve as a benchmark for how well modern
embedding models recover human semantic judgments.

Working hypothesis:

- embeddings alone should recover a substantial amount of human semantic
  judgment structure across multiple lexical variables
- embeddings plus structured, leakage-safe lexical features should outperform
  either source alone
- the size and pattern of those gains should differ by target, revealing which
  semantic dimensions are well captured by modern embeddings and which still
  require auxiliary lexical structure

What we are hoping to see:

- stable, nontrivial out-of-sample performance across multiple norm targets
- clear target-specific performance patterns rather than one flat leaderboard
- consistent gains from combining embeddings with non-leaky lexical features
- eventually, meaningful differences across embedding families such as OpenAI,
  Gemini, and multimodal systems

Why this matters conceptually:

- it frames psycholinguistic norms as a cognitive alignment surface rather than
  just a collection of regression targets
- it lets the package ask where modern semantic representations align with human
  judgments and where they still miss structured aspects of meaning
- it creates a publishable bridge between classical lexical semantics and
  foundation-model evaluation

Why it matters:

- no new human data collection is required
- it connects psycholinguistics and foundation-model evaluation
- it supports direct comparisons across OpenAI, Gemini, and future multimodal
  embedding systems

Initial deliverables:

- fixed benchmark subsets and folds
- leakage-safe target definitions
- model-family comparisons for multiple semantic variables
- reproducible benchmark tables and plots
- legacy-source replication targets from the MRC Psycholinguistic Database

Current empirical note:

- first full benchmark results are summarized in
  [research/results/benchmarks/cognitive_alignment_results.md](research/results/benchmarks/cognitive_alignment_results.md)

## 2. Auditing Classical Lexical Norms

Core claim: modern embeddings and cross-dataset redundancy can reveal
measurement tension, polysemy contamination, and construct drift in canonical
norm resources.

Working hypothesis:

- some lexical variables, such as valence or concreteness, will show high
  cross-dataset agreement
- others, especially imageability and context-sensitive terms, will show much
  weaker agreement and more item-level instability
- duplicated Glasgow entries with explicit contexts will expose cases where
  word-level norms collapse meaningful sense distinctions

What we are hoping to see:

- clear family-level differences in cross-dataset agreement
- a ranked list of words with unusually large disagreement across norm sources
- interpretable Glasgow context conflicts for polysemous or ambiguous words

Why this matters conceptually:

- it reframes norm datasets as measurement instruments that can themselves be
  audited
- it avoids treating legacy norms as unquestioned gold standards
- it creates a publishable bridge between lexical semantics, psychometrics, and
  modern representation learning

Why it matters:

- reframes the package from predictor to measurement-audit tool
- does not assume older datasets are perfect gold standards
- can surface publishable disagreement structure without collecting new ratings

Initial deliverables:

- disagreement tables across overlapping norm sources
- duplicate-word and context-sensitive conflict summaries
- flagged lexical items for probable annotation or construct mismatch
- explicit legacy-vs-modern comparisons using harmonized MRC norms

Current empirical note:

- current audit findings are summarized in
  [research/results/audit/norm_audit_results.md](research/results/audit/norm_audit_results.md)

## 3. Synthetic Norm Expansion with Uncertainty

Core claim: sparse lexical norms can be expanded credibly when predictions are
paired with uncertainty and provenance rather than point estimates alone.

Working hypothesis:

- some sparse lexical norms will be predictable enough to justify expansion at
  scale
- the strongest candidates will combine good held-out performance with large
  missing coverage
- bootstrap or repeated-resampling uncertainty can separate high-confidence
  candidate expansions from weaker speculative predictions

What we are hoping to see:

- a subset of targets with clearly usable held-out performance
- missing rows that can be expanded with nontrivial but interpretable
  uncertainty intervals
- a resource-oriented output table that carries both provenance and confidence

Why this matters conceptually:

- it turns the package from a predictor into a norm-expansion resource builder
- it makes the output more defensible than bare point imputation
- it creates a path to a publishable data-release contribution

Why it matters:

- useful to other researchers as a resource contribution
- leverages the package's merged feature frame directly
- creates a path from package utility to publishable data release

Initial deliverables:

- held-out evaluation protocol for sparse targets
- interval or repeated-CV uncertainty estimates
- expanded norm tables with metadata on source coverage and model confidence
- cross-source transfer checks using legacy MRC targets

Current empirical note:

- current exploratory expansion results are summarized in
  [research/results/expansion/norm_expansion_results.md](research/results/expansion/norm_expansion_results.md)

## Strategy

Push the cognitive-alignment benchmark first, but build the underlying
infrastructure so the audit and expansion projects reuse the same data-loading,
split-definition, and reporting machinery.

The MRC Psycholinguistic Database now sits in the research stack as a classic
legacy source. The package ships the parsed raw dataset, while the research
layer harmonizes it into:

- a long-form norm table alongside the NoRaRe-derived sources
- a research-only training frame with `*_mrc_1981` targets
- a shared bridge for benchmark, audit, and expansion analyses

## Combined Paper Framing

Ideas 1 and 2 likely belong in the same paper.

Integrated claim:

- lexical norm datasets are useful cognitive-alignment benchmarks for semantic
  representations
- but they are not perfectly stable targets, and their disagreement structure
  is itself theoretically informative
- modern semantic models can therefore both be evaluated by lexical norms and
  help audit those norms as measurement instruments

Why the combination is stronger:

- idea 1 alone risks becoming a benchmark paper
- idea 2 alone risks becoming a measurement paper
- together they create a reciprocal argument: the norms evaluate the models,
  and the models help evaluate the norms

Likely paper kernel:

- some semantic variables are both predictable and cross-dataset stable
- some are predictable but unstable across datasets
- some are hard to predict and also unstable
- some disagreement is explained by context-sensitive polysemy rather than
  simple noise
