# Model Improvement Plan

The next model-improvement pass should follow a measured evaluation loop rather than ad hoc tuning.

## Step 1
Build a reproducible imageability evaluation script using the 4,683 words that have both embeddings and non-missing `imageability_m` values. Report outer-cross-validated `R^2`, RMSE, and MAE, and save fold-level predictions.

## Step 2
Compare a small set of high-value candidate models:

- elastic net on raw embeddings
- elastic net with tuned `alpha`
- PCA plus elastic net
- PCA plus elastic net plus word length
- raw embeddings plus word length

## Step 3
Only promote a new model into the package if it shows a consistent out-of-sample win. If preprocessing such as PCA is used, ship the preprocessing object together with the fitted model and metadata.

## Step 4
Add a lightweight regression guard so future edits do not silently degrade predictive performance.
