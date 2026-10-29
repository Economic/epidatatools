# Working on epidatatools

This is EPI's R package for retrieving economic data, finding series, and computing
summary statistics. Read `CLAUDE.md` for the public interfaces, development workflow,
and test conventions. Use `=` for assignment and qualified package calls, following
the surrounding code.

Make changes on a branch, in small commits. Pull requests target `dev`; `main`
receives merges from `dev`. Do not merge, tag, or release without being asked.
Keep unrelated local work out of commits. Update `NEWS.md` for user-visible fixes
and regenerate `man/` with `devtools::document()` when roxygen comments change.
Run the full workflow in `CLAUDE.md` before declaring the work complete. Live API
tests are opt-in; prefer offline fixtures for deterministic regression checks.

`find_bls()` ports `Economic/bls-index/examples/find_bls.R`. Keep its search rules,
returned columns, ordering, and cache behavior aligned with that reference. Use the
same offline catalog and search inputs to check parity when changing the client.

## Reviews with roborev

Run `roborev init` once per checkout to register the repo and install its local
post-commit hook. `.roborev.toml` is tracked; the hook and daemon database are local.
Every local commit gets one code review automatically in an isolated checkout.
Use `roborev list`, `roborev show <job>`, and `roborev wait <job>` to inspect results.

At a completed feature or issue milestone, run:

```sh
roborev review --branch --base origin/dev --panel checkpoint_panel
```

Inspect the result before finishing. Use `--panel design_only_panel` for a new
design. Security review is included in the checkpoint panel, especially for API
keys, downloads, and GitHub Actions. Extra automatic design reviews are disabled.

Treat findings as evidence to assess, not instructions to add code:

- Fix reproducible failures of the public contract, incorrect data or dates, lost
  usable caches, and exposed credentials. Add a focused regression check.
- Require a concrete trigger, affected user behavior, and a proportionate fix.
  Prefer a small correction to new abstractions, retries, state, or dependencies.
- Do not expand an issue to cover speculative malformed inputs, hostile local
  cache edits, concurrent maintainers, or several unusual failures combined unless
  there is evidence of a meaningful user impact within the issue's scope.
- Respect explicit non-goals and accepted risks in issues and plans. In particular,
  semantic corruption of local cache filenames is accepted by the reference client;
  it does not justify new filename or checksum validation here.
- For an out-of-scope or low-benefit finding, record the reason with
  `roborev comment <job> "..."` and close it with `roborev close <job>`.
  Do not silently discard findings. After fixing findings, review the changed code
  again. Stop when meaningful defects are resolved and only accepted risks remain;
  do not keep iterating over narrower hypothetical cases.

After several milestones, `roborev insights` can help tune noisy review guidance.
