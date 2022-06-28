# Contributing to Atlas

This is a draft. It may be subject to change.

## Versioning

This project uses Semantic Versioning[^1] v2.0.0.

A summary from the website:

Given a version number MAJOR.MINOR.PATCH, increment the:

* MAJOR version when you make incompatible API changes,
* MINOR version when you add functionality in a backwards compatible manner, and
* PATCH version when you make backwards compatible bug fixes.

Additional labels for pre-release and build metadata are available as
extensions to the MAJOR.MINOR.PATCH format.

## Commit message format

This project makes use of the Conventional Commits[^2] v1.0.0. Any
commit that makes its way into the `main` branch shall respect such
convention.

The following types are accepted besides `fix` and `feat`: `revert`,
`build`, `chore`, `ci`, `docs`, `refactor`, `perf`, `test`, `format`,
and `lint`.

Titles of commit messages shall be in lower case.

Regarding revert commits, Conventional Commits doesn't define a way of
dealing with them, leaving it to the authors to define it themselves. We
take a recommendation from CC to define them like the following:

```
revert: let us never again speak of the noodle incident

Refs: 676104e, a215868
```

The footer must contain references to the commits being reverted, as
to facilitate Semantic Versioning. The highest level of change amongst
such commits is the one that gets used as the version for the revert
commit. For example, if the referenced commits are a mixture of MINOR
and PATCH changes, then MINOR is picked. But, if one of them is a MAJOR
change, then MAJOR is picked.

[^1]: https://semver.org/spec/v2.0.0.html
[^2]: https://www.conventionalcommits.org/en/v1.0.0
