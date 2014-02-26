
Bumper is a tool for working with cabal packages. It lets you manage the version bounds of packages by transitively bumping packages (and their dependencies transitively), without you needing to edit the cabal files manually.

It's useful if you have a set of packages that you develop together.

To get started you should have all projects under a common directory, such as
```
project/package-1
project/package-2
```

You can now make changes to these packages and use bumper to handle version bumps. See Examples below.


## Interface

```
$ bumper --help
Usage: bumper [OPTIONS...], with the following options:
  -m PACKAGE(,PACKAGE)*  --major=PACKAGE(,PACKAGE)*                         Comma-separated list of packages which will get a major bump (bump at position 1).
  -n PACKAGE(,PACKAGE)*  --minor=PACKAGE(,PACKAGE)*                         Comma-separated list of packages which will get a minor bump (bump at position 2).
  -0 PACKAGE(,PACKAGE)*  --bump-0=PACKAGE(,PACKAGE)*                        Comma-separated list of packages which will get a bump at position 0.
  -1 PACKAGE(,PACKAGE)*  --bump-1=PACKAGE(,PACKAGE)*                        Comma-separated list of packages which will get a bump at position 1.
  -2 PACKAGE(,PACKAGE)*  --bump-2=PACKAGE(,PACKAGE)*                        Comma-separated list of packages which will get a bump at position 2.
  -3 PACKAGE(,PACKAGE)*  --bump-3=PACKAGE(,PACKAGE)*                        Comma-separated list of packages which will get a bump at position 3.
                         --set-versions=PACKAGE@VERSION(,PACKAGE@VERSION)*  Comma-separated list of packages and their versions.
  -t                     --no-transitive                                    Do not apply bumping transitively.
  -i PACKAGE(,PACKAGE)*  --ignore=PACKAGE(,PACKAGE)*                        Comma-separated list of packages which will be ignored when transitive bumping.
  -g PATH                --global=PATH                                      Bump according to latest version number in the given package database.
  -d                     --dry-run                                          Just output the dependencies that will be updated.
  -?                     --help                                             Show usage help and exit.
  -v                     --version                                          Show version info and exit.
```


## Behavior

* Bumper does not reformat your cabal file, it only replaces the version ranges without using Cabal's pretty printer (which ignores existing formatting)

* Transitive bumps are always position 3 (D) bumps. Note that your transitive packages may re-export modules or instances from their dependencies, so they may need to bumped further

* There is no support for bumping external packages. If you depend on something you are not maintaining you need to bump that version manually

* Bumper respects version ranges you have specified, if `b` depends on `a >= 0.1 && < 0.3` and you bump `a` to `0.2.3` the bound will not change, but if you bump `a` to `0.3` `b`'s dependency will change to `a == 0.3.*`

* If you depend on a specific version range (minor, major, ...) the level of the range may change after bumper. Depending on `a == 0.1.2.*` and doing a major bump of `a` will change your dependency to `a == 0.2.*`, but...

* If you have an explicit version dependency (`a == 0.1.0.1`) then a major bump of `a` will result in `a == 0.2`.


## Examples

You can find this repository with empty cabal projects at http://github.com/silkapp/bumper-example

```shell
$ git clone http://www.github.com/silkapp/bumper-example.git
```

It contains four empty cabal projects, all with version `0.1`:

* `b` depends on `a == 0.1` (fixed version)
* `c` depends on `a == 0.1.*` and `b == 0.1.*` (major dependencies)
* `d` depends on `a` (any version) and `b >= 0.1 && < 0.2` (major dependency)

Say we make a minor change to `a` and we've checked that all packages build together, so we run

```shell
$ bumper --minor a # or bumper -2 a
$ git diff

a/a.cabal
-version:             0.1
+version:             0.1.1

b/b.cabal
-version:             0.1
+version:             0.1.0.1
-                     , a == 0.1
+                     , a == 0.1.1
```

`b`'s version and its dependency on `a` was bumped as well. `c` and `d` are still compatible with `a` so they were not modified.

It works in the same fashion no matter what degree of bumping you do.

```shell
$ git reset --hard
$ bumper --major a
$ git diff

a/a.cabal
-version:             0.1
+version:             0.2

b/b.cabal
-version:             0.1
+version:             0.1.0.1
-                     , a == 0.1
+                     , a == 0.2

c/c.cabal
-version:             0.1
+version:             0.1.1
-                     , a == 0.1.*
+                     , a == 0.2.*
```

Bumper combines all changes you do so versions aren't bumped several times, below `b` gets one major bump instead of a major and a minor.

```shell
$ git reset --hard
$ bumper --major a,b
$ git diff

a/a.cabal
-version:             0.1
+version:             0.2

b/b.cabal
-version:             0.1
+version:             0.2

c/c.cabal
-version:             0.1
+version:             0.1.0.1
-                     , a == 0.1.*
+                     , a == 0.2.*
-                     , b == 0.1.*
+                     , b == 0.2.*

d/d.cabal
-version:             0.1
+version:             0.1.0.1
-                     , b >= 0.1 && < 0.2
+                     , b == 0.2
```
