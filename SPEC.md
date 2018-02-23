pandoc-sync.yaml spec
=====================

The high-level model is that *pandoc-sync* keeps track of a canonical version
of the file system/directory structure, and each file.

This canonical representation is then realized as concrete files in the file
system, in each different format (called "branches").  *pandoc-sync* tracks
changes in these, and propagates changes throughout all versions.

Basic configuration
-------------------

For example, with configuration:

```yaml
branches:
- format: markdown
  root: md
- format: docx
  root: doc
- format: rtf
  root: doc
- format: html
  root: web
```

Would define four different branches ("realizations") of the canonical files.

If the canonical file system looks like:

```
.
+- foo      (file)
+- bar      (file)
+- more/    (folder)
   +- baz   (file)
```

This would be realized as:

```
.
+- md/
|  +- foo.md
|  +- bar.md
|  +- more/
|     +- baz.md
+- doc/
|  +- foo.docx
|  +- foo.rtf
|  +- bar.docx
|  +- bar.rtf
|  +- more/
|     +- baz.docx
|     +- baz.rtf
+- web/
   +- foo.html
   +- bar.html
   +- more/
      +- baz.html
```

Note that branches can overlap, but only if they have different file
extensions, because discovery is driven by extensions.

Options
-------

### Global

Some options can be specified either in the top-level `options` key, and also
in the `options` key per-branch:

*   `always-backup`: `true` or `false` --- *always* back-up when things are
    being over-written, even if there is no conflict.

    Default: `false`
*   `pandoc`: Settings for various pandoc reader and writer options
*   `variables`: pandoc's key-value store used for templates

### Branch only

For each branch, in an `options` key, one may also specify:

*   `mode`: can be one of:

    *   `normal`: read for new changes, and over-write (or merge changes) if
        changes occur.  Could potentially reset formatting.

    *   `write`: write-only (do not look for changes or incorporate changes
        into sync).  Always overwrite when changes are found in sources.

    *   `read`: read-only, never write (or over-write).  Essentially sets the
        "canonical", top-priority sync.  Can be set for multiple branches, but
        only really behaves well if the same canonical file only ever appears
        in one actual branch.  Otherwise, every change would be a conflict.

        This is different than just setting `priority` to be a large positive
        number, because this will *never* write, even if there are no
        conflicts.  High priority `normal` mode can still be overwritten.

    *   `auto`: Leave as the default option for the given format.

    Default: `normal` for all bidirectional formats (like *markdown*, *html*).
    `write` for all write-only formats (*pdf*).  `read` for all read-only
    formats.

*   `priority`: An optional integer representing what branch to prioritize if
    more than one change is found, or when resolving conflicts. Higher numbers
    indicate higher priority.  If two conflicts are found with the same
    priority, the *most recently changed* version is preferred.

    Only is considered for `normal` and `write` modes.

    Default is 5.  Positive integers are recommended, but negative integers are
    allowed for convenience.

    Note that for all conflicts involving readable modes (anything but `write`)
    where multiple files are changed before a sync occurs, backups are always
    stored.
*   `reference`: used for formats able to use reference files.  Can be one of:
    *   `self`: the file is its own reference file.  Tries to preserve style
        whenever possible.
    *   `default`: Use the default reference file
    *   `file`: Use the file given in the `reference-file` field.  Is
        implied/not necessary if `reference-file` is provided.
*   `reference-file`: path to reference file, for formats able to use reference
    files.  If provided, overrides `reference` field.

Explicit Files
--------------

One can also specify, at the top level, the individual files one wishes to
sync.

If used without `branches`, this disables discovery and allows one to
specifically state only the files they wish to sync.

If used with `branches`, this allows one to set per-file options:

```yaml
files:
  foo:
    options:                # for file 'foo' all branches
      always-backup: true
    branches:
      - format: markdown
        root: md
        options:            $ for file 'foo' in 'markdown' branch ('foo.md')
          priority: 10
```

Discovery
---------

New files are discovered under the given format's default extension.  They are
then realized in each branch by keeping the same basename and changing the
extension to the format's default extension.
