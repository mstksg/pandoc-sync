pandoc-sync
===========

Keep files of different formats all synced, using [pandoc][].

[pandoc]: https://pandoc.org

Requires a configuration file, which is by default `pandoc-sync.yaml`.  Run
`pandoc-sync gen-config` to generate a sample configuration file.

In "sync" mode, tracks which files were changed (or added) since the last time
`sync` was run, and, if any files were changed, updates them all to synchronize
their contents (in different formats).  In "watch" mode, continually watches
the directory for changes and re-runs `sync` when any are discovered.

Discover/Sync Modes
-------------------

There are two methods for "discovering" files:

### Parallel directory tree

Keeps two directories recursively synced, matching the file tree of one with
the file tree of another, except with different formats.  For configuration:

```yaml
parallel-mode: true
format-tree:
  src: md
  src-html: html
  out: pdf
formats:
  md: markdown
  html: html5
  pdf: pdf
```

This will scan the directory `src` for any files with an `.md` extension and
consider them to be `markdown` sources, the directory `src-html` for any files
with a `.html` extension and consider them to be `html5` sources, and the
directory `out` for any files with a `.pdf` extension and consider them to be
PDFs.

If I then had a directory structure:

```
.
|-- src
|   `-- foo
|       `-- file1.md
`-- src-html
    |-- foo
    |   `-- file2.html
    `-- bar
        `-- file3.html
```

And I ran `pandoc-sync sync`, the result would be:

```
.
|-- src
|   |-- foo
|   |   |-- file1.md
|   |   `-- file2.md
|   `-- bar
|       `-- file3.md
|-- src-html
|   |-- foo
|   |   |-- file1.html
|   |   `-- file2.html
|   `-- bar
|       `-- file3.html
`-- out
    |-- foo
    |   |-- file1.pdf
    |   `-- file2.pdf
    `-- bar
        `-- file3.pdf
```

### Output in same directory

Slightly simpler but less organized discovery mode.  For configuration:

```yaml
parallel-mode: false
formats:
  md: markdown
  html: html5
  pdf: pdf
```

Then with a directory structure:

```
.
|-- foo
|   |-- file1.md
|   `-- file2.html
`-- bar
    `-- file3.html
```

Running `pandoc-sync sync` will get you:

```
.
|-- foo
|   |-- file1.md
|   |-- file1.html
|   |-- file1.pdf
|   |-- file2.md
|   |-- file2.html
|   `-- file2.pdf
`-- bar
    |-- file3.md
    |-- file3.html
    `-- file3.pdf
```


Formats and Options
-------------------

Documentation still to be written, and full support for all options still be to
be added.

You can turn any format into a "write-only format" (that is, the file will not
be used as a source and any changes will be ignored or overwritten) by adding a
`*` after its name.

Some formats (like `pdf`) are already write-only.
