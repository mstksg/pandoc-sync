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


Formats
-------

You can turn any format into a "write-only format" (that is, the file will not
be used as a source and any changes will be ignored or overwritten) by adding a
`*` after its name.

For example, `markdown*` is a write-only format, that will
not be attempted to be read by pandoc-sync.  Its changes will also be
overwritten if updates are detected.

| Format                 | Identifier      | Default for extensions  | Readable?   | Notes                                        |
| --------               | ----------      | ------------            | ----------- | -------                                      |
| Markdown (pandoc)      | markdown        | md, markdown, txt, text | Yes         |                                              |
| Markdown (strict)      | markdown strict |                         | Yes         |                                              |
| Markdown (php)         | markdown php    |                         | Yes         |                                              |
| Multimarkdown          | multimarkdown   |                         | Yes         |                                              |
| Commonmark             | commonmark      |                         | Yes         |                                              |
| Plain                  | plain           |                         | No          |                                              |
| HTML                   | html4           |                         | Yes         |                                              |
| HTML5                  | html            | assumed default         | Yes         |                                              |
| LaTeX                  | latex           | tex, ltx, latex         | Yes         |                                              |
| Beamer (PDF)           | beamer          |                         | No          |                                              |
| ConTeXt                | context         | context, ctx            | No          |                                              |
| PDF (via LaTeX)        | pdf             | pdf                     | No          |                                              |
| PDF (via ConTeXt)      | context-pdf     |                         | No          |                                              |
| PDF (via HTML5)        | html-pdf        |                         | No          |                                              |
| DocX (Microsoft Word)  | docx            | doc, docx               | Yes         | Will use current file as template, if exists |
| ODT                    | odt             | odt                     | Yes         | Will use current file as template, if exists |
| RST                    | rst             | rst                     | Yes         |                                              |
| RTF                    | rtf             | rtf                     | No          |                                              |
| S5 slideshow           | s5              |                         | No          |                                              |
| Slidy slideshow        | slidy           |                         | No          |                                              |
| Slideous slideshow     | slideous        |                         | No          |                                              |
| dzslides slideshow     | dzslides        |                         | No          |                                              |
| reaval.js slideshow    | revealjs        |                         | No          |                                              |
| Beamer slideshow (pdf) | beamer          |                         | No          |                                              |
| Beamer slideshow (tex) | beamer-latex    |                         | No          |                                              |
| asciidoc               | asciidoc        | adoc, asciidoc          | No          |                                              |
| manfile (unix)         | man             | digits 1 - 9            | No          |                                              |
| texinfo                | texinfo         | texinfo, texi           | No          |                                              |
| Haddock                | haddock         |                         | Yes         |                                              |
| Textile                | textile         | textile                 | Yes         |                                              |
| Mediawiki              | mediawiki       |                         | Yes         |                                              |
| DokuWiki               | dokuwiki        |                         | No          |                                              |
| ZimWiki                | zimwiki         |                         | No          |                                              |
| Docbook 4              | docbook         | db                      | Yes         |                                              |
| Docbook 5              | docbook5        | db5                     | Yes         |                                              |
| Fictionbook 2          | fictionbook     | fb2                     | No          |                                              |
| EPUB 2                 | epub2           | epub, epub2             | Yes         |                                              |
| EPUB 3                 | epub3           | epub3                   | Yes         |                                              |
| Org                    | org             | org                     | Yes         |                                              |
| OPML                   | opml            | opml                    | Yes         |                                              |
| ICML                   | icml            | icml                    | No          |                                              |
| TEI                    | tei             | tei, tei.xml            | No          |                                              |
| Pandoc AST (json)      | json            | json                    | Yes         |                                              |
| Pandoc AST (haskell)   | native          | native                  | Yes         |                                              |

Options
-------

Full pandoc option support is still being developed/documented.
