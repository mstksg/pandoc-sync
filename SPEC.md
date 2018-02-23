pandoc-sync.yaml spec
=====================

The high-level model is that *pandoc-sync* keeps track of a canonical version
of the file system/directory structure, and each file.

This canonical representation is then realized as concrete files in the file
system, in each different format.  *pandoc-sync* tracks changes in these, and
propagates changes throughout all versions.

Basic configuration
-------------------

For example, with roots:

```yaml
tree:
- format: markdown
  root: md
- format: docx
  root: doc
- format: rtf
  root: doc
- format: html
  root: web
```

Would define four different "realizations" of the canonical files.

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

Note that realizations can overlap, but only if they have different file
extensions, because discovery is driven by extensions.


