# Index

I don't want to necessarily claim this is a [Zettelkasten][] implementation, but it might be.

[Zettelkasten]: https://en.wikipedia.org/wiki/Zettelkasten

## Installation and Usage

```sh
# Install:
gh repo clone foxfriends/index
cd index
stack install

# Use:
index ./notes-dir ./out-dir
```

The notes directory should be a flat folder (no subfolders) containing only Markdown files.

The syntax of each note is standard Markdown, with the addition of the custom link-like notation for tags: `@[text][tag]`.
Note that this syntax is just hacked in so may behave weirdly if you do something non-standard with it.

If the text and tag are (or normalize to) the same string, the tag part may be omitted (e.g. `@[text][]`).

Tag normalization is as follows:
1.  Lowercased
2.  Leading and trailing whitespace is trimmed
3.  Characters matched by the following regular expression are removed: `[^ a-zA-Z0-9_-]`
4.  Spaces (any number of them) are converted to a single hyphen.

A tag-link is translated to a link to another note by the same name as the tag. If no note exists for that tag, an empty one is created.
