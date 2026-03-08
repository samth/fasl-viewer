#lang scribble/manual

@title{@exec{fasl-viewer}: Viewing and Analyzing Chez Scheme Boot Files}

An interactive TUI for inspecting Chez Scheme boot files (fasl and
vfasl formats), Racket executables with embedded boot files, and
Racket @tt{.zo} files; plus a command-line analyzer for quick
summaries.

@section{Installation}

Run @exec{raco pkg install fasl-viewer} or install from the DrRacket
package manager.

@section[#:tag "raco-fasl-viewer"]{@exec{raco fasl-viewer}}

@nested[#:style 'inset @exec{raco fasl-viewer @italic{file}}]

Opens an interactive full-screen terminal interface for exploring the
hierarchical structure of @italic{file}.

The @italic{file} argument must be one of:
@itemlist[
  @item{A Chez Scheme boot file (@tt{petite.boot}, @tt{scheme.boot})}
  @item{A Racket executable (ELF) with an embedded @tt{.rackboot} section}
  @item{A Racket @tt{.zo} file}
]

The viewer displays the file as a collapsible tree.  Boot files show
headers, fasl and vfasl entries with sizes, compression, and content
types.  Racket executables show each embedded boot file as a subtree.
Racket @tt{.zo} files show the linklet directory, bundles, phases, and
decompiled linklet code.

@subsection{Keybindings}

@tabular[#:sep @hspace[2]
         #:style 'boxed
  (list (list @bold{Key}                                       @bold{Action})
        (list @elem{@tt{j} / @tt{Down}}                       "Move cursor down")
        (list @elem{@tt{k} / @tt{Up}}                         "Move cursor up")
        (list @elem{@tt{Enter} / @tt{Space}}                  "Toggle expand/collapse")
        (list @elem{@tt{l} / @tt{Right}}                      "Expand or move into children")
        (list @elem{@tt{h} / @tt{Left}}                       "Collapse or move to parent")
        (list @tt{g}                                           "Go to top")
        (list @tt{G}                                           "Go to bottom")
        (list @elem{@tt{Ctrl-d} / @tt{Page Down}}             "Page down")
        (list @elem{@tt{Ctrl-u} / @tt{Page Up}}               "Page up")
        (list @tt{/}                                           "Start search")
        (list @tt{n}                                           "Next search match")
        (list @elem{@tt{N} / @tt{p}}                           "Previous search match")
        (list @tt{?}                                           "Show register legend")
        (list @tt{q}                                           "Quit"))]

@subsection{Auto-describe on expand}

When a fasl or vfasl entry is expanded for the first time, the
viewer automatically invokes Chez Scheme's internal
@tt{$describe-fasl-from-port} to show detailed object information
including types, sizes, and field values.

@subsection{Register annotations}

Disassembled code is annotated with Chez Scheme register roles
(e.g., @tt{ac0} for the first accumulator, @tt{sfp} for the
stack-frame pointer) to make assembly output more readable.

@subsection{Architecture-specific legend}

The register legend (@tt{?}) shows a reference table
appropriate for the architecture of the file being viewed (x86-64
or ARM64).

@section[#:tag "raco-fasl-analyze"]{@exec{raco fasl-analyze}}

@nested[#:style 'inset @exec{raco fasl-analyze @italic{file}}]

Prints a non-interactive textual analysis of @italic{file} to
standard output.  Accepts the same file types as @exec{raco
fasl-viewer}.

The output includes:

@itemlist[
  @item{@bold{File format} — whether the file is a Chez boot file,
    Racket executable, or @tt{.zo} file.}
  @item{@bold{Headers} — machine type, Chez Scheme version, and
    dependency strings for each header in a boot file.}
  @item{@bold{Object summary} — total number of objects, broken down
    by kind (fasl vs. vfasl), situation (visit, revisit,
    visit-revisit), and compression method.}
  @item{@bold{Size statistics} — total compressed and uncompressed
    sizes, with the overall compression ratio.}
  @item{@bold{Content type breakdown} — for fasl objects, a count of
    each top-level content type (closure, code, record, etc.).}
  @item{@bold{vfasl vspace totals} — for vfasl objects, the aggregate
    size and percentage of each virtual-space segment (symbol, rtd,
    closure, code, data, etc.).}
  @item{@bold{Object listing} — one line per object with its index,
    situation, compression, kind, size, and content type.}
]

For @bold{Racket executables}, the analysis is printed per embedded
boot file (petite, scheme, racket), each with its own header, summary,
and object listing.

For @bold{Racket @tt{.zo} files}, the output includes the version,
machine type, tag, content type (directory, bundle, or single
linklet), and a summary of modules and phases.
