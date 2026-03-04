#lang scribble/manual

@title{@exec{fasl-viewer}: Viewing Chez Scheme Boot Files}

An interactive TUI for inspecting Chez Scheme boot files (fasl and
vfasl formats), Racket executables with embedded boot files, and
Racket @tt{.zo} files.

@section{Installation}

Run @exec{raco pkg install fasl-viewer} or install from the DrRacket
package manager.

@section{Usage}

@nested[#:style 'inset @exec{racket -l fasl-viewer -- @italic{file}}]

where @italic{file} is one of:
@itemlist[
  @item{A Chez Scheme boot file (@tt{petite.boot}, @tt{scheme.boot})}
  @item{A Racket executable with an embedded @tt{.rackboot} section}
  @item{A Racket @tt{.zo} file}
]

The viewer opens a full-screen terminal interface showing the
hierarchical structure of the file. Boot files are displayed as a
tree of fasl or vfasl entries, each expandable to show object
details, disassembled code, and register annotations.

@section{Keybindings}

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
        (list @tt{?}                                           "Show keybinding legend")
        (list @tt{q}                                           "Quit"))]

@section{Features}

@subsection{Auto-describe on expand}

When a fasl or vfasl entry is expanded for the first time, the
viewer automatically invokes Chez Scheme's internal
@tt{$describe-fasl-from-port} to show detailed object information
including types, sizes, and field values.

@subsection{Register annotations}

Disassembled code is annotated with Chez Scheme register roles
(e.g., @tt{%ac0} for the first accumulator, @tt{%sfp} for the
stack-frame pointer) to make assembly output more readable.

@subsection{Split-color assembly}

Assembly lines use distinct colors for labels, opcodes, operands,
and comments, making it easier to scan disassembly output.

@subsection{Architecture-specific legend}

The keybinding legend (@tt{?}) includes a register reference table
appropriate for the architecture of the file being viewed (x86-64
or ARM64).
