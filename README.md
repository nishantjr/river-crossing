River Crossing, or Crossing for short, is a Window Manager for the River Wayland
compositor inspired by XMonad. We do not aim for XMonad configurations to run on
Crossing---the protocols and architecture are just too different. For example,
XMonad's StackSet was designed with a single seat in mind, while Wayland allows
multiple seats. Instead we implement a capatability layer to allow us to bring
in some of the community goodies, such as layouts.

Immediate Goals
===============

Our first goal is to get something we can use as a daily driver:

- [x] Support River's Window Management interface basic window management
- [x] Support River's XKB Binding for keybindings (only keyPress right now)
- [x] Compatability for XMonad Layouts--atleast the ones that are mostly pure.
      We choose *not* to bring in workspaces because it seems like a fairly
      opinionated method of organizing windows.
      For example, tags, multiple of which may be selected, are an alternative;
      XMonad forces floating windows to be per-output rather than per-workspace...
      They will instead be implemented as a Layout making them configurable.

- [x] Remove dependency on focused window from Layouts (i.e. move from StackSet
      to plain list of Windows.
- [x] Layout combinator for Full
- [ ] Layout combinator for Workspaces
- [x] Layout combinator for Floating
- [ ] Port pure portions of BSP Layout
- [ ] Inform River re the stacking order

- [ ] Allow layouts to garbage collect destroyed windows.

- [ ] Mouse bindings

- [ ] Automatically generate Wayland bindings.
- [ ] Clean up abstractions

- [ ] Lifetime management of WM objects
      (send destroy messages to server; send bookkeeping messages to layouts etc)

- [ ] `sendMessage` and multi-seat? Layouts such as Workspaces and XMonad's
      Magnifier need to be aware of the focused window in order to forward
      messages to the correct workspace and to magnify the correct windows.

      The SeatHandle may not just be another member of the message since
      then the Workspaces layout would need to be aware of which messages use
      the Seat.

      At the same time other messages may be triggered outside the context of
      seats: e.g. the `Hide` messasge during the doLayout function of `(|||)`.


Design Choices
==============

### Why are layouts different from XMonad?

Layouts in XMonad rely on the focused window as part of the layout.
In Wayland multiple windows may be focused. This is not representable with
the StackSet datastructure. We'd need to figure out some multi-holed zipper
to make it work. Instead, we plan on using a simple list to represent the windows
and Layouts that need to know which windows are focused can do so through the
non-pure interface.

### Why split Workspace/Floating out of the core interface?

- Allows that part of the core API to evolve.
- Does not lock us in to a particular paradigm for workspaces.


Possible Directions
===================

These are some ideas that could be considered to improve our interface.
Some of these are half-baked--the pupose of this list is partly to move things
out of my mind so that I stop ruminating about them.


Layout Contexts
---------------

Since Layouts may be nested in others, outer layouts may provide context to
inner ones. For example, the workspace or output name for that layout on
which the layout is in. I'm not sure if this will make things more
complicated than necessary though.


--------------------------- ----------------------------------------------------

Layout                      Base monadic layout class. This only provides a
                            list of windows to be arranged. It does not even
                            provide a rectangle in which to do the layout.

                            This would allow us to decide how different outputs
                            are arranged---is there a single unified co-ordinate
                            system, or can windows on one output extend into
                            another?

LayoutRectangle             Provides a rectangle in which to render the windows.
                            This is close to the pure interface for XMonad.

OutputContext               Provides the name of the output the layout is
                            being rendered in, and various properties
                            of the output.

WorkspaceContext            Provides a list of workspaces available, the windows
                            in each of them, and a current workspace.
                            We split this from the OutputContext since we may
                            have multiple Workspaces displayed on a single Output
                            e.g. a tall monitor may be split into two.

MasterContext               Provides a "master" window that the layout may treat
                            specially. E.g. containers may have this follow
                            the focused window for a particular seat etc,
                            allowing us to emulate XMonad layouts such as Full
                            and Zoom.

SeatContext                 The Seat that triggered a particular message to
                            be sent.

LayerContext                Provides the "layer" in which the layout is being
                            executed. This may allow different layouts for
                            background, top-level, ... layers.
                            This cannot be supported on River yet.



--------------------------------------------------------------------------------


Type families for stronger typed messages
----------------------------------------

This could allow us to use `sendMessage` only to layouts that accept them.


Other
-----

* Should `Layout`'s `doMessage` use MaybeT? Are `SomeMessage` and `SomeLayout`
  idiomatic in modern Haskell?
