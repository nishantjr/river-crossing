River Crossing, or Crossing for short, is a Window Manager for the River Wayland
compositor inspired by XMonad. We do not aim for XMonad configurations to run on
Crossing---the protocols and architecture are just too different. For example,
XMonad's StackSet was designed with a single seat in mind, while Wayland allows
multiple seats. Instead we implement a capatability layer to allow us to bring
in some of the community goodies, such as layouts.

Immediate Goals
---------------

Our first goal is to get something we can use as a daily driver:

- [x] Support River's Window Management interface basic window management
- [x] Support River's XKB Binding for keybindings (only keyPress right now)
- [x] Compatability for XMonad Layouts--atleast the ones that are mostly pure.
      We choose *not* to bring in workspaces because it seems like a fairly
      opinionated method of organizing windows.
      For example, tags, multiple of which may be selected, are an alternative;
      XMonad forces floating windows to be per-output rather than per-workspace...
      They will instead be implemented as a Layout making them configurable.

- [ ]  Mouse bindings
- [ ]  Full screen

- [ ]  Layout combinator for Workspaces
- [ ]  Layout for Floating
- [ ]  Port pure portions of BSP Layout

- [ ] Clean up Stack, Layout: The Stack and StackSet no longer make sense
      with the Wayland set: E.g. we can have multiple seats.

- [ ] Automatically generate Wayland bindings.
- [ ] Clean up abstractions
