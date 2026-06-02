River Crossing, or Crossing for short, is a Window Manager for the River Wayland
compositor inspired by XMonad. We do not aim for XMonad configurations to run on
Crossing---the protocols and architecture are just too different. For example,
XMonad's StackSet was designed with a single seat in mind, while Wayland allows
multiple seats. Instead we implement a capatability layer to allow us to bring
in some of the community goodies, such as layouts.

Immediate Goals
---------------

- [x] Support River's Window Management interface basic window management
- [x] Support River's XKB Binding for keybindings (only keyPress right now)
- [ ] Compatability for XMonad Layouts--atleast the ones that are mostly pure.
      We choose *not* to bring in workspaces because it seems like a fairly
      opinionated method of organizing windows.
      For example, tags may be considered an alternative.
- [ ] Implement XMonad Layout for Workspaces.
- [ ] Clean up configuration interface.
- [ ] Automatically generate Wayland bindings.
- [ ] Clean up abstractions
