#include "river.h"

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


// Event Queue
// ===========

// Callback back based bindings don't make much sense in Haskell.
// A more natural representation is a queue of events.
// This would also simplify testing.

// If we had a Haskell scanner for wayland protocols, this would be straight
// forward to do. Instead, we shoe-horn this on top of the C callback based
// code.

static struct wl_list event_queue = {0};

void init_event_queue() {
    wl_list_init(&event_queue);
}

// Allocates and adds a new event in the queue; initialized to null.
struct wxyz_event* wxyz_new_event() {
    struct wxyz_event *ev = calloc(1, sizeof(*ev));
    wl_list_insert(&event_queue, &ev->link); // insert at front
    return ev;
}
// Returns event from the head of the queue. Must be freed.
struct wxyz_event* wxyz_next_event(struct wl_display* display)
{
    while (true) {
        if (!wl_list_empty(&event_queue)) {
            struct wl_list* last = event_queue.prev;
            wl_list_remove(last);
            struct wxyz_event* ret = wl_container_of(last, ret, link);
            return ret;
        }

        if (wl_display_dispatch(display) < 0) {
            fprintf(stderr, "dispatch failed\n");
            return NULL;
        }
    }
    return NULL;
}

// Output Callbacks
// ----------------

static void output_handle_removed(void *data, struct river_output_v1 *output)
{
    struct wxyz_event* event = wxyz_new_event();
    event->type = OUTPUT_REMOVED;
    event->output_removed.output = output;
}
static void output_handle_wl_output(void *data, struct river_output_v1 *output, uint32_t name)
{
    struct wxyz_event* event = wxyz_new_event();
    event->type = OUTPUT_WL_OUTPUT;
    event->output_wl_output.output = output;
}
static void output_handle_position(void *data, struct river_output_v1 *output, int32_t x, int32_t y) {
    struct wxyz_event* event = wxyz_new_event();
    event->type = OUTPUT_POSITION;
    event->output_position.output = output;
    event->output_position.x = x;
    event->output_position.y = y;
}
static void output_handle_dimensions(void *data, struct river_output_v1 *output, int32_t width, int32_t height) {
    struct wxyz_event* event = wxyz_new_event();
    event->type = OUTPUT_DIMENSIONS;
    event->output_dimensions.output = output;
    event->output_dimensions.width = width;
    event->output_dimensions.height = height;
}

const struct river_output_v1_listener river_output_listener = {
    .removed = output_handle_removed,
    .wl_output = output_handle_wl_output,
    .position = output_handle_position,
    .dimensions = output_handle_dimensions,
};


// Window Callbacks
// ----------------

static void window_handle_closed(void *data, struct river_window_v1 *window)
{
    struct wxyz_event* event = wxyz_new_event();
    event->type = WINDOW_CLOSED;
    event->window_closed.window = window;
}

static void window_handle_dimensions( void *data, struct river_window_v1 *window, int32_t width, int32_t height)
{
    struct wxyz_event* event = wxyz_new_event();
    event->type = WINDOW_DIMENSIONS;
    event->window_dimensions.window = window;
}

static void window_handle_pointer_move_requested( void *data, struct river_window_v1 *window, struct river_seat_v1 *river_seat)
{
    struct wxyz_event* event = wxyz_new_event();
    event->type = WINDOW_POINTER_MOVE_REQUESTED;
    event->window_pointer_move_requested.window = window;
}

static void window_handle_pointer_resize_requested( void *data, struct river_window_v1 *window, struct river_seat_v1 *river_seat, uint32_t edges)
{
    struct wxyz_event* event = wxyz_new_event();
    event->type = WINDOW_POINTER_RESIZE_REQUESTED;
    event->window_pointer_resize_requested.window = window;
}

static void window_handle_dimensions_hint(void *data, struct river_window_v1 *window, int32_t min_width, int32_t min_height, int32_t max_width, int32_t max_height)
{
    struct wxyz_event* event = wxyz_new_event();
    event->type = WINDOW_DIMENSIONS_HINT;
    event->window_dimensions_hint.window = window;
}

static void window_handle_app_id(void *data, struct river_window_v1 *window, const char *app_id)
{
    struct wxyz_event* event = wxyz_new_event();
    event->type = WINDOW_APP_ID;
    event->window_app_id.window = window;
}

static void window_handle_title(void *data, struct river_window_v1 *window, const char *title)
{
    struct wxyz_event* event = wxyz_new_event();
    event->type = WINDOW_TITLE;
    event->window_title.window = window;
}

static void window_handle_parent(void *data, struct river_window_v1 *window, struct river_window_v1 *parent)
{
    struct wxyz_event* event = wxyz_new_event();
    event->type = WINDOW_PARENT;
    event->window_parent.window = window;
}

static void window_handle_decoration_hint(void *data, struct river_window_v1 *window, uint32_t hint)
{
    struct wxyz_event* event = wxyz_new_event();
    event->type = WINDOW_DECORATION_HINT;
    event->window_decoration_hint.window = window;
}

static void window_handle_show_window_menu_requested(void *data, struct river_window_v1 *window, int32_t x, int32_t y)
{
    struct wxyz_event* event = wxyz_new_event();
    event->type = WINDOW_SHOW_WINDOW_MENU_REQUESTED;
    event->window_show_window_menu_requested.window = window;
}

static void window_handle_maximize_requested(void *data, struct river_window_v1 *window)
{
    struct wxyz_event* event = wxyz_new_event();
    event->type = WINDOW_MAXIMIZE_REQUESTED;
    event->window_maximize_requested.window = window;
}

static void window_handle_unmaximize_requested(void *data, struct river_window_v1 *window)
{
    struct wxyz_event* event = wxyz_new_event();
    event->type = WINDOW_UNMAXIMIZE_REQUESTED;
    event->window_unmaximize_requested.window = window;
}

static void window_handle_fullscreen_requested(void *data, struct river_window_v1 *window, struct river_output_v1 *river_output)
{
    struct wxyz_event* event = wxyz_new_event();
    event->type = WINDOW_FULLSCREEN_REQUESTED;
    event->window_fullscreen_requested.window = window;
}

static void window_handle_exit_fullscreen_requested(void *data, struct river_window_v1 *window)
{
    struct wxyz_event* event = wxyz_new_event();
    event->type = WINDOW_EXIT_FULLSCREEN_REQUESTED;
    event->window_exit_fullscreen_requested.window = window;
}

static void window_handle_minimize_requested(void *data, struct river_window_v1 *window)
{
    struct wxyz_event* event = wxyz_new_event();
    event->type = WINDOW_MINIMIZE_REQUESTED;
    event->window_minimize_requested.window = window;
}

static void window_handle_unreliable_pid(void *data, struct river_window_v1 *window, int32_t unreliable_pid)
{
    struct wxyz_event* event = wxyz_new_event();
    event->type = WINDOW_UNRELIABLE_PID;
    event->window_unreliable_pid.window = window;
}

static void window_handle_presentation_hint(void *data, struct river_window_v1 *window, uint32_t hint)
{
    struct wxyz_event* event = wxyz_new_event();
    event->type = WINDOW_PRESENTATION_HINT;
    event->window_presentation_hint.window = window;
}

static void window_handle_identifier(void *data, struct river_window_v1 *window, const char *indentifier)
{
    struct wxyz_event* event = wxyz_new_event();
    event->type = WINDOW_IDENTIFIER;
    event->window_identifier.window = window;
}

const struct river_window_v1_listener river_window_listener = {
    .closed = window_handle_closed,
    .dimensions_hint = window_handle_dimensions_hint,
    .dimensions = window_handle_dimensions,
    .app_id = window_handle_app_id,
    .title = window_handle_title,
    .parent = window_handle_parent,
    .decoration_hint = window_handle_decoration_hint,
    .pointer_move_requested = window_handle_pointer_move_requested,
    .pointer_resize_requested = window_handle_pointer_resize_requested,
    .show_window_menu_requested = window_handle_show_window_menu_requested,
    .maximize_requested = window_handle_maximize_requested,
    .unmaximize_requested = window_handle_unmaximize_requested,
    .fullscreen_requested = window_handle_fullscreen_requested,
    .exit_fullscreen_requested = window_handle_exit_fullscreen_requested,
    .minimize_requested = window_handle_minimize_requested,
    .unreliable_pid = window_handle_unreliable_pid,
    .presentation_hint = window_handle_presentation_hint,
    .identifier = window_handle_identifier,
};


// Window Manager Callbacks
// ------------------------

static void wm_handle_unavailable(void *data, struct river_window_manager_v1 *obj)
{
    struct wxyz_event* wx_event = wxyz_new_event();
    wx_event->type = WM_UNAVAILABLE;
    wx_event->wm_unavailable.river_wm = obj;
}

static void wm_handle_finished(void *data, struct river_window_manager_v1 *obj)
{
    struct wxyz_event* wx_event = wxyz_new_event();
    wx_event->type = WM_FINISHED;
    wx_event->wm_finished.river_wm = obj;
}

static void wm_handle_manage_start(void *data, struct river_window_manager_v1 *obj)
{
    struct wxyz_event* wx_event = wxyz_new_event();
    wx_event->type = WM_MANAGE_START;
    wx_event->wm_manage_start.river_wm = obj;
}

static void wm_handle_render_start(void *data, struct river_window_manager_v1 *obj)
{
    struct wxyz_event* wx_event = wxyz_new_event();
    wx_event->type = WM_RENDER_START;
    wx_event->wm_render_start.river_wm = obj;
}

static void wm_handle_session_locked(void *data, struct river_window_manager_v1 *obj)
{
    struct wxyz_event* wx_event = wxyz_new_event();
    wx_event->type = WM_SESSION_LOCKED;
    wx_event->wm_session_locked.river_wm = obj;
}

static void wm_handle_session_unlocked(void *data, struct river_window_manager_v1 *obj)
{
    struct wxyz_event* wx_event = wxyz_new_event();
    wx_event->type = WM_SESSION_UNLOCKED;
    wx_event->wm_session_unlocked.river_wm = obj;
}

static void wm_handle_window(
    void *data, struct river_window_manager_v1 *wm, struct river_window_v1 *river_window)
{
    struct wxyz_event* wx_event = wxyz_new_event();
    wx_event->type = WM_WINDOW;
    wx_event->wm_window.river_wm = wm;
    wx_event->wm_window.window = river_window;
    river_window_v1_add_listener(river_window, &river_window_listener, NULL);
}

static void wm_handle_output(
    void *data, struct river_window_manager_v1 *obj, struct river_output_v1 *river_output)
{
    struct wxyz_event* wx_event = wxyz_new_event();
    wx_event->type = WM_OUTPUT;
    wx_event->wm_output.river_wm = obj;
    wx_event->wm_output.output = river_output;
    river_output_v1_add_listener(river_output, &river_output_listener, NULL);
}

static void wm_handle_seat(
    void *data, struct river_window_manager_v1 *obj, struct river_seat_v1 *river_seat)
{
    struct wxyz_event* wx_event = wxyz_new_event();
    wx_event->type = WM_SEAT;
    wx_event->wm_seat.river_wm = obj;
    // river_seat_v1_add_listener(seat->obj, &river_seat_listener, seat);
}

static const struct river_window_manager_v1_listener wm_listener = {
    .unavailable = wm_handle_unavailable,
    .finished = wm_handle_finished,
    .manage_start = wm_handle_manage_start,
    .render_start = wm_handle_render_start,
    .session_locked = wm_handle_session_locked,
    .session_unlocked = wm_handle_session_unlocked,
    .window = wm_handle_window,
    .output = wm_handle_output,
    .seat = wm_handle_seat,
};


// Connecting to River
// ===================

// Given a connection to a Wayland display, we connect to the registry
// and obtain handles to the river protocols we need.
// Since the whole use of C callbacks is awkward, this code is a bit ugly.
// Fortunately it is only needed during set up. Hopefully we will get around
// to writing a Wayland scanner and get rid of the need for this.

struct river_window_manager_v1 *window_manager_v1 = NULL;

struct river_window_manager_v1* get_river_window_manager() { return window_manager_v1; }

void river_wm_add_event_listeners(struct river_window_manager_v1* window_manager_v1) {
    river_window_manager_v1_add_listener(window_manager_v1, &wm_listener, NULL);
}
struct river_xkb_bindings_v1 *xkb_bindings_v1 = NULL;


static void handle_registry_global(
    void *data, struct wl_registry *registry, uint32_t name, const char *interface, uint32_t version)
{
    if (strcmp(interface, river_window_manager_v1_interface.name) == 0) {
        if (version >= 4) {
            window_manager_v1 = wl_registry_bind(registry, name, &river_window_manager_v1_interface, 4);
        }
    } else if (strcmp(interface, river_xkb_bindings_v1_interface.name) == 0) {
        xkb_bindings_v1 = wl_registry_bind(registry, name, &river_xkb_bindings_v1_interface, 1);
    }
}
static void handle_registry_global_remove(void *data, struct wl_registry *registry, uint32_t name)
{}
static const struct wl_registry_listener registry_listener = {
    .global        = handle_registry_global,
    .global_remove = handle_registry_global_remove,
};

bool await_registry(struct wl_display* display) {
    struct wl_registry *registry = wl_display_get_registry(display);
    wl_registry_add_listener(registry, &registry_listener, NULL);
    if (wl_display_roundtrip(display) < 0) {
        fprintf(stderr, "roundtrip failed\n");
        return false;
    }
    return true;
}

