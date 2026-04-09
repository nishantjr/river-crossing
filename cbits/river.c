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

// Window Manager Callbacks
// ------------------------

static void wm_handle_unavailable(void *data, struct river_window_manager_v1 *obj)
{
    struct wxyz_event* wx_event = wxyz_new_event();
    wx_event->type = WM_UNAVAILABLE;
}

static void wm_handle_finished(void *data, struct river_window_manager_v1 *obj)
{
    struct wxyz_event* wx_event = wxyz_new_event();
    wx_event->type = WM_FINISHED;
}

static void wm_handle_manage_start(void *data, struct river_window_manager_v1 *obj)
{
    struct wxyz_event* wx_event = wxyz_new_event();
    wx_event->type = WM_MANAGE_START;
}

static void wm_handle_render_start(void *data, struct river_window_manager_v1 *obj)
{
    struct wxyz_event* wx_event = wxyz_new_event();
    wx_event->type = WM_RENDER_START;
}

static void wm_handle_session_locked(void *data, struct river_window_manager_v1 *obj)
{
    struct wxyz_event* wx_event = wxyz_new_event();
    wx_event->type = WM_SESSION_LOCKED;
}

static void wm_handle_session_unlocked(void *data, struct river_window_manager_v1 *obj)
{
    struct wxyz_event* wx_event = wxyz_new_event();
    wx_event->type = WM_SESSION_UNLOCKED;
}

static void wm_handle_window(
    void *data, struct river_window_manager_v1 *obj, struct river_window_v1 *river_window)
{
    struct wxyz_event* wx_event = wxyz_new_event();
    wx_event->type = WM_WINDOW;
    // river_window_v1_add_listener(window->obj, &river_window_listener, window);
}

static void wm_handle_output(
    void *data, struct river_window_manager_v1 *obj, struct river_output_v1 *river_output)
{
    struct wxyz_event* wx_event = wxyz_new_event();
    wx_event->type = WM_OUTPUT;
    // river_output_v1_add_listener(output->obj, &river_output_listener, output);
}

static void wm_handle_seat(
    void *data, struct river_window_manager_v1 *obj, struct river_seat_v1 *river_seat)
{
    struct wxyz_event* wx_event = wxyz_new_event();
    wx_event->type = WM_SEAT;
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

