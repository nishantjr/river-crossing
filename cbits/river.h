#pragma once

#include <wayland-client-core.h>
#include <river-window-management-v1-client.h>
#include <river-xkb-bindings-v1-client.h>

bool await_registry(struct wl_display* display);

enum wxyz_event_type {
    WM_UNAVAILABLE,
    WM_FINISHED,
    WM_MANAGE_START,
    WM_RENDER_START,
    WM_SESSION_LOCKED,
    WM_SESSION_UNLOCKED,
    WM_WINDOW,
    WM_OUTPUT,
    WM_SEAT,
};

struct wm_unavailable      { };
struct wm_finished         { };
struct wm_manage_start     { };
struct wm_render_start     { };
struct wm_session_locked   { };
struct wm_session_unlocked { };
struct wm_window           { struct river_window_v1 *window; };
struct wm_output           { struct river_output_v1 *output; };
struct wm_seat             { struct river_seat_v1   *seat; };

struct wxyz_event {
    struct wl_list link;
    enum wxyz_event_type type;
    union {
        struct wm_unavailable       wm_unavailable;
        struct wm_finished          wm_finished;
        struct wm_manage_start      wm_manage_start;
        struct wm_render_start      wm_render_start;
        struct wm_session_locked    wm_session_locked;
        struct wm_session_unlocked  wm_session_unlocked;
        struct wm_window            wm_window;
        struct wm_output            wm_output;
        struct wm_seat              wm_seat;
    };
};

struct wxyz_event* wxyz_next_event(struct wl_display* display);

void init_event_queue();
struct river_window_manager_v1*  get_river_window_manager();
void river_wm_add_event_listeners(struct river_window_manager_v1* window_manager_v1);
