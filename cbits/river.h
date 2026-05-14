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

    WINDOW_CLOSED,
    WINDOW_DIMENSIONS_HINT,
    WINDOW_DIMENSIONS,
    WINDOW_APP_ID,
    WINDOW_TITLE,
    WINDOW_PARENT,
    WINDOW_DECORATION_HINT,
    WINDOW_POINTER_MOVE_REQUESTED,
    WINDOW_POINTER_RESIZE_REQUESTED,
    WINDOW_SHOW_WINDOW_MENU_REQUESTED,
    WINDOW_MAXIMIZE_REQUESTED,
    WINDOW_UNMAXIMIZE_REQUESTED,
    WINDOW_FULLSCREEN_REQUESTED,
    WINDOW_EXIT_FULLSCREEN_REQUESTED,
    WINDOW_MINIMIZE_REQUESTED,
    WINDOW_UNRELIABLE_PID,
    WINDOW_PRESENTATION_HINT,
    WINDOW_IDENTIFIER,

    OUTPUT_REMOVED,
    OUTPUT_WL_OUTPUT,
    OUTPUT_POSITION,
    OUTPUT_DIMENSIONS,

    SEAT_REMOVED,
};

struct wm_unavailable      { struct river_window_manager_v1* river_wm; };
struct wm_finished         { struct river_window_manager_v1* river_wm; };
struct wm_manage_start     { struct river_window_manager_v1* river_wm; };
struct wm_render_start     { struct river_window_manager_v1* river_wm; };
struct wm_session_locked   { struct river_window_manager_v1* river_wm; };
struct wm_session_unlocked { struct river_window_manager_v1* river_wm; };
struct wm_window           { struct river_window_manager_v1* river_wm; struct river_window_v1 *window; };
struct wm_output           { struct river_window_manager_v1* river_wm; struct river_output_v1 *output; };
struct wm_seat             { struct river_window_manager_v1* river_wm; struct river_seat_v1   *seat; };

struct window_closed                        { struct river_window_v1* window; };
struct window_dimensions_hint               { struct river_window_v1* window; };
struct window_dimensions                    { struct river_window_v1* window; };
struct window_app_id                        { struct river_window_v1* window; };
struct window_title                         { struct river_window_v1* window; };
struct window_parent                        { struct river_window_v1* window; };
struct window_decoration_hint               { struct river_window_v1* window; };
struct window_pointer_move_requested        { struct river_window_v1* window; };
struct window_pointer_resize_requested      { struct river_window_v1* window; };
struct window_show_window_menu_requested    { struct river_window_v1* window; };
struct window_maximize_requested            { struct river_window_v1* window; };
struct window_unmaximize_requested          { struct river_window_v1* window; };
struct window_fullscreen_requested          { struct river_window_v1* window; };
struct window_exit_fullscreen_requested     { struct river_window_v1* window; };
struct window_minimize_requested            { struct river_window_v1* window; };
struct window_unreliable_pid                { struct river_window_v1* window; };
struct window_presentation_hint             { struct river_window_v1* window; };
struct window_identifier                    { struct river_window_v1* window; };

struct output_removed                       { struct river_output_v1* output; };
struct output_wl_output                     { struct river_output_v1* output; uint32_t name; };
struct output_position                      { struct river_output_v1* output; int32_t x; int32_t y; };
struct output_dimensions                    { struct river_output_v1* output; int32_t width; int32_t height; };

struct seat_removed                         { struct river_seat_v1* seat; };

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

        struct window_closed                     window_closed;
        struct window_dimensions_hint            window_dimensions_hint;
        struct window_dimensions                 window_dimensions;
        struct window_app_id                     window_app_id;
        struct window_title                      window_title;
        struct window_parent                     window_parent;
        struct window_decoration_hint            window_decoration_hint;
        struct window_pointer_move_requested     window_pointer_move_requested;
        struct window_pointer_resize_requested   window_pointer_resize_requested;
        struct window_show_window_menu_requested window_show_window_menu_requested;
        struct window_maximize_requested         window_maximize_requested;
        struct window_unmaximize_requested       window_unmaximize_requested;
        struct window_fullscreen_requested       window_fullscreen_requested;
        struct window_exit_fullscreen_requested  window_exit_fullscreen_requested;
        struct window_minimize_requested         window_minimize_requested;
        struct window_unreliable_pid             window_unreliable_pid;
        struct window_presentation_hint          window_presentation_hint;
        struct window_identifier                 window_identifier;

        struct output_removed       output_removed;
        struct output_wl_output     output_wl_output;
        struct output_position      output_position;
        struct output_dimensions    output_dimensions;

        struct seat_removed         seat_removed;
    };
};

struct wxyz_event* wxyz_next_event(struct wl_display* display);

void init_event_queue();
struct river_window_manager_v1*  get_river_window_manager();
void river_wm_add_event_listeners(struct river_window_manager_v1* window_manager_v1);
