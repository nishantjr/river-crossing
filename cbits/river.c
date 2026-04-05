#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <wayland-client-core.h>
#include <river-window-management-v1-client.h>
#include <river-xkb-bindings-v1-client.h>

#include "river.h"

// Connecting to River
// ===================

// Given a connection to a Wayland display, we connect to the registry
// and obtain handles to the river protocols we need.

struct river_window_manager_v1 *window_manager_v1 = NULL;
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

bool await_river_protocols(struct wl_display* display) {
    struct wl_registry *registry = wl_display_get_registry(display);
    wl_registry_add_listener(registry, &registry_listener, NULL);
    if (wl_display_roundtrip(display) < 0) {
        fprintf(stderr, "roundtrip failed\n");
        return false;
    }

    if (window_manager_v1 == NULL || xkb_bindings_v1 == NULL) {
        fprintf(stderr,
                "river_window_manager_v1 or river_xkb_bindings_v1 "
                "not supported by the Wayland server\n");
        return false;
    }

    river_window_manager_v1_add_listener(window_manager_v1, NULL, NULL);
    return true;
}

