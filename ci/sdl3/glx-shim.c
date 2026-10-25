/* Lets Skiko's Skia, which is built for GLX, work on the EGL context SDL makes on Wayland (#2380).
 *
 * Skia's GLX native interface gives up when glXGetCurrentContext() reports no context, and under EGL it never has one.
 * This answers non-null. It is loaded RTLD_GLOBAL before Skiko's library (com.serenity.ui.sdl.GlxShim), so Skiko's call
 * lands here instead of libGL's. The value is never dereferenced by Skia; it only has to be non-null.
 */
void *glXGetCurrentContext(void) { return (void *) 1; }
