/* Throwaway diagnostic (#2380, never merge): makes Skia's GLX native-interface check see "a current context". */
void *glXGetCurrentContext(void) { return (void *)1; }
