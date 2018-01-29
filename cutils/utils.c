#include <SDL2/SDL.h>
#include <SDL2/SDL_surface.h>
#include <unistd.h>

void* SDL_Surface_pixels (SDL_Surface* surface)
{
	return surface->pixels;
}

int get_cpu_count()
{
	return sysconf(_SC_NPROCESSORS_CONF);
}
