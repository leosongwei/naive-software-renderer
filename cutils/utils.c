#include <SDL2/SDL.h>
#include <SDL2/SDL_surface.h>

void* SDL_Surface_pixels (SDL_Surface* surface)
{
	return surface->pixels;
}
