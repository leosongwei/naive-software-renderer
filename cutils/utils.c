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

void* SDL_use_surface(SDL_Surface* surface)
{
	int err = SDL_LockSurface(surface);
	if(err==0){
		return surface->pixels;
	}else{
		return NULL;
	}
}

void SDL_freeze_surface(SDL_Surface* surface)
{
	SDL_UnlockSurface(surface);
}
