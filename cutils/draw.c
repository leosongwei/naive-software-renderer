#include <stdlib.h>
#include <stdint.h>

#define DOT_OFFSET(x, y, w) (y * w + x)

int dot_offset(int x, int y, int width)
{
	return y*width + x;
}

void c_draw_line
(int x0, int y0,
 int x1, int y1,
 uint32_t rgb,
 void* pixels, int width)
{
	int steep;
	if(abs(y1 - y0) > abs(x1 - x0))
		steep = 1;
	else
		steep = 0;

	if(steep){
		unsigned int c;
	        c = x0; x0 = y0; y0 = c;
		c = x1; x1 = y1; y1 = c;
	}
	if(x0 > x1){
		unsigned int c;
	        c = x0; x0 = x1; x1 = c;
		c = y0; y0 = y1; y1 = c;
	}

	int dx = x1 - x0;
	int dy = abs(y1 - y0);
	float err = (float)dx / 2;
	int ystep = (y0 < y1) ? 1 : -1;
	int y = y0;

	uint32_t* pixels_p = (uint32_t*)pixels;

	for(int x = x0; x < x1; x++){
		// int offset = steep ? dot_offset(y, x, width) : dot_offset(x, y, width);
		int offset = steep ? DOT_OFFSET(y, x, width) : DOT_OFFSET(x, y, width);
		pixels_p[offset] = rgb;
		err = err - dy;
		if(err < 0){
			y = y + ystep;
			err = err + dx;
		}
	}
}


