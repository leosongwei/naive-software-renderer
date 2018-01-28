#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define STB_IMAGE_IMPLEMENTATION
#include "stb_image.h"

#define STB_IMAGE_RESIZE_IMPLEMENTATION
#include "stb_image_resize.h"

#define STB_IMAGE_WRITE_IMPLEMENTATION
#include "stb_image_write.h"

unsigned char* read_image(const char* filepath, int* x, int* y)
{
	int n;
	int desired_channels = 3; // RGB
	unsigned char* data = stbi_load(filepath, x, y, &n, desired_channels);

	/*
	unsigned char* resized_data = (unsigned char*)malloc(128 * 128 * 3);
	stbir_resize_uint8(data, x, y, 0,
			resized_data, 128, 128, 0,
			desired_channels);
	stbi_image_free(data);
	*/

	return data;
}

void free_img(void* data)
{
	stbi_image_free(data);
}

void write_img(void* data)
{
	stbi_write_png("testoutput.png", 128, 128, 3, data, 0);
}

/*
int main()
{
	unsigned char* img_ptr = read_image("./Lenna.png");
	free(img_ptr);
	return 0;
}
*/
