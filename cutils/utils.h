void* SDL_Surface_pixels (SDL_Surface* surface);

int dot_offset(int x, int y, int width);

void c_draw_line
(int x0, int y0,
 int x1, int y1,
 uint32_t rgb,
 void* pixels, int width);

unsigned char* read_image(const char* filepath);
void free_img(void* data);
void write_img(void* data);
