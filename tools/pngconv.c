#include <stdio.h>
#include <stdlib.h>
#include <getopt.h>
#include <string.h>
#include <libpng16/png.h>
#include <stdbool.h>

static int binary=0, verbose=0, check=0, msx1_color_sprites=0;

static int msx1_palette_r[15]={0,62,116,89,128,185,101,219,255,204,222,58,183,204,255};
static int msx1_palette_g[15]={0,184,208,85,118,94,219,101,137,195,208,162,102,204,255};
static int msx1_palette_b[15]={0,73,125,224,241,81,239,89,125,94,135,65,181,204,255};

int width, height;
png_byte color_type;
png_byte bit_depth;
png_bytep *row_pointers = NULL;

int mr,mb,mg,zr,zg,zb;
unsigned char *mask;
unsigned char *data;
int sprite_color_num, sprite_num;
int sr[15],sg[15],sb[15];
static unsigned char *sprite_data;
static int sprite_x_offset[256], sprite_y_offset[256];

void read_png_file(char *filename) {
  FILE *fp = fopen(filename, "rb");
  if (!fp) {
    printf("Error opening %s\n", filename);
    abort();
  }

  png_structp png = png_create_read_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
  if(!png) abort();

  png_infop info = png_create_info_struct(png);
  if(!info) abort();

  if(setjmp(png_jmpbuf(png))) abort();

  png_init_io(png, fp);

  png_read_info(png, info);

  width      = png_get_image_width(png, info);
  height     = png_get_image_height(png, info);
  color_type = png_get_color_type(png, info);
  bit_depth  = png_get_bit_depth(png, info);

  // Read any color_type into 8bit depth, RGBA format.
  // See http://www.libpng.org/pub/png/libpng-manual.txt

  if(bit_depth == 16)
    png_set_strip_16(png);

  if(color_type == PNG_COLOR_TYPE_PALETTE)
    png_set_palette_to_rgb(png);

  // PNG_COLOR_TYPE_GRAY_ALPHA is always 8 or 16bit depth.
  if(color_type == PNG_COLOR_TYPE_GRAY && bit_depth < 8)
    png_set_expand_gray_1_2_4_to_8(png);

  if(png_get_valid(png, info, PNG_INFO_tRNS))
    png_set_tRNS_to_alpha(png);

  // These color_type don't have an alpha channel then fill it with 0xff.
  if(color_type == PNG_COLOR_TYPE_RGB ||
     color_type == PNG_COLOR_TYPE_GRAY ||
     color_type == PNG_COLOR_TYPE_PALETTE)
    png_set_filler(png, 0xFF, PNG_FILLER_AFTER);

  if(color_type == PNG_COLOR_TYPE_GRAY ||
     color_type == PNG_COLOR_TYPE_GRAY_ALPHA)
    png_set_gray_to_rgb(png);

  png_read_update_info(png, info);

  if (row_pointers) abort();

  row_pointers = (png_bytep*)malloc(sizeof(png_bytep) * height);
  for(int y = 0; y < height; y++) {
    row_pointers[y] = (png_byte*)malloc(png_get_rowbytes(png,info));
  }

  png_read_image(png, row_pointers);

  fclose(fp);

  png_destroy_read_struct(&png, &info, NULL);
}

void extract_data_and_mask (unsigned char* data, unsigned char* mask) {
  int x, y, l;
  unsigned char b;
  png_bytep p;

  for (x = 0; x < width; x++) {
    //printf ("x=%d\n",x);
    b = 0x80 >> (x%8);
    //printf("b=%02x\n",b);
    for (y = 0; y < height; y++) {
      //printf ("y=%d\n",y);
      p = &(row_pointers[y][x * 4]);
      l = (y>>3)*width+(x>>3)*8+(y%8);
      //printf ("l=%d\n",l);
      // check for mask
      if ((p[0]==mr) && (p[1]==mg) && (p[2]==mb)) {
        mask[l]|=b;
      }
      // check for data
      else if ((p[0]!=zr) || (p[1]!=zg) || (p[2]!=zb)) {
        data[l]|=b;
        //printf ("%d\n",data[l]);
      }
      //printf ("x=%d,y=%d,b=%d,l=%d,mask=%d,data=%d\n",x,y,b,l,mask[l],data[l]);
      //getchar();
    }
  }
}

// locate horizontal starting location for a specified color in a WxH window, but up to width and height global vars
int locate_color (int start_x, int start_y, int w, int h, int r, int g, int b) {
  int x, y;
  png_bytep p;

  for (x = start_x; (x < start_x+w) && (x < width); x++) {
    for (y = start_y; (y < start_y+h) && (y < height); y++) {
      p = &(row_pointers[y][x * 4]);
      if ((p[0]==r) && (p[1]==g) && (p[2]==b))
        return x;
    }
  }
  return -1;
}

// grab sprite 16x16 data from a specified location
void grab_sprite (int start_x, int start_y, int r, int g, int b) {
  int x, y, i, j, l;
  png_bytep p;
  unsigned char bit;
  unsigned char *s;
  int *t;

  //printf ("grab entry: (%d,%d), sprite_num=%d\n", start_x, start_y,sprite_num);

  sprite_num++;
  if (sprite_num>=256) {
    printf ("Sprite number limit reached\n");
    exit(1);
  }

  sprite_x_offset[sprite_num-1]=start_x;
  sprite_y_offset[sprite_num-1]=start_y;
  s=sprite_data+(sprite_num-1)*32;
  for (l=0; l<32; l++)
    s[l]=0;

  i=0;
  for (x = start_x; (x < start_x+16) && (x < width); x++) {
    bit = 0x80 >> (i%8);
    j=0;
    for (y = start_y; (y < start_y+16) && (y < height); y++) {
      p = &(row_pointers[y][x * 4]);
      l = (i>>3)*16+j;
      if ((p[0]==r) && (p[1]==g) && (p[2]==b))
        s[l]|=bit;
      j++;
    }
    i++;
  }
  //printf ("grab exit: (%d,%d), sprite_num=%d\n", start_x, start_y,sprite_num);
}

//locate and grab sprites of specified colors, return number of defined sprites
//this function starts looking for sprites from row 0 in 16 row increments
int locate_and_grab_sprites1 (int r, int g, int b) {
  int x,y, num, l;

  num=sprite_num;
  for (y=0; y<height; y+=16) {
    //printf("%d\n",y);
    x=0;
    while (x<width) {
      l=locate_color(x,y,width,16,r,g,b);
      //printf("l=%d, y=%d\n",l,y);
      if (l != -1) {
        grab_sprite(l,y,r,g,b);
        //printf("l=%d, y=%d\n",l,y);
        x=l+16;
      }
      else
        break;
    }
  }
  return sprite_num-num;
}

//locate and grab sprites of specified colors, return number of defined sprites
//this function skips lines where target color is not present and then starts looking for 16x16 sprites
int locate_and_grab_sprites2 (int r, int g, int b) {
  int x, y, y0, num, l;
  png_bytep p;
  bool a;

  num=sprite_num;
  y0=0;
  while (y0 < height)
  {
    // skip empty rows
    a=false;
    for (y=y0; (y<height) && (!a); y++)
      for (x=0; (x<width) && (!a); x++)
      {
        p = &(row_pointers[y][x * 4]);
        a=(p[0]==r) && (p[1]==g) && (p[2]==b);
      }
    if (y == height) break;
    y0=--y;
  
    x=0;
    while (x<width) {
      l=locate_color(x,y,width,16,r,g,b);
      //printf("l=%d, y=%d\n",l,y);
      if (l != -1) {
        grab_sprite(l,y,r,g,b);
        //printf("l=%d, y=%d\n",l,y);
        x=l+16;
      }
      else
        break;
    }
    y0+=16;
  }
  return sprite_num-num;
}

// seeks for colors that are different from mask, zero bit and sprites and lists pixel locations
int check_colors () 
{
  int x, y, i;
  png_bytep p;

  for (x = 0; x < width; x++) {
    for (y = 0; y < height; y++) {
      p = &(row_pointers[y][x * 4]);
      if ((p[0]==mr) && (p[1]==mg) && (p[2]==mb)) continue;
      if ((p[0]==zr) && (p[1]==zg) && (p[2]==zb)) continue;
      i=0;
      while (i<sprite_color_num) {
        if ((p[0]==sr[i]) && (p[1]==sg[i]) && (p[2]==sb[i])) break;
        i++;
      }
      if ((sprite_color_num>0) && (i==sprite_color_num))
        printf("(%d,%d)\n",x,y);
    }
  }
}

int main (int argc, char **argv)
{
  int c;
  char *input_file=NULL;
  char *output_file=NULL;
  char *mask_color_str=NULL;
  char *zerobit_color_str=NULL;
  int algorithm=0, basic_start_line=10;

  sprite_color_num=0;
  sprite_num=0;
  sprite_data=malloc(256*32);
  if (!sprite_data) {
    printf ("Error allocating memory for sprite data\n");
    exit(1);
  }

  while (1)
    {
      static struct option long_options[] =
        {
          {"binary", no_argument, &binary, 1},
          {"verbose", no_argument, &verbose, 1},
          {"check", no_argument, &check, 1},
          {"msx1_color_sprites", no_argument, &msx1_color_sprites, 1},
          /* These options don’t set a flag.
             We distinguish them by their indices. */
          {"input", required_argument, 0, 'i'},
          {"output", required_argument, 0, 'o'},
          {"mask_color", required_argument, 0, 'm'},
          {"zerobit_color", required_argument, 0, 'z'},
          {"sprite_color", required_argument, 0, 's'},
          {"algorithm", required_argument, 0, 'a'},
          {"line", required_argument, 0, 'l'},
          {0, 0, 0, 0}
        };
      /* getopt_long stores the option index here. */
      int option_index = 0;

      c = getopt_long (argc, argv, "i:o:m:z:s:a:l:",
                      long_options, &option_index);

      /* Detect the end of the options. */
      if (c == -1)
        break;

      switch (c)
        {
        case 0:
          /* If this option set a flag, do nothing else now. */
          if (long_options[option_index].flag != 0)
            break;
          printf ("option %s", long_options[option_index].name);
          if (optarg)
            printf (" with arg %s", optarg);
          printf ("\n");
          break;

        case 'i':
          //printf ("option -i with value `%s'\n", optarg);
		      input_file = strdup(optarg);
          break;

        case 'o':
          //printf ("option -o with value `%s'\n", optarg);
		      output_file = strdup(optarg);
          break;

        case 'm':
          //printf ("option -e with value `%s'\n", optarg);
		      mask_color_str = strdup(optarg);
          break;

        case 'z':
          //printf ("option -z with value `%s'\n", optarg);
		      zerobit_color_str = strdup(optarg);
          break;

        case 's':
          //printf ("option -s with value `%s'\n", optarg);
          if (sprite_color_num >= 15) {
            printf ("Too many sprite colors specified - 15 maximum\n");
            exit (1);
          }
          // check if colors correctly passed as 6 char hex string
          if ((strlen(optarg) != 6) || (sscanf(optarg,"%2x%2x%2x",&sr[sprite_color_num],&sg[sprite_color_num],&sb[sprite_color_num]) <3)) {
            printf("Invalid color format for mask - %s\n", optarg);
            exit(1);
          }
		      sprite_color_num++;
          break;

        case 'a':
          if ((strlen(optarg) != 1) || (sscanf(optarg,"%d",&algorithm) < 1) || (algorithm < 1) || (algorithm > 2)) {
            printf("Invalid algorithm specified - %s\n", optarg);
            exit(1);
          }

        case 'l':
          if ((sscanf(optarg,"%d",&basic_start_line) < 1) || (basic_start_line < 1) || (basic_start_line > 65500)) {
            printf("Invalid BASIC start line specified - %s\n", optarg);
            exit(1);
          }

        case '?':
          /* getopt_long already printed an error message. */
          break;

        default:
          exit (1);
        }
    }

	if ((input_file == NULL) || (output_file == NULL) || (mask_color_str == NULL) || (zerobit_color_str == NULL) || (algorithm == 0))
	{
		printf ("Usage:\n");
		printf ("-i | --input <filename> specify input .png file\n");
		printf ("-o | --output <filename> specify output .bin file\n");
		printf ("-m | --mask_color <hex RGB> color defining AND mask\n");
    printf ("-z | --zerobit_color <hex RGB> color defining zero valued bit in data\n");
    printf ("-s | --sprite_color <hex RGB> generate sprite data for a specific color [optional][multiple]\n");
    printf ("-a | --algorithm <1|2> select sprite search algorithm, can provide different results\n");
    printf ("-l | --line <n> starting line of BASIC DATA statements [optional, default 10]\n");
    printf ("--msx1_color_sprites add MSX1 palette to sprite color list\n");
    printf ("--binary write .BIN header [optional]\n");
    printf ("--verbose print data and mask as ASCII [optional]\n");
    printf ("--check detect pixels of color other than mask, zero bit and sprites [optional]\n");
		exit (1);
	}

  // check if colors correctly passed as 6 char hex string
  if ((strlen(mask_color_str) != 6) || (sscanf(mask_color_str,"%2x%2x%2x",&mr,&mg,&mb) <3)) {
    printf("Invalid color format for mask - %s\n", mask_color_str);
    exit(1);
  }
  if ((strlen(zerobit_color_str) != 6) || (sscanf(zerobit_color_str,"%2x%2x%2x",&zr,&zg,&zb) <3)) {
    printf("Invalid color format for zero bit - %s\n", zerobit_color_str);
    exit(1);
  }

  // overwrite sprite colors if palette specified
  if (msx1_color_sprites) {
    for (int i=0; i<15; i++) {
      sr[i]=msx1_palette_r[i];
      sg[i]=msx1_palette_g[i];
      sb[i]=msx1_palette_b[i];
    }
    sprite_color_num=15;
  }

  read_png_file(input_file);
  printf ("Read %dx%d picture\n",width,height);
  if ((width % 8 != 0) || (height % 8 != 0))  {
    printf("Picture dimensions must be multiplier of 8\n");
    exit(1);
  }
  printf ("Mask color [%02X,%02X,%02X], zero bit color [%02X,%02X,%02X]\n",mr,mg,mb,zr,zg,zb);
  if (sprite_color_num > 0) {
    for (int i=0; i<sprite_color_num; i++)
    {
      printf ("Sprite color %d = [%02X,%02X,%02X]\n", i+1, sr[i], sg[i], sb[i]);
    }
  }

  unsigned char *mask = calloc(width*height/8,1);
  unsigned char *data = calloc(width*height/8,1);
  if ((!mask) || (!data)) {
    printf("Error allocating memory\n");
    exit(1);
  }

  extract_data_and_mask(data,mask);
  if (verbose) {
    int x,y;
    unsigned char a;
    unsigned char *p;

    printf("Data:\n");
    for (y=0; y<height; y++) {
      for (x=0; x<width; x++) {
        p=data+(y>>3)*width+(x>>3)*8+(y%8);
        a=0x80 >> (x%8);
        if (*p & a)
          printf ("*");
        else
          printf (".");
      }
      printf("\n");
    }
    printf("Mask:\n");
    for (y=0; y<height; y++) {
      for (x=0; x<width; x++) {
        p=mask+(y>>3)*width+(x>>3)*8+(y%8);
        a=0x80 >> (x%8);
        if (*p & a)
          printf ("*");
        else
          printf (".");
      }
      printf("\n");
    }
  }
  else
    printf ("Extracted data and mask\n");
  
  for (int i=0; i<sprite_color_num; i++) {
    int j;
    
    if (algorithm==1)
      j=locate_and_grab_sprites1(sr[i],sg[i],sb[i]);
    else
      j=locate_and_grab_sprites2(sr[i],sg[i],sb[i]);
    printf ("%d REM Sprite color %02d = [%02X,%02X,%02X] - %d sprites found\n", basic_start_line, i+1, sr[i], sg[i], sb[i], j);

    printf("%d DATA %d,%d\n",basic_start_line++,i+1,j);
    for (int k=sprite_num-j; k<sprite_num; k++) {
      printf ("%d DATA %d,%d,",basic_start_line++,sprite_x_offset[k],sprite_y_offset[k]);
      for (int l=0; l<32; l++) {
        printf ("%d",sprite_data[k*32+l]);
        if (l<31)
          printf (",");
      }
      printf ("\n");
    }
  }

  if (check) {
    printf("Searching for stray pixels....\n");
    check_colors();
  }

  FILE *fo = fopen (output_file, "wb");
  if (fo == NULL)
  {
    printf ("Error opening %s\n", output_file);
    exit (1);
  }
  if (binary) {
    unsigned char h[7];
    h[0]=0xfe;
    h[1]=0;
    h[2]=0;
    int size=width*height/4-1;
    h[3]=size&0xff;
    h[4]=size>>8;
    h[5]=0;
    h[6]=0;

    if (fwrite (&h, 7, 1, fo) != 1)
    {
      printf ("Error writing SC2 header\n");
      exit (1);
    }
  }
  if (fwrite (data, width*height/8, 1, fo) != 1)
  {
    printf ("Error writing data\n");
    exit (1);
  }  
  if (fwrite (mask, width*height/8, 1, fo) != 1)
  {
    printf ("Error writing mask\n");
    exit (1);
  }  
  
  fclose(fo);
  printf ("Wrote output file %s\n", output_file);

  exit (0);
}