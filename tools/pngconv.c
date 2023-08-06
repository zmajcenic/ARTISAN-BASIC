#include <stdio.h>
#include <stdlib.h>
#include <getopt.h>
#include <string.h>
#include <libpng16/png.h>

static int binary=0;

int width, height;
png_byte color_type;
png_byte bit_depth;
png_bytep *row_pointers = NULL;

int mr,mb,mg,zr,zg,zb;
unsigned char *mask;
unsigned char *data;

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

int main (int argc, char **argv)
{
  int c;
  char *input_file=NULL;
  char *output_file=NULL;
  char *mask_color_str=NULL;
  char *zerobit_color_str=NULL;

  while (1)
    {
      static struct option long_options[] =
        {
          {"binary", no_argument, &binary, 1},
          /* These options don’t set a flag.
             We distinguish them by their indices. */
          {"input", required_argument, 0, 'i'},
          {"output", required_argument, 0, 'o'},
          {"mask_color", required_argument, 0, 'm'},
          {"zerobit_color", required_argument, 0, 'z'},
          {0, 0, 0, 0}
        };
      /* getopt_long stores the option index here. */
      int option_index = 0;

      c = getopt_long (argc, argv, "i:o:m:z:",
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

        case '?':
          /* getopt_long already printed an error message. */
          break;

        default:
          exit (1);
        }
    }

	if ((input_file == NULL) || (output_file == NULL) || (mask_color_str == NULL) || (zerobit_color_str == NULL))
	{
		printf ("Usage:\n");
		printf ("-i | --input <filename> specify input .png file\n");
		printf ("-o | --output <filename> specify output .bin file\n");
		printf ("-m | --mask_color <hex RGB> color defining AND mask\n");
    printf ("-z | --zerobit_color <hex RGB> color defining zero valued bit in data\n");
    printf ("--binary write .BIN header [optional]\n");
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

  read_png_file(input_file);
  printf ("Read %dx%d picture\n",width,height);
  if ((width % 8 != 0) || (height % 8 != 0))  {
    printf("Picture dimensions must be multiplier of 8\n");
    exit(1);
  }
  printf ("Mask color [%02X,%02X,%02X], zero bit color [%02X,%02X,%02X]\n",mr,mg,mb,zr,zg,zb);

  unsigned char *mask = calloc(width*height/8,1);
  unsigned char *data = calloc(width*height/8,1);
  if ((!mask) || (!data)) {
    printf("Error allocating memory\n");
    exit(1);
  }

  extract_data_and_mask(data,mask);

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
    h[3]=size>>8;
    h[4]=size&0xff;
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

  /*
  int x,y;
  unsigned char a;
  unsigned char *p;

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
  printf("\n");
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
  */

  exit (0);
}