// ******************************************************************
// Compiler: FPC 3.1.1, CodeTyphon 540
// Author: Dinko Miljak,
// Original Template: MuPDF lib header port to FPC (c) 2013 by Blestan Tabakov
// Date: 2016-01-21
// ******************************************************************
unit libmupdf18;

{$mode objfpc}{$H+}

{$IFDEF FPC}
  {$PackEnum 4}    // use 4-byte enums
  {$PACKRECORDS C} // C/C++-compatible record packing
{$ELSE}
  {$MINENUMSIZE 4} // use 4-byte enums
{$ENDIF}

// {$A1}    // use 4-byte enums

interface

uses
  Classes, SysUtils,ctypes;


const
{$IF Defined(MSWINDOWS)}
  {$IFDEF WIN32}
  muLibName = 'libmupdf18-32.dll';
  {$ELSE}
  muLibName = 'libmupdf18-64.dll';
  {$ENDIF}
{$ELSEIF Defined(DARWIN)}
  muLibName = 'libmupdf.dylib';
  {$LINKLIB mylib}
{$ELSEIF Defined(UNIX)}
  muLibName = 'libmupdf.so';
  // muLibName = 'fitz';
  {uncomment the following lines to link statically instead of to a shared library}
  //{$LINKLIB /path-to/mupdf-1.2-source/build/debug/libfitz.a}
  //{$LINKLIB /path-to/mupdf-1.2-source/build/debug/libfreetype.a}
  //{$LINKLIB /path-to/mupdf-1.2-source/build/debug/libopenjpeg.a}
  //{$LINKLIB /path-to/mupdf-1.2-source/build/debug/libjbig2dec.a}
  //{$LINKLIB /path-to/mupdf-1.2-source/build/debug/libjpeg.a}
{$IFEND}

FZ_VERSION: String = '1.8';


type

    pfz_bbox = ^ fz_bbox;
    fz_bbox = record
               x0,y0,x1,y1: cint
              end;

    pfz_rectangle=^fz_rect;
    fz_rect= record
                   x0,y0,x1,y1: cfloat
                  end;

    fz_matrix = record
                 a,b,c,d,e,f: cfloat
                end;

    fz_irect = record
	    x0, y0: cint;
	    x1, y1: cint;
    end;

    pfz_document_handler = ^ fz_document_handler;
    fz_document_handler = record
      recognize: Pointer; // fz_document_recognize_fn
      open: Pointer; // fz_document_open_fn *open;
      open_with_stream: Pointer; // fz_document_open_with_stream_fn;
    end;


// pointer types to various internal structures. Should be considered as OPAQUE for now :)


 pfz_context = pointer;
 pfz_document = pointer;
 pfz_page= pointer;
 pfz_device = pointer;

{
    Pixmaps represent a set of pixels for a 2 dimensional region of a
    plane. Each pixel has n components per pixel, the last of which is
    always alpha. The data is in premultiplied alpha when rendering, but
    non-premultiplied for colorspace conversions and rescaling.
}
 pfz_pixmap = pointer;

 {
    An fz_colorspace object represents an abstract colorspace. While
    this should be treated as a black box by callers of the library at
    this stage, know that it encapsulates knowledge of how to convert
    colors to and from the colorspace, any lookup tables generated, the
    number of components in the colorspace etc.
}

 pfz_colorspace = pointer;

 {
      Fitz COOKIE - simple communication channel between app/library.
      Provide two-way communication between application and library.
      Intended for multi-threaded applications where one thread is
      rendering pages and another thread wants read progress
      feedback or abort a job that takes a long time to finish. The
      communication is unsynchronized without locking.

      abort: The appliation should set this field to 0 before
      calling fz_run_page to render a page. At any point when the
      page is being rendered the application my set this field to 1
      which will cause the rendering to finish soon. This field is
      checked periodically when the page is rendered, but exactly
      when is not known, therefore there is no upper bound on
      exactly when the the rendering will abort. If the application
      did not provide a set of locks to fz_new_context, it must also
      await the completion of fz_run_page before issuing another
      call to fz_run_page. Note that once the application has set
      this field to 1 after it called fz_run_page it may not change
      the value again.

      progress: Communicates rendering progress back to the
      application and is read only. Increments as a page is being
      rendered. The value starts out at 0 and is limited to less
      than or equal to progress_max, unless progress_max is -1.

      progress_max: Communicates the known upper bound of rendering
      back to the application and is read only. The maximum value
      that the progress field may take. If there is no known upper
      bound on how long the rendering may take this value is -1 and
      progress is not limited. Note that the value of progress_max
      may change from -1 to a positive value once an upper bound is
      known, so take this into consideration when comparing the
      value of progress to that of progress_max.

      errors: count of errors during current rendering.
 }


type

      pfz_cookie = ^fz_cookie_rec;
      fz_cookie_rec= record
               abort,
               progress,
               progress_max, // -1 for unknown
               errors: cint
           end;


VAR
  {
  extern fz_document_handler pdf_document_handler;
  extern fz_document_handler xps_document_handler;
  extern fz_document_handler cbz_document_handler;
  extern fz_document_handler img_document_handler;
  extern fz_document_handler tiff_document_handler;
  extern fz_document_handler html_document_handler;
  extern fz_document_handler epub_document_handler;
  extern fz_document_handler gprf_document_handler;
  }
  pdf_document_handler: fz_document_handler; EXTERNAL muLibName NAME 'pdf_document_handler';
  xps_document_handler: fz_document_handler; EXTERNAL muLibName NAME 'xps_document_handler';
  cbz_document_handler: fz_document_handler; EXTERNAL muLibName NAME 'cbz_document_handler';
  img_document_handler: fz_document_handler; EXTERNAL muLibName NAME 'img_document_handler';
  tiff_document_handler: fz_document_handler; EXTERNAL muLibName NAME 'tiff_document_handler';
  html_document_handler: fz_document_handler; EXTERNAL muLibName NAME 'html_document_handler';
  epub_document_handler: fz_document_handler; EXTERNAL muLibName NAME 'epub_document_handler';

  { Another way.  It does compiles but aborts as well. }
//  another_c_string: STRING; EXTERNAL 'the_lib_file.so' NAME 'the_c_var_name';

const

//********************************************************************
// Fitz CONTEXT
//********************************************************************

{   Specifies the maximum size in bytes of the resource store in
    pfz_context. Given as argument to fz_new_context.

    FZ_STORE_UNLIMITED: Let resource store grow unbounded.

    FZ_STORE_DEFAULT: A reasonable upper bound on the size, for
    devices that are not memory constrained.
}
    FZ_STORE_UNLIMITED = 0;
    FZ_STORE_DEFAULT = 256 << 20;

// following functions are used to test byte alignment and parameter passing and retrieving
// function fz_test(pero: cint; pero2: int32; var fz_rect: fz_rectangle): fz_rectangle;cdecl;external muLibName name 'fz_dinko';
// procedure fz_test(pero: cint; var pero2: cint; var fz_rect: fz_rectangle);cdecl;external muLibName name 'fz_dinko';


//********************************************************************
// Fitz Context
//********************************************************************

{
fz_new_context: Allocate context containing global state.

The global state contains an exception stack, resource store,
etc. Most functions in MuPDF take a context argument to be
able to reference the global state. See fz_drop_context for
freeing an allocated context.

alloc: Supply a custom memory allocator through a set of
function pointers. Set to NULL for the standard library
allocator. The context will keep the allocator pointer, so the
data it points to must not be modified or freed during the
lifetime of the context.

locks: Supply a set of locks and functions to lock/unlock
them, intended for multi-threaded applications. Set to NULL
when using MuPDF in a single-threaded applications. The
context will keep the locks pointer, so the data it points to
must not be modified or freed during the lifetime of the
context.

max_store: Maximum size in bytes of the resource store, before
it will start evicting cached resources such as fonts and
images. FZ_STORE_UNLIMITED can be used if a hard limit is not
desired. Use FZ_STORE_DEFAULT to get a reasonable size.

Does not throw exceptions, but may return NULL.

fz_context *fz_new_context_imp(fz_alloc_context *alloc, fz_locks_context *locks, unsigned int max_store, const char *version);
#define fz_new_context(alloc, locks, max_store) fz_new_context_imp(alloc, locks, max_store, FZ_VERSION)
}

function fz_new_context_imp(alloc:Pointer; locks: Pointer; max_store: cuint; version: PChar): pfz_context;cdecl;external muLibName name 'fz_new_context_imp';
function fz_new_context(alloc:Pointer; locks: Pointer; max_store: cuint): pfz_context;
procedure fz_new_document_handler_context(ctx: pfz_context);cdecl;external muLibName name 'fz_new_document_handler_context';
procedure fz_drop_document_handler_context(ctx: pfz_context);cdecl;external muLibName name 'fz_drop_document_handler_context';

{
extern fz_document_handler pdf_document_handler;
extern fz_document_handler xps_document_handler;
extern fz_document_handler cbz_document_handler;
extern fz_document_handler img_document_handler;
extern fz_document_handler tiff_document_handler;
extern fz_document_handler html_document_handler;
extern fz_document_handler epub_document_handler;
extern fz_document_handler gprf_document_handler;

void fz_register_document_handler(fz_context *ctx, const fz_document_handler *handler);
}
procedure fz_register_document_handler(ctx: pfz_context; var handler: fz_document_handler);cdecl;external muLibName name 'fz_register_document_handler';

{
void fz_register_document_handlers(fz_context *ctx);  // the procedure exists in library dll but I make my own
}
procedure fz_register_document_handlers(ctx: pfz_context);


{
fz_drop_context: Free a context and its global state.

The context and all of its global state is freed, and any
buffered warnings are flushed (see fz_flush_warnings). If NULL
is passed in nothing will happen.

Does not throw exceptions.

void fz_drop_context(fz_context *ctx);
}
procedure fz_drop_context_original(ctx: pfz_context);cdecl;external muLibName name 'fz_drop_context';
procedure fz_drop_context(ctx: pfz_context);  // My function


{
fz_rotate: Create a rotation matrix.

The returned matrix is of the form
[ cos(deg) sin(deg) -sin(deg) cos(deg) 0 0 ].

m: Pointer to place to store matrix

degrees: Degrees of counter clockwise rotation. Values less
than zero and greater than 360 are handled as expected.

Returns m.

Does not throw exceptions.

fz_matrix *fz_rotate(fz_matrix *m, float degrees);
}
procedure fz_rotate(var m: fz_matrix; theta: cfloat);cdecl;external muLibName name 'fz_rotate';

{
fz_pre_scale: Scale a matrix by premultiplication.

m: Pointer to the matrix to scale

sx, sy: Scaling factors along the X- and Y-axes. A scaling
factor of 1.0 will not cause any scaling along the relevant
axis.

Returns m (updated).

Does not throw exceptions.

fz_matrix *fz_pre_scale(fz_matrix *m, float sx, float sy);
}
procedure fz_pre_scale(var mat: fz_matrix; sx: cfloat; sy: cfloat);cdecl;external muLibName name 'fz_pre_scale';


// Fitz COLORSPACE

{
fz_lookup_device_colorspace: Find a standard colorspace based upon
it's name.

fz_colorspace *fz_lookup_device_colorspace(fz_context *ctx, char *name);
}
function fz_lookup_device_colorspace(ctx:pfz_context; name: pchar):pfz_colorspace;cdecl;external muLibName name 'fz_lookup_device_colorspace';

//********************************************************************
//   Fitz Pixmap
//********************************************************************

{
fz_pixmap_bbox: Return the bounding box for a pixmap.

fz_irect *fz_pixmap_bbox(fz_context *ctx, fz_pixmap *pix, fz_irect *bbox);
}
function fz_pixmap_bbox(ctx:pfz_context; pix:pfz_pixmap):fz_bbox;cdecl;external muLibName name 'fz_pixmap_bbox';

{
fz_pixmap_width: Return the width of the pixmap in pixels.

int fz_pixmap_width(fz_context *ctx, fz_pixmap *pix);
}
function fz_pixmap_width(ctx:pfz_context; pix: pfz_pixmap):cint;cdecl;external muLibName name 'fz_pixmap_width';

{
fz_pixmap_height: Return the height of the pixmap in pixels.

int fz_pixmap_height(fz_context *ctx, fz_pixmap *pix);
}
function fz_pixmap_height(ctx:pfz_context; pix: pfz_pixmap):cint;cdecl;external muLibName name 'fz_pixmap_height';

{
fz_new_pixmap: Create a new pixmap, with it's origin at (0,0)

cs: The colorspace to use for the pixmap, or NULL for an alpha
plane/mask.

w: The width of the pixmap (in pixels)

h: The height of the pixmap (in pixels)

Returns a pointer to the new pixmap. Throws exception on failure to
allocate.

fz_pixmap *fz_new_pixmap(fz_context *ctx, fz_colorspace *cs, int w, int h);
}
function fz_new_pixmap(ctx:pfz_context; cs:pfz_colorspace; w,h: cint):pfz_pixmap;cdecl;external muLibName name 'fz_new_pixmap';

{
fz_new_pixmap_with_bbox: Create a pixmap of a given size,
location and pixel format.

The bounding box specifies the size of the created pixmap and
where it will be located. The colorspace determines the number
of components per pixel. Alpha is always present. Pixmaps are
reference counted, so drop references using fz_drop_pixmap.

colorspace: Colorspace format used for the created pixmap. The
pixmap will keep a reference to the colorspace.

bbox: Bounding box specifying location/size of created pixmap.

Returns a pointer to the new pixmap. Throws exception on failure to
allocate.

fz_pixmap *fz_new_pixmap_with_bbox(fz_context *ctx, fz_colorspace *colorspace, const fz_irect *bbox);
}
function fz_new_pixmap_with_bbox(ctx: pfz_context; cs: pfz_colorspace; var bbox: fz_irect):pfz_pixmap;cdecl;external muLibName name 'fz_new_pixmap_with_bbox';

{
fz_new_pixmap_with_data: Create a new pixmap, with it's origin at
(0,0) using the supplied data block.

cs: The colorspace to use for the pixmap, or NULL for an alpha
plane/mask.

w: The width of the pixmap (in pixels)

h: The height of the pixmap (in pixels)

samples: The data block to keep the samples in.

Returns a pointer to the new pixmap. Throws exception on failure to
allocate.

fz_pixmap *fz_new_pixmap_with_data(fz_context *ctx, fz_colorspace *colorspace, int w, int h, unsigned char *samples);
}
function fz_new_pixmap_with_data(ctx:pfz_context; cs:pfz_colorspace; w,h: cint; samples: pointer):pfz_pixmap;cdecl;external muLibName name 'fz_new_pixmap_with_data';

{
fz_new_pixmap_with_bbox_and_data: Create a pixmap of a given size,
location and pixel format, using the supplied data block.

The bounding box specifies the size of the created pixmap and
where it will be located. The colorspace determines the number
of components per pixel. Alpha is always present. Pixmaps are
reference counted, so drop references using fz_drop_pixmap.

colorspace: Colorspace format used for the created pixmap. The
pixmap will keep a reference to the colorspace.

bbox: Bounding box specifying location/size of created pixmap.

samples: The data block to keep the samples in.

Returns a pointer to the new pixmap. Throws exception on failure to
allocate.

fz_pixmap *fz_new_pixmap_with_bbox_and_data(fz_context *ctx, fz_colorspace *colorspace, const fz_irect *rect, unsigned char *samples);
}
function fz_new_pixmap_with_bbox_and_data(ctx: pfz_context; cs: pfz_colorspace; var r: fz_irect; samples: pointer):pfz_pixmap;cdecl;external muLibName name 'fz_new_pixmap_with_bbox_and_data';


{
fz_keep_pixmap: Take a reference to a pixmap.
pix: The pixmap to increment the reference for.
Returns pix. Does not throw exceptions.

fz_pixmap *fz_keep_pixmap(fz_context *ctx, fz_pixmap *pix);
}
function  fz_keep_pixmap(ctx: pfz_context; pix: pfz_pixmap):pfz_pixmap;cdecl;external muLibName name 'fz_keep_pixmap';

{
fz_drop_pixmap: Drop a reference and free a pixmap.

Decrement the reference count for the pixmap. When no
references remain the pixmap will be freed.

Does not throw exceptions.

void fz_drop_pixmap(fz_context *ctx, fz_pixmap *pix);
}
procedure fz_drop_pixmap(ctx: pfz_context; pix: pfz_pixmap);cdecl;external muLibName name 'fz_drop_pixmap';


{
fz_pixmap_colorspace: Return the colorspace of a pixmap
Returns colorspace. Does not throw exceptions.

fz_colorspace *fz_pixmap_colorspace(fz_context *ctx, fz_pixmap *pix);
}
function  fz_pixmap_colorspace(ctx: pfz_context; pix: pfz_pixmap):pfz_colorspace;cdecl;external muLibName name 'fz_pixmap_colorspace';

{
fz_pixmap_components: Return the number of components in a pixmap.

Returns the number of components. Does not throw exceptions.

int fz_pixmap_components(fz_context *ctx, fz_pixmap *pix);
}
function fz_pixmap_components(ctx:pfz_context; pix:pfz_pixmap):cint;cdecl;external muLibName name 'fz_pixmap_components';


{
fz_pixmap_samples: Returns a pointer to the pixel data of a pixmap.

Returns the pointer. Does not throw exceptions.

unsigned char *fz_pixmap_samples(fz_context *ctx, fz_pixmap *pix);
}
function fz_pixmap_samples(ctx: pfz_context; pix: pfz_pixmap): pointer;cdecl;external muLibName name 'fz_pixmap_samples';

{
fz_clear_pixmap_with_value: Clears a pixmap with the given value.

pix: The pixmap to clear.

value: Values in the range 0 to 255 are valid. Each component
sample for each pixel in the pixmap will be set to this value,
while alpha will always be set to 255 (non-transparent).

Does not throw exceptions.

void fz_clear_pixmap_with_value(fz_context *ctx, fz_pixmap *pix, int value);
}
procedure fz_clear_pixmap_with_value(ctx:pfz_context; pix: pfz_pixmap; value: cint);cdecl;external muLibName name 'fz_clear_pixmap_with_value';

{
fz_clear_pixmap_with_value: Sets all components (including alpha) of
all pixels in a pixmap to 0.

pix: The pixmap to clear.

Does not throw exceptions.

void fz_clear_pixmap(fz_context *ctx, fz_pixmap *pix);
}
procedure fz_clear_pixmap(ctx:pfz_context; pix:pfz_pixmap);cdecl;external muLibName name 'fz_clear_pixmap';


//********************************************************************
// Fitz DEVICE
//********************************************************************

{
The different format handlers (pdf, xps etc) interpret pages to a
device. These devices can then process the stream of calls they
recieve in various ways:
The trace device outputs debugging information for the calls.
The draw device will render them.
The list device stores them in a list to play back later.
The text device performs text extraction and searching.
The bbox device calculates the bounding box for the page.
Other devices can (and will) be written in future.
}


{
fz_drop_device: Free a devices of any type and its resources.
void fz_drop_device(fz_context *ctx, fz_device *dev);
}
procedure fz_drop_device(ctx: pfz_context; dev: pfz_device);cdecl;external muLibName name 'fz_drop_device';


{
fz_new_trace_device: Create a device to print a debug trace of
all device calls.

fz_device *fz_new_trace_device(fz_context *ctx);
}
function  fz_new_trace_device(ctx:pfz_context):pfz_device;cdecl;external muLibName name 'fz_new_trace_device';

{
fz_new_bbox_device: Create a device to compute the bounding
box of all marks on a page.

The returned bounding box will be the union of all bounding
boxes of all objects on a page.

fz_device *fz_new_bbox_device(fz_context *ctx, fz_rect *rectp);
}
function  fz_new_bbox_device(ctx: pfz_context; var rect: fz_rect):pfz_device;cdecl;external muLibName name 'fz_new_bbox_device';

{
fz_new_draw_device: Create a device to draw on a pixmap.

dest: Target pixmap for the draw device. See fz_new_pixmap*
for how to obtain a pixmap. The pixmap is not cleared by the
draw device, see fz_clear_pixmap* for how to clear it prior to
calling fz_new_draw_device. Free the device by calling
fz_drop_device.

fz_device *fz_new_draw_device(fz_context *ctx, fz_pixmap *dest);
}
function fz_new_draw_device(ctx: pfz_context; dest: pfz_pixmap):pfz_device;cdecl;external muLibName name 'fz_new_draw_device';

{
fz_new_draw_device_with_bbox: Create a device to draw on a pixmap.

dest: Target pixmap for the draw device. See fz_new_pixmap*
for how to obtain a pixmap. The pixmap is not cleared by the
draw device, see fz_clear_pixmap* for how to clear it prior to
calling fz_new_draw_device. Free the device by calling
fz_drop_device.

clip: Bounding box to restrict any marking operations of the
draw device.

fz_device *fz_new_draw_device_with_bbox(fz_context *ctx, fz_pixmap *dest, const fz_irect *clip);
}
function fz_new_draw_device_with_bbox(ctx: pfz_context; dest: pfz_pixmap; var clip: fz_irect):pfz_device;cdecl;external muLibName name 'fz_new_draw_device_with_bbox';

//********************************************************************
// Fitz DOCUMENT
//********************************************************************

{
fz_open_document: Open a PDF, XPS or CBZ document.

Open a document file and read its basic structure so pages and
objects can be located. MuPDF will try to repair broken
documents (without actually changing the file contents).

The returned fz_document is used when calling most other
document related functions. Note that it wraps the context, so
those functions implicitly can access the global state in context.

filename: a path to a file as it would be given to open(2).
}
function fz_open_document(ctx: pfz_context; const filename: PChar): pfz_document;cdecl;external muLibName name 'fz_open_document';

{
fz_drop_document: Release an open document.

The resource store in the context associated with fz_document
is emptied, and any allocations for the document are freed when
the last reference is dropped.

Does not throw exceptions.
void fz_drop_document(fz_context *ctx, fz_document *doc);
}
procedure fz_drop_document(ctx: pfz_context; doc: pfz_document);cdecl;external muLibName name 'fz_drop_document';


{
fz_needs_password: Check if a document is encrypted with a
non-blank password.

Does not throw exceptions.
int fz_needs_password(fz_context *ctx, fz_document *doc);
}
function fz_needs_password(ctx: pfz_context; doc: pfz_document):cint;cdecl;external muLibName name 'fz_needs_password';

{
fz_authenticate_password: Test if the given password can
decrypt the document.

password: The password string to be checked. Some document
specifications do not specify any particular text encoding, so
neither do we.

Does not throw exceptions.

int fz_authenticate_password(fz_context *ctx, fz_document *doc, const char *password);
}
function fz_authenticate_password(ctx: pfz_context; doc: pfz_document; password: PChar):cint;cdecl;external muLibName name 'fz_authenticate_password';

{
fz_count_pages: Return the number of pages in document

May return 0 for documents with no pages.

int fz_count_pages(fz_context *ctx, fz_document *doc);
}
function fz_count_pages(ctx: pfz_context; doc: pfz_document):cint;cdecl;external muLibName name 'fz_count_pages';

//********************************************************************
// Fitz Page
//********************************************************************


{
fz_load_page: Load a page.

After fz_load_page is it possible to retrieve the size of the
page using fz_bound_page, or to render the page using
fz_run_page_*. Free the page by calling fz_drop_page.

number: page number, 0 is the first page of the document.

fz_page *fz_load_page(fz_context *ctx, fz_document *doc, int number);
}
function fz_load_page(ctx: pfz_context; doc: pfz_document; number: cint):pfz_page;cdecl;external muLibName name 'fz_load_page';

{
fz_drop_page: Free a loaded page.

Does not throw exceptions.
void fz_drop_page(fz_context *ctx, fz_page *page);
}
procedure fz_drop_page(ctx: pfz_context; page: pfz_page);cdecl;external muLibName name 'fz_drop_page';

{
fz_bound_page: Determine the size of a page at 72 dpi.

Does not throw exceptions.

fz_rect *fz_bound_page(fz_context *ctx, fz_page *page, fz_rect *rect);
}
procedure fz_bound_page(ctx: pfz_context; page: pfz_page; var r: fz_rect);cdecl;external muLibName name 'fz_bound_page';


{
fz_run_page: Run a page through a device.

page: Page obtained from fz_load_page.

dev: Device obtained from fz_new_*_device.

transform: Transform to apply to page. May include for example
scaling and rotation, see fz_scale, fz_rotate and fz_concat.
Set to fz_identity if no transformation is desired.

cookie: Communication mechanism between caller and library
rendering the page. Intended for multi-threaded applications,
while single-threaded applications set cookie to NULL. The
caller may abort an ongoing rendering of a page. Cookie also
communicates progress information back to the caller. The
fields inside cookie are continually updated while the page is
rendering.

void fz_run_page(fz_context *ctx, fz_page *page, fz_device *dev, const fz_matrix *transform, fz_cookie *cookie);
}
procedure fz_run_page(ctx: pfz_context; page: pfz_page; dev: pfz_device; var transform: fz_matrix; cookie: pfz_cookie);cdecl;external muLibName name 'fz_run_page';



{
fz_transform_rect: Apply a transform to a rectangle.

After the four corner points of the axis-aligned rectangle
have been transformed it may not longer be axis-aligned. So a
new axis-aligned rectangle is created covering at least the
area of the transformed rectangle.

transform: Transformation matrix to apply. See fz_concat,
fz_scale and fz_rotate for how to create a matrix.

rect: Rectangle to be transformed. The two special cases
fz_empty_rect and fz_infinite_rect, may be used but are
returned unchanged as expected.

Does not throw exceptions.

fz_rect *fz_transform_rect(fz_rect *restrict rect, const fz_matrix *restrict transform);
}
procedure fz_transform_rect(var rect :  fz_rect; var transform: fz_matrix);cdecl;external muLibName name 'fz_transform_rect';

const
  Init_fz_bbox: fz_bbox = ({$H+});
  Init_fz_rectangle: fz_rect = ({$H+});
  Init_fz_matrix: fz_matrix = ({$H+});
  Init_fz_irect: fz_irect = ({$H+});


implementation

function fz_new_context(alloc: Pointer; locks: Pointer; max_store: cuint
  ): pfz_context;
var MyContext: pfz_context;
begin
  MyContext := nil;
  MyContext := fz_new_context_imp(alloc, locks, max_store, PChar(FZ_VERSION));
  if MyContext <> nil then begin
    fz_new_document_handler_context(MyContext);
    fz_register_document_handler(MyContext, pdf_document_handler);
  end;
  Result := MyContext;
end;

procedure fz_register_document_handlers(ctx: pfz_context);
begin
	fz_register_document_handler(ctx, pdf_document_handler);
	fz_register_document_handler(ctx, xps_document_handler);
	fz_register_document_handler(ctx, cbz_document_handler);
	fz_register_document_handler(ctx, img_document_handler);
	fz_register_document_handler(ctx, tiff_document_handler);
	fz_register_document_handler(ctx, html_document_handler);
	fz_register_document_handler(ctx, epub_document_handler);
end;

procedure fz_drop_context(ctx: pfz_context);
begin
  if ctx <> nil then begin
    fz_drop_document_handler_context(ctx);
    fz_drop_context_original(ctx);
  end;
end;

end.

