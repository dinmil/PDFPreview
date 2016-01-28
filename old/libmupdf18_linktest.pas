// ******************************************************************
// Compiler: FPC 3.1.1, CodeTyphon 540
// Author: Dinko Miljak,
// Original Template: MuPDF lib header port to FPC (c) 2013 by Blestan Tabakov
// Date: 2016-01-21
// ******************************************************************
unit libmupdf181;

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
  FZ_VERSION: String = '1.8';
{$ELSEIF Defined(UNIX)}
 FZ_VERSION: String = '1.7a';
{$IFEND}




{$IF Defined(MSWINDOWS)}
  {$IFDEF WIN32}
//    muLibName = 'libmupdf18-32.dll';
    muLibName = '';

//    {$linklib libcurl.a}
//    {$linklib libjbig2dec.a}
    //{$linklib libjpeg.a}
    //{$linklib libmujs.a}
    //    {$linklib libopenjpeg.a}
    {$Link g:\sb\src\branch0650\Test\PDFPreview\externallibs\i386_win32\libmupdf.a}
    //{$linklib libfreetype.a}
    //{$linklib libz.a}
  {$ELSE}
  muLibName = 'libmupdf18-64.dll';
//    muLibName = '';
    {$Link g:\sb\src\branch0650\Test\PDFPreview\externallibs\x86_64_win64\libmupdf.a}
  {$ENDIF}
{$ELSEIF Defined(DARWIN)}
  muLibName = 'libmupdf.dylib';
  {$LINKLIB mylib}
{$ELSEIF Defined(UNIX)}
  //muLibName = 'libmupdf.so';
  muLibName = '';

  {$linklib m}
  {$linklib c}
  {$linklib dl}

//  {$linklib libcairo}
//  {$linklib libglfw}

  {$linklib libcurl.a}
  {$linklib libjbig2dec.a}
  {$linklib libjpeg.a}
  {$linklib libmujs.a}
  {$linklib libopenjpeg.a}
  {$linklib libmupdf.a}
  {$linklib libfreetype.a}
  {$linklib libz.a}

  (*
  {$linklib /home/dinko/projects/mupdf-1.7a-source/build/release/libcurl.a}
  {$linklib /home/dinko/projects/mupdf-1.7a-source/build/release/libjbig2dec.a}
  {$linklib /home/dinko/projects/mupdf-1.7a-source/build/release/libjpeg.a}
  {$linklib /home/dinko/projects/mupdf-1.7a-source/build/release/libmujs.a}
  {$linklib /home/dinko/projects/mupdf-1.7a-source/build/release/libopenjpeg.a}
  {$linklib /home/dinko/projects/mupdf-1.7a-source/build/release/libmupdf.a}
  {$linklib /home/dinko/projects/mupdf-1.7a-source/build/release/libfreetype.a}
  {$linklib /home/dinko/projects/mupdf-1.7a-source/build/release/libz.a}
  *)
// muLibName = 'fitz';
  {uncomment the following lines to link statically instead of to a shared library}
  //{$LINKLIB /path-to/mupdf-1.2-source/build/debug/libfitz.a}
  //{$LINKLIB /path-to/mupdf-1.2-source/build/debug/libfreetype.a}
  //{$LINKLIB /path-to/mupdf-1.2-source/build/debug/libopenjpeg.a}
  //{$LINKLIB /path-to/mupdf-1.2-source/build/debug/libjbig2dec.a}
  //{$LINKLIB /path-to/mupdf-1.2-source/build/debug/libjpeg.a}
{$IFEND}


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


//VAR
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
//  pdf_document_handler: fz_document_handler; external muLibName;

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

function _fz_new_context_imp(alloc:Pointer; locks: Pointer; max_store: cuint; version: PChar): pfz_context;cdecl;external muLibName name '_fz_new_context_imp';

implementation

end.

