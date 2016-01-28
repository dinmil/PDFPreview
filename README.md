# PDFPreview
PDFPreview for FPC/CodeTyphon/Lazarus

This project use mupdf library to generate preview and printout of pdf documents.
The reason for this is I need to port my old application to Linux and I use external Sumatra pdf viewer for preview and printing of pdf documents. Sumatra does not exists on linux, so...
Also sumatra use libmupdf 1.6 and I want 1.8.
The purpose of the document is to explain how to build libmupdf.dll (32 and 64 bit) for windows and Linux, how to link it in FPC (lazarus or codetyphon) application.
Example project show the use of library with common preview functions. You are encourage to extend this project. 
Initial idea for this FPC project is done by Blestan Tabakov. His version use one of first version of libmupdf library. 
Problems:
There are lot of changes in cdecl function calls between versions. Big change is between version 1.6 and 1.7. Almost every call in version 1.7 is changed in way that first param is CONTEXT variable. Some functions are changed in way that they have different params or param types.
