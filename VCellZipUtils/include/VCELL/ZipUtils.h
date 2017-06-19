/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
/////////////////////////////////////////////////////////////
// ZipUtils.h
///////////////////////////////////////////////////////////
#ifndef ZIPUTILS_H
#define ZIPUTILS_H

extern void extractFileFromZip(const char *zipFilename, const char *zipEntryName);
extern void addFilesToZip(const char *zipFilename, const char *filename1, const char *filename2=NULL);

#endif
