/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */

#include <iostream>
#include <sstream>
using std::stringstream;
using std::cout;
using std::endl;

#include <VCELL/ZipUtils.h>
#include <sys/timeb.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include <fcntl.h>
#include <cstring>
#include <fstream>
#include <memory>
#include <stdint.h>
#include <stdlib.h>

#include <zip.h>
#include <ziptool_lib.h>

using namespace std;
struct zip;

#if ( !defined(WIN32) && !defined(WIN64) ) // UNIX
#include <unistd.h>
#endif

bool exists(const char* name){
    if (FILE *file = fopen(name, "r")) {
        fclose(file);
        return true;
    } else {
        return false;
    }   
}

/**

ziptool -c test.zip add_file file4.txt ../VCellZipUtils/test/file2.txt 0 0 set_file_compression 3 store none

*/

static zip_t *
read_from_file(const char *archive, int flags, zip_error_t *error)
{

	zip_uint64_t offset = 0;
	zip_uint64_t length = 0;

    zip_t *zaa;
    zip_source_t *source;
    int err;

    if (offset == 0 && length == 0) {
		if ((zaa = zip_open(archive, flags, &err)) == NULL) {
		    zip_error_set(error, err, errno);
		    return NULL;
		}
    }
    else {
//        if (length > ZIP_INT64_MAX) {
//            zip_error_set(error, ZIP_ER_INVAL, 0);
//            return NULL;
//        }
		if ((source = zip_source_file_create(archive, offset, (zip_int64_t)length, error)) == NULL
		    || (zaa = zip_open_from_source(source, flags, error)) == NULL) {
		    zip_source_free(source);
		    return NULL;
		}
    }

    return zaa;
}

/**
  adds one or two files as uncompressed entries into a zip archive (creating the archive if necessary).
  entry names are the stripped filenames without full path.
*/
void addFilesToZip(const char *ziparchive, const char *filepath1, const char *filepath2)
{
#if ( !defined(WIN32) && !defined(WIN64) && defined(USE_MESSAGING)) // UNIX
#define USE_SHELL_ZIP 1
#endif

#ifdef USE_SHELL_ZIP

	//printf("---------- using shell zip --------\n");
	char zipcommand [1024];
	if (filepath2 != NULL){
		sprintf(zipcommand, "zip -0 -g %s %s %s", ziparchive, filepath1, filepath2);
	}else{
		sprintf(zipcommand, "zip -0 -g %s %s", ziparchive, filepath1);
	}
	int retcode = system(zipcommand);
	if (retcode != 0){
		char errmsg [2048];
		sprintf(errmsg, "zip command failed: %s", zipcommand);
		throw errmsg;
	}

#else
	//printf("------- using libzip --------\n");

    zip_error_t error;
    zip_error_init(&error);

	int existingEntries = 0;
	zip_t *za = read_from_file(ziparchive, 0, &error);
	if (za != NULL){
		existingEntries = zip_get_num_entries(za, 0);
		zip_close(za);
	}
	char indexFirstAddedEntry [128];
	sprintf(indexFirstAddedEntry, "%d", existingEntries);

	//
	// strip filename from filepath1
	//
	std::string entryName1 = filepath1;
	std::size_t dirPos1 = entryName1.find_last_of("/\\");
	if (dirPos1 > 0){
		entryName1 = entryName1.substr(dirPos1+1, entryName1.length());
	}
	
	const char* argv[50];	
	int argc = 0;
	argv[argc++] = "ziptool_main";
	argv[argc++] = "-cn";
	argv[argc++] = ziparchive;
	argv[argc++] = "add_file";
	argv[argc++] = entryName1.c_str();
	argv[argc++] = filepath1;
	argv[argc++] = "0";
	argv[argc++] = "0";
	argv[argc++] = "set_file_compression";
	argv[argc++] = indexFirstAddedEntry;
	argv[argc++] = "store";
	argv[argc++] = "none";
	
	if (filepath2 != NULL){
		//
		// strip filename from filepath2
		//
		std::string entryName2 = filepath2;
		std::size_t dirPos2 = entryName2.find_last_of("/\\");
		if (dirPos2 > 0){
			entryName2 = entryName1.substr(dirPos2+1, entryName2.length());
		}
		
		char indexSecondAddedEntry [128];
		sprintf(indexSecondAddedEntry, "%d", existingEntries + 1);
		argv[argc++] = "add_file";
		argv[argc++] = entryName2.c_str();
		argv[argc++] = filepath2;
		argv[argc++] = "0";
		argv[argc++] = "0";
		argv[argc++] = "set_file_compression";
		argv[argc++] = indexSecondAddedEntry;
		argv[argc++] = "store";
		argv[argc++] = "none";
	}
	int retcode = ziptool_main(argc, argv);
#endif
}

static zip_int64_t
name_locate(zip_t *za, const char* entryName) {
    zip_flags_t flags = 0;
    zip_int64_t idx;

    if ((idx=zip_name_locate(za, entryName, flags)) < 0) {
		throw "can't find named entry";
    }

    return idx;
}

/*
void extractFileFromZip(const char *zipFilename, const char *zipEntryName){
    zip_error_t error;
    zip_error_init(&error);

	int existingEntries = 0;
	zip_t *za = read_from_file(ziparchive, 0, &error);
	if (za != NULL){
		existingEntries = zip_get_num_entries(za, 0);
		zip_close(za);
	}
	
	zip_int64_t index = name_locate(za, zipEntryName);
	
	char indexFirstAddedEntry [128];
	sprintf(indexFirstAddedEntry, "%d", existingEntries);

	const char* argv[50];	
	int argc = 0;
	argv[argc++] = "ziptool_main";
	argv[argc++] = "-cn";
	argv[argc++] = ziparchive;
	argv[argc++] = "add_file";
	argv[argc++] = filename1;
	argv[argc++] = filename1;
	argv[argc++] = "0";
	argv[argc++] = "0";
	argv[argc++] = "set_file_compression";
	argv[argc++] = indexFirstAddedEntry;
	argv[argc++] = "store";
	argv[argc++] = "none";
	
	if (filename2 != NULL){
		char indexSecondAddedEntry [128];
		sprintf(indexSecondAddedEntry, "%d", existingEntries + 1);
		argv[argc++] = "add_file";
		argv[argc++] = filename2;
		argv[argc++] = filename2;
		argv[argc++] = "0";
		argv[argc++] = "0";
		argv[argc++] = "set_file_compression";
		argv[argc++] = indexSecondAddedEntry;
		argv[argc++] = "store";
		argv[argc++] = "none";
	}
	int retcode = ziptool_main(argc, argv);
}
*/

void extractFileFromZip(const char *zipFilename, const char *zipEntryName)
{
    char buf[100];
    int err;
    int fd;
    long long sum;

	zip_t* za = NULL;
	
    if ((za = zip_open(zipFilename, 0, &err)) == NULL) {
        zip_error_to_str(buf, sizeof(buf), err, errno);
        fprintf(stderr, "can't open zip archive `%s': %s\n", zipFilename, buf);
        throw "cannot open zip archive";
    }

	zip_int64_t i = name_locate(za, zipEntryName);
	
    zip_stat_t sb;
    if (zip_stat_index(za, i, 0, &sb) == 0) {
        printf("Name: [%s], ", sb.name);
        printf("Size: [%llu], ", sb.size);
        printf("mtime: [%u]\n", (unsigned int)sb.mtime);
	    zip_file_t *zf = zip_fopen_index(za, i, 0);
        if (!zf) {
            fprintf(stderr, "failed to open zip archive %s\n", zipFilename);
            throw "failed to open archive";
        }

        fd = open(sb.name, O_RDWR | O_TRUNC | O_CREAT, 0644);
        if (fd < 0) {
            fprintf(stderr, "failed to open zip entry %s : %s\n",zipFilename,zipEntryName);
            throw "failed to open zip entry";
        }

        sum = 0;
        while (sum != sb.size) {
            int len = zip_fread(zf, buf, 100);
            if (len < 0) {
                fprintf(stderr, "failed to read zip entry %s : %s\n", zipFilename, zipEntryName);
                throw "failed to read zip entry";
            }
            write(fd, buf, len);
            sum += len;
        }
        close(fd);
        zip_fclose(zf);
    } else {
        printf("File[%s] Line[%d]\n", __FILE__, __LINE__);
    }


    if (zip_close(za) == -1) {
        fprintf(stderr, "%can't close zip archive `%s'\n", zipFilename);
        throw "failed to close zip archive";
    }
}


/* ----------- using zipper ---------------

#include <zipper/zipper.h>
#include <zipper/unzipper.h>

void zip(const char *outname, const char **filename)
{
	std::string zipFilename(outname, strnlen(outname, 1024));
	zipper::Zipper zipper(zipFilename);
	while (*filename){
		std::string entryFilename(*filename, strnlen(*filename, 1024));
		zipper.add(*filename);
		filename++;
	}
	zipper.close();
}

void unzip(const char *zipFilename, const char *zipEntryName)
{
	std::string zipFile(zipFilename, strnlen(zipFilename, 1024));
	std::string entry(zipEntryName, strnlen(zipEntryName, 1024));
	zipper::Unzipper unzipper(zipFile);
	unzipper.extractEntry(entry);
	unzipper.close();
}
------------ end using zipper --------------*/


/*---------- using libarchive ---------------

#include <archive.h>
#include <archive_entry.h>
#include <fcntl.h>

int copy_data(struct archive *ar, struct archive *aw)
{
  int r;
  const void *buff;
  size_t size;
  la_int64_t offset;

  for (;;) {
    r = archive_read_data_block(ar, &buff, &size, &offset);
    if (r == ARCHIVE_EOF)
      return (ARCHIVE_OK);
    if (r < ARCHIVE_OK)
      return (r);
    r = archive_write_data_block(aw, buff, size, offset);
    if (r < ARCHIVE_OK) {
      fprintf(stderr, "%s\n", archive_error_string(aw));
      return (r);
    }
  }
}



void unzip(const char *zipFilename, const char *zipEntryName)
{
	struct archive *a;
	struct archive *ext;
	struct archive_entry *entry;
	int flags;
	int r;

	// Select which attributes we want to restore.
	flags = ARCHIVE_EXTRACT_TIME;
	flags |= ARCHIVE_EXTRACT_PERM;
	flags |= ARCHIVE_EXTRACT_ACL;
	flags |= ARCHIVE_EXTRACT_FFLAGS;

	a = archive_read_new();
	archive_read_support_format_all(a);
	archive_read_support_compression_all(a);
	ext = archive_write_disk_new();
	archive_write_disk_set_options(ext, flags);
	archive_write_disk_set_standard_lookup(ext);
	if ((r = archive_read_open_filename(a, zipEntryName, 10240))){
		return; // empty file
	}
	for (;;) {
		r = archive_read_next_header(a, &entry);
		if (r == ARCHIVE_EOF){
			throw "zip entry not found";
		}
		if (r < ARCHIVE_OK) {
			fprintf(stderr, "%s\n", archive_error_string(a));
			if (r < ARCHIVE_WARN) {
				throw archive_error_string(a);
			}
		}
		if (strcmp(archive_entry_pathname(entry),zipEntryName)){
			// not the correct entry
			continue;
		}
		r = archive_write_header(ext, entry);
		if (r < ARCHIVE_OK) {
			fprintf(stderr, "%s\n", archive_error_string(ext));
		} else if (archive_entry_size(entry) > 0) {
			r = copy_data(a, ext);
 			if (r < ARCHIVE_OK){
				fprintf(stderr, "%s\n", archive_error_string(ext));
				if (r < ARCHIVE_WARN) {
					exit(1);
				}
			}
		}
		r = archive_write_finish_entry(ext);
		if (r < ARCHIVE_OK) {
			fprintf(stderr, "%s\n", archive_error_string(ext));
		}
		if (r < ARCHIVE_WARN) {
			throw archive_error_string(a);
		}
		break;
	}
	archive_read_close(a);
	archive_read_free(a);
	archive_write_close(ext);
	archive_write_free(ext);
}


void zip(const char *outname, const char **filename)
{
	struct archive *a;
	struct archive_entry *entry;
	struct stat st;
	char buff[8192];
	int len;
	int fd;

	a = archive_write_new();
	archive_write_set_format_zip(a);
	archive_write_set_options(a, "zip:compression=store");
	archive_write_set_options(a, "zip:fakecrc32");
	//archive_write_set_options(a, "zip:experimental");
	//archive_write_add_filter_none(a);
	archive_write_set_bytes_per_block(a, 0);   // no buffering
	archive_write_set_bytes_in_last_block(a, 1);
	archive_write_open_memory(a, buff, sizeof(buff), &used));



	archive_write_open_filename(a, outname);
	while (*filename) {
		stat(*filename, &st);


		entry = archive_entry_new();
		archive_entry_set_pathname(entry, *filename);
		archive_entry_set_mode(entry, S_IFREG | 0644);
		archive_entry_set_size(entry, st.st_size);
		archive_entry_set_uid(entry, st.st_uid);
		archive_entry_set_gid(entry, st.st_gid);
		archive_entry_set_mtime(entry, now, 0);
		archive_entry_set_atime(entry, now + 3, 0);
		archive_write_header(a, entry);
		archive_write_data(a, file_data1, sizeof(file_data1)));

		fd = open(*filename, O_RDONLY);
		std::cout << "opening file " << *filename << std::endl;
		len = read(fd, buff, sizeof(buff));
		std::cout << "read " << len << " bytes from file " << *filename << std::endl;
		while ( len > 0 ) {
			std::cout << "writing " << len << " bytes to zip entry " << *filename << std::endl;
			archive_write_data(a, buff, len);
			len = read(fd, buff, sizeof(buff));
			std::cout << "read " << len << " bytes from file " << *filename << std::endl;
		}
		close(fd);
		archive_entry_free(entry);
		filename++;
	}
	archive_write_close(a);
	archive_write_free(a);
}
-------------- end libarchive -------------------------*/
