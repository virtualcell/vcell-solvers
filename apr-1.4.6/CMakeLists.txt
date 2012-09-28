project(apr)

if (UNIX)
	set(APR_OS_DIR unix)
elseif (MSVC)
	set(APR_OS_DIR win32)
endif()

file (GLOB HEADER_FILES include/*.h include/arch/*.h include/arch/${APR_OS_DIR}/*.h) 

if (MSVC)
	set(SRC_FILES
		atomic/${APR_OS_DIR}/apr_atomic.c
		dso/${APR_OS_DIR}/dso.c
		file_io/${APR_OS_DIR}/buffer.c
		file_io/${APR_OS_DIR}/dir.c
		file_io/${APR_OS_DIR}/filedup.c
		file_io/${APR_OS_DIR}/filepath.c
		file_io/${APR_OS_DIR}/filestat.c
		file_io/${APR_OS_DIR}/filesys.c
		file_io/${APR_OS_DIR}/flock.c
		file_io/${APR_OS_DIR}/open.c
		file_io/${APR_OS_DIR}/pipe.c
		file_io/${APR_OS_DIR}/readwrite.c
		file_io/${APR_OS_DIR}/seek.c
		file_io/unix/copy.c
		file_io/unix/fileacc.c
		file_io/unix/filepath_util.c
		file_io/unix/fullrw.c
		file_io/unix/mktemp.c
		file_io/unix/tempdir.c
		locks/${APR_OS_DIR}/proc_mutex.c
		locks/${APR_OS_DIR}/thread_cond.c
		locks/${APR_OS_DIR}/thread_mutex.c
		locks/${APR_OS_DIR}/thread_rwlock.c
		memory/unix/apr_pools.c
		misc/${APR_OS_DIR}/apr_app.c
		misc/${APR_OS_DIR}/charset.c
		misc/${APR_OS_DIR}/env.c
		misc/${APR_OS_DIR}/internal.c
		misc/${APR_OS_DIR}/misc.c
		misc/${APR_OS_DIR}/rand.c
		misc/${APR_OS_DIR}/start.c
		misc/${APR_OS_DIR}/utf8.c
		misc/unix/errorcodes.c
		misc/unix/getopt.c
		misc/unix/otherchild.c
		misc/unix/version.c
		mmap/unix/common.c
		mmap/${APR_OS_DIR}/mmap.c
		network_io/${APR_OS_DIR}/sendrecv.c
		network_io/${APR_OS_DIR}/sockets.c
		network_io/${APR_OS_DIR}/sockopt.c
		network_io/unix/inet_ntop.c
		network_io/unix/inet_pton.c
		network_io/unix/multicast.c
		network_io/unix/sockaddr.c
		network_io/unix/socket_util.c
		passwd/apr_getpass.c
		poll/unix/poll.c
		poll/unix/pollcb.c
		poll/unix/pollset.c
		poll/unix/select.c
		random/unix/apr_random.c
		random/unix/sha2.c
		random/unix/sha2_glue.c
		shmem/${APR_OS_DIR}/shm.c
		strings/apr_cpystrn.c
		strings/apr_fnmatch.c
		strings/apr_snprintf.c
		strings/apr_strings.c
		strings/apr_strnatcmp.c
		strings/apr_strtok.c
		tables/apr_hash.c
		tables/apr_tables.c
		threadproc/${APR_OS_DIR}/proc.c
		threadproc/${APR_OS_DIR}/signals.c
		threadproc/${APR_OS_DIR}/thread.c
		threadproc/${APR_OS_DIR}/threadpriv.c
		time/${APR_OS_DIR}/time.c
		time/${APR_OS_DIR}/timestr.c
		user/${APR_OS_DIR}/groupinfo.c
		user/${APR_OS_DIR}/userinfo.c
	)
elseif (UNIX)
	file(GLOB SRC_FILES
		atomic/${APR_OS_DIR}/*.c
		dso/${APR_OS_DIR}/*.c
		file_io/${APR_OS_DIR}/*.c
		locks/${APR_OS_DIR}/*.c
		memory/${APR_OS_DIR}/*.c
		misc/${APR_OS_DIR}/*.c
		mmap/${APR_OS_DIR}/*.c
		network_io/${APR_OS_DIR}/*.c
		passwd/*.c
		poll/${APR_OS_DIR}/*.c
		random/${APR_OS_DIR}/*.c
		shmem/${APR_OS_DIR}/*.c
		strings/*.c
		support/unix/*.c
		tables/*.c
		threadproc/${APR_OS_DIR}/*.c
		time/${APR_OS_DIR}/*.c
		user/${APR_OS_DIR}/*.c
	)
endif()

include_directories(include include/arch/ include/arch/${APR_OS_DIR}/ include/arch/unix)
add_library(apr-1 STATIC ${SRC_FILES} ${HEADER_FILES})

file(TO_NATIVE_PATH ${CMAKE_CURRENT_BINARY_DIR}/include APR_INCLUDE_DIR)
include_directories(${APR_INCLUDE_DIR})
if (MSVC)
	add_definitions(-DAPR_DECLARE_STATIC)
	
	# copy apr.hw to apr.h
	file(TO_NATIVE_PATH	${CMAKE_CURRENT_SOURCE_DIR}/include/apr.hw APR_HW)
	file(TO_NATIVE_PATH	${APR_INCLUDE_DIR}/apr.h APR_H)
	ADD_CUSTOM_COMMAND(
		TARGET apr-1
		PRE_BUILD
		COMMAND if not exist ${APR_INCLUDE_DIR} mkdir ${APR_INCLUDE_DIR}
		COMMAND copy ${APR_HW} ${APR_H}
	)
elseif (UNIX)
	add_definitions(-DHAVE_CONFIG_H -DLINUX=2 -D_REENTRANT -D_GNU_SOURCE)
   	# another way to copy file
	configure_file (
		${CMAKE_CURRENT_SOURCE_DIR}/include/apr.hu
		${CMAKE_CURRENT_BINARY_DIR}/include/apr.h
	)
	configure_file (
		${CMAKE_CURRENT_SOURCE_DIR}/include/arch/unix/apr_private.hu
		${CMAKE_CURRENT_BINARY_DIR}/include/apr_private.h
	)
endif()
