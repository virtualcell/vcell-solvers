project(apriutil)
find_file(SQL3 sqlite3.h)
if (SQL3 MATCHES SQL3-NOTFOUND)
	message(FATAL_ERROR "${PROJECT_NAME} requires sqlite3")
endif (SQL3 MATCHES SQL3-NOTFOUND)

set (HEADER_FILES 
	dbm/sdbm/sdbm_pair.h
	dbm/sdbm/sdbm_private.h
	dbm/sdbm/sdbm_tune.h
	include/apr_anylock.h
	include/apr_base64.h
	include/apr_buckets.h
	include/apr_date.h
	include/apr_dbm.h
	include/apr_hooks.h
	include/apr_ldap_url.h
	include/apr_md4.h
	include/apr_md5.h
	include/apr_memcache.h
	include/apr_optional.h
	include/apr_optional_hooks.h
	include/apr_queue.h
	include/apr_reslist.h
	include/apr_rmm.h
	include/apr_sdbm.h
	include/apr_sha1.h
	include/apr_strmatch.h
	include/apr_thread_pool.h
	include/apr_uri.h
	include/apr_uuid.h
	include/apr_xlate.h
	include/apr_xml.h
	include/apu_version.h
) 

set(SRC_FILES
	buckets/apr_brigade.c
	buckets/apr_buckets.c
	buckets/apr_buckets_alloc.c
	buckets/apr_buckets_eos.c
	buckets/apr_buckets_file.c
	buckets/apr_buckets_flush.c
	buckets/apr_buckets_heap.c
	buckets/apr_buckets_mmap.c
	buckets/apr_buckets_pipe.c
	buckets/apr_buckets_pool.c
	buckets/apr_buckets_refcount.c
	buckets/apr_buckets_simple.c
	buckets/apr_buckets_socket.c
	crypto/apr_crypto.c
	crypto/apr_md4.c
	crypto/apr_md5.c
	crypto/apr_sha1.c
	crypto/getuuid.c
	crypto/uuid.c
	dbd/apr_dbd.c
	dbd/apr_dbd_freetds.c
	dbd/apr_dbd_mysql.c
	dbd/apr_dbd_odbc.c
	dbd/apr_dbd_oracle.c
	dbd/apr_dbd_pgsql.c
	dbd/apr_dbd_sqlite2.c
	dbd/apr_dbd_sqlite3.c
	dbm/apr_dbm.c
	dbm/apr_dbm_berkeleydb.c
	dbm/apr_dbm_gdbm.c
	dbm/apr_dbm_sdbm.c
	encoding/apr_base64.c
	hooks/apr_hooks.c
	ldap/apr_ldap_init.c
	ldap/apr_ldap_option.c
	ldap/apr_ldap_rebind.c
	ldap/apr_ldap_stub.c
	ldap/apr_ldap_url.c
	memcache/apr_memcache.c
	misc/apr_date.c
	misc/apu_dso.c
	misc/apr_queue.c
	misc/apr_reslist.c
	misc/apr_rmm.c
	misc/apr_thread_pool.c
	misc/apu_version.c
	dbm/sdbm/sdbm.c
	dbm/sdbm/sdbm_hash.c
	dbm/sdbm/sdbm_lock.c
	dbm/sdbm/sdbm_pair.c
	strmatch/apr_strmatch.c
	uri/apr_uri.c
	xlate/xlate.c
	xml/apr_xml.c
)

include_directories(include include/private xml/expat/lib dbm/sdbm ../apr-1.4.6/include ../apr-iconv-1.2.1/include)
add_library(aprutil-1 STATIC ${SRC_FILES} ${HEADER_FILES})
set(APR_INCLUDE_DIR ${CMAKE_BINARY_DIR}/apr-1.4.6/include)
include_directories(${APR_INCLUDE_DIR})

file(TO_NATIVE_PATH ${CMAKE_CURRENT_BINARY_DIR}/include APU_INCLUDE_DIR)
include_directories(${APU_INCLUDE_DIR})

if (MSVC)
	add_definitions(-DAPR_DECLARE_STATIC -DAPU_DECLARE_STATIC -DAPI_DECLARE_STATIC -DAPU_USE_SDBM -DHAVE_SQL_H -DXML_STATIC)
	
	# copy apu.hw apu.h
	file(TO_NATIVE_PATH	${CMAKE_CURRENT_SOURCE_DIR}/include/apu.hw APU_HW)
	file(TO_NATIVE_PATH	${APU_INCLUDE_DIR}/apu.h APU_H)
	file(TO_NATIVE_PATH	${CMAKE_CURRENT_SOURCE_DIR}/include/apu_want.hw APU_WANT_HW)
	file(TO_NATIVE_PATH	${APU_INCLUDE_DIR}/apu_want.h APU_WANT_H)
	file(TO_NATIVE_PATH	${CMAKE_CURRENT_SOURCE_DIR}/include/apr_ldap.hw APR_LDAP_HW)
	file(TO_NATIVE_PATH	${APU_INCLUDE_DIR}/apr_ldap.h APR_LDAP_H)
	
	file(TO_NATIVE_PATH	${CMAKE_CURRENT_SOURCE_DIR}/include/private/apu_config.hw APU_CONFIG_HW)
	file(TO_NATIVE_PATH	${APU_INCLUDE_DIR}/apu_config.h APU_CONFIG_H)
	file(TO_NATIVE_PATH	${CMAKE_CURRENT_SOURCE_DIR}/include/private/apu_select_dbm.hw APU_SELECT_DBM_HW)
	file(TO_NATIVE_PATH	${APU_INCLUDE_DIR}/apu_select_dbm.h APU_SELECT_DBM_H)
	
	ADD_CUSTOM_COMMAND(
		TARGET aprutil-1
		PRE_BUILD
		# include
		COMMAND if not exist ${APU_INCLUDE_DIR} mkdir ${APU_INCLUDE_DIR}
		# copy apu.hw to apu.h
		COMMAND copy ${APU_HW} ${APU_H}
		# copy apu_want.hw to apu_want.h
		COMMAND copy ${APU_WANT_HW} ${APU_WANT_H}
		# copy apu_ldap.hw apu_ldap.h
		COMMAND copy ${APR_LDAP_HW} ${APR_LDAP_H}
		# copy apu_config.hw to apu_config.h
		COMMAND copy ${APU_CONFIG_HW} ${APU_CONFIG_H}
		# copy apu_select_dbm.hw to apu_select_dbm.h
		COMMAND copy ${APU_SELECT_DBM_HW} ${APU_SELECT_DBM_H}
	)
elseif (UNIX)
	add_definitions(-DLINUX=2 -D_REENTRANT -D_GNU_SOURCE)
	configure_file (
		${CMAKE_CURRENT_SOURCE_DIR}/include/apu.hu
		${APU_INCLUDE_DIR}/apu.h
	)
	configure_file (
		${CMAKE_CURRENT_SOURCE_DIR}/include/apu_want.hu
		${APU_INCLUDE_DIR}/apu_want.h
	)
	configure_file (
		${CMAKE_CURRENT_SOURCE_DIR}/include/apr_ldap.hu
		${APU_INCLUDE_DIR}/apr_ldap.h
	)
	configure_file (
		${CMAKE_CURRENT_SOURCE_DIR}/include/private/apu_config.hu
		${APU_INCLUDE_DIR}/apu_config.h
	)
	configure_file (
		${CMAKE_CURRENT_SOURCE_DIR}/include/private/apu_select_dbm.hu
		${APU_INCLUDE_DIR}/apu_select_dbm.h
	)
endif()
add_dependencies(aprutil-1 apr-1)
