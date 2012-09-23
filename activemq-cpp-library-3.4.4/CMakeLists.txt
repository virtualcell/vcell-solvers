project(activemq-cpp)

if (UNIX)
	set(AMQ_OS_DIR unix)
elseif (MSVC)
	set(AMQ_OS_DIR windows)
endif()

file(GLOB HEADER_FILES 
	src/main/cms/*.h
	src/main/activemq/util/*.h
	src/main/activemq/commands/*.h
	src/main/activemq/core/*.h
	src/main/activemq/core/policies/*.h
	src/main/activemq/exceptions/*.h
	src/main/activemq/io/*.h
	src/main/activemq/library/*.h
	src/main/activemq/transport/*.h
	src/main/activemq/wireformat/*.h
	src/main/activemq/wireformat/openwire/*.h
	src/main/activemq/wireformat/openwire/marshal/*.h
	src/main/activemq/wireformat/openwire/marshal/generated/*.h
	src/main/activemq/wireformat/openwire/utils/*.h
	src/main/activemq/wireformat/stomp/*.h
	src/main/activemq/cmsutil/*.h
	src/main/activemq/state/*.h
	src/main/activemq/threads/*.h
	src/main/decaf/internal/*.h
	src/main/decaf/internal/net/*.h
	src/main/decaf/internal/net/ssl/*.h
	src/main/decaf/internal/net/tcp/*.h
	src/main/decaf/internal/util/*.h
	src/main/decaf/internal/nio/*.h
	src/main/decaf/internal/util/*.h
	src/main/decaf/internal/util/concurrent/*.h
	src/main/decaf/internal/util/concurrent/${AMQ_OS_DIR}/*.h
	src/main/decaf/internal/util/zip/*.h
	src/main/decaf/internal/io/*.h
	src/main/decaf/internal/security/${AMQ_OS_DIR}/*.h
	src/main/decaf/lang/*.h
	src/main/decaf/lang/exceptions/*.h
	src/main/decaf/io/*.h
	src/main/decaf/net/*.h
	src/main/decaf/net/ssl/*.h
	src/main/decaf/nio/*.h
	src/main/decaf/security/*.h
	src/main/decaf/security/auth/x500/*.h
	src/main/decaf/util/*.h
	src/main/decaf/util/concurrent/*.h
	src/main/decaf/util/concurrent/atomic/*.h
	src/main/decaf/util/concurrent/locks/*.h
	src/main/decaf/util/logging/*.h
	src/main/decaf/util/zip/*.h) 

file(GLOB SRC_FILES
	src/main/cms/*.cpp
	src/main/activemq/util/*.cpp
	src/main/activemq/commands/*.cpp
	src/main/activemq/core/*.cpp
	src/main/activemq/core/policies/*.cpp
	src/main/activemq/exceptions/*.cpp
	src/main/activemq/io/*.cpp
	src/main/activemq/library/*.cpp
	src/main/activemq/transport/*.cpp
	src/main/activemq/transport/correlator/*.cpp
	src/main/activemq/transport/mock/*.cpp
	src/main/activemq/transport/tcp/*.cpp
	src/main/activemq/transport/failover/*.cpp
	src/main/activemq/transport/logging/*.cpp
	src/main/activemq/transport/inactivity/*.cpp
	src/main/activemq/wireformat/*.cpp
	src/main/activemq/wireformat/openwire/*.cpp
	src/main/activemq/wireformat/openwire/marshal/*.cpp
	src/main/activemq/wireformat/openwire/marshal/generated/*.cpp
	src/main/activemq/wireformat/openwire/utils/*.cpp
	src/main/activemq/wireformat/stomp/*.cpp
	src/main/activemq/cmsutil/*.cpp
	src/main/activemq/state/*.cpp
	src/main/activemq/threads/*.cpp
	src/main/decaf/internal/*.cpp
	src/main/decaf/internal/net/*.cpp
	src/main/decaf/internal/net/ssl/*.cpp
	src/main/decaf/internal/net/ssl/openssl/*.cpp
	src/main/decaf/internal/net/tcp/*.cpp
	src/main/decaf/internal/util/*.cpp
	src/main/decaf/internal/nio/*.cpp
	src/main/decaf/internal/util/*.cpp
	src/main/decaf/internal/util/concurrent/*.cpp
	src/main/decaf/internal/util/concurrent/${AMQ_OS_DIR}/*.cpp
	src/main/decaf/internal/util/zip/*.c
	src/main/decaf/internal/io/*.cpp
	src/main/decaf/internal/security/${AMQ_OS_DIR}/*.cpp
	src/main/decaf/lang/*.cpp
	src/main/decaf/io/*.cpp
	src/main/decaf/net/*.cpp
	src/main/decaf/net/ssl/*.cpp
	src/main/decaf/nio/*.cpp
	src/main/decaf/security/*.cpp
	src/main/decaf/util/*.cpp
	src/main/decaf/util/concurrent/*.cpp
	src/main/decaf/util/concurrent/atomic/*.cpp
	src/main/decaf/util/concurrent/locks/*.cpp
	src/main/decaf/util/logging/*.cpp
	src/main/decaf/util/zip/*.cpp
)

include_directories(src/main ../apr-1.4.6/include ../apr-util-1.4.1/include)
if (MSVC)
	set(APR_INCLUDE_DIR ${CMAKE_CURRENT_BINARY_DIR}/../apr-1.4.6/include)
	set(APU_INCLUDE_DIR ${CMAKE_CURRENT_BINARY_DIR}/../apr-util-1.4.1/include)
	include_directories(${APR_INCLUDE_DIR} ${APU_INCLUDE_DIR})
	add_definitions(-D_LIB -DWIN32_LEAN_AND_MEAN -D_CRT_SECURE_NO_DEPRECATE -DNOMINMAX -DAPR_DECLARE_STATIC -DAPU_DECLARE_STATIC -DAPI_DECLARE_STATIC)
endif()
add_library(activemq-cpp STATIC ${SRC_FILES} ${HEADER_FILES})
add_dependencies(activemq-cpp apr-1)
add_dependencies(activemq-cpp aprutil-1)
add_dependencies(activemq-cpp apriconv-1)
install(TARGETS activemq-cpp ARCHIVE DESTINATION bin)