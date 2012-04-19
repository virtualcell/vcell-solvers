#ifndef _OBJCOUNTERS_H_
#define _OBJCOUNTERS_H_
/*
 * Copyright (c) 2001 - 2008 Progress Software Corporation. All Rights Reserved.
 * This software is the confidential and proprietary information of Progress
 * Software Corporation ("Confidential Information").  You shall not
 * disclose such Confidential Information and shall use it only in
 * accordance with the terms of the license agreement you entered into
 * with Progress Software Corporation.
 */

#ifdef __cplusplus
extern "C" {
#endif

/**
 * Structure used to communicate object counts per classtype.
 */
typedef struct {
    /**
     * Identifier of unique classtype within client library for which
     * objects may be created at runtime.
     */
    int classtype;

    /**
     * A string describing the classtype. This is memory owned
     * by the library and should not be freed by the caller.
     * The character encoding is ISO8859-1.
     */
    const char *classname;

    /**
     * The object count, the interpretation of which depends
     * on how it was obtained. It could be the count since the
     * last reset, or the difference relative to a baseline.
     *
     * Access to the count is not synchronized with respect 
     * to library threads for performance, so it is possible
     * for it to be inaccurate.
     */
    long count;
} OBJECT_COUNTER;


/**
 * Fetch the current object counter value for the classtype.
 *
 * Access to the counter is not synchronized with respect
 * to library threads, so it is possible for the returned 
 * value to be inaccurate.
 *
 * @param classtype The classtype for which to retrieve counter
 * @return The number of objects of the given type that have
 *         been created since the counter was last reset,
 *         or -1 if an invalid classtype was specified
 */
long SMQ_API get_counter(int classtype);

/**
 * Reset the object counter for the classtype to zero.
 *
 * @param classtype The classtype for which to reset counter
 *
 * The function does nothing if the classtype is invalid.
 */
void SMQ_API reset_counter(int classtype);

/**
 * Allocates and copies out all current object counters.
 *
 * Memory used for the counters should be released by
 * calling free_counters().
 *
 * Access to the counters is not synchronized with respect
 * to library threads, so it is possible for the returned 
 * values to be inaccurate.
 *
 * @param counters Output buffer into which will be copied the
 *                 address of an array of OBJECT_COUNTER on success,
 *                 or NULL on memory allocation failure.
 * @return The number of counters in the array copied to the
 *         caller's buffer, or -1 if memory allocation failed.
 */
int SMQ_API snapshot_counters(OBJECT_COUNTER **counters);

/**
 * Allocates and copies out differences between current object
 * counters and baseline as returned by snapshot_counters.
 *
 * Memory used for the counters should be released by
 * calling free_counters().
 *
 * Access to the counters is not synchronized with respect
 * to library threads, so it is possible for the returned 
 * values to be inaccurate.
 *
 * @param counters Output buffer into which will be copied the
 *                 address of an array of OBJECT_COUNTER on success.
 * @param baseline An array of counters returned by snapshot_counters
 *                 against which to compare the current counters.
 * @param num_counters The number of counters in baseline, which must
 *                     be the same value returned by snapshot_counters.
 */
void SMQ_API diff_counters(OBJECT_COUNTER **counters, OBJECT_COUNTER *baseline, int num_counters);

/**
 * Frees a buffer returned from snapshot_counters or diff_counters.
 *
 * @param counters The pointer to be freed, which must have been
 *                 returned by snapshot_counters or diff_counters.
 */
void SMQ_API free_counters(OBJECT_COUNTER *counters);

/**
 * Return an ISO8859-1-encoded string representing the classname. 
 *
 * The memory returned by this function is owned by the library 
 * and should not be freed by the caller.
 *
 * @param classtype The classtype for which to return the name
 *
 * @return The name corresponding to the given classtype,
 *         or an empty null-terminated string if an invalid 
 *         classtype was specified.
 */
SMQ_API char *get_classname(int classtype);

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* _OBJCOUNTERS_H_ */

