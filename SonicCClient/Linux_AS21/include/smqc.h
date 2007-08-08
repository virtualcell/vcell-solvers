#ifndef _SMQC_H_
#define _SMQC_H_
/*
 * Copyright (c) 2003 Sonic Software Corporation. All Rights Reserved.
 *
 * This software is the confidential and proprietary information of Sonic
 * Software Corporation. ("Confidential Information").  You shall not
 * disclose such Confidential Information and shall use it only in
 * accordance with the terms of the license agreement you entered into
 * with Sonic.
 *
 * SONIC MAKES NO REPRESENTATIONS OR WARRANTIES ABOUT THE SUITABILITY OF THE
 * SOFTWARE, EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
 * PURPOSE, OR NON-INFRINGEMENT. SONIC SHALL NOT BE LIABLE FOR ANY DAMAGES
 * SUFFERED BY LICENSEE AS A RESULT OF USING, MODIFYING OR DISTRIBUTING
 * THIS SOFTWARE OR ITS DERIVATIVES.
 *
 * CopyrightVersion 1.0
 */

#ifdef __cplusplus
extern "C" {
#endif

/* C interface */


/* Success, error return codes */
#define SMQ_SUCCESS 0
#define SMQ_ERROR -1

#include <stddef.h>
#include <java_decls.h>
#include <objcounters.h>
#include <jmsconstants.h>


/** 
 * Returns the last error that occurred in the current thread.
 */
jint SMQ_API SMQ_getLastError();

/**
 * Returns a String object containing the text, if any, of the last 
 * error that occurred. The caller is responsible for releasing
 * the String object by calling String_release.
 */
HOBJ SMQ_API SMQ_getLastErrorText();

/**
 * Decrements the reference count of the object referenced by an HOBJ.
 *
 * @param obj the object to operate against.
 * @param pret will contain the reference count after decrementing.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define SMQ_release(obj, pret)                   Object_release(obj, pret)

/**
 * Get the int corresponding to the type of a given object.
 *
 * @param thisObj the object to operate against.
 * @param pret will contain the int corresponding to the type.
 */
#define SMQ_getType(obj, pret)                   Object_getType(obj, pret)

/**
 * Determines whether the given object is an instance of the given type.
 *
 * @param obj the object to check.
 * @param classtype the type to check against.
 * @param pret non-zero if the object is an instance of the type.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero i error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define SMQ_instanceof(obj, classtype, pret)     Object_instanceof(obj, classtype, pret)

/**
 * Determines whether the two objects are "equal"
 *
 * @param obj1 the first object to compare.
 * @param obj2 the second object to compare.
 * @param pret true if the objects are "equal"; false otherwise.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define SMQ_equals(obj1, obj2, pret)             Object_equals(obj1, obj2, pret)

/**
 * Get the String representation for an object.
 *
 * @param obj the object to operate against.
 * @param pret will contain the String that represents the object.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define SMQ_toString(obj, pret)                  Object_toString(obj, pret)

/*
//-------------------------------------------------------------------------
// java/lang
//-------------------------------------------------------------------------
*/

/*
//---------------------------------
// AString
//---------------------------------
*/
/**
 * Decrements the reference count of the AString object.
 *
 * @param thisObj the AString object to operate against.
 * @param pret will contain the reference count after decrementing.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define AString_release(thisObj, pret)                Object_release(thisObj, pret)

/**
 * Get the int corresponding to the type of a given object.
 *
 * @param thisObj the object to operate against.
 * @param pret will contain the int corresponding to the type.
 */
#define AString_getType(thisObj, pret)                Object_getType(thisObj, pret)

/**
 * Determines whether the given object is an instance of the given type.
 *
 * @param thisObj the object to check.
 * @param classtype the type to check against.
 * @param pret non-zero if the object is an instance of the type.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define AString_instanceof(thisObj, classtype, pret)  Object_instanceof(thisObj, classtype, pret)

/**
 * Determines whether the two AStrings refer to the same strings.
 *
 * @param thisObj the first string object to compare.
 * @param hobj2 the second string object to compare.
 * @param pret true if the AStrings refer to identical strings, false otherwise.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define AString_equals(thisObj, hobj2, pret)          Object_equals(thisObj, hobj2, pret)

/**
 * Get the String representation for this object.
 *
 * @param thisObj the AString object to operate against.
 * @param pret will contain the String that represents the object.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define AString_toString(thisObj, pret)               Object_toString(thisObj, pret)

/**
 * Returns the int corresponding to the AString type.
 *
 * @return the int corresponding to the AString type
 */
int SMQ_API	AString_type();

/**
 * Retrieve the const char* corresponding to the value of the AString.
 *
 * @param thisObj the AString object to operate against.
 * @param pret will contain the const char* corresponding to the AString value.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API AString_getValue(HOBJ thisObj, const char **pret);

/**
 * Get the length of the AString.
 *
 * @param thisObj the AString to operate against.
 * @param pret will contain the length of the AString in characters.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API AString_length(HOBJ thisObj, jint *pret);

/**
 * Get the char at a particular location in the AString. Invalid indices will trigger an error.
 *
 * @param thisObj the AString to operate against.
 * @param index the zero-based index of the character to retrieve.
 * @param pret will contain the char at the designated location in the AString.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API AString_at(HOBJ thisObj, jint index, char *pret);

/*
//---------------------------------
// Boolean
//---------------------------------
*/
/**
 * Decrements the reference count of the Boolean object.
 *
 * @param thisObj the Boolean object to operate against.
 * @param pret will contain the reference count after decrementing.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Boolean_release(thisObj, pret)                   Object_release(thisObj, pret)

/**
 * Get the int corresponding to the type of a given object.
 *
 * @param thisObj the object to operate against.
 * @param pret will contain the int corresponding to the type.
 */
#define Boolean_getType(thisObj, pret)                   Object_getType(thisObj, pret)

/**
 * Determines whether the given object is an instance of the given type.
 *
 * @param thisObj the object to check.
 * @param classtype the type to check against.
 * @param pret non-zero if the object is an instance of the type.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Boolean_instanceof(thisObj, classtype, pret)     Object_instanceof(thisObj, classtype, pret)

/**
 * Get the String representation for this Boolean object.
 *
 * @param thisObj the Boolean object to operate against.
 * @param pret will contain the String that represents the Boolean object, 
 *        i.e. "true" or "false"
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Boolean_toString(thisObj, pret)                  Object_toString(thisObj, pret)

/**
 * Determines whether the two Boolean objects are the same.
 *
 * @param thisObj the first Boolean to compare.
 * @param hobj2 the second Boolean to compare.
 * @param pret true if the Booleans refer have identical content, false otherwise.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Boolean_equals(thisObj, hobj2, pret)             Object_equals(thisObj, hobj2, pret)

/**
 * Returns the value of the given <code>Boolean</code> object as a boolean primitive.
 *
 * @param   hObj the Boolean object whose value to retrieve.
 * @param   pBool the primitive <code>boolean</code> value of this object.
 *
 * @return  SMQ_SUCCESS (0) if successful, non-zero if error.
 *          Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API Boolean_booleanValue(HOBJ hObj, jboolean *pBool);

/**
 * Returns the boolean value represented by the specified String.
 *   A new Boolean object is constructed. This Boolean represents 
 *   the value true if the string argument is not null and is 
 *   equal, ignoring case, to the string "true".
 *
 * @param   string the String object to parse.
 * @param   pBool the Boolean object represented by the string.
 *
 * @return  SMQ_SUCCESS (0) if successful, non-zero if error.
 *          Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API Boolean_valueOf(HOBJ string, HOBJ *pBool);

/**
 * Returns the int corresponding to the Boolean type.
 *
 * @return the int corresponding to the Boolean type
 */
int SMQ_API Boolean_type();

/**
 * Creates a new <code>Boolean</code> object that initially represents the 
 *   value passed in.
 *
 * @param b the initial value for the Boolean object.
 * @param pBoolean will contain a handle to the newly created Boolean object.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API Boolean_create(jboolean b, HOBJ *pBoolean);

/*
//---------------------------------
// Exception
//---------------------------------
*/
/**
 * Decrements the reference count of the Exception object.
 *
 * @param thisObj the Exception object to operate against.
 * @param pret will contain the reference count after decrementing.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Exception_release(thisObj, pret)                   Object_release(thisObj, pret)

/**
 * Get the int corresponding to the type of a given object.
 *
 * @param thisObj the object to operate against.
 * @param pret will contain the int corresponding to the type.
 */
#define Exception_getType(thisObj, pret)                   Object_getType(thisObj, pret)

/**
 * Determines whether the given object is an instance of the given type.
 *
 * @param thisObj the object to check.
 * @param classtype the type to check against.
 * @param pret non-zero if the object is an instance of the type.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Exception_instanceof(thisObj, classtype, pret)     Object_instanceof(thisObj, classtype, pret)

/**
 * Determines whether the two Exception objects are the same.
 *
 * @param thisObj the first Exception to compare.
 * @param hobj2 the second Exception to compare.
 * @param pret true if the Exceptions refer have identical content, false otherwise.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Exception_equals(thisObj, hobj2, pret)             Object_equals(thisObj, hobj2, pret)

/**
 * Get the String representation for this Exception.
 *
 * @param thisObj the Exception object to operate against.
 * @param pret will contain the String that represents the Exception.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Exception_toString(thisObj, pret)                  Object_toString(thisObj, pret)

/**
 * Retrieve the String containing the localized message associated w/ this Exception.
 *
 * @param thisObj the Exception object to operate against.
 * @param pretString will contain the String containing the localized message.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Exception_getLocalizedMessage(thisObj, pretString) Throwable_getLocalizedMessage(thisObj, pretString)

/**
 * Retrieve the String containing the message associated w/ this Exception.
 *
 * @param thisObj the Exception object to operate against.
 * @param pretString will contain the String containing the message.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Exception_getMessage(thisObj, pretString)          Throwable_getMessage(thisObj, pretString)

/**
 * Prints a stack trace w/ to the error location corresponding to the Exception object.
 *
 * @param thisObj the Exception object to operate against.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Exception_printStackTrace(thisObj)                 Throwable_printStackTrace(thisObj)

/**
 * Returns the int corresponding to the Exception type.
 *
 * @return the int corresponding to the Exception type
 */
int SMQ_API Exception_type();


/*
//---------------------------------
// RuntimeException
//---------------------------------
*/
/**
 * Decrements the reference count of the RuntimeException object.
 *
 * @param thisObj the RuntimeException object to operate against.
 * @param pret will contain the reference count after decrementing.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define RuntimeException_release(thisObj, pret)                   Object_release(thisObj, pret)

/**
 * Get the int corresponding to the type of a given object.
 *
 * @param thisObj the object to operate against.
 * @param pret will contain the int corresponding to the type.
 */
#define RuntimeException_getType(thisObj, pret)                   Object_getType(thisObj, pret)

/**
 * Determines whether the given object is an instance of the given type.
 *
 * @param thisObj the object to check.
 * @param classtype the type to check against.
 * @param pret non-zero if the object is an instance of the type.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define RuntimeException_instanceof(thisObj, classtype, pret)     Object_instanceof(thisObj, classtype, pret)

/**
 * Determines whether the two RuntimeException objects are the same.
 *
 * @param thisObj the first RuntimeException to compare.
 * @param hobj2 the second RuntimeException to compare.
 * @param pret true if the RuntimeException objects have identical content, false otherwise.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define RuntimeException_equals(thisObj, hobj2, pret)             Object_equals(thisObj, hobj2, pret)

/**
 * Get the String representation for the given RuntimeException.
 *
 * @param thisObj the RuntimeException object to operate against.
 * @param pret will contain the String that represents the RuntimeException.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define RuntimeException_toString(thisObj, pret)                  Object_toString(thisObj, pret)

/**
 * Retrieve the String containing the localized message associated w/ this RuntimeException.
 *
 * @param thisObj the RuntimeException object to operate against.
 * @param pretString will contain the String containing the localized message.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define RuntimeException_getLocalizedMessage(thisObj, pretString) Throwable_getLocalizedMessage(thisObj, pretString)

/**
 * Retrieve the String containing the message associated w/ this RuntimeException.
 *
 * @param thisObj the RuntimeException object to operate against.
 * @param pretString will contain the String containing the message.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define RuntimeException_getMessage(thisObj, pretString)          Throwable_getMessage(thisObj, pretString)

/**
 * Prints a stack trace w/ to the error location corresponding to the RuntimeException object.
 *
 * @param thisObj the RuntimeException object to operate against.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define RuntimeException_printStackTrace(thisObj)                 Throwable_printStackTrace(thisObj)

/**
 * Returns the int corresponding to the RuntimeException type.
 *
 * @return the int corresponding to the RuntimeException type
 */
int SMQ_API RuntimeException_type();


/*
//---------------------------------
// ClassCastException
//---------------------------------
*/
/**
 * Decrements the reference count of the ClassCastException object.
 *
 * @param thisObj the ClassCastException object to operate against.
 * @param pret will contain the reference count after decrementing.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define ClassCastException_release(thisObj, pret)                   Object_release(thisObj, pret)

/**
 * Get the int corresponding to the type of a given object.
 *
 * @param thisObj the object to operate against.
 * @param pret will contain the int corresponding to the type.
 */
#define ClassCastException_getType(thisObj, pret)                   Object_getType(thisObj, pret)

/**
 * Determines whether the given object is an instance of the given type.
 *
 * @param thisObj the object to check.
 * @param classtype the type to check against.
 * @param pret non-zero if the object is an instance of the type.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define ClassCastException_instanceof(thisObj, classtype, pret)     Object_instanceof(thisObj, classtype, pret)

/**
 * Determines whether the two ClassCastException objects are the same.
 *
 * @param thisObj the first ClassCastException to compare.
 * @param hobj2 the second ClassCastException to compare.
 * @param pret true if the ClassCastException objects have identical content, false otherwise.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define ClassCastException_equals(thisObj, hobj2, pret)             Object_equals(thisObj, hobj2, pret)

/**
 * Get the String representation for the given ClassCastException.
 *
 * @param thisObj the ClassCastException object to operate against.
 * @param pret will contain the String that represents the ClassCastException.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define ClassCastException_toString(thisObj, pret)                  Object_toString(thisObj, pret)

/**
 * Retrieve the String containing the localized message associated w/ this ClassCastException.
 *
 * @param thisObj the ClassCastException object to operate against.
 * @param pretString will contain the String containing the localized message.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define ClassCastException_getLocalizedMessage(thisObj, pretString) Throwable_getLocalizedMessage(thisObj, pretString)

/**
 * Retrieve the String containing the message associated w/ this ClassCastException.
 *
 * @param thisObj the ClassCastException object to operate against.
 * @param pretString will contain the String containing the message.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define ClassCastException_getMessage(thisObj, pretString)          Throwable_getMessage(thisObj, pretString)

/**
 * Prints a stack trace w/ to the error location corresponding to the ClassCastException object.
 *
 * @param thisObj the ClassCastException object to operate against.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define ClassCastException_printStackTrace(thisObj)                 Throwable_printStackTrace(thisObj)

/**
 * Returns the int corresponding to the ClassCastException type.
 *
 * @return the int corresponding to the ClassCastException type
 */
int SMQ_API ClassCastException_type();


/*
//---------------------------------
// CloneNotSupportedException
//---------------------------------
*/
/**
 * Decrements the reference count of the CloneNotSupportedException object.
 *
 * @param thisObj the CloneNotSupportedException object to operate against.
 * @param pret will contain the reference count after decrementing.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define CloneNotSupportedException_release(thisObj, pret)                   Object_release(thisObj, pret)

/**
 * Get the int corresponding to the type of a given object.
 *
 * @param thisObj the object to operate against.
 * @param pret will contain the int corresponding to the type.
 */
#define CloneNotSupportedException_getType(thisObj, pret)                   Object_getType(thisObj, pret)

/**
 * Determines whether the given object is an instance of the given type.
 *
 * @param thisObj the object to check.
 * @param classtype the type to check against.
 * @param pret non-zero if the object is an instance of the type.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define CloneNotSupportedException_instanceof(thisObj, classtype, pret)     Object_instanceof(thisObj, classtype, pret)

/**
 * Determines whether the two CloneNotSupportedException objects are the same.
 *
 * @param thisObj the first CloneNotSupportedException to compare.
 * @param hobj2 the second CloneNotSupportedException to compare.
 * @param pret true if the CloneNotSupportedException objects have identical content, false otherwise.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define CloneNotSupportedException_equals(thisObj, hobj2, pret)             Object_equals(thisObj, hobj2, pret)

/**
 * Get the String representation for the given CloneNotSupportedException.
 *
 * @param thisObj the CloneNotSupportedException object to operate against.
 * @param pret will contain the String that represents the CloneNotSupportedException.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define CloneNotSupportedException_toString(thisObj, pret)                  Object_toString(thisObj, pret)

/**
 * Retrieve the String containing the localized message associated w/ this CloneNotSupportedException.
 *
 * @param thisObj the CloneNotSupportedException object to operate against.
 * @param pretString will contain the String containing the localized message.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define CloneNotSupportedException_getLocalizedMessage(thisObj, pretString) Throwable_getLocalizedMessage(thisObj, pretString)

/**
 * Retrieve the String containing the message associated w/ this CloneNotSupportedException.
 *
 * @param thisObj the CloneNotSupportedException object to operate against.
 * @param pretString will contain the String containing the message.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define CloneNotSupportedException_getMessage(thisObj, pretString)          Throwable_getMessage(thisObj, pretString)

/**
 * Prints a stack trace w/ to the error location corresponding to the CloneNotSupportedException object.
 *
 * @param thisObj the CloneNotSupportedException object to operate against.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define CloneNotSupportedException_printStackTrace(thisObj)                 Throwable_printStackTrace(thisObj)

/**
 * Returns the int corresponding to the CloneNotSupportedException type.
 *
 * @return the int corresponding to the CloneNotSupportedException type
 */
int SMQ_API CloneNotSupportedException_type();


/*
//---------------------------------
// NullPointerException
//---------------------------------
*/
/**
 * Decrements the reference count of the NullPointerException object.
 *
 * @param thisObj the NullPointerException object to operate against.
 * @param pret will contain the reference count after decrementing.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define NullPointerException_release(thisObj, pret)                   Object_release(thisObj, pret)

/**
 * Get the int corresponding to the type of a given object.
 *
 * @param thisObj the object to operate against.
 * @param pret will contain the int corresponding to the type.
 */
#define NullPointerException_getType(thisObj, pret)                   Object_getType(thisObj, pret)

/**
 * Determines whether the given object is an instance of the given type.
 *
 * @param thisObj the object to check.
 * @param classtype the type to check against.
 * @param pret non-zero if the object is an instance of the type.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define NullPointerException_instanceof(thisObj, classtype, pret)     Object_instanceof(thisObj, classtype, pret)

/**
 * Determines whether the two NullPointerException objects are the same.
 *
 * @param thisObj the first NullPointerException to compare.
 * @param hobj2 the second NullPointerException to compare.
 * @param pret true if the NullPointerExceptions refer have identical content, false otherwise.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define NullPointerException_equals(thisObj, hobj2, pret)             Object_equals(thisObj, hobj2, pret)

/**
 * Get the String representation for this NullPointerException.
 *
 * @param thisObj the NullPointerException object to operate against.
 * @param pret will contain the String that represents the NullPointerException.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define NullPointerException_toString(thisObj, pret)                  Object_toString(thisObj, pret)

/**
 * Retrieve the String containing the localized message associated w/ this NullPointerException.
 *
 * @param thisObj the NullPointerException object to operate against.
 * @param pretString will contain the String containing the localized message.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define NullPointerException_getLocalizedMessage(thisObj, pretString) Throwable_getLocalizedMessage(thisObj, pretString)

/**
 * Retrieve the String containing the message associated w/ this NullPointerException.
 *
 * @param thisObj the NullPointerException object to operate against.
 * @param pretString will contain the String containing the message.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define NullPointerException_getMessage(thisObj, pretString)          Throwable_getMessage(thisObj, pretString)

/**
 * Prints a stack trace w/ to the error location corresponding to the NullPointerException object.
 *
 * @param thisObj the NullPointerException object to operate against.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define NullPointerException_printStackTrace(thisObj)                 Throwable_printStackTrace(thisObj)

/**
 * Returns the int corresponding to the NullPointerException type.
 *
 * @return the int corresponding to the NullPointerException type
 */
int SMQ_API NullPointerException_type();


/*
//---------------------------------
// jbyteArray
//---------------------------------
*/
/**
 * Decrements the reference count of the jbyteArray object.
 *
 * @param thisObj the jbyteArray object to operate against.
 * @param pret will contain the reference count after decrementing.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define jbyteArray_release(thisObj, pret)                Object_release(thisObj, pret)

/**
 * Get the int corresponding to the type of a given object.
 *
 * @param thisObj the object to operate against.
 * @param pret will contain the int corresponding to the type.
 */
#define jbyteArray_getType(thisObj, pret)                Object_getType(thisObj, pret)

/**
 * Determines whether the given object is an instance of the given type.
 *
 * @param thisObj the object to check.
 * @param classtype the type to check against.
 * @param pret non-zero if the object is an instance of the type.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define jbyteArray_instanceof(thisObj, classtype, pret)  Object_instanceof(thisObj, classtype, pret)

/**
 * Determines whether the two jbyteArray objects are the same.
 *
 * @param thisObj the first jbyteArray to compare.
 * @param hobj2 the second jbyteArray to compare.
 * @param pret true if the jbyteArray objects have identical content, false otherwise.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define jbyteArray_equals(thisObj, hobj2, pret)          Object_equals(thisObj, hobj2, pret)

/**
 * Get the String representation for the given jbyteArray.
 *
 * @param thisObj the jbyteArray object to operate against.
 * @param pret will contain the String that represents the jbyteArray.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define jbyteArray_toString(thisObj, pret)               Object_toString(thisObj, pret)

/**
 * Create a new jbyteArray that is a copy of the given jbyteArray.
 *
 * @param thisObj the jbyteArray to clone.
 * @param pret will contain the newly created jbyteArray.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define jbyteArray_clone(thisObj, pret)                  Object_clone(thisObj, pret)

/**
 * Returns the int corresponding to the jbyteArray type.
 *
 * @return the int corresponding to the jbyteArray type
 */
int SMQ_API jbyteArray_type();

/**
 * Set an HOBJ to the jbyteArray referred to by another HOBJ.
 *
 * @param thisObj the jbyteArray object to set.
 * @param hobj2 the jbyteArray object to assign from.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API jbyteArray_assign(HOBJ thisObj, HOBJ hobj2);

/**
 * Set one byte of a jbyteArray.
 *
 * @param thisObj the jbyteArray object to operate against.
 * @param index the location of the jbyteArray element to set.
 * @param value the value to set the appropriate jbyteArray element to.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API jbyteArray_assignAt(HOBJ thisObj, jint index, jbyte value);

/**
 * Retrieve one byte from a jbyteArray.
 *
 * @param thisObj the jbyteArray object to operate against.
 * @param index the location of the jbyteArray element to set.
 * @param pret will contain the value of the appropriate jbyteArray element.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API jbyteArray_at(HOBJ thisObj, jint index, jbyte *pret);

/**
 * Returns the length of the jbyteArray.
 *
 * @param thisObj the jbyteArray object to operate against.
 * @param pret will contain the length of the jbyteArray.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API jbyteArray_length(HOBJ thisObj, jint *pret);

/**
 * Create a new empty jbyteArray.
 *
 * @param size the desired size of the jbyteArray.
 * @param pret will contain the HOBJ referring to the newly created jbyteArray.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API jbyteArray_create(jint size, HOBJ *pret);

/**
 * Create a new jbyteArray with initialization data.
 *
 * @param data the data used to initialize the jbyteArray.
 * @param size the desired size of the jbyteArray.
 * @param pret will contain the HOBJ referring to the newly created jbyteArray.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API jbyteArray_create2(jbyte *data, jint size, HOBJ *pret);


/**
 * Get a reference to the array data.  This is a raw pointer to the internal
 * data.  Bounds checking is up to the user.  It should not be freed.
 *
 * @param hobj the jbyteArray.
 * @param pret Pointer to the data contained by the jbyteArray.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API jbyteArray_getArray(HOBJ hobj, jbyte **pret);

/*
//---------------------------------
// jcharArray
//---------------------------------
*/
/**
 * Decrements the reference count of the jcharArray object.
 *
 * @param thisObj the jcharArray object to operate against.
 * @param pret will contain the reference count after decrementing.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define jcharArray_release(thisObj, pret)                Object_release(thisObj, pret)

/**
 * Get the int corresponding to the type of a given object.
 *
 * @param thisObj the object to operate against.
 * @param pret will contain the int corresponding to the type.
 */
#define jcharArray_getType(thisObj, pret)                Object_getType(thisObj, pret)

/**
 * Determines whether the given object is an instance of the given type.
 *
 * @param thisObj the object to check.
 * @param classtype the type to check against.
 * @param pret non-zero if the object is an instance of the type.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define jcharArray_instanceof(thisObj, classtype, pret)  Object_instanceof(thisObj, classtype, pret)

/**
 * Determines whether the two jcharArray objects are the same.
 *
 * @param thisObj the first jcharArray to compare.
 * @param hobj2 the second jcharArray to compare.
 * @param pret true if the jcharArray objects have identical content, false otherwise.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define jcharArray_equals(thisObj, hobj2, pret)          Object_equals(thisObj, hobj2, pret)

/**
 * Get the String representation for the given jcharArray.
 *
 * @param thisObj the jcharArray object to operate against.
 * @param pret will contain the String that represents the jcharArray.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define jcharArray_toString(thisObj, pret)               Object_toString(thisObj, pret)

/**
 * Create a new jcharArray that is a copy of the given jcharArray.
 *
 * @param thisObj the jcharArray to clone.
 * @param pret will contain the newly created jcharArray.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define jcharArray_clone(thisObj, pret)                  Object_clone(thisObj, pret)

/**
 * Returns the int corresponding to the jcharArray type.
 *
 * @return the int corresponding to the jcharArray type
 */
int SMQ_API jcharArray_type();

/**
 * Set an HOBJ to the jcharArray referred to by another HOBJ.
 *
 * @param thisObj the jcharArray object to set.
 * @param hobj2 the jcharArray object to assign from.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API jcharArray_assign(HOBJ thisObj, HOBJ hobj2);

/**
 * Set one char of a jcharArray.
 *
 * @param thisObj the jcharArray object to operate against.
 * @param index the location of the jcharArray element to set.
 * @param value the value to set the appropriate jcharArray element to.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API jcharArray_assignAt(HOBJ thisObj, jint index, jchar value);

/**
 * Retrieve one char from a jcharArray.
 *
 * @param thisObj the jcharArray object to operate against.
 * @param index the location of the jcharArray element to set.
 * @param pret will contain the value of the appropriate jcharArray element.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API jcharArray_at(HOBJ thisObj, jint index, jchar *pret);

/**
 * Returns the length of the jcharArray.
 *
 * @param thisObj the jcharArray object to operate against.
 * @param pret will contain the length of the jcharArray.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API jcharArray_length(HOBJ thisObj, jint *pret);

/**
 * Create a new empty jcharArray.
 *
 * @param size the desired size of the jcharArray.
 * @param pret will contain the HOBJ referring to the newly created jcharArray.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API jcharArray_create(jint size, HOBJ *pret);

/**
 * Create a new jcharArray populated w/ the contents of a jchar[].
 *
 * @param array a jchar* with which to populate the newly created jcharArray.
 * @param pret will contain the HOBJ referring to the newly created jcharArray.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API jcharArray_create2(jchar *array, HOBJ *pret);
 
/**
 * Create a new jcharArray with initialization data.
 *
 * @param data the data used to initialize the jcharArray.
 * @param size the desired size of the jcharArray.
 * @param pret will contain the HOBJ referring to the newly created jbyteArray.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API jcharArray_create3(jchar *data, jint size, HOBJ *pret);

/**
 * Get a reference to the array data.  This is a raw pointer to the internal
 * data.  Bounds checking is up to the user.  It should not be freed.
 *
 * @param hobj the jcharArray.
 * @param pret Pointer to the data contained by the jcharArray.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API jcharArray_getArray(HOBJ hobj, jchar **pret);

/*
//---------------------------------
// Object
//---------------------------------
*/

/**
 * Returns the int corresponding to the Object type.
 *
 * @return the int corresponding to the Object type
 */
int SMQ_API Object_type();

/**
 * Decrements the reference count of the Object.
 *
 * @param thisObj the object to operate against.
 * @param pret will contain the reference count after decrementing.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API Object_release(HOBJ thisObj, long *pret);

/**
 * Get the int corresponding to the type of a given object.
 *
 * @param thisObj the object to operate against.
 * @param pret will contain the int corresponding to the type.
 */
int SMQ_API Object_getType(HOBJ thisObj, int *pret);

/**
 * Determines whether the given object is an instance of the given type.
 *
 * @param thisObj the object to check.
 * @param classtype the type to check against.
 * @param pret non-zero if the object is an instance of the type.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API Object_instanceof(HOBJ thisObj, int classtype, int *pret);

/**
 * Determines whether the two Objects have the same content.
 *
 * @param thisObj the first object to compare.
 * @param hobj2 the second object to compare.
 * @param pret true if the Objects have identical content, false otherwise.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API Object_equals(HOBJ thisObj, HOBJ hobj2, jboolean *pret);

/**
 * Get the String representation for this object.
 *
 * @param thisObj the object to operate against.
 * @param pret will contain the String that represents the object.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API Object_toString(HOBJ thisObj, HOBJ *pret);

/**
 * Create a new Object that is a copy of the given Object.
 *
 * @param thisObj the object to clone.
 * @param pret will contain the newly created Object.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API Object_clone(HOBJ thisObj, HOBJ *pret);

/*
//---------------------------------
// IllegalArgumentException
//---------------------------------
*/
/**
 * Decrements the reference count of the IllegalArgumentException object.
 *
 * @param thisObj the IllegalArgumentException object to operate against.
 * @param pret will contain the reference count after decrementing.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define IllegalArgumentException_release(thisObj, pret)                Object_release(thisObj, pret)

/**
 * Get the int corresponding to the type of a given object.
 *
 * @param thisObj the object to operate against.
 * @param pret will contain the int corresponding to the type.
 */
#define IllegalArgumentException_getType(thisObj, pret)                Object_getType(thisObj, pret)

/**
 * Determines whether the given object is an instance of the given type.
 *
 * @param thisObj the object to check.
 * @param classtype the type to check against.
 * @param pret non-zero if the object is an instance of the type.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define IllegalArgumentException_instanceof(thisObj, classtype, pret)  Object_instanceof(thisObj, classtype, pret)

/**
 * Determines whether the two IllegalArgumentException objects are the same.
 *
 * @param thisObj the first IllegalArgumentException to compare.
 * @param hobj2 the second IllegalArgumentException to compare.
 * @param pret true if the IllegalArgumentException objects have identical content, false otherwise.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define IllegalArgumentException_equals(thisObj, hobj2, pret)          Object_equals(thisObj, hobj2, pret)

/**
 * Get the String representation for the given IllegalArgumentException.
 *
 * @param thisObj the IllegalArgumentException object to operate against.
 * @param pret will contain the String that represents the IllegalArgumentException.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define IllegalArgumentException_toString(thisObj, pret)               Object_toString(thisObj, pret)

/**
 * Retrieve the String containing the localized message associated w/ this IllegalArgumentException.
 *
 * @param thisObj the IllegalArgumentException object to operate against.
 * @param pretString will contain the String containing the localized message.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define IllegalArgumentException_getLocalizedMessage(thisObj, pretString) Throwable_getLocalizedMessage(thisObj, pretString)

/**
 * Retrieve the String containing the message associated w/ this IllegalArgumentException.
 *
 * @param thisObj the IllegalArgumentException object to operate against.
 * @param pretString will contain the String containing the message.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define IllegalArgumentException_getMessage(thisObj, pretString)       Throwable_getMessage(thisObj, pretString)

/**
 * Prints a stack trace w/ to the error location corresponding to the IllegalArgumentException object.
 *
 * @param thisObj the IllegalArgumentException object to operate against.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define IllegalArgumentException_printStackTrace(thisObj)              Throwable_printStackTrace(thisObj)

/**
 * Returns the int corresponding to the IllegalArgumentException type.
 *
 * @return the int corresponding to the IllegalArgumentException type
 */
int SMQ_API IllegalArgumentException_type();

/*
//---------------------------------
// Number
//---------------------------------
*/
/**
 * Decrements the reference count of the Number object.
 *
 * @param thisObj the Number object to operate against.
 * @param pret will contain the reference count after decrementing.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Number_release(thisObj, pret)                   Object_release(thisObj, pret)

/**
 * Get the int corresponding to the type of a given object.
 *
 * @param thisObj the object to operate against.
 * @param pret will contain the int corresponding to the type.
 */
#define Number_getType(thisObj, pret)                   Object_getType(thisObj, pret)

/**
 * Determines whether the given object is an instance of the given type.
 *
 * @param thisObj the object to check.
 * @param classtype the type to check against.
 * @param pret non-zero if the object is an instance of the type.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Number_instanceof(thisObj, classtype, pret)     Object_instanceof(thisObj, classtype, pret)

/**
 * Returns the value of this <code>Number</code> object as a jbyte.
 *
 * @param   hObj the Number object to operate against.
 * @param   pByte the jbyte value represented by this object.
 *
 * @return  SMQ_SUCCESS (0) if successful, non-zero if error.
 *          Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API Number_byteValue(HOBJ hObj, jbyte *pByte);

/**
 * Returns the value of this <code>Number</code> object as a jdouble.
 *
 * @param   hObj the Number object to operate against.
 * @param   pDouble the jdouble value represented by this object.
 *
 * @return  SMQ_SUCCESS (0) if successful, non-zero if error.
 *          Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API Number_doubleValue(HOBJ hObj, jdouble *pDouble);

/**
 * Returns the value of this <code>Number</code> object as a jfloat.
 *
 * @param   hObj the Number object to operate against.
 * @param   pFloat the jfloat value represented by this object.
 *
 * @return  SMQ_SUCCESS (0) if successful, non-zero if error.
 *          Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API Number_floatValue(HOBJ hObj, jfloat *pFloat);

/**
 * Returns the value of this <code>Number</code> object as a jint.
 *
 * @param   hObj the Number object to operate against.
 * @param   pInt the jint value represented by this object.
 *
 * @return  SMQ_SUCCESS (0) if successful, non-zero if error.
 *          Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API Number_intValue(HOBJ hObj, jint *pInt);

/**
 * Returns the value of this <code>Number</code> object as a jlong.
 *
 * @param   hObj the Number object to operate against.
 * @param   pLong the jlong value represented by this object.
 *
 * @return  SMQ_SUCCESS (0) if successful, non-zero if error.
 *          Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API Number_longValue(HOBJ hObj, jlong *pLong);

/**
 * Returns the value of this <code>Number</code> object as a jshort.
 *
 * @param   hObj the Number object to operate against.
 * @param   pShort the jshort value represented by this object.
 *
 * @return  SMQ_SUCCESS (0) if successful, non-zero if error.
 *          Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API Number_shortValue(HOBJ hObj, jshort *pShort);

/**
 * Returns the int corresponding to the Number type.
 *
 * @return the int corresponding to the Number type
 */
int SMQ_API Number_type();

/*
//---------------------------------
// Byte
//---------------------------------
*/
/**
 * Decrements the reference count of the Byte object.
 *
 * @param thisObj the String object to operate against.
 * @param pret will contain the reference count after decrementing.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Byte_release(thisObj, pret)                   Object_release(thisObj, pret)

/**
 * Get the int corresponding to the type of a given object.
 *
 * @param thisObj the object to operate against.
 * @param pret will contain the int corresponding to the type.
 */
#define Byte_getType(thisObj, pret)                   Object_getType(thisObj, pret)

/**
 * Determines whether the given object is an instance of the given type.
 *
 * @param thisObj the object to check.
 * @param classtype the type to check against.
 * @param pret non-zero if the object is an instance of the type.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Byte_instanceof(thisObj, classtype, pret)     Object_instanceof(thisObj, classtype, pret)

/**
 * Get the String representation for the given Byte.
 *
 * @param thisObj the object to operate against.
 * @param pret will contain the String that represents the Byte.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Byte_toString(thisObj, pret)                 Object_toString(thisObj, pret)

/**
 * Determines whether the two Byte objects are the same.
 *
 * @param thisObj the first Byte to compare.
 * @param hobj2 the second Byte to compare.
 * @param pret true if the Byte objects have identical content, false otherwise.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Byte_equals(thisObj, hobj2, pret)            Object_equals(thisObj, hobj2, pret)

/**
 * Returns the value of this <code>Byte</code> object as a jbyte.
 *
 * @param   hObj the Byte object to operate against.
 * @param   pByte the jbyte value represented by this object.
 *
 * @return  SMQ_SUCCESS (0) if successful, non-zero if error.
 *          Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Byte_byteValue(hObj, pByte)                  Number_byteValue(hObj, pByte)

/**
 * Returns the value of this <code>Byte</code> object as a jdouble.
 *
 * @param   hObj the Byte object to operate against.
 * @param   pDouble the jdouble value represented by this object.
 *
 * @return  SMQ_SUCCESS (0) if successful, non-zero if error.
 *          Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Byte_doubleValue(hObj, pDouble)              Number_doubleValue(hObj, pDouble)

/**
 * Returns the value of this <code>Byte</code> object as a jfloat.
 *
 * @param   hObj the Byte object to operate against.
 * @param   pFloat the jfloat value represented by this object.
 *
 * @return  SMQ_SUCCESS (0) if successful, non-zero if error.
 *          Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Byte_floatValue(hObj, pFloat)                Number_floatValue(hObj, pFloat)

/**
 * Returns the value of this <code>Byte</code> object as a jint.
 *
 * @param   hObj the Byte object to operate against.
 * @param   pInt the jint value represented by this object.
 *
 * @return  SMQ_SUCCESS (0) if successful, non-zero if error.
 *          Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Byte_intValue(hObj, pInt)                    Number_intValue(hObj, pInt)

/**
 * Returns the value of this <code>Byte</code> object as a jlong.
 *
 * @param   hObj the Byte object to operate against.
 * @param   pLong the jlong value represented by this object.
 *
 * @return  SMQ_SUCCESS (0) if successful, non-zero if error.
 *          Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Byte_longValue(hObj, pLong)                  Number_longValue(hObj, pLong)

/**
 * Returns the value of this <code>Byte</code> object as a jshort.
 *
 * @param   hObj the Byte object to operate against.
 * @param   pShort the jshort value represented by this object.
 *
 * @return  SMQ_SUCCESS (0) if successful, non-zero if error.
 *          Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Byte_shortValue(hObj, pShort)                Number_shortValue(hObj, pShort)

/**
 * Assuming the specified String represents a byte, returns a new 
 *  Byte object initialized to that value. Throws an exception if the 
 *  String cannot be parsed as a byte. The radix is assumed to be 10.
 *
 * @param   string the String object containing the byte value.
 * @param   pByte the newly created Byte object represented by the string.
 *
 * @return  SMQ_SUCCESS (0) if successful, non-zero if error.
 *          Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API Byte_valueOf(HOBJ string, HOBJ *pByte);

/**
 * Returns the int corresponding to the Byte type.
 *
 * @return the int corresponding to the Byte type
 */
int SMQ_API Byte_type();

/**
 * Creates a new <code>Byte</code> object that initially represents the 
 *   value passed in.
 *
 * @param b the initial value for the Byte object.
 * @param pByte will contain a handle to the newly created Byte object.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API Byte_create(jbyte b, HOBJ *pByte);

/*
//---------------------------------
// Integer
//---------------------------------
*/
/**
 * Decrements the reference count of the Integer object.
 *
 * @param thisObj the Integer object to operate against.
 * @param pret will contain the reference count after decrementing.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Integer_release(thisObj, pret)                   Object_release(thisObj, pret)

/**
 * Get the int corresponding to the type of a given object.
 *
 * @param thisObj the object to operate against.
 * @param pret will contain the int corresponding to the type.
 */
#define Integer_getType(thisObj, pret)                   Object_getType(thisObj, pret)

/**
 * Determines whether the given object is an instance of the given type.
 *
 * @param thisObj the object to check.
 * @param classtype the type to check against.
 * @param pret non-zero if the object is an instance of the type.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Integer_instanceof(thisObj, classtype, pret)     Object_instanceof(thisObj, classtype, pret)

/**
 * Get the String representation for the given Integer.
 *
 * @param thisObj the Integer object to operate against.
 * @param pret will contain the String that represents the Integer.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Integer_toString(thisObj, pret)                 Object_toString(thisObj, pret)

/**
 * Determines whether the two Integer objects are the same.
 *
 * @param thisObj the first Integer to compare.
 * @param hobj2 the second Integer to compare.
 * @param pret true if the Integer objects have identical content, false otherwise.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Integer_equals(thisObj, hobj2, pret)            Object_equals(thisObj, hobj2, pret)

/**
 * Returns the value of this <code>Integer</code> object as a jbyte.
 *
 * @param   hObj the Integer object to operate against.
 * @param   pByte the jbyte value represented by this object.
 *
 * @return  SMQ_SUCCESS (0) if successful, non-zero if error.
 *          Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Integer_byteValue(hObj, pByte)                  Number_byteValue(hObj, pByte)

/**
 * Returns the value of this <code>Integer</code> object as a jdouble.
 *
 * @param   hObj the Integer object to operate against.
 * @param   pDouble the jdouble value represented by this object.
 *
 * @return  SMQ_SUCCESS (0) if successful, non-zero if error.
 *          Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Integer_doubleValue(hObj, pDouble)              Number_doubleValue(hObj, pDouble)

/**
 * Returns the value of this <code>Integer</code> object as a jfloat.
 *
 * @param   hObj the Integer object to operate against.
 * @param   pFloat the jfloat value represented by this object.
 *
 * @return  SMQ_SUCCESS (0) if successful, non-zero if error.
 *          Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Integer_floatValue(hObj, pFloat)                Number_floatValue(hObj, pFloat)

/**
 * Returns the value of this <code>Integer</code> object as a jint.
 *
 * @param   hObj the Integer object to operate against.
 * @param   pInt the jint value represented by this object.
 *
 * @return  SMQ_SUCCESS (0) if successful, non-zero if error.
 *          Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Integer_intValue(hObj, pInt)                    Number_intValue(hObj, pInt)

/**
 * Returns the value of this <code>Integer</code> object as a jlong.
 *
 * @param   hObj the Integer object to operate against.
 * @param   pLong the jlong value represented by this object.
 *
 * @return  SMQ_SUCCESS (0) if successful, non-zero if error.
 *          Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Integer_longValue(hObj, pLong)                  Number_longValue(hObj, pLong)

/**
 * Returns the value of this <code>Integer</code> object as a jshort.
 *
 * @param   hObj the Integer object to operate against.
 * @param   pShort the jshort value represented by this object.
 *
 * @return  SMQ_SUCCESS (0) if successful, non-zero if error.
 *          Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Integer_shortValue(hObj, pShort)                Number_shortValue(hObj, pShort)

/**
 * Creates a string representation of the integer argument as an 
 *  unsigned integer in base 16.
 *
 * @param   i an integer
 * @param   pString the String representation of the unsigned integer value 
 *          represented by the argument in hexadecimal (base 16).
 *
 * @return  SMQ_SUCCESS (0) if successful, non-zero if error.
 *          Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API Integer_toHexString(jint i, HOBJ *pString);

/**
 * Returns a jint w/ the value represented by the specified String, 
 *  as performed by the valueOf method of class Integer.
 *
 * @param   string the string object to be parsed.
 * @param   pInt the jint value represented by the string argument.
 *
 * @return  SMQ_SUCCESS (0) if successful, non-zero if error.
 *          Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API Integer_parseInt(HOBJ string, jint *pInt);

/**
 * Returns a new Integer object initialized to the value represented by the 
 *  specified string.
 *
 * @param   string the String object containing the string to be parsed.
 * @param   pInt a newly constructed Integer object initialized to the value represented 
 *          by the string argument.
 *
 * @return  SMQ_SUCCESS (0) if successful, non-zero if error.
 *          Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API Integer_valueOf(HOBJ string, HOBJ *pInt);

/**
 * Returns the int corresponding to the Integer type.
 *
 * @return the int corresponding to the Integer type
 */
int SMQ_API Integer_type();

/**
 * Creates a new <code>Integer</code> object that initially represents the 
 *   value passed in.
 *
 * @param i the initial value for the Integer object.
 * @param pInteger will contain a handle to the newly created Integer object.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API Integer_create(jint i, HOBJ *pInteger);

/*
//---------------------------------
// Long
//---------------------------------
*/
/**
 * Decrements the reference count of the Long object.
 *
 * @param thisObj the Long object to operate against.
 * @param pret will contain the reference count after decrementing.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Long_release(thisObj, pret)                   Object_release(thisObj, pret)

/**
 * Get the int corresponding to the type of a given object.
 *
 * @param thisObj the object to operate against.
 * @param pret will contain the int corresponding to the type.
 */
#define Long_getType(thisObj, pret)                   Object_getType(thisObj, pret)

/**
 * Determines whether the given object is an instance of the given type.
 *
 * @param thisObj the object to check.
 * @param classtype the type to check against.
 * @param pret non-zero if the object is an instance of the type.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Long_instanceof(thisObj, classtype, pret)     Object_instanceof(thisObj, classtype, pret)

/**
 * Get the String representation for the given Long.
 *
 * @param thisObj the Long object to operate against.
 * @param pret will contain the String that represents the Long.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Long_toString(thisObj, pret)                 Object_toString(thisObj, pret)

/**
 * Determines whether the two Long objects are the same.
 *
 * @param thisObj the first Long to compare.
 * @param hobj2 the second Long to compare.
 * @param pret true if the Long objects have identical content, false otherwise.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Long_equals(thisObj, hobj2, pret)            Object_equals(thisObj, hobj2, pret)

/**
 * Returns the value of this <code>Long</code> object as a jbyte.
 *
 * @param   hObj the Long object to operate against.
 * @param   pByte the jbyte value represented by this object.
 *
 * @return  SMQ_SUCCESS (0) if successful, non-zero if error.
 *          Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Long_byteValue(hObj, pByte)                  Number_byteValue(hObj, pByte)

/**
 * Returns the value of this <code>Long</code> object as a jdouble.
 *
 * @param   hObj the Long object to operate against.
 * @param   pDouble the jdouble value represented by this object.
 *
 * @return  SMQ_SUCCESS (0) if successful, non-zero if error.
 *          Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Long_doubleValue(hObj, pDouble)              Number_doubleValue(hObj, pDouble)

/**
 * Returns the value of this <code>Long</code> object as a jfloat.
 *
 * @param   hObj the Long object to operate against.
 * @param   pFloat the jfloat value represented by this object.
 *
 * @return  SMQ_SUCCESS (0) if successful, non-zero if error.
 *          Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Long_floatValue(hObj, pFloat)                Number_floatValue(hObj, pFloat)

/**
 * Returns the value of this <code>Long</code> object as a jint.
 *
 * @param   hObj the Long object to operate against.
 * @param   pInt the jint value represented by this object.
 *
 * @return  SMQ_SUCCESS (0) if successful, non-zero if error.
 *          Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Long_intValue(hObj, pInt)                    Number_intValue(hObj, pInt)

/**
 * Returns the value of this <code>Long</code> object as a jlong.
 *
 * @param   hObj the Long object to operate against.
 * @param   pLong the jlong value represented by this object.
 *
 * @return  SMQ_SUCCESS (0) if successful, non-zero if error.
 *          Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Long_longValue(hObj, pLong)                  Number_longValue(hObj, pLong)

/**
 * Returns the value of this <code>Long</code> object as a jshort.
 *
 * @param   hObj the Long object to operate against.
 * @param   pShort the jshort value represented by this object.
 *
 * @return  SMQ_SUCCESS (0) if successful, non-zero if error.
 *          Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Long_shortValue(hObj, pShort)                Number_shortValue(hObj, pShort)

/**
 * Creates a string representation of the jlong argument as an 
 *  unsigned integer in base 16.
 *
 * @param   l a jlong
 * @param   pString the String representation of the unsigned long value 
 *          represented by the argument in hexadecimal (base 16).
 *
 * @return  SMQ_SUCCESS (0) if successful, non-zero if error.
 *          Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API Long_toHexString(jlong l, HOBJ *pString);

/**
 * Returns a jlong w/ the value represented by the specified String, 
 *  as performed by the valueOf method of class Long.
 *
 * @param   string the string to be parsed.
 * @param   pLong the jlong value represented by the string argument.
 *
 * @return  SMQ_SUCCESS (0) if successful, non-zero if error.
 *          Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API Long_parseLong(HOBJ string, jlong *pLong);

/**
 * Returns a new Long object initialized to the value represented by the 
 *  specified string.
 *
 * @param   string the String object containing the string to be parsed.
 * @param   pLong a newly constructed Long object initialized to the value represented 
 *          by the string argument.
 *
 * @return  SMQ_SUCCESS (0) if successful, non-zero if error.
 *          Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API Long_valueOf(HOBJ string, HOBJ *pLong);

/**
 * Returns the int corresponding to the Long type.
 *
 * @return the int corresponding to the Long type
 */
int SMQ_API Long_type();

/**
 * Creates a new <code>Long</code> object that initially represents the 
 *   value passed in.
 *
 * @param l the initial value for the Long object.
 * @param pLong will contain a handle to the newly created Long object.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API Long_create(jlong l, HOBJ *pLong);

/*
//---------------------------------
// Short
//---------------------------------
*/
/**
 * Decrements the reference count of the Short object.
 *
 * @param thisObj the Short object to operate against.
 * @param pret will contain the reference count after decrementing.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Short_release(thisObj, pret)                   Object_release(thisObj, pret)

/**
 * Get the int corresponding to the type of a given object.
 *
 * @param thisObj the object to operate against.
 * @param pret will contain the int corresponding to the type.
 */
#define Short_getType(thisObj, pret)                   Object_getType(thisObj, pret)

/**
 * Determines whether the given object is an instance of the given type.
 *
 * @param thisObj the object to check.
 * @param classtype the type to check against.
 * @param pret non-zero if the object is an instance of the type.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Short_instanceof(thisObj, classtype, pret)     Object_instanceof(thisObj, classtype, pret)

/**
 * Get the String representation for the given Short.
 *
 * @param thisObj the Short object to operate against.
 * @param pret will contain the String that represents the Short.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Short_toString(thisObj, pret)                 Object_toString(thisObj, pret)

/**
 * Determines whether the two Short objects are the same.
 *
 * @param thisObj the first Short to compare.
 * @param hobj2 the second Short to compare.
 * @param pret true if the Short objects have identical content, false otherwise.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Short_equals(thisObj, hobj2, pret)            Object_equals(thisObj, hobj2, pret)

/**
 * Returns the value of this <code>Short</code> object as a jbyte.
 *
 * @param   hObj the Short object to operate against.
 * @param   pByte the jbyte value represented by this object.
 *
 * @return  SMQ_SUCCESS (0) if successful, non-zero if error.
 *          Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Short_byteValue(hObj, pByte)                  Number_byteValue(hObj, pByte)

/**
 * Returns the value of this <code>Short</code> object as a jdouble.
 *
 * @param   hObj the Short object to operate against.
 * @param   pDouble the jdouble value represented by this object.
 *
 * @return  SMQ_SUCCESS (0) if successful, non-zero if error.
 *          Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Short_doubleValue(hObj, pDouble)              Number_doubleValue(hObj, pDouble)

/**
 * Returns the value of this <code>Short</code> object as a jfloat.
 *
 * @param   hObj the Short object to operate against.
 * @param   pFloat the jfloat value represented by this object.
 *
 * @return  SMQ_SUCCESS (0) if successful, non-zero if error.
 *          Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Short_floatValue(hObj, pFloat)                Number_floatValue(hObj, pFloat)

/**
 * Returns the value of this <code>Short</code> object as a jint.
 *
 * @param   hObj the Short object to operate against.
 * @param   pInt the jint value represented by this object.
 *
 * @return  SMQ_SUCCESS (0) if successful, non-zero if error.
 *          Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Short_intValue(hObj, pInt)                    Number_intValue(hObj, pInt)

/**
 * Returns the value of this <code>Short</code> object as a jlong.
 *
 * @param   hObj the Short object to operate against.
 * @param   pLong the jlong value represented by this object.
 *
 * @return  SMQ_SUCCESS (0) if successful, non-zero if error.
 *          Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Short_longValue(hObj, pLong)                  Number_longValue(hObj, pLong)

/**
 * Returns the value of this <code>Short</code> object as a jshort.
 *
 * @param   hObj the Short object to operate against.
 * @param   pShort the jshort value represented by this object.
 *
 * @return  SMQ_SUCCESS (0) if successful, non-zero if error.
 *          Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Short_shortValue(hObj, pShort)                Number_shortValue(hObj, pShort)

/**
 * Returns a jshort w/ the value represented by the specified String, 
 *  as performed by the valueOf method of class Short.
 *
 * @param   string the string to be parsed.
 * @param   pShort the jshort value represented by the string argument.
 *
 * @return  SMQ_SUCCESS (0) if successful, non-zero if error.
 *          Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API Short_parseShort(HOBJ string, jshort *pShort);

/**
 * Returns a new Short object initialized to the value represented by the 
 *  specified string.
 *
 * @param   string the String object containing the string to be parsed.
 * @param   pShort a newly constructed Short object initialized to the value represented 
 *          by the string argument.
 *
 * @return  SMQ_SUCCESS (0) if successful, non-zero if error.
 *          Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API Short_valueOf(HOBJ string, HOBJ *pShort);

/**
 * Returns the int corresponding to the Short type.
 *
 * @return the int corresponding to the Short type
 */
int SMQ_API Short_type();

/**
 * Creates a new <code>Short</code> object that initially represents the 
 *   value passed in.
 *
 * @param s the initial value for the Short object.
 * @param pShort will contain a handle to the newly created Short object.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API Short_create(jshort s, HOBJ *pShort);

/*
//---------------------------------
// Float
//---------------------------------
*/
/**
 * Decrements the reference count of the Float object.
 *
 * @param thisObj the Float object to operate against.
 * @param pret will contain the reference count after decrementing.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Float_release(thisObj, pret)                   Object_release(thisObj, pret)

/**
 * Get the int corresponding to the type of a given object.
 *
 * @param thisObj the object to operate against.
 * @param pret will contain the int corresponding to the type.
 */
#define Float_getType(thisObj, pret)                   Object_getType(thisObj, pret)

/**
 * Determines whether the given object is an instance of the given type.
 *
 * @param thisObj the object to check.
 * @param classtype the type to check against.
 * @param pret non-zero if the object is an instance of the type.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Float_instanceof(thisObj, classtype, pret)     Object_instanceof(thisObj, classtype, pret)

/**
 * Get the String representation for the given Float.
 *
 * @param thisObj the Float object to operate against.
 * @param pret will contain the String that represents the Float.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Float_toString(thisObj, pret)                 Object_toString(thisObj, pret)

/**
 * Determines whether the two Float objects are the same.
 *
 * @param thisObj the first Float to compare.
 * @param hobj2 the second Float to compare.
 * @param pret true if the Float objects have identical content, false otherwise.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Float_equals(thisObj, hobj2, pret)            Object_equals(thisObj, hobj2, pret)

/**
 * Returns the value of this <code>Float</code> object as a jbyte (by casting to a jbyte).
 *
 * @param   hObj the Float object to operate against.
 * @param   pByte the jbyte value represented by this object.
 *
 * @return  SMQ_SUCCESS (0) if successful, non-zero if error.
 *          Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Float_byteValue(hObj, pByte)                  Number_byteValue(hObj, pByte)

/**
 * Returns the value of this <code>Float</code> object as a jdouble (by casting to a jdouble).
 *
 * @param   hObj the Float object to operate against.
 * @param   pDouble the jdouble value represented by this object.
 *
 * @return  SMQ_SUCCESS (0) if successful, non-zero if error.
 *          Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Float_doubleValue(hObj, pDouble)              Number_doubleValue(hObj, pDouble)

/**
 * Returns the value of this <code>Float</code> object as a jfloat.
 *
 * @param   hObj the Float object to operate against.
 * @param   pFloat the jfloat value represented by this object.
 *
 * @return  SMQ_SUCCESS (0) if successful, non-zero if error.
 *          Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Float_floatValue(hObj, pFloat)                Number_floatValue(hObj, pFloat)

/**
 * Returns the value of this <code>Float</code> object as a jint (by casting to a jint).
 *
 * @param   hObj the Float object to operate against.
 * @param   pInt the jint value represented by this object.
 *
 * @return  SMQ_SUCCESS (0) if successful, non-zero if error.
 *          Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Float_intValue(hObj, pInt)                    Number_intValue(hObj, pInt)

/**
 * Returns the value of this <code>Float</code> object as a jlong (by casting to a jlong).
 *
 * @param   hObj the Float object to operate against.
 * @param   pLong the jlong value represented by this object.
 *
 * @return  SMQ_SUCCESS (0) if successful, non-zero if error.
 *          Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Float_longValue(hObj, pLong)                  Number_longValue(hObj, pLong)

/**
 * Returns the value of this <code>Float</code> object as a jshort (by casting to a jshort).
 *
 * @param   hObj the Float object to operate against.
 * @param   pShort the jshort value represented by this object.
 *
 * @return  SMQ_SUCCESS (0) if successful, non-zero if error.
 *          Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Float_shortValue(hObj, pShort)                Number_shortValue(hObj, pShort)

/**
 * Returns a jfloat w/ the value represented by the specified String, 
 *  as performed by the valueOf method of class Float.
 *
 * @param   string the string to be parsed.
 * @param   pFloat the jfloat value represented by the string argument.
 *
 * @return  SMQ_SUCCESS (0) if successful, non-zero if error.
 *          Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API Float_parseFloat(HOBJ string, jfloat *pFloat);

/**
 * Returns a new Float object initialized to the value represented by the 
 *  specified string.
 *
 * @param   string the String object containing the string to be parsed.
 * @param   pFloat a newly constructed Float object initialized to the value represented 
 *          by the string argument.
 *
 * @return  SMQ_SUCCESS (0) if successful, non-zero if error.
 *          Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API Float_valueOf(HOBJ string, HOBJ *pFloat);

/**
 * Returns the int corresponding to the Float type.
 *
 * @return the int corresponding to the Float type
 */
int SMQ_API Float_type();

/**
 * Creates a new <code>Float</code> object that initially represents the 
 *   value passed in.
 *
 * @param f the initial value for the Float object.
 * @param pFloat will contain a handle to the newly created Float object.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API Float_create(jfloat f, HOBJ *pFloat);

/*
//---------------------------------
// Double
//---------------------------------
*/
/**
 * Decrements the reference count of the Double object.
 *
 * @param thisObj the Double object to operate against.
 * @param pret will contain the reference count after decrementing.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Double_release(thisObj, pret)                   Object_release(thisObj, pret)

/**
 * Get the int corresponding to the type of a given object.
 *
 * @param thisObj the object to operate against.
 * @param pret will contain the int corresponding to the type.
 */
#define Double_getType(thisObj, pret)                   Object_getType(thisObj, pret)

/**
 * Determines whether the given object is an instance of the given type.
 *
 * @param thisObj the object to check.
 * @param classtype the type to check against.
 * @param pret non-zero if the object is an instance of the type.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Double_instanceof(thisObj, classtype, pret)     Object_instanceof(thisObj, classtype, pret)

/**
 * Get the String representation for the given Double.
 *
 * @param thisObj the Double object to operate against.
 * @param pret will contain the String that represents the Double.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Double_toString(thisObj, pret)                 Object_toString(thisObj, pret)

/**
 * Determines whether the two Double objects are the same.
 *
 * @param thisObj the first Double to compare.
 * @param hobj2 the second Double to compare.
 * @param pret true if the Double objects have identical content, false otherwise.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Double_equals(thisObj, hobj2, pret)            Object_equals(thisObj, hobj2, pret)

/**
 * Returns the value of this <code>Double</code> object as a jbyte (by casting to a jbyte).
 *
 * @param   hObj the Double object to operate against.
 * @param   pByte the jbyte value represented by this object.
 *
 * @return  SMQ_SUCCESS (0) if successful, non-zero if error.
 *          Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Double_byteValue(hObj, pByte)                  Number_byteValue(hObj, pByte)

/**
 * Returns the value of this <code>Double</code> object as a jdouble.
 *
 * @param   hObj the Double object to operate against.
 * @param   pDouble the jdouble value represented by this object.
 *
 * @return  SMQ_SUCCESS (0) if successful, non-zero if error.
 *          Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Double_doubleValue(hObj, pDouble)              Number_doubleValue(hObj, pDouble)

/**
 * Returns the value of this <code>Double</code> object as a jfloat (by casting to a jfloat).
 *
 * @param   hObj the Double object to operate against.
 * @param   pFloat the jfloat value represented by this object.
 *
 * @return  SMQ_SUCCESS (0) if successful, non-zero if error.
 *          Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Double_floatValue(hObj, pFloat)                Number_floatValue(hObj, pFloat)

/**
 * Returns the value of this <code>Double</code> object as a jint (by casting to a jint).
 *
 * @param   hObj the Double object to operate against.
 * @param   pInt the jint value represented by this object.
 *
 * @return  SMQ_SUCCESS (0) if successful, non-zero if error.
 *          Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Double_intValue(hObj, pInt)                    Number_intValue(hObj, pInt)

/**
 * Returns the value of this <code>Double</code> object as a jlong (by casting to a jlong).
 *
 * @param   hObj the Double object to operate against.
 * @param   pLong the jlong value represented by this object.
 *
 * @return  SMQ_SUCCESS (0) if successful, non-zero if error.
 *          Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Double_longValue(hObj, pLong)                  Number_longValue(hObj, pLong)

/**
 * Returns the value of this <code>Double</code> object as a jshort (by casting to a jshort).
 *
 * @param   hObj the Double object to operate against.
 * @param   pShort the jshort value represented by this object.
 *
 * @return  SMQ_SUCCESS (0) if successful, non-zero if error.
 *          Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Double_shortValue(hObj, pShort)                Number_shortValue(hObj, pShort)

/**
 * Returns a jdouble w/ the value represented by the specified String, 
 *  as performed by the valueOf method of class Double.
 *
 * @param   string the string to be parsed.
 * @param   pDouble the jdouble value represented by the string argument.
 *
 * @return  SMQ_SUCCESS (0) if successful, non-zero if error.
 *          Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API Double_parseDouble(HOBJ string, jdouble *pDouble);

/**
 * Returns a new Double object initialized to the value represented by the 
 *  specified string.
 *
 * @param   string the String object containing the string to be parsed.
 * @param   pDouble a newly constructed Double object initialized to the value represented 
 *          by the string argument.
 *
 * @return  SMQ_SUCCESS (0) if successful, non-zero if error.
 *          Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API Double_valueOf(HOBJ string, HOBJ *pDouble);

/**
 * Returns the int corresponding to the Double type.
 *
 * @return the int corresponding to the Double type
 */
int SMQ_API Double_type();

/**
 * Creates a new <code>Double</code> object that initially represents the 
 *   value passed in.
 *
 * @param d the initial value for the Double object.
 * @param pDouble will contain a handle to the newly created Double object.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API Double_create(jdouble d, HOBJ *pDouble);

/*
//---------------------------------
// NumberFormatException
//---------------------------------
*/
/**
 * Decrements the reference count of the NumberFormatException object.
 *
 * @param thisObj the NumberFormatException object to operate against.
 * @param pret will contain the reference count after decrementing.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define NumberFormatException_release(thisObj, pret)                Object_release(thisObj, pret)

/**
 * Get the int corresponding to the type of a given object.
 *
 * @param thisObj the object to operate against.
 * @param pret will contain the int corresponding to the type.
 */
#define NumberFormatException_getType(thisObj, pret)                Object_getType(thisObj, pret)

/**
 * Determines whether the given object is an instance of the given type.
 *
 * @param thisObj the object to check.
 * @param classtype the type to check against.
 * @param pret non-zero if the object is an instance of the type.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define NumberFormatException_instanceof(thisObj, classtype, pret)  Object_instanceof(thisObj, classtype, pret)

/**
 * Determines whether the two NumberFormatException objects are the same.
 *
 * @param thisObj the first NumberFormatException to compare.
 * @param hobj2 the second NumberFormatException to compare.
 * @param pret true if the NumberFormatException objects have identical content, false otherwise.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define NumberFormatException_equals(thisObj, hobj2, pret)          Object_equals(thisObj, hobj2, pret)

/**
 * Get the String representation for the given NumberFormatException.
 *
 * @param thisObj the NumberFormatException object to operate against.
 * @param pret will contain the String that represents the NumberFormatException.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define NumberFormatException_toString(thisObj, pret)               Object_toString(thisObj, pret)

/**
 * Retrieve the String containing the localized message associated w/ this NumberFormatException.
 *
 * @param thisObj the NumberFormatException object to operate against.
 * @param pretString will contain the String containing the localized message.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define NumberFormatException_getLocalizedMessage(thisObj, pretString) Throwable_getLocalizedMessage(thisObj, pretString)

/**
 * Retrieve the String containing the message associated w/ this NumberFormatException.
 *
 * @param thisObj the NumberFormatException object to operate against.
 * @param pretString will contain the String containing the message.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define NumberFormatException_getMessage(thisObj, pretString)       Throwable_getMessage(thisObj, pretString)

/**
 * Prints a stack trace w/ to the error location corresponding to the NumberFormatException object.
 *
 * @param thisObj the NumberFormatException object to operate against.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define NumberFormatException_printStackTrace(thisObj)              Throwable_printStackTrace(thisObj)

/**
 * Returns the int corresponding to the NumberFormatException type.
 *
 * @return the int corresponding to the NumberFormatException type
 */
int SMQ_API NumberFormatException_type();

/*
//---------------------------------
// Throwable
//---------------------------------
*/
/**
 * Decrements the reference count of the Throwable object.
 *
 * @param thisObj the Throwable object to operate against.
 * @param pret will contain the reference count after decrementing.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Throwable_release(thisObj, pret)                Object_release(thisObj, pret)

/**
 * Get the int corresponding to the type of a given object.
 *
 * @param thisObj the object to operate against.
 * @param pret will contain the int corresponding to the type.
 */
#define Throwable_getType(thisObj, pret)                Object_getType(thisObj, pret)

/**
 * Determines whether the given object is an instance of the given type.
 *
 * @param thisObj the object to check.
 * @param classtype the type to check against.
 * @param pret non-zero if the object is an instance of the type.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Throwable_instanceof(thisObj, classtype, pret)  Object_instanceof(thisObj, classtype, pret)

/**
 * Determines whether the two Throwable objects are the same.
 *
 * @param thisObj the first Throwable to compare.
 * @param hobj2 the second Throwable to compare.
 * @param pret true if the Throwable objects have identical content, false otherwise.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Throwable_equals(thisObj, hobj2, pret)          Object_equals(thisObj, hobj2, pret)

/**
 * Get the String representation for the given Throwable.
 *
 * @param thisObj the Throwable object to operate against.
 * @param pret will contain the String that represents the Throwable.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Throwable_toString(thisObj, pret)               Object_toString(thisObj, pret)

/**
 * Returns the int corresponding to the Throwable type.
 *
 * @return the int corresponding to the Throwable type
 */
int SMQ_API Throwable_type();

/**
 * Retrieve the String containing the localized message associated w/ this Throwable.
 *
 * @param thisObj the Throwable object to operate against.
 * @param pret will contain the String containing the localized message.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API Throwable_getLocalizedMessage(HOBJ thisObj, HOBJ *pret);

/**
 * Retrieve the String containing the message associated w/ this Throwable.
 *
 * @param thisObj the Throwable object to operate against.
 * @param pret will contain the String containing the message.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API Throwable_getMessage(HOBJ thisObj, HOBJ *pret);

/**
 * Prints a stack trace w/ to the error location corresponding to the Throwable object.
 *
 * @param thisObj the Throwable object to operate against.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API Throwable_printStackTrace(HOBJ thisObj);

/*
//---------------------------------
// String
//---------------------------------
*/
/**
 * Decrements the reference count of the String object.
 *
 * @param stringObj the String object to operate against.
 * @param pret will contain the reference count after decrementing.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define String_release(stringObj, pret)                Object_release(stringObj, pret)

/**
 * Get the int corresponding to the type of a given object.
 *
 * @param thisObj the object to operate against.
 * @param pret will contain the int corresponding to the type.
 */
#define String_getType(thisObj, pret)                  Object_getType(thisObj, pret)

/**
 * Determines whether the given object is an instance of the given type.
 *
 * @param thisObj the object to check.
 * @param classtype the type to check against.
 * @param pret non-zero if the object is an instance of the type.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define String_instanceof(thisObj, classtype, pret)  Object_instanceof(thisObj, classtype, pret)

/**
 * Determines whether the two String objects refer to the same string.
 *
 * @param stringObj the first String to compare.
 * @param hobj2 the second String to compare.
 * @param pret true if the String objects have identical content, false otherwise.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define String_equals(stringObj, hobj2, pret)          Object_equals(stringObj, hobj2, pret)

/**
 * Get the String representation for the given String.
 *
 * @param stringObj the String object to operate against.
 * @param pret will contain the String that represents the String.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define String_toString(stringObj, pret)               Object_toString(stringObj, pret)

/**
 * Allocates a new string that contains the same
 * sequence of characters as the string argument.
 *
 * @param pString String to clone from
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */

/** 
 * Allocate a new String containing no characters.
 *
 * @param pString the new String (which contains no characters)
 */
int SMQ_API String_create(HOBJ *pString);

/**
 * Allocate a new String so that it represents the
 * sequence of 8-bit characters are pointed to by
 * the argument c.
 *
 * Note: The only supported 8-bit encoding is ISO8859-1.
 * If you are using a different encoding, you should initialize
 * Strings with String_create8 (which takes wchar_t*) or
 * String_create9 (which takes UTF-8).
 *
 * @param c the incoming char pointer 
 * @param pString the new String that was created
 */
int SMQ_API String_create2(char *c, HOBJ *pString);

/**
 * Allocate a new String that contains the same
 * sequence of characters as the argument, which can
 * be a String, a jcharArray, or a jbyteArray.
 *
 * Note: When inData is a jbyteArray, this function will
 * use 0 for the high byte of each jchar in the String,
 * which will only work properly if your character encoding
 * is ISO8859-1. If you are using a different encoding, 
 * you should initialize Strings with String_create8 (which 
 * takes wchar_t*) or String_create9 (which takes UTF-8).
 *
 * @param inData object from which to initialize String.
 * @param pString the new String that was created
 */
int SMQ_API String_create3(HOBJ inData, HOBJ *pString);

/**
 * Allocate a new String initialized from a subarray of
 * the inData argument, which can be either a jcharArray
 * or a jbyteArray. The offset argument is the index of 
 * the first jchar or jbyte of the subarray and the count 
 * argument specifies the length of the subarray.
 *
 * Note: When inData is a jbyteArray, this function will
 * use 0 for the high byte of each jchar in the String,
 * which will only work properly if your character encoding
 * is ISO8859-1. If you are using a different encoding, 
 * you should initialize Strings with String_create8 (which 
 * takes wchar_t*) or String_create9 (which takes UTF-8).
 *
 * @param inData object from which to initialize String
 * @param offset the initial offset. 
 * @param count the length.
 * @param pString the newly created String
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Likely causes of errors:
 *            StringIndexOutOfBoundsException if the offset and
 *            count arguments index characters outside the bounds
 *            of the array.          
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API String_create4(HOBJ inData, jint offset, jint count, HOBJ *pString);

/**
 * Allocate a new String initialized from a subarray of
 * the inData argument, which must be a jbyteArray.
 * The offset argument is the index of the first jbyte
 * of the subarray and the count argument specifies the 
 * length of the subarray.
 *
 * Note: This function uses hibyte for the high byte of each 
 * jchar in the String, which will only work properly if hibyte
 * is 0 and your character encoding is ISO8859-1. If you are 
 * using a different encoding, you should initialize Strings 
 * with String_create8 (which takes wchar_t*) or String_create9 
 * (which takes UTF-8).
 *
 * @param inData the jbyteArray to be converted to characters. 
 * @param hibyte the top 8 bits of each 16-bit Unicode character.
 * @param offset the initial offset.
 * @param count the length.
 * @param pString the newly created String
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Likely causes of errors:
 *            StringIndexOutOfBoundsException if the offset and
 *            count arguments index characters outside the bounds
 *            of the array.          
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API String_create5(HOBJ inData, jint hibyte, jint offset, jint count, HOBJ *pString);

/**
 * Allocate a new String initialized from a subarray of
 * the inData argument, which must be a jbyteArray. The 
 * offset argument is the index of the first jbyte of the 
 * subarray and the count argument specifies the length of the subarray.
 *
 * The enc parameter, meant to be a general specification of character
 * encoding, currently must be the string "ISO8859_1".
 * 
 * Note: The only 8-bit encoding supported is ISO8859-1, in which the
 * high byte is set to 0 for each character. If you are using a different 
 * encoding, you should initialize Strings with String_create8 (which 
 * takes wchar_t*) or String_create9 (which takes UTF-8).
 *
 * @param inData the jbyteArray to be converted into characters 
 * @param offset index of the first byte to convert 
 * @param length number of bytes to convert 
 * @param enc the name of a character encoding 
 * @param pString the newly created String
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Likely causes of errors:
 *            UnsupportedEncodingException if the named encoding is not supported
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API String_create6(HOBJ inData, jint offset, jint length, HOBJ enc, HOBJ *pString);

/**
 * Allocate a new String initialized from a subarray of
 * the inData argument, which must be a jbyteArray. The 
 * offset argument is the index of the first jbyte of the 
 * subarray and the count argument specifies the length of the subarray.
 *
 * Note: The only 8-bit encoding supported is ISO8859-1, in which the
 * high byte is set to 0 for each character. If you are using a different 
 * encoding, you should initialize Strings with String_create8 (which 
 * takes wchar_t*) or String_create9 (which takes UTF-8).
 *
 * @param inData the jbyteArray to be converted into characters 
 * @param offset Index of the first byte to convert
 * @param length Number of bytes to convert 
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API String_create7(HOBJ inData, jint offset, jint length, HOBJ *pString);

/**
 * Allocates a new String so that it represents the
 * sequence of wide characters that are pointed to by
 * the argument c. The character encoding is assumed to be USC-2,
 * which means the first two bytes of each wchar_t are copied to
 * the String as a jchar, and any higher bytes are ignored.
 *
 * @param c the incoming wchar_t pointer 
 * @param pString the new String that was created
 */
int SMQ_API String_create8(wchar_t *c, HOBJ *pString);

/**
 * Allocates a new String from a  UTF-8 encoded string.  The encoding
 * is converted from UTF-8 to UCS-2.
 *
 * @param c the incoming char pointer 
 * @param e the encoding of the incoming char pointer (should be some form of UTF-8)
 * @param pString the new String that was created
 */
int SMQ_API String_create9(unsigned char *c, char *e, HOBJ *pString);

/**
 * Returns the int corresponding to the String type.
 *
 * @return the int corresponding to the String type
 */
int SMQ_API String_type();

/**
 * Returns the length of this string. The length 
 * is equal to the number of jchar(s) string.
 *
 * @param stringObj the String object to operate against
 * @param pLength the length of the sequence of jchars in the String. 
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API String_length(HOBJ stringObj, jint *pLength);

/**
 * Returns the internal jchar array of this string. Note: the String owns the
 * memory.
 *
 * @param stringObj the String object to operate against
 * @param pJchars pointer to the sequence of characters represented by this
 *          object.
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API String_getArray(HOBJ stringObj, const jchar **pJchars);

/**
 * Returns the jchar at the specified index. An index 
 * ranges from 0 to length() - 1. The first character is at index 0.
 *
 * @param stringObj the String object to operate against
 * @param index the index of the jchar.
 * @param pJchar the jchar at the specified index of this string.
 * 
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API String_charAt(HOBJ stringObj, jint index, jchar *pJchar);

/**
 * Copies characters from this string into the destination
 * character array. The first character to be copied is at
 * index srcBegin; the last character to be copied is at
 * index srcEnd-1 (thus the total number of characters to
 * be copied is srcEnd-srcBegin). The characters are copied
 * into the subarray of dst starting at index dstBegin and
 * ending at index: 
 *
 *    dstbegin + (srcEnd-srcBegin) - 1
 *
 *
 * @param stringObj the String object to operate against
 * @param srcBegin index of the first character in the string to copy. 
 * @param srcEnd index after the last character in the string to copy. 
 * @param dst the destination array. 
 * @dstBegin the start offset in the destination array. 
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Likely causes of errors:
 *            StringIndexOutOfBoundsException if srcBegin or srcEnd is out
 *            of range, or if srcBegin is greater than the srcEnd.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API String_getChars(HOBJ stringObj, jint srcBegin, jint srcEnd, HOBJ dst, jint dstBegin);

/**
*
 * Copies characters from this string into the destination byte array.
 * Each byte receives the 8 low-order bits of the corresponding
 * character. 
 *
 * The first character to be copied is at index srcBegin;
 * the last character to be copied is at index srcEnd-1.
 * The total number of characters to be copied is srcEnd-srcBegin.
 * The characters, converted to bytes, are copied into the subarray
 * of dst starting at index dstBegin and ending at index: 
 * Note: getBytes() is deprecated. This method does not properly
 * convert characters into bytes. The preferred
 * way to do this is via the getBytes(String enc) method,
 * which takes a character-encoding name, or the getBytes() method,
 * which uses the platform's default encoding.
 *
 *   dstbegin + (srcEnd-srcBegin) - 1
 *
 * @param stringObj the String object to operate against
 * @param srcBegin index of the first character in the string to copy. 
 * @param srcEnd index after the last character in the string to copy. 
 * @param dst the destination array.
 * @param dstBegin the start offset in the destination array. 
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Likely causes of errors:
 *            StringIndexOutOfBoundsException if srcBegin or srcEnd is out
 *            of range, or if srcBegin is greater than the srcEnd.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API String_getBytes(HOBJ stringObj, jint srcBegin, jint srcEnd, HOBJ dst, jint dstBegin);

/**
 * Convert this String into bytes according to the specified character
 * encoding, storing the result into a new byte array.
 * 
 * @param enc A character-encoding name 
 * @param pJbyteArr the resultant byte array
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API String_getBytes2(HOBJ stringObj, HOBJ enc, HOBJ *pJbyteArr);

/** Convert this String into bytes according to the platform's default
 * character encoding, storing the result into a new byte array.
 *
 * @param stringObj the String object to operate against
 * @param pJbytesArr the resultant byte array.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API String_getBytes3(HOBJ stringObj, HOBJ *pJbytesArr);

/**
 * Determines whether the two String objects refer to the same string ignoring case differences.
 *
 * @param stringObj the first String to compare.
 * @param anotherString the second String to compare.
 * @param pret true if the String objects have identical content, false otherwise.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API String_equalsIgnoreCase(HOBJ stringObj, HOBJ anotherString, jboolean *pret);

/**
 * Compares two strings lexicographically. The comparison is based on the 
 * Unicode value of each character in the strings.
 *
 * @param o the String to be compared. 
 *
 * @param pret returns the value 0 if the argument string is equal to this string;
 * a value less than 0 if this string is lexicographically less than 
 * the string argument; and a value greater than 0 if this string is 
 * lexicographically greater than the string argument. 
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API String_compareTo(HOBJ stringObj, HOBJ anotherString, jint *pret);

/**
 * Compares two strings lexicographically. The comparison is based on the
 * Unicode value of each character in the strings.
 *
 * @param stringObj the String object to operate against
 * @param anotherString the String to be compared. 
 * @param pret returns the value 0 if the argument string is equal to this string; 
 * a value less than 0 if this string is lexicographically less than the
 * string argument; and a value greater than 0 if this string is 
 * lexicographically greater than the string argument. 
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API String_compareToIgnoreCase(HOBJ stringObj, HOBJ str, jint *pret);

/**
 * Tests if two string regions are equal. If toffset or ooffset is negative,
 * or if toffset+length is greater than the length of this string, or if 
 * ooffset+length is greater than the length of the string argument, 
 * then this method returns false.
 *
 * @param stringObj the String object to operate against
 * @param toffset the starting offset of the subregion in this string. 
 * @param other the string argument. 
 * @param ooffset the starting offset of the subregion in the string argument. 
 * @param len the number of characters to compare.
 *
 * @param pret return true if the specified subregion of this string matches the
 * specified subregion of the string argument; false otherwise.
 * The matching is always case-sensitive.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API String_regionMatches(HOBJ stringObj, jint toffset, HOBJ other, jint ooffset, jint len, jboolean *pret);

/**
 * Tests if two string regions are equal. If toffset or ooffset is negative,
 * or if toffset+length is greater than the length of this string, or if 
 * ooffset+length is greater than the length of the string argument, 
 * then this method returns false.
 *
 * @param stringObj the String object to operate against
 * @param ignoreCase if true, ignore case when comparing characters. 
 * @param toffset the starting offset of the subregion in this string. 
 * @param other the string argument. 
 * @param ooffset the starting offset of the subregion in the string argument. 
 * @param len the number of characters to compare.
 *
 * @param pret return true if the specified subregion of this string matches the
 * specified subregion of the string argument; false otherwise. Whether
 * the matching is exact or case insensitive depends on the ignoreCase 
 * argument.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API String_regionMatches2(HOBJ stringObj, jboolean ignoreCase, jint toffset, HOBJ other, jint ooffset, jint len, jboolean *pret);

/**
 * Tests if this string starts with the specified prefix.
 *
 * @param stringObj the String object to operate against
 * @param prefix the prefix. 
 * @param toffset where to begin looking in the string. 
 * @param pStartsWith returns true if the character sequence represented by
 *        the argument is a prefix of the substring of this object starting
 *        at index toffset; false otherwise.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API String_startsWith(HOBJ stringObj, HOBJ prefix, jint toffset, jboolean *pStartsWith);

/**
 * Tests if this string starts with the specified prefix.
 *
 * @param stringObj the String object to operate against
 * @param prefix a String containing the prefix. 
 * @param pStartsWith returns true if the character sequence represented by
 *        the argument is a prefix of the substring of this object starting
 *        at index toffset; false otherwise.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API String_startsWith2(HOBJ stringObj, HOBJ prefix, jboolean *pret);

/**
 * Tests if this string ends with the specified suffix.
 *
 * @param stringObj the String object to operate against
 * @param suffix a String containing the suffix.
 *
 * @param pret true if the character sequence represented by the argument
 * is a suffix of the character sequence represented by this object;
 * false otherwise.

 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API String_endsWith(HOBJ stringObj, HOBJ suffix, jboolean *pret);

/**
 * Returns the index within this string of the first occurrence
 * of the specified character.
 * 
 * @param stringObj the String object to operate against
 * @param ch a character. 
 * @param pIndex the index of the first occurrence of the character in
 * the character sequence represented by this object, or -1 if 
 * the character does not occur. 
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API String_indexOf(HOBJ stringObj, jint ch, jint *pIndex);

/**
 * Returns the index within this string of the first occurrence of 
 * the specified character, starting the search at the specified
 * index.
 *
 * @param stringObj the String object to operate against
 * @param ch a character.
 * @param fromIndex the index to start the search from. 
 * @param pIndex the index of the first occurrence of the character
 * in the character sequence represented by this object that 
 * is greater than or equal to fromIndex, or -1 if the character 
 * does not occur.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API String_indexOf2(HOBJ stringObj, jint ch, jint fromIndex, jint *pIndex);
/**
 * Returns the index within this string of the first occurrence of
 * the specified substring.
 *
 * @param stringObj the String object to operate against.
 * @param str any String.
 * @param pIndex if the string argument occurs as a substring within
 * this object, then the index of the first character of
 * the first such substring is returned; if it does not occur
 * as a substring, -1 is returned.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API String_indexOf3(HOBJ stringObj, HOBJ str, jint *pIndex);

/**
 * Returns the index within this string of the first occurrence of the
 * specified substring, starting at the specified index.
 *
 * @param stringObj the String object to operate against.
 * @param str the substring to search for. 
 * @param fromIndex  the index to start the search from. 
 *
 * @param pIndex if the string argument occurs as a substring within
 * this object at a starting index no smaller than fromIndex,
 * then the index of the first character of the first such substring
 * is returned. If it does not occur as a substring starting at fromIndex
 * or beyond, -1 is returned.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API String_indexOf4(HOBJ stringObj, HOBJ str, jint fromIndex, jint *pIndex);

/**
 * Returns the index within this string of the last occurrence 
 * of the specified character. The String is searched 
 * backwards starting at the last character.
 *
 * @param stringObj the String object to operate against
 * @param ch a character. 
 * @param pIndex the index of the last occurrence of the 
 * character in the character sequence represented by this 
 * object, or -1 if the character does not occur.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API String_lastIndexOf(HOBJ stringObj, jint ch, jint *pIndex);

/**
 * Returns the index within this string of the last occurrence
 * of the specified character, searching backward starting at the
 * specified index.
 * 
 * @param stringObj the String object to operate against
 * @param ch a character.
 * @param fromIndex the index to start the search from. 
 * @param pIndex the index of the last occurrence of the character 
 * in the character sequence represented by this object that 
 * is less than or equal to fromIndex, or -1 if the character
 * does not occur before that point.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API String_lastIndexOf2(HOBJ stringObj, jint ch, jint fromIndex, jint *pIndex);

/**
 * Returns the index within this string of the rightmost
 * occurrence of the specified substring. The rightmost empty
 * string "" is considered to occur at the index value this.length().
 *
 * @param stringObj the String object to operate against
 * @param str the substring to search for. 
 * @param pIndex if the string argument occurs one or more times
 * as a substring within this object, then the index of the 
 * first character of the last such substring is returned.
 * If it does not occur as a substring, -1 is returned.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API String_lastIndexOf3(HOBJ stringObj, HOBJ str, jint *pIndex);

/**
 * Returns the index within this string of the last occurrence
 * of the specified substring. The returned index indicates the
 * start of the substring, and it must be equal to or less than
 * fromIndex.
 *
 * @param stringObj the String object to operate against
 * @param str the substring to search for. 
 * @param fromIndex the index to start the search from. 
 *
 * @param pIndex if the string argument occurs one or more times
 * as a substring within this object at a starting index no
 * greater than fromIndex, then the index of the first character
 * of the last such substring is returned. If it does not occur
 * as a substring starting at fromIndex or earlier, -1 is returned. 
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API String_lastIndexOf4(HOBJ stringObj, HOBJ str, jint fromIndex, jint *pIndex);


/**
 * Returns a new string that is a substring of this string.
 * The substring begins at the specified index and extends
 * to the end of this string.
 * 
 * @param stringObj the String object to operate against
 * @param beginIndex the beginning index, inclusive. 
 * @param pString the specified substring.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Likely causes of errors:
 *           StringIndexOutOfBoundsException if the beginIndex is out of range.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API String_substring(HOBJ stringObj, jint beginIndex, HOBJ *pString);

/**
 * Returns a new string that is a substring of this string. 
 * The substring begins at the specified beginIndex and extends
 * to the character at index endIndex - 1.
 *
 * @param stringObj the String object to operate against
 * @param beginIndex the beginning index, inclusive. 
 * @param endIndex the ending index, exclusive. 
 * @param pString the specified substring.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Likely causes of errors:
 *           StringIndexOutOfBoundsException if the beginIndex or endIndex
 *           is out of range.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API String_substring2(HOBJ stringObj, jint beginIndex, jint endIndex, HOBJ *pString);

/**
 * Concatenates the specified string to the end of this string.
 * If the length of the argument string is 0, then this object is
 * returned.
 *
 * @param stringObj the String object to operate against
 * @param str the String that is concatenated to the end of this String. 
 * @param pString a string that represents the concatenation of this object's 
 * characters followed by the string argument's characters
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API String_concat(HOBJ stringObj, HOBJ str, HOBJ *pString);

/**
 * Replaces all occurrences of a given character in the String w/ another character.
 *
 * @param stringObj the String object to operate against.
 * @param oldChar the old character.
 * @param newChar the new character.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API String_replace(HOBJ stringObj, jchar oldChar, jchar newChar);

/**
 * Converts all of the characters in this String to lower case.
 *
 * @param stringObj the String object to operate against.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API String_toLowerCase(HOBJ stringObj);

/**
 * Converts all of the characters in this String to upper case.
 *
 * @param stringObj the String object to operate against.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API String_toUpperCase(HOBJ stringObj);

/**
 * Removes white space from both ends of this String.
 *
 * @param stringObj the String object to operate against.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API String_trim(HOBJ stringObj);

/**
 * Converts this String to a new jcharArray.
 *
 * @param stringObj the String object to operate against.
 * @param pret will contain the new jcharArray.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API String_toCharArray(HOBJ stringObj, HOBJ *pret);

/**
 * Converts this String to an AString (string of 8-bit characters).
 * This function is misnamed as an AString contains 8-bit characters, not ASCII.
 *
 * @param stringObj the String object to operate against.
 * @param pret the new AString.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API String_toAscii(HOBJ stringObj, HOBJ *pret);

/**
 * Converts this String to a WString (wide character string).
 *
 * @param stringObj the String object to operate against.
 * @param pret the new WString.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API String_toWide(HOBJ stringObj, HOBJ *pret);

/**
 * Converts this String to a UTF8String (UTF-8 encoded character string).
 *
 * @param stringObj the String object to operate against.
 * @param pret the new UTF8String.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API String_toUTF8(HOBJ stringObj, HOBJ *pret);

/**
 * Determines if each jchar in the String has a high byte of 0, which would allow
 * extracting an AString without data loss.
 * 
 * This function is misnamed as an AString contains 8-bit characters, not ASCII.
 *
 * @param stringObj the String object to operate against.
 * @param pret true if all jchars have a 0 high byte, false otherwise.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API String_isAscii(HOBJ stringObj, jboolean *pret);

/**
 * Returns a canonical representation for the string object. 
 * It follows that for any two strings s and t, s.intern() == t.intern() is true if and 
 * only if s.equals(t) is true.
 *
 * @param stringObj the String object to operate against.
 * @param pret the String containing the canonical representation.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API String_intern(HOBJ stringObj, HOBJ *pret);

/**
 * Assigns the String represented by str to the String represented by stringObj.
 *
 * @param stringObj the String object to assign to.
 * @param str the String to assign from.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API String_assign(HOBJ stringObj, HOBJ str);

/**
 * Appends a subarray of a jcharArray to the end of this String.
 *
 * @param stringObj the String object to append to.
 * @param str the jcharArray containing the characters to append.
 * @param offset the index of the first character to append.
 * @param len the number of characters to append.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API String_append(HOBJ stringObj, HOBJ str, jint offset, jint len);

/**
 * Appends a jchar to the end of this String.
 *
 * @param stringObj the String object to append to.
 * @param c the character to append.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API String_append2(HOBJ stringObj, jchar c);

/**
 * Appends a String to the end of this String.
 *
 * @param stringObj the String object to append to.
 * @param s the String to append.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API String_append3(HOBJ stringObj, HOBJ s);

/**
 * Returns the string representation of the obj argument.
 *
 * @param obj the String object to operate against.
 * @param pret will contain the String representation of the obj.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API String_valueOf(HOBJ obj, HOBJ *pret);

/**
 * Returns the string corresponding to a subarray of a jcharArray.
 *
 * @param data the jcharArray containing the characters to return as a String.
 * @param offset the index of the first character to be part of the String.
 * @param count the number of characters to be part of the String.
 * @param pret will contain the new String.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API String_valueOf2(HOBJ data, jint offset, jint count, HOBJ *pret);

/**
 * Returns the string representation of the jboolean argument.
 *
 * @param b a boolean.
 * @param pret will contain the String representation of b.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API String_valueOfBoolean(jboolean b, HOBJ *pret);

/**
 * Returns the string representation of the jbyte argument.
 *
 * @param b a jbyte.
 * @param pret will contain the String representation of b.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API String_valueOfByte(jbyte b, HOBJ *pret);

/**
 * Returns the string representation of the jchar argument.
 *
 * @param c a jchar.
 * @param pret will contain the String representation of c.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API String_valueOfChar(jchar c, HOBJ *pret);

/**
 * Returns the string representation of the jshort argument.
 *
 * @param s a jshort.
 * @param pret will contain the String representation of s.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API String_valueOfShort(jshort s, HOBJ *pret);

/**
 * Returns the string representation of the jint argument.
 *
 * @param i a jint.
 * @param pret will contain the String representation of i.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API String_valueOfInt(jint i, HOBJ *pret);

/**
 * Returns the string representation of the jlong argument.
 *
 * @param l a jlong.
 * @param pret will contain the String representation of l.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API String_valueOfLong(jlong l, HOBJ *pret);

/**
 * Returns the string representation of the jfloat argument.
 *
 * @param f a jfloat.
 * @param pret will contain the String representation of f.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API String_valueOfFloat(jfloat f, HOBJ *pret);

/**
 * Returns the string representation of the jdouble argument.
 *
 * @param d a jdouble.
 * @param pret will contain the String representation of d.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API String_valueOfDouble(jdouble d, HOBJ *pret);

/**
 * Returns a String that is equivalent to the specified character array. It creates a new 
 * array and copies the characters into it.
 *
 * @param data the jcharArray.
 * @param offset initial offset of the subarray.
 * @param count length of the subarray.
 * @param pret will contain a String w/ the specified subarray of the jcharArray.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API String_copyValueOf(HOBJ data, jint offset, jint count, HOBJ *pret);

/**
 * Returns a String that is equivalent to the specified character array. It creates a new 
 * array and copies the characters into it.
 *
 * @param data the jcharArray.
 * @param pret will contain a String w/ the characters of the jcharArray.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API String_copyValueOf2(HOBJ data, HOBJ *pret);


/*
//---------------------------------
// UTF8String
//---------------------------------
*/
/**
 * Decrements the reference count of the UTF8String object.
 *
 * @param thisObj the UTF8String object to operate against.
 * @param pret will contain the reference count after decrementing.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define UTF8String_release(thisObj, pret)                Object_release(thisObj, pret)

/**
 * Get the int corresponding to the type of a given object.
 *
 * @param thisObj the object to operate against.
 * @param pret will contain the int corresponding to the type.
 */
#define UTF8String_getType(thisObj, pret)                Object_getType(thisObj, pret)

/**
 * Determines whether the given object is an instance of the given type.
 *
 * @param thisObj the object to check.
 * @param classtype the type to check against.
 * @param pret non-zero if the object is an instance of the type.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define UTF8String_instanceof(thisObj, classtype, pret)  Object_instanceof(thisObj, classtype, pret)

/**
 * Determines whether the two UTF8Strings refer to the same strings.
 *
 * @param thisObj the first string object to compare.
 * @param hobj2 the second string object to compare.
 * @param pret true if the UTF8Strings refer to identical strings, false otherwise.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define UTF8String_equals(thisObj, hobj2, pret)          Object_equals(thisObj, hobj2, pret)

/**
 * Returns the int corresponding to the UTF8String type.
 *
 * @return the int corresponding to the UTF8String type
 */
int SMQ_API	UTF8String_type();

/**
 * Retrieve the const unsigned char* corresponding to the value of the UTF8String.
 *
 * @param thisObj the UTF8String object to operate against.
 * @param pret will contain the const unsigned char* corresponding to the UTF8String value.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API UTF8String_getValue(HOBJ thisObj, const unsigned char **pret);

/**
 * Get the length of the UTF8String.
 *
 * @param thisObj the UTF8String to operate against.
 * @param pret will contain the length of the UTF8String in characters.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API UTF8String_length(HOBJ thisObj, jint *pret);

/**
 * Get the byte at a particular location in the UTF8String. Invalid indices will trigger an error.
 *
 * @param thisObj the UTF8String to operate against.
 * @param index the zero-based index of the byte to retrieve.
 * @param pret will contain the byte at the designated location in the UTF8String.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API UTF8String_at(HOBJ thisObj, jint index, unsigned char *pret);

/*
//---------------------------------
// WString
//---------------------------------
*/
/**
 * Decrements the reference count of the WString object.
 *
 * @param thisObj the WString object to operate against.
 * @param pret will contain the reference count after decrementing.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define WString_release(thisObj, pret)                Object_release(thisObj, pret)

/**
 * Get the int corresponding to the type of a given object.
 *
 * @param thisObj the object to operate against.
 * @param pret will contain the int corresponding to the type.
 */
#define WString_getType(thisObj, pret)                Object_getType(thisObj, pret)

/**
 * Determines whether the given object is an instance of the given type.
 *
 * @param thisObj the object to check.
 * @param classtype the type to check against.
 * @param pret non-zero if the object is an instance of the type.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define WString_instanceof(thisObj, classtype, pret)  Object_instanceof(thisObj, classtype, pret)

/**
 * Determines whether the two WString objects are the same.
 *
 * @param thisObj the first WString to compare.
 * @param hobj2 the second WString to compare.
 * @param pret true if the WString objects have identical content, false otherwise.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define WString_equals(thisObj, hobj2, pret)          Object_equals(thisObj, hobj2, pret)

/**
 * Get the String representation for the given WString.
 *
 * @param thisObj the WString object to operate against.
 * @param pret will contain the String that represents the WString.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define WString_toString(thisObj, pret)               Object_toString(thisObj, pret)

/**
 * Returns the int corresponding to the WString type.
 *
 * @return the int corresponding to the WString type
 */
int SMQ_API	WString_type();

/**
 * Retrieve the const wchar_t* corresponding to the value of the WString.
 *
 * @param thisObj the WString object to operate against.
 * @param pret will contain the const char* corresponding to the WString value.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API WString_getValue(HOBJ thisObj, const wchar_t **pret);

/**
 * Get the length of the WString.
 *
 * @param thisObj the WString to operate against.
 * @param pret will contain the length of the WString in characters.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API WString_length(HOBJ thisObj, jint *pret);

/**
 * Get the wchar_t at a particular location in the WString. Invalid indices will trigger an error.
 *
 * @param thisObj the WString to operate against.
 * @param index the zero-based index of the character to retrieve.
 * @param pret will contain the char at the designated location in the WString.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API WString_at(HOBJ thisObj, jint index, wchar_t *pret);


/*
//-------------------------------------------------------------------------
// java/util
//-------------------------------------------------------------------------
*/

/*
//---------------------------------
// Dictionary
//---------------------------------
*/
/**
 * Decrements the reference count of the Dictionary object.
 *
 * @param thisObj the Dictionary object to operate against.
 * @param pret will contain the reference count after decrementing.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Dictionary_release(thisObj, pret)                Object_release(thisObj, pret)

/**
 * Get the int corresponding to the type of a given object.
 *
 * @param thisObj the object to operate against.
 * @param pret will contain the int corresponding to the type.
 */
#define Dictionary_getType(thisObj, pret)                Object_getType(thisObj, pret)

/**
 * Determines whether the given object is an instance of the given type.
 *
 * @param thisObj the object to check.
 * @param classtype the type to check against.
 * @param pret non-zero if the object is an instance of the type.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Dictionary_instanceof(thisObj, classtype, pret)  Object_instanceof(thisObj, classtype, pret)

/**
 * Determines whether the two Dictionary objects are the same.
 *
 * @param thisObj the first Dictionary to compare.
 * @param hobj2 the second Dictionary to compare.
 * @param pret true if the Dictionary objects have identical content, false otherwise.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Dictionary_equals(thisObj, hobj2, pret)          Object_equals(thisObj, hobj2, pret)

/**
 * Get the String representation for the given Dictionary.
 *
 * @param thisObj the Dictionary object to operate against.
 * @param pret will contain the String that represents the Dictionary.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Dictionary_toString(thisObj, pret)               Object_toString(thisObj, pret)

/**
 * Create a new jcharArray that is a copy of the given Dictionary.
 *
 * @param thisObj the Dictionary to clone.
 * @param pret will contain the newly created Dictionary.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Dictionary_clone(thisObj, pret)                  Object_clone(thisObj, pret)

/**
 * Returns the int corresponding to the Dictionary type.
 *
 * @return the int corresponding to the Dictionary type
 */
int SMQ_API Dictionary_type();

/**
 * Retrieves an Enumeration of the values in this dictionary.
 *
 * @param thisObj the Dictionary object to operate against.
 * @param pret will contain the Enumeration object containing the values in the dictionary.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API Dictionary_elements(HOBJ thisObj, HOBJ *pret);

/**
 * Retrieves the value to which the key is mapped in this dictionary.
 *
 * @param thisObj the Dictionary object to operate against.
 * @param key a key in this dictionary
 * @param pret will contain the object in the dictionary corresponding to the key.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API Dictionary_get(HOBJ thisObj, HOBJ key, HOBJ *pret);

/**
 * Tests if this dictionary maps no keys to value.
 *
 * @param thisObj the Dictionary object to operate against.
 * @param pret true if this dictionary maps no keys to values; false otherwise.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API Dictionary_isEmpty(HOBJ thisObj, jboolean *pret);

/**
 * Returns an Enumeration of the keys in this dictionary.
 *
 * @param thisObj the Dictionary object to operate against.
 * @param pret an Enumeration of the keys in this dictionary.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API Dictionary_keys(HOBJ thisObj, HOBJ *pret);

/**
 * Maps the specified key to the specified value in this dictionary. Neither the key 
 * nor the value can be null.
 *
 * If this dictionary already contains an entry for the specified key, the value 
 * already in this dictionary for that key is returned, after modifying the entry to 
 * contain the new element.
 *
 * If this dictionary does not already have an entry for the specified key, an entry 
 * is created for the specified key and value, and null is returned.
 *
 * The value can be retrieved by calling the get method with a key that is equal to the original key.
 *
 * @param thisObj the Dictionary object to operate against.
 * @param key the hashtable key
 * @param value the value
 * @param pret the previous Object to which the key was mapped in this dictionary, or null if 
 *        the key did not have a previous mapping.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API Dictionary_put(HOBJ thisObj, HOBJ key, HOBJ value, HOBJ *pret);

/**
 * Removes the key (and its corresponding value) from this dictionary.
 * This method does nothing if the key is not in this dictionary.
 *
 * @param thisObj the Dictionary object to operate against.
 * @param key the key that needs to be removed.
 * @param pret the Object to which the key had been mapped in this dictionary, or null if 
 *        the key did not have a mapping 
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API Dictionary_remove(HOBJ thisObj, HOBJ key, HOBJ *pret);

/**
 * Returns the number of entries (distinct keys) in this dictionary.
 *
 * @param thisObj the Dictionary object to operate against.
 * @param pret will contain the number of keys in this dictionary.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API Dictionary_size(HOBJ thisObj, jint *pret);


/*
//---------------------------------
// Enumeration
//---------------------------------
*/
/**
 * Decrements the reference count of the Enumeration object.
 *
 * @param thisObj the Enumeration object to operate against.
 * @param pret will contain the reference count after decrementing.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Enumeration_release(thisObj, pret)                Object_release(thisObj, pret)

/**
 * Get the int corresponding to the type of a given object.
 *
 * @param thisObj the object to operate against.
 * @param pret will contain the int corresponding to the type.
 */
#define Enumeration_getType(thisObj, pret)                Object_getType(thisObj, pret)

/**
 * Determines whether the given object is an instance of the given type.
 *
 * @param thisObj the object to check.
 * @param classtype the type to check against.
 * @param pret non-zero if the object is an instance of the type.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Enumeration_instanceof(thisObj, classtype, pret)  Object_instanceof(thisObj, classtype, pret)

/**
 * Determines whether the two Enumeration objects are the same.
 *
 * @param thisObj the first Enumeration to compare.
 * @param hobj2 the second Enumeration to compare.
 * @param pret true if the Enumeration objects have identical content, false otherwise.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Enumeration_equals(thisObj, hobj2, pret)          Object_equals(thisObj, hobj2, pret)

/**
 * Get the String representation for the given Enumeration.
 *
 * @param thisObj the Enumeration object to operate against.
 * @param pret will contain the String that represents the Enumeration.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Enumeration_toString(thisObj, pret)               Object_toString(thisObj, pret)

/**
 * Returns the int corresponding to the Enumeration type.
 *
 * @return the int corresponding to the Enumeration type
 */
int SMQ_API Enumeration_type();

/**
 * Tests if this Enumeration contains more elements.
 *
 * @param thisObj the Enumeration object to operate against.
 * @param pret true if and only if this enumeration object contains at least one more 
 *        element to provide; false otherwise.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API Enumeration_hasMoreElements(HOBJ thisObj, jboolean *pret);

/**
 * Returns the next element of this enumeration if this enumeration object has at 
 * least one more element to provide.
 *
 * @param thisObj the Enumeration object to operate against.
 * @param pret an Object that is the next element of this enumeration.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API Enumeration_nextElement(HOBJ thisObj, HOBJ *pret);                                                                                                                                 


/*
//---------------------------------
// Hashtable
//---------------------------------
*/
/**
 * Decrements the reference count of the Hashtable object.
 *
 * @param thisObj the Hashtable object to operate against.
 * @param pret will contain the reference count after decrementing.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Hashtable_release(thisObj, pret)                Object_release(thisObj, pret)

/**
 * Get the int corresponding to the type of a given object.
 *
 * @param thisObj the object to operate against.
 * @param pret will contain the int corresponding to the type.
 */
#define Hashtable_getType(thisObj, pret)                Object_getType(thisObj, pret)

/**
 * Determines whether the given object is an instance of the given type.
 *
 * @param thisObj the object to check.
 * @param classtype the type to check against.
 * @param pret non-zero if the object is an instance of the type.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Hashtable_instanceof(thisObj, classtype, pret)  Object_instanceof(thisObj, classtype, pret)

/**
 * Determines whether the two Hashtable objects are the same.
 *
 * @param thisObj the first Hashtable to compare.
 * @param hobj2 the second Hashtable to compare.
 * @param pret true if the Hashtable objects have identical content, false otherwise.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Hashtable_equals(thisObj, hobj2, pret)          Object_equals(thisObj, hobj2, pret)

/**
 * Get the String representation for the given Hashtable.
 *
 * @param thisObj the Hashtable object to operate against.
 * @param pret will contain the String that represents the Hashtable.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Hashtable_toString(thisObj, pret)               Object_toString(thisObj, pret)

/**
 * Create a new Hashtable that is a copy of the given Hashtable.
 *
 * @param thisObj the Hashtable to clone.
 * @param pret will contain the newly created Hashtable.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Hashtable_clone(thisObj, pret)                  Object_clone(thisObj, pret)

/**
 * Retrieves an Enumeration of the values in this Hashtable.
 *
 * @param thisObj the Hashtable object to operate against.
 * @param pret will contain the Enumeration object containing the values in the Hashtable.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Hashtable_elements(thisObj, pret)               Dictionary_elements(thisObj, pret)

/**
 * Retrieves the value to which the key is mapped in this Hashtable.
 *
 * @param thisObj the Hashtable object to operate against.
 * @param key a key in this Hashtable
 * @param pret will contain the object in the Hashtable corresponding to the key.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Hashtable_get(thisObj, key, pret)               Dictionary_get(thisObj, key, pret)

/**
 * Tests if this Hashtable maps no keys to value.
 *
 * @param thisObj the Hashtable object to operate against.
 * @param pret true if this Hashtable maps no keys to values; false otherwise.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Hashtable_isEmpty(thisObj, pret)                Dictionary_isEmpty(thisObj, pret)

/**
 * Returns an Enumeration of the keys in this Hashtable.
 *
 * @param thisObj the Hashtable object to operate against.
 * @param pret an Enumeration of the keys in this Hashtable.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Hashtable_keys(thisObj, pret)                   Dictionary_keys(thisObj, pret)

/**
 * Maps the specified key to the specified value in this Hashtable. Neither the key 
 * nor the value can be null.
 *
 * If this Hashtable already contains an entry for the specified key, the value 
 * already in this dictionary for that key is returned, after modifying the entry to 
 * contain the new element.
 *
 * If this Hashtable does not already have an entry for the specified key, an entry 
 * is created for the specified key and value, and null is returned.
 *
 * The value can be retrieved by calling the get method with a key that is equal to the original key.
 *
 * @param thisObj the Hashtable object to operate against.
 * @param key the hashtable key
 * @param value the value
 * @param pret the previous Object to which the key was mapped in this dictionary, or null if 
 *        the key did not have a previous mapping.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Hashtable_put(thisObj, key, value, pret)        Dictionary_put(thisObj, key, value, pret)

/**
 * Removes the key (and its corresponding value) from this Hashtable.
 * This method does nothing if the key is not in this Hashtable.
 *
 * @param thisObj the Hashtable object to operate against.
 * @param key the key that needs to be removed.
 * @param pret the Object to which the key had been mapped in this Hashtable, or null if 
 *        the key did not have a mapping 
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Hashtable_remove(thisObj, key, pret)            Dictionary_remove(thisObj, key, pret)

/**
 * Returns the number of entries (distinct keys) in this Hashtable.
 *
 * @param thisObj the Hashtable object to operate against.
 * @param pret will contain the number of keys in this Hashtable.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Hashtable_size(thisObj, pret)                   Dictionary_size(thisObj, pret)

/**
 * Returns the int corresponding to the Hashtable type.
 *
 * @return the int corresponding to the Hashtable type
 */
int SMQ_API Hashtable_type();

/**
 * Clears this hashtable so that it contains no keys.
 *
 * @param thisObj the Hashtable object to operate against.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API Hashtable_clear(HOBJ thisObj);

/**
 * Tests if some key maps into the specified value in this hashtable.
 *
 * @param thisObj the Hashtable object to operate against.
 * @param value a value to search for.
 * @param pret will contain true if and only if some key maps to the value argument in this 
 *        hashtable as determined by the equals method; false otherwise.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API Hashtable_contains(HOBJ thisObj, HOBJ value, jboolean *pret);

/**
 * Tests if the specified object is a key in this hashtable.
 *
 * @param thisObj the Hashtable object to operate against.
 * @param key possible key.
 * @param pret will contain true if and only if the specified object is a key in this hashtable, 
 *        as determined by the equals method; false otherwise.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API Hashtable_containsKey(HOBJ thisObj, HOBJ key, jboolean *pret);

/*
//---------------------------------
// NoSuchElementException
//---------------------------------
*/
/**
 * Decrements the reference count of the NoSuchElementException object.
 *
 * @param thisObj the NoSuchElementException object to operate against.
 * @param pret will contain the reference count after decrementing.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define NoSuchElementException_release(thisObj, pret)                   Object_release(thisObj, pret)

/**
 * Get the int corresponding to the type of a given object.
 *
 * @param thisObj the object to operate against.
 * @param pret will contain the int corresponding to the type.
 */
#define NoSuchElementException_getType(thisObj, pret)                   Object_getType(thisObj, pret)

/**
 * Determines whether the given object is an instance of the given type.
 *
 * @param thisObj the object to check.
 * @param classtype the type to check against.
 * @param pret non-zero if the object is an instance of the type.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define NoSuchElementException_instanceof(thisObj, classtype, pret)     Object_instanceof(thisObj, classtype, pret)

/**
 * Determines whether the two NoSuchElementException objects are the same.
 *
 * @param thisObj the first NoSuchElementException to compare.
 * @param hobj2 the second NoSuchElementException to compare.
 * @param pret true if the NoSuchElementExceptions refer have identical content, false otherwise.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define NoSuchElementException_equals(thisObj, hobj2, pret)             Object_equals(thisObj, hobj2, pret)

/**
 * Get the String representation for this NoSuchElementException.
 *
 * @param thisObj the NoSuchElementException object to operate against.
 * @param pret will contain the String that represents the NoSuchElementException.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define NoSuchElementException_toString(thisObj, pret)                  Object_toString(thisObj, pret)

/**
 * Retrieve the String containing the localized message associated w/ this NoSuchElementException.
 *
 * @param thisObj the NoSuchElementException object to operate against.
 * @param pretString will contain the String containing the localized message.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define NoSuchElementException_getLocalizedMessage(thisObj, pretString) Throwable_getLocalizedMessage(thisObj, pretString)

/**
 * Retrieve the String containing the message associated w/ this NoSuchElementException.
 *
 * @param thisObj the NoSuchElementException object to operate against.
 * @param pretString will contain the String containing the message.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define NoSuchElementException_getMessage(thisObj, pretString)          Throwable_getMessage(thisObj, pretString)

/**
 * Prints a stack trace w/ to the error location corresponding to the NoSuchElementException object.
 *
 * @param thisObj the NoSuchElementException object to operate against.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define NoSuchElementException_printStackTrace(thisObj)                 Throwable_printStackTrace(thisObj)

/**
 * Returns the int corresponding to the NoSuchElementException type.
 *
 * @return the int corresponding to the NoSuchElementException type
 */
int SMQ_API NoSuchElementException_type();


/*
//-------------------------------------------------------------------------
// progress/message/jclient
//-------------------------------------------------------------------------
*/

/*
//---------------------------------
// BrokerName
//---------------------------------
*/
/**
 * Decrements the reference count of the BrokerName object.
 *
 * @param thisObj the BrokerName object to operate against.
 * @param pret will contain the reference count after decrementing.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define BrokerName_release(thisObj, pret)                Object_release(thisObj, pret)

/**
 * Get the int corresponding to the type of a given object.
 *
 * @param thisObj the object to operate against.
 * @param pret will contain the int corresponding to the type.
 */
#define BrokerName_getType(thisObj, pret)                Object_getType(thisObj, pret)

/**
 * Determines whether the given object is an instance of the given type.
 *
 * @param thisObj the object to check.
 * @param classtype the type to check against.
 * @param pret non-zero if the object is an instance of the type.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define BrokerName_instanceof(thisObj, classtype, pret)  Object_instanceof(thisObj, classtype, pret)

/**
 * Determines whether the two BrokerName objects are the same.
 *
 * @param thisObj the first BrokerName to compare.
 * @param hobj2 the second BrokerName to compare.
 * @param pret true if the BrokerName objects have identical content, false otherwise.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define BrokerName_equals(thisObj, hobj2, pret)          Object_equals(thisObj, hobj2, pret)

/**
 * Get the String representation for the given BrokerName.
 *
 * @param thisObj the BrokerName object to operate against.
 * @param pret will contain the String that represents the BrokerName.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define BrokerName_toString(thisObj, pret)               Object_toString(thisObj, pret)

/** Constructs a BrokerName object
 *
 * @param brokerURL the URL (in the form [protocol://]hostname[:port]) 
 * of the message broker to which connections are made.
 * The constructor parses the URL and sets all the attributes
 *
 * @param pBrokerName pointer to the BrokerName instance created
 * @return SMQ_SUCCESS (0) if successful, non-zero if error (e.g. brokerURL is not valid).
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API BrokerName_create(HOBJ brokerURL, HOBJ* pBrokerName);

/** Constructs a BrokerName object
 *
 * @param brokerHostName the broker host name, can be null or ""
 * @param brokerPort the broker port number, can be null or ""
 * @param brokerProtocol the broker protocol, can be null or ""
 * @param pBrokerName the BrokerName instance created
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error (e.g., invalid protocol).
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API BrokerName_create2(HOBJ brokerHostName, jint brokerPort, HOBJ brokerProtocol, HOBJ* pBrokerName);

/**
 * Returns the int corresponding to the BrokerName type.
 *
 * @return the int corresponding to the BrokerName type
 */
int SMQ_API BrokerName_type();

/**
 * Retrieve the hostname of a BrokerName object.
 *
 * @param thisObj the BrokerName object to operate against.
 * @param pHostName a String object containing the hostname.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error (e.g., invalid protocol).
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API BrokerName_getBrokerHostName(HOBJ thisObj, HOBJ* pHostName);

/**
 * Retrieve the port number of a BrokerName object.
 *
 * @param thisObj the BrokerName object to operate against.
 * @param pBrokerPort will contain the port number.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error (e.g., invalid protocol).
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API BrokerName_getBrokerPort(HOBJ thisObj, jint* pBrokerPort);

/**
 * Retrieve the protocol for a BrokerName object.
 *
 * @param thisObj the BrokerName object to operate against.
 * @param pBrokerProtocol a String object containing the protocol.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error (e.g., invalid protocol).
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API BrokerName_getBrokerProtocol(HOBJ thisObj, HOBJ* pBrokerProtocol);

/**
 * Retrieve the URL for a BrokerName object.
 *
 * @param thisObj the BrokerName object to operate against.
 * @param pBrokerURL a String object containing the URL.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error (e.g., invalid protocol).
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API BrokerName_getBrokerURL(HOBJ thisObj, HOBJ* pBrokerURL);

/*
//---------------------------------
// BytesMessage
//---------------------------------
*/
/**
 * Decrements the reference count of the BytesMessage object.
 *
 * @param thisObj the BytesMessage object to operate against.
 * @param pret will contain the reference count after decrementing.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define BytesMessage_release(thisObj, pret)                     Object_release(thisObj, pret)

/**
 * Get the int corresponding to the type of a given object.
 *
 * @param thisObj the object to operate against.
 * @param pret will contain the int corresponding to the type.
 */
#define BytesMessage_getType(thisObj, pret)                     Object_getType(thisObj, pret)

/**
 * Determines whether the given object is an instance of the given type.
 *
 * @param thisObj the object to check.
 * @param classtype the type to check against.
 * @param pret non-zero if the object is an instance of the type.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define BytesMessage_instanceof(thisObj, classtype, pret)       Object_instanceof(thisObj, classtype, pret)

/**
 * Determines whether the two BytesMessage objects are the same.
 *
 * @param thisObj the first BytesMessage to compare.
 * @param hobj2 the second BytesMessage to compare.
 * @param pret true if the BytesMessage objects have identical content, false otherwise.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define BytesMessage_equals(thisObj, hobj2, pret)               Object_equals(thisObj, hobj2, pret)

/**
 * Get the String representation for the given BytesMessage.
 *
 * @param thisObj the BytesMessage object to operate against.
 * @param pret will contain the String that represents the BytesMessage.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define BytesMessage_toString(thisObj, pret)                    Object_toString(thisObj, pret)

/**
 * Create a new BytesMessage that is a copy of the given BytesMessage.
 *
 * @param thisObj the BytesMessage to clone.
 * @param pret will contain the newly created BytesMessage.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define BytesMessage_clone(thisObj, pret)                       Object_clone(thisObj, pret)

/** Get the message ID.
  *
  * <P>The messageID header field contains a value that uniquely
  * identifies each message sent by a SonicMQ client.
  *
  * <P>When a message is sent, messageID can be ignored. When
  * the send method returns it contains an assigned value.
  *
  * <P>A JMSMessageID is a String value which should function as a
  * unique key for identifying messages in a historical repository.
  *
  * <P>All JMSMessageID values must start with the prefix 'ID:'.
  * Uniqueness of message ID values across different brokers is
  * not required.
  *
  * <P>Since message ID's take some effort to create and increase a
  * message's size, SonicMQ can optimize message
  * overhead if it is given a hint that message ID is not used by
  * an application. SonicMQ message Producers provide a hint to disable
  * message ID. When a client sets a Producer to disable message ID
  * they are saying that they do not depend on the value of message
  * ID for the messages it produces. These messages must either have
  * message ID set to null or, if the hint is ignored, messageID must
  * be set to its normal unique value.
  *
  * @param thisObj the BytesMessage to operate against
  * @param pret the returned message ID
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  *
  * @see BytesMessage_setJMSMessageID()
  */
#define BytesMessage_getJMSMessageID(thisObj, pret)             Message_getJMSMessageID(thisObj, pret)

/** Set the message ID.
  *
  * <P>The SonicMQ run time sets this field when a message is sent. This operation
  * can be used to change the value of a message that has been received.
  *
  * @param thisObj the BytesMessage to operate against
  * @param value the value to set the message ID to
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  *
  * @see BytesMessage_getJMSMessageID()
  */
#define BytesMessage_setJMSMessageID(thisObj, value)            Message_setJMSMessageID(thisObj, value)

/** Get the message timestamp.
  *
  * <P>The JMSTimestamp header field contains the time a message was
  * handed off to the SonicMQW client run time to be sent. It is not the time the
  * message was actually transmitted because the actual send may occur
  * later due to transactions or other client side queueing of messages.
  *
  * <P>When a message is sent, JMSTimestamp is ignored. When the send
  * method returns it contains a a time value somewhere in the interval
  * between the call and the return. It is in the format of a normal
  * Java millis time value.
  *
  * <P>Since timestamps take some effort to create and increase a
  * message's size, SonicMQ can optimize message
  * overhead if it is given a hint that timestamp is not used by an
  * application. SonicMQ message Producers provide a hint to disable
  * timestamps. When a client sets a producer to disable timestamps
  * they are saying that they do not depend on the value of timestamp
  * for the messages it produces. These messages must either have
  * timestamp set to null or, if the hint is ignored, timestamp must
  * be set to its normal value.
  *
  * @param thisObj the BytesMessage to operate against
  * @param pret the message timestamp value
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  *
  * @see BytesMessage_setJMSTimestamp()
  */
#define BytesMessage_getJMSTimestamp(thisObj, pret)             Message_getJMSTimestamp(thisObj, pret)

/** Set the message timestamp.
  *
  * <P>SonicMQ sets this field when a message is sent. This operation
  * can be used to change the value of a message that's been received.
  *
  * @param thisObj the BytesMessage to operate against
  * @param value the timestamp for this message
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  *
  * @see BytesMessage_getJMSTimestamp()
  */
#define BytesMessage_setJMSTimestamp(thisObj, value)            Message_setJMSTimestamp(thisObj, value)

/** Get the correlation ID for the message.
  *
  * <P>This method is used to return correlation id values that are
  * either SonicMQ-specific message ID's or application-specific Strings.
  *
  * @param thisObj the BytesMessage to operate against
  * @param pret the correlation ID of a message as a String.
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  *
  * @see BytesMessage_setJMSCorrelationID()
  * @see BytesMessage_getJMSCorrelationIDAsBytes()
  * @see BytesMessage_setJMSCorrelationIDAsBytes()
  */
#define BytesMessage_getJMSCorrelationID(thisObj, pret)         Message_getJMSCorrelationID(thisObj, pret)

/** Set the correlation ID for the message.
  *
  * <P>A client can use the JMSCorrelationID header field to link one
  * message with another. A typically use is to link a response message
  * with its request message.
  *
  * <P>JMSCorrelationID can hold one of the following:
  *    <UL>
  *      <LI>A SonicMQ-specific message ID
  *      <LI>An application-specific String
  *      <LI>A native jbyteArray value.
  *    </UL>
  *
  * <P>Since each message sent in SonicMQ is assigned a message ID
  * value it is convenient to link messages via message ID. All message ID
  * values must start with the 'ID:' prefix.
  *
  * <P>In some cases, an application (made up of several clients) needs to
  * use an application specific value for linking messages. For instance,
  * an application may use JMSCorrelationID to hold a value referencing
  * some external information. Application specified values must not start
  * with the 'ID:' prefix; this is reserved for SonicMQ-generated message
  * ID values.
  *
  * <P>If a provider supports the native concept of correlation ID, a SonicMQ
  * client may need to assign specific JMSCorrelationID values to match
  * those expected by non-JMS clients. A jbyteArray value is used for this
  * purpose. JMS providers without native correlation ID values are not
  * required to support jbyteArray values. The use of a jbyteArray value for
  * JMSCorrelationID is non-portable.
  *
  * @param thisObj the BytesMessage to operate against
  * @param value the message ID of a message being referred to.
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  *
  * @see BytesMessage_getJMSCorrelationID()
  * @see BytesMessage_getJMSCorrelationIDAsBytes()
  * @see BytesMessage_setJMSCorrelationIDAsBytes()
  */
#define BytesMessage_setJMSCorrelationID(thisObj, value)        Message_setJMSCorrelationID(thisObj, value)

/** Get the correlation ID as an array of bytes for the message.
  *
  * <P>The use of a jbyteArray value for JMSCorrelationID is non-portable.
  *
  * @param thisObj the BytesMessage to operate against
  * @param pret the correlation ID of a message as an array of bytes.
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  *
  * @see BytesMessage_setJMSCorrelationID()
  * @see BytesMessage_getJMSCorrelationID()
  */
#define BytesMessage_getJMSCorrelationIDAsBytes(thisObj, pret)  Message_getJMSCorrelationIDAsBytes(thisObj, pret)

/** Set the correlation ID as an array of bytes for the message.
  *
  * <P>If a provider supports the native concept of correlation id, a
  * SonicMQ client may need to assign specific JMSCorrelationID values to
  * match those expected by non-JMS clients. JMS providers without native
  * correlation id values are not required to support this (and the
  * corresponding get) method; their implementation may throw
  * java.lang.UnsupportedOperationException).
  *
  * <P>The use of a jbyteArray value for JMSCorrelationID is non-portable.
  *
  * @param thisObj the BytesMessage to operate against
  * @param value the correlation ID value as an array of bytes.
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  *
  * @see BytesMessage_setJMSCorrelationID()
  * @see BytesMessage_getJMSCorrelationID()
  * @see BytesMessage_getJMSCorrelationIDAsBytes()
  */
#define BytesMessage_setJMSCorrelationIDAsBytes(thisObj, value) Message_setJMSCorrelationIDAsBytes(thisObj, value)

/** Get where a reply to this message should be sent.
  *
  * @param thisObj the BytesMessage to operate against
  * @param pret the destination to send a response to this message
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  *
  * @see BytesMessage_setJMSReplyTo()
  */
#define BytesMessage_getJMSReplyTo(thisObj, pret)               Message_getJMSReplyTo(thisObj, pret)

/** Set where a reply to this message should be sent.
  *
  * <P>The replyTo header field contains the destination where a reply
  * to the current message should be sent. If it is null no reply is
  * expected. The destination may be either a Queue or a Topic.
  *
  * <P>Messages with a null replyTo value are called SonicMQ datagrams.
  * Datagrams may be a notification of some change in the sender (i.e.
  * they signal a sender event) or they may just be some data the sender
  * thinks is of interest.
  *
  * Messages with a replyTo value are typically expecting a response.
  * A response may be optional, it is up to the client to decide. These
  * messages are called SonicMQ requests. A message sent in response to a
  * request is called a reply.
  *
  * In some cases a client may wish to match up a request it sent earlier
  * with a reply it has just received. This can be done using the
  * correlationID.
  *
  * @param thisObj the BytesMessage to operate against
  * @param value Destination to send a response to this message
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  *
  * @see BytesMessage_getJMSReplyTo()
  */
#define BytesMessage_setJMSReplyTo(thisObj, value)              Message_setJMSReplyTo(thisObj, value)

/** Get the destination for this message.
  *
  * <P>The destination field contains the destination to which the
  * message is being sent.
  *
  * <P>When a message is sent this value is ignored. After completion
  * of the send method it holds the destination specified by the send.
  *
  * <P>When a message is received, its destination value must be
  * equivalent to the value assigned when it was sent.
  *
  * @param thisObj the BytesMessage to operate against
  * @param pret the destination of this message.
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  *
  * @see BytesMessage_setJMSDestination()
  */
#define BytesMessage_getJMSDestination(thisObj, pret)           Message_getJMSDestination(thisObj, pret)

/** Set the destination for this message.
  *
  * <P>SonicMQ sets this field when a message is sent. This operation
  * can be used to change the value of a message that's been received.
  *
  * @param thisObj the BytesMessage to operate against
  * @param value the destination for this message.
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  *
  * @see BytesMessage_getJMSDestination()
  */
#define BytesMessage_setJMSDestination(thisObj, value)          Message_setJMSDestination(thisObj, value)

/** Get the delivery mode for this message.
  *
  * @param thisObj the BytesMessage to operate against
  * @param pret the delivery mode of this message.
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  *
  * @see BytesMessage_setJMSDeliveryMode()
  */
#define BytesMessage_getJMSDeliveryMode(thisObj, pret)          Message_getJMSDeliveryMode(thisObj, pret)

/**
 * Set the delivery mode for this message.
 *
 * <p>Providers set this field when a message is sent. This operation
 * can be used to change the value of a message that's been received.
 *
 * @param thisObj the BytesMessage to operate against
 * @param value the delivery mode for this message.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @see BytesMessage_setJMSDeliveryMode()
 *
 * @exception JMSException if JMS fails to set JMS DeliveryMode
 *                         due to some internal JMS error.
 */
#define BytesMessage_setJMSDeliveryMode(thisObj, value)         Message_setJMSDeliveryMode(thisObj, value)

/**
 * Get an indication of whether this message is being redelivered.
 *
 * <p>If a client receives a message with the redelivered indicator set,
 * it is likely, but not guaranteed, that this message was delivered to
 * the client earlier but the client did not acknowledge its receipt at
 * that earlier time.
 *
 * @param thisObj the BytesMessage to operate against
 * @param pret set to true if this message is being redelivered.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @see BytesMessage_setJMSDeliveryMode()
 *
 *
 * @exception JMSException if JMS fails to get JMS Redelivered flag
 *                         due to some internal JMS error.
 */
#define BytesMessage_getJMSRedelivered(thisObj, pret)           Message_getJMSRedelivered(thisObj, pret)

/**
 * Set to indicate whether this message is being redelivered.
 *
 * <p>This field is set at the time the message is delivered. This
 * operation can be used to change the value of a message that's
 * been received.
 *
 * @param thisObj the BytesMessage to operate against
 * @param value an indication of whether this message is being redelivered.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @see BytesMessage_setJMSDeliveryMode()
 *
 * @exception JMSException if JMS fails to set JMS Redelivered flag
 *                         due to some internal JMS error.
 */
#define BytesMessage_setJMSRedelivered(thisObj, value)          Message_setJMSRedelivered(thisObj, value)

/**
 * Get the message type.
 *
 * @param thisObj the BytesMessage to operate against
 * @param pret the message type
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to get JMS message type
 *                         due to some internal JMS error.
 */
#define BytesMessage_getJMSType(thisObj, pret)                  Message_getJMSType(thisObj, pret)

/** Set the message type.
 *
 * <p>Some JMS providers use a message repository that contains the
 * definition of messages sent by applications. The type header field
 * contains the name of a message's definition.
 *
 * <p>JMS does not define a standard message definition repository nor
 * does it define a naming policy for the definitions it contains. JMS
 * clients should use symbolic values for type that can be configured
 * at installation time to the values defined in the current providers
 * message repository.
 *
 * <p>JMS clients should assign a value to type whether the application
 * makes use of it or not. This insures that it is properly set for
 * those providers that require it.
 *
 * @param thisObj the BytesMessage to operate against
 * @param value the class of message
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to set JMS message type
 *                         due to some internal JMS error.
 */
#define BytesMessage_setJMSType(thisObj, value)                 Message_setJMSType(thisObj, value)

/**
 * Get the message's expiration value.
 *
 * <p>When a message is sent, expiration is left unassigned. After
 * completion of the send method, it holds the expiration time of the
 * message. This is the sum of the time-to-live value specified by the
 * client and the GMT at the time of the send.
 *
 * <p>If the time-to-live is specified as zero, expiration is set to
 * zero which indicates the message does not expire.
 *
 * <p>When a message's expiration time is reached, a provider should
 * discard it. JMS does not define any form of notification of message
 * expiration.
 *
 * <p>Clients should not receive messages that have expired; however,
 * JMS does not guarantee that this will not happen.
 *
 * @param thisObj the BytesMessage to operate against
 * @param pret the time the message expires. It is the sum of the
 * time-to-live value specified by the client, and the GMT at the
 * time of the send.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to get JMS message expiration
 *                         due to some internal JMS error.
 */
#define BytesMessage_getJMSExpiration(thisObj, pret)            Message_getJMSExpiration(thisObj, pret)

/**
 * Set the message's expiration value.
 *
 * <p>Providers set this field when a message is sent. This operation
 * can be used to change the value of a message that's been received.
 *
 * @param thisObj the BytesMessage to operate against
 * @param value the message's expiration time
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to set JMS message expiration
 *                         due to some internal JMS error.
 */
#define BytesMessage_setJMSExpiration(thisObj, value)           Message_setJMSExpiration(thisObj, value)

/**
 * Get the message priority.
 *
 * <p>JMS defines a ten level priority value with 0 as the lowest
 * priority and 9 as the highest. In addition, clients should consider
 * priorities 0-4 as gradations of normal priority and priorities 5-9
 * as gradations of expedited priority.
 *
 * <p>JMS does not require that a provider strictly implement priority
 * ordering of messages; however, it should do its best to deliver
 * expedited messages ahead of normal messages.
 *
 * @param thisObj the BytesMessage to operate against
 * @param pret the default message priority
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to get JMS message priority
 *                         due to some internal JMS error.
 */
#define BytesMessage_getJMSPriority(thisObj, pret)              Message_getJMSPriority(thisObj, pret)

/**
 * Set the priority for this message.
 *
 * <p>Providers set this field when a message is sent. This operation
 * can be used to change the value of a message that's been received.
 *
 * @param thisObj the BytesMessage to operate against
 * @param value the priority of this message
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to set JMS message priority
 *                         due to some internal JMS error.
 */
#define BytesMessage_setJMSPriority(thisObj, value)             Message_setJMSPriority(thisObj, value)

/**
 * Clear a message's properties.
 *
 * @param thisObj the BytesMessage to operate against
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to clear JMS message
 *                         properties due to some internal JMS
 *                         error.
 */
#define BytesMessage_clearProperties(thisObj)                   Message_clearProperties(thisObj)

/** Check if a property value exists.
 *
 * @param thisObj the BytesMessage to operate against
 * @param name the name of the property to test
 * @param pret true if the property does exist.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to  check if property
 *                         exists due to some internal JMS
 *                         error.
 */
#define BytesMessage_propertyExists(thisObj, name, pret)        Message_propertyExists(thisObj, name, pret)

/**
 * Return the boolean property value with the given name.
 *
 * @param thisObj the BytesMessage to operate against
 * @param name the name of the boolean property
 * @param pret the boolean property value with the given name.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to  get Property due to
 *                         some internal JMS error.
 * @exception MessageFormatException if this type conversion is invalid.
 * @exception NullPointerException if property does not exist.
 */
#define BytesMessage_getBooleanProperty(thisObj, name, pret)    Message_getBooleanProperty(thisObj, name, pret)

/**
 * Return the byte property value with the given name.
 *
 * @param thisObj the BytesMessage to operate against
 * @param name the name of the byte property
 * @param pret the byte property value with the given name.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to  get Property due to
 *                         some internal JMS error.
 * @exception MessageFormatException if this type conversion is invalid.
 * @exception NullPointerException if property does not exist.
 */
#define BytesMessage_getByteProperty(thisObj, name, pret)       Message_getByteProperty(thisObj, name, pret)

/**
 * Return the short property value with the given name.
 *
 * @param thisObj the BytesMessage to operate against
 * @param name the name of the short property
 * @param pret the short property value with the given name.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to  get Property due to
 *                         some internal JMS error.
 * @exception MessageFormatException if this type conversion is invalid.
 * @exception NullPointerException if property does not exist.
 */
#define BytesMessage_getShortProperty(thisObj, name, pret)      Message_getShortProperty(thisObj, name, pret)

/**
 * Return the integer property value with the given name.
 *
 * @param thisObj the BytesMessage to operate against
 * @param name the name of the integer property
 * @param pret the integer property value with the given name.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to  get Property due to
 *                         some internal JMS error.
 * @exception MessageFormatException if this type conversion is invalid.
 * @exception NullPointerException if property does not exist.
 */
#define BytesMessage_getIntProperty(thisObj, name, pret)        Message_getIntProperty(thisObj, name, pret)

/**
 * Return the long property value with the given name.
 *
 * @param thisObj the BytesMessage to operate against
 * @param name the name of the long property
 * @param pret the long property value with the given name.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to  get Property due to
 *                         some internal JMS error.
 * @exception MessageFormatException if this type conversion is invalid.
 * @exception NullPointerException if property does not exist.
 */
#define BytesMessage_getLongProperty(thisObj, name, pret)       Message_getLongProperty(thisObj, name, pret)

/**
 * Return the float property value with the given name.
 *
 * @param thisObj the BytesMessage to operate against
 * @param name the name of the float property
 * @param pret the float property value with the given name.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to  get Property due to
 *                         some internal JMS error.
 * @exception MessageFormatException if this type conversion is invalid.
 * @exception NullPointerException if property does not exist.
 */
#define BytesMessage_getFloatProperty(thisObj, name, pret)      Message_getFloatProperty(thisObj, name, pret)

/**
 * Return the double property value with the given name.
 *
 * @param thisObj the BytesMessage to operate against
 * @param name the name of the double property
 * @param pret the double property value with the given name.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to  get Property due to
 *                         some internal JMS error.
 * @exception MessageFormatException if this type conversion is invalid.
 * @exception NullPointerException if property does not exist.
 */
#define BytesMessage_getDoubleProperty(thisObj, name, pret)     Message_getDoubleProperty(thisObj, name, pret)

/**
 * Return the String property value with the given name.
 *
 * @param thisObj the BytesMessage to operate against
 * @param name the name of the String property
 * @param pret the String property value with the given name.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to  get Property due to
 *                         some internal JMS error.
 * @exception MessageFormatException if this type conversion is invalid.
 * @exception NullPointerException if property does not exist.
 */
#define BytesMessage_getStringProperty(thisObj, name, pret)     Message_getStringProperty(thisObj, name, pret)

/**
 * Return the Object property value with the given name.
 *
 * @param thisObj the BytesMessage to operate against
 * @param name the name of the Object property
 * @param pret the Object property value with the given name.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to  get Property due to
 *                         some internal JMS error.
 * @exception MessageFormatException if this type conversion is invalid.
 * @exception NullPointerException if property does not exist.
 */
#define BytesMessage_getObjectProperty(thisObj, name, pret)     Message_getObjectProperty(thisObj, name, pret)

/**
 * Return an Enumeration of all the property names.
 *
 * @param thisObj the BytesMessage to operate against
 * @param pret an Enumeration w/ all the names of property values.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to  get Property names due to
 *                         some internal JMS error.
 */
#define BytesMessage_getPropertyNames(thisObj, pret)            Message_getPropertyNames(thisObj, pret)

/**
 * Set a boolean property value with the given name, into the Message.
 *
 * @param thisObj the BytesMessage to operate against
 * @param name the name of the boolean property
 * @param value the boolean property value to set in the Message.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to  set Property due to
 *                         some internal JMS error.
 * @exception MessageNotWriteableException if properties are read-only
 */
#define BytesMessage_setBooleanProperty(thisObj, name, value)   Message_setBooleanProperty(thisObj, name, value)

/**
 * Set a byte property value with the given name, into the Message.
 *
 * @param thisObj the BytesMessage to operate against
 * @param name the name of the byte property
 * @param value the byte property value to set in the Message.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to  set Property due to
 *                         some internal JMS error.
 * @exception MessageNotWriteableException if properties are read-only
 */
#define BytesMessage_setByteProperty(thisObj, name, value)      Message_setByteProperty(thisObj, name, value)

/**
 * Set a short property value with the given name, into the Message.
 *
 * @param thisObj the BytesMessage to operate against
 * @param name the name of the short property
 * @param value the short property value to set in the Message.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to  set Property due to
 *                         some internal JMS error.
 * @exception MessageNotWriteableException if properties are read-only
 */
#define BytesMessage_setShortProperty(thisObj, name, value)     Message_setShortProperty(thisObj, name, value)

/**
 * Set a integer property value with the given name, into the Message.
 *
 * @param thisObj the BytesMessage to operate against
 * @param name the name of the integer property
 * @param value the integer property value to set in the Message.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to  set Property due to
 *                         some internal JMS error.
 * @exception MessageNotWriteableException if properties are read-only
 */
#define BytesMessage_setIntProperty(thisObj, name, value)       Message_setIntProperty(thisObj, name, value)

/**
 * Set a long property value with the given name, into the Message.
 *
 * @param thisObj the BytesMessage to operate against
 * @param name the name of the long property
 * @param value the long property value to set in the Message.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to  set Property due to
 *                         some internal JMS error.
 * @exception MessageNotWriteableException if properties are read-only
 */
#define BytesMessage_setLongProperty(thisObj, name, value)      Message_setLongProperty(thisObj, name, value)

/**
 * Set a float property value with the given name, into the Message.
 *
 * @param thisObj the BytesMessage to operate against
 * @param name the name of the float property
 * @param value the float property value to set in the Message.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to  set Property due to
 *                         some internal JMS error.
 * @exception MessageNotWriteableException if properties are read-only
 */
#define BytesMessage_setFloatProperty(thisObj, name, value)     Message_setFloatProperty(thisObj, name, value)

/**
 * Set a double property value with the given name, into the Message.
 *
 * @param thisObj the BytesMessage to operate against
 * @param name the name of the double property
 * @param value the double property value to set in the Message.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to  set Property due to
 *                         some internal JMS error.
 * @exception MessageNotWriteableException if properties are read-only
 */
#define BytesMessage_setDoubleProperty(thisObj, name, value)    Message_setDoubleProperty(thisObj, name, value)

/**
 * Set a String property value with the given name, into the Message.
 *
 * @param thisObj the BytesMessage to operate against
 * @param name the name of the String property
 * @param value the String property value to set in the Message.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to  set Property due to
 *                         some internal JMS error.
 * @exception MessageNotWriteableException if properties are read-only
 */
#define BytesMessage_setStringProperty(thisObj, name, value)    Message_setStringProperty(thisObj, name, value)

/**
 * Set an Object property value with the given name, into the Message.
 *
 * @param thisObj the BytesMessage to operate against
 * @param name the name of the Object property
 * @param value the Object property value to set in the Message.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to  set Property due to
 *                         some internal JMS error.
 * @exception MessageNotWriteableException if properties are read-only
 */
#define BytesMessage_setObjectProperty(thisObj, name, value)    Message_setObjectProperty(thisObj, name, value)

/**
 * Acknowledge this or all previous messages received by the session.
 * 
 * <p>All JMS messages support the acknowledge() method for use when a client has specified
 * that a JMS session's messages are to be explicitly acknowledged. A client requests explicit
 * acknowledgement by creating a session with either the standard JMS CLIENT_ACKKNOWLEDGE mode,
 * or the non-JMS acknowledgement mode SINGLE_MESSAGE_ACKNOWLEDGE.
 *
 * <p>In the standard JMS CLIENT_ACKNOWLEDGE mode, all messages previously received for the
 * session are acknowledged. If the session has been created with SINGLE_MESSAGE_ACKNOWLEDGE,
 * only the current message is acknowledged.
 *
 * <p>JMS defaults to implicit message acknowledgement, AUTO_ACKNOWLEDGE. In this mode,
 * calls to acknowledge() are ignored, as JMS automatically acknowledges messages on behalf
 * of the client.
 *
 * <p>Messages that have been received but not acknowledged may be
 * redelivered to the consumer.
 *
 * @param thisObj the BytesMessage to operate against
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to acknowledge due to some
 *                         internal JMS error.
 */
#define BytesMessage_acknowledge(thisObj)                       Message_acknowledge(thisObj)

/**
 * Clear out the message body. All other parts of the message are left
 * untouched.
 *
 * @param thisObj the BytesMessage to operate against
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to due to some internal JMS error.
 */
#define BytesMessage_clearBody(thisObj)                         Message_clearBody(thisObj)

/**
 * Get all message properties (Progress-specific message interface extensions) in a 
 *  Hashtable object. Note that both the name and the value of a Hashtable entry
 *  will be objects. E.g. a boolean property of "true" w/ the name "isPublic" will 
 *  be represented in the Hashtable as an entry w/ the String object "isPublic" as 
 *  the key and the Boolean object w/ value "true" as the value. Use the appropriate 
 *  API functions to access the values of the objectified values.
 *
 * @param bytesMsg the BytesMessage whose properties to retrieve
 * @param pret a Hashtable object containing the message properties as 
 *        key-value pairs (all objects)
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define BytesMessage_getProperties(bytesMsg, pret)              Message_getProperties(bytesMsg, pret)

/**
 * Returns the int corresponding to the BytesMessage type.
 *
 * @return the int corresponding to the BytesMessage type
 */
int SMQ_API BytesMessage_type();

/**
 * Create a new BytesMessage.
 *
 * @param pret the newly created BytesMessage object.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error (e.g., invalid protocol).
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API BytesMessage_create(HOBJ *pret);

/** Read a <code>jboolean</code> from the stream message.
  *
  * @param bytesMsg the BytesMessage to operate against
  * @param pBoolean pointer to the <code>jboolean</code> return value
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Likely causes for errors:
  *           MessageNotReadable if message is in write-only mode
  *           MessageEOFException if end of message stream
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  */
int SMQ_API BytesMessage_readBoolean(HOBJ bytesMsg, jboolean* pBoolean); 

/** Read a signed 8-bit value from the stream message.
  *
  * @param bytesMessage the BytesMessage to operate against
  * @param pJbyte the next <code>jbyte</code> from the stream message.
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Likely causes for errors:
  *           MessageNotReadable if message is in write-only mode
  *           MessageEOFException if end of message stream
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  */ 
int SMQ_API BytesMessage_readByte(HOBJ bytesMsg, jbyte* pJbyte);

/** Read an unsigned 8-bit number from the stream message.
  * 
  * @param bytesMsg the BytesMessage to operate against
  * @param pJint the next byte from the stream message, interpreted as a <code>jint</code> (an
  * unsigned 8-bit number).
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Likely causes for errors:
  *           MessageNotReadable if message is in write-only mode
  *           MessageEOFException if end of message stream
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  */
int SMQ_API BytesMessage_readUnsignedByte(HOBJ bytesMsg, jint* pJint);

/** Read a signed 16-bit number from the stream message.
  *
  * @param bytesMessage the BytesMessage to operate against
  * @param pJshort the next two bytes from the stream message, interpreted as a
  * <code>jshort</code> (signed 16-bit number).
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Likely causes for errors:
  *           MessageNotReadable if message is in write-only mode
  *           MessageEOFException if end of message stream
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  */ 
int SMQ_API BytesMessage_readShort(HOBJ bytesMsg, jshort* pJshort);

/** Read an unsigned 16-bit number from the stream message.
  *  
  * @param bytesMessage the BytesMessage to operate against
  * @param pJint the next two bytes from the stream message, interpreted as an
  * <code>jint</code> (unsigned 16-bit integer).
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Likely causes for errors:
  *           MessageNotReadable if message is in write-only mode
  *           MessageEOFException if end of message stream
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  */ 
int SMQ_API BytesMessage_readUnsignedShort(HOBJ bytesMsg, jint* pJint);

/** Read a Unicode character value from the stream message.
  *
  * @param bytesMessage the BytesMessage to operate against
  * @param the next two bytes from the stream message as a <code>jchar</code> (Unicode
  * character).
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Likely causes for errors:
  *           MessageNotReadable if message is in write-only mode
  *           MessageEOFException if end of message stream
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  */ 
int SMQ_API BytesMessage_readChar(HOBJ bytesMsg, jchar* pJchar);

/** Read a signed 32-bit integer from the stream message.
  *
  * @param bytesMsg the BytesMessage to operate against
  * @param pJint the next four bytes from the stream message, interpreted as
  * an <code>int</code>.
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Likely causes for errors:
  *           MessageNotReadable if message is in write-only mode
  *           MessageEOFException if end of message stream
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  */ 
int SMQ_API BytesMessage_readInt(HOBJ bytesMsg, jint* pJint);

/** Read a signed 64-bit integer from the stream message.
  *
  * @param bytesMsg the BytesMessage to operate against
  * @param pJlong the next eight bytes from the stream message, interpreted as
  * a <code>jlong</code>.
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Likely causes for errors:
  *           MessageNotReadable if message is in write-only mode
  *           MessageEOFException if end of message stream
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  */ 
int SMQ_API BytesMessage_readLong(HOBJ bytesMsg, jlong* pJlong);

/** Read a <code>jfloat</code> from the stream message.
  *
  * @param bytesMsg the BytesMessage to operate against
  * @param pJfloat the next four bytes from the stream message, interpreted as
  * a <code>jfloat</code>.
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Likely causes for errors:
  *           MessageNotReadable if message is in write-only mode
  *           MessageEOFException if end of message stream
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  */ 
int SMQ_API BytesMessage_readFloat(HOBJ bytesMsg, jfloat* pJfloat);

/** Read a <code>jdouble</code> from the stream message
  *
  * @param bytesMessage the BytesMessage to operate against
  * @param pJdouble the next eight bytes from the stream message, interpreted as
  * a <code>jdouble</code>.
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Likely causes for errors:
  *           MessageNotReadable if message is in write-only mode
  *           MessageEOFException if end of message stream
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  */ 
int SMQ_API BytesMessage_readDouble(HOBJ bytesMsg, jdouble* pJdouble);

/** Read in a string that has been encoded using a modified UTF-8
  * format from the stream message.
  *
  * <P>For more information on the UTF-8 format, see "File System Safe
  * UCS Transformation Format (FSS_UTF)", X/Open Preliminary Specification,
  * X/Open Company Ltd., Document Number: P316. This information also
  * appears in ISO/IEC 10646, Annex P.
  *
  * @param bytesMessage the BytesMessage to operate against
  * @param pUTF a Unicode string from the stream message.
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Likely causes for errors:
  *           MessageNotReadable if message is in write-only mode
  *           MessageEOFException if end of message stream
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  */ 
int SMQ_API BytesMessage_readUTF(HOBJ bytesMsg, HOBJ* pUTF);

/** Read a byte array from the stream message.
  *
  * @param bytesMessage the BytesMessage to operate against
  * @param value the buffer into which the data is read.
  * @param pNumBytes the total number of bytes read into the buffer, or -1 if 
  * there is no more data because the end of the stream has been reached.
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Likely causes for errors:
  *           MessageNotReadable if message is in write-only mode
  *           MessageEOFException if end of message stream
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  */ 
int SMQ_API BytesMessage_readBytes(HOBJ bytesMsg, HOBJ value, jint* pNumBytes); 

/** Read a portion of the bytes message.
  *  
  * @param bytesMsg the BytesMessage to operate against
  * @param value the buffer into which the data is read.
  * @param length the number of bytes to read.
  *  
  * @param pNumBytes the total number of bytes read into the buffer, or -1 if
  * there is no more data because the end of the stream has been reached.
  *
  * If length is negative, or length is greater than the length of the array 
  * value, then an IndexOutOfBoundsException error is returned. No bytes will be read
  * from the stream for this error case.
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Likely causes for errors:
  *           IndexOutOfBoundsException if length is out of bound.
  *           MessageNotReadableException if message in write-only mode.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  */ 
int SMQ_API BytesMessage_readBytes2(HOBJ bytesMsg, HOBJ value, jint length, jint* pNumBytes); 

/** Write a <code>jboolean</code> to the stream message as a 1-byte value.
  * The value <code>true</code> is written out as the value 
  * <code>(byte)1</code> the value <code>false</code> is written out as 
  * the value <code>(byte)0</code>.
  *
  * @param bytesMsg the BytesMessage to operate against
  * @param value the <code>jboolean</code> value to be written.
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Likely causes for errors:
  *           MessageNotWriteableException if message in read-only mode.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  */
int SMQ_API BytesMessage_writeBoolean(HOBJ bytesMsg, jboolean value);

/** Write out a <code>byte</code> to the stream message as a 1-byte value.
  *
  * @param bytesMsg the BytesMessage to operate against
  * @param value the <code>byte</code> to be written.
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Likely causes for errors:
  *           MessageNotWriteableException if message in read-only mode.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  */ 
int SMQ_API BytesMessage_writeByte(HOBJ bytesMsg, jbyte value);

/** Write a <code>jshort</code> to the stream message as two bytes, high 
  * byte first.
  *
  * @param bytesMsg the BytesMessage to operate against
  * @param value the <code>short</code> to be written.
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Likely causes for errors:
  *           MessageNotWriteableException if message in read-only mode.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  */ 
int SMQ_API BytesMessage_writeShort(HOBJ bytesMsg, jshort value);

/** Write a <code>jchar</code> to the stream message as a 2-byte value, 
  * high byte first.
  *
  * @param bytesMsg the BytesMessage to operate against
  * @param value the <code>jchar</code> value to be written.
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Likely causes for errors:
  *           MessageNotWriteableException if message in read-only mode.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  */ 
int SMQ_API BytesMessage_writeChar(HOBJ bytesMsg, jchar value);

/** Write an <code>jint</code> to the stream message as four bytes, 
  * high byte first.
  *
  * @param bytesMsg the BytesMessage to operate against
  * @param value the <code>jint</code> to be written.
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Likely causes for errors:
  *           MessageNotWriteableException if message in read-only mode.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  */ 
int SMQ_API BytesMessage_writeInt(HOBJ bytesMsg, jint value);

/** Write a <code>jlong</code> to the stream message as eight bytes, 
  * high byte first.
  *
  * @param bytesMsg the BytesMessage to operate against
  * @param value the <code>jlong</code> to be written.
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Likely causes for errors:
  *           MessageNotWriteableException if message in read-only mode.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  */ 
int SMQ_API BytesMessage_writeLong(HOBJ bytesMsg, jlong value);

/** Convert the float argument to an <code>int</code> using the
  * <code>floatToIntBits</code> method in java class <code>Float</code>,
  * and then writes that <code>int</code> value to the stream
  * message as a 4-byte quantity, high byte first.
  *
  * @param bytesMsg the BytesMessage to operate against
  * @param value the <code>float</code> value to be written.
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Likely causes for errors:
  *           MessageNotWriteableException if message in read-only mode.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  */ 
int SMQ_API BytesMessage_writeFloat(HOBJ bytesMsg, jfloat value);

/** Convert the double argument to a <code>long</code> using the
  * <code>doubleToLongBits</code> method in Java class <code>Double</code>,
  * and then writes that <code>long</code> value to the stream
  * message as an 8-byte quantity, high byte first.
  *
  * @param bytesMsg the BytesMessage to operate against
  * @param value the <code>double</code> value to be written.
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Likely causes for errors:
  *           MessageNotWriteableException if message in read-only mode.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  */ 
int SMQ_API BytesMessage_writeDouble(HOBJ bytesMsg, jdouble value);

/** Write a string to the stream message using UTF-8 encoding in a 
  * machine-independent manner.
  *
  * <P>For more information on the UTF-8 format, see "File System Safe 
  * UCS Transformation Format (FSS_UFT)", X/Open Preliminary Specification,       * X/Open Company Ltd., Document Number: P316. This information also 
  * appears in ISO/IEC 10646, Annex P. 
  *
  * @param bytesMsg the BytesMessage to operate against
  * @param value the <code>String</code> value to be written.
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Likely causes for errors:
  *           MessageNotWriteableException if message in read-only mode.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  */ 
int SMQ_API BytesMessage_writeUTF(HOBJ bytesMsg, HOBJ value);

/** Write a byte array to the stream message.
  *
  * @param bytesMsg the BytesMessage to operate against
  * @param value the byte array to be written.
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Likely causes for errors:
  *           MessageNotWriteableException if message in read-only mode.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  */ 
int SMQ_API BytesMessage_writeBytes(HOBJ bytesMsg, HOBJ value);

/** Write a portion of a byte array to the stream message.
  *  
  * @param bytesMsg the BytesMessage to operate against
  * @param value the byte array value to be written.
  * @param offset the initial offset within the byte array.
  * @param length the number of bytes to use.
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Likely causes for errors:
  *           MessageNotWriteableException if message in read-only mode.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  */ 
int SMQ_API BytesMessage_writeBytes2(HOBJ bytesMsg,
									 HOBJ value,
									 jint offset,
									 jint length);

/** Put the message in read-only mode, and reposition the stream of 
  * bytes to the beginning.
  *  
  * @param bytesMsg the BytesMessage to operate against
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  */ 
int SMQ_API BytesMessage_reset(HOBJ bytesMsg);

/** Get the size, in bytes, of the message body.
  *
  * @param thisObj the BytesMessage to operate against
  * @param pret the number of bytes in the message body 
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  *
  */
#define BytesMessage_getBodySize(thisObj, pret)    Message_getBodySize(thisObj, pret)

/*
//---------------------------------
// Connection
//---------------------------------
*/
/**
 * Decrements the reference count of the Connection object.
 *
 * @param thisObj the Connection object to operate against.
 * @param pret will contain the reference count after decrementing.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Connection_release(connObj, pret)                Object_release(connObj, pret)

/**
 * Get the int corresponding to the type of a given object.
 *
 * @param thisObj the object to operate against.
 * @param pret will contain the int corresponding to the type.
 */
#define Connection_getType(thisObj, pret)                Object_getType(thisObj, pret)

/**
 * Determines whether the given object is an instance of the given type.
 *
 * @param thisObj the object to check.
 * @param classtype the type to check against.
 * @param pret non-zero if the object is an instance of the type.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Connection_instanceof(connObj, classtype, pret)  Object_instanceof(connObj, classtype, pret)

/**
 * Determines whether the two Connection objects are the same.
 *
 * @param thisObj the first Connection to compare.
 * @param hobj2 the second Connection to compare.
 * @param pret true if the Connection objects have identical content, false otherwise.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Connection_equals(connObj, hobj2, pret)          Object_equals(connObj, hobj2, pret)

/**
 * Get the String representation for the given Connection.
 *
 * @param thisObj the Connection object to operate against.
 * @param pret will contain the String that represents the Connection.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Connection_toString(connObj, pret)               Object_toString(connObj, pret)

/**
 * Returns the int corresponding to the Connection type.
 *
 * @return the int corresponding to the Connection type
 */
int SMQ_API Connection_type();

/**
 * Closes the connection. There is no need to close the sessions, producers, and consumers 
 * of a closed connection.
 *
 * @param obj the Connection object to close.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API Connection_close(HOBJ obj);

/**
 * Start (or restart) a Connection's delivery of incoming messages. Starting a started 
 * session is ignored.
 *
 * @param obj the Connection object to start.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API Connection_start(HOBJ obj);

/**
 * Used to temporarily stop a Connection's delivery of incoming messages. It can be 
 * restarted using its start method. When stopped, delivery to all the Connection's 
 * message consumers is inhibited: synchronous receive's block and messages are not 
 * delivered to message listeners. 
 *
 * <p>This call blocks until receives and/or message listeners in progress have completed. 
 *
 * <p>Stopping a Session has no affect on its ability to send messages. Stopping a stopped 
 * session is ignored. 
 *
 * <p>A stop method call must not return until delivery of messages has paused. This means 
 * a client can rely on the fact that none of its message listeners will be called and 
 * all threads of control waiting for receive to return will not return with a message 
 * until the connection is restarted. The receive timers for a stopped connection continue 
 * to advance so receives may time out while the connection is stopped. 
 *
 * <p>If MessageListeners are running when stop is invoked, stop must wait until all of them 
 * have returned before it may return. While these MessageListeners are completing, they 
 * must have full services of the connection available to them.
 *
 * @param obj the Connection object to stop.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API Connection_stop(HOBJ obj);

/**
 * Retrieve the URL of the broker for this Connection.
 *
 * @param connObj the Connection object to operate against.
 * @param pBrokerURL will contain the broker URL.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API Connection_getBrokerURL(HOBJ connObj, HOBJ* pBrokerURL);

/**
 * Returns URLs available on a standby broker, if any, which is paired for
 * fault-tolerance with the currently connected broker, and that may be used
 * for the purpose re-connection to a standby broker in the event of currently
 * connected broker failure.
 * <P>
 * Re-connection is automatically performed by the client runtime. Standby
 * broker reconnect URLs are provided for informational use.
 * <P>
 * The standby broker reconnect URLs are derived from the broker configuration.
 * If a default routing URL is configured on the standby, this will be used
 * for standby re-connection. Otherwise, standby broker acceptors URLs, configured
 * with the same acceptor name as the currently connected broker URL are
 * used for re-connection.
 * <P>
 * If called after the connection is closed, the last known standby reconnect URLs
 * are returned.
 * <P>
 * If the currently connected broker is standalone, or, if from the configuration,
 * no redundant URLs are present on the standby, or, if the connection is non
 * fault-tolerant, null is returned.
 * <P>
 * @param connObj the Connection object to operate against
 * @param pret an enumeration of strings of URLs to the standby broker.
 * 
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @see getBrokerURL()
 * @see getBrokerReconnectURLs()
 * @see isFaultTolerant()
 */
int SMQ_API Connection_getStandbyBrokerReconnectURLs(HOBJ connObj, HOBJ *pret);

/**
 * Returns URLs available on the currently connected broker, that may be used for
 * the purpose re-connection of fault-tolerant connections when temporary
 * or permanent network failure is encountered.
 * <P>
 * Re-connection is automatically performed by the client runtime. Broker reconnect URLs
 * are provided for informational use.
 * <P>
 * The broker reconnect URLs are derived from the broker configuration. If a default routing
 * URL is configured on the broker, the currently connected URL is used for reconnection.
 * Otherwise, acceptors URLs, configured with the same acceptor name as the currently
 * connectedURL are used for reconnection.
 * <P>
 * If called after the connection is closed, the last known reconnect URLs are
 * returned.
 *<P>
 * If the connection is non fault-tolerant, returns null.
 *
 * @param connObj the Connection object to operate against
 * @param pret an enumeration of strings of reconnect URLs
 * 
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 * @see getBrokerURL()
 * @see getStandbyBrokerReconnectURLs()
 * @see isFaultTolerant()
 */
int SMQ_API Connection_getBrokerReconnectURLs(HOBJ connObj, HOBJ *pret);


/** Get the client identifier for this connection.
  *
  * @param connObj the Connection object to operate against
  * @param pClientID the unique client identifier.
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  */
int SMQ_API Connection_getClientID(HOBJ connObj, HOBJ* pClientID);

/** Get the connection identifier for this connection.
  *
  * @param connObj the Connection object to operate against
  * @param connectID the unique connection ID identifier.
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  */
int SMQ_API Connection_getConnectID(HOBJ connObj, HOBJ* connectID);

/**
 * Retrieve the connection ID for this Connection.
 *
 * @param connObj the Connection object to operate against.
 * @param pConnID will contain the connection ID.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API Connection_getConnectionID(HOBJ connObj, jlong* pConnID);

/**
 * Get the ExceptionListener for this Connection.
 *
 * @param connObj the Connection object to operate against
 * @param p_pfnListener The exception listener (i.e. callback function) for this 
 *        Connection. The signature is given by 
 *        <code>typedef void (*pfnCExceptionListener)(HOBJ hobjException);</code>
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API Connection_getExceptionListener(HOBJ connObj,
											pfnCExceptionListener* p_pfnListener);

/**
 * Get the ExceptionListener2 for this Connection.
 *
 * @param connObj the Connection object to operate against
 * @param p_pfnListener The exception listener (i.e. callback function) for this 
 *        Connection. The signature is given by 
 *        <code>typedef void (*pfnCExceptionListener2)(HOBJ hobjException, HOBJ hobjUser);</code>
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API Connection_getExceptionListener2(HOBJ connObj,
											 pfnCExceptionListener2* p_pfnListener);

/**
 * Get the ExceptionListener2 for this Connection.
 *
 * @param connObj the Connection object to operate against
 * @param hobjUser The user information suppiled to the excpetion listener.
 * 
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API Connection_getExceptionListener2hobjUser(HOBJ connObj,
										             HOBJ* hobjUser);

/**
 * Get the Connection State Change Listener for this Connection.
 *
 * @param connObj the Connection object to operate against
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API Connection_getConnectionStateChangeListener(HOBJ connObj,
                                                        pfnCConnectionStateChangeListener* p_pfnListener);

/**
 * Get the Connection State Change Listener User Information for this Connection.
 *
 * @param connObj the Connection object to operate against
 * @param hobjUser The user information suppiled to the connection state change listener.
 * 
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API Connection_getConnectionStateChangeListenerHobjUser(HOBJ connObj,
                                                                HOBJ* hobjUser);


/** Get the meta data for this connection.
  *
  * @param connObj the Connection object to operate against
  * @param pMetaData the ConnectionMetadata object returned
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  *
  * @see ConnectionMetaData_* functions
  */
int SMQ_API Connection_getMetaData(HOBJ connObj, HOBJ* pMetaData);

/** Get the username for this connection.
  *
  * @param connObj the Connection object to operate against
  * @param pUserName returns a String object containing the username.
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  *
  * @see ConnectionMetaData_* functions
  */
int SMQ_API Connection_getUsername(HOBJ connObj, HOBJ* pUserName);


/** Get the routing node name for this connection.
  *
  * @param connObj the Connection object to operate against
  * @param pRoutingNodeName returns a String object containing the routing node name.
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  *
  * @see ConnectionMetaData_* functions
  */
int SMQ_API Connection_getRoutingNodeName(HOBJ connObj, HOBJ* pRoutingNodeName);

/** Set the client identifier for this connection.
  *
  * <P>The preferred way to assign a Client's client identifier is for
  * it to be configured in a client-specific ConnectionFactory and
  * transparently assigned to the Connection it creates. Alternatively,
  * a client can set a Connections's client identifier using a
  * specific value.
  *
  * <P>The purpose of client identifier is to associate a session and
  * its objects with a state maintained on behalf of the client by a
  * SonicMQ broker. The only such state identified by SonicMQ is that required
  * to support durable subscriptions
  *
  * @param connObj the Connection object to operate against
  * @param clientID the unique client identifier
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Likely causes of errors:
  *           InvalidClientIDException if SonicMQ client specifies an invalid or duplicate client id.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  */
int SMQ_API Connection_setClientID(HOBJ connObj, HOBJ clientID);

/** Set up an exception listener (i.e. a callback function) for this connection.
  *
  * <P>If SonicMQ detects a serious problem with a connection it
  * will inform the connection's "exception listener" if one has been
  * registered. It does this by invoking the callback function set via 
  * <code>Connection_setExceptionListener()</code and passing it a JMSException 
  * object describing the problem.
  *
  * <P>This allows a client to be asynchronously notified of a problem.
  * Some connections only consume messages so they would have no other
  * way to learn their connection has failed.
  *
  * <P>A Connection serializes execution of its exception listener.
  *
  * <P>A SonicMQ broker attempts to resolve connection problems
  * itself prior to notifying the client of them.
  *
  * @param connObj the Connection object to operate against
  * @param listener the callback function to set as the exception listener--
  *        the signature is <code>typedef void (*pfnCExceptionListener)(HOBJ hobjException);</code>
  *        where hobjException is a JMSException object handle
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Likely causes of errors:
  *           InvalidClientIDException if SonicMQ client specifies an invalid or duplicate client id.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  */
int SMQ_API Connection_setExceptionListener(HOBJ connObj, pfnCExceptionListener listener);


/** 
 * Set a C-signature exception listener for this connection.
 *
 * <P>If a JMS provider detects a serious problem with a connection it
 * will inform the connection's ExceptionListener if one has been
 * registered. It does this by calling the listener's onException()
 * method passing it a JMSException describing the problem.
 *
 * <P>This allows a client to be asynchronously notified of a problem.
 * Some connections only consume messages so they would have no other
 * way to learn their connection has failed.
 *
 * <P>A Connection serializes execution of its ExceptionListener.
 *
 * <P>A JMS provider should attempt to resolve connection problems
 * itself prior to notifying the client of them.
 *
 * @param listener the exception listener.
 * @param hobjUser user supplied opaque pointer passed to the listener
 *                 function when invoked.
 * @exception JMSException general exception if JMS implementation fails to
 *                         set the Exception listener for this Connection.
 */
int SMQ_API Connection_setExceptionListener2(HOBJ connObj, pfnCExceptionListener2 listener, HOBJ hobjUser);


/**
 * Set a C-signature Connection State Change listener for this connection.
 *
 * @param connObj the Connection object to operate against
 * @param listener the connection state change listener.
 * @param hobjUser user supplied opaque pointer passed to the listener
 *                 function when invoked.
 * 
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API Connection_setConnectionStateChangeListener(HOBJ connObj, pfnCConnectionStateChangeListener listener, HOBJ hobjUser);

/** 
 * Enable/disable sending active pings on this connection. This is a non-JMS public method.
 *
 * @param obj the Connection object to operate against
 * @param interval indicates the interval in seconds for sending a ping. Setting 
 *        interval to 0 or any negative value effectively disables the ping. 
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Likely causes of errors:
 *           InvalidClientIDException if SonicMQ client specifies an invalid or duplicate client id.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API Connection_setPingInterval(HOBJ obj, jlong interval);

/**
 * Returns true if the connection is fault tolerant
 * <P>For a connection to be fault-tolerant, fault-tolerant must be set in the
 * ConnectionFactory, and the broker must support(be licensed for) fault-tolerance.
 *
 * @param connObj the Connection object to operate against
 * @param pret Returns the true if the connectgion is fault tolerant.
 * 
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @see progress.message.jclient.ConnectionFactory#setFaultTolerant
 */
int SMQ_API Connection_isFaultTolerant(HOBJ connObj, jboolean *pret);

/**
 * Get connection state, one of ACTIVE, RECONNECTING, FAILED or CLOSED.
 * <P>
 * A non fault-tolerant connection will never see a RECONNECTING value.
 * This method may be called after the connection is closed.
 * 
 * @param connObj the Connection object to operate against
 * @param pret Returns the connection state.
 * 
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @see progress.message.jclient.Constants#ACTIVE
 * @see progress.message.jclient.Constants#RECONNECTING
 * @see progress.message.jclient.Constants#FAILED
 * @see progress.message.jclient.Constants#CLOSED
 * @see progress.message.jclient.ConnectionStateChangeListener
 * @see setConnectionStateChangeListener(ConnectionStateChangeListener)
 * @see getConnectionStateChangeListener()
 */
int SMQ_API Connection_getConnectionState(HOBJ connObj, jint *pret);

/*
//---------------------------------
// ConnectionMetaData
//---------------------------------
*/
/**
 * Decrements the reference count of the ConnectionMetaData object.
 *
 * @param thisObj the ConnectionMetaData object to operate against.
 * @param pret will contain the reference count after decrementing.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define ConnectionMetaData_release(connMeta, pret)                Object_release(connMeta, pret)

/**
 * Get the int corresponding to the type of a given object.
 *
 * @param thisObj the object to operate against.
 * @param pret will contain the int corresponding to the type.
 */
#define ConnectionMetaData_getType(thisObj, pret)                 Object_getType(thisObj, pret)

/**
 * Determines whether the given object is an instance of the given type.
 *
 * @param thisObj the object to check.
 * @param classtype the type to check against.
 * @param pret non-zero if the object is an instance of the type.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define ConnectionMetaData_instanceof(thisObj, classtype, pret)  Object_instanceof(thisObj, classtype, pret)

/**
 * Determines whether the two ConnectionMetaData objects are the same.
 *
 * @param thisObj the first ConnectionMetaData to compare.
 * @param hobj2 the second ConnectionMetaData to compare.
 * @param pret true if the ConnectionMetaData objects have identical content, false otherwise.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define ConnectionMetaData_equals(connMeta, hobj2, pret)          Object_equals(connMeta, hobj2, pret)

/**
 * Get the String representation for the given ConnectionMetaData.
 *
 * @param thisObj the ConnectionMetaData object to operate against.
 * @param pret will contain the String that represents the ConnectionMetaData.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define ConnectionMetaData_toString(connMeta, pret)               Object_toString(connMeta, pret)

/**
 * Returns the int corresponding to the ConnectionMetaData type.
 *
 * @return the int corresponding to the ConnectionMetaData type
 */
int SMQ_API ConnectionMetaData_type();

/**
 * Get the JMS major version number.
 *
 * @param connMeta the ConnectionMetaData object to operate against.
 * @param pMajorVersion the JMS major version number.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error (e.g., invalid protocol).
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API ConnectionMetaData_getJMSMajorVersion(HOBJ connMeta, jint* pMajorVersion);

/**
 * Get the JMS minor version number.
 *
 * @param connMeta the ConnectionMetaData object to operate against.
 * @param pMinorVersion the JMS minor version number.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error (e.g., invalid protocol).
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API ConnectionMetaData_getJMSMinorVersion(HOBJ connMeta, jint* pMinorVersion);

/**
 * Get the JMS provider name.
 *
 * @param connMeta the ConnectionMetaData object to operate against.
 * @param pProvider a String containing the JMS provider name.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error (e.g., invalid protocol).
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API ConnectionMetaData_getJMSProviderName(HOBJ connMeta, HOBJ* pProvider);

/** Get an enumeration of JMSX Property Names.
  *  
  * @param connMeta the ConnectionMetaData object to operate against
  * @param pXPropNames an Enumeration of the JMSX PropertyNames.
  *  
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  */
int SMQ_API ConnectionMetaData_getJMSXPropertyNames(HOBJ connMeta, HOBJ* pXPropNames);

/**
 * Get the JMS provider major version number.
 *
 * @param connMeta the ConnectionMetaData object to operate against.
 * @param pMajorVersion the JMS provider major version number.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error (e.g., invalid protocol).
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API ConnectionMetaData_getProviderMajorVersion(HOBJ connMeta, jint* pMajorVersion);

/**
 * Get the JMS provider minor version number.
 *
 * @param connMeta the ConnectionMetaData object to operate against.
 * @param pMinorVersion the JMS provider minor version number.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error (e.g., invalid protocol).
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API ConnectionMetaData_getProviderMinorVersion(HOBJ connMeta, jint* pMinorVersion);

/**
 * Get the JMS provider version.
 *
 * @param connMeta the ConnectionMetaData object to operate against.
 * @param pVersion a String containing the JMS provider version.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error (e.g., invalid protocol).
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API ConnectionMetaData_getProviderVersion(HOBJ connMeta, HOBJ* pVersion);

/**
 * Get the JMS version.
 *
 * @param connMeta the ConnectionMetaData object to operate against.
 * @param pret a String containing the JMS version.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error (e.g., invalid protocol).
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API ConnectionMetaData_getJMSVersion(HOBJ connMeta, HOBJ *pret);

/*
//---------------------------------
// Constants
//---------------------------------
*/
/**  Returns a string representation of the NOTIFY_UNDELIVERED reserved property name used in
 *   dynamic routing.
 *
 * @param pString the NOTIFY_UNDELIVERED string
 */
int SMQ_API Constants_getNOTIFY_UNDELIVERED(HOBJ *pString);

/**  Returns a string representation of the PRESERVE_UNDELIVERED reserved property name used in
 *   dynamic routing.
 *
 * @param pString the PRESERVE_UNDELIVERED string
 */
int SMQ_API Constants_getPRESERVE_UNDELIVERED(HOBJ *pString);

/**  Returns a string representation of the UNDELIVERED_REASON_CODE reserved property name used in
 *   dynamic routing.
 *
 * @param pString the UNDELIVERED_REASON_CODE string
 */
int SMQ_API Constants_getUNDELIVERED_REASON_CODE(HOBJ *pString);

/**  Returns a string representation of the UNDELIVERED_TIMESTAMP reserved property name used in
 *   dynamic routing.
 *
 * @param pString the UNDELIVERED_TIMESTAMP string
 */
int SMQ_API Constants_getUNDELIVERED_TIMESTAMP(HOBJ *pString);

/*
//---------------------------------
// Destination
//---------------------------------
*/
/**
 * Decrements the reference count of the Destination object.
 *
 * @param thisObj the Destination object to operate against.
 * @param pret will contain the reference count after decrementing.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Destination_release(destObj, pret)                Object_release(destObj, pret)

/**
 * Get the int corresponding to the type of a given object.
 *
 * @param thisObj the object to operate against.
 * @param pret will contain the int corresponding to the type.
 */
#define Destination_getType(thisObj, pret)                Object_getType(thisObj, pret)

/**
 * Determines whether the given object is an instance of the given type.
 *
 * @param thisObj the object to check.
 * @param classtype the type to check against.
 * @param pret non-zero if the object is an instance of the type.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Destination_instanceof(thisObj, classtype, pret)  Object_instanceof(thisObj, classtype, pret)

/**
 * Determines whether the two Destination objects are the same.
 *
 * @param thisObj the first Destination to compare.
 * @param hobj2 the second Destination to compare.
 * @param pret true if the Destination objects have identical content, false otherwise.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Destination_equals(destObj, hobj2, pret)          Object_equals(destObj, hobj2, pret)

/**
 * Get the String representation for the given Destination.
 *
 * @param thisObj the Destination object to operate against.
 * @param pret will contain the String that represents the Destination.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Destination_toString(destObj, pret)               Object_toString(destObj, pret)

/**
 * Returns the int corresponding to the Destination type.
 *
 * @return the int corresponding to the Destination type
 */
int SMQ_API Destination_type();

/*
//---------------------------------
// ErrorCodes
//---------------------------------
*/

/**
 * Tests the error code associated with a JMSException against
 * an integer value associated with a Progress SonicMQ ErrorCode.
 *
 * @param anException A JMSException that needs to be tested.
 * @param aCode An error code specified as an integer.
 * @param pret True, if the error code of the exception is the one specified.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API ErrorCodes_testException(HOBJ anException, jint aCode, jboolean *pret);

/*
//---------------------------------
// JMSException
//---------------------------------
*/
/**
 * Decrements the reference count of the JMSException object.
 *
 * @param thisObj the JMSException object to operate against.
 * @param pret will contain the reference count after decrementing.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define JMSException_release(thisObj, pret)                   Object_release(thisObj, pret)

/**
 * Get the int corresponding to the type of a given object.
 *
 * @param thisObj the object to operate against.
 * @param pret will contain the int corresponding to the type.
 */
#define JMSException_getType(thisObj, pret)                   Object_getType(thisObj, pret)

/**
 * Determines whether the given object is an instance of the given type.
 *
 * @param thisObj the object to check.
 * @param classtype the type to check against.
 * @param pret non-zero if the object is an instance of the type.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define JMSException_instanceof(thisObj, classtype, pret)     Object_instanceof(thisObj, classtype, pret)

/**
 * Determines whether the two JMSException objects are the same.
 *
 * @param thisObj the first JMSException to compare.
 * @param hobj2 the second JMSException to compare.
 * @param pret true if the JMSException objects have identical content, false otherwise.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define JMSException_equals(thisObj, hobj2, pret)             Object_equals(thisObj, hobj2, pret)

/**
 * Get the String representation for the given JMSException.
 *
 * @param thisObj the JMSException object to operate against.
 * @param pret will contain the String that represents the JMSException.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define JMSException_toString(thisObj, pret)                  Object_toString(thisObj, pret)

/**
 * Retrieve the String containing the localized message associated w/ this JMSException.
 *
 * @param thisObj the JMSException object to operate against.
 * @param pretString will contain the String containing the localized message.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define JMSException_getLocalizedMessage(thisObj, pretString) Throwable_getLocalizedMessage(thisObj, pretString)

/**
 * Retrieve the String containing the message associated w/ this JMSException.
 *
 * @param thisObj the JMSException object to operate against.
 * @param pretString will contain the String containing the message.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define JMSException_getMessage(thisObj, pretString)          Throwable_getMessage(thisObj, pretString)

/**
 * Prints a stack trace w/ to the error location corresponding to the JMSException object.
 *
 * @param thisObj the JMSException object to operate against.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define JMSException_printStackTrace(thisObj)                 Throwable_printStackTrace(thisObj)

/**
 * Returns the int corresponding to the JMSException type.
 *
 * @return the int corresponding to the JMSException type
 */
int SMQ_API JMSException_type();

/**
 * Get the vendor-specific error code.
 *
 * @param thisObj the JMSException to operate against.
 * @param pretString will contain a String object specifying the vendor-specific error code.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API JMSException_getErrorCode(HOBJ thisObj, HOBJ *pretString);

/**
 * Get the exception linked to this one.
 *
 * @param thisObj the JMSException to operate against.
 * @param pretException will contain the linked Exception object or null if none.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API JMSException_getLinkedException(HOBJ thisObj, HOBJ *pretException);

/**
 * Add a linked exception.
 *
 * @param thisObj the JMSException to operate against.
 * @param hobjLinked the linked Exception object.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API JMSException_setLinkedException(HOBJ thisObj, HOBJ hobjLinked);

/*
//---------------------------------
// JMSSecurityException
//---------------------------------
*/
/**
 * Decrements the reference count of the JMSSecurityException object.
 *
 * @param thisObj the JMSSecurityException object to operate against.
 * @param pret will contain the reference count after decrementing.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define JMSSecurityException_release(thisObj, pret)                     Object_release(thisObj, pret)

/**
 * Get the int corresponding to the type of a given object.
 *
 * @param thisObj the object to operate against.
 * @param pret will contain the int corresponding to the type.
 */
#define JMSSecurityException_getType(thisObj, pret)                     Object_getType(thisObj, pret)

/**
 * Determines whether the given object is an instance of the given type.
 *
 * @param thisObj the object to check.
 * @param classtype the type to check against.
 * @param pret non-zero if the object is an instance of the type.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define JMSSecurityException_instanceof(thisObj, classtype, pret)       Object_instanceof(thisObj, classtype, pret)

/**
 * Determines whether the two JMSSecurityException objects are the same.
 *
 * @param thisObj the first JMSSecurityException to compare.
 * @param hobj2 the second JMSSecurityException to compare.
 * @param pret true if the JMSSecurityException objects have identical content, false otherwise.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define JMSSecurityException_equals(thisObj, hobj2, pret)               Object_equals(thisObj, hobj2, pret)

/**
 * Get the String representation for the given JMSSecurityException.
 *
 * @param thisObj the JMSSecurityException object to operate against.
 * @param pret will contain the String that represents the JMSSecurityException.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define JMSSecurityException_toString(thisObj, pret)                    Object_toString(thisObj, pret)

/**
 * Retrieve the String containing the localized message associated w/ this JMSSecurityException.
 *
 * @param thisObj the JMSSecurityException object to operate against.
 * @param pretString will contain the String containing the localized message.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define JMSSecurityException_getLocalizedMessage(thisObj, pretString)   Throwable_getLocalizedMessage(thisObj, pretString)

/**
 * Retrieve the String containing the message associated w/ this JMSSecurityException.
 *
 * @param thisObj the JMSSecurityException object to operate against.
 * @param pretString will contain the String containing the message.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define JMSSecurityException_getMessage(thisObj, pretString)            Throwable_getMessage(thisObj, pretString)

/**
 * Prints a stack trace w/ to the error location corresponding to the JMSSecurityException object.
 *
 * @param thisObj the JMSSecurityException object to operate against.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define JMSSecurityException_printStackTrace(thisObj)                   Throwable_printStackTrace(thisObj)

/**
 * Get the vendor-specific error code.
 *
 * @param thisObj the JMSSecurityException to operate against.
 * @param pretString will contain a String object specifying the vendor-specific error code.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define JMSSecurityException_getErrorCode(thisObj, pretString)          JMSException_getErrorCode(thisObj, pretString)

/**
 * Get the exception linked to this one.
 *
 * @param thisObj the JMSSecurityException to operate against.
 * @param pretException will contain the linked Exception object or null if none.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define JMSSecurityException_getLinkedException(thisObj, pretException) JMSException_getLinkedException(thisObj, pretException)

/**
 * Add a linked exception.
 *
 * @param thisObj the JMSSecurityException to operate against.
 * @param hobjLinked the linked Exception object.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define JMSSecurityException_setLinkedException(thisObj, hobjLinked)    JMSException_setLinkedException(thisObj, hobjLinked)

/**
 * Returns the int corresponding to the JMSSecurityException type.
 *
 * @return the int corresponding to the JMSSecurityException type
 */
int SMQ_API JMSSecurityException_type();

/*
//---------------------------------
// IllegalStateException
//---------------------------------
*/
/**
 * Decrements the reference count of the IllegalStateException object.
 *
 * @param thisObj the IllegalStateException object to operate against.
 * @param pret will contain the reference count after decrementing.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define IllegalStateException_release(thisObj, pret)                     Object_release(thisObj, pret)

/**
 * Get the int corresponding to the type of a given object.
 *
 * @param thisObj the object to operate against.
 * @param pret will contain the int corresponding to the type.
 */
#define IllegalStateException_getType(thisObj, pret)                     Object_getType(thisObj, pret)

/**
 * Determines whether the given object is an instance of the given type.
 *
 * @param thisObj the object to check.
 * @param classtype the type to check against.
 * @param pret non-zero if the object is an instance of the type.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define IllegalStateException_instanceof(thisObj, classtype, pret)       Object_instanceof(thisObj, classtype, pret)

/**
 * Determines whether the two IllegalStateException objects are the same.
 *
 * @param thisObj the first IllegalStateException to compare.
 * @param hobj2 the second IllegalStateException to compare.
 * @param pret true if the IllegalStateException objects have identical content, false otherwise.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define IllegalStateException_equals(thisObj, hobj2, pret)               Object_equals(thisObj, hobj2, pret)

/**
 * Get the String representation for the given IllegalStateException.
 *
 * @param thisObj the IllegalStateException object to operate against.
 * @param pret will contain the String that represents the IllegalStateException.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define IllegalStateException_toString(thisObj, pret)                    Object_toString(thisObj, pret)

/**
 * Retrieve the String containing the localized message associated w/ this IllegalStateException.
 *
 * @param thisObj the IllegalStateException object to operate against.
 * @param pretString will contain the String containing the localized message.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define IllegalStateException_getLocalizedMessage(thisObj, pretString)   Throwable_getLocalizedMessage(thisObj, pretString)

/**
 * Retrieve the String containing the message associated w/ this IllegalStateException.
 *
 * @param thisObj the IllegalStateException object to operate against.
 * @param pretString will contain the String containing the message.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define IllegalStateException_getMessage(thisObj, pretString)            Throwable_getMessage(thisObj, pretString)

/**
 * Prints a stack trace w/ to the error location corresponding to the IllegalStateException object.
 *
 * @param thisObj the IllegalStateException object to operate against.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define IllegalStateException_printStackTrace(thisObj)                   Throwable_printStackTrace(thisObj)

/**
 * Get the vendor-specific error code.
 *
 * @param thisObj the IllegalStateException to operate against.
 * @param pretString will contain a String object specifying the vendor-specific error code.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define IllegalStateException_getErrorCode(thisObj, pretString)          JMSException_getErrorCode(thisObj, pretString)

/**
 * Get the exception linked to this one.
 *
 * @param thisObj the IllegalStateException to operate against.
 * @param pretException will contain the linked Exception object or null if none.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define IllegalStateException_getLinkedException(thisObj, pretException) JMSException_getLinkedException(thisObj, pretException)

/**
 * Add a linked exception.
 *
 * @param thisObj the IllegalStateException to operate against.
 * @param hobjLinked the linked Exception object.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define IllegalStateException_setLinkedException(thisObj, hobjLinked)    JMSException_setLinkedException(thisObj, hobjLinked)

/**
 * Returns the int corresponding to the IllegalStateException type.
 *
 * @return the int corresponding to the IllegalStateException type
 */
int SMQ_API IllegalStateException_type();

/*
//---------------------------------
// InvalidClientIDException
//---------------------------------
*/
/**
 * Decrements the reference count of the InvalidClientIDException object.
 *
 * @param thisObj the InvalidClientIDException object to operate against.
 * @param pret will contain the reference count after decrementing.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define InvalidClientIDException_release(thisObj, pret)                     Object_release(thisObj, pret)

/**
 * Get the int corresponding to the type of a given object.
 *
 * @param thisObj the object to operate against.
 * @param pret will contain the int corresponding to the type.
 */
#define InvalidClientIDException_getType(thisObj, pret)                     Object_getType(thisObj, pret)

/**
 * Determines whether the given object is an instance of the given type.
 *
 * @param thisObj the object to check.
 * @param classtype the type to check against.
 * @param pret non-zero if the object is an instance of the type.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define InvalidClientIDException_instanceof(thisObj, classtype, pret)       Object_instanceof(thisObj, classtype, pret)

/**
 * Determines whether the two InvalidClientIDException objects are the same.
 *
 * @param thisObj the first InvalidClientIDException to compare.
 * @param hobj2 the second InvalidClientIDException to compare.
 * @param pret true if the InvalidClientIDException objects have identical content, false otherwise.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define InvalidClientIDException_equals(thisObj, hobj2, pret)               Object_equals(thisObj, hobj2, pret)

/**
 * Get the String representation for the given InvalidClientIDException.
 *
 * @param thisObj the InvalidClientIDException object to operate against.
 * @param pret will contain the String that represents the InvalidClientIDException.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define InvalidClientIDException_toString(thisObj, pret)                    Object_toString(thisObj, pret)

/**
 * Retrieve the String containing the localized message associated w/ this InvalidClientIDException.
 *
 * @param thisObj the InvalidClientIDException object to operate against.
 * @param pretString will contain the String containing the localized message.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define InvalidClientIDException_getLocalizedMessage(thisObj, pretString)   Throwable_getLocalizedMessage(thisObj, pretString)

/**
 * Retrieve the String containing the message associated w/ this InvalidClientIDException.
 *
 * @param thisObj the InvalidClientIDException object to operate against.
 * @param pretString will contain the String containing the message.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define InvalidClientIDException_getMessage(thisObj, pretString)            Throwable_getMessage(thisObj, pretString)

/**
 * Prints a stack trace w/ to the error location corresponding to the InvalidClientIDException object.
 *
 * @param thisObj the InvalidClientIDException object to operate against.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define InvalidClientIDException_printStackTrace(thisObj)                   Throwable_printStackTrace(thisObj)

/**
 * Get the vendor-specific error code.
 *
 * @param thisObj the InvalidClientIDException to operate against.
 * @param pretString will contain a String object specifying the vendor-specific error code.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define InvalidClientIDException_getErrorCode(thisObj, pretString)          JMSException_getErrorCode(thisObj, pretString)

/**
 * Get the exception linked to this one.
 *
 * @param thisObj the InvalidClientIDException to operate against.
 * @param pretException will contain the linked Exception object or null if none.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define InvalidClientIDException_getLinkedException(thisObj, pretException) JMSException_getLinkedException(thisObj, pretException)

/**
 * Add a linked exception.
 *
 * @param thisObj the InvalidClientIDException to operate against.
 * @param hobjLinked the linked Exception object.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define InvalidClientIDException_setLinkedException(thisObj, hobjLinked)    JMSException_setLinkedException(thisObj, hobjLinked)

/**
 * Returns the int corresponding to the InvalidClientIDException type.
 *
 * @return the int corresponding to the InvalidClientIDException type
 */
int SMQ_API InvalidClientIDException_type();

/*
//---------------------------------
// InvalidDestinationException
//---------------------------------
*/
/**
 * Decrements the reference count of the InvalidDestinationException object.
 *
 * @param thisObj the InvalidDestinationException object to operate against.
 * @param pret will contain the reference count after decrementing.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define InvalidDestinationException_release(thisObj, pret)                     Object_release(thisObj, pret)

/**
 * Get the int corresponding to the type of a given object.
 *
 * @param thisObj the object to operate against.
 * @param pret will contain the int corresponding to the type.
 */
#define InvalidDestinationException_getType(thisObj, pret)                     Object_getType(thisObj, pret)

/**
 * Determines whether the given object is an instance of the given type.
 *
 * @param thisObj the object to check.
 * @param classtype the type to check against.
 * @param pret non-zero if the object is an instance of the type.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define InvalidDestinationException_instanceof(thisObj, classtype, pret)       Object_instanceof(thisObj, classtype, pret)

/**
 * Determines whether the two InvalidDestinationException objects are the same.
 *
 * @param thisObj the first InvalidDestinationException to compare.
 * @param hobj2 the second InvalidDestinationException to compare.
 * @param pret true if the InvalidDestinationException objects have identical content, false otherwise.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define InvalidDestinationException_equals(thisObj, hobj2, pret)               Object_equals(thisObj, hobj2, pret)

/**
 * Get the String representation for the given InvalidDestinationException.
 *
 * @param thisObj the InvalidDestinationException object to operate against.
 * @param pret will contain the String that represents the InvalidDestinationException.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define InvalidDestinationException_toString(thisObj, pret)                    Object_toString(thisObj, pret)

/**
 * Retrieve the String containing the localized message associated w/ this InvalidDestinationException.
 *
 * @param thisObj the InvalidDestinationException object to operate against.
 * @param pretString will contain the String containing the localized message.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define InvalidDestinationException_getLocalizedMessage(thisObj, pretString)   Throwable_getLocalizedMessage(thisObj, pretString)

/**
 * Retrieve the String containing the message associated w/ this InvalidDestinationException.
 *
 * @param thisObj the InvalidDestinationException object to operate against.
 * @param pretString will contain the String containing the message.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define InvalidDestinationException_getMessage(thisObj, pretString)            Throwable_getMessage(thisObj, pretString)

/**
 * Prints a stack trace w/ to the error location corresponding to the InvalidDestinationException object.
 *
 * @param thisObj the InvalidDestinationException object to operate against.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define InvalidDestinationException_printStackTrace(thisObj)                   Throwable_printStackTrace(thisObj)

/**
 * Get the vendor-specific error code.
 *
 * @param thisObj the InvalidDestinationException to operate against.
 * @param pretString will contain a String object specifying the vendor-specific error code.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define InvalidDestinationException_getErrorCode(thisObj, pretString)          JMSException_getErrorCode(thisObj, pretString)

/**
 * Get the exception linked to this one.
 *
 * @param thisObj the InvalidDestinationException to operate against.
 * @param pretException will contain the linked Exception object or null if none.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define InvalidDestinationException_getLinkedException(thisObj, pretException) JMSException_getLinkedException(thisObj, pretException)

/**
 * Add a linked exception.
 *
 * @param thisObj the InvalidDestinationException to operate against.
 * @param hobjLinked the linked Exception object.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define InvalidDestinationException_setLinkedException(thisObj, hobjLinked)    JMSException_setLinkedException(thisObj, hobjLinked)

/**
 * Returns the int corresponding to the InvalidDestinationException type.
 *
 * @return the int corresponding to the InvalidDestinationException type
 */
int SMQ_API InvalidDestinationException_type();

/*
//---------------------------------
// InvalidSelectorException
//---------------------------------
*/
/**
 * Decrements the reference count of the InvalidSelectorException object.
 *
 * @param thisObj the InvalidSelectorException object to operate against.
 * @param pret will contain the reference count after decrementing.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define InvalidSelectorException_release(thisObj, pret)                     Object_release(thisObj, pret)

/**
 * Get the int corresponding to the type of a given object.
 *
 * @param thisObj the object to operate against.
 * @param pret will contain the int corresponding to the type.
 */
#define InvalidSelectorException_getType(thisObj, pret)                     Object_getType(thisObj, pret)

/**
 * Determines whether the given object is an instance of the given type.
 *
 * @param thisObj the object to check.
 * @param classtype the type to check against.
 * @param pret non-zero if the object is an instance of the type.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define InvalidSelectorException_instanceof(thisObj, classtype, pret)       Object_instanceof(thisObj, classtype, pret)

/**
 * Determines whether the two InvalidSelectorException objects are the same.
 *
 * @param thisObj the first InvalidSelectorException to compare.
 * @param hobj2 the second InvalidSelectorException to compare.
 * @param pret true if the InvalidSelectorException objects have identical content, false otherwise.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define InvalidSelectorException_equals(thisObj, hobj2, pret)               Object_equals(thisObj, hobj2, pret)

/**
 * Get the String representation for the given InvalidSelectorException.
 *
 * @param thisObj the InvalidSelectorException object to operate against.
 * @param pret will contain the String that represents the InvalidSelectorException.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define InvalidSelectorException_toString(thisObj, pret)                    Object_toString(thisObj, pret)

/**
 * Retrieve the String containing the localized message associated w/ this InvalidSelectorException.
 *
 * @param thisObj the InvalidSelectorException object to operate against.
 * @param pretString will contain the String containing the localized message.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define InvalidSelectorException_getLocalizedMessage(thisObj, pretString)   Throwable_getLocalizedMessage(thisObj, pretString)

/**
 * Retrieve the String containing the message associated w/ this InvalidSelectorException.
 *
 * @param thisObj the InvalidSelectorException object to operate against.
 * @param pretString will contain the String containing the message.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define InvalidSelectorException_getMessage(thisObj, pretString)            Throwable_getMessage(thisObj, pretString)

/**
 * Prints a stack trace w/ to the error location corresponding to the InvalidSelectorException object.
 *
 * @param thisObj the InvalidSelectorException object to operate against.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define InvalidSelectorException_printStackTrace(thisObj)                   Throwable_printStackTrace(thisObj)

/**
 * Get the vendor-specific error code.
 *
 * @param thisObj the InvalidSelectorException to operate against.
 * @param pretString will contain a String object specifying the vendor-specific error code.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define InvalidSelectorException_getErrorCode(thisObj, pretString)          JMSException_getErrorCode(thisObj, pretString)

/**
 * Get the exception linked to this one.
 *
 * @param thisObj the InvalidSelectorException to operate against.
 * @param pretException will contain the linked Exception object or null if none.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define InvalidSelectorException_getLinkedException(thisObj, pretException) JMSException_getLinkedException(thisObj, pretException)

/**
 * Add a linked exception.
 *
 * @param thisObj the InvalidSelectorException to operate against.
 * @param hobjLinked the linked Exception object.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define InvalidSelectorException_setLinkedException(thisObj, hobjLinked)    JMSException_setLinkedException(thisObj, hobjLinked)

/**
 * Returns the int corresponding to the InvalidSelectorException type.
 *
 * @return the int corresponding to the InvalidSelectorException type
 */
int SMQ_API InvalidSelectorException_type();

/*
//---------------------------------
// MessageEOFException
//---------------------------------
*/
/**
 * Decrements the reference count of the MessageEOFException object.
 *
 * @param thisObj the MessageEOFException object to operate against.
 * @param pret will contain the reference count after decrementing.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define MessageEOFException_release(thisObj, pret)                     Object_release(thisObj, pret)

/**
 * Get the int corresponding to the type of a given object.
 *
 * @param thisObj the object to operate against.
 * @param pret will contain the int corresponding to the type.
 */
#define MessageEOFException_getType(thisObj, pret)                     Object_getType(thisObj, pret)

/**
 * Determines whether the given object is an instance of the given type.
 *
 * @param thisObj the object to check.
 * @param classtype the type to check against.
 * @param pret non-zero if the object is an instance of the type.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define MessageEOFException_instanceof(thisObj, classtype, pret)       Object_instanceof(thisObj, classtype, pret)

/**
 * Determines whether the two MessageEOFException objects are the same.
 *
 * @param thisObj the first MessageEOFException to compare.
 * @param hobj2 the second MessageEOFException to compare.
 * @param pret true if the MessageEOFException objects have identical content, false otherwise.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define MessageEOFException_equals(thisObj, hobj2, pret)               Object_equals(thisObj, hobj2, pret)

/**
 * Get the String representation for the given MessageEOFException.
 *
 * @param thisObj the MessageEOFException object to operate against.
 * @param pret will contain the String that represents the MessageEOFException.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define MessageEOFException_toString(thisObj, pret)                    Object_toString(thisObj, pret)

/**
 * Retrieve the String containing the localized message associated w/ this MessageEOFException.
 *
 * @param thisObj the MessageEOFException object to operate against.
 * @param pretString will contain the String containing the localized message.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define MessageEOFException_getLocalizedMessage(thisObj, pretString)   Throwable_getLocalizedMessage(thisObj, pretString)

/**
 * Retrieve the String containing the message associated w/ this MessageEOFException.
 *
 * @param thisObj the MessageEOFException object to operate against.
 * @param pretString will contain the String containing the message.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define MessageEOFException_getMessage(thisObj, pretString)            Throwable_getMessage(thisObj, pretString)

/**
 * Prints a stack trace w/ to the error location corresponding to the MessageEOFException object.
 *
 * @param thisObj the MessageEOFException object to operate against.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define MessageEOFException_printStackTrace(thisObj)                   Throwable_printStackTrace(thisObj)

/**
 * Get the vendor-specific error code.
 *
 * @param thisObj the MessageEOFException to operate against.
 * @param pretString will contain a String object specifying the vendor-specific error code.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define MessageEOFException_getErrorCode(thisObj, pretString)          JMSException_getErrorCode(thisObj, pretString)

/**
 * Get the exception linked to this one.
 *
 * @param thisObj the MessageEOFException to operate against.
 * @param pretException will contain the linked Exception object or null if none.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define MessageEOFException_getLinkedException(thisObj, pretException) JMSException_getLinkedException(thisObj, pretException)

/**
 * Add a linked exception.
 *
 * @param thisObj the MessageEOFException to operate against.
 * @param hobjLinked the linked Exception object.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define MessageEOFException_setLinkedException(thisObj, hobjLinked)    JMSException_setLinkedException(thisObj, hobjLinked)

/**
 * Returns the int corresponding to the MessageEOFException type.
 *
 * @return the int corresponding to the MessageEOFException type
 */
int SMQ_API MessageEOFException_type();

/*
//---------------------------------
// MessageFormatException
//---------------------------------
*/
/**
 * Decrements the reference count of the MessageFormatException object.
 *
 * @param thisObj the MessageFormatException object to operate against.
 * @param pret will contain the reference count after decrementing.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define MessageFormatException_release(thisObj, pret)                     Object_release(thisObj, pret)

/**
 * Get the int corresponding to the type of a given object.
 *
 * @param thisObj the object to operate against.
 * @param pret will contain the int corresponding to the type.
 */
#define MessageFormatException_getType(thisObj, pret)                     Object_getType(thisObj, pret)

/**
 * Determines whether the given object is an instance of the given type.
 *
 * @param thisObj the object to check.
 * @param classtype the type to check against.
 * @param pret non-zero if the object is an instance of the type.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define MessageFormatException_instanceof(thisObj, classtype, pret)       Object_instanceof(thisObj, classtype, pret)

/**
 * Determines whether the two MessageFormatException objects are the same.
 *
 * @param thisObj the first MessageFormatException to compare.
 * @param hobj2 the second MessageFormatException to compare.
 * @param pret true if the MessageFormatException objects have identical content, false otherwise.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define MessageFormatException_equals(thisObj, hobj2, pret)               Object_equals(thisObj, hobj2, pret)

/**
 * Get the String representation for the given MessageFormatException.
 *
 * @param thisObj the MessageFormatException object to operate against.
 * @param pret will contain the String that represents the MessageFormatException.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define MessageFormatException_toString(thisObj, pret)                    Object_toString(thisObj, pret)

/**
 * Retrieve the String containing the localized message associated w/ this MessageFormatException.
 *
 * @param thisObj the MessageFormatException object to operate against.
 * @param pretString will contain the String containing the localized message.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define MessageFormatException_getLocalizedMessage(thisObj, pretString)   Throwable_getLocalizedMessage(thisObj, pretString)

/**
 * Retrieve the String containing the message associated w/ this MessageFormatException.
 *
 * @param thisObj the MessageFormatException object to operate against.
 * @param pretString will contain the String containing the message.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define MessageFormatException_getMessage(thisObj, pretString)            Throwable_getMessage(thisObj, pretString)

/**
 * Prints a stack trace w/ to the error location corresponding to the MessageFormatException object.
 *
 * @param thisObj the MessageFormatException object to operate against.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define MessageFormatException_printStackTrace(thisObj)                   Throwable_printStackTrace(thisObj)

/**
 * Get the vendor-specific error code.
 *
 * @param thisObj the MessageFormatException to operate against.
 * @param pretString will contain a String object specifying the vendor-specific error code.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define MessageFormatException_getErrorCode(thisObj, pretString)          JMSException_getErrorCode(thisObj, pretString)

/**
 * Get the exception linked to this one.
 *
 * @param thisObj the MessageFormatException to operate against.
 * @param pretException will contain the linked Exception object or null if none.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define MessageFormatException_getLinkedException(thisObj, pretException) JMSException_getLinkedException(thisObj, pretException)

/**
 * Add a linked exception.
 *
 * @param thisObj the MessageFormatException to operate against.
 * @param hobjLinked the linked Exception object.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define MessageFormatException_setLinkedException(thisObj, hobjLinked)    JMSException_setLinkedException(thisObj, hobjLinked)

/**
 * Returns the int corresponding to the MessageFormatException type.
 *
 * @return the int corresponding to the MessageFormatException type
 */
int SMQ_API MessageFormatException_type();

/*
//---------------------------------
// MessageNotReadableException
//---------------------------------
*/
/**
 * Decrements the reference count of the MessageNotReadableException object.
 *
 * @param thisObj the MessageNotReadableException object to operate against.
 * @param pret will contain the reference count after decrementing.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define MessageNotReadableException_release(thisObj, pret)                     Object_release(thisObj, pret)

/**
 * Get the int corresponding to the type of a given object.
 *
 * @param thisObj the object to operate against.
 * @param pret will contain the int corresponding to the type.
 */
#define MessageNotReadableException_getType(thisObj, pret)                     Object_getType(thisObj, pret)

/**
 * Determines whether the given object is an instance of the given type.
 *
 * @param thisObj the object to check.
 * @param classtype the type to check against.
 * @param pret non-zero if the object is an instance of the type.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define MessageNotReadableException_instanceof(thisObj, classtype, pret)       Object_instanceof(thisObj, classtype, pret)

/**
 * Determines whether the two MessageNotReadableException objects are the same.
 *
 * @param thisObj the first MessageNotReadableException to compare.
 * @param hobj2 the second MessageNotReadableException to compare.
 * @param pret true if the MessageNotReadableException objects have identical content, false otherwise.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define MessageNotReadableException_equals(thisObj, hobj2, pret)               Object_equals(thisObj, hobj2, pret)

/**
 * Get the String representation for the given MessageNotReadableException.
 *
 * @param thisObj the MessageNotReadableException object to operate against.
 * @param pret will contain the String that represents the MessageNotReadableException.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define MessageNotReadableException_toString(thisObj, pret)                    Object_toString(thisObj, pret)

/**
 * Retrieve the String containing the localized message associated w/ this MessageNotReadableException.
 *
 * @param thisObj the MessageNotReadableException object to operate against.
 * @param pretString will contain the String containing the localized message.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define MessageNotReadableException_getLocalizedMessage(thisObj, pretString)   Throwable_getLocalizedMessage(thisObj, pretString)

/**
 * Retrieve the String containing the message associated w/ this MessageNotReadableException.
 *
 * @param thisObj the MessageNotReadableException object to operate against.
 * @param pretString will contain the String containing the message.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define MessageNotReadableException_getMessage(thisObj, pretString)            Throwable_getMessage(thisObj, pretString)

/**
 * Prints a stack trace w/ to the error location corresponding to the MessageNotReadableException object.
 *
 * @param thisObj the MessageNotReadableException object to operate against.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define MessageNotReadableException_printStackTrace(thisObj)                   Throwable_printStackTrace(thisObj)

/**
 * Get the vendor-specific error code.
 *
 * @param thisObj the MessageNotReadableException to operate against.
 * @param pretString will contain a String object specifying the vendor-specific error code.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define MessageNotReadableException_getErrorCode(thisObj, pretString)          JMSException_getErrorCode(thisObj, pretString)

/**
 * Get the exception linked to this one.
 *
 * @param thisObj the MessageNotReadableException to operate against.
 * @param pretException will contain the linked Exception object or null if none.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define MessageNotReadableException_getLinkedException(thisObj, pretException) JMSException_getLinkedException(thisObj, pretException)

/**
 * Add a linked exception.
 *
 * @param thisObj the MessageNotReadableException to operate against.
 * @param hobjLinked the linked Exception object.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define MessageNotReadableException_setLinkedException(thisObj, hobjLinked)    JMSException_setLinkedException(thisObj, hobjLinked)

/**
 * Returns the int corresponding to the MessageNotReadableException type.
 *
 * @return the int corresponding to the MessageNotReadableException type
 */
int SMQ_API MessageNotReadableException_type();

/*
//---------------------------------
// MessageNotWriteableException
//---------------------------------
*/
/**
 * Decrements the reference count of the MessageNotWriteableException object.
 *
 * @param thisObj the MessageNotWriteableException object to operate against.
 * @param pret will contain the reference count after decrementing.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define MessageNotWriteableException_release(thisObj, pret)                     Object_release(thisObj, pret)

/**
 * Get the int corresponding to the type of a given object.
 *
 * @param thisObj the object to operate against.
 * @param pret will contain the int corresponding to the type.
 */
#define MessageNotWriteableException_getType(thisObj, pret)                     Object_getType(thisObj, pret)

/**
 * Determines whether the given object is an instance of the given type.
 *
 * @param thisObj the object to check.
 * @param classtype the type to check against.
 * @param pret non-zero if the object is an instance of the type.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define MessageNotWriteableException_instanceof(thisObj, classtype, pret)       Object_instanceof(thisObj, classtype, pret)

/**
 * Determines whether the two MessageNotWriteableException objects are the same.
 *
 * @param thisObj the first MessageNotWriteableException to compare.
 * @param hobj2 the second MessageNotWriteableException to compare.
 * @param pret true if the MessageNotWriteableException objects have identical content, false otherwise.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define MessageNotWriteableException_equals(thisObj, hobj2, pret)               Object_equals(thisObj, hobj2, pret)

/**
 * Get the String representation for the given MessageNotWriteableException.
 *
 * @param thisObj the MessageNotWriteableException object to operate against.
 * @param pret will contain the String that represents the MessageNotWriteableException.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define MessageNotWriteableException_toString(thisObj, pret)                    Object_toString(thisObj, pret)

/**
 * Retrieve the String containing the localized message associated w/ this MessageNotWriteableException.
 *
 * @param thisObj the MessageNotWriteableException object to operate against.
 * @param pretString will contain the String containing the localized message.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define MessageNotWriteableException_getLocalizedMessage(thisObj, pretString)   Throwable_getLocalizedMessage(thisObj, pretString)

/**
 * Retrieve the String containing the message associated w/ this MessageNotWriteableException.
 *
 * @param thisObj the MessageNotWriteableException object to operate against.
 * @param pretString will contain the String containing the message.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define MessageNotWriteableException_getMessage(thisObj, pretString)            Throwable_getMessage(thisObj, pretString)

/**
 * Prints a stack trace w/ to the error location corresponding to the MessageNotWriteableException object.
 *
 * @param thisObj the MessageNotWriteableException object to operate against.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define MessageNotWriteableException_printStackTrace(thisObj)                   Throwable_printStackTrace(thisObj)

/**
 * Get the vendor-specific error code.
 *
 * @param thisObj the MessageNotWriteableException to operate against.
 * @param pretString will contain a String object specifying the vendor-specific error code.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define MessageNotWriteableException_getErrorCode(thisObj, pretString)          JMSException_getErrorCode(thisObj, pretString)

/**
 * Get the exception linked to this one.
 *
 * @param thisObj the MessageNotWriteableException to operate against.
 * @param pretException will contain the linked Exception object or null if none.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define MessageNotWriteableException_getLinkedException(thisObj, pretException) JMSException_getLinkedException(thisObj, pretException)

/**
 * Add a linked exception.
 *
 * @param thisObj the MessageNotWriteableException to operate against.
 * @param hobjLinked the linked Exception object.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define MessageNotWriteableException_setLinkedException(thisObj, hobjLinked)    JMSException_setLinkedException(thisObj, hobjLinked)

/**
 * Returns the int corresponding to the MessageNotWriteableException type.
 *
 * @return the int corresponding to the MessageNotWriteableException type
 */
int SMQ_API MessageNotWriteableException_type();

/*
//---------------------------------
// ResourceAllocationException
//---------------------------------
*/
/**
 * Decrements the reference count of the ResourceAllocationException object.
 *
 * @param thisObj the ResourceAllocationException object to operate against.
 * @param pret will contain the reference count after decrementing.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define ResourceAllocationException_release(thisObj, pret)                     Object_release(thisObj, pret)

/**
 * Get the int corresponding to the type of a given object.
 *
 * @param thisObj the object to operate against.
 * @param pret will contain the int corresponding to the type.
 */
#define ResourceAllocationException_getType(thisObj, pret)                     Object_getType(thisObj, pret)

/**
 * Determines whether the given object is an instance of the given type.
 *
 * @param thisObj the object to check.
 * @param classtype the type to check against.
 * @param pret non-zero if the object is an instance of the type.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define ResourceAllocationException_instanceof(thisObj, classtype, pret)       Object_instanceof(thisObj, classtype, pret)

/**
 * Determines whether the two ResourceAllocationException objects are the same.
 *
 * @param thisObj the first ResourceAllocationException to compare.
 * @param hobj2 the second ResourceAllocationException to compare.
 * @param pret true if the ResourceAllocationException objects have identical content, false otherwise.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define ResourceAllocationException_equals(thisObj, hobj2, pret)               Object_equals(thisObj, hobj2, pret)

/**
 * Get the String representation for the given ResourceAllocationException.
 *
 * @param thisObj the ResourceAllocationException object to operate against.
 * @param pret will contain the String that represents the ResourceAllocationException.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define ResourceAllocationException_toString(thisObj, pret)                    Object_toString(thisObj, pret)

/**
 * Retrieve the String containing the localized message associated w/ this ResourceAllocationException.
 *
 * @param thisObj the ResourceAllocationException object to operate against.
 * @param pretString will contain the String containing the localized message.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define ResourceAllocationException_getLocalizedMessage(thisObj, pretString)   Throwable_getLocalizedMessage(thisObj, pretString)

/**
 * Retrieve the String containing the message associated w/ this ResourceAllocationException.
 *
 * @param thisObj the ResourceAllocationException object to operate against.
 * @param pretString will contain the String containing the message.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define ResourceAllocationException_getMessage(thisObj, pretString)            Throwable_getMessage(thisObj, pretString)

/**
 * Prints a stack trace w/ to the error location corresponding to the ResourceAllocationException object.
 *
 * @param thisObj the ResourceAllocationException object to operate against.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define ResourceAllocationException_printStackTrace(thisObj)                   Throwable_printStackTrace(thisObj)

/**
 * Get the vendor-specific error code.
 *
 * @param thisObj the ResourceAllocationException to operate against.
 * @param pretString will contain a String object specifying the vendor-specific error code.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define ResourceAllocationException_getErrorCode(thisObj, pretString)          JMSException_getErrorCode(thisObj, pretString)

/**
 * Get the exception linked to this one.
 *
 * @param thisObj the ResourceAllocationException to operate against.
 * @param pretException will contain the linked Exception object or null if none.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define ResourceAllocationException_getLinkedException(thisObj, pretException) JMSException_getLinkedException(thisObj, pretException)

/**
 * Add a linked exception.
 *
 * @param thisObj the ResourceAllocationException to operate against.
 * @param hobjLinked the linked Exception object.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define ResourceAllocationException_setLinkedException(thisObj, hobjLinked)    JMSException_setLinkedException(thisObj, hobjLinked)

/**
 * Returns the int corresponding to the ResourceAllocationException type.
 *
 * @return the int corresponding to the ResourceAllocationException type
 */
int SMQ_API ResourceAllocationException_type();

/*
//---------------------------------
// TransactionInProgressException
//---------------------------------
*/
/**
 * Decrements the reference count of the TransactionInProgressException object.
 *
 * @param thisObj the TransactionInProgressException object to operate against.
 * @param pret will contain the reference count after decrementing.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define TransactionInProgressException_release(thisObj, pret)                     Object_release(thisObj, pret)

/**
 * Get the int corresponding to the type of a given object.
 *
 * @param thisObj the object to operate against.
 * @param pret will contain the int corresponding to the type.
 */
#define TransactionInProgressException_getType(thisObj, pret)                     Object_getType(thisObj, pret)

/**
 * Determines whether the given object is an instance of the given type.
 *
 * @param thisObj the object to check.
 * @param classtype the type to check against.
 * @param pret non-zero if the object is an instance of the type.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define TransactionInProgressException_instanceof(thisObj, classtype, pret)       Object_instanceof(thisObj, classtype, pret)

/**
 * Determines whether the two TransactionInProgressException objects are the same.
 *
 * @param thisObj the first TransactionInProgressException to compare.
 * @param hobj2 the second TransactionInProgressException to compare.
 * @param pret true if the TransactionInProgressException objects have identical content, false otherwise.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define TransactionInProgressException_equals(thisObj, hobj2, pret)               Object_equals(thisObj, hobj2, pret)

/**
 * Get the String representation for the given TransactionInProgressException.
 *
 * @param thisObj the TransactionInProgressException object to operate against.
 * @param pret will contain the String that represents the TransactionInProgressException.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define TransactionInProgressException_toString(thisObj, pret)                    Object_toString(thisObj, pret)

/**
 * Retrieve the String containing the localized message associated w/ this TransactionInProgressException.
 *
 * @param thisObj the TransactionInProgressException object to operate against.
 * @param pretString will contain the String containing the localized message.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define TransactionInProgressException_getLocalizedMessage(thisObj, pretString)   Throwable_getLocalizedMessage(thisObj, pretString)

/**
 * Retrieve the String containing the message associated w/ this TransactionInProgressException.
 *
 * @param thisObj the TransactionInProgressException object to operate against.
 * @param pretString will contain the String containing the message.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define TransactionInProgressException_getMessage(thisObj, pretString)            Throwable_getMessage(thisObj, pretString)

/**
 * Prints a stack trace w/ to the error location corresponding to the TransactionInProgressException object.
 *
 * @param thisObj the TransactionInProgressException object to operate against.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define TransactionInProgressException_printStackTrace(thisObj)                   Throwable_printStackTrace(thisObj)

/**
 * Get the vendor-specific error code.
 *
 * @param thisObj the TransactionInProgressException to operate against.
 * @param pretString will contain a String object specifying the vendor-specific error code.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define TransactionInProgressException_getErrorCode(thisObj, pretString)          JMSException_getErrorCode(thisObj, pretString)

/**
 * Get the exception linked to this one.
 *
 * @param thisObj the TransactionInProgressException to operate against.
 * @param pretException will contain the linked Exception object or null if none.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define TransactionInProgressException_getLinkedException(thisObj, pretException) JMSException_getLinkedException(thisObj, pretException)

/**
 * Add a linked exception.
 *
 * @param thisObj the TransactionInProgressException to operate against.
 * @param hobjLinked the linked Exception object.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define TransactionInProgressException_setLinkedException(thisObj, hobjLinked)    JMSException_setLinkedException(thisObj, hobjLinked)

/**
 * Returns the int corresponding to the TransactionInProgressException type.
 *
 * @return the int corresponding to the TransactionInProgressException type
 */
int SMQ_API TransactionInProgressException_type();

/*
//---------------------------------
// TransactionRolledBackException
//---------------------------------
*/
/**
 * Decrements the reference count of the TransactionRolledBackException object.
 *
 * @param thisObj the TransactionRolledBackException object to operate against.
 * @param pret will contain the reference count after decrementing.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define TransactionRolledBackException_release(thisObj, pret)                     Object_release(thisObj, pret)

/**
 * Get the int corresponding to the type of a given object.
 *
 * @param thisObj the object to operate against.
 * @param pret will contain the int corresponding to the type.
 */
#define TransactionRolledBackException_getType(thisObj, pret)                     Object_getType(thisObj, pret)

/**
 * Determines whether the given object is an instance of the given type.
 *
 * @param thisObj the object to check.
 * @param classtype the type to check against.
 * @param pret non-zero if the object is an instance of the type.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define TransactionRolledBackException_instanceof(thisObj, classtype, pret)       Object_instanceof(thisObj, classtype, pret)

/**
 * Determines whether the two TransactionRolledBackException objects are the same.
 *
 * @param thisObj the first TransactionRolledBackException to compare.
 * @param hobj2 the second TransactionRolledBackException to compare.
 * @param pret true if the TransactionRolledBackException objects have identical content, false otherwise.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define TransactionRolledBackException_equals(thisObj, hobj2, pret)               Object_equals(thisObj, hobj2, pret)

/**
 * Get the String representation for the given TransactionRolledBackException.
 *
 * @param thisObj the TransactionRolledBackException object to operate against.
 * @param pret will contain the String that represents the TransactionRolledBackException.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define TransactionRolledBackException_toString(thisObj, pret)                    Object_toString(thisObj, pret)

/**
 * Retrieve the String containing the localized message associated w/ this TransactionRolledBackException.
 *
 * @param thisObj the TransactionRolledBackException object to operate against.
 * @param pretString will contain the String containing the localized message.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define TransactionRolledBackException_getLocalizedMessage(thisObj, pretString)   Throwable_getLocalizedMessage(thisObj, pretString)

/**
 * Retrieve the String containing the message associated w/ this TransactionRolledBackException.
 *
 * @param thisObj the TransactionRolledBackException object to operate against.
 * @param pretString will contain the String containing the message.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define TransactionRolledBackException_getMessage(thisObj, pretString)            Throwable_getMessage(thisObj, pretString)

/**
 * Prints a stack trace w/ to the error location corresponding to the TransactionRolledBackException object.
 *
 * @param thisObj the TransactionRolledBackException object to operate against.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define TransactionRolledBackException_printStackTrace(thisObj)                   Throwable_printStackTrace(thisObj)

/**
 * Get the vendor-specific error code.
 *
 * @param thisObj the TransactionRolledBackException to operate against.
 * @param pretString will contain a String object specifying the vendor-specific error code.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define TransactionRolledBackException_getErrorCode(thisObj, pretString)          JMSException_getErrorCode(thisObj, pretString)

/**
 * Get the exception linked to this one.
 *
 * @param thisObj the TransactionRolledBackException to operate against.
 * @param pretException will contain the linked Exception object or null if none.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define TransactionRolledBackException_getLinkedException(thisObj, pretException) JMSException_getLinkedException(thisObj, pretException)

/**
 * Add a linked exception.
 *
 * @param thisObj the TransactionRolledBackException to operate against.
 * @param hobjLinked the linked Exception object.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define TransactionRolledBackException_setLinkedException(thisObj, hobjLinked)    JMSException_setLinkedException(thisObj, hobjLinked)

/**
 * Returns the int corresponding to the TransactionRolledBackException type.
 *
 * @return the int corresponding to the TransactionRolledBackException type
 */
int SMQ_API TransactionRolledBackException_type();

/*
//---------------------------------
// Message
//---------------------------------
*/
/**
 * Decrements the reference count of the Message object.
 *
 * @param msgObj the Message object to operate against.
 * @param pret will contain the reference count after decrementing.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Message_release(msgObj, pret)               Object_release(msgObj, pret)

/**
 * Get the int corresponding to the type of a given object.
 *
 * @param thisObj the object to operate against.
 * @param pret will contain the int corresponding to the type.
 */
#define Message_getType(thisObj, pret)              Object_getType(thisObj, pret)

/**
 * Determines whether the given object is an instance of the given type.
 *
 * @param thisObj the object to check.
 * @param classtype the type to check against.
 * @param pret non-zero if the object is an instance of the type.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Message_instanceof(thisObj, classtype, pret) Object_instanceof(thisObj, classtype, pret)

/**
 * Determines whether the two Message objects are the same.
 *
 * @param msgObj the first Message to compare.
 * @param hobj2 the second Message to compare.
 * @param pret true if the Message objects have identical content, false otherwise.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Message_equals(msgObj, hobj2, pret)         Object_equals(msgObj, hobj2, pret)

/**
 * Get the String representation for the given Message.
 *
 * @param msgObj the Message object to operate against.
 * @param pret will contain the String that represents the Message.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Message_toString(msgObj, pret)              Object_toString(msgObj, pret)

/**
 * Create a new Message that is a copy of the given Message.
 *
 * @param msgObj the Message to clone.
 * @param pret will contain the newly created Message.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Message_clone(msgObj, pret)                 Object_clone(msgObj, pret)

/**
 * Returns the int corresponding to the Message type.
 *
 * @return the int corresponding to the Message type
 */
int SMQ_API Message_type();

/** Get the message ID.
  *
  * <P>The messageID header field contains a value that uniquely
  * identifies each message sent by a SonicMQ client.
  *
  * <P>When a message is sent, messageID can be ignored. When
  * the send method returns it contains an assigned value.
  *
  * <P>A JMSMessageID is a String value which should function as a
  * unique key for identifying messages in a historical repository.
  *
  * <P>All JMSMessageID values must start with the prefix 'ID:'.
  * Uniqueness of message ID values across different brokers is
  * not required.
  *
  * <P>Since message ID's take some effort to create and increase a
  * message's size, SonicMQ can optimize message
  * overhead if it is given a hint that message ID is not used by
  * an application. SonicMQ message Producers provide a hint to disable
  * message ID. When a client sets a Producer to disable message ID
  * they are saying that they do not depend on the value of message
  * ID for the messages it produces. These messages must either have
  * message ID set to null or, if the hint is ignored, messageID must
  * be set to its normal unique value.
  *
  * @param msgObj the Message to operate against
  * @param pMsgId the returned message ID as a String
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  *
  * @see Message_setJMSMessageID()
  */
int SMQ_API Message_getJMSMessageID(HOBJ msgObj, HOBJ *pmsgId);

/** Set the message ID.
  *
  * <P>The SonicMQ run time sets this field when a message is sent. This operation
  * can be used to change the value of a message that has been received.
  *
  * @param msgObj the Message to operate against
  * @param msgId the String to set the message ID to
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  *
  * @see Message_getJMSMessageID()
  */
int SMQ_API Message_setJMSMessageID(HOBJ msgObj, HOBJ msgId);

/** Get the message timestamp.
  *
  * <P>The JMSTimestamp header field contains the time a message was
  * handed off to the SonicMQW client run time to be sent. It is not the time the
  * message was actually transmitted because the actual send may occur
  * later due to transactions or other client side queueing of messages.
  *
  * <P>When a message is sent, JMSTimestamp is ignored. When the send
  * method returns it contains a a time value somewhere in the interval
  * between the call and the return. It is in the format of a normal
  * Java millis time value.
  *
  * <P>Since timestamps take some effort to create and increase a
  * message's size, SonicMQ can optimize message
  * overhead if it is given a hint that timestamp is not used by an
  * application. SonicMQ message Producers provide a hint to disable
  * timestamps. When a client sets a producer to disable timestamps
  * they are saying that they do not depend on the value of timestamp
  * for the messages it produces. These messages must either have
  * timestamp set to null or, if the hint is ignored, timestamp must
  * be set to its normal value.
  *
  * @param msgObj the Message to operate against
  * @param pTimestamp the message timestamp value
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  *
  * @see Message_setJMSTimestamp()
  */
int SMQ_API Message_getJMSTimestamp(HOBJ msgObj, jlong *pTimestamp);

/** Set the message timestamp.
  *
  * <P>SonicMQ sets this field when a message is sent. This operation
  * can be used to change the value of a message that's been received.
  *
  * @param msgObj the Message to operate against
  * @param timestamp the timestamp for this message
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  *
  * @see Message_getJMSTimestamp()
  */
int SMQ_API Message_setJMSTimestamp(HOBJ msgObj, jlong timestamp);

/** Get the correlation ID for the message.
  *
  * <P>This method is used to return correlation id values that are
  * either SonicMQ-specific message ID's or application-specific Strings.
  *
  * @param msgObj the Message to operate against
  * @param pCOrrelationID the correlation ID of a message as a String.
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  *
  * @see Message_setJMSCorrelationID()
  * @see Message_getJMSCorrelationIDAsBytes()
  * @see Message_setJMSCorrelationIDAsBytes()
  */
int SMQ_API Message_getJMSCorrelationID(HOBJ msgObj, HOBJ *pCorrelationID);

/** Set the correlation ID for the message.
  *
  * <P>A client can use the JMSCorrelationID header field to link one
  * message with another. A typically use is to link a response message
  * with its request message.
  *
  * <P>JMSCorrelationID can hold one of the following:
  *    <UL>
  *      <LI>A SonicMQ-specific message ID
  *      <LI>An application-specific String
  *      <LI>A native jbyteArray value.
  *    </UL>
  *
  * <P>Since each message sent in SonicMQ is assigned a message ID
  * value it is convenient to link messages via message ID. All message ID
  * values must start with the 'ID:' prefix.
  *
  * <P>In some cases, an application (made up of several clients) needs to
  * use an application specific value for linking messages. For instance,
  * an application may use JMSCorrelationID to hold a value referencing
  * some external information. Application specified values must not start
  * with the 'ID:' prefix; this is reserved for SonicMQ-generated message
  * ID values.
  *
  * <P>If a provider supports the native concept of correlation ID, a SonicMQ
  * client may need to assign specific JMSCorrelationID values to match
  * those expected by non-JMS clients. A jbyteArray value is used for this
  * purpose. JMS providers without native correlation ID values are not
  * required to support jbyteArray values. The use of a jbyteArray value for
  * JMSCorrelationID is non-portable.
  *
  * @param msgObj the Message to operate against
  * @param correlationID the message ID of a message being referred to.
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  *
  * @see Message_getJMSCorrelationID()
  * @see Message_getJMSCorrelationIDAsBytes()
  * @see Message_setJMSCorrelationIDAsBytes()
  */
int SMQ_API Message_setJMSCorrelationID(HOBJ msgObj, HOBJ correlationID);

/** Get the correlation ID as an array of bytes for the message.
  *
  * <P>The use of a jbyteArray value for JMSCorrelationID is non-portable.
  *
  * @param msgObj the Message to operate against
  * @param pCorrelationID the correlation ID of a message as an array of bytes.
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  *
  * @see Message_setJMSCorrelationID()
  * @see Message_getJMSCorrelationID()
  */
int SMQ_API Message_getJMSCorrelationIDAsBytes(HOBJ msgObj, HOBJ *pCorrelationID);

/** Set the correlation ID as an array of bytes for the message.
  *
  * <P>If a provider supports the native concept of correlation id, a
  * SonicMQ client may need to assign specific JMSCorrelationID values to
  * match those expected by non-JMS clients. JMS providers without native
  * correlation id values are not required to support this (and the
  * corresponding get) method; their implementation may throw
  * java.lang.UnsupportedOperationException).
  *
  * <P>The use of a jbyteArray value for JMSCorrelationID is non-portable.
  *
  * @param msgObj the Message to operate against
  * @param correlationID the correlation ID value as an array of bytes.
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  *
  * @see Message_setJMSCorrelationID()
  * @see Message_getJMSCorrelationID()
  * @see Message_getJMSCorrelationIDAsBytes()
  */
int SMQ_API Message_setJMSCorrelationIDAsBytes(HOBJ msgObj, HOBJ value);

/** Get where a reply to this message should be sent.
  *
  * @param msgObj the Message to operate against
  * @param pDestination the destination to send a response to this message
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  *
  * @see Message_setJMSReplyTo()
  */
int SMQ_API Message_getJMSReplyTo(HOBJ msgObj, HOBJ *pDestination);

/** Set where a reply to this message should be sent.
  *
  * <P>The replyTo header field contains the destination where a reply
  * to the current message should be sent. If it is null no reply is
  * expected. The destination may be either a Queue or a Topic.
  *
  * <P>Messages with a null replyTo value are called SonicMQ datagrams.
  * Datagrams may be a notification of some change in the sender (i.e.
  * they signal a sender event) or they may just be some data the sender
  * thinks is of interest.
  *
  * Messages with a replyTo value are typically expecting a response.
  * A response may be optional, it is up to the client to decide. These
  * messages are called SonicMQ requests. A message sent in response to a
  * request is called a reply.
  *
  * In some cases a client may wish to match up a request it sent earlier
  * with a reply it has just received. This can be done using the
  * correlationID.
  *
  * @param msgObj the Message to operate against
  * @param replyTo Destination to send a response to this message
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  *
  * @see Message_getJMSReplyTo()
  */
int SMQ_API Message_setJMSReplyTo(HOBJ msgObj, HOBJ replyto);

/** Get the destination for this message.
  *
  * <P>The destination field contains the destination to which the
  * message is being sent.
  *
  * <P>When a message is sent this value is ignored. After completion
  * of the send method it holds the destination specified by the send.
  *
  * <P>When a message is received, its destination value must be
  * equivalent to the value assigned when it was sent.
  *
  * @param msgObj the Message to operate against
  * @param pDestination the Destination of this message.
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  *
  * @see Message_setJMSDestination()
  */
int SMQ_API Message_getJMSDestination(HOBJ msgObj, HOBJ *pDestination);

/** Set the destination for this message.
  *
  * <P>SonicMQ sets this field when a message is sent. This operation
  * can be used to change the value of a message that's been received.
  *
  * @param msgObj the Message to operate against
  * @param destination the Destination for this message.
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  *
  * @see Message_getJMSDestination()
  */
int SMQ_API Message_setJMSDestination(HOBJ msgObj, HOBJ destination);

/** Get the delivery mode for this message.
  *
  * @param msgObj the Message to operate against
  * @param pdeliveryMode the delivery mode of this message.
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  *
  * @see Message_setJMSDeliveryMode()
  */
int SMQ_API Message_getJMSDeliveryMode(HOBJ msgObj, jint *pdeliveryMode);

/**
 * Set the delivery mode for this message.
 *
 * <p>Providers set this field when a message is sent. This operation
 * can be used to change the value of a message that's been received.
 *
 * @param msgObj the Message to operate against
 * @param value the delivery mode for this message.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @see Message_setJMSDeliveryMode()
 *
 * @exception JMSException if JMS fails to set JMS DeliveryMode
 *                         due to some internal JMS error.
 */
int SMQ_API Message_setJMSDeliveryMode(HOBJ msgObj, jint value);

/**
 * Get an indication of whether this message is being redelivered.
 *
 * <p>If a client receives a message with the redelivered indicator set,
 * it is likely, but not guaranteed, that this message was delivered to
 * the client earlier but the client did not acknowledge its receipt at
 * that earlier time.
 *
 * @param msgObj the Message to operate against
 * @param pret set to true if this message is being redelivered.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @see Message_setJMSDeliveryMode()
 *
 *
 * @exception JMSException if JMS fails to get JMS Redelivered flag
 *                         due to some internal JMS error.
 */
int SMQ_API Message_getJMSRedelivered(HOBJ msgObj, jboolean *pret);

/**
 * Set to indicate whether this message is being redelivered.
 *
 * <p>This field is set at the time the message is delivered. This
 * operation can be used to change the value of a message that's
 * been received.
 *
 * @param msgObj the Message to operate against
 * @param value an indication of whether this message is being redelivered.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @see Message_setJMSDeliveryMode()
 *
 * @exception JMSException if JMS fails to set JMS Redelivered flag
 *                         due to some internal JMS error.
 */
int SMQ_API Message_setJMSRedelivered(HOBJ msgObj, jboolean value);

/**
 * Get the message type.
 *
 * @param msgObj the Message to operate against
 * @param pret the message type
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to get JMS message type
 *                         due to some internal JMS error.
 */
int SMQ_API Message_getJMSType(HOBJ msgObj, HOBJ *pret);

/** Set the message type.
 *
 * <p>Some JMS providers use a message repository that contains the
 * definition of messages sent by applications. The type header field
 * contains the name of a message's definition.
 *
 * <p>JMS does not define a standard message definition repository nor
 * does it define a naming policy for the definitions it contains. JMS
 * clients should use symbolic values for type that can be configured
 * at installation time to the values defined in the current providers
 * message repository.
 *
 * <p>JMS clients should assign a value to type whether the application
 * makes use of it or not. This insures that it is properly set for
 * those providers that require it.
 *
 * @param msgObj the Message to operate against
 * @param value the class of message
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to set JMS message type
 *                         due to some internal JMS error.
 */
int SMQ_API Message_setJMSType(HOBJ msgObj, HOBJ value);

/**
 * Get the message's expiration value.
 *
 * <p>When a message is sent, expiration is left unassigned. After
 * completion of the send method, it holds the expiration time of the
 * message. This is the sum of the time-to-live value specified by the
 * client and the GMT at the time of the send.
 *
 * <p>If the time-to-live is specified as zero, expiration is set to
 * zero which indicates the message does not expire.
 *
 * <p>When a message's expiration time is reached, a provider should
 * discard it. JMS does not define any form of notification of message
 * expiration.
 *
 * <p>Clients should not receive messages that have expired; however,
 * JMS does not guarantee that this will not happen.
 *
 * @param msgObj the Message to operate against
 * @param pret the time the message expires. It is the sum of the
 * time-to-live value specified by the client, and the GMT at the
 * time of the send.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to get JMS message expiration
 *                         due to some internal JMS error.
 */
int SMQ_API Message_getJMSExpiration(HOBJ msgObj, jlong *pret);

/**
 * Set the message's expiration value.
 *
 * <p>Providers set this field when a message is sent. This operation
 * can be used to change the value of a message that's been received.
 *
 * @param msgObj the Message to operate against
 * @param value the message's expiration time
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to set JMS message expiration
 *                         due to some internal JMS error.
 */
int SMQ_API Message_setJMSExpiration(HOBJ msgObj, jlong value);

/**
 * Get the message priority.
 *
 * <p>JMS defines a ten level priority value with 0 as the lowest
 * priority and 9 as the highest. In addition, clients should consider
 * priorities 0-4 as gradations of normal priority and priorities 5-9
 * as gradations of expedited priority.
 *
 * <p>JMS does not require that a provider strictly implement priority
 * ordering of messages; however, it should do its best to deliver
 * expedited messages ahead of normal messages.
 *
 * @param msgObj the Message to operate against
 * @param pret the default message priority
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to get JMS message priority
 *                         due to some internal JMS error.
 */
int SMQ_API Message_getJMSPriority(HOBJ msgObj, jint *pret);

/**
 * Set the priority for this message.
 *
 * <p>Providers set this field when a message is sent. This operation
 * can be used to change the value of a message that's been received.
 *
 * @param msgObj the Message to operate against
 * @param value the priority of this message
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to set JMS message priority
 *                         due to some internal JMS error.
 */
int SMQ_API Message_setJMSPriority(HOBJ msgObj, jint value);

/**
 * Clear a message's properties.
 *
 * @param msgObj the Message to operate against
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to clear JMS message
 *                         properties due to some internal JMS
 *                         error.
 */
int SMQ_API Message_clearProperties(HOBJ msgObj);

/** Check if a property value exists.
 *
 * @param msgObj the Message to operate against
 * @param name the name of the property to test as a String
 * @param pret true if the property does exist.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to  check if property
 *                         exists due to some internal JMS
 *                         error.
 */
int SMQ_API Message_propertyExists(HOBJ msgObj, HOBJ name, jboolean *pret);

/**
 * Return the boolean property value with the given name.
 *
 * @param msgObj the Message to operate against
 * @param name the name of the boolean property as a String
 * @param pret the boolean property value with the given name.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to  get Property due to
 *                         some internal JMS error.
 * @exception MessageFormatException if this type conversion is invalid.
 * @exception NullPointerException if property does not exist.
 */
int SMQ_API Message_getBooleanProperty(HOBJ msgObj, HOBJ name, jboolean *pret);

/**
 * Return the byte property value with the given name.
 *
 * @param msgObj the Message to operate against
 * @param name the name of the byte property as a String
 * @param pret the byte property value with the given name.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to  get Property due to
 *                         some internal JMS error.
 * @exception MessageFormatException if this type conversion is invalid.
 * @exception NullPointerException if property does not exist.
 */
int SMQ_API Message_getByteProperty(HOBJ msgObj, HOBJ name, jbyte *pret);

/**
 * Return the short property value with the given name.
 *
 * @param msgObj the Message to operate against
 * @param name the name of the short property as a String
 * @param pret the short property value with the given name.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to  get Property due to
 *                         some internal JMS error.
 * @exception MessageFormatException if this type conversion is invalid.
 * @exception NullPointerException if property does not exist.
 */
int SMQ_API Message_getShortProperty(HOBJ msgObj, HOBJ name, jshort *pret);

/**
 * Return the integer property value with the given name.
 *
 * @param msgObj the Message to operate against
 * @param name the name of the integer property as a String
 * @param pret the integer property value with the given name.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to  get Property due to
 *                         some internal JMS error.
 * @exception MessageFormatException if this type conversion is invalid.
 * @exception NullPointerException if property does not exist.
 */
int SMQ_API Message_getIntProperty(HOBJ msgObj, HOBJ name, jint *pret);

/**
 * Return the long property value with the given name.
 *
 * @param msgObj the Message to operate against
 * @param name the name of the long property as a String
 * @param pret the long property value with the given name.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to  get Property due to
 *                         some internal JMS error.
 * @exception MessageFormatException if this type conversion is invalid.
 * @exception NullPointerException if property does not exist.
 */
int SMQ_API Message_getLongProperty(HOBJ msgObj, HOBJ name, jlong *pret);

/**
 * Return the float property value with the given name.
 *
 * @param msgObj the Message to operate against
 * @param name the name of the float property as a String
 * @param pret the float property value with the given name.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to  get Property due to
 *                         some internal JMS error.
 * @exception MessageFormatException if this type conversion is invalid.
 * @exception NullPointerException if property does not exist.
 */
int SMQ_API Message_getFloatProperty(HOBJ msgObj, HOBJ name, jfloat *pret);

/**
 * Return the double property value with the given name.
 *
 * @param msgObj the Message to operate against
 * @param name the name of the double property as a String
 * @param pret the double property value with the given name.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to  get Property due to
 *                         some internal JMS error.
 * @exception MessageFormatException if this type conversion is invalid.
 * @exception NullPointerException if property does not exist.
 */
int SMQ_API Message_getDoubleProperty(HOBJ msgObj, HOBJ name, jdouble *pret);

/**
 * Return the String property value with the given name.
 *
 * @param msgObj the Message to operate against
 * @param name the name of the String property as a String
 * @param pret the String property value with the given name.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to  get Property due to
 *                         some internal JMS error.
 * @exception MessageFormatException if this type conversion is invalid.
 * @exception NullPointerException if property does not exist.
 */
int SMQ_API Message_getStringProperty(HOBJ msgObj, HOBJ name, HOBJ *pret);

/**
 * Return the Object property value with the given name.
 *
 * @param msgObj the Message to operate against
 * @param name the name of the Object property as a String
 * @param pret the Object property value with the given name.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to  get Property due to
 *                         some internal JMS error.
 * @exception MessageFormatException if this type conversion is invalid.
 * @exception NullPointerException if property does not exist.
 */
int SMQ_API Message_getObjectProperty(HOBJ msgObj, HOBJ name, HOBJ *pret);

/**
 * Return an Enumeration of all the property names.
 *
 * @param msgObj the Message to operate against
 * @param pret an Enumeration w/ all the names of property values.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to  get Property names due to
 *                         some internal JMS error.
 */
int SMQ_API Message_getPropertyNames(HOBJ msgObj, HOBJ *pret);

/**
 * Set a boolean property value with the given name, into the Message.
 *
 * @param msgObj the Message to operate against
 * @param name the name of the boolean property as a String
 * @param value the boolean property value to set in the Message.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to  set Property due to
 *                         some internal JMS error.
 * @exception MessageNotWriteableException if properties are read-only
 */
int SMQ_API Message_setBooleanProperty(HOBJ msgObj, HOBJ name, jboolean value);

/**
 * Set a byte property value with the given name, into the Message.
 *
 * @param msgObj the Message to operate against
 * @param name the name of the byte property as a String
 * @param value the byte property value to set in the Message.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to  set Property due to
 *                         some internal JMS error.
 * @exception MessageNotWriteableException if properties are read-only
 */
int SMQ_API Message_setByteProperty(HOBJ msgObj, HOBJ name, jbyte value);

/**
 * Set a short property value with the given name, into the Message.
 *
 * @param msgObj the Message to operate against
 * @param name the name of the short property as a String
 * @param value the short property value to set in the Message.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to  set Property due to
 *                         some internal JMS error.
 * @exception MessageNotWriteableException if properties are read-only
 */
int SMQ_API Message_setShortProperty(HOBJ msgObj, HOBJ name, jshort value);

/**
 * Set a integer property value with the given name, into the Message.
 *
 * @param msgObj the Message to operate against
 * @param name the name of the integer property as a String
 * @param value the integer property value to set in the Message.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to  set Property due to
 *                         some internal JMS error.
 * @exception MessageNotWriteableException if properties are read-only
 */
int SMQ_API Message_setIntProperty(HOBJ msgObj, HOBJ name, jint value);

/**
 * Set a long property value with the given name, into the Message.
 *
 * @param msgObj the Message to operate against
 * @param name the name of the long property as a String
 * @param value the long property value to set in the Message.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to  set Property due to
 *                         some internal JMS error.
 * @exception MessageNotWriteableException if properties are read-only
 */
int SMQ_API Message_setLongProperty(HOBJ msgObj, HOBJ name, jlong value);

/**
 * Set a float property value with the given name, into the Message.
 *
 * @param msgObj the Message to operate against
 * @param name the name of the float property as a String
 * @param value the float property value to set in the Message.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to  set Property due to
 *                         some internal JMS error.
 * @exception MessageNotWriteableException if properties are read-only
 */
int SMQ_API Message_setFloatProperty(HOBJ msgObj, HOBJ name, jfloat value);

/**
 * Set a double property value with the given name, into the Message.
 *
 * @param msgObj the Message to operate against
 * @param name the name of the double property as a String
 * @param value the double property value to set in the Message.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to  set Property due to
 *                         some internal JMS error.
 * @exception MessageNotWriteableException if properties are read-only
 */
int SMQ_API Message_setDoubleProperty(HOBJ msgObj, HOBJ name, jdouble value);

/**
 * Set a String property value with the given name, into the Message.
 *
 * @param msgObj the Message to operate against
 * @param name the name of the String property as a String
 * @param value the String property value to set in the Message.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to  set Property due to
 *                         some internal JMS error.
 * @exception MessageNotWriteableException if properties are read-only
 */
int SMQ_API Message_setStringProperty(HOBJ msgObj, HOBJ name, HOBJ value);

/**
 * Set an Object property value with the given name, into the Message.
 *
 * @param msgObj the Message to operate against
 * @param name the name of the Object property as a String
 * @param value the Object property value to set in the Message.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to  set Property due to
 *                         some internal JMS error.
 * @exception MessageNotWriteableException if properties are read-only
 */
int SMQ_API Message_setObjectProperty(HOBJ msgObj, HOBJ name, HOBJ value);

/**
 * Acknowledge this or all previous messages received by the session.
 * 
 * <p>All JMS messages support the acknowledge() method for use when a client has specified
 * that a JMS session's messages are to be explicitly acknowledged. A client requests explicit
 * acknowledgement by creating a session with either the standard JMS CLIENT_ACKKNOWLEDGE mode,
 * or the non-JMS acknowledgement mode SINGLE_MESSAGE_ACKNOWLEDGE.
 *
 * <p>In the standard JMS CLIENT_ACKNOWLEDGE mode, all messages previously received for the
 * session are acknowledged. If the session has been created with SINGLE_MESSAGE_ACKNOWLEDGE,
 * only the current message is acknowledged.
 *
 * <p>JMS defaults to implicit message acknowledgement, AUTO_ACKNOWLEDGE. In this mode,
 * calls to acknowledge() are ignored, as JMS automatically acknowledges messages on behalf
 * of the client.
 *
 * <p>Messages that have been received but not acknowledged may be
 * redelivered to the consumer.
 *
 * @param msgObj the Message to operate against
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to acknowledge due to some
 *                         internal JMS error.
 */
int SMQ_API Message_acknowledge(HOBJ msgObj);

/**
 * Clear out the message body. All other parts of the message are left
 * untouched.
 *
 * @param msgObj the Message to operate against
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to due to some internal JMS error.
 */
int SMQ_API Message_clearBody(HOBJ msgObj);

/**
 * Get all message properties (Progress-specific message interface extensions) in a 
 *  Hashtable object. Note that both the name and the value of a Hashtable entry
 *  will be objects. E.g. a boolean property of "true" w/ the name "isPublic" will 
 *  be represented in the Hashtable as an entry w/ the String object "isPublic" as 
 *  the key and the Boolean object w/ value "true" as the value. Use the appropriate 
 *  API functions to access the values of the objectified values.
 *
 * @param msgObj the Message whose properties to retrieve
 * @param pret a Hashtable object containing the message properties as 
 *        key-value pairs (all objects)
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API Message_getProperties(HOBJ msgObj, HOBJ *pret);

/**
 * Create a new message.
 *
 * @param pret the newly created Message.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to due to some internal JMS error.
 */
int SMQ_API Message_create(HOBJ *pret);

/** Get the size, in bytes, of the message body.
  *
  * @param thisObj the Message to operate against
  * @param pret the number of Text in the message body 
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  *
  */
int SMQ_API Message_getBodySize(HOBJ msgObj, jint *pret);

/*
//---------------------------------
// MessageConsumer
//---------------------------------
*/
/**
 * Decrements the reference count of the MessageConsumer object.
 *
 * @param msgCnsmr the MessageConsumer object to operate against.
 * @param pret will contain the reference count after decrementing.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define MessageConsumer_release(msgCnsmr, pret)               Object_release(msgCnsmr, pret)

/**
 * Get the int corresponding to the type of a given object.
 *
 * @param thisObj the object to operate against.
 * @param pret will contain the int corresponding to the type.
 */
#define MessageConsumer_getType(thisObj, pret)                Object_getType(thisObj, pret)

/**
 * Determines whether the given object is an instance of the given type.
 *
 * @param thisObj the object to check.
 * @param classtype the type to check against.
 * @param pret non-zero if the object is an instance of the type.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define MessageConsumer_instanceof(thisObj, classtype, pret) Object_instanceof(thisObj, classtype, pret)

/**
 * Determines whether the two MessageConsumer objects are the same.
 *
 * @param msgCnsmr the first MessageConsumer to compare.
 * @param hobj2 the second MessageConsumer to compare.
 * @param pret true if the MessageConsumer objects have identical content, false otherwise.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define MessageConsumer_equals(msgCnsmr, hobj2, pret)         Object_equals(msgCnsmr, hobj2, pret)

/**
 * Get the String representation for the given MessageConsumer.
 *
 * @param msgCnsmr the MessageConsumer object to operate against.
 * @param pret will contain the String that represents the MessageConsumer.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define MessageConsumer_toString(msgCnsmr, pret)              Object_toString(msgCnsmr, pret)

/**
 * Returns the int corresponding to the MessageConsumer type.
 *
 * @return the int corresponding to the MessageConsumer type
 */
int SMQ_API MessageConsumer_type();

/** Get this message consumer's message selector expression.
  *
  * @param msgCnsmr the MessageConsumer to operate against
  * @param pSelector this message consumer's message selector
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  */
int SMQ_API MessageConsumer_getMessageSelector(HOBJ msgCnsmr, HOBJ *pSelector);

/** Get the message consumer's "message listener" (i.e. callback function).
  *
  * @param msgCnsmr the MessageConsumer to operate against
  * @param pListener The listener for the message consumer, or null if there isn't
  *        one set. The proper signature for the message listener function is: 
  *        <code>typedef void (*pfnCMessageListener)(HOBJ msg);</code>
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  *
  * @see MessageConsumer_setMessageListener()
  */
int SMQ_API MessageConsumer_getMessageListener(HOBJ msgCnsmr, pfnCMessageListener *plistener);

/** Set the message consumer's "message listener" (i.e. callback function).
  *
  * <p>When a message arrives for this message consumer, the callback function is 
  * called and passed an HOBJ for a Message object. More specifically, the Message 
  * may be a TextMessage or a BytesMessage.
  *
  * <p>The signature for an appropriate callback function is: 
  * <code>typedef void (*pfnCMessageListener)(HOBJ msg);</code>
  *
  * @param msgCnsmr the MessageConsumer to register the message listener for
  * @param messageListener the messages are delivered to this listener
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  *
  * @see MessageConsumer_getMessageListener()
  */
int SMQ_API MessageConsumer_setMessageListener(HOBJ msgCnsmr, pfnCMessageListener listener);

/** Receive the next message produced for this message consumer.
  *
  * <P>This call blocks indefinitely until a message is produced.
  *
  * <P>If this receive is done within a transaction, the message
  * remains on the consumer until the transaction commits.
  *
  * @param msgCnsmr the MessageConsumer to operate against
  * @param pMsg the next message produced for this message consumer, 
  *         or null, if the connection closes while waiting
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  */
int SMQ_API MessageConsumer_receive(HOBJ msgCnsmr, HOBJ *pMsg);

/** Receive the next message that arrives within the specified
  * timeout interval.
  *
  * <P>This call blocks until either a message arrives or the
  * timeout expires.
  *
  * @param msgCnsmr the MessageConsumer to operate against
  * @param timeout the timeout value (in milliseconds)
  * @param pMsg the next message produced for this message consumer, 
  *         or null, if the connection closes while waiting, or if
  *         the wait times out.
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  */
int SMQ_API MessageConsumer_receive2(HOBJ msgCnsmr, jlong timeout, HOBJ *pMsg);

/** Receive the next message if one is immediately available.
  *
  * @param msgCnsmr the MessageConsumer to operate against
  * @return the next message produced for this message consumer, or
  * null if one is not available.
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  */
int SMQ_API MessageConsumer_receiveNoWait(HOBJ msgCnsmr, HOBJ *pret);

/*
//---------------------------------
// MessageProducer
//---------------------------------
*/
/**
 * Decrements the reference count of the MessageProducer object.
 *
 * @param msgPrdcr the MessageProducer object to operate against.
 * @param pret will contain the reference count after decrementing.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define MessageProducer_release(msgPrdcr, pret)               Object_release(msgPrdcr, pret)

/**
 * Get the int corresponding to the type of a given object.
 *
 * @param thisObj the object to operate against.
 * @param pret will contain the int corresponding to the type.
 */
#define MessageProducer_getType(thisObj, pret)                Object_getType(thisObj, pret)

/**
 * Determines whether the given object is an instance of the given type.
 *
 * @param thisObj the object to check.
 * @param classtype the type to check against.
 * @param pret non-zero if the object is an instance of the type.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define MessageProducer_instanceof(thisObj, classtype, pret) Object_instanceof(thisObj, classtype, pret)

/**
 * Determines whether the two MessageProducer objects are the same.
 *
 * @param msgPrdcr the first MessageProducer to compare.
 * @param hobj2 the second MessageProducer to compare.
 * @param pret true if the MessageProducer objects have identical content, false otherwise.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define MessageProducer_equals(msgPrdcr, hobj2, pret)         Object_equals(msgPrdcr, hobj2, pret)

/**
 * Get the String representation for the given MessageProducer.
 *
 * @param msgPrdcr the MessageProducer object to operate against.
 * @param pret will contain the String that represents the MessageProducer.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define MessageProducer_toString(msgPrdcr, pret)              Object_toString(msgPrdcr, pret)

/**
 * Returns the int corresponding to the MessageProducer type.
 *
 * @return the int corresponding to the MessageProducer type
 */
int SMQ_API MessageProducer_type();

/** Get an indication of whether message IDs are disabled.
  *  
  * @param msgPrdcr the MessageProducer to operate against
  * @param pDisabled an indication of whether message IDs are disabled.
  *  
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  */ 
int SMQ_API MessageProducer_getDisableMessageID(HOBJ msgPrdcr, jboolean *pdisabled);

/** Set whether message IDs are disabled.
  *  
  * <P>Since message ID's take some effort to create and increase a
  * message's size, SonicMQ can optimize message
  * overhead if they are given a hint that message ID is not used by
  * an application. SonicMQ message Producers provide a hint to disable
  * message ID. When a client sets a Producer to disable message ID
  * they are saying that they do not depend on the value of message
  * ID for the messages it produces. These messages must either have
  * message ID set to null or, if the hint is ignored, messageID must
  * be set to its normal unique value.
  *
  * <P>Message IDs are enabled by default.
  *
  * @param msgPrdcr the MessageProducer to operate against
  * @param value indicates if message IDs are disabled.
  *  
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  */ 
int SMQ_API MessageProducer_setDisableMessageID(HOBJ msgPrdcr, jboolean value);

/** Get an indication of whether message timestamps are disabled.
  *  
  * @param msgPrdcr the MessageProducer to operate against
  * @param pDisabled an indication of whether message IDs are disabled.
  *  
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  */
int SMQ_API MessageProducer_getDisableMessageTimestamp(HOBJ msgPrdcr, jboolean *pDisabled);

/** Set whether message timestamps are disabled.
  *  
  * <P>Since timestamps take some effort to create and increase a 
  * message's size, SonicMQ can optimize message 
  * overhead if they are given a hint that timestamp is not used by an 
  * application. SonicMQ message Producers provide a hint to disable 
  * timestamps. When a client sets a producer to disable timestamps 
  * they are saying that they do not depend on the value of timestamp 
  * for the messages it produces. These messages must either have 
  * timestamp set to null or, if the hint is ignored, timestamp must 
  * be set to its normal value.
  *  
  * <P>Message timestamps are enabled by default.
  *
  * @param msgPrdcr the MessageProducer to operate against
  * @param value indicates if message timestamps are disabled.
  *  
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  */ 
int SMQ_API MessageProducer_setDisableMessageTimestamp(HOBJ msgPrdcr, jboolean value);

/** Get the producer's default delivery mode.
  *  
  * @param msgPrdcr the MessageProducer to operate against
  * @param pDelMode the message delivery mode for this message producer.
  *  
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  *
  * @see MessageProducer_setDeliveryMode()
  */ 
int SMQ_API MessageProducer_getDeliveryMode(HOBJ msgPrdcr, jint *pDelMode);

/** Set the producer's default delivery mode.
  *  
  * <P>Delivery mode is set to NON_PERSISTENT by default.
  *
  * @param msgPrdcr the MessageProducer to operate against
  * @param deliveryMode the message delivery mode for this message
  * producer.
  *  
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  *
  * @see MessageProducer_getDeliveryMode()
  */ 
int SMQ_API MessageProducer_setDeliveryMode(HOBJ msgPrdcr, jint deliveryMode);

/** Get the producer's default priority.
  *  
  * @param msgPrdcr the MessageProducer to operate against
  * @param pPriority the message priority for this message producer.
  *  
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  *
  * @see MessageProducer_setPriority()
  */ 
int SMQ_API MessageProducer_getPriority(HOBJ msgPrdcr, jint *pPriority);

/** Set the producer's default priority.
  *  
  * <P>Priority is set to 4, by default.
  *
  * @param msgPrdcr the MessageProducer to operate against
  * @param value the message priority for this message producer.
  *  
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  *
  * @see MessageProducer_getPriority()
  */ 
int SMQ_API MessageProducer_setPriority(HOBJ msgPrdcr, jint value);

/** Get the default length of time in milliseconds from its dispatch time
  * that a produced message should be retained by the message system.
  *
  * @param msgPrdcr the MessageProducer to operate against
  * @param pTTL the message time to live in milliseconds; zero is unlimited
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  *
  * @see MessageProducer_setTimeToLive()
  */ 
int SMQ_API MessageProducer_getTimeToLive(HOBJ msgPrdcr, jlong *pTTL);

/** Set the default length of time in milliseconds from its dispatch time
  * that a produced message should be retained by the message system.
  *
  * <P>Time to live is set to zero by default.
  *
  * @param msgPrdcr the MessageProducer to operate against
  * @param timeToLive the message time to live in milliseconds; zero is
  * unlimited
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  *
  * @see MessageProducer_getTimeToLive()
  */
int SMQ_API MessageProducer_setTimeToLive(HOBJ msgPrdcr, jlong value);

/** Since SonicMQ allocates some resources on behalf of a
  * MessageProducer, clients should close them when they
  * are not needed.
  *  
  * @param msgPrdcr the MessageProducer to operate against
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  */ 
int SMQ_API MessageProducer_close(HOBJ msgPrdcr);

/*
//---------------------------------
// Queue
//---------------------------------
*/
/**
 * Decrements the reference count of the Queue object.
 *
 * @param qObj the Queue object to operate against.
 * @param pret will contain the reference count after decrementing.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Queue_release(qObj, pret)               Object_release(qObj, pret)

/**
 * Get the int corresponding to the type of a given object.
 *
 * @param thisObj the object to operate against.
 * @param pret will contain the int corresponding to the type.
 */
#define Queue_getType(thisObj, pret)            Object_getType(thisObj, pret)

/**
 * Determines whether the given object is an instance of the given type.
 *
 * @param thisObj the object to check.
 * @param classtype the type to check against.
 * @param pret non-zero if the object is an instance of the type.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Queue_instanceof(thisObj, classtype, pret) Object_instanceof(thisObj, classtype, pret)

/**
 * Determines whether the two Queue objects are the same.
 *
 * @param qObj the first Queue to compare.
 * @param hobj2 the second Queue to compare.
 * @param pret true if the Queue objects have identical content, false otherwise.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Queue_equals(qObj, hobj2, pret)         Object_equals(qObj, hobj2, pret)

/**
 * Get the String representation for the given Queue.
 *
 * @param qObj the Queue object to operate against.
 * @param pret will contain the String that represents the Queue.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Queue_toString(qObj, pret)              Object_toString(qObj, pret)

/**
 * Returns the int corresponding to the Queue type.
 *
 * @return the int corresponding to the Queue type
 */
int SMQ_API Queue_type();

/** Get the name of this queue.
  *  
  * <P>Clients that depend upon the name are not portable.
  *  
  * @param qObj the Queue object to operate against
  * @param pNameStr the queue name returned as a String
  *  
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  */ 
int SMQ_API Queue_getQueueName(HOBJ qObj, HOBJ *pNameStr);

/*
//---------------------------------
// TemporaryQueue
//---------------------------------
*/
/**
 * Decrements the reference count of the TemporaryQueue object.
 *
 * @param tmpQ the TemporaryQueue object to operate against.
 * @param pret will contain the reference count after decrementing.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define TemporaryQueue_release(tmpQ, pret)               Object_release(tmpQ, pret)

/**
 * Get the int corresponding to the type of a given object.
 *
 * @param thisObj the object to operate against.
 * @param pret will contain the int corresponding to the type.
 */
#define TemporaryQueue_getType(thisObj, pret)               Object_getType(thisObj, pret)

/**
 * Determines whether the given object is an instance of the given type.
 *
 * @param thisObj the object to check.
 * @param classtype the type to check against.
 * @param pret non-zero if the object is an instance of the type.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define TemporaryQueue_instanceof(thisObj, classtype, pret) Object_instanceof(thisObj, classtype, pret)

/**
 * Determines whether the two TemporaryQueue objects are the same.
 *
 * @param tmpQ the first TemporaryQueue to compare.
 * @param hobj2 the second TemporaryQueue to compare.
 * @param pret true if the TemporaryQueue objects have identical content, false otherwise.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define TemporaryQueue_equals(tmpQ, hobj2, pret)         Object_equals(tmpQ, hobj2, pret)

/**
 * Get the String representation for the given TemporaryQueue.
 *
 * @param tmpQ the TemporaryQueue object to operate against.
 * @param pret will contain the String that represents the TemporaryQueue.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define TemporaryQueue_toString(tmpQ, pret)              Object_toString(tmpQ, pret)

/** Get the name of this temporary queue.
  *  
  * <P>Clients that depend upon the name are not portable.
  *  
  * @param qObj the TemporaryQueue object to operate against
  * @param pNameStr the queue name returned as a String
  *  
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  */ 
#define TemporaryQueue_getQueueName(tmpQ, pret)          Queue_getQueueName(tmpQ, pret)

/**
 * Returns the int corresponding to the TemporaryQueue type.
 *
 * @return the int corresponding to the TemporaryQueue type
 */
int SMQ_API TemporaryQueue_type();

/** Delete this temporary queue. If there are still senders
 * or receivers using it, an error will be returned.
 *  
 * @param tmpQ the TemporaryQueue object to operate against
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API TemporaryQueue_delete(HOBJ tmpQ);

/*
//---------------------------------
// QueueBrowser
//---------------------------------
*/
/**
 * Decrements the reference count of the QueueBrowser object.
 *
 * @param qBrowser the QueueBrowser object to operate against.
 * @param pret will contain the reference count after decrementing.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define QueueBrowser_release(qBrowser, pret)               Object_release(qBrowser, pret)

/**
 * Get the int corresponding to the type of a given object.
 *
 * @param thisObj the object to operate against.
 * @param pret will contain the int corresponding to the type.
 */
#define QueueBrowser_getType(thisObj, pret)               Object_getType(thisObj, pret)

/**
 * Determines whether the given object is an instance of the given type.
 *
 * @param thisObj the object to check.
 * @param classtype the type to check against.
 * @param pret non-zero if the object is an instance of the type.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define QueueBrowser_instanceof(thisObj, classtype, pret) Object_instanceof(thisObj, classtype, pret)

/**
 * Determines whether the two QueueBrowser objects are the same.
 *
 * @param qBrowser the first QueueBrowser to compare.
 * @param hobj2 the second QueueBrowser to compare.
 * @param pret true if the QueueBrowser objects have identical content, false otherwise.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define QueueBrowser_equals(qBrowser, hobj2, pret)         Object_equals(qBrowser, hobj2, pret)

/**
 * Get the String representation for the given QueueBrowser.
 *
 * @param qBrowser the QueueBrowser object to operate against.
 * @param pret will contain the String that represents the QueueBrowser.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define QueueBrowser_toString(qBrowser, pret)              Object_toString(qBrowser, pret)

/**
 * Returns the int corresponding to the QueueBrowser type.
 *
 * @return the int corresponding to the QueueBrowser type
 */
int SMQ_API QueueBrowser_type();

/** Since SonicMQ allocates some resources on behalf of a
 * MessageConsumer inside the SonicMQ client run-time, clients should close them when they
 * are not needed.
 *
 * @param qBrowser the QueueBrowser to operate against
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API QueueBrowser_close(HOBJ qBrowser);

/** Get the queue associated with this queue Browser.
 *
 * @param qBrowser the QueueBrowser to operate against
 * @param pQueue the Queue that the QueueBrowser is associated with
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API QueueBrowser_getQueue(HOBJ qBrowser, HOBJ *pQueue);

/** Get this queue browser's message selector expression.
 *
 * @param qBrowser the QueueBrowser to operate against
 * @param pMsgSelector this queue browser's message selector
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API QueueBrowser_getMessageSelector(HOBJ qBrowser, HOBJ *pMsgSelector);

/** Get an enumeration for browsing the current queue
 * messages in the order they would be received.
 *
 * @param qBrowser the QueueBrowser to operate against
 * @param pEnum an enumeration for browsing the messages
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API QueueBrowser_getEnumeration(HOBJ qBrowser, HOBJ *pEnum);

/*
//---------------------------------
// QueueConnection
//---------------------------------
*/
/**
 * Decrements the reference count of the QueueConnection object.
 *
 * @param thisObj the QueueConnection object to operate against.
 * @param pret will contain the reference count after decrementing.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define QueueConnection_release(thisObj, pret)                       Object_release(thisObj, pret)

/**
 * Get the int corresponding to the type of a given object.
 *
 * @param thisObj the object to operate against.
 * @param pret will contain the int corresponding to the type.
 */
#define QueueConnection_getType(thisObj, pret)                       Object_getType(thisObj, pret)

/**
 * Determines whether the given object is an instance of the given type.
 *
 * @param thisObj the object to check.
 * @param classtype the type to check against.
 * @param pret non-zero if the object is an instance of the type.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define QueueConnection_instanceof(thisObj, classtype, pret)         Object_instanceof(thisObj, classtype, pret)

/**
 * Determines whether the two QueueConnection objects are the same.
 *
 * @param thisObj the first QueueConnection to compare.
 * @param hobj2 the second QueueConnection to compare.
 * @param pret true if the QueueConnection objects have identical content, false otherwise.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define QueueConnection_equals(thisObj, hobj2, pret)                 Object_equals(thisObj, hobj2, pret)

/**
 * Get the String representation for the given QueueConnection.
 *
 * @param thisObj the QueueConnection object to operate against.
 * @param pret will contain the String that represents the QueueConnection.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define QueueConnection_toString(thisObj, pret)                      Object_toString(thisObj, pret)

/**
 * Closes the QueueConnection. There is no need to close the sessions, producers, and consumers 
 * of a closed connection.
 *
 * @param thisObj the QueueConnection object to close.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define QueueConnection_close(thisObj)                               Connection_close(thisObj) 

/**
 * Start (or restart) a QueueConnection's delivery of incoming messages. Starting a started 
 * session is ignored.
 *
 * @param thisObj the QueueConnection object to start.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define QueueConnection_start(thisObj)                               Connection_start(thisObj) 

/**
 * Used to temporarily stop a QueueConnection's delivery of incoming messages. It can be 
 * restarted using its start method. When stopped, delivery to all the QueueConnection's 
 * message consumers is inhibited: synchronous receive's block and messages are not 
 * delivered to message listeners. 
 *
 * <p>This call blocks until receives and/or message listeners in progress have completed. 
 *
 * <p>Stopping a Session has no affect on its ability to send messages. Stopping a stopped 
 * session is ignored. 
 *
 * <p>A stop method call must not return until delivery of messages has paused. This means 
 * a client can rely on the fact that none of its message listeners will be called and 
 * all threads of control waiting for receive to return will not return with a message 
 * until the connection is restarted. The receive timers for a stopped connection continue 
 * to advance so receives may time out while the connection is stopped. 
 *
 * <p>If MessageListeners are running when stop is invoked, stop must wait until all of them 
 * have returned before it may return. While these MessageListeners are completing, they 
 * must have full services of the connection available to them.
 *
 * @param thisObj the QueueConnection object to stop.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define QueueConnection_stop(thisObj)                                Connection_stop(thisObj) 

/**
 * Retrieve the URL of the broker for this QueueConnection.
 *
 * @param thisObj the QueueConnection object to operate against.
 * @param pBrokerURL will contain the broker URL.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define QueueConnection_getBrokerURL(thisObj, pBrokerURL)            Connection_getBrokerURL(thisObj, pBrokerURL) 

/** Get the client identifier for this QueueConnection.
  *
  * @param thisObj the QueueConnection object to operate against
  * @param pClientID the unique client identifier.
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  */
#define QueueConnection_getClientID(thisObj, pClientID)              Connection_getClientID(thisObj, pClientID) 

/** Set the client identifier for this queue connection.
  *
  * <P>The preferred way to assign a Client's client identifier is for
  * it to be configured in a client-specific ConnectionFactory and
  * transparently assigned to the Connection it creates. Alternatively,
  * a client can set a Connections's client identifier using a
  * specific value.
  *
  * <P>The purpose of client identifier is to associate a session and
  * its objects with a state maintained on behalf of the client by a
  * SonicMQ broker. The only such state identified by SonicMQ is that required
  * to support durable subscriptions
  *
  * @param thisObj the QueueConnection object to operate against
  * @param clientID the unique client identifier
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Likely causes of errors:
  *           InvalidClientIDException if SonicMQ client specifies an invalid or duplicate client id.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  */
#define QueueConnection_setClientID(thisObj, clientID)               Connection_setClientID(thisObj, clientID) 

/**
 * Retrieve the connection ID for this QueueConnection.
 *
 * @param thisObj the QueueConnection object to operate against.
 * @param pConnID will contain the connection ID.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define QueueConnection_getConnectionID(thisObj, pConnID)            Connection_getConnectionID(thisObj, pConnID) 

/**
 * Get the ExceptionListener for this QueueConnection.
 *
 * @param thisObj the QueueConnection object to operate against
 * @param p_pfnListener The exception listener (i.e. callback function) for this 
 *        QueueConnection. The signature is given by 
 *        <code>typedef void (*pfnCExceptionListener)(HOBJ hobjException);</code>
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define QueueConnection_getExceptionListener(thisObj, p_pfnListener) Connection_getExceptionListener(thisObj, p_pfnListener) 

/** Set up an exception listener (i.e. a callback function) for this queue connection.
  *
  * <P>If SonicMQ detects a serious problem with a connection it
  * will inform the connection's "exception listener" if one has been
  * registered. It does this by invoking the callback function set via 
  * <code>Connection_setExceptionListener()</code and passing it a JMSException 
  * object describing the problem.
  *
  * <P>This allows a client to be asynchronously notified of a problem.
  * Some connections only consume messages so they would have no other
  * way to learn their connection has failed.
  *
  * <P>A Connection serializes execution of its exception listener.
  *
  * <P>A SonicMQ broker attempts to resolve connection problems
  * itself prior to notifying the client of them.
  *
  * @param connObj the QueueConnection object to operate against
  * @param listener the callback function to set as the exception listener--
  *        the signature is <code>typedef void (*pfnCExceptionListener)(HOBJ hobjException);</code>
  *        where hobjException is a JMSException object handle
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Likely causes of errors:
  *           InvalidClientIDException if SonicMQ client specifies an invalid or duplicate client id.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  */
#define QueueConnection_setExceptionListener(thisObj, listener)      Connection_setExceptionListener(thisObj, listener) 



/** Set a C-signature Connection State Change listener for this connection.
 */
#define QueueConnection_setConnectionStateChangeListener(thisObj, listener, user) Connection_setConnectionStateChangeListener(thisObj, listener, user)


/** Set a C-signature Connection State Change listener for this connection.
 */
#define QueueConnection_getConnectionStateChangeListener(thisObj, p_pfnListener) Connection_getConnectionStateChangeListener(thisObj, p_pfnListener)



/** Get the meta data for this queue connection.
  *
  * @param thisObj the QueueConnection object to operate against
  * @param pMetaData the ConnectionMetadata object returned
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  *
  * @see ConnectionMetaData_* functions
  */
#define QueueConnection_getMetaData(thisObj, pMetaData)              Connection_getMetaData(thisObj, pMetaData) 

/** Enable/disable sending active pings on this connection. This is a non-JMS public method.
  *
  * @param thisObj the QueueConnection object to operate against
  * @param interval indicates the interval in seconds for sending a ping. Setting 
  *        interval to 0 or any negative value effectively disables the ping. 
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Likely causes of errors:
  *           InvalidClientIDException if SonicMQ client specifies an invalid or duplicate client id.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  */
#define QueueConnection_setPingInterval(thisObj, interval)           Connection_setPingInterval(thisObj, interval) 

/**
 * Returns true if the connection is fault tolerant
 * <P>For a connection to be fault-tolerant, fault-tolerant must be set in the
 * ConnectionFactory, and the broker must support(be licensed for) fault-tolerance.
 *
 * @param thisObj the Connection object to operate against
 * @param pret Returns the true if the connection is fault tolerant.
 * 
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @see progress.message.jclient.ConnectionFactory#setFaultTolerant
 */
#define QueueConnection_isFaultTolerant(thisObj, pret)             Connection_isFaultTolerant(thisObj, pret) 

/**
 * Get connection state, one of ACTIVE, RECONNECTING, FAILED or CLOSED.
 * <P>
 * A non fault-tolerant connection will never see a RECONNECTING value.
 * This method may be called after the connection is closed.
 * 
 * @param thisObj the Connection object to operate against
 * @param pret Returns the connection state.
 * 
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @see progress.message.jclient.Constants#ACTIVE
 * @see progress.message.jclient.Constants#RECONNECTING
 * @see progress.message.jclient.Constants#FAILED
 * @see progress.message.jclient.Constants#CLOSED
 * @see progress.message.jclient.ConnectionStateChangeListener
 * @see setConnectionStateChangeListener(ConnectionStateChangeListener)
 * @see getConnectionStateChangeListener()
 */
#define QueueConnection_getConnectionState(thisObj, pret)          Connection_getConnectionState(thisObj, pret) 


/** Get the username for this connection.
  *
  * @param thisObj the QueueConnection object to operate against
  * @param pUserName returns a String object containing the username.
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  *
  * @see QueueConnectionMetaData_* functions
  */
#define QueueConnection_getUsername(thisObj, pUserName)              Connection_getUsername(thisObj, pUserName) 

/**
 * Returns the int corresponding to the QueueConnection type.
 *
 * @return the int corresponding to the QueueConnection type
 */
int SMQ_API QueueConnection_type();

/** Construct a QueueConnection to message broker.
 *
 * @param brokerURL the URL of the message broker in the for [protocol://]hostname[:port]
 * @param connectID the client id used for connection.
 * @param username the name of client's user account.
 * @param password the password for client's user account
 * @param pQConn the newly created QueueConnection
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API QueueConnection_create(HOBJ brokerURL, HOBJ connectID, HOBJ username, HOBJ password, HOBJ *pQConn);

/** Create a QueueSession associated with the queue connection.
 *  
 * @param qConn the QueueConnection to operate against 
 * @param transacted if true, the session is transacted.
 * @param acknowledgeMode indicates whether the consumer or the
 * client will acknowledge any messages it receives. This parameter
 * will be ignored if the session is transacted.
 * @param pQSession the newly created queue session.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API QueueConnection_createQueueSession(HOBJ qConn, jboolean transacted, jint acknowledgeMode, HOBJ *pQSession);

/*
//---------------------------------
// QueueConnectionFactory
//---------------------------------
*/
/**
 * Decrements the reference count of the QueueConnectionFactory object.
 *
 * @param qConnectionFactory the QueueConnectionFactory object to operate against.
 * @param pret will contain the reference count after decrementing.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define QueueConnectionFactory_release(qConnectionFactory, pret)                Object_release(qConnectionFactory, pret)

/**
 * Get the int corresponding to the type of a given object.
 *
 * @param thisObj the object to operate against.
 * @param pret will contain the int corresponding to the type.
 */
#define QueueConnectionFactory_getType(thisObj, pret)                Object_getType(thisObj, pret)

/**
 * Determines whether the given object is an instance of the given type.
 *
 * @param thisObj the object to check.
 * @param classtype the type to check against.
 * @param pret non-zero if the object is an instance of the type.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define QueueConnectionFactory_instanceof(thisObj, classtype, pret)  Object_instanceof(thisObj, classtype, pret)

/**
 * Determines whether the two QueueConnectionFactory objects are the same.
 *
 * @param qConnectionFactory the first QueueConnectionFactory to compare.
 * @param hobj2 the second QueueConnectionFactory to compare.
 * @param pret true if the QueueConnectionFactory objects have identical content, false otherwise.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define QueueConnectionFactory_equals(qConnectionFactory, hobj2, pret)          Object_equals(qConnectionFactory, hobj2, pret)

/**
 * Get the String representation for the given QueueConnectionFactory.
 *
 * @param qConnectionFactory the QueueConnectionFactory object to operate against.
 * @param pret will contain the String that represents the QueueConnectionFactory.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define QueueConnectionFactory_toString(qConnectionFactory, pret)               Object_toString(qConnectionFactory, pret)

/**
 * Create a queue connection with default user identity.
 *
 * @param thisObj the QueueConnectionFactory object to operate against.
 * @param pret a newly created QueueConnection.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS Provider fails to create a Queue Connection
 *                         due to some internal error.
 * @exception JMSSecurityException  if client authentication fails due to
 *                         invalid user name or password.
 */
int SMQ_API QueueConnectionFactory_createQueueConnection(HOBJ thisObj, HOBJ *pret);

/** C interface to register the LoginSPI with the queue connection factory.
 *  When used with 4.0 brokers only clear text passwords are supported.  Users should
 *  use the setPassword method to set the clear text password.
 *  When used with 5.0 brokers only both clear text and encrypted (byte array) 
 *  passwords are supported.  Users should use the setPassword method to set the clear 
 *  text password and/or use setTransformedPassword to set the encrypted byte array
 *  password.
 *
 * @param thisObj the QueueConnectionFactory object to operate against.
 * @param map the map that implements the necessary login SPI routines
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API QueueConnectionFactory_setLoginSPI(HOBJ thisObj, ILoginSPICMap *cmap);

/**
 * Create a queue connection with specified user identity.
 *
 * @param thisObj the QueueConnectionFactory object to operate against.
 * @param username the caller's user name
 * @param password the caller's password
 * @param pret a newly created QueueConnection.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS Provider fails to create a Queue Connection
 *                         due to some internal error.
 * @exception JMSSecurityException  if client authentication fails due to
 *                         invalid user name or password.
 */
int SMQ_API QueueConnectionFactory_createQueueConnection2(HOBJ thisObj, HOBJ username, HOBJ password, HOBJ *pret);

/**
 * Retrieve the broker hostname.
 *
 * @param thisObj the QueueConnectionFactory object to operate against.
 * @param pret will contain a String w/ the broker hostname.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API QueueConnectionFactory_getBrokerHostName(HOBJ thisObj, HOBJ *pret);

/**
 * Retrieve the broker port number.
 *
 * @param thisObj the QueueConnectionFactory object to operate against.
 * @param pret will contain the broker port number.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API QueueConnectionFactory_getBrokerPort(HOBJ thisObj, jint *pret);

/**
 * Retrieve the broker protocol.
 *
 * @param thisObj the QueueConnectionFactory object to operate against.
 * @param pret a String containing the broker protocol.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API QueueConnectionFactory_getBrokerProtocol(HOBJ thisObj, HOBJ *pret);

/**
 * Retrieve the broker URL.
 *
 * @param thisObj the QueueConnectionFactory object to operate against.
 * @param pret the String containing the broker URL.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API QueueConnectionFactory_getBrokerURL(HOBJ thisObj, HOBJ *pret);

/**
 * Retrieve the client ID.
 *
 * @param thisObj the QueueConnectionFactory object to operate against.
 * @param pret the String containing the client ID.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API QueueConnectionFactory_getClientID(HOBJ thisObj, HOBJ *pret);

/**
 * Retrieve the connect ID.
 *
 * @param thisObj the QueueConnectionFactory object to operate against.
 * @param pret the String containing the connect ID.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API QueueConnectionFactory_getConnectID(HOBJ thisObj, HOBJ *pret);

/**
 * Get list of brokers to try to connect to.
 *
 * @param thisObj the QueueConnectionFactory object to operate against.
 * @param pret a String containing a comma-separated list of broker URLs..
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API QueueConnectionFactory_getConnectionURLs(HOBJ thisObj, HOBJ *pret);

/**
 * Retrieve the default password.
 *
 * @param thisObj the QueueConnectionFactory object to operate against.
 * @param pret a String containing the default password.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API QueueConnectionFactory_getDefaultPassword(HOBJ thisObj, HOBJ *pret);

/**
 * Retrieve the default username.
 *
 * @param thisObj the QueueConnectionFactory object to operate against.
 * @param pret a String containing the default username.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API QueueConnectionFactory_getDefaultUser(HOBJ thisObj, HOBJ *pret);

/**
 * Determines whether client-side load balancing is enabled.
 *
 * @param thisObj the QueueConnectionFactory object to operate against.
 * @param pret if true, indicates that the client is willing to have a connect request
 *        redirected to another broker within a SonicMQ cluster.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @see    QueueConnectionFactory_setLoadBalancing()
 */
int SMQ_API QueueConnectionFactory_getLoadBalancing(HOBJ thisObj, jboolean *pret);

/**
 * Determines whether the option to start with the first or a random element of the broker list
 * has been selected.
 *
 * @param thisObj the QueueConnectionFactory object to operate against.
 * @param pret If true, connect attempts start with the first broker in the list.
 *        If false, connect attempts start with a random element in the broker list.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @see QueueConnectionFactory_setSequential()
 * @see QueueConnectionFactory_setConnectionURLs()
 * @see QueueConnectionFactory_getConnectionURLs()
 */
int SMQ_API QueueConnectionFactory_getSequential(HOBJ thisObj, jboolean *pret);

/**
 * Determines whether the option to disable Nagle's algorithm for TCP connections
 * has been selected.
 *
 * @param thisObj the QueueConnectionFactory object to operate against.
 * @param pret If true, created TCP connections will have Nagle's algorithm disabled.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API QueueConnectionFactory_getTcpNoDelay(HOBJ thisObj, jboolean *pret);

/** 
 * Gets the default batch size of all sessions created via connections
 * created from this connection factory. A session's batch size specifies
 * the amount of information to be batched on the client.  This is a
 * performance optimization utilized on transacted sessions.  The size
 * parameter is a measure of the message payloads only.
 *
 * @param thisObj the QueueConnectionFactory object to operate against.
 * @param pret Points to the number of bytes to batch, specified as a jint.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API QueueConnectionFactory_getDefaultTxnBatchSize(HOBJ thisObj, jint *pret);

/** 
 * Gets the time, in milliseconds, that C-Client will wait for a connection before aborting.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API QueueConnectionFactory_getTCPConnectionTimeout(HOBJ thisObj, jint *pret);

/** Get the administrative flow control monitoring interval.
 *
 * @param thisObj the QueueConnectionFactory object to operate against.
 * @param pinterval Pointer to the interval in seconds over which administrative monitoring 
 *                  of flow control will occur. A value of 0 (zero) indicates that no 
 *                  monitoring will occur.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @see    setMonitorInterval()
 */
int SMQ_API QueueConnectionFactory_getMonitorInterval(HOBJ thisObj, jint *pinterval);

/** Determines whether fault tolerant connection creation is enabled.
 *<P>
 * By default, connections are created non-fault tolerant(false). For a connection to
 * be created fault-tolerant, fault-tolerant must be set in the ConnectionFactory,
 * and, the broker must support(be licensed for) fault-tolerance.
 *
 * @param thisObj the QueueConnectionFactory object to operate against.
 *
 * @param  pfaulttolerant true indicates the client wishes to create fault-tolerant
 * connections
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 * 
 * @see    setFaultTolerant(Boolean)
 * @see    progress.message.jclient.Connection#isFaultTolerant()
 * @see    setFaultTolerantReconnectTimeout(Integer)
 * @see    getFaultTolerantReconnectTimeout()
 * @see    setInitialConnectTimeout(Integer)
 * @see    getInitialConnectTimeout()
 * @see    setClientTransactionBufferSize(Long)
 * @see    getClientTransactionBufferSize()
 */
int SMQ_API QueueConnectionFactory_getFaultTolerant(HOBJ thisObj, jboolean *pfaulttolerant);

/** Get fault tolerance reconnect timeout.
 *
 * @return maximum time in seconds to attempt reconnection of a failed
 *         fault-tolerant connection
 * @see    setFaultTolerantReconnectTimeout(Integer)
 * @see    setFaultTolerant(Boolean)
 * @see    getFaultTolerant()
 * @see    setInitialConnectTimeout(Integer)
 * @see    getInitialConnectTimeout()
 */
int SMQ_API QueueConnectionFactory_getFaultTolerantReconnectTimeout(HOBJ thisObj, jint *pseconds);

/** Get initial connect timeout for a fault-tolerant connection.
 *
 * @return maximum time in seconds to attempt initial connection of a fault-tolerant connection
 *         fault-tolerant connection
 * @see    setInitialConnectTimeout(Integer)
 * @see    setFaultTolerant(Boolean)
 * @see    getFaultTolerant()
 * @see    setFaultTolerantReconnectTimeout(Integer)
 * @see    getFaultTolerantReconnectTimeout()
 */
int SMQ_API QueueConnectionFactory_getInitialConnectTimeout(HOBJ thisObj, jint *pseconds);

/** Get client transaction buffer size.
 *
 * @return client transaction buffer size.
 * @see    setClientTransactionBufferSize(Long)
 * @see    setFaultTolerant(Boolean)
 * @see    getFaultTolerant()
 */
int SMQ_API QueueConnectionFactory_getClientTransactionBufferSize(HOBJ thisObj, jlong *psize);

/**
 * Sets the broker hostname.
 *
 * @param thisObj the QueueConnectionFactory object to operate against.
 * @param brokerHostName a String w/ the new broker hostname to set.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API QueueConnectionFactory_setBrokerHostName(HOBJ thisObj, HOBJ brokerHostName);

/**
 * Sets the broker port number.
 *
 * @param thisObj the QueueConnectionFactory object to operate against.
 * @param brokerPort the new broker port number to set.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API QueueConnectionFactory_setBrokerPort(HOBJ thisObj, jint brokerPort);

/**
 * Sets the broker protocol.
 *
 * @param thisObj the QueueConnectionFactory object to operate against.
 * @param brokerPort a String w/ the new broker protocol to set.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API QueueConnectionFactory_setBrokerProtocol(HOBJ thisObj, HOBJ brokerProtocol);

/**
 * Sets the broker URL.
 *
 * @param thisObj the QueueConnectionFactory object to operate against.
 * @param brokerPort a String w/ the new broker URL to set.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API QueueConnectionFactory_setBrokerURL(HOBJ thisObj, HOBJ brokerURL);

/**
 * Sets the client ID.
 *
 * @param thisObj the QueueConnectionFactory object to operate against.
 * @param brokerPort a String w/ the new client ID to set.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API QueueConnectionFactory_setClientID(HOBJ thisObj, HOBJ clientID);

/**
 * Sets the connect ID.
 *
 * @param thisObj the QueueConnectionFactory object to operate against.
 * @param brokerPort a String w/ the new connect ID to set.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API QueueConnectionFactory_setConnectID(HOBJ thisObj, HOBJ connectID);

/**
 * Configures a list of brokers to try when creating a connection.
 *
 * @param thisObj the QueueConnectionFactory object to operate against.
 * @param brokerList   a String containing a comma-separated list of broker URLs.
 *                     Allows a client to connect to the first available broker on a list.
 *                     If present, this parameter overrides the brokerURL parameter in the
 *                     QueueConnectionFactory constructor, which specifies a single broker.
 *                     This option can be used independently of any other load balancing options.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @see QueueConnectionFactory_getConnectionURLs()
 * @see QueueConnectionFactory_setSequential()
 * @see QueueConnectionFactory_getSequential()
 */
int SMQ_API QueueConnectionFactory_setConnectionURLs(HOBJ thisObj, HOBJ brokerList);

/** 
 * Set the default Password for connections created from this factory.
 * 
 * @param password The password as a String.
 * 
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 * 
 * @see QueueConnectionFactory_getDefaultPassword()
 */
int SMQ_API QueueConnectionFactory_setDefaultPassword(HOBJ thisObj, HOBJ password);
    
/** 
 * Set the default Username for connections created from this factory.
 * 
 * @param username The Username as a String.
 * 
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 * 
 * @see QueueConnectionFactory_getDefaultUser()
 */

int SMQ_API QueueConnectionFactory_setDefaultUser(HOBJ thisObj, HOBJ username);
/**
 * Enable or disable client-side load balancing.
 *
 * @param thisObj the QueueConnectionFactory object to operate against.
 * @param loadBalancing    if true, indicates that the client is willing to have
 *                         a connect request re-directed to another broker within
 *                         a SonicMQ cluster.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @see QueueConnectionFactory_getLoadBalancing()
 */
int SMQ_API QueueConnectionFactory_setLoadBalancing(HOBJ thisObj, jboolean loadBalancing);

/**
 * Specifies whether to start with the first broker in the list or a random element.
 *
 * @param thisObj the QueueConnectionFactory object to operate against.
 * @param sequential   If true, starts attempting to connect to the first broker
 *                     in the list; if false, starts attempting to connect to a random element in 
 *                     the list. 
 *                     After that, tries to connect to each broker in sequence 
 *                     until a successful connect occurs, or the list is exhausted.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @see QueueConnectionFactory_getSequential()
 * @see QueueConnectionFactory_setConnectionURLs()
 * @see QueueConnectionFactory_getConnectionURLs()
 */
int SMQ_API QueueConnectionFactory_setSequential(HOBJ thisObj, jboolean sequential);

/**
 * Enables or disables Nagle's algorithm for created TCP connections.
 *
 * @param thisObj the QueueConnectionFactory object to operate against.
 * @param noDelay   If true, created connections have Nagle's algorithm disabled.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API QueueConnectionFactory_setTcpNoDelay(HOBJ thisObj, jboolean noDelay);

/** 
 * Sets the default batch size of all sessions created via connections
 * created from this connection factory. A session's batch size specifies
 * the amount of information to be batched on the client.  This is a
 * performance optimization utilized on transacted sessions.  The size
 * parameter is a measure of the message payloads only.
 *
 * @param thisObj the QueueConnectionFactory object to operate against.
 * @param size  The number of bytes to batch, specified as a jint.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API QueueConnectionFactory_setDefaultTxnBatchSize(HOBJ thisObj, jint size);

/** 
 * Sets the time, in milliseconds, that C-Client will wait for a connection before aborting.
 * Default value is zero, which tells C-Client to make an untimed connect call.
 *
 * @param thisObj the QueueConnectionFactory object to operate against.
 * @param timeout The number of milliseconds to wait for a connection to be established.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API QueueConnectionFactory_setTCPConnectionTimeout(HOBJ thisObj, jint timeout);

/** Set the administrative flow control monitoring interval.
 *
 * @param thisObj the QueueConnectionFactory object to operate against.
 * @param interval The interval in seconds over which administrative monitoring of flow control will occur.
 *                 A value of 0 (zero) indicates that no monitoring will occur.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @see    getMonitorInterval()
 */
int SMQ_API QueueConnectionFactory_setMonitorInterval(HOBJ thisObj, jint interval);

/** Enable or disable fault tolerant connection creation.
 *<P>
 * By default, connections are created non-fault tolerant(false). For a connection to
 * be created fault-tolerant, fault-tolerant must be set in the ConnectionFactory,
 * and, the broker must support(be licensed for) fault-tolerance.
 *
 * @param faultTolerant    true indicates the client wishes to create fault-tolerant
 * connections
 * @see    getFaultTolerant()
 * @see    progress.message.jclient.Connection#isFaultTolerant()
 * @see    setFaultTolerantReconnectTimeout(Integer)
 * @see    getFaultTolerantReconnectTimeout()
 * @see    setInitialConnectTimeout(Integer)
 * @see    getInitialConnectTimeout()
 * @see    setClientTransactionBufferSize(Long)
 * @see    getClientTransactionBufferSize()
 */
int SMQ_API QueueConnectionFactory_setFaultTolerant(HOBJ thisObj, jboolean faultTolerant);
  

/** Set fault tolerance reconnect timeout.
 * <P>
 * Indicates how long the client runtime will try to establish a physical network connection for the purpose of
 * connection re-establishment after failure. Applicable to fault-tolerant connections only.
 * <P>
 * For reconnects, attempts are made to the previously connected broker URL and to redundant broker URLs
 * reported by the previously connected broker. Redundant broker URLs may be on the last connected
 * broker(if redundant network interfaces are available), or, on a standby broker. URLs are redundantly
 * associated if they have the same Directory Service acceptor name. The broker dynamically supplies the
 * client runtime with redundant URL information(at connect time and on configuration change). Redundant
 * URL information can be obtained by the progress.message.jclient.Connection methods getBrokerReconnectURLs()
 * and getBrokerStandbyReconnectURLs().
 * <P>
 * Default 60 seconds. 0 means no timeout; the runtime will try indefinitely, or, until the broker reports that
 * it can no longer support the fault-tolerant client. A broker can limit the amount of time it maintains
 * state on behalf of a failed fault-tolerant client that has not reconnected by configuration of the
 * advanced broker connections parameter "Client Reconnect Timeout".
 * <P>
 * For reconnects after connection failure, if the connection cannot be reconnected within the allocated time,
 * an exception will be returned to the ExceptionListener(if configured), and all pending connection operations
 * will fail. This is the usual failed connection behavior.
 * <P>
 * @param seconds    maximum time in seconds to attempt reconnection of a failed
 *                   fault-tolerant connection
 * @see    getFaultTolerantReconnectTimeout()
 * @see    setFaultTolerant(Boolean)
 * @see    getFaultTolerant()
 * @see    progress.message.jclient.Connection#getBrokerReconnectURLs()
 * @see    progress.message.jclient.Connection#getStandbyBrokerReconnectURLs()
 * @see    setInitialConnectTimeout(Integer)
 * @see    getInitialConnectTimeout()
 */
  
int SMQ_API QueueConnectionFactory_setFaultTolerantReconnectTimeout(HOBJ thisObj, jint seconds);
  
/** Set initial connect timeout for a fault-tolerant connection.
 * <P>
 * Indicates how long the client runtime will try to establish a physical network connection for the purpose of
 * initial connection establishment. Applicable to fault-tolerant connections only.
 * <P>
 * For initial connections, attempts are made against all URLs listed in the ConnectionFactory. URLs
 * are attempted sequentially. The starting URL is normally the first in the list but can be randomly
 * selected.
 * <P>
 * Default 30.
 * 0 means no timeout; the runtime will try indefinitely.
 * -1 means each URL is attempted one time only; the runtime will try each URL sequentially one at a time
 * until a successful connection is made, or until all URLs have been tried.
 * <P>
 * For initial connection establishment, if connection cannot be made within the allocated time the
 * ConnectionFactory create method throws a JMSException.
 *
 * @param seconds    maximum time in seconds to attempt initial connection of a
 *                   fault-tolerant connection
 * @see    getInitialConnectTimeout()
 * @see    setFaultTolerant(Boolean)
 * @see    getFaultTolerant()
 * @see    setFaultTolerantReconnectTimeout(Integer)
 * @see    getFaultTolerantReconnectTimeout()
 */
int SMQ_API QueueConnectionFactory_setInitialConnectTimeout(HOBJ thisObj, jint seconds);

/** Set client transaction buffer size.
 * Indicates the maximum size of messages in bytes the client runtime is willing to buffer
 * per transaction to support transactions over fault tolerant connections.
 * <P>
 * Transacted message remain in the client runtime until saved or replicated by the broker.
 * JMS client threads sending transacted messages will block if the buffer size is reached,
 * and resume when the broker saves or replicates more messages.
 * A larger buffer size should yield better performance at the expense of more client memory
 * and longer resend time during fault-tolerant reconnect.
 * <P>
 * The default setting (0) indicates that the client runtime must be able to buffer up
 * to the broker Transactions Buffer Size parameter per transaction
 * <P>
 * @param size client transaction buffer size in bytes.
 * @see    getClientTransactionBufferSize()
 * @see    setFaultTolerant(Boolean)
 * @see    getFaultTolerant()
 */
int SMQ_API QueueConnectionFactory_setClientTransactionBufferSize(HOBJ thisObj, jlong size);


/** Get the prefetch count for the QueueConnectionFactory. When this value
 * is greater than one, the broker can send multiple
 * messages as part of a single QueueReceiver request.
 *
 * @param thisObj the QueueConnectionFactory to operate against
 * @param pCount The number of messages to prefetch.
 *
 * @see QueueConnectionFactory_setPrefetchCount()
 */
int SMQ_API QueueConnectionFactory_getPrefetchCount(HOBJ thisObj, jint *pCount);

/** Set the prefetch count for the QueueConnectionFactory.  When this value
 * is greater than one, the broker can send multiple
 * messages as part of a single QueueReceiver request. This can
 * improve performance.
 *
 * <p>Note that this is a SonicMQ extension not found in the
 * standard  interface.
 *
 * @param thisObj the QueueConnectionFactory to operate against
 * @param count The number of messages to prefetch.
 * 
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @see QueueConnectionFactory_setPrefetchThreshold(int)
 * @see QueueConnectionFactory_getPrefetchCount()
 */
int SMQ_API QueueConnectionFactory_setPrefetchCount(HOBJ thisObj, jint count);

/**
 * Get the prefetch threshold for the QueueConnectionFactory.  When the
 * number of messages waiting to be processed by the QueueReceiver
 * falls to, or below, this number, a new batch of messages will be fetched.
 *
 * @param thisObj the QueueConnectionFactory to operate against
 * @param pCount The threshold value for prefetching messages.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @see QueueConnectionFactory_setPrefetchCount()
 */
int SMQ_API QueueConnectionFactory_getPrefetchThreshold(HOBJ thisObj, jint *pCount);

/**
 * Set the prefetch threshold for the QueueConnectionFactory.  When the
 * number of messages waiting to be processed by the QueueReceiver
 * falls to, or below, this number, a new batch of messages is fetched.
 * This number cannot exceed the prefetch count.
 *
 * <p>Setting this to a value greater than zero allows the QueueReceiver
 * to always have messages available for processing locally without
 * waiting for a broker interaction.  This improves performance.
 *
 * <p>For example, a threshold value of two and a prefetch count of five
 * will cause the QueueReceiver to fetch batches of five messages from the
 * broker whenever the number of messages locally waiting for processing
 * drops below two.
 *
 * <p>Note that this is a SonicMQ extension not found in the
 * standard interface.
 *
 * @param thisObj the QueueConnectionFactory to operate against
 * @param threshold The threshold value for prefetching messages.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @see QueueConnectionFactory_setPrefetchCount(int)
 * @see QueueConnectionFactory_getPrefetchThreshold()
 */
int SMQ_API QueueConnectionFactory_setPrefetchThreshold(HOBJ thisObj, jint threshold);

/**
 * Create a new QueueConnectionFactory.
 *
 * @param brokerURL a String containing the broker URL.
 * @param pret will contain the newly created QueueConnectionFactory
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API QueueConnectionFactory_create(HOBJ brokerURL, HOBJ *pret);

/**
 * Create a new QueueConnectionFactory.
 *
 * @param brokerHostName a String containing the broker hostname.
 * @param brokerPort the broker port number.
 * @param brokerProtocol a String containing the broker protocol.
 * @param defaultUserName a String containing the default username.
 * @param defaultPassword a String containing the default password.
 * @param pret will contain the newly created QueueConnectionFactory
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API QueueConnectionFactory_create2(
	HOBJ brokerHostName, 
	jint brokerPort,
	HOBJ brokerProtocol, 
	HOBJ defaultUserName,
	HOBJ defaultPassword, 
	HOBJ *pret);

/**
 * Create a new QueueConnectionFactory.
 *
 * @param brokerHostName a String containing the broker hostname.
 * @param brokerPort the broker port number.
 * @param brokerProtocol a String containing the broker protocol.
 * @param connectID a String containing the connection ID.
 * @param defaultUserName a String containing the default username.
 * @param defaultPassword a String containing the default password.
 * @param pret will contain the newly created QueueConnectionFactory
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API QueueConnectionFactory_create3(
	HOBJ brokerHostName, 
	jint brokerPort,
	HOBJ brokerProtocol, 
	HOBJ connectID, 
	HOBJ defaultUserName,
	HOBJ defaultPassword,
	HOBJ *pret);

/**
 * Create a new QueueConnectionFactory.
 *
 * @param brokerURL a String containing the broker URL.
 * @param connectID a String containing the connection ID.
 * @param pret will contain the newly created QueueConnectionFactory
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API QueueConnectionFactory_create4(
	HOBJ brokerURL, 
	HOBJ connectID, 
	HOBJ *pret);

/**
 * Create a new QueueConnectionFactory.
 *
 * @param brokerURL a String containing the broker URL.
 * @param defaultUserName a String containing the default username.
 * @param defaultPassword a String containing the default password.
 * @param pret will contain the newly created QueueConnectionFactory
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API QueueConnectionFactory_create5(
	HOBJ brokerURL, 
	HOBJ defaultUserName,
	HOBJ defaultPassword,
	HOBJ *pret);

/**
 * Create a new QueueConnectionFactory.
 *
 * @param brokerURL a String containing the broker URL.
 * @param connectID a String containing the connection ID.
 * @param defaultUserName a String containing the default username.
 * @param defaultPassword a String containing the default password.
 * @param pret will contain the newly created QueueConnectionFactory
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API QueueConnectionFactory_create6(
	HOBJ brokerURL, 
	HOBJ connectID,
	HOBJ defaultUserName, 
	HOBJ defaultPassword,
	HOBJ *pret);

/*
//---------------------------------
// QueueReceiver
//---------------------------------
*/
/**
 * Decrements the reference count of the QueueReceiver object.
 *
 * @param qReceiver the QueueReceiver object to operate against.
 * @param pret will contain the reference count after decrementing.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define QueueReceiver_release(qReceiver, pret)                Object_release(qReceiver, pret)

/**
 * Get the int corresponding to the type of a given object.
 *
 * @param thisObj the object to operate against.
 * @param pret will contain the int corresponding to the type.
 */
#define QueueReceiver_getType(thisObj, pret)                Object_getType(thisObj, pret)

/**
 * Determines whether the given object is an instance of the given type.
 *
 * @param thisObj the object to check.
 * @param classtype the type to check against.
 * @param pret non-zero if the object is an instance of the type.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define QueueReceiver_instanceof(thisObj, classtype, pret)  Object_instanceof(thisObj, classtype, pret)

/**
 * Determines whether the two QueueReceiver objects are the same.
 *
 * @param qReceiver the first QueueReceiver to compare.
 * @param hobj2 the second QueueReceiver to compare.
 * @param pret true if the QueueReceiver objects have identical content, false otherwise.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define QueueReceiver_equals(qReceiver, hobj2, pret)          Object_equals(qReceiver, hobj2, pret)

/**
 * Get the String representation for the given QueueReceiver.
 *
 * @param qReceiver the QueueReceiver object to operate against.
 * @param pret will contain the String that represents the QueueReceiver.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define QueueReceiver_toString(qReceiver, pret)               Object_toString(qReceiver, pret)

/** Get this queue receiver's message selector expression.
  *
  * @param qReceiver the QueueReceiver to operate against
  * @param pret this QueueReceiver's message selector
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  */
#define QueueReceiver_getMessageSelector(qReceiver, pret)     MessageConsumer_getMessageSelector(qReceiver, pret)

/** Get the queue receiver's MessageListener.
  *
  * @param qReceiver the QueueReceiver to operate against
  * @param pret the listener for the QueueReceiver, or null if this isn't
  * one set.
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  *
  * @see MessageConsumer_setMessageListener()
  */
#define QueueReceiver_getMessageListener(qReceiver, pret)     MessageConsumer_getMessageListener(qReceiver, pret)

/** Set the QueueReceiver's MessageListener.  If the connection is
 * started, this will also start the process of getting messages.
 *
 * @param qReceiver the QueueReceiver to operate against
 * @param listener the messages are delivered to this listener
 * 
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 * 
 * @see MessageConsumer_getMessageListener()
 */
#define QueueReceiver_setMessageListener(qReceiver, listener) MessageConsumer_setMessageListener(qReceiver, listener)

/** Receive the next message produced for this message consumer.
 * <P>This call blocks indefinitely until a message is produced.
 *
 * <P>If this receive is done within a transaction, the message
 * remains on the consumer until the transaction commits.
 *
 * @param qReceiver the QueueReceiver to operate against
 * @param pMsg the next message produced for this message consumer,
 *         or null, if the connection closes while waiting
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define QueueReceiver_receive(qReceiver, pMsg)                MessageConsumer_receive(qReceiver, pMsg)

/** Receive the next message that arrives within the specified
 * timeout interval.
 *
 * <P>This call blocks until either a message arrives or the
 * timeout expires.
 *
 * @param qReceiver the QueueReceiver to operate against
 * @param timeout the timeout value (in milliseconds)
 * @param pMsg the next message produced for this message consumer,
 *        or null, if the connection closes while waiting, or if
 *        the wait times out.
 */
#define QueueReceiver_receive2(qReceiver, timeout, pMsg)      MessageConsumer_receive2(qReceiver, timeout, pMsg)

/** Receive the next message if one is immediately available.
 *
 * @param qReceiver the QueueReceiver to operate against
 * @param pMsg the next message produced for this message consumer, or
 *        null if one is not available.
 */
#define QueueReceiver_receiveNoWait(qReceiver, pMsg)          MessageConsumer_receiveNoWait(qReceiver, pMsg)

/**
 * Returns the int corresponding to the QueueReceiver type.
 *
 * @return the int corresponding to the QueueReceiver type
 */
int SMQ_API QueueReceiver_type();

/** Since SonicMQ allocates some resources on behalf of a
 * MessageConsumer inside the SonicMQ client run-time, clients should close them when they
 * are not needed.
 *
 * @param qReceiver the QueueReceiver to operate against
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API QueueReceiver_close(HOBJ qReceiver);

/** Get the queue associated with this queue receiver.
 *
 * @param qReceiver the QueueReceiver to operate against
 * @param pQueue the queue
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API QueueReceiver_getQueue(HOBJ qReceiver, HOBJ *pQueue);

/** Get the prefetch count for the QueueReceiver. When this value
 * is greater than one, the broker can send multiple
 * messages as part of a single QueueReceiver request.
 *
 * @param qReceiver the QueueReceiver to operate against
 * @param pCount The number of messages to prefetch.
 *
 * @see QueueReceiver_setPrefetchCount()
 */
int SMQ_API QueueReceiver_getPrefetchCount(HOBJ qReceiver, jint *pCount);

/** Set the prefetch count for the QueueReceiver.  When this value
 * is greater than one, the broker can send multiple
 * messages as part of a single QueueReceiver request. This can
 * improve performance.
 *
 * <p>Note that this is a SonicMQ extension not found in the
 * standard  interface.
 *
 * @param qReceiver the QueueReceiver to operate against
 * @param count The number of messages to prefetch.
 * 
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @see QueueReceiver_setPrefetchThreshold(int)
 * @see QueueReceiver_getPrefetchCount()
 */
int SMQ_API QueueReceiver_setPrefetchCount(HOBJ qReceiver, jint count);

/**
 * Get the prefetch threshold for the QueueReceiver.  When the
 * number of messages waiting to be processed by the QueueReceiver
 * falls to, or below, this number, a new batch of messages will be fetched.
 *
 * @param qReceiver the QueueReceiver to operate against
 * @param pCount The threshold value for prefetching messages.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @see QueueReceiver_setPrefetchCount()
 */
int SMQ_API QueueReceiver_getPrefetchThreshold(HOBJ qReceiver, jint *pCount);

/**
 * Set the prefetch threshold for the QueueReceiver.  When the
 * number of messages waiting to be processed by the QueueReceiver
 * falls to, or below, this number, a new batch of messages is fetched.
 * This number cannot exceed the prefetch count.
 *
 * <p>Setting this to a value greater than zero allows the QueueReceiver
 * to always have messages available for processing locally without
 * waiting for a broker interaction.  This improves performance.
 *
 * <p>For example, a threshold value of two and a prefetch count of five
 * will cause the QueueReceiver to fetch batches of five messages from the
 * broker whenever the number of messages locally waiting for processing
 * drops below two.
 *
 * <p>Note that this is a SonicMQ extension not found in the
 * standard interface.
 *
 * @param qReceiver the QueueReceiver to operate against
 * @param threshold The threshold value for prefetching messages.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @see QueueReceiver_setPrefetchCount(int)
 * @see QueueReceiver_getPrefetchThreshold()
 */
int SMQ_API QueueReceiver_setPrefetchThreshold(HOBJ qReceiver, jint threshold);

/*
//---------------------------------
// QueueSender
//---------------------------------
*/
/**
 * Decrements the reference count of the QueueSender object.
 *
 * @param thisObj the QueueSender object to operate against.
 * @param pret will contain the reference count after decrementing.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define QueueSender_release(thisObj, pret)                     Object_release(thisObj, pret)

/**
 * Get the int corresponding to the type of a given object.
 *
 * @param thisObj the object to operate against.
 * @param pret will contain the int corresponding to the type.
 */
#define QueueSender_getType(thisObj, pret)                     Object_getType(thisObj, pret)

/**
 * Determines whether the given object is an instance of the given type.
 *
 * @param thisObj the object to check.
 * @param classtype the type to check against.
 * @param pret non-zero if the object is an instance of the type.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define QueueSender_instanceof(thisObj, classtype, pret)       Object_instanceof(thisObj, classtype, pret)

/**
 * Determines whether the two QueueSender objects are the same.
 *
 * @param thisObj the first QueueSender to compare.
 * @param hobj2 the second QueueSender to compare.
 * @param pret true if the QueueSender objects have identical content, false otherwise.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define QueueSender_equals(thisObj, hobj2, pret)               Object_equals(thisObj, hobj2, pret)

/**
 * Get the String representation for the given QueueSender.
 *
 * @param thisObj the QueueSender object to operate against.
 * @param pret will contain the String that represents the QueueSender.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define QueueSender_toString(thisObj, pret)                    Object_toString(thisObj, pret)

/** Set whether message IDs are disabled.
  *  
  * <P>Since message ID's take some effort to create and increase a
  * message's size, SonicMQ can optimize message
  * overhead if they are given a hint that message ID is not used by
  * an application. SonicMQ message Producers provide a hint to disable
  * message ID. When a client sets a Producer to disable message ID
  * they are saying that they do not depend on the value of message
  * ID for the messages it produces. These messages must either have
  * message ID set to null or, if the hint is ignored, messageID must
  * be set to its normal unique value.
  *
  * <P>Message IDs are enabled by default.
  *
  * @param thisObj the QueueSender to operate against
  * @param value indicates if message IDs are disabled.
  *  
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  */ 
#define QueueSender_setDisableMessageID(thisObj, value)        MessageProducer_setDisableMessageID(thisObj, value)

/** Get an indication of whether message IDs are disabled.
  *  
  * @param thisObj the QueueSender to operate against
  * @param pDisabled an indication of whether message IDs are disabled.
  *  
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  */ 
#define QueueSender_getDisableMessageID(thisObj, pDisabled)    MessageProducer_getDisableMessageID(thisObj, pDisabled)

/** Set whether message timestamps are disabled.
  *  
  * <P>Since timestamps take some effort to create and increase a 
  * message's size, SonicMQ can optimize message 
  * overhead if they are given a hint that timestamp is not used by an 
  * application. SonicMQ message Producers provide a hint to disable 
  * timestamps. When a client sets a producer to disable timestamps 
  * they are saying that they do not depend on the value of timestamp 
  * for the messages it produces. These messages must either have 
  * timestamp set to null or, if the hint is ignored, timestamp must 
  * be set to its normal value.
  *  
  * <P>Message timestamps are enabled by default.
  *
  * @param thisObj the QueueSender to operate against
  * @param value indicates if message timestamps are disabled.
  *  
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  */ 
#define QueueSender_setDisableMessageTimestamp(thisObj, value) MessageProducer_setDisableMessageTimestamp(thisObj, value)

/** Get an indication of whether message timestamps are disabled.
  *  
  * @param thisObj the QueueSender to operate against
  * @param pDisabled an indication of whether message IDs are disabled.
  *  
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  */
#define QueueSender_getDisableMessageTimestamp(thisObj, pDisabled) MessageProducer_getDisableMessageTimestamp(thisObj, pDisabled)

/** Set the producer's default delivery mode.
  *  
  * <P>Delivery mode is set to NON_PERSISTENT by default.
  *
  * @param thisObj the QueueSender to operate against
  * @param deliveryMode the message delivery mode for this QueueSender
  *  
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  *
  * @see QueueSender_getDeliveryMode()
  */ 
#define QueueSender_setDeliveryMode(thisObj, deliveryMode)     MessageProducer_setDeliveryMode(thisObj, deliveryMode)

/** Get the producer's default delivery mode.
  *  
  * @param thisObj the QueueSender to operate against
  * @param pDelMode the message delivery mode for this QueueSender.
  *  
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  *
  * @see QueueSender_setDeliveryMode()
  */ 
#define QueueSender_getDeliveryMode(thisObj, pDelMode)         MessageProducer_getDeliveryMode(thisObj, pDelMode)

/** Set the producer's default priority.
  *  
  * <P>Priority is set to 4, by default.
  *
  * @param thisObj the QueueSender to operate against
  * @param value the message priority for this QueueSender.
  *  
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  *
  * @see QueueSender_getPriority()
  */ 
#define QueueSender_setPriority(thisObj, value)                MessageProducer_setPriority(thisObj, value)

/** Get the producer's default priority.
  *  
  * @param thisObj the QueueSender to operate against
  * @param pPriority the message priority for this QueueSender.
  *  
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  *
  * @see QueueSender_setPriority()
  */ 
#define QueueSender_getPriority(thisObj, pPriority)            MessageProducer_getPriority(thisObj, pPriority)

/** Set the default length of time in milliseconds from its dispatch time
  * that a produced message should be retained by the message system.
  *
  * <P>Time to live is set to zero by default.
  *
  * @param thisObj the QueueSender to operate against
  * @param timeToLive the message time to live in milliseconds; zero is
  * unlimited
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  *
  * @see MessageProducer_getTimeToLive()
  */
#define QueueSender_setTimeToLive(thisObj, value)              MessageProducer_setTimeToLive(thisObj, value)

/** Get the default length of time in milliseconds from its dispatch time
  * that a produced message should be retained by the message system.
  *
  * @param thisObj the QueueSender to operate against
  * @param pTTL the message time to live in milliseconds; zero is unlimited
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  *
  * @see QueueSender_setTimeToLive()
  */ 
#define QueueSender_getTimeToLive(thisObj, pTTL)               MessageProducer_getTimeToLive(thisObj, pTTL)

/** Since SonicMQ allocates some resources on behalf of a
  * QueueSender, clients should close them when they are not needed.
  *  
  * @param thisObj the QueueSender to operate against
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  */ 
#define QueueSender_close(thisObj)                             MessageProducer_close(thisObj)

/**
 * Returns the int corresponding to the QueueSender type.
 *
 * @return the int corresponding to the QueueSender type
 */
int SMQ_API QueueSender_type();

/**
 * Get the queue associated with this sender.
 *
 * @param thisObj the QueueSender object to operate against.
 * @param pret this sender's Queue object
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to get queue for this queue sender
 *                         due to some internal error.
 */
int SMQ_API QueueSender_getQueue(HOBJ thisObj, HOBJ *pret);

/**
 * Send a Message to the queue
 * Use the queues default delivery mode, timeToLive and priority. 
 *
 * @param thisObj the QueueSender object to operate against.
 * @param message the Message to send
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to send the message
 *                         due to some internal error.
 * @exception MessageFormatException if invalid message specified
 * @exception InvalidDestinationException if a client uses
 *                         this method with a Queue Sender with
 *                         an invalid queue.
 */
int SMQ_API QueueSender_send(HOBJ thisObj, HOBJ message);

/**
 * Send a Message to the queue specifying delivery mode, priority 
 * and time to live to the queue.
 *
 * @param thisObj the QueueSender object to operate against.
 * @param message the Message to send
 * @param deliveryMode the delivery mode to use
 * @param priority the priority for this message
 * @param timeToLive the message's lifetime (in milliseconds).
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to send the message
 *                         due to some internal error.
 * @exception MessageFormatException if invalid message specified
 * @exception InvalidDestinationException if a client uses
 *                         this method with a Queue Sender with
 *                         an invalid queue.
 * @exception IllegalStateException if the delivery mode is set
 *                         to DISCARDABLE.
 */
int SMQ_API QueueSender_send2(HOBJ thisObj,HOBJ message,jint deliveryMode,jint priority,jlong timeToLive);

/**
 * Send a Message to a queue for an unidentified message producer.	
 * Use the queues default delivery mode, timeToLive and priority.
 *
 * <p>Typically a JMS message producer is assigned a queue at creation 
 * time; however, JMS also supports unidentified message producers 
 * which require that the queue be supplied on every message send.
 *
 * @param thisObj the QueueConnectionFactory object to operate against.
 * @param queue the Queue to send this message to
 * @param message the Message to send
 *  
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to send the message
 *                         due to some internal error.
 * @exception MessageFormatException if invalid message specified
 * @exception InvalidDestinationException if a client uses
 *                         this method with an invalid queue.
 */ 
int SMQ_API QueueSender_send3(HOBJ thisObj,HOBJ queue,HOBJ message);

/**
 * Send a Message to a queue for an unidentified message producer,
 * specifying delivery mode, priority and time to live.
 *  
 * <p>Typically a JMS message producer is assigned a queue at creation
 * time; however, JMS also supports unidentified message producers
 * which require that the queue be supplied on every message send.
 *
 * @param thisObj the QueueConnectionFactory object to operate against.
 * @param queue the Queue to send this message to
 * @param message the Message to send
 * @param deliveryMode the delivery mode to use
 * @param priority the priority for this message
 * @param timeToLive the message's lifetime (in milliseconds).
 *  
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to send the message due to some internal error.
 * @exception MessageFormatException if invalid message specified
 * @exception InvalidDestinationException if a client uses this method with an invalid Queue.
 * @exception IllegalStateException if the delivery mode is set to DISCARDABLE.
 */ 
int SMQ_API QueueSender_send4(HOBJ thisObj,HOBJ queue,HOBJ message,jint deliveryMode,jint priority,jlong timeToLive);

/*
//---------------------------------
// QueueSession
//---------------------------------
*/
/**
 * Decrements the reference count of the QueueSession object.
 *
 * @param thisObj the QueueSession object to operate against.
 * @param pret will contain the reference count after decrementing.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define QueueSession_release(thisObj, pret)                  Object_release(thisObj, pret)

/**
 * Get the int corresponding to the type of a given object.
 *
 * @param thisObj the object to operate against.
 * @param pret will contain the int corresponding to the type.
 */
#define QueueSession_getType(thisObj, pret)                  Object_getType(thisObj, pret)

/**
 * Determines whether the given object is an instance of the given type.
 *
 * @param thisObj the object to check.
 * @param classtype the type to check against.
 * @param pret non-zero if the object is an instance of the type.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define QueueSession_instanceof(thisObj, classtype, pret)    Object_instanceof(thisObj, classtype, pret)

/**
 * Determines whether the two QueueSession objects are the same.
 *
 * @param thisObj the first QueueSession to compare.
 * @param hobj2 the second QueueSession to compare.
 * @param pret true if the QueueSession objects have identical content, false otherwise.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define QueueSession_equals(thisObj, hobj2, pret)            Object_equals(thisObj, hobj2, pret)

/**
 * Get the String representation for the given QueueSession.
 *
 * @param thisObj the QueueSession object to operate against.
 * @param pret will contain the String that represents the QueueSession.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define QueueSession_toString(thisObj, pret)                 Object_toString(thisObj, pret)

/**
 * Create a Message. The Message interface is the root interface of
 * all JMS messages. It holds all the standard message header
 * information. It can be sent when a message containing only header
 * information is sufficient.
 *
 * @param thisObj the QueueSession object to operate against.
 * @param pret will contain the newly created Message
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to create this message
 *                         due to some internal error.
 */
#define QueueSession_createMessage(thisObj, pret)            Session_createMessage(thisObj, pret)

/**
 * Create a BytesMessage. A BytesMessage is used to send a message
 * containing a stream of uninterpreted bytes.
 *
 * @param thisObj the QueueSession object to operate against.
 * @param pret will contain the newly created Message
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to create this message
 *                         due to some internal error.
 */
#define QueueSession_createBytesMessage(thisObj, pret)       Session_createBytesMessage(thisObj, pret)


/**
 * Create a BytesMessage. A BytesMessage is used to send a message
 * containing a stream of uninterpreted bytes.
 *
 * @param thisObj the QueueSession object to operate against.
 * @param pret will contain the newly created Message
 * @param preAllocatdSize The preallocated size of the bytes message
 * @param growthSize The size by with the bytes message will grow once the
 * previous buffer size is filled up
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to create this message
 *                         due to some internal error.
 */
#define QueueSession_createBytesMessage2(thisObj, preAllocatedSize, growthSize, pret)       Session_createBytesMessage2(thisObj, preAllocatedSize, growthSize, pret)

/**
 * Create a TextMessage. A TextMessage is used to send a message
 * containing a String.
 *
 * @param thisObj the QueueSession object to operate against.
 * @param pret will contain the newly created Message
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to create this message
 *                         due to some internal error.
 */
#define QueueSession_createTextMessage(thisObj, pret)        Session_createTextMessage(thisObj, pret)

/**
 * Create an initialized TextMessage. A TextMessage is used to send
 * a message containing a String.
 *
 * @param thisObj the QueueSession object to operate against.
 * @param text a String containing the text used to initialize this message.
 * @param pret will contain the newly created Message
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to create this message
 *                         due to some internal error.
 */
#define QueueSession_createTextMessage2(thisObj, text, pret) Session_createTextMessage2(thisObj, text, pret)

/**
 * Is the session in transacted mode?
 *
 * @param thisObj the QueueSession object to operate against.
 * @param pret true if in transacted mode
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to return the transaction
 *                         mode due to internal error in JMS Provider.
 */
#define QueueSession_getTransacted(thisObj, pret)            Session_getTransacted(thisObj, pret)

/**
 * Commit all messages done in this transaction and release any locks
 * currently held.
 *
 * @param thisObj the QueueSession object to operate against.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS implementation fails to commit the
 *                         the transaction due to some internal error.
 * @exception TransactionRolledBackException  if the transaction
 *                         gets rolled back due to some internal error
 *                         during commit.
 * @exception IllegalStateException if commit() is called on a 
 *                         non-transacted session
 */
#define QueueSession_commit(thisObj)                         Session_commit(thisObj)

/**
 * Rollback any messages done in this transaction and releases any locks
 * currently held.
 *
 * @param thisObj the QueueSession object to operate against.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS implementation fails to rollback the
 *                         the transaction due to some internal error.
 * @exception IllegalStateException if rollback() called in a non-transacted
 *                         session
 */
#define QueueSession_rollback(thisObj)                       Session_rollback(thisObj)

/**
 * Close a session. Clients should close sessions when they are not needed.
 *
 * @param thisObj the QueueSession object to operate against.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS implementation fails to close a
 *                         Session due to some internal error.
 */
#define QueueSession_close(thisObj)                          Session_close(thisObj)

/**
 * Stop message delivery in this session, and restart sending messages
 * with the oldest unacknowledged message.
 *
 * <p>All consumers deliver messages in a serial order.
 * Acknowledging a received message automatically acknowledges all
 * messages that have been delivered to the client in the current session.
 *
 * <p>Restarting a session causes it to take the following actions:
 *
 * <ul>
 *   <li>Stop message delivery
 *   <li>Mark all messages that might have been delivered but not
 *       acknowledged as `redelivered'
 *   <li>Restart the delivery sequence including all unacknowledged
 *       messages that had been previously delivered.
 *
 *     <p>Redelivered messages do not have to be delivered in
 *        exactly their original delivery order.
 * </ul>
 *
 * @param thisObj the QueueSession object to operate against.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS implementation fails to stop message
 *                         delivery and restart message send due to
 *                         due to some internal error.
 * @exception IllegalStateException if recover() is called in a transacted
 *                         session
 */
#define QueueSession_recover(thisObj)                        Session_recover(thisObj)

/** 
 * Gets the transaction batch size in bytes of this session.
 * A session's batch size specifies the amount of information 
 * to be batched on the client.  This is a performance optimization 
 * utilized on transacted sessions.  The size parameter is a measure 
 * of the message payloads only.
 *
 * @param thisObj the QueueSession object to operate against.
 * @param pret Points to the number of bytes to batch, specified as a jint.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define QueueSession_getTxnBatchSize(thisObj, pret)          Session_getTxnBatchSize(thisObj, pret)

/** 
 * Sets the transaction batch size in bytes of this session.
 * A session's batch size specifies the amount of information 
 * to be batched on the client.  This is a performance optimization 
 * utilized on transacted sessions.  The size parameter is a measure 
 * of the message payloads only.
 *
 * @param thisObj the QueueSession object to operate against.
 * @param batchSize The number of bytes to batch, specified as a jint.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define QueueSession_setTxnBatchSize(thisObj, batchSize)     Session_setTxnBatchSize(thisObj, batchSize)

/**
 * Returns the int corresponding to the QueueSession type.
 *
 * @return the int corresponding to the QueueSession type
 */
int SMQ_API QueueSession_type();

/**
 * Create a Queue given a Queue name.
 *
 * <p>This facility is provided for the rare cases where clients need to
 * dynamically manipulate queue identity. This allows the creation of a
 * queue with a provider specific name. Clients that depend on this
 * ability are not portable.
 *
 * @param thisObj the QueueSession object to operate against.
 * @param name a String containing the name of the Queue to create
 * @param pret will contain a newly created Queue with the given name.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if a session fails to create a queue
 *                         due to some JMS error.
 */ 
int SMQ_API QueueSession_createQueue(HOBJ thisObj, HOBJ name, HOBJ *pret);

/**
 * Create a QueueReceiver to receive messages from the specified queue.
 *
 * @param thisObj the QueueSession object to operate against.
 * @param queue the queue to access
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if a session fails to create a receiver
 *                         due to some SonicMQ error.
 * @exception InvalidDestinationException if invalid Queue specified.
 */
int SMQ_API QueueSession_createReceiver(HOBJ thisObj, HOBJ queue, HOBJ *pReceiver);

/**
 * Create a QueueReceiver to receive messages from the specified queue.
 *
 * @param thisObj the QueueSession object to operate against.
 * @param queue the queue to access
 * @param messageSelector only messages with properties matching the
 * message selector expression are delivered
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if a session fails to create a receiver
 *                         due to some SonicMQ error.
 * @exception InvalidDestinationException if invalid Queue specified.
 * @exception InvalidSelectorException if the message selector is invalid.
 *
 */
int SMQ_API QueueSession_createReceiver2(HOBJ thisObj, HOBJ queue, HOBJ messageSelector, HOBJ *pret);

/**
 * Create a QueueSender to send messages to the specified queue.
 *
 * @param thisObj the QueueSession object to operate against.
 * @param queue the Queue to access, or null if this is an unidentifed producer.
 * @param pret will contain the newly created Queue
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if a session fails to create a sender due to some JMS error.
 * @exception InvalidDestinationException if invalid Queue specified.
 */
int SMQ_API QueueSession_createSender(HOBJ thisObj, HOBJ queue, HOBJ *pret);

/** Create a QueueBrowser to Browse messages from the specified queue.
 *
 * @param thisObj the QueueSession object to operate against.
 * @param queue the queue to access
 * @param pret will contain the newly created QueueBrowser
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if a session fails to create a Browser
 *                         due to some SonicMQ error.
 * @exception InvalidDestinationException if invalid Queue specified.
 */
int SMQ_API QueueSession_createBrowser(HOBJ thisObj, HOBJ queue, HOBJ *pret);

/** Create a QueueBrowser to Browse messages from the specified queue.
 *
 * @param thisObj the QueueSession object to operate against.
 * @param queue the queue to access
 * @param messageSelector only messages with properties matching the 
 *        message selector expression are delivered
 * @param pret will contain the newly created QueueBrowser
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if a session fails to create a Browser
 *               due to some SonicMQ error.
 * @exception InvalidDestinationException if invalid Queue specified.
 * @exception InvalidSelectorException if the message selector is invalid.
 *
 */
int SMQ_API QueueSession_createBrowser2(HOBJ thisObj, HOBJ queue, HOBJ messageSelector, HOBJ *pret);

/** Create a temporary queue. Its lifetime will be that of the 
 * QueueConnection unless deleted earlier.
 *
 * @param thisObj the QueueSession object to operate against.
 * @param pret the newly created TemporaryQueue.
 *
 * @exception JMSException if a session fails to create a Temporary Queue
 *                         due to some JMS error.
 */
int SMQ_API QueueSession_createTemporaryQueue(HOBJ thisObj, HOBJ *pret);

/**
 * Disables or reenables publish flow control.  If flow control is disabled
 * an exception will be thrown when the client enters a flow control condition.
 * In the C API this will cause send on the queue to fail.
 *
 * @param thisObj the QueueSession to operate against
 * @param disabled true to disable flow control
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error (e.g., invalid Queue).
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate
 *         error details.
 */
int SMQ_API QueueSession_setFlowControlDisabled(HOBJ thisObj, jboolean disabled);

/**
 * Returns the acknowledgement mode of the session. The acknowledgement
 * mode is set at the time that the session is created. If the session is
 * transacted, the acknowledgement mode is ignored.
 *
 * @param thisObj the Session object to operate against.
 * @param pret    If the session is not transacted, returns the current
 *                acknowledgement mode for the session. If the session
 *                is transacted, returns SESSION_TRANSACTED.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define QueueSession_getAcknowledgeMode(thisObj, pret)  Session_getAcknowledgeMode(thisObj, pret)

/*
//---------------------------------
// Session
//---------------------------------
*/
/**
 * Decrements the reference count of the Session object.
 *
 * @param thisObj the Session object to operate against.
 * @param pret will contain the reference count after decrementing.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Session_release(thisObj, pret)                     Object_release(thisObj, pret)

/**
 * Get the int corresponding to the type of a given object.
 *
 * @param thisObj the object to operate against.
 * @param pret will contain the int corresponding to the type.
 */
#define Session_getType(thisObj, pret)                     Object_getType(thisObj, pret)

/**
 * Determines whether the given object is an instance of the given type.
 *
 * @param thisObj the object to check.
 * @param classtype the type to check against.
 * @param pret non-zero if the object is an instance of the type.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Session_instanceof(thisObj, classtype, pret)       Object_instanceof(thisObj, classtype, pret)

/**
 * Determines whether the two Session objects are the same.
 *
 * @param thisObj the first Session to compare.
 * @param hobj2 the second Session to compare.
 * @param pret true if the Session objects have identical content, false otherwise.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Session_equals(thisObj, hobj2, pret)               Object_equals(thisObj, hobj2, pret)

/**
 * Get the String representation for the given Session.
 *
 * @param thisObj the Session object to operate against.
 * @param pret will contain the String that represents the Session.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Session_toString(thisObj, pret)                    Object_toString(thisObj, pret)

/**
 * Returns the int corresponding to the Session type.
 *
 * @return the int corresponding to the Session type
 */
int SMQ_API Session_type();

/**
 * Create a Message. The Message interface is the root interface of
 * all JMS messages. It holds all the standard message header
 * information. It can be sent when a message containing only header
 * information is sufficient.
 *
 * @param thisObj the Session object to operate against.
 * @param pret will contain the newly created Message
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to create this message
 *                         due to some internal error.
 */
int SMQ_API Session_createMessage(HOBJ thisObj, HOBJ *pret);

/**
 * Create a BytesMessage. A BytesMessage is used to send a message
 * containing a stream of uninterpreted bytes.
 *
 * @param thisObj the Session object to operate against.
 * @param pret will contain the newly created Message
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to create this message
 *                         due to some internal error.
 */
int SMQ_API Session_createBytesMessage(HOBJ thisObj, HOBJ *pret);

/**
 * Create a BytesMessage. A BytesMessage is used to send a message
 * containing a stream of uninterpreted bytes.
 *
 * @param thisObj the Session object to operate against.
 * @param pret will contain the newly created Message
 * @param preAllocatdSize The preallocated size of the bytes message
 * @param growthSize The size by with the bytes message will grow once the
 * previous buffer size is filled up 
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to create this message
 *                         due to some internal error.
 */
int SMQ_API Session_createBytesMessage2(HOBJ thisObj, jint preAllocatedSize, jint growthSize, HOBJ *pret);

/**
 * Create a TextMessage. A TextMessage is used to send a message
 * containing a String.
 *
 * @param thisObj the Session object to operate against.
 * @param pret will contain the newly created Message
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to create this message
 *                         due to some internal error.
 */
int SMQ_API Session_createTextMessage(HOBJ thisObj, HOBJ *pret);

/**
 * Create an initialized TextMessage. A TextMessage is used to send
 * a message containing a String.
 *
 * @param thisObj the Session object to operate against.
 * @param text a String containing the text used to initialize this message.
 * @param pret will contain the newly created Message
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to create this message
 *                         due to some internal error.
 */
int SMQ_API Session_createTextMessage2(HOBJ thisObj, HOBJ text, HOBJ *pret);

/**
 * Is the session in transacted mode?
 *
 * @param thisObj the Session object to operate against.
 * @param pret true if in transacted mode
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to return the transaction
 *                         mode due to internal error in JMS Provider.
 */
int SMQ_API Session_getTransacted(HOBJ thisObj, jboolean *pret);

/**
 * Commit all messages done in this transaction and release any locks
 * currently held.
 *
 * @param thisObj the Session object to operate against.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS implementation fails to commit the
 *                         the transaction due to some internal error.
 * @exception TransactionRolledBackException  if the transaction
 *                         gets rolled back due to some internal error
 *                         during commit.
 * @exception IllegalStateException if commit() is called on a 
 *                         non-transacted session
 */
int SMQ_API Session_commit(HOBJ thisObj);

/**
 * Rollback any messages done in this transaction and releases any locks
 * currently held.
 *
 * @param thisObj the Session object to operate against.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS implementation fails to rollback the
 *                         the transaction due to some internal error.
 * @exception IllegalStateException if rollback() called in a non-transacted
 *                         session
 */
int SMQ_API Session_rollback(HOBJ thisObj);

/**
 * Close a session. Clients should close sessions when they are not needed.
 *
 * @param thisObj the Session object to operate against.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS implementation fails to close a
 *                         Session due to some internal error.
 */
int SMQ_API Session_close(HOBJ thisObj);

/**
 * Stop message delivery in this session, and restart sending messages
 * with the oldest unacknowledged message.
 *
 * <p>All consumers deliver messages in a serial order.
 * Acknowledging a received message automatically acknowledges all
 * messages that have been delivered to the client in the current session.
 *
 * <p>Restarting a session causes it to take the following actions:
 *
 * <ul>
 *   <li>Stop message delivery
 *   <li>Mark all messages that might have been delivered but not
 *       acknowledged as `redelivered'
 *   <li>Restart the delivery sequence including all unacknowledged
 *       messages that had been previously delivered.
 *
 *     <p>Redelivered messages do not have to be delivered in
 *        exactly their original delivery order.
 * </ul>
 *
 * @param thisObj the Session object to operate against.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS implementation fails to stop message
 *                         delivery and restart message send due to
 *                         due to some internal error.
 * @exception IllegalStateException if recover() is called in a transacted
 *                         session
 */
int SMQ_API Session_recover(HOBJ thisObj);

/** 
 * Gets the transaction batch size in bytes of this session.
 * A session's batch size specifies the amount of information 
 * to be batched on the client.  This is a performance optimization 
 * utilized on transacted sessions.  The size parameter is a measure 
 * of the message payloads only.
 *
 * @param thisObj the Session object to operate against.
 * @param pret Points to the number of bytes to batch, specified as a jint.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API Session_getTxnBatchSize(HOBJ thisObj, jint *pret);

/** 
 * Sets the transaction batch size in bytes of this session.
 * A session's batch size specifies the amount of information 
 * to be batched on the client.  This is a performance optimization 
 * utilized on transacted sessions.  The size parameter is a measure 
 * of the message payloads only.
 *
 * @param thisObj the Session object to operate against.
 * @param batchSize The number of bytes to batch, specified as a jint.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API Session_setTxnBatchSize(HOBJ thisObj, jint batchSize);

/**
 * Returns the acknowledgement mode of the session. The acknowledgement
 * mode is set at the time that the session is created. If the session is
 * transacted, the acknowledgement mode is ignored.
 *
 * @param thisObj the Session object to operate against.
 * @param pret    If the session is not transacted, returns the current
 *                acknowledgement mode for the session. If the session
 *                is transacted, returns SESSION_TRANSACTED.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API Session_getAcknowledgeMode(HOBJ thisObj, jint *pret);


/**
 * Specifies whether the batching of acknowledgments will be permitted in the session.
 *
 * @param thisObj the Session object to operate against.
 * @param enabled
 * 
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API Session_setAckBatchingEnabled(HOBJ thisObj, jboolean enabled);

/**
 * Returns whether the batching of acknowledgments is be permitted in the session.
 *
 * @param thisObj the Session object to operate against.
 * @param penabled
 * 
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API Session_getAckBatchingEnabled(HOBJ thisObj, jboolean *penabled);

/*
//---------------------------------
// TextMessage
//---------------------------------
*/
/**
 * Decrements the reference count of the TextMessage object.
 *
 * @param textMsg the TextMessage object to operate against.
 * @param pret will contain the reference count after decrementing.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define TextMessage_release(textMsg, pret)                     Object_release(textMsg, pret)

/**
 * Get the int corresponding to the type of a given object.
 *
 * @param thisObj the object to operate against.
 * @param pret will contain the int corresponding to the type.
 */
#define TextMessage_getType(thisObj, pret)                     Object_getType(thisObj, pret)

/**
 * Determines whether the given object is an instance of the given type.
 *
 * @param thisObj the object to check.
 * @param classtype the type to check against.
 * @param pret non-zero if the object is an instance of the type.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define TextMessage_instanceof(thisObj, classtype, pret)       Object_instanceof(thisObj, classtype, pret)

/**
 * Determines whether the two TextMessage objects are the same.
 *
 * @param textMsg the first TextMessage to compare.
 * @param hobj2 the second TextMessage to compare.
 * @param pret true if the TextMessage objects have identical content, false otherwise.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define TextMessage_equals(textMsg, hobj2, pret)               Object_equals(textMsg, hobj2, pret)

/**
 * Get the String representation for the given TextMessage.
 *
 * @param textMsg the TextMessage object to operate against.
 * @param pret will contain the String that represents the TextMessage.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define TextMessage_toString(textMsg, pret)                    Object_toString(textMsg, pret)

/**
 * Create a new TextMessage that is a copy of the given TextMessage.
 *
 * @param textMsg the TextMessage to clone.
 * @param pret will contain the newly created TextMessage.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define TextMessage_clone(textMsg, pret)                       Object_clone(textMsg, pret)

/** Get the message ID.
  *
  * <P>The messageID header field contains a value that uniquely
  * identifies each message sent by a SonicMQ client.
  *
  * <P>When a message is sent, messageID can be ignored. When
  * the send method returns it contains an assigned value.
  *
  * <P>A JMSMessageID is a String value which should function as a
  * unique key for identifying messages in a historical repository.
  *
  * <P>All JMSMessageID values must start with the prefix 'ID:'.
  * Uniqueness of message ID values across different brokers is
  * not required.
  *
  * <P>Since message ID's take some effort to create and increase a
  * message's size, SonicMQ can optimize message
  * overhead if it is given a hint that message ID is not used by
  * an application. SonicMQ message Producers provide a hint to disable
  * message ID. When a client sets a Producer to disable message ID
  * they are saying that they do not depend on the value of message
  * ID for the messages it produces. These messages must either have
  * message ID set to null or, if the hint is ignored, messageID must
  * be set to its normal unique value.
  *
  * @param textObj the TextMessage to operate against
  * @param pret the returned message ID as a String
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  *
  * @see TextMessage_setJMSMessageID()
  */
#define TextMessage_getJMSMessageID(textMsg, pret)             Message_getJMSMessageID(textMsg, pret)

/** Set the message ID.
  *
  * <P>The SonicMQ run time sets this field when a message is sent. This operation
  * can be used to change the value of a message that has been received.
  *
  * @param textObj the TextMessage to operate against
  * @param value the String to set the message ID to
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  *
  * @see TextMessage_getJMSMessageID()
  */
#define TextMessage_setJMSMessageID(textMsg, value)            Message_setJMSMessageID(textMsg, value)

/** Get the message timestamp.
  *
  * <P>The JMSTimestamp header field contains the time a message was
  * handed off to the SonicMQW client run time to be sent. It is not the time the
  * message was actually transmitted because the actual send may occur
  * later due to transactions or other client side queueing of messages.
  *
  * <P>When a message is sent, JMSTimestamp is ignored. When the send
  * method returns it contains a a time value somewhere in the interval
  * between the call and the return. It is in the format of a normal
  * Java millis time value.
  *
  * <P>Since timestamps take some effort to create and increase a
  * message's size, SonicMQ can optimize message
  * overhead if it is given a hint that timestamp is not used by an
  * application. SonicMQ message Producers provide a hint to disable
  * timestamps. When a client sets a producer to disable timestamps
  * they are saying that they do not depend on the value of timestamp
  * for the messages it produces. These messages must either have
  * timestamp set to null or, if the hint is ignored, timestamp must
  * be set to its normal value.
  *
  * @param textObj the TextMessage to operate against
  * @param pTimestamp the message timestamp value
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  *
  * @see TextMessage_setJMSTimestamp()
  */
#define TextMessage_getJMSTimestamp(textMsg, pret)             Message_getJMSTimestamp(textMsg, pret)

/** Set the message timestamp.
  *
  * <P>SonicMQ sets this field when a message is sent. This operation
  * can be used to change the value of a message that's been received.
  *
  * @param textObj the TextMessage to operate against
  * @param value the timestamp for this message
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  *
  * @see TextMessage_getJMSTimestamp()
  */
#define TextMessage_setJMSTimestamp(textMsg, value)            Message_setJMSTimestamp(textMsg, value)

/** Get the correlation ID for the message.
  *
  * <P>This method is used to return correlation id values that are
  * either SonicMQ-specific message ID's or application-specific Strings.
  *
  * @param textMsg the TextMessage to operate against
  * @param pCOrrelationID the correlation ID of a message as a String.
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  *
  * @see TextMessage_setJMSCorrelationID()
  * @see TextMessage_getJMSCorrelationIDAsBytes()
  * @see TextMessage_setJMSCorrelationIDAsBytes()
  */
#define TextMessage_getJMSCorrelationID(textMsg, pret)         Message_getJMSCorrelationID(textMsg, pret)

/** Set the correlation ID for the message.
  *
  * <P>A client can use the JMSCorrelationID header field to link one
  * message with another. A typically use is to link a response message
  * with its request message.
  *
  * <P>JMSCorrelationID can hold one of the following:
  *    <UL>
  *      <LI>A SonicMQ-specific message ID
  *      <LI>An application-specific String
  *      <LI>A native jbyteArray value.
  *    </UL>
  *
  * <P>Since each message sent in SonicMQ is assigned a message ID
  * value it is convenient to link messages via message ID. All message ID
  * values must start with the 'ID:' prefix.
  *
  * <P>In some cases, an application (made up of several clients) needs to
  * use an application specific value for linking messages. For instance,
  * an application may use JMSCorrelationID to hold a value referencing
  * some external information. Application specified values must not start
  * with the 'ID:' prefix; this is reserved for SonicMQ-generated message
  * ID values.
  *
  * <P>If a provider supports the native concept of correlation ID, a SonicMQ
  * client may need to assign specific JMSCorrelationID values to match
  * those expected by non-JMS clients. A jbyteArray value is used for this
  * purpose. JMS providers without native correlation ID values are not
  * required to support jbyteArray values. The use of a jbyteArray value for
  * JMSCorrelationID is non-portable.
  *
  * @param textObj the TextMessage to operate against
  * @param value the message ID of a message being referred to.
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  *
  * @see TextMessage_getJMSCorrelationID()
  * @see TextMessage_getJMSCorrelationIDAsBytes()
  * @see TextMessage_setJMSCorrelationIDAsBytes()
  */
#define TextMessage_setJMSCorrelationID(textMsg, value)        Message_setJMSCorrelationID(textMsg, value)

/** Get the correlation ID as an array of bytes for the message.
  *
  * <P>The use of a jbyteArray value for JMSCorrelationID is non-portable.
  *
  * @param textObj the TextMessage to operate against
  * @param pret the correlation ID of a message as an array of bytes.
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  *
  * @see TextMessage_setJMSCorrelationID()
  * @see TextMessage_getJMSCorrelationID()
  */
#define TextMessage_getJMSCorrelationIDAsBytes(textMsg, pret)  Message_getJMSCorrelationIDAsBytes(textMsg, pret)

/** Set the correlation ID as an array of bytes for the message.
  *
  * <P>If a provider supports the native concept of correlation id, a
  * SonicMQ client may need to assign specific JMSCorrelationID values to
  * match those expected by non-JMS clients. JMS providers without native
  * correlation id values are not required to support this (and the
  * corresponding get) method; their implementation may throw
  * java.lang.UnsupportedOperationException).
  *
  * <P>The use of a jbyteArray value for JMSCorrelationID is non-portable.
  *
  * @param textObj the TextMessage to operate against
  * @param value the correlation ID value as an array of bytes.
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  *
  * @see TextMessage_setJMSCorrelationID()
  * @see TextMessage_getJMSCorrelationID()
  * @see TextMessage_getJMSCorrelationIDAsBytes()
  */
#define TextMessage_setJMSCorrelationIDAsBytes(textMsg, value) Message_setJMSCorrelationIDAsBytes(textMsg, value)

/** Get where a reply to this message should be sent.
  *
  * @param textObj the TextMessage to operate against
  * @param pret the destination to send a response to this message
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  *
  * @see TextMessage_setJMSReplyTo()
  */
#define TextMessage_getJMSReplyTo(textMsg, pret)               Message_getJMSReplyTo(textMsg, pret)

/** Set where a reply to this message should be sent.
  *
  * <P>The replyTo header field contains the destination where a reply
  * to the current message should be sent. If it is null no reply is
  * expected. The destination may be either a Queue or a Topic.
  *
  * <P>Messages with a null replyTo value are called SonicMQ datagrams.
  * Datagrams may be a notification of some change in the sender (i.e.
  * they signal a sender event) or they may just be some data the sender
  * thinks is of interest.
  *
  * Messages with a replyTo value are typically expecting a response.
  * A response may be optional, it is up to the client to decide. These
  * messages are called SonicMQ requests. A message sent in response to a
  * request is called a reply.
  *
  * In some cases a client may wish to match up a request it sent earlier
  * with a reply it has just received. This can be done using the
  * correlationID.
  *
  * @param textObj the TextMessage to operate against
  * @param value Destination to send a response to this message
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  *
  * @see TextMessage_getJMSReplyTo()
  */
#define TextMessage_setJMSReplyTo(textMsg, value)              Message_setJMSReplyTo(textMsg, value)

/** Get the destination for this message.
  *
  * <P>The destination field contains the destination to which the
  * message is being sent.
  *
  * <P>When a message is sent this value is ignored. After completion
  * of the send method it holds the destination specified by the send.
  *
  * <P>When a message is received, its destination value must be
  * equivalent to the value assigned when it was sent.
  *
  * @param textObj the TextMessage to operate against
  * @param pret the Destination of this message.
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  *
  * @see TextMessage_setJMSDestination()
  */
#define TextMessage_getJMSDestination(textMsg, pret)           Message_getJMSDestination(textMsg, pret)

/** Set the destination for this message.
  *
  * <P>SonicMQ sets this field when a message is sent. This operation
  * can be used to change the value of a message that's been received.
  *
  * @param textObj the TextMessage to operate against
  * @param value the Destination for this message.
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  *
  * @see TextMessage_getJMSDestination()
  */
#define TextMessage_setJMSDestination(textMsg, value)          Message_setJMSDestination(textMsg, value)

/** Get the delivery mode for this message.
  *
  * @param textObj the TextMessage to operate against
  * @param pret the delivery mode of this message.
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  *
  * @see TextMessage_setJMSDeliveryMode()
  */
#define TextMessage_getJMSDeliveryMode(textMsg, pret)          Message_getJMSDeliveryMode(textMsg, pret)

/**
 * Set the delivery mode for this message.
 *
 * <p>Providers set this field when a message is sent. This operation
 * can be used to change the value of a message that's been received.
 *
 * @param textObj the TextMessage to operate against
 * @param value the delivery mode for this message.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @see TextMessage_setJMSDeliveryMode()
 *
 * @exception JMSException if JMS fails to set JMS DeliveryMode
 *                         due to some internal JMS error.
 */
#define TextMessage_setJMSDeliveryMode(textMsg, value)         Message_setJMSDeliveryMode(textMsg, value)

/**
 * Get an indication of whether this message is being redelivered.
 *
 * <p>If a client receives a message with the redelivered indicator set,
 * it is likely, but not guaranteed, that this message was delivered to
 * the client earlier but the client did not acknowledge its receipt at
 * that earlier time.
 *
 * @param textObj the TextMessage to operate against
 * @param pret set to true if this message is being redelivered.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @see TextMessage_setJMSDeliveryMode()
 *
 *
 * @exception JMSException if JMS fails to get JMS Redelivered flag
 *                         due to some internal JMS error.
 */
#define TextMessage_getJMSRedelivered(textMsg, pret)           Message_getJMSRedelivered(textMsg, pret)

/**
 * Set to indicate whether this message is being redelivered.
 *
 * <p>This field is set at the time the message is delivered. This
 * operation can be used to change the value of a message that's
 * been received.
 *
 * @param textObj the TextMessage to operate against
 * @param value an indication of whether this message is being redelivered.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @see TextMessage_setJMSDeliveryMode()
 *
 * @exception JMSException if JMS fails to set JMS Redelivered flag
 *                         due to some internal JMS error.
 */
#define TextMessage_setJMSRedelivered(textMsg, value)          Message_setJMSRedelivered(textMsg, value)

/**
 * Get the message type.
 *
 * @param textObj the TextMessage to operate against
 * @param pret the message type
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to get JMS message type
 *                         due to some internal JMS error.
 */
#define TextMessage_getJMSType(textMsg, pret)                  Message_getJMSType(textMsg, pret)

/** Set the message type.
 *
 * <p>Some JMS providers use a message repository that contains the
 * definition of messages sent by applications. The type header field
 * contains the name of a message's definition.
 *
 * <p>JMS does not define a standard message definition repository nor
 * does it define a naming policy for the definitions it contains. JMS
 * clients should use symbolic values for type that can be configured
 * at installation time to the values defined in the current providers
 * message repository.
 *
 * <p>JMS clients should assign a value to type whether the application
 * makes use of it or not. This insures that it is properly set for
 * those providers that require it.
 *
 * @param textObj the TextMessage to operate against
 * @param value the class of message
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to set JMS message type
 *                         due to some internal JMS error.
 */
#define TextMessage_setJMSType(textMsg, value)                 Message_setJMSType(textMsg, value)

/**
 * Get the message's expiration value.
 *
 * <p>When a message is sent, expiration is left unassigned. After
 * completion of the send method, it holds the expiration time of the
 * message. This is the sum of the time-to-live value specified by the
 * client and the GMT at the time of the send.
 *
 * <p>If the time-to-live is specified as zero, expiration is set to
 * zero which indicates the message does not expire.
 *
 * <p>When a message's expiration time is reached, a provider should
 * discard it. JMS does not define any form of notification of message
 * expiration.
 *
 * <p>Clients should not receive messages that have expired; however,
 * JMS does not guarantee that this will not happen.
 *
 * @param textObj the TextMessage to operate against
 * @param pret the time the message expires. It is the sum of the
 * time-to-live value specified by the client, and the GMT at the
 * time of the send.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to get JMS message expiration
 *                         due to some internal JMS error.
 */
#define TextMessage_getJMSExpiration(textMsg, pret)            Message_getJMSExpiration(textMsg, pret)

/**
 * Set the message's expiration value.
 *
 * <p>Providers set this field when a message is sent. This operation
 * can be used to change the value of a message that's been received.
 *
 * @param textObj the TextMessage to operate against
 * @param value the message's expiration time
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to set JMS message expiration
 *                         due to some internal JMS error.
 */
#define TextMessage_setJMSExpiration(textMsg, value)           Message_setJMSExpiration(textMsg, value)

/**
 * Get the message priority.
 *
 * <p>JMS defines a ten level priority value with 0 as the lowest
 * priority and 9 as the highest. In addition, clients should consider
 * priorities 0-4 as gradations of normal priority and priorities 5-9
 * as gradations of expedited priority.
 *
 * <p>JMS does not require that a provider strictly implement priority
 * ordering of messages; however, it should do its best to deliver
 * expedited messages ahead of normal messages.
 *
 * @param textObj the TextMessage to operate against
 * @param pret the default message priority
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to get JMS message priority
 *                         due to some internal JMS error.
 */
#define TextMessage_getJMSPriority(textMsg, pret)              Message_getJMSPriority(textMsg, pret)

/**
 * Set the priority for this message.
 *
 * <p>Providers set this field when a message is sent. This operation
 * can be used to change the value of a message that's been received.
 *
 * @param textObj the TextMessage to operate against
 * @param value the priority of this message
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to set JMS message priority
 *                         due to some internal JMS error.
 */
#define TextMessage_setJMSPriority(textMsg, value)             Message_setJMSPriority(textMsg, value)

/**
 * Clear a message's properties.
 *
 * @param textObj the TextMessage to operate against
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to clear JMS message
 *                         properties due to some internal JMS
 *                         error.
 */
#define TextMessage_clearProperties(textMsg)                   Message_clearProperties(textMsg)

/** Check if a property value exists.
 *
 * @param textObj the TextMessage to operate against
 * @param name the name of the property to test as a String
 * @param pret true if the property does exist.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to  check if property
 *                         exists due to some internal JMS
 *                         error.
 */
#define TextMessage_propertyExists(textMsg, name, pret)        Message_propertyExists(textMsg, name, pret)

/**
 * Return the boolean property value with the given name.
 *
 * @param textObj the TextMessage to operate against
 * @param name the name of the boolean property as a String
 * @param pret the boolean property value with the given name.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to  get Property due to
 *                         some internal JMS error.
 * @exception MessageFormatException if this type conversion is invalid.
 * @exception NullPointerException if property does not exist.
 */
#define TextMessage_getBooleanProperty(textMsg, name, pret)    Message_getBooleanProperty(textMsg, name, pret)

/**
 * Return the byte property value with the given name.
 *
 * @param textObj the TextMessage to operate against
 * @param name the name of the byte property as a String
 * @param pret the byte property value with the given name.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to  get Property due to
 *                         some internal JMS error.
 * @exception MessageFormatException if this type conversion is invalid.
 * @exception NullPointerException if property does not exist.
 */
#define TextMessage_getByteProperty(textMsg, name, pret)       Message_getByteProperty(textMsg, name, pret)

/**
 * Return the short property value with the given name.
 *
 * @param textObj the TextMessage to operate against
 * @param name the name of the short property as a String
 * @param pret the short property value with the given name.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to  get Property due to
 *                         some internal JMS error.
 * @exception MessageFormatException if this type conversion is invalid.
 * @exception NullPointerException if property does not exist.
 */
#define TextMessage_getShortProperty(textMsg, name, pret)      Message_getShortProperty(textMsg, name, pret)

/**
 * Return the integer property value with the given name.
 *
 * @param textObj the TextMessage to operate against
 * @param name the name of the integer property as a String
 * @param pret the integer property value with the given name.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to  get Property due to
 *                         some internal JMS error.
 * @exception MessageFormatException if this type conversion is invalid.
 * @exception NullPointerException if property does not exist.
 */
#define TextMessage_getIntProperty(textMsg, name, pret)        Message_getIntProperty(textMsg, name, pret)

/**
 * Return the long property value with the given name.
 *
 * @param textObj the TextMessage to operate against
 * @param name the name of the long property as a String
 * @param pret the long property value with the given name.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to  get Property due to
 *                         some internal JMS error.
 * @exception MessageFormatException if this type conversion is invalid.
 * @exception NullPointerException if property does not exist.
 */
#define TextMessage_getLongProperty(textMsg, name, pret)       Message_getLongProperty(textMsg, name, pret)

/**
 * Return the float property value with the given name.
 *
 * @param textObj the TextMessage to operate against
 * @param name the name of the float property as a String
 * @param pret the float property value with the given name.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to  get Property due to
 *                         some internal JMS error.
 * @exception MessageFormatException if this type conversion is invalid.
 * @exception NullPointerException if property does not exist.
 */
#define TextMessage_getFloatProperty(textMsg, name, pret)      Message_getFloatProperty(textMsg, name, pret)

/**
 * Return the double property value with the given name.
 *
 * @param textObj the TextMessage to operate against
 * @param name the name of the double property as a String
 * @param pret the double property value with the given name.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to  get Property due to
 *                         some internal JMS error.
 * @exception MessageFormatException if this type conversion is invalid.
 * @exception NullPointerException if property does not exist.
 */
#define TextMessage_getDoubleProperty(textMsg, name, pret)     Message_getDoubleProperty(textMsg, name, pret)

/**
 * Return the String property value with the given name.
 *
 * @param textObj the TextMessage to operate against
 * @param name the name of the String property as a String
 * @param pret the String property value with the given name.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to  get Property due to
 *                         some internal JMS error.
 * @exception MessageFormatException if this type conversion is invalid.
 * @exception NullPointerException if property does not exist.
 */
#define TextMessage_getStringProperty(textMsg, name, pret)     Message_getStringProperty(textMsg, name, pret)

/**
 * Return the Object property value with the given name.
 *
 * @param textObj the TextMessage to operate against
 * @param name the name of the Object property as a String
 * @param pret the Object property value with the given name.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to  get Property due to
 *                         some internal JMS error.
 * @exception MessageFormatException if this type conversion is invalid.
 * @exception NullPointerException if property does not exist.
 */
#define TextMessage_getObjectProperty(textMsg, name, pret)     Message_getObjectProperty(textMsg, name, pret)

/**
 * Return an Enumeration of all the property names.
 *
 * @param textObj the TextMessage to operate against
 * @param pret an Enumeration w/ all the names of property values.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to  get Property names due to
 *                         some internal JMS error.
 */
#define TextMessage_getPropertyNames(textMsg, pret)            Message_getPropertyNames(textMsg, pret)

/**
 * Set a boolean property value with the given name, into the Message.
 *
 * @param textObj the TextMessage to operate against
 * @param name the name of the boolean property as a String
 * @param value the boolean property value to set in the Message.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to  set Property due to
 *                         some internal JMS error.
 * @exception MessageNotWriteableException if properties are read-only
 */
#define TextMessage_setBooleanProperty(textMsg, name, value)   Message_setBooleanProperty(textMsg, name, value)

/**
 * Set a byte property value with the given name, into the Message.
 *
 * @param textObj the TextMessage to operate against
 * @param name the name of the byte property as a String
 * @param value the byte property value to set in the Message.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to  set Property due to
 *                         some internal JMS error.
 * @exception MessageNotWriteableException if properties are read-only
 */
#define TextMessage_setByteProperty(textMsg, name, value)      Message_setByteProperty(textMsg, name, value)

/**
 * Set a short property value with the given name, into the Message.
 *
 * @param textObj the TextMessage to operate against
 * @param name the name of the short property as a String
 * @param value the short property value to set in the Message.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to  set Property due to
 *                         some internal JMS error.
 * @exception MessageNotWriteableException if properties are read-only
 */
#define TextMessage_setShortProperty(textMsg, name, value)     Message_setShortProperty(textMsg, name, value)

/**
 * Set a integer property value with the given name, into the Message.
 *
 * @param textObj the TextMessage to operate against
 * @param name the name of the integer property as a String
 * @param value the integer property value to set in the Message.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to  set Property due to
 *                         some internal JMS error.
 * @exception MessageNotWriteableException if properties are read-only
 */
#define TextMessage_setIntProperty(textMsg, name, value)       Message_setIntProperty(textMsg, name, value)

/**
 * Set a long property value with the given name, into the Message.
 *
 * @param textObj the TextMessage to operate against
 * @param name the name of the long property as a String
 * @param value the long property value to set in the Message.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to  set Property due to
 *                         some internal JMS error.
 * @exception MessageNotWriteableException if properties are read-only
 */
#define TextMessage_setLongProperty(textMsg, name, value)      Message_setLongProperty(textMsg, name, value)

/**
 * Set a float property value with the given name, into the Message.
 *
 * @param textObj the TextMessage to operate against
 * @param name the name of the float property as a String
 * @param value the float property value to set in the Message.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to  set Property due to
 *                         some internal JMS error.
 * @exception MessageNotWriteableException if properties are read-only
 */
#define TextMessage_setFloatProperty(textMsg, name, value)     Message_setFloatProperty(textMsg, name, value)

/**
 * Set a double property value with the given name, into the Message.
 *
 * @param textObj the TextMessage to operate against
 * @param name the name of the double property as a String
 * @param value the double property value to set in the Message.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to  set Property due to
 *                         some internal JMS error.
 * @exception MessageNotWriteableException if properties are read-only
 */
#define TextMessage_setDoubleProperty(textMsg, name, value)    Message_setDoubleProperty(textMsg, name, value)

/**
 * Set a String property value with the given name, into the Message.
 *
 * @param textObj the TextMessage to operate against
 * @param name the name of the String property as a String
 * @param value the String property value to set in the Message.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to  set Property due to
 *                         some internal JMS error.
 * @exception MessageNotWriteableException if properties are read-only
 */
#define TextMessage_setStringProperty(textMsg, name, value)    Message_setStringProperty(textMsg, name, value)

/**
 * Set an Object property value with the given name, into the Message.
 *
 * @param textObj the TextMessage to operate against
 * @param name the name of the Object property as a String
 * @param value the Object property value to set in the Message.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to  set Property due to
 *                         some internal JMS error.
 * @exception MessageNotWriteableException if properties are read-only
 */
#define TextMessage_setObjectProperty(textMsg, name, value)    Message_setObjectProperty(textMsg, name, value)

/**
 * Acknowledge this or all previous messages received by the session.
 * 
 * <p>All JMS messages support the acknowledge() method for use when a client has specified
 * that a JMS session's messages are to be explicitly acknowledged. A client requests explicit
 * acknowledgement by creating a session with either the standard JMS CLIENT_ACKKNOWLEDGE mode,
 * or the non-JMS acknowledgement mode SINGLE_MESSAGE_ACKNOWLEDGE.
 *
 * <p>In the standard JMS CLIENT_ACKNOWLEDGE mode, all messages previously received for the
 * session are acknowledged. If the session has been created with SINGLE_MESSAGE_ACKNOWLEDGE,
 * only the current message is acknowledged.
 *
 * <p>JMS defaults to implicit message acknowledgement, AUTO_ACKNOWLEDGE. In this mode,
 * calls to acknowledge() are ignored, as JMS automatically acknowledges messages on behalf
 * of the client.
 *
 * <p>Messages that have been received but not acknowledged may be
 * redelivered to the consumer.
 *
 * @param textObj the TextMessage to operate against
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to acknowledge due to some
 *                         internal JMS error.
 */
#define TextMessage_acknowledge(textMsg)                       Message_acknowledge(textMsg)

/**
 * Clear out the message body. All other parts of the message are left
 * untouched.
 *
 * @param textObj the TextMessage to operate against
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to due to some internal JMS error.
 */
#define TextMessage_clearBody(textMsg)                         Message_clearBody(textMsg)

/**
 * Get all message properties (Progress-specific message interface extensions) in a 
 *  Hashtable object. Note that both the name and the value of a Hashtable entry
 *  will be objects. E.g. a boolean property of "true" w/ the name "isPublic" will 
 *  be represented in the Hashtable as an entry w/ the String object "isPublic" as 
 *  the key and the Boolean object w/ value "true" as the value. Use the appropriate 
 *  API functions to access the values of the objectified values.
 *
 * @param textMsg the TextMessage whose properties to retrieve
 * @param pret a Hashtable object containing the message properties as 
 *        key-value pairs (all objects)
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define TextMessage_getProperties(textMsg, pret)               Message_getProperties(textMsg, pret)

/**
 * Returns the int corresponding to the TextMessage type.
 *
 * @return the int corresponding to the TextMessage type
 */
int SMQ_API TextMessage_type();

/**
 * Create a new TextMessage
 *
 * @param pret will contain the newly created TextMessage.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API TextMessage_create(HOBJ *pret);


/** Set the string containing this message's data.
 * 
 * @param textMsg the TextMessage to operate against
 * @param string the String containing the message's data
 *  
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Likely causes of errors:
 *           MessageNotWriteableException if message in read-only mode.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */ 
int SMQ_API TextMessage_setText(HOBJ textMsg, HOBJ s);

/** Get the string containing this message's data.  The default
 * value is null.
 *  
 * @param textMsg the TextMessage to operate against
 * @param pString the String containing the message's data
 *  
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */ 
int SMQ_API TextMessage_getText(HOBJ textMsg, HOBJ *pString);


/** Get the size, in bytes, of the message body.
  *
  * @param thisObj the TextMessage to operate against
  * @param pret the number of Text in the message body 
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  *
  */
#define TextMessage_getBodySize(thisObj, pret)    Message_getBodySize(thisObj, pret)

/*
//---------------------------------
// Topic
//---------------------------------
*/

/**
 * Decrements the reference count of the Topic object.
 *
 * @param topicObj the Topic object to operate against.
 * @param pret will contain the reference count after decrementing.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Topic_release(topicObj, pret)                       Object_release(topicObj, pret)

/**
 * Get the int corresponding to the type of a given object.
 *
 * @param thisObj the object to operate against.
 * @param pret will contain the int corresponding to the type.
 */
#define Topic_getType(thisObj, pret)                       Object_getType(thisObj, pret)

/**
 * Determines whether the given object is an instance of the given type.
 *
 * @param thisObj the object to check.
 * @param classtype the type to check against.
 * @param pret non-zero if the object is an instance of the type.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Topic_instanceof(thisObj, classtype, pret)         Object_instanceof(thisObj, classtype, pret)

/**
 * Determines whether the two Topic objects are the same.
 *
 * @param topicObj the first Topic to compare.
 * @param hobj2 the second Topic to compare.
 * @param pret true if the Topic objects have identical content, false otherwise.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Topic_equals(topicObj, hobj2, pret)                 Object_equals(topicObj, hobj2, pret)

/**
 * Get the String representation for the given Topic.
 *
 * @param topicObj the Topic object to operate against.
 * @param pret will contain the String that represents the Topic.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define Topic_toString(topicObj, pret)                      Object_toString(topicObj, pret)

/**
 * Returns the int corresponding to the Topic type.
 *
 * @return the int corresponding to the Topic type
 */
int SMQ_API Topic_type();

/**
 * Get the name of this topic.
 *  
 * <p>Clients that depend upon the name are not portable.
 *
 * @param topicObj the Topic object to operate against
 * @param pret a String containing the topic name
 *  
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS implementation for Topic fails to
 *                         to return topic name due to some internal error.
 */ 
int SMQ_API Topic_getTopicName(HOBJ topicObj, HOBJ *pret);

/*
//---------------------------------
// TemporaryTopic
//---------------------------------
*/
/**
 * Decrements the reference count of the TemporaryTopic object.
 *
 * @param tmpTopic the TemporaryTopic object to operate against.
 * @param pret will contain the reference count after decrementing.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define TemporaryTopic_release(tmpTopic, pret)               Object_release(tmpTopic, pret)

/**
 * Get the int corresponding to the type of a given object.
 *
 * @param thisObj the object to operate against.
 * @param pret will contain the int corresponding to the type.
 */
#define TemporaryTopic_getType(thisObj, pret)               Object_getType(thisObj, pret)

/**
 * Determines whether the given object is an instance of the given type.
 *
 * @param thisObj the object to check.
 * @param classtype the type to check against.
 * @param pret non-zero if the object is an instance of the type.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define TemporaryTopic_instanceof(thisObj, classtype, pret) Object_instanceof(thisObj, classtype, pret)

/**
 * Determines whether the two TemporaryTopic objects are the same.
 *
 * @param tmpTopic the first TemporaryTopic to compare.
 * @param hobj2 the second TemporaryTopic to compare.
 * @param pret true if the TemporaryTopic objects have identical content, false otherwise.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define TemporaryTopic_equals(tmpTopic, hobj2, pret)         Object_equals(tmpTopic, hobj2, pret)

/**
 * Get the String representation for the given TemporaryTopic.
 *
 * @param tmpTopic the TemporaryTopic object to operate against.
 * @param pret will contain the String that represents the TemporaryTopic.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define TemporaryTopic_toString(tmpTopic, pret)              Object_toString(tmpTopic, pret)

/**
 * Get the name of this temporary topic.
 *  
 * <p>Clients that depend upon the name are not portable.
 *
 * @param tmpTopic the TemporaryTopic object to operate against
 * @param pret a String containing the topic name
 *  
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS implementation for Topic fails to
 *                         to return topic name due to some internal error.
 */ 
#define TemporaryTopic_getTopicName(tmpTopic, pret)          Topic_getTopicName(tmpTopic, pret)

/**
 * Returns the int corresponding to the TemporaryTopic type.
 *
 * @return the int corresponding to the TemporaryTopic type
 */
int SMQ_API TemporaryTopic_type();

/**
 * Delete this temporary topic. If there are still existing publishers
 * or subscribers still using it, then a JMSException will be thrown.
 *
 * @param tmpTopic the TemporaryTopic to operate against
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS implementation fails to delete a 
 *                         Temporary queue due to some internal error.
 */
int SMQ_API TemporaryTopic_delete(HOBJ tmpTopic);

/*
//---------------------------------
// TopicConnection
//---------------------------------
*/
/**
 * Decrements the reference count of the TopicConnection object.
 *
 * @param topicConn the TopicConnection object to operate against.
 * @param pret will contain the reference count after decrementing.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define TopicConnection_release(topicConn, pret)                       Object_release(topicConn, pret)

/**
 * Get the int corresponding to the type of a given object.
 *
 * @param thisObj the object to operate against.
 * @param pret will contain the int corresponding to the type.
 */
#define TopicConnection_getType(thisObj, pret)                       Object_getType(thisObj, pret)

/**
 * Determines whether the given object is an instance of the given type.
 *
 * @param thisObj the object to check.
 * @param classtype the type to check against.
 * @param pret non-zero if the object is an instance of the type.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define TopicConnection_instanceof(thisObj, classtype, pret)         Object_instanceof(thisObj, classtype, pret)

/**
 * Determines whether the two TopicConnection objects are the same.
 *
 * @param topicConn the first TopicConnection to compare.
 * @param hobj2 the second TopicConnection to compare.
 * @param pret true if the TopicConnection objects have identical content, false otherwise.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define TopicConnection_equals(topicConn, hobj2, pret)                 Object_equals(topicConn, hobj2, pret)

/**
 * Get the String representation for the given TopicConnection.
 *
 * @param topicConn the TopicConnection object to operate against.
 * @param pret will contain the String that represents the TopicConnection.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define TopicConnection_toString(topicConn, pret)                      Object_toString(topicConn, pret)

/**
 * Close the TopicConnection. Since SonicMQ typically allocates 
 * significant resources in the SonicMQ client run-time on behalf 
 * of a TopicConnection, clients should close them when they are 
 * not needed. Closing a TopicConnection will implicitly close all
 * of its sessions, producers, and consumers.
 *
 * @param topicConn the TopicConnection object to close.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define TopicConnection_close(topicConn)                               Connection_close(topicConn) 

/**
 * Start (or restart) a TopicConnection's delivery of incoming messages.
 * Restart begins with the oldest unacknowledged message.
 * Starting a started session is ignored.
 *
 * @param topicConn the TopicConnection object to start.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  * 
  * @see TopicConnection_stop()
 */
#define TopicConnection_start(topicConn)                               Connection_start(topicConn) 

/**
 * Used to temporarily stop a TopicConnection's delivery of incoming messages. It can be 
 * restarted using its start method. When stopped, delivery to all the TopicConnection's 
 * message consumers is inhibited: synchronous receive's block and messages are not 
 * delivered to message listeners. 
 *
 * <p>This call blocks until receives and/or message listeners in progress have completed. 
 *
 * <p>Stopping a Session has no affect on its ability to send messages. Stopping a stopped 
 * session is ignored. 
 *
 * <p>A stop method call must not return until delivery of messages has paused. This means 
 * a client can rely on the fact that none of its message listeners will be called and 
 * all threads of control waiting for receive to return will not return with a message 
 * until the connection is restarted. The receive timers for a stopped connection continue 
 * to advance so receives may time out while the connection is stopped. 
 *
 * <p>If MessageListeners are running when stop is invoked, stop must wait until all of them 
 * have returned before it may return. While these MessageListeners are completing, they 
 * must have full services of the connection available to them.
 *
 * @param topicConn the TopicConnection object to stop.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define TopicConnection_stop(topicConn)                                Connection_stop(topicConn) 

/**
 * Retrieve the URL of the broker for this TopicConnection.
 *
 * @param topicConn the TopicConnection object to operate against.
 * @param pBrokerURL will contain the broker URL.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define TopicConnection_getBrokerURL(topicConn, pBrokerURL)            Connection_getBrokerURL(topicConn, pBrokerURL) 

/** Get the client identifier for this topic connection.
  *
  * @param topicConn the TopicConnection object to operate against
  * @param pClientID the unique client identifier.
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  */
#define TopicConnection_getClientID(topicConn, pClientID)              Connection_getClientID(topicConn, pClientID) 

/** Set the client identifier for this topic connection.
  *
  * <P>The preferred way to assign a Client's client identifier is for
  * it to be configured in a client-specific ConnectionFactory and
  * transparently assigned to the Connection it creates. Alternatively,
  * a client can set a Connections's client identifier using a
  * specific value.
  *
  * <P>The purpose of client identifier is to associate a session and
  * its objects with a state maintained on behalf of the client by a
  * SonicMQ broker. The only such state identified by SonicMQ is that required
  * to support durable subscriptions
  *
  * @param topicConn the TopicConnection object to operate against
  * @param clientID the unique client identifier
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Likely causes of errors:
  *           InvalidClientIDException if SonicMQ client specifies an invalid or duplicate client id.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  */
#define TopicConnection_setClientID(topicConn, clientID)               Connection_setClientID(topicConn, clientID) 

/**
 * Retrieve the connection ID for this TopicConnection.
 *
 * @param topicConn the TopicConnection object to operate against.
 * @param pConnID will contain the connection ID.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define TopicConnection_getConnectionID(topicConn, pConnID)            Connection_getConnectionID(topicConn, pConnID) 

/**
 * Get the ExceptionListener for this TopicConnection.
 *
 * @param topicConn the TopicConnection object to operate against
 * @param p_pfnListener The exception listener (i.e. callback function) for this 
 *        Connection. The signature is given by 
 *        <code>typedef void (*pfnCExceptionListener)(HOBJ hobjException);</code>
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define TopicConnection_getExceptionListener(topicConn, p_pfnListener) Connection_getExceptionListener(topicConn, p_pfnListener) 

/** Set up an exception listener (i.e. a callback function) for this topic connection.
  *
  * <P>If SonicMQ detects a serious problem with a connection it
  * will inform the connection's "exception listener" if one has been
  * registered. It does this by invoking the callback function set via 
  * <code>Connection_setExceptionListener()</code and passing it a JMSException 
  * object describing the problem.
  *
  * <P>This allows a client to be asynchronously notified of a problem.
  * Some connections only consume messages so they would have no other
  * way to learn their connection has failed.
  *
  * <P>A Connection serializes execution of its exception listener.
  *
  * <P>A SonicMQ broker attempts to resolve connection problems
  * itself prior to notifying the client of them.
  *
  * @param connObj the TopicConnection object to operate against
  * @param listener the callback function to set as the exception listener--
  *        the signature is <code>typedef void (*pfnCExceptionListener)(HOBJ hobjException);</code>
  *        where hobjException is a JMSException object handle
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Likely causes of errors:
  *           InvalidClientIDException if SonicMQ client specifies an invalid or duplicate client id.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  */
#define TopicConnection_setExceptionListener(topicConn, listener)      Connection_setExceptionListener(topicConn, listener) 


/** Set a C-signature Connection State Change listener for this connection.
 */
#define TopicConnection_setConnectionStateChangeListener(topicConn, listener, user) Connection_setConnectionStateChangeListener(topicConn, listener, user)


/** Set a C-signature Connection State Change listener for this connection.
 */
#define TopicConnection_getConnectionStateChangeListener(topicConn, p_pfnListener) Connection_getConnectionStateChangeListener(topicConn, p_pfnListener)


/** Get the meta data for this topic connection.
  *
  * @param topicConn the TopicConnection object to operate against
  * @param pMetaData the ConnectionMetadata object returned
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  *
  * @see ConnectionMetaData_* functions
  */
#define TopicConnection_getMetaData(topicConn, pMetaData)              Connection_getMetaData(topicConn, pMetaData) 

/** Enable/disable sending active pings on this connection. This is a non-JMS public method.
  *
  * @param topicConn the TopicConnection object to operate against
  * @param interval indicates the interval in seconds for sending a ping. Setting 
  *        interval to 0 or any negative value effectively disables the ping. 
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Likely causes of errors:
  *           InvalidClientIDException if SonicMQ client specifies an invalid or duplicate client id.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  */
#define TopicConnection_setPingInterval(topicConn, interval)           Connection_setPingInterval(topicConn, interval) 

/**
 * Returns true if the connection is fault tolerant
 * <P>For a connection to be fault-tolerant, fault-tolerant must be set in the
 * ConnectionFactory, and the broker must support(be licensed for) fault-tolerance.
 *
 * @param topicConn the Connection object to operate against
 * @param pret Returns the true if the connection is fault tolerant.
 * 
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @see progress.message.jclient.ConnectionFactory#setFaultTolerant
 */
#define TopicConnection_isFaultTolerant(topicConn, pret)               Connection_isFaultTolerant(topicConn, pret) 

/**
 * Get connection state, one of ACTIVE, RECONNECTING, FAILED or CLOSED.
 * <P>
 * A non fault-tolerant connection will never see a RECONNECTING value.
 * This method may be called after the connection is closed.
 * 
 * @param topicConn the Connection object to operate against
 * @param pret Returns the connection state.
 * 
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @see progress.message.jclient.Constants#ACTIVE
 * @see progress.message.jclient.Constants#RECONNECTING
 * @see progress.message.jclient.Constants#FAILED
 * @see progress.message.jclient.Constants#CLOSED
 * @see progress.message.jclient.ConnectionStateChangeListener
 * @see setConnectionStateChangeListener(ConnectionStateChangeListener)
 * @see getConnectionStateChangeListener()
 */
#define TopicConnection_getConnectionState(topicConn, pret)            Connection_getConnectionState(topicConn, pret) 
                                                    
/** Get the username for this topic connection.
  *
  * @param topicConn the TopicConnection object to operate against
  * @param pUserName returns a String object containing the username.
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  *
  * @see ConnectionMetaData_* functions
  */
#define TopicConnection_getUsername(topicConn, pUserName)              Connection_getUsername(topicConn, pUserName) 

/** Creates a TopicConnection to a message broker
  * A TopicConnection is an active connection to a SonicMQ Pub/Sub messaging domain.
  * A client uses a TopicConnection to create one or more TopicSessions
  * for producing and consuming messages.
  *
  * @param brokerURL the URL of the message broker in the for [protocol://]hostname[:port]
  * @param connectID the id used for connection. This may be null.
  * @param username the name of client's user account.
  * @param password the password for client's user account
  * @param pTopicConn pointer to the new topic connection
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error (e.g., connection to message
  *         broker can not be created). Use SMQ_getLastError() and SMQ_getLastErrorText() to
  *         interrogate error details.
  */
int SMQ_API TopicConnection_create(HOBJ brokerURL, HOBJ connectID, HOBJ username, HOBJ password, HOBJ* pTopicConn);

/** Creates a TopicSession associated with the TopicConnection
  *
  * @param topicConn the TopicConnection to operate against
  * @param transacted if true, the session is transacted.
  * @param acknowledgeMode indicates whether the consumer or the
  *        client will acknowledge any messages it receives. This parameter
  *        will be ignored if the session is transacted.
  * @param pTopicSession pointer to the new TopicSession
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error. Use SMQ_getLastError() and
  *         SMQ_getLastErrorText() to interrogate error details.
  */
int SMQ_API TopicConnection_createTopicSession(HOBJ topicConn, jboolean transacted, jint ackMode, HOBJ* pTopicSession);

/**
 * Returns the int corresponding to the TopicConnection type.
 *
 * @return the int corresponding to the TopicConnection type
 */
int SMQ_API TopicConnection_type();

/*
//---------------------------------
// TopicConnectionFactory
//---------------------------------
*/
/**
 * Decrements the reference count of the TopicConnectionFactory object.
 *
 * @param topicConnectionFactory the TopicConnectionFactory object to operate against.
 * @param pret will contain the reference count after decrementing.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define TopicConnectionFactory_release(topicConnectionFactory, pret)                Object_release(topicConnectionFactory, pret)

/**
 * Get the int corresponding to the type of a given object.
 *
 * @param thisObj the object to operate against.
 * @param pret will contain the int corresponding to the type.
 */
#define TopicConnectionFactory_getType(thisObj, pret)                Object_getType(thisObj, pret)

/**
 * Determines whether the given object is an instance of the given type.
 *
 * @param thisObj the object to check.
 * @param classtype the type to check against.
 * @param pret non-zero if the object is an instance of the type.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define TopicConnectionFactory_instanceof(thisObj, classtype, pret)  Object_instanceof(thisObj, classtype, pret)

/**
 * Determines whether the two TopicConnectionFactory objects are the same.
 *
 * @param topicConnectionFactory the first TopicConnectionFactory to compare.
 * @param hobj2 the second TopicConnectionFactory to compare.
 * @param pret true if the TopicConnectionFactory objects have identical content, false otherwise.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define TopicConnectionFactory_equals(topicConnectionFactory, hobj2, pret)          Object_equals(topicConnectionFactory, hobj2, pret)

/**
 * Get the String representation for the given TopicConnectionFactory.
 *
 * @param topicConnectionFactory the TopicConnectionFactory object to operate against.
 * @param pret will contain the String that represents the TopicConnectionFactory.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define TopicConnectionFactory_toString(topicConnectionFactory, pret)               Object_toString(topicConnectionFactory, pret)

/**
 * Create a queue connection with default user identity.
 *
 * @param thisObj the TopicConnectionFactory object to operate against.
 * @param pret a newly created TopicConnection.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS Provider fails to create a Queue Connection
 *                         due to some internal error.
 * @exception JMSSecurityException  if client authentication fails due to
 *                         invalid user name or password.
 */
int SMQ_API TopicConnectionFactory_createTopicConnection(HOBJ thisObj, HOBJ *pret);


/** C interface to register the LoginSPI with the topic connection factory.
 *  When used with 4.0 brokers only clear text passwords are supported.  Users should
 *  use the setPassword method to set the clear text password.
 *  When used with 5.0 brokers only both clear text and encrypted (byte array) 
 *  passwords are supported.  Users should use the setPassword method to set the clear 
 *  text password and/or use setTransformedPassword to set the encrypted byte array
 *  password.
 *
 * @param thisObj the TopicConnectionFactory object to operate against.
 * @param map the map that implements the necessary login SPI routines
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API TopicConnectionFactory_setLoginSPI(HOBJ thisObj, ILoginSPICMap *cmap);

/**
 * Create a topic connection with specified user identity.
 *
 * @param thisObj the TopicConnectionFactory object to operate against.
 * @param username the caller's user name
 * @param password the caller's password
 * @param pret a newly created TopicConnection.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS Provider fails to create a Topic Connection
 *                         due to some internal error.
 * @exception JMSSecurityException  if client authentication fails due to
 *                         invalid user name or password.
 */
int SMQ_API TopicConnectionFactory_createTopicConnection2(HOBJ thisObj, HOBJ username, HOBJ password, HOBJ *pret);

/**
 * Retrieve the broker hostname.
 *
 * @param thisObj the TopicConnectionFactory object to operate against.
 * @param pret will contain a String w/ the broker hostname.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API TopicConnectionFactory_getBrokerHostName(HOBJ thisObj, HOBJ *pret);

/**
 * Retrieve the broker port number.
 *
 * @param thisObj the TopicConnectionFactory object to operate against.
 * @param pret will contain the broker port number.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API TopicConnectionFactory_getBrokerPort(HOBJ thisObj, jint *pret);

/**
 * Retrieve the broker protocol.
 *
 * @param thisObj the TopicConnectionFactory object to operate against.
 * @param pret a String containing the broker protocol.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API TopicConnectionFactory_getBrokerProtocol(HOBJ thisObj, HOBJ *pret);

/**
 * Retrieve the broker URL.
 *
 * @param thisObj the TopicConnectionFactory object to operate against.
 * @param pret the String containing the broker URL.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API TopicConnectionFactory_getBrokerURL(HOBJ thisObj, HOBJ *pret);

/**
 * Retrieve the client ID.
 *
 * @param thisObj the TopicConnectionFactory object to operate against.
 * @param pret the String containing the client ID.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API TopicConnectionFactory_getClientID(HOBJ thisObj, HOBJ *pret);

/**
 * Retrieve the connect ID.
 *
 * @param thisObj the TopicConnectionFactory object to operate against.
 * @param pret the String containing the connect ID.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API TopicConnectionFactory_getConnectID(HOBJ thisObj, HOBJ *pret);

/**
 * Get list of brokers to try to connect to.
 *
 * @param thisObj the TopicConnectionFactory object to operate against.
 * @param pret a String containing a comma-separated list of broker URLs..
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API TopicConnectionFactory_getConnectionURLs(HOBJ thisObj, HOBJ *pret);

/**
 * Retrieve the default password.
 *
 * @param thisObj the TopicConnectionFactory object to operate against.
 * @param pret a String containing the default password.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API TopicConnectionFactory_getDefaultPassword(HOBJ thisObj, HOBJ *pret);

/**
 * Retrieve the default username.
 *
 * @param thisObj the TopicConnectionFactory object to operate against.
 * @param pret a String containing the default username.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API TopicConnectionFactory_getDefaultUser(HOBJ thisObj, HOBJ *pret);

/**
 * Determines whether client-side load balancing is enabled.
 *
 * @param thisObj the TopicConnectionFactory object to operate against.
 * @param pret if true, indicates that the client is willing to have a connect request
 *        redirected to another broker within a SonicMQ cluster.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @see    TopicConnectionFactory_setLoadBalancing()
 */
int SMQ_API TopicConnectionFactory_getLoadBalancing(HOBJ thisObj, jboolean *pret);

/**
 * Determines whether the option to start with the first or a random element of the broker list
 * has been selected.
 *
 * @param thisObj the TopicConnectionFactory object to operate against.
 * @param pret If true, connect attempts start with the first broker in the list.
 *        If false, connect attempts start with a random element in the broker list.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @see TopicConnectionFactory_setSequential()
 * @see TopicConnectionFactory_setConnectionURLs()
 * @see TopicConnectionFactory_getConnectionURLs()
 */
int SMQ_API TopicConnectionFactory_getSequential(HOBJ thisObj, jboolean *pret);

/**
 * Determines whether the option to disable Nagle's algorithm for TCP connections
 * has been selected.
 *
 * @param thisObj the TopicConnectionFactory object to operate against.
 * @param pret If true, created TCP connections will have Nagle's algorithm disabled.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API TopicConnectionFactory_getTcpNoDelay(HOBJ thisObj, jboolean *pret);

/** 
 * Gets the default batch size of all sessions created via connections
 * created from this connection factory. A session's batch size specifies
 * the amount of information to be batched on the client.  This is a
 * performance optimization utilized on transacted sessions.  The size
 * parameter is a measure of the message payloads only.
 *
 * @param thisObj the TopicConnectionFactory object to operate against.
 * @param pret Points to the number of bytes to batch, specified as a jint.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API TopicConnectionFactory_getDefaultTxnBatchSize(HOBJ thisObj, jint *pret);

/** 
 * Gets the time, in milliseconds, that C-Client will wait for a connection before aborting.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API TopicConnectionFactory_getTCPConnectionTimeout(HOBJ thisObj, jint *pret);

/** Get the administrative flow control monitoring interval.
 *
 * @param thisObj the TopicConnectionFactory object to operate against.
 * @param pinterval Pointer to the interval in seconds over which administrative monitoring 
 *                  of flow control will occur. A value of 0 (zero) indicates that no 
 *                  monitoring will occur.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @see    setMonitorInterval()
 */
int SMQ_API TopicConnectionFactory_getMonitorInterval(HOBJ thisObj, jint *pinterval);

/** Determines whether fault tolerant connection creation is enabled.
 *<P>
 * By default, connections are created non-fault tolerant(false). For a connection to
 * be created fault-tolerant, fault-tolerant must be set in the ConnectionFactory,
 * and, the broker must support(be licensed for) fault-tolerance.
 *
 * @param thisObj the TopicConnectionFactory object to operate against.
 *
 * @param  pfaulttolerant true indicates the client wishes to create fault-tolerant
 * connections
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 * 
 * @see    setFaultTolerant(Boolean)
 * @see    progress.message.jclient.Connection#isFaultTolerant()
 * @see    setFaultTolerantReconnectTimeout(Integer)
 * @see    getFaultTolerantReconnectTimeout()
 * @see    setInitialConnectTimeout(Integer)
 * @see    getInitialConnectTimeout()
 * @see    setClientTransactionBufferSize(Long)
 * @see    getClientTransactionBufferSize()
 */
int SMQ_API TopicConnectionFactory_getFaultTolerant(HOBJ thisObj, jboolean *pfaulttolerant);

/** Get fault tolerance reconnect timeout.
 *
 * @return maximum time in seconds to attempt reconnection of a failed
 *         fault-tolerant connection
 * @see    setFaultTolerantReconnectTimeout(Integer)
 * @see    setFaultTolerant(Boolean)
 * @see    getFaultTolerant()
 * @see    setInitialConnectTimeout(Integer)
 * @see    getInitialConnectTimeout()
 */
int SMQ_API TopicConnectionFactory_getFaultTolerantReconnectTimeout(HOBJ thisObj, jint *pseconds);

/** Get initial connect timeout for a fault-tolerant connection.
 *
 * @return maximum time in seconds to attempt initial connection of a fault-tolerant connection
 *         fault-tolerant connection
 * @see    setInitialConnectTimeout(Integer)
 * @see    setFaultTolerant(Boolean)
 * @see    getFaultTolerant()
 * @see    setFaultTolerantReconnectTimeout(Integer)
 * @see    getFaultTolerantReconnectTimeout()
 */
int SMQ_API TopicConnectionFactory_getInitialConnectTimeout(HOBJ thisObj, jint *pseconds);

/** Get client transaction buffer size.
 *
 * @return client transaction buffer size.
 * @see    setClientTransactionBufferSize(Long)
 * @see    setFaultTolerant(Boolean)
 * @see    getFaultTolerant()
 */
int SMQ_API TopicConnectionFactory_getClientTransactionBufferSize(HOBJ thisObj, jlong *psize);

/**
 * Sets the broker hostname.
 *
 * @param thisObj the TopicConnectionFactory object to operate against.
 * @param brokerHostName a String w/ the new broker hostname to set.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API TopicConnectionFactory_setBrokerHostName(HOBJ thisObj, HOBJ brokerHostName);

/**
 * Sets the broker port number.
 *
 * @param thisObj the TopicConnectionFactory object to operate against.
 * @param brokerPort the new broker port number to set.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API TopicConnectionFactory_setBrokerPort(HOBJ thisObj, jint brokerPort);

/**
 * Sets the broker protocol.
 *
 * @param thisObj the TopicConnectionFactory object to operate against.
 * @param brokerPort a String w/ the new broker protocol to set.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API TopicConnectionFactory_setBrokerProtocol(HOBJ thisObj, HOBJ brokerProtocol);

/**
 * Sets the broker URL.
 *
 * @param thisObj the TopicConnectionFactory object to operate against.
 * @param brokerPort a String w/ the new broker URL to set.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API TopicConnectionFactory_setBrokerURL(HOBJ thisObj, HOBJ brokerURL);

/**
 * Sets the client ID.
 *
 * @param thisObj the TopicConnectionFactory object to operate against.
 * @param brokerPort a String w/ the new client ID to set.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API TopicConnectionFactory_setClientID(HOBJ thisObj, HOBJ clientID);

/**
 * Sets the connect ID.
 *
 * @param thisObj the TopicConnectionFactory object to operate against.
 * @param brokerPort a String w/ the new connect ID to set.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API TopicConnectionFactory_setConnectID(HOBJ thisObj, HOBJ connectID);

/**
 * Configures a list of brokers to try when creating a connection.
 *
 * @param thisObj the TopicConnectionFactory object to operate against.
 * @param brokerList   a String containing a comma-separated list of broker URLs.
 *                     Allows a client to connect to the first available broker on a list.
 *                     If present, this parameter overrides the brokerURL parameter in the
 *                     TopicConnectionFactory constructor, which specifies a single broker.
 *                     This option can be used independently of any other load balancing options.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @see TopicConnectionFactory_getConnectionURLs()
 * @see TopicConnectionFactory_setSequential()
 * @see TopicConnectionFactory_getSequential()
 */
int SMQ_API TopicConnectionFactory_setConnectionURLs(HOBJ thisObj, HOBJ brokerList);

/** 
 * Set the default Password for connections created from this factory.
 * 
 * @param password The password as a String.
 * 
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 * 
 * @see TopicConnectionFactory_getDefaultPassword()
 */
int SMQ_API TopicConnectionFactory_setDefaultPassword(HOBJ thisObj, HOBJ password);
    
/** 
 * Set the default Username for connections created from this factory.
 * 
 * @param username The Username as a String.
 * 
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 * 
 * @see TopicConnectionFactory_getDefaultUser()
 */
int SMQ_API TopicConnectionFactory_setDefaultUser(HOBJ thisObj, HOBJ username);

/**
 * Enable or disable client-side load balancing.
 *
 * @param thisObj the TopicConnectionFactory object to operate against.
 * @param loadBalancing    if true, indicates that the client is willing to have
 *                         a connect request re-directed to another broker within
 *                         a SonicMQ cluster.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @see TopicConnectionFactory_getLoadBalancing()
 */
int SMQ_API TopicConnectionFactory_setLoadBalancing(HOBJ thisObj, jboolean loadBalancing);

/**
 * Specifies whether to start with the first broker in the list or a random element.
 *
 * @param thisObj the TopicConnectionFactory object to operate against.
 * @param sequential   If true, starts attempting to connect to the first broker
 *                     in the list; if false, starts attempting to connect to a random element in 
 *                     the list. 
 *                     After that, tries to connect to each broker in sequence 
 *                     until a successful connect occurs, or the list is exhausted.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @see TopicConnectionFactory_getSequential()
 * @see TopicConnectionFactory_setConnectionURLs()
 * @see TopicConnectionFactory_getConnectionURLs()
 */
int SMQ_API TopicConnectionFactory_setSequential(HOBJ thisObj, jboolean sequential);

/**
 * Enables or disables Nagle's algorithm for created TCP connections.
 *
 * @param thisObj the TopicConnectionFactory object to operate against.
 * @param noDelay   If true, created connections have Nagle's algorithm disabled.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API TopicConnectionFactory_setTcpNoDelay(HOBJ thisObj, jboolean noDelay);

/** 
 * Sets the default batch size of all sessions created via connections
 * created from this connection factory. A session's batch size specifies
 * the amount of information to be batched on the client.  This is a
 * performance optimization utilized on transacted sessions.  The size
 * parameter is a measure of the message payloads only.
 *
 * @param thisObj the TopicConnectionFactory object to operate against.
 * @param size  The number of bytes to batch, specified as a jint.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API TopicConnectionFactory_setDefaultTxnBatchSize(HOBJ thisObj, jint size);

/** 
 * Sets the time, in milliseconds, that C-Client will wait for a connection before aborting.
 * Default value is zero, which tells C-Client to make an untimed connect call.
 *
 * @param thisObj the QueueConnectionFactory object to operate against.
 * @param timeout The number of milliseconds to wait for a connection to be established.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API TopicConnectionFactory_setTCPConnectionTimeout(HOBJ thisObj, jint timeout);

/** Set the administrative flow control monitoring interval.
 *
 * @param thisObj the TopicConnectionFactory object to operate against.
 * @param interval The interval in seconds over which administrative monitoring of flow control will occur.
 *                 A value of 0 (zero) indicates that no monitoring will occur.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @see    getMonitorInterval()
 */
int SMQ_API TopicConnectionFactory_setMonitorInterval(HOBJ thisObj, jint interval);


/** Enable or disable fault tolerant connection creation.
 *<P>
 * By default, connections are created non-fault tolerant(false). For a connection to
 * be created fault-tolerant, fault-tolerant must be set in the ConnectionFactory,
 * and, the broker must support(be licensed for) fault-tolerance.
 *
 * @param faultTolerant    true indicates the client wishes to create fault-tolerant
 * connections
 * @see    getFaultTolerant()
 * @see    progress.message.jclient.Connection#isFaultTolerant()
 * @see    setFaultTolerantReconnectTimeout(Integer)
 * @see    getFaultTolerantReconnectTimeout()
 * @see    setInitialConnectTimeout(Integer)
 * @see    getInitialConnectTimeout()
 * @see    setClientTransactionBufferSize(Long)
 * @see    getClientTransactionBufferSize()
 */
int SMQ_API TopicConnectionFactory_setFaultTolerant(HOBJ thisObj, jboolean faultTolerant);
  

/** Set fault tolerance reconnect timeout.
 * <P>
 * Indicates how long the client runtime will try to establish a physical network connection for the purpose of
 * connection re-establishment after failure. Applicable to fault-tolerant connections only.
 * <P>
 * For reconnects, attempts are made to the previously connected broker URL and to redundant broker URLs
 * reported by the previously connected broker. Redundant broker URLs may be on the last connected
 * broker(if redundant network interfaces are available), or, on a standby broker. URLs are redundantly
 * associated if they have the same Directory Service acceptor name. The broker dynamically supplies the
 * client runtime with redundant URL information(at connect time and on configuration change). Redundant
 * URL information can be obtained by the progress.message.jclient.Connection methods getBrokerReconnectURLs()
 * and getBrokerStandbyReconnectURLs().
 * <P>
 * Default 60 seconds. 0 means no timeout; the runtime will try indefinitely, or, until the broker reports that
 * it can no longer support the fault-tolerant client. A broker can limit the amount of time it maintains
 * state on behalf of a failed fault-tolerant client that has not reconnected by configuration of the
 * advanced broker connections parameter "Client Reconnect Timeout".
 * <P>
 * For reconnects after connection failure, if the connection cannot be reconnected within the allocated time,
 * an exception will be returned to the ExceptionListener(if configured), and all pending connection operations
 * will fail. This is the usual failed connection behavior.
 * <P>
 * @param seconds    maximum time in seconds to attempt reconnection of a failed
 *                   fault-tolerant connection
 * @see    getFaultTolerantReconnectTimeout()
 * @see    setFaultTolerant(Boolean)
 * @see    getFaultTolerant()
 * @see    progress.message.jclient.Connection#getBrokerReconnectURLs()
 * @see    progress.message.jclient.Connection#getStandbyBrokerReconnectURLs()
 * @see    setInitialConnectTimeout(Integer)
 * @see    getInitialConnectTimeout()
 */
  
int SMQ_API TopicConnectionFactory_setFaultTolerantReconnectTimeout(HOBJ thisObj, jint seconds);
  
/** Set initial connect timeout for a fault-tolerant connection.
 * <P>
 * Indicates how long the client runtime will try to establish a physical network connection for the purpose of
 * initial connection establishment. Applicable to fault-tolerant connections only.
 * <P>
 * For initial connections, attempts are made against all URLs listed in the ConnectionFactory. URLs
 * are attempted sequentially. The starting URL is normally the first in the list but can be randomly
 * selected.
 * <P>
 * Default 30.
 * 0 means no timeout; the runtime will try indefinitely.
 * -1 means each URL is attempted one time only; the runtime will try each URL sequentially one at a time
 * until a successful connection is made, or until all URLs have been tried.
 * <P>
 * For initial connection establishment, if connection cannot be made within the allocated time the
 * ConnectionFactory create method throws a JMSException.
 *
 * @param seconds    maximum time in seconds to attempt initial connection of a
 *                   fault-tolerant connection
 * @see    getInitialConnectTimeout()
 * @see    setFaultTolerant(Boolean)
 * @see    getFaultTolerant()
 * @see    setFaultTolerantReconnectTimeout(Integer)
 * @see    getFaultTolerantReconnectTimeout()
 */
int SMQ_API TopicConnectionFactory_setInitialConnectTimeout(HOBJ thisObj, jint seconds);


/** Set client transaction buffer size.
 * Indicates the maximum size of messages in bytes the client runtime is willing to buffer
 * per transaction to support transactions over fault tolerant connections.
 * <P>
 * Transacted message remain in the client runtime until saved or replicated by the broker.
 * JMS client threads sending transacted messages will block if the buffer size is reached,
 * and resume when the broker saves or replicates more messages.
 * A larger buffer size should yield better performance at the expense of more client memory
 * and longer resend time during fault-tolerant reconnect.
 * <P>
 * The default setting (0) indicates that the client runtime must be able to buffer up
 * to the broker Transactions Buffer Size parameter per transaction
 * <P>
 * @param size client transaction buffer size in bytes.
 * @see    getClientTransactionBufferSize()
 * @see    setFaultTolerant(Boolean)
 * @see    getFaultTolerant()
 */
int SMQ_API TopicConnectionFactory_setClientTransactionBufferSize(HOBJ thisObj, jlong size );
 
 /**
 * Create a new TopicConnectionFactory.
 *
 * @param brokerURL a String containing the broker URL.
 * @param pret will contain the newly created TopicConnectionFactory
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API TopicConnectionFactory_create(HOBJ brokerURL, HOBJ *pret);

/**
 * Create a new TopicConnectionFactory.
 *
 * @param brokerHostName a String containing the broker hostname.
 * @param brokerPort the broker port number.
 * @param brokerProtocol a String containing the broker protocol.
 * @param defaultUserName a String containing the default username.
 * @param defaultPassword a String containing the default password.
 * @param pret will contain the newly created TopicConnectionFactory
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API TopicConnectionFactory_create2(
	HOBJ brokerHostName, 
	jint brokerPort,
	HOBJ brokerProtocol, 
	HOBJ defaultUserName,
	HOBJ defaultPassword, 
	HOBJ *pret);

/**
 * Create a new TopicConnectionFactory.
 *
 * @param brokerHostName a String containing the broker hostname.
 * @param brokerPort the broker port number.
 * @param brokerProtocol a String containing the broker protocol.
 * @param connectID a String containing the connection ID.
 * @param defaultUserName a String containing the default username.
 * @param defaultPassword a String containing the default password.
 * @param pret will contain the newly created TopicConnectionFactory
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API TopicConnectionFactory_create3(
	HOBJ brokerHostName, 
	jint brokerPort,
	HOBJ brokerProtocol, 
	HOBJ connectID, 
	HOBJ defaultUserName,
	HOBJ defaultPassword,
	HOBJ *pret);

/**
 * Create a new TopicConnectionFactory.
 *
 * @param brokerURL a String containing the broker URL.
 * @param connectID a String containing the connection ID.
 * @param pret will contain the newly created TopicConnectionFactory
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API TopicConnectionFactory_create4(
	HOBJ brokerURL, 
	HOBJ connectID, 
	HOBJ *pret);

/**
 * Create a new TopicConnectionFactory.
 *
 * @param brokerURL a String containing the broker URL.
 * @param defaultUserName a String containing the default username.
 * @param defaultPassword a String containing the default password.
 * @param pret will contain the newly created TopicConnectionFactory
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API TopicConnectionFactory_create5(
	HOBJ brokerURL, 
	HOBJ defaultUserName,
	HOBJ defaultPassword,
	HOBJ *pret);

/**
 * Create a new TopicConnectionFactory.
 *
 * @param brokerURL a String containing the broker URL.
 * @param connectID a String containing the connection ID.
 * @param defaultUserName a String containing the default username.
 * @param defaultPassword a String containing the default password.
 * @param pret will contain the newly created TopicConnectionFactory
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API TopicConnectionFactory_create6(
	HOBJ brokerURL, 
	HOBJ connectID,
	HOBJ defaultUserName, 
	HOBJ defaultPassword,
	HOBJ *pret);

/*
//---------------------------------
// TopicPublisher
//---------------------------------
*/
/**
 * Decrements the reference count of the TopicPublisher object.
 *
 * @param topicPub the TopicPublisher object to operate against.
 * @param pret will contain the reference count after decrementing.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define TopicPublisher_release(topicPub, pret)                     Object_release(topicPub, pret)

/**
 * Get the int corresponding to the type of a given object.
 *
 * @param thisObj the object to operate against.
 * @param pret will contain the int corresponding to the type.
 */
#define TopicPublisher_getType(thisObj, pret)                     Object_getType(thisObj, pret)

/**
 * Determines whether the given object is an instance of the given type.
 *
 * @param thisObj the object to check.
 * @param classtype the type to check against.
 * @param pret non-zero if the object is an instance of the type.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define TopicPublisher_instanceof(thisObj, classtype, pret)       Object_instanceof(thisObj, classtype, pret)

/**
 * Determines whether the two TopicPublisher objects are the same.
 *
 * @param topicPub the first TopicPublisher to compare.
 * @param hobj2 the second TopicPublisher to compare.
 * @param pret true if the TopicPublisher objects have identical content, false otherwise.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define TopicPublisher_equals(topicPub, hobj2, pret)               Object_equals(topicPub, hobj2, pret)

/**
 * Get the String representation for the given TopicPublisher.
 *
 * @param topicPub the TopicPublisher object to operate against.
 * @param pret will contain the String that represents the TopicPublisher.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define TopicPublisher_toString(topicPub, pret)                    Object_toString(topicPub, pret)

/** Set whether message IDs are disabled.
  *  
  * <P>Since message ID's take some effort to create and increase a
  * message's size, SonicMQ can optimize message
  * overhead if they are given a hint that message ID is not used by
  * an application. SonicMQ message Producers provide a hint to disable
  * message ID. When a client sets a Producer to disable message ID
  * they are saying that they do not depend on the value of message
  * ID for the messages it produces. These messages must either have
  * message ID set to null or, if the hint is ignored, messageID must
  * be set to its normal unique value.
  *
  * <P>Message IDs are enabled by default.
  *
  * @param topicPub the TopicPublisher to operate against
  * @param value indicates if message IDs are disabled.
  *  
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  */ 
#define TopicPublisher_setDisableMessageID(topicPub, value)        MessageProducer_setDisableMessageID(topicPub, value)

/** Get an indication of whether message IDs are disabled.
  *  
  * @param topicPub the TopicPublisher to operate against
  * @param pDisabled an indication of whether message IDs are disabled.
  *  
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  */ 
#define TopicPublisher_getDisableMessageID(topicPub, pDisabled)    MessageProducer_getDisableMessageID(topicPub, pDisabled)

/** Set whether message timestamps are disabled.
  *  
  * <P>Since timestamps take some effort to create and increase a 
  * message's size, SonicMQ can optimize message 
  * overhead if they are given a hint that timestamp is not used by an 
  * application. SonicMQ message Producers provide a hint to disable 
  * timestamps. When a client sets a producer to disable timestamps 
  * they are saying that they do not depend on the value of timestamp 
  * for the messages it produces. These messages must either have 
  * timestamp set to null or, if the hint is ignored, timestamp must 
  * be set to its normal value.
  *  
  * <P>Message timestamps are enabled by default.
  *
  * @param topicPub the TopicPublisher to operate against
  * @param value indicates if message timestamps are disabled.
  *  
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  */ 
#define TopicPublisher_setDisableMessageTimestamp(topicPub, value) MessageProducer_setDisableMessageTimestamp(topicPub, value)

/** Get an indication of whether message timestamps are disabled.
  *  
  * @param topicPub the TopicPublisher to operate against
  * @param pDisabled an indication of whether message IDs are disabled.
  *  
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  */
#define TopicPublisher_getDisableMessageTimestamp(topicPub, pDisabled) MessageProducer_getDisableMessageTimestamp(topicPub, pDisabled)

/** Set the producer's default delivery mode.
  *  
  * <P>Delivery mode is set to PERSISTENT by default.
  *
  * @param topicPub the TopicPublisher to operate against
  * @param deliveryMode the message delivery mode for this TopicPublisher.
  *  
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate
  *         error details.
  */
#define TopicPublisher_setDeliveryMode(topicPub, deliveryMode)     MessageProducer_setDeliveryMode(topicPub, deliveryMode)

/** Get the producer's default delivery mode.
  *  
  * @param topicPub the TopicPublisher to operate against
  * @param pDelMode the message delivery mode for this TopicPublisher.
  *  
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  *
  * @see TopicPublisher_setDeliveryMode()
  */ 
#define TopicPublisher_getDeliveryMode(topicPub, pDelMode)         MessageProducer_getDeliveryMode(topicPub, pDelMode)

/** Set the producer's default priority.
  *  
  * <P>Priority is set to 4, by default.
  *
  * @param topicPub the TopicPublisher to operate against
  * @param value the message priority for this TopicPublisher.
  *  
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  *
  * @see TopicPublisher_getPriority()
  */ 
#define TopicPublisher_setPriority(topicPub, value)                MessageProducer_setPriority(topicPub, value)

/** Get the producer's default priority.
  *  
  * @param topicPub the TopicPublisher to operate against
  * @param pPriority the message priority for this TopicPublisher.
  *  
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  *
  * @see TopicPublisher_setPriority()
  */ 
#define TopicPublisher_getPriority(topicPub, pPriority)            MessageProducer_getPriority(topicPub, pPriority)

/** Set the default length of time in milliseconds from its dispatch time
  * that a produced message should be retained by the message system.
  *
  * <P>Time to live is set to zero by default.
  *
  * @param topicPub the TopicPublisher to operate against
  * @param timeToLive the message time to live in milliseconds; zero is
  * unlimited
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  *
  * @see TopicPublisher_getTimeToLive()
  */
#define TopicPublisher_setTimeToLive(topicPub, value)              MessageProducer_setTimeToLive(topicPub, value)

/** Get the default length of time in milliseconds from its dispatch time
  * that a produced message should be retained by the message system.
  *
  * @param topicPub the TopicPublisher to operate against
  * @param pTTL the message time to live in milliseconds; zero is unlimited
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  *
  * @see TopicPublisher_setTimeToLive()
  */ 
#define TopicPublisher_getTimeToLive(topicPub, pTTL)               MessageProducer_getTimeToLive(topicPub, pTTL)

/** Since SonicMQ allocates some resources on behalf of a
  * TopicPublisher, clients should close them when they
  * are not needed.
  *  
  * @param topicPub the TopicPublisher to operate against
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  */ 
#define TopicPublisher_close(topicPub)                             MessageProducer_close(topicPub)

/**
 * Returns the int corresponding to the TopicPublisher type.
 *
 * @return the int corresponding to the TopicPublisher type
 */
int SMQ_API TopicPublisher_type();

/** Get the topic associated with this publisher.
  *
  * @param topicPub the TopicPublisher to operate against
  * @param pTopic this publisher's Topic.  
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate
  *         error details.
  */
int SMQ_API TopicPublisher_getTopic(HOBJ topicPub, HOBJ *pTopic);

/** Publish a Message to the topic
  * Uses the topics default delivery mode, timeToLive and priority. 
  *
  * @param topicPub the TopicPublisher to operate against
  * @param message the message to publish
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Likely causes of errors:
  *           MessageFormatException if an invalid message is specified.
  *           InvalidDestinationException if invalid topic.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate
  *         error details.
  */
int SMQ_API TopicPublisher_publish(HOBJ topicPub, HOBJ msg);

/** Publish a Message to the topic specifying delivery mode, priority 
  * and time to live.
  *
  * @param topicPub the TopicPublisher to operate against
  * @param message the message to publish
  * @param deliveryMode the delivery mode to use
  * @param priority the priority for this message
  * @param timeToLive the message's lifetime (in milliseconds).
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Likely causes of errors:
  *           MessageFormatException if invalid message specified
  *           IllegalStateException if DISCARDABLE is set on a transacted session.
  *           InvalidDestinationException if invalid topic.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate
  *         error details.
  */
int SMQ_API TopicPublisher_publish2(HOBJ topicPub, HOBJ msg, jint deliverymode, jint priority, jlong timeToLive);

/** Publish a Message to a topic for an unidentified message producer.	
  * Use the topics default delivery mode, timeToLive and priority.
  *  
  * <P>Typically a SonicMQ message producer is assigned a topic at creation 
  * time; however, SonicMQ also supports unidentified message producers 
  * which require that the topic be supplied on every message publish.
  *
  * @param topicPub the TopicPublisher to operate against
  * @param topic the topic to publish this message to
  * @param message the message to send
  *  
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Likely causes of errors include:
  *           MessageFormatException if invalid message specified
  *           InvalidDestinationException if a client uses this method
  *             with an invalid topic.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate
  *         error details.
  */ 
int SMQ_API TopicPublisher_publish3(HOBJ topicPub, HOBJ topic, HOBJ msg);

/** Publish a Message to a topic for an unidentified message producer,
  * specifying delivery mode, priority and time to live.
  *  
  * <P>Typically a SonicMQ message producer is assigned a topic at creation
  * time; however, SonicMQ also supports unidentified message producers
  * which require that the topic be supplied on every message publish.
  *
  * @param topicPub the TopicPublisher to operate against
  * @param topic the topic to publish this message to
  * @param message the message to send
  * @param deliveryMode the delivery mode to use
  * @param priority the priority for this message
  * @param timeToLive the message's lifetime (in milliseconds).
  *  
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Likely causes of errors:
  *           MessageFormatException if invalid message specified
  *           IllegalStateException if DISCARDABLE is set on a transacted session.
  *           InvalidDestinationException if invalid topic.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate
  *         error details.
  */ 
int SMQ_API TopicPublisher_publish4(HOBJ thisObj, HOBJ topic, HOBJ msg, jint deliverymode, jint priority, jlong timeToLive);

/*
//---------------------------------
// TopicSession
//---------------------------------
*/
/**
 * Decrements the reference count of the TopicSession object.
 *
 * @param topicSsn the TopicSession object to operate against.
 * @param pret will contain the reference count after decrementing.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define TopicSession_release(topicSsn, pret)                  Object_release(topicSsn, pret)

/**
 * Get the int corresponding to the type of a given object.
 *
 * @param thisObj the object to operate against.
 * @param pret will contain the int corresponding to the type.
 */
#define TopicSession_getType(thisObj, pret)                  Object_getType(thisObj, pret)

/**
 * Determines whether the given object is an instance of the given type.
 *
 * @param thisObj the object to check.
 * @param classtype the type to check against.
 * @param pret non-zero if the object is an instance of the type.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define TopicSession_instanceof(thisObj, classtype, pret)    Object_instanceof(thisObj, classtype, pret)

/**
 * Determines whether the two TopicSession objects are the same.
 *
 * @param topicSsn the first TopicSession to compare.
 * @param hobj2 the second TopicSession to compare.
 * @param pret true if the TopicSession objects have identical content, false otherwise.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define TopicSession_equals(topicSsn, hobj2, pret)            Object_equals(topicSsn, hobj2, pret)

/**
 * Get the String representation for the given TopicSession.
 *
 * @param topicSsn the TopicSession object to operate against.
 * @param pret will contain the String that represents the TopicSession.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define TopicSession_toString(topicSsn, pret)                 Object_toString(topicSsn, pret)

/**
 * Create a Message. The Message interface is the root interface of
 * all JMS messages. It holds all the standard message header
 * information. It can be sent when a message containing only header
 * information is sufficient.
 *
 * @param topicSsn the TopicSession object to operate against.
 * @param pret will contain the newly created Message
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to create this message
 *                         due to some internal error.
 */
#define TopicSession_createMessage(topicSsn, pret)            Session_createMessage(topicSsn, pret)

/**
 * Create a BytesMessage. A BytesMessage is used to send a message
 * containing a stream of uninterpreted bytes.
 *
 * @param topicSsn the TopicSession object to operate against.
 * @param pret will contain the newly created Message
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to create this message
 *                         due to some internal error.
 */
#define TopicSession_createBytesMessage(topicSsn, pret)       Session_createBytesMessage(topicSsn, pret)

/**
 * Create a BytesMessage. A BytesMessage is used to send a message
 * containing a stream of uninterpreted bytes.
 *
 * @param thisObj the TopicSession object to operate against.
 * @param pret will contain the newly created Message
 * @param preAllocatdSize The preallocated size of the bytes message
 * @param growthSize The size by with the bytes message will grow once the
 * previous buffer size is filled up
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to create this message
 *                         due to some internal error.
 */
#define TopicSession_createBytesMessage2(thisObj, preAllocatedSize, growthSize, pret)       Session_createBytesMessage2(thisObj, preAllocatedSize, growthSize, pret)

/**
 * Create a TextMessage. A TextMessage is used to send a message
 * containing a String.
 *
 * @param topicSsn the TopicSession object to operate against.
 * @param pret will contain the newly created Message
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to create this message
 *                         due to some internal error.
 */
#define TopicSession_createTextMessage(topicSsn, pret)        Session_createTextMessage(topicSsn, pret)

/**
 * Create an initialized TextMessage. A TextMessage is used to send
 * a message containing a String.
 *
 * @param topicSsn the TopicSession object to operate against.
 * @param text a String containing the text used to initialize this message.
 * @param pret will contain the newly created Message
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to create this message
 *                         due to some internal error.
 */
#define TopicSession_createTextMessage2(topicSsn, text, pret) Session_createTextMessage2(topicSsn, text, pret)

/**
 * Is the session in transacted mode?
 *
 * @param topicSsn the TopicSession object to operate against.
 * @param pret true if in transacted mode
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS fails to return the transaction
 *                         mode due to internal error in JMS Provider.
 */
#define TopicSession_getTransacted(topicSsn, pret)            Session_getTransacted(topicSsn, pret)

/**
 * Commit all messages done in this transaction and release any locks
 * currently held.
 *
 * @param topicSsn the TopicSession object to operate against.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS implementation fails to commit the
 *                         the transaction due to some internal error.
 * @exception TransactionRolledBackException  if the transaction
 *                         gets rolled back due to some internal error
 *                         during commit.
 * @exception IllegalStateException if commit() is called on a 
 *                         non-transacted session
 */
#define TopicSession_commit(topicSsn)                           Session_commit(topicSsn)

/**
 * Rollback any messages done in this transaction and releases any locks
 * currently held.
 *
 * @param topicSsn the TopicSession object to operate against.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS implementation fails to rollback the
 *                         the transaction due to some internal error.
 * @exception IllegalStateException if rollback() called in a non-transacted
 *                         session
 */
#define TopicSession_rollback(topicSsn)                       Session_rollback(topicSsn)

/**
 * Close the TopicSession. Clients should close sessions when they are not needed.
 * Closes all subscribers and publishers active in this session.
 *
 * @param topicSsn the TopicSession object to operate against.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS implementation fails to close a
 *                         Session due to some internal error.
 */
#define TopicSession_close(topicSsn)                          Session_close(topicSsn)

/**
 * Stop message delivery in this session, and restart sending messages
 * with the oldest unacknowledged message.
 *
 * <p>All consumers deliver messages in a serial order.
 * Acknowledging a received message automatically acknowledges all
 * messages that have been delivered to the client in the current session.
 *
 * <p>Restarting a session causes it to take the following actions:
 *
 * <ul>
 *   <li>Stop message delivery
 *   <li>Mark all messages that might have been delivered but not
 *       acknowledged as `redelivered'
 *   <li>Restart the delivery sequence including all unacknowledged
 *       messages that had been previously delivered.
 *
 *     <p>Redelivered messages do not have to be delivered in
 *        exactly their original delivery order.
 * </ul>
 *
 * @param topicSsn the TopicSession object to operate against.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 *
 * @exception JMSException if JMS implementation fails to stop message
 *                         delivery and restart message send due to
 *                         due to some internal error.
 * @exception IllegalStateException if recover() is called in a transacted
 *                         session
 */
#define TopicSession_recover(topicSsn)                        Session_recover(topicSsn)

/** 
 * Gets the transaction batch size in bytes of this session.
 * A session's batch size specifies the amount of information 
 * to be batched on the client.  This is a performance optimization 
 * utilized on transacted sessions.  The size parameter is a measure 
 * of the message payloads only.
 *
 * @param thisObj the TopicSession object to operate against.
 * @param pret Points to the number of bytes to batch, specified as a jint.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define TopicSession_getTxnBatchSize(thisObj, pret)          Session_getTxnBatchSize(thisObj, pret)

/** 
 * Sets the transaction batch size in bytes of this session.
 * A session's batch size specifies the amount of information 
 * to be batched on the client.  This is a performance optimization 
 * utilized on transacted sessions.  The size parameter is a measure 
 * of the message payloads only.
 *
 * @param thisObj the TopicSession object to operate against.
 * @param batchSize The number of bytes to batch, specified as a jint.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define TopicSession_setTxnBatchSize(thisObj, batchSize)     Session_setTxnBatchSize(thisObj, batchSize)

/** Create a Topic given a Topic name.
  *
  * <P>This facility is provided for the rare cases where clients need to
  * dynamically manipulate topic identity. This allows the creation of a
  * topic with a specific name. Clients that depend on this
  * ability are not portable.
  *
  * @param topicSsn the TopicSession to operate against
  * @param topicName the name of this topic
  *
  * @param pTopic pointer to the new Topic
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate
  *         error details.
  */
int SMQ_API TopicSession_createTopic(HOBJ topicSsn, HOBJ topicName, HOBJ* pTopic);

/** Create a non-durable Subscriber to the specified topic.
  *  
  * <P>A client uses a TopicSubscriber for receiving messages that have 
  * been published to a topic.
  *
  * <P>Regular TopicSubscriber's are not durable. They only receive 
  * messages that are published while they are active.
  *
  *
  * @param topicSsn the TopicSession to operate against
  * @param topic the topic to subscribe to
  * @param pTopicSubscriber pointer to the new TopicSubscriber
  *  
  * @return SMQ_SUCCESS (0) if successful, non-zero if error (e.g.,
  *         InvalidDestinationException due to invalid topic name).
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate
  *         error details.
  */ 
int SMQ_API TopicSession_createSubscriber(HOBJ topicSsn, HOBJ topic, HOBJ* pTopicSubscriber);

/** Create a non-durable Subscriber to the specified topic.
  *
  * <P>A client uses a TopicSubscriber for receiving messages that have 
  * been published to a topic.
  *  
  * <P>Regular TopicSubscriber's are not durable. They only receive 
  * messages that are published while they are active.
  *
  * <P>Messages filtered out by a subscriber's message selector will 
  * never be delivered to the subscriber. From the subscriber's 
  * perspective they simply don't exist.
  *
  * <P>In some cases, a connection may both publish and subscribe to a 
  * topic. The subscriber NoLocal attribute allows a subscriber to 
  * inhibit the delivery of messages published by its own connection.
  *
  * @param topicSsn the TopicSession to operate against
  * @param topic the topic to subscribe to
  * @param messageSelector only messages with properties matching the
  * message selector expression are delivered. This value may be null.
  * @param noLocal if set, inhibits the delivery of messages published
  * by its own connection.
  * @param pTopicSubscriber the new TopicSubscriber
  * 
  * @return SMQ_SUCCESS (0) if successful, non-zero if error (e.g.,
  *         invalid topic name or invalid selector).
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate
  *         error details.
  */
int SMQ_API TopicSession_createSubscriber2(HOBJ topicSsn,HOBJ topic,HOBJ messageSelector,jboolean noLocal, HOBJ* pTopicSubscriber);

/** Create a durable Subscriber to the specified topic.
  *  
  * <P>If a client needs to receive all the messages published on a 
  * topic including the ones published while the subscriber is inactive,
  * it uses a durable TopicSubscriber. JMS retains a record of this 
  * durable subscription and insures that all messages from the topic's 
  * publishers are retained until they are either acknowledged by this 
  * durable subscriber or they have expired.
  *
  * <P>Sessions with durable subscribers must always provide the same
  * client identifier. In addition, each client must specify a name which
  * uniquely identifies (within client identifier) each durable
  * subscription it creates. Only one session at a time can have a
  * TopicSubscriber for a particular durable subscription.
  *  
  * <P>A client can change an existing durable subscription by creating 
  * a durable TopicSubscriber with the same name and a new topic and/or 
  * message selector. Changing a durable subscriber is equivalent to 
  * deleting and recreating it
  *
  * @param topicSsn the TopicSession to operate against
  * @param topic the topic to subscribe to
  * @param name the name used to identify this subscription.
  * @param messageSelector only messages with properties matching the message 
           selector expression are delivered. This value may be null.
  * @param noLocal if set, inhibits the delivery of messages published
  *        by its own connection.
  * @param ttl time in milliseconds that a DurableSubscriber
  *        will live for after it has disconnected.
  *        NOTE: A ttl value <= 0 will result in NOT setting a disconnected timeToLive.
  * @param pTopicSubscriber pointer to the new TopicSubscriber 
  *
  * @exception JMSException if a session fails to create a subscriber
  *                         due to some JMS error or invalid selector.
  * @exception javax::jms::InvalidDestinationException if invalid Topic specified.
  * @exception javax::jms::InvalidSelectorException if the message selector is invalid.
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error (e.g.,
  *         invalid topic name).
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate
  *         error details.
  */ 
int SMQ_API TopicSession_createDurableSubscriber4(HOBJ topicSsn,HOBJ topic, HOBJ name,HOBJ messageSelector, jboolean noLocal, jlong ttl, HOBJ* pTopicSubscriber);

/** Create a durable Subscriber to the specified topic.
  *  
  * <P>If a client needs to receive all the messages published on a 
  * topic including the ones published while the subscriber is inactive,
  * it uses a durable TopicSubscriber. The SonicMQ broker retains a record of this 
  * durable subscription and insures that all messages from the topic's 
  * publishers are retained until they are either acknowledged by this 
  * durable subscriber or they have expired.
  *
  * <P>Sessions with durable subscribers must always provide the same 
  * client identifier. In addition, each client must specify a name which 
  * uniquely identifies (within client identifier) each durable 
  * subscription it creates. Only one session at a time can have a 
  * TopicSubscriber for a particular durable subscription.
  *
  * <P>A client can change an existing durable subscription by creating 
  * a durable TopicSubscriber with the same name and a new topic and/or 
  * message selector. Changing a durable subscriber is equivalent to 
  * deleting and recreating it
  *
  * @param topicSsn the TopicSession to operate against
  * @param topic the topic to subscribe to
  * @param name the name used to identify this subscription.
  * @param pTopicSubscriber pointer to the new TopicSubscriber 
  *  
  * @return SMQ_SUCCESS (0) if successful, non-zero if error (e.g.,
  *         invalid topic name).
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate
  *         error details.
  */ 
int SMQ_API TopicSession_createDurableSubscriber(HOBJ topicSsn, HOBJ topic, HOBJ name, HOBJ *pTopicSubscriber);

/** Create a durable Subscriber to the specified topic.
  *  
  * <P>If a client needs to receive all the messages published on a 
  * topic including the ones published while the subscriber is inactive,
  * it uses a durable TopicSubscriber. The SonicMQ broker retains a record of this 
  * durable subscription and insures that all messages from the topic's 
  * publishers are retained until they are either acknowledged by this 
  * durable subscriber or they have expired.
  *
  * <P>Sessions with durable subscribers must always provide the same
  * client identifier. In addition, each client must specify a name which
  * uniquely identifies (within client identifier) each durable
  * subscription it creates. Only one session at a time can have a
  * TopicSubscriber for a particular durable subscription.
  *  
  * <P>A client can change an existing durable subscription by creating 
  * a durable TopicSubscriber with the same name and a new topic and/or 
  * message selector. Changing a durable subscriber is equivalent to 
  * deleting and recreating it
  *
  * @param topicSsn the TopicSession to operate against
  * @param topic the topic to subscribe to
  * @param name the name used to identify this subscription.
  * @param messageSelector only messages with properties matching the
  * message selector expression are delivered. This value may be null.
  * @param noLocal if set, inhibits the delivery of messages published
  * by its own connection.
  * @param noLoc if set, inhibits the delivery of messages published
  * by its own connection.
  * @param pTopicSubscriber pointer to the new TopicSubscriber
  *  
  * @return SMQ_SUCCESS (0) if successful, non-zero if error (e.g.,
  *         invalid topic name or invalid selector).
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate
  *         error details.
  */ 
int SMQ_API TopicSession_createDurableSubscriber2(HOBJ topicSsn, HOBJ topic, HOBJ name, HOBJ messageSelector, jboolean noLocal, HOBJ *pTopicSubscriber);

/** Create a durable Subscriber to the specified topic.
  *  
  * <P>If a client needs to receive all the messages published on a 
  * topic including the ones published while the subscriber is inactive,
  * it uses a durable TopicSubscriber. The SonicMQ broker retains a record of this 
  * durable subscription and insures that all messages from the topic's 
  * publishers are retained until they are either acknowledged by this 
  * durable subscriber or they have expired.
  *
  * <P>Sessions with durable subscribers must always provide the same 
  * client identifier. In addition, each client must specify a name which 
  * uniquely identifies (within client identifier) each durable 
  * subscription it creates. Only one session at a time can have a 
  * TopicSubscriber for a particular durable subscription.
  *
  * <P>A client can change an existing durable subscription by creating 
  * a durable TopicSubscriber with the same name and a new topic and/or 
  * message selector. Changing a durable subscriber is equivalent to 
  * deleting and recreating it
  *
  * @param topicSsn the TopicSession to operate against
  * @param topic the topic to subscribe to
  * @param name the name used to identify this subscription.
  * @param pTopicSubscriber pointer to the new TopicSubscriber 
  * @param ttl time in milliseconds that a DurableSubscriber
  *                   will live for after it has disconnected.
  * NOTE: A timeToLive value <= 0 will result in NOT setting a disconnected timeToLive.
  * 
  * @return SMQ_SUCCESS (0) if successful, non-zero if error (e.g.,
  *         invalid topic name).
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate
  *         error details.
  */ 
int SMQ_API TopicSession_createDurableSubscriber3(HOBJ topicSsn, HOBJ topic, HOBJ name, HOBJ *pTopicSubscriber, jlong ttl);

/** Create a Publisher for the specified topic.
  *
  * <P>A client uses a TopicPublisher for publishing messages on a topic.
  * Each time a client creates a TopicPublisher on a topic, it defines a 
  * new sequence of messages that have no ordering relationship with the 
  * messages it has previously sent.
  *
  * @param topicSsn the TopicSession to operate against
  * @param topic the topic to publish to, or null if this is an 
  * unidentifed producer.
  * @param pTopicPublisher pointer to the new TopicPublisher
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error (e.g.,
  *         invalid topic name).
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate
  *         error details.
  */
int SMQ_API TopicSession_createPublisher(HOBJ topicSsn, HOBJ topic, HOBJ* pTopicPublisher);

/** Create a temporary topic. Its lifetime is that of the 
  * TopicConnection.
  *
  * @param topicSsn the TopicSession to operate against
  * @param pTemporaryTopic pointer to the new temporary topic.
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate
  *         error details.
  */
int SMQ_API TopicSession_createTemporaryTopic(HOBJ topicSsn, HOBJ* pTemporaryTopic);

/** Unsubscribe a durable subscription that has been created by a client.
  *  
  * <P>This deletes the state being maintained on behalf of the 
  * subscriber by its SonicMQ broker.
  *
  * @param topicSsn the TopicSession to operate against
  * @param name the name used to identify this subscription.
  *  
  * @return SMQ_SUCCESS (0) if successful, non-zero if error (e.g., invalid topic).
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate
  *         error details.
  *                          
  */
int SMQ_API TopicSession_unsubscribe(HOBJ topicSsn, HOBJ name);

/**
 * Disables or reenables publish flow control.  If flow control is disabled
 * an exception will be thrown when the client enters a flow control condition.
 * In the C API this will cause publish on the topic to fail.
 *
 * @param topicSsn the TopicSession to operate against
 * @param disabled true to disable flow control
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error (e.g., invalid topic).
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate
 *         error details.
 */
int SMQ_API TopicSession_setFlowControlDisabled(HOBJ topicSsn, jboolean disabled);

/**
 * Returns the int corresponding to the TopicSession type.
 *
 * @return the int corresponding to the TopicSession type
 */
int SMQ_API TopicSession_type();

/**
 * Returns the acknowledgement mode of the session. The acknowledgement
 * mode is set at the time that the session is created. If the session is
 * transacted, the acknowledgement mode is ignored.
 *
 * @param thisObj the Session object to operate against.
 * @param pret    If the session is not transacted, returns the current
 *                acknowledgement mode for the session. If the session
 *                is transacted, returns SESSION_TRANSACTED.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define TopicSession_getAcknowledgeMode(thisObj, pret)  Session_getAcknowledgeMode(thisObj, pret)

/*
//---------------------------------
// ITopicSubscriber
//---------------------------------
*/
/**
 * Decrements the reference count of the ITopicSubscriber object.
 *
 * @param thisObj the ITopicSubscriber object to operate against.
 * @param pret will contain the reference count after decrementing.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define ITopicSubscriber_release(thisObj, pret)                Object_release(thisObj, pret)

/**
 * Get the int corresponding to the type of a given object.
 *
 * @param thisObj the object to operate against.
 * @param pret will contain the int corresponding to the type.
 */
#define ITopicSubscriber_getType(thisObj, pret)                Object_getType(thisObj, pret)

/**
 * Determines whether the given object is an instance of the given type.
 *
 * @param thisObj the object to check.
 * @param classtype the type to check against.
 * @param pret non-zero if the object is an instance of the type.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define ITopicSubscriber_instanceof(thisObj, classtype, pret)  Object_instanceof(thisObj, classtype, pret)

/**
 * Determines whether the two ITopicSubscriber objects are the same.
 *
 * @param thisObj the first ITopicSubscriber to compare.
 * @param hobj2 the second ITopicSubscriber to compare.
 * @param pret true if the ITopicSubscriber objects have identical content, false otherwise.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define ITopicSubscriber_equals(thisObj, hobj2, pret)          Object_equals(thisObj, hobj2, pret)

/**
 * Get the String representation for the given ITopicSubscriber.
 *
 * @param thisObj the ITopicSubscriber object to operate against.
 * @param pret will contain the String that represents the ITopicSubscriber.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define ITopicSubscriber_toString(thisObj, pret)               Object_toString(thisObj, pret)

/** Get this ITopicSubscriber's message selector expression.
  *
  * @param thisObj the ITopicSubscriber to operate against
  * @param pret this ITopicSubscriber's message selector
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  */
#define ITopicSubscriber_getMessageSelector(thisObj, pret)     MessageConsumer_getMessageSelector(thisObj, pret)

/** Get the ITopicSubscriber's MessageListener.
  *
  * @param thisObj the ITopicSubscriber to operate against
  * @param pret the listener for the ITopicSubscriber, or null if this isn't
  * one set.
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  *
  * @see ITopicSubscriber_setMessageListener()
  */
#define ITopicSubscriber_getMessageListener(thisObj, pret)     MessageConsumer_getMessageListener(thisObj, pret)

/** Set the ITopicSubscriber's MessageListener.
  *
  * @param thisObj the ITopicSubscriber to operate against
  * @param listener the messages are delivered to this listener
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  *
  * @see ITopicSubscriber_getMessageListener()
  */
#define ITopicSubscriber_setMessageListener(thisObj, listener) MessageConsumer_setMessageListener(thisObj, listener)

/**
 * Returns the int corresponding to the ITopicSubscriber type.
 *
 * @return the int corresponding to the ITopicSubscriber type
 */
int SMQ_API ITopicSubscriber_type();

/**
 * Close an ITopicSubscriber.
 *
 * @param thisObj the ITopicSubscriber to close.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API ITopicSubscriber_close(HOBJ thisObj);

/**
 * Get the topic this ITopicSubscriber is subscribed to.
 *
 * @param thisObj the ITopicSubscriber to operate against.
 * @param pret the Topic
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API ITopicSubscriber_getTopic(HOBJ thisObj, HOBJ *pret);

/**
 * Get the NoLocal attribute for this ITopicSubscriber.
 *
 * @param thisObj the ITopicSubscriber to operate against.
 * @param pret will contain true if locally published messages are being inhibited.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API ITopicSubscriber_getNoLocal(HOBJ thisObj, jboolean *pret);

/*
//---------------------------------
// TopicSubscriber
//---------------------------------
*/
/**
 * Decrements the reference count of the TopicSubscriber object.
 *
 * @param topicSub the TopicSubscriber object to operate against.
 * @param pret will contain the reference count after decrementing.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define TopicSubscriber_release(topicSub, pret)                Object_release(topicSub, pret)

/**
 * Get the int corresponding to the type of a given object.
 *
 * @param thisObj the object to operate against.
 * @param pret will contain the int corresponding to the type.
 */
#define TopicSubscriber_getType(thisObj, pret)                Object_getType(thisObj, pret)

/**
 * Determines whether the given object is an instance of the given type.
 *
 * @param thisObj the object to check.
 * @param classtype the type to check against.
 * @param pret non-zero if the object is an instance of the type.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define TopicSubscriber_instanceof(thisObj, classtype, pret)  Object_instanceof(thisObj, classtype, pret)

/**
 * Determines whether the two TopicSubscriber objects are the same.
 *
 * @param topicSub the first TopicSubscriber to compare.
 * @param hobj2 the second TopicSubscriber to compare.
 * @param pret true if the TopicSubscriber objects have identical content, false otherwise.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define TopicSubscriber_equals(topicSub, hobj2, pret)          Object_equals(topicSub, hobj2, pret)

/**
 * Get the String representation for the given TopicSubscriber.
 *
 * @param topicSub the TopicSubscriber object to operate against.
 * @param pret will contain the String that represents the TopicSubscriber.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define TopicSubscriber_toString(topicSub, pret)               Object_toString(topicSub, pret)

/** Get this TopicSubscriber's message selector expression.
  *
  * @param topicSub the TopicSubscriber to operate against
  * @param pret this TopicSubscriber's message selector
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  */
#define TopicSubscriber_getMessageSelector(topicSub, pret)     MessageConsumer_getMessageSelector(topicSub, pret)

/** Get the TopicSubscriber's MessageListener.
  *
  * @param topicSub the TopicSubscriber to operate against
  * @param pret the listener for the TopicSubscriber, or null if this isn't
  * one set.
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  *
  * @see TopicSubscriber_setMessageListener()
  */
#define TopicSubscriber_getMessageListener(topicSub, pret)     MessageConsumer_getMessageListener(topicSub, pret)

/** Set the TopicSubscriber's MessageListener.
  *
  * @param topicSub the TopicSubscriber to operate against
  * @param listener the messages are delivered to this listener
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  *
  * @see TopicSubscriber_getMessageListener()
  */
#define TopicSubscriber_setMessageListener(topicSub, listener) MessageConsumer_setMessageListener(topicSub, listener)

/** Receive the next message produced for this message consumer.
  *
  * <P>This call blocks indefinitely until a message is produced.
  *
  * <P>If this receive is done within a transaction, the message
  * remains on the consumer until the transaction commits.
  *
  * @param topicSub the TopicSubscriber to operate against
  * @param pMsg pointer to the next Message produced for this message consumer, 
  *         or null, if the connection closes while waiting
  * @return SMQ_SUCCESS (0) if successful, non-zero if error (e.g., invalid topic).
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate
  *         error details.
  */
#define TopicSubscriber_receive(topicSub, pMsg)                MessageConsumer_receive(topicSub, pMsg)

/** Receive the next message that arrives within the specified
  * timeout interval.
  *
  * <P>This call blocks until either a message arrives or the
  * timeout expires.
  *
  * @param topicSub the TopicSubscriber to operate against
  * @param timeout the timeout value (in milliseconds)
  * @param pMsg pointer to the next Message produced for this message consumer, 
  *         or null, if the connection closes while waiting, or if
  *         the wait times out.
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error (e.g., invalid topic).
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate
  *         error details.
  */
#define TopicSubscriber_receive2(topicSub, timeout, pMsg)      MessageConsumer_receive2(topicSub, timeout, pMsg)

/** Receive the next message if one is immediately available.
  *
  * @param topicSub the TopicSubscriber to operate against
  * @param pMsg pointer to the next Message produced for this message consumer, or
  * null if one is not available.
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error (e.g., invalid topic).
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate
  *         error details.
  */
#define TopicSubscriber_receiveNoWait(topicSub, pMsg)          MessageConsumer_receiveNoWait(topicSub, pMsg)

/** Since SonicMQ allocates some resources on behalf of a
  * MessageConsumer inside the client run-time, clients should close them when they
  * are not needed.
  *  
  * @param topicSub the TopicSubscriber to operate against
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error (e.g., invalid topic).
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate
  *         error details.
  */ 
#define TopicSubscriber_close(topicSub)                        ITopicSubscriber_close(topicSub)

/** Get the topic associated with this subscriber.
  *  
  * @param topicSub the TopicSubscriber to operate against
  * @param pTopic this subscriber's Topic
  *  
  * @return SMQ_SUCCESS (0) if successful, non-zero if error (e.g., invalid topic).
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate
  *         error details.
  */ 
#define TopicSubscriber_getTopic(topicSub, pTopic)               ITopicSubscriber_getTopic(topicSub, pTopic)

/** Get the NoLocal attribute for this TopicSubscriber.
  * The default value for this attribute is false.
  *  
  * @param topicSub the TopicSubscriber to operate against
  * @param pNoLocal set to true if locally published messages are being inhibited.
  *  
  * @return SMQ_SUCCESS (0) if successful, non-zero if error (e.g., invalid topic).
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate
  *         error details.
  */ 
#define TopicSubscriber_getNoLocal(topicSub, pNoLocal)             ITopicSubscriber_getNoLocal(topicSub, pNoLocal)

/**
 * Returns the int corresponding to the TopicSubscriber type.
 *
 * @return the int corresponding to the TopicSubscriber type
 */
int SMQ_API TopicSubscriber_type();

/*
//---------------------------------
// DurableTopicSubscriber
//---------------------------------
*/
/**
 * Decrements the reference count of the DurableTopicSubscriber object.
 *
 * @param thisObj the DurableTopicSubscriber object to operate against.
 * @param pret will contain the reference count after decrementing.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define DurableTopicSubscriber_release(thisObj, pret)                Object_release(thisObj, pret)

/**
 * Get the int corresponding to the type of a given object.
 *
 * @param thisObj the object to operate against.
 * @param pret will contain the int corresponding to the type.
 */
#define DurableTopicSubscriber_getType(thisObj, pret)                Object_getType(thisObj, pret)

/**
 * Determines whether the given object is an instance of the given type.
 *
 * @param thisObj the object to check.
 * @param classtype the type to check against.
 * @param pret non-zero if the object is an instance of the type.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define DurableTopicSubscriber_instanceof(thisObj, classtype, pret)  Object_instanceof(thisObj, classtype, pret)

/**
 * Determines whether the two DurableTopicSubscriber objects are the same.
 *
 * @param thisObj the first DurableTopicSubscriber to compare.
 * @param hobj2 the second DurableTopicSubscriber to compare.
 * @param pret true if the DurableTopicSubscriber objects have identical content, false otherwise.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define DurableTopicSubscriber_equals(thisObj, hobj2, pret)          Object_equals(thisObj, hobj2, pret)

/**
 * Get the String representation for the given DurableTopicSubscriber.
 *
 * @param thisObj the DurableTopicSubscriber object to operate against.
 * @param pret will contain the String that represents the DurableTopicSubscriber.
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
#define DurableTopicSubscriber_toString(thisObj, pret)               Object_toString(thisObj, pret)

/** Get this DurableTopicSubscriber's message selector expression.
  *
  * @param thisObj the DurableTopicSubscriber to operate against
  * @param pret this DurableTopicSubscriber's message selector
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  */
#define DurableTopicSubscriber_getMessageSelector(thisObj, pret)     MessageConsumer_getMessageSelector(thisObj, pret)

/** Get the DurableTopicSubscriber's MessageListener.
  *
  * @param thisObj the DurableTopicSubscriber to operate against
  * @param pret the listener for the DurableTopicSubscriber, or null if this isn't
  * one set.
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  *
  * @see DurableTopicSubscriber_setMessageListener()
  */
#define DurableTopicSubscriber_getMessageListener(thisObj, pret)     MessageConsumer_getMessageListener(thisObj, pret)

/** Set the DurableTopicSubscriber's MessageListener.
  *
  * @param thisObj the DurableTopicSubscriber to operate against
  * @param listener the messages are delivered to this listener
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error.
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
  *
  * @see DurableTopicSubscriber_getMessageListener()
  */
#define DurableTopicSubscriber_setMessageListener(thisObj, listener) MessageConsumer_setMessageListener(thisObj, listener)

/** Receive the next message produced for this DurableTopicSubscriber.
  *
  * <P>This call blocks indefinitely until a message is produced.
  *
  * <P>If this receive is done within a transaction, the message
  * remains on the consumer until the transaction commits.
  *
  * @param thisObj the DurableTopicSubscriber to operate against
  * @param pret pointer to the next Message produced for this message consumer, 
  *         or null, if the connection closes while waiting
  * @return SMQ_SUCCESS (0) if successful, non-zero if error (e.g., invalid topic).
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate
  *         error details.
  */
#define DurableTopicSubscriber_receive(thisObj, pret)                MessageConsumer_receive(thisObj, pret)

/** Receive the next message that arrives within the specified
  * timeout interval.
  *
  * <P>This call blocks until either a message arrives or the
  * timeout expires.
  *
  * @param thisObj the DurableTopicSubscriber to operate against
  * @param timeout the timeout value (in milliseconds)
  * @param pret pointer to the next Message produced for this DurableTopicSubscriber, 
  *         or null, if the connection closes while waiting, or if
  *         the wait times out.
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error (e.g., invalid topic).
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate
  *         error details.
  */
#define DurableTopicSubscriber_receive2(thisObj, timeout, pret)      MessageConsumer_receive2(thisObj, timeout, pret)

/** Receive the next message if one is immediately available.
  *
  * @param thisObj the DurableTopicSubscriber to operate against
  * @param pret pointer to the next Message produced for this DurableTopicSubscriber, or
  * null if one is not available.
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error (e.g., invalid topic).
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate
  *         error details.
  */
#define DurableTopicSubscriber_receiveNoWait(thisObj, pret)          MessageConsumer_receiveNoWait(thisObj, pret)

/** Since SonicMQ allocates some resources on behalf of a
  * DurableTopicSubscriber inside the client run-time, clients should close them when they
  * are not needed.
  *
  * @param thisObj the DurableTopicSubscriber to close
  *
  * @return SMQ_SUCCESS (0) if successful, non-zero if error (e.g., invalid topic).
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate
  *         error details.
  */ 
#define DurableTopicSubscriber_close(thisObj)                        ITopicSubscriber_close(thisObj)

/** Get the topic associated with this DurableTopicSubscriber.
  *  
  * @param thisObj the DurableTopicSubscriber to operate against
  * @param pret this subscriber's Topic
  *  
  * @return SMQ_SUCCESS (0) if successful, non-zero if error (e.g., invalid topic).
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate
  *         error details.
  */ 
#define DurableTopicSubscriber_getTopic(thisObj, pret)               ITopicSubscriber_getTopic(thisObj, pret)

/** Get the NoLocal attribute for this DurableTopicSubscriber.
  * The default value for this attribute is false.
  *  
  * @param thisObj the DurableTopicSubscriber to operate against
  * @param pret set to true if locally published messages are being inhibited.
  *  
  * @return SMQ_SUCCESS (0) if successful, non-zero if error (e.g., invalid topic).
  *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate
  *         error details.
  */ 
#define DurableTopicSubscriber_getNoLocal(thisObj, pret)             ITopicSubscriber_getNoLocal(thisObj, pret)

/**
 * Returns the int corresponding to the DurableTopicSubscriber type.
 *
 * @return the int corresponding to the DurableTopicSubscriber type
 */
int SMQ_API DurableTopicSubscriber_type();

/** C interface to register the LoginSPI for use with XA logins.  Users should set
 *  the login SPI prior to invocation of xa_open.
 *  When used with 4.0 brokers only clear text passwords are supported.  Users should
 *  use the setPassword method to set the clear text password.
 *  When used with 5.0 brokers only both clear text and encrypted (byte array) 
 *  passwords are supported.  Users should use the setPassword method to set the clear 
 *  text password and/or use setTransformedPassword to set the encrypted byte array
 *  password.
 *
 * @param map the map that implements the necessary login SPI routines
 *
 * @return SMQ_SUCCESS (0) if successful, non-zero if error.
 *         Use SMQ_getLastError() and SMQ_getLastErrorText() to interrogate error details.
 */
int SMQ_API setXALoginSPIMap (ILoginSPICMap *cmap);

#ifdef __cplusplus
}; /* extern "C" */
#endif

#endif /* _SMQC_H_ */

