#ifndef _JAVA_LANG_STRING_H_
#define _JAVA_LANG_STRING_H_
/*
* Copyright (c) 2001 Sonic Software Corporation. All Rights Reserved.
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

#include <java/lang/package_decls.h>

namespace java { 
    namespace lang { 
        /**
        * Creates a new <code>String</code> object so that it 
        * represents an empty character sequence. 
        */
        SMQJ_API StringRef createString();

        /**
        * Creates a new <code>String</code> so that it represents the 
        * sequence of characters currently contained in the character array 
        * pointed to by 'c'. The contents of the character array are copied; subsequent 
        * modification of the character array does not affect the newly created 
        * string. 
        *
        * @param  c   the initial value of the string.
        */
        SMQJ_API StringRef createString(const char *c);

        /**
        * Creates a new <code>String</code> so that it represents the 
        * sequence of characters currently contained in the UTF-8 character array 
        * pointed to by 'c'. The contents of the character array are converted
        * to UCS-2 for internal use. 
        *
        * @param  c   the initial value of the string.
        * @param  encoding  The UTF encoding.  Must be some form of UTF-8.
        */
        SMQJ_API StringRef createString(const unsigned char *c, const char *encoding);

        /**
        * Creates a new <code>String</code> so that it represents the 
        * sequence of characters currently contained in the wide character array 
        * pointed to by 'c'. The contents of the character array are copied; subsequent 
        * modification of the character array does not affect the newly created 
        * string. 
        *
        * @param  c   the initial value of the string.
        */
        SMQJ_API StringRef createString(const wchar_t *c);

        /**
        * Creates a new <code>String</code> object so that it 
        * represents the same sequence of characters as the argument; in other 
        * words, the newly created string is a copy of the argument string. 
        *
        * @param   value   a <code>String</code>.
        */
        SMQJ_API StringRef createString(const StringRef value);

        /**
        * Creates a new <code>String</code> so that it represents the 
        * sequence of characters currently contained in the character array 
        * argument. The contents of the character array are copied; subsequent 
        * modification of the character array does not affect the newly created 
        * string. 
        *
        * @param  value   the initial value of the string.
        */
        SMQJ_API StringRef createString(const jcharArray * value); 

        /**
        * Creates a new <code>String</code> that contains characters from 
        * a subarray of the character array argument. The <code>offset</code> 
        * argument is the index of the first character of the subarray and 
        * the <code>count</code> argument specifies the length of the 
        * subarray. The contents of the subarray are copied; subsequent 
        * modification of the character array does not affect the newly 
        * created string. 
        *
        * @param      value    array that is the source of characters.
        * @param      offset   the initial offset.
        * @param      count    the length.
        * @exception  IndexOutOfBoundsException  if the <code>offset</code>
        *               and <code>count</code> arguments index characters outside
        *               the bounds of the <code>value</code> array.
        */
        SMQJ_API StringRef createString(const jcharArray * value, jint offset, jint count);

        /**
        * Creates a new <code>String</code> constructed from a subarray 
        * of an array of 8-bit integer values. 
        * <p>
        * The <code>offset</code> argument is the index of the first byte 
        * of the subarray, and the <code>count</code> argument specifies the 
        * length of the subarray. 
        * <p>
        * Each <code>byte</code> in the subarray is converted to a 
        * <code>char</code> as specified in the method above. 
        *
        * @param      bytes     the bytes to be converted to characters.
        * @param      hibyte    the top 8 bits of each 16-bit Unicode character.
        * @param      offset    the initial offset.
        * @param      count     the length.
        * @exception  IndexOutOfBoundsException  if the <code>offset</code>
        *               or <code>count</code> argument is invalid.
        */
        SMQJ_API StringRef createString(const jbyteArray * bytes, jint hibyte, jint offset, jint count);

        /**
        * Creates a new <code>String</code> by converting the specified
        * subarray of bytes using the specified character encoding.  The length of
        * the new <code>String</code> is a function of the encoding, and hence may
        * not be equal to the length of the subarray.
        *
        * @param  bytes   The bytes to be converted into characters
        * @param  offset  Index of the first byte to convert
        * @param  length  Number of bytes to convert
        * @param  enc     The name of a character encoding
        * @exception  IndexOutOfBoundsException  if the <code>offset</code>
        *               or <code>count</code> argument is invalid.
        */
        SMQJ_API StringRef createString(const jbyteArray * bytes, jint offset, jint length, const StringRef enc); 

        /**
        * Creates a new <code>String</code> by converting the specified
        * subarray of bytes using the platform's default character encoding.  The
        * length of the new <code>String</code> is a function of the encoding, and
        * hence may not be equal to the length of the subarray.
        *
        * @param  bytes   The bytes to be converted into characters
        * @param  offset  Index of the first byte to convert
        * @param  length  Number of bytes to convert
        * @exception  IndexOutOfBoundsException  if the <code>offset</code>
        *               or <code>count</code> argument is invalid.
        */
        SMQJ_API StringRef createString(const jbyteArray * bytes, jint offset, jint length); 

        /**
        * Create a new <code>String</code> by converting the specified array
        * of bytes using the platform's default character encoding.  The length of
        * the new <code>String</code> is a function of the encoding, and hence may
        * not be equal to the length of the byte array.
        *
        * @param  bytes   The bytes to be converted into characters
        */
        SMQJ_API StringRef createString(const jbyteArray * bytes); 

        /**
        * The <code>String</code> class represents character strings. 
        * <p>
        * Strings are constant; their values cannot be changed after they 
        * are created. String buffers support mutable strings.
        * Because String objects are immutable they can be shared. For example:
        * <p><blockquote><pre>
        *     StringRef str = createString("abc");
        * </pre></blockquote><p>
        * is equivalent to:
        * <p><blockquote><pre>
        *     jcharArrayRef data = createjcharArray{"abc"};
        *     StringRef str = createString(data);
        * </pre></blockquote><p>
        * Here are some more examples of how strings can be used:
        * <p><blockquote><pre>
        *     StringRef cde = createString("cde");
        *     StringRef abccde = createString("abc") + cde;
        *     StringRef c = abccde->substring(2,3);
        *     StringRef d = cde->substring(1, 2);
        * </pre></blockquote>
        * <p>
        * The class <code>String</code> includes methods for examining 
        * individual characters of the sequence, for comparing strings, for 
        * searching strings, for extracting substrings, and for creating a 
        * copy of a string with all characters translated to uppercase or to 
        * lowercase. 
        * <p>
        * The SonicMQ runtime provides special support for the string 
        * concatentation operator (&nbsp;+&nbsp;).
        */
        class SMQJ_API String : public Object
        {
        public:
            /**
            * Returns the length of this string.
            * The length is equal to the number of 16-bit
            * Unicode characters in the string.
            *
            * @return  the length of the sequence of characters represented by this
            *          object.
            */
            jint length() const;

            /**
            * Returns the internal jchar array of this string. Note: the String owns the
            * memory.
            *
            * @return  pointer to the sequence of characters represented by this
            *          object.
            */
            const jchar* getArray() const;

            /**
            * Returns the character at the specified index. An index ranges
            * from <code>0</code> to <code>length() - 1</code>. The first character 
            * of the sequence is at index <code>0</code>, the next at index 
            * <code>1</code>, and so on, as for array indexing.
            *
            * @param      index   the index of the character.
            * @return     the character at the specified index of this string.
            *             The first character is at index <code>0</code>.
            * @exception  IndexOutOfBoundsException  if the <code>index</code> 
            *             argument is negative or not less than the length of this 
            *             string.
            */
            jchar charAt(jint index) const;

            /**
            * Copies characters from this string into the destination character 
            * array. 
            * <p>
            * The first character to be copied is at index <code>srcBegin</code>; 
            * the last character to be copied is at index <code>srcEnd-1</code> 
            * (thus the total number of characters to be copied is 
            * <code>srcEnd-srcBegin</code>). The characters are copied into the 
            * subarray of <code>dst</code> starting at index <code>dstBegin</code> 
            * and ending at index: 
            * <p><blockquote><pre>
            *     dstbegin + (srcEnd-srcBegin) - 1
            * </pre></blockquote>
            *
            * @param      srcBegin   index of the first character in the string
            *                        to copy.
            * @param      srcEnd     index after the last character in the string
            *                        to copy.
            * @param      dst        the destination array.
            * @param      dstBegin   the start offset in the destination array.
            * @exception IndexOutOfBoundsException If any of the following 
            *            is true:
            *            <ul><li><code>srcBegin</code> is negative.
            *            <li><code>srcBegin</code> is greater than <code>srcEnd</code>
            *            <li><code>srcEnd</code> is greater than the length of this 
            *                string
            *            <li><code>dstBegin</code> is negative
            *            <li><code>dstBegin+(srcEnd-srcBegin)</code> is larger than 
            *                <code>dst.length</code></ul>
            */
            void getChars(jint srcBegin, jint srcEnd, jcharArray * dst, jint dstBegin) const;

            /**
            * Copies characters from this string into the destination byte 
            * array. Each byte receives the 8 low-order bits of the 
            * corresponding character. The eight high-order bits of each character 
            * are not copied and do not participate in the transfer in any way.
            * <p>
            * The first character to be copied is at index <code>srcBegin</code>; 
            * the last character to be copied is at index <code>srcEnd-1</code>. 
            * The total number of characters to be copied is 
            * <code>srcEnd-srcBegin</code>. The characters, converted to bytes, 
            * are copied into the subarray of <code>dst</code> starting at index 
            * <code>dstBegin</code> and ending at index: 
            * <p><blockquote><pre>
            *     dstbegin + (srcEnd-srcBegin) - 1
            * </pre></blockquote>
            *
            * @param      srcBegin   index of the first character in the string
            *                        to copy.
            * @param      srcEnd     index after the last character in the string
            *                        to copy.
            * @param      dst        the destination array.
            * @param      dstBegin   the start offset in the destination array.
            * @exception IndexOutOfBoundsException if any of the following 
            *            is true:
            *           <ul<li><code>srcBegin</code> is negative 
            *           <li><code>srcBegin</code> is greater than <code>srcEnd</code> 
            *           <li><code>srcEnd</code> is greater than the length of this 
            *            String 
            *           <li><code>dstBegin</code> is negative 
            *           <li><code>dstBegin+(srcEnd-srcBegin)</code> is larger than 
            *            <code>dst.length</code> 
            */
            void getBytes(jint srcBegin, jint srcEnd, jbyteArray * dst, jint dstBegin) const;

            /**
            * Tests if this string ends with the specified suffix.
            *
            * @param   suffix   the suffix.
            * @return  <code>jtrue</code> if the character sequence represented by the
            *          argument is a suffix of the character sequence represented by
            *          this object; <code>jfalse</code> otherwise. Note that the 
            *          result will be <code>jtrue</code> if the argument is the 
            *          empty string or is equal to this <code>String</code> object 
            *          as determined by the equals(Object) method.
            */
            jboolean endsWith(const String * suffix) const;

            /**
            * Compares this <code>String</code> to another <code>String</code>,
            * ignoring case considerations.  Two strings are considered equal
            * ignoring case if they are of the same length, and corresponding
            * characters in the two strings are equal ignoring case.
            * <p>
            * Two characters <code>c1</code> and <code>c2</code> are considered
            * the same, ignoring case if at least one of the following is true:
            * <ul><li>The two characters are the same (as compared by the 
            * <code>==</code> operator).
            * <li>Applying the method java.lang.Character::toUppercase(char) 
            * to each character produces the same result.
            * <li>Applying the method java.lang.Character::toLowercase(char) 
            * to each character produces the same result.</ul>
            *
            * @param   anotherString   the <code>String</code> to compare this
            *                          <code>String</code> against.
            * @return  <code>jtrue</code> if the argument is not <code>null</code> 
            *          and the <code>String</code>s are equal,
            *          ignoring case; <code>jfalse</code> otherwise.
            */
            jboolean equalsIgnoreCase(const String * anotherString) const;

            /**
            * Tests if this string starts with the specified prefix.
            *
            * @param   prefix   the prefix.
            * @return  <code>jtrue</code> if the character sequence represented by the
            *          argument is a prefix of the character sequence represented by
            *          this string; <code>jfalse</code> otherwise.      
            *          Note also that <code>jtrue</code> will be returned if the 
            *          argument is an empty string or is equal to this 
            *          <code>String</code> object as determined by the 
            *          equals(Object) method.
            */
            jboolean startsWith(const String * prefix) const;

            /**
            * Tests if this string starts with the specified prefix.
            *
            * @param   prefix   the prefix.
            * @return  <code>jtrue</code> if the character sequence represented by the
            *          argument is a prefix of the character sequence represented by
            *          this string; <code>jfalse</code> otherwise.      
            *          Note also that <code>jtrue</code> will be returned if the 
            *          argument is an empty string or is equal to this 
            *          <code>String</code> object as determined by the 
            *          equals(Object) method.
            */
            jboolean startsWith(const char* prefix) const;

            /**
            * Compares two strings lexicographically. 
            * The comparison is based on the Unicode value of each character in
            * the strings. The character sequence represented by this 
            * <code>String</code> object is compared lexicographically to the 
            * character sequence represented by the argument string. The result is 
            * a negative integer if this <code>String</code> object 
            * lexicographically precedes the argument string. The result is a 
            * positive integer if this <code>String</code> object lexicographically 
            * follows the argument string. The result is zero if the strings
            * are equal; <code>compareTo</code> returns <code>0</code> exactly when 
            * the equals(Object) method would return <code>jtrue</code>. 
            * <p>
            * This is the definition of lexicographic ordering. If two strings are 
            * different, then either they have different characters at some index 
            * that is a valid index for both strings, or their lengths are different, 
            * or both. If they have different characters at one or more index 
            * positions, let <i>k</i> be the smallest such index; then the string
            * whose character at position <i>k</i> has the smaller value, as 
            * determined by using the < operator, lexicographically precedes the 
            * other string. In this case, <code>compareTo</code> returns the 
            * difference of the two character values at position <code>k</code> in 
            * the two string -- that is, the value:
            * <blockquote><pre>
            * this->charAt(k) - anotherString->charAt(k)
            * </pre></blockquote>
            * If there is no index position at which they differ, then the shorter 
            * string lexicographically precedes the longer string. In this case, 
            * <code>compareTo</code> returns the difference of the lengths of the 
            * strings -- that is, the value: 
            * <blockquote><pre>
            * this->length() - anotherString->length()
            * </pre></blockquote>
            *
            * @param   anotherString   the <code>String</code> to be compared.
            * @return  the value <code>0</code> if the argument string is equal to
            *          this string; a value less than <code>0</code> if this string
            *          is lexicographically less than the string argument; and a
            *          value greater than <code>0</code> if this string is
            *          lexicographically greater than the string argument.
            */
            jint compareTo(const String * anotherString) const;

            /**
            * Compares two strings lexicographically. 
            * The comparison is based on the Unicode value of each character in
            * the strings. The character sequence represented by this 
            * <code>String</code> object is compared lexicographically to the 
            * character sequence represented by the argument string. The result is 
            * a negative integer if this <code>String</code> object 
            * lexicographically precedes the argument string. The result is a 
            * positive integer if this <code>String</code> object lexicographically 
            * follows the argument string. The result is zero if the strings
            * are equal; <code>compareTo</code> returns <code>0</code> exactly when 
            * the equals(Object) method would return <code>jtrue</code>. 
            * <p>
            * This is the definition of lexicographic ordering. If two strings are 
            * different, then either they have different characters at some index 
            * that is a valid index for both strings, or their lengths are different, 
            * or both. If they have different characters at one or more index 
            * positions, let <i>k</i> be the smallest such index; then the string
            * whose character at position <i>k</i> has the smaller value, as 
            * determined by using the < operator, lexicographically precedes the 
            * other string. In this case, <code>compareTo</code> returns the 
            * difference of the two character values at position <code>k</code> in 
            * the two string -- that is, the value:
            * <blockquote><pre>
            * this->charAt(k) - anotherString->charAt(k)
            * </pre></blockquote>
            * If there is no index position at which they differ, then the shorter 
            * string lexicographically precedes the longer string. In this case, 
            * <code>compareTo</code> returns the difference of the lengths of the 
            * strings -- that is, the value: 
            * <blockquote><pre>
            * this->length() - anotherString->length()
            * </pre></blockquote>
            *
            * @param   anotherString   the <code>String</code> to be compared.
            * @return  the value <code>0</code> if the argument string is equal to
            *          this string; a value less than <code>0</code> if this string
            *          is lexicographically less than the string argument; and a
            *          value greater than <code>0</code> if this string is
            *          lexicographically greater than the string argument.
            */
            jint compareTo(const StringRef anotherString) const;

            /**
            * Convert this <code>String</code> into bytes according to the platform's
            * default character encoding, storing the result into a new byte array.
            *
            * @return  the resultant byte array.
            */
            jbyteArray * getBytes() const;

            /**
            * Tests if this string starts with the specified prefix beginning 
            * a specified index.
            *
            * @param   prefix    the prefix.
            * @param   toffset   where to begin looking in the string.
            * @return  <code>jtrue</code> if the character sequence represented by the
            *          argument is a prefix of the substring of this object starting
            *          at index <code>toffset</code>; <code>jfalse</code> otherwise. 
            *          The result is <code>jfalse</code> if <code>toffset</code> is 
            *          negative or greater than the length of this 
            *          <code>String</code> object; otherwise the result is the same 
            *          as the result of the expression
            *          <pre>
            *          this->subString(toffset)->startsWith(prefix)
            *          </pre>
            */
            jboolean startsWith(const String * prefix, jint toffset) const;

            /**
            * Tests if this string starts with the specified prefix beginning 
            * a specified index.
            *
            * @param   prefix    the prefix.
            * @param   toffset   where to begin looking in the string.
            * @return  <code>jtrue</code> if the character sequence represented by the
            *          argument is a prefix of the substring of this object starting
            *          at index <code>toffset</code>; <code>jfalse</code> otherwise. 
            *          The result is <code>jfalse</code> if <code>toffset</code> is 
            *          negative or greater than the length of this 
            *          <code>String</code> object; otherwise the result is the same 
            *          as the result of the expression
            *          <pre>
            *          this->subString(toffset)->startsWith(prefix)
            *          </pre>
            */
            jboolean startsWith(const char* prefix, jint toffset) const;

            /**
            * Tests if two string regions are equal. 
            * <p>
            * A substring of this <tt>String</tt> object is compared to a substring 
            * of the argument other. The result is true if these substrings 
            * represent identical character sequences. The substring of this 
            * <tt>String</tt> object to be compared begins at index <tt>toffset</tt> 
            * and has length <tt>len</tt>. The substring of other to be compared 
            * begins at index <tt>ooffset</tt> and has length <tt>len</tt>. The 
            * result is <tt>jfalse</tt> if and only if at least one of the following 
            * is true: 
            * <ul><li><tt>toffset</tt> is negative. 
            * <li><tt>ooffset</tt> is negative. 
            * <li><tt>toffset+len</tt> is greater than the length of this 
            * <tt>String</tt> object. 
            * <li><tt>ooffset+len</tt> is greater than the length of the other 
            * argument. 
            * <li>There is some nonnegative integer <i>k</i> less than <tt>len</tt> 
            * such that: 
            * <tt>this->charAt(toffset+<i>k</i>)&nbsp;!=&nbsp;other->charAt(ooffset+<i>k</i>)</tt> 
            * </u>
            *
            * @param   toffset   the starting offset of the subregion in this string.
            * @param   other     the string argument.
            * @param   ooffset   the starting offset of the subregion in the string
            *                    argument.
            * @param   len       the number of characters to compare.
            * @return  <code>jtrue</code> if the specified subregion of this string
            *          exactly matches the specified subregion of the string argument;
            *          <code>jfalse</code> otherwise.
            */
            jboolean regionMatches(jint toffset, const String * other, jint ooffset, jint len) const;

            /**
            * Convert this <code>String</code> into bytes according to the specified
            * character encoding, storing the result into a new byte array.
            *
            * @param  enc  A character-encoding name
            * @return      The resultant byte array
            */
            jbyteArray * getBytes(const String * enc) const;

            /**
            * Compares two strings lexicographically, ignoring case considerations.
            * This method returns an integer whose sign is that of
            * <code>this->toUpperCase()->toLowerCase()->compareTo(
            * str->toUpperCase()->toLowerCase())</code>.
            *
            * @param   str   the <code>String</code> to be compared.
            * @return  a negative integer, zero, or a positive integer as the
            *        the specified String is greater than, equal to, or less
            *        than this String, ignoring case considerations.
            */
            jint compareToIgnoreCase(const String * str) const;

            /**
            * Compares this String to another Object.  If the Object is a String,
            * this function behaves like <code>compareTo(String)</code>.  Otherwise,
            * it throws a <code>ClassCastException</code> (as Strings are comparable
            * only to other Strings).
            *
            * @param   o the <code>Object</code> to be compared.
            * @return  the value <code>0</code> if the argument is a string
            *        lexicographically equal to this string; a value less than
            *        <code>0</code> if the argument is a string lexicographically 
            *        greater than this string; and a value greater than
            *        <code>0</code> if the argument is a string lexicographically
            *        less than this string.
            * @exception <code>ClassCastException</code> if the argument is not a
            *          <code>String</code>. 
            */
            jint compareTo(const Object * anObject) const;

            /**
            * Tests if two string regions are equal. 
            * <p>
            * A substring of this <tt>String</tt> object is compared to a substring 
            * of the argument <tt>other</tt>. The result is <tt>jtrue</tt> if these 
            * substrings represent character sequences that are the same, ignoring 
            * case if and only if <tt>ignoreCase</tt> is true. The substring of
            * this <tt>String</tt> object to be compared begins at index 
            * <tt>toffset</tt> and has length <tt>len</tt>. The substring of 
            * <tt>other</tt> to be compared begins at index <tt>ooffset</tt> and 
            * has length <tt>len</tt>. The result is <tt>jfalse</tt> if and only if 
            * at least one of the following is true: 
            * <ul><li><tt>toffset</tt> is negative. 
            * <li><tt>ooffset</tt> is negative. 
            * <li><tt>toffset+len</tt> is greater than the length of this 
            * <tt>String</tt> object. 
            * <li><tt>ooffset+len</tt> is greater than the length of the other 
            * argument. 
            * <li>There is some nonnegative integer <i>k</i> less than <tt>len</tt> 
            * such that:
            * <blockquote><pre>
            * this->charAt(toffset+k) != other->charAt(ooffset+k) 
            * </pre></blockquote>
            * <li><tt>ignoreCase</tt> is <tt>jtrue</tt> and there is some nonnegative 
            * integer <i>k</i> less than <tt>len</tt> such that: 
            * <blockquote><pre>
            * this->toLowerCase()->charAt(toffset+k) !=
            *        other->toLowerCase()->charAt(ooffset+k)
            * </pre></blockquote> 
            * and: 
            * <blockquote><pre>
            * this->toUpperCase()->charAt(toffset+k) !=
            *         other->toUpperCase()->charAt(ooffset+k)
            * </pre></blockquote>
            * </ul>
            *
            * @param   ignoreCase   if <code>jtrue</code>, ignore case when comparing
            *                       characters.
            * @param   toffset      the starting offset of the subregion in this
            *                       string.
            * @param   other        the string argument.
            * @param   ooffset      the starting offset of the subregion in the string
            *                       argument.
            * @param   len          the number of characters to compare.
            * @return  <code>jtrue</code> if the specified subregion of this string
            *          matches the specified subregion of the string argument;
            *          <code>jfalse</code> otherwise. Whether the matching is exact
            *          or case insensitive depends on the <code>ignoreCase</code>
            *          argument.
            */
            jboolean regionMatches(jboolean ignoreCase, jint toffset, const String * other, jint ooffset, jint len) const;

            jint hashCode();

            /**
            * Compares this string to the specified object.
            * The result is <code>jtrue</code> if and only if the argument is not 
            * <code>null</code> and is a <code>String</code> object that represents 
            * the same sequence of characters as this object. 
            *
            * @param   anObject   the object to compare this <code>String</code>
            *                     against.
            * @return  <code>jtrue</code> if the <code>String </code>are equal;
            *          <code>jfalse</code> otherwise.
            */
            jboolean equals(const Object * anString) const;

            /**
            * Compares this string to the specified object.
            * The result is <code>jtrue</code> if and only if the argument is not 
            * <code>null</code> and is a <code>String</code> object that represents 
            * the same sequence of characters as this object. 
            *
            * @param   anObject   the object to compare this <code>String</code>
            *                     against.
            * @return  <code>jtrue</code> if the <code>String </code>are equal;
            *          <code>jfalse</code> otherwise.
            */
            jboolean equals(ObjectRef anString) const;
            jboolean equals(Object * anString) const;

            /**
            * Returns the index within this string of the first occurrence of the
            * specified character. If a character with value <code>ch</code> occurs 
            * in the character sequence represented by this <code>String</code> 
            * object, then the index of the first such occurrence is returned -- 
            * that is, the smallest value <i>k</i> such that: 
            * <blockquote><pre>
            * this->charAt(<i>k</i>) == ch
            * </pre></blockquote>
            * is <code>jtrue</code>. If no such character occurs in this string, 
            * then <code>-1</code> is returned.
            *
            * @param   ch   a character.
            * @return  the index of the first occurrence of the character in the
            *          character sequence represented by this object, or
            *          <code>-1</code> if the character does not occur.
            */
            jint indexOf(jint ch) const;

            /**
            * Returns the index within this string of the first occurrence of the
            * specified character, starting the search at the specified index.
            * <p>
            * If a character with value <code>ch</code> occurs in the character 
            * sequence represented by this <code>String</code> object at an index 
            * no smaller than <code>fromIndex</code>, then the index of the first
            * such occurrence is returned--that is, the smallest value <i>k</i> 
            * such that: 
            * <blockquote><pre>
            * (this->charAt(<i>k</i>) == ch) && (<i>k</i> >= fromIndex)
            * </pre></blockquote>
            * is true. If no such character occurs in this string at or after 
            * position <code>fromIndex</code>, then <code>-1</code> is returned.
            * <p>
            * There is no restriction on the value of <code>fromIndex</code>. If it 
            * is negative, it has the same effect as if it were zero: this entire 
            * string may be searched. If it is greater than the length of this 
            * string, it has the same effect as if it were equal to the length of 
            * this string: <code>-1</code> is returned.
            *
            * @param   ch          a character.
            * @param   fromIndex   the index to start the search from.
            * @return  the index of the first occurrence of the character in the
            *          character sequence represented by this object that is greater
            *          than or equal to <code>fromIndex</code>, or <code>-1</code>
            *          if the character does not occur.
            */
            jint indexOf(jint ch, jint fromIndex) const;

            /**
            * Returns the index within this string of the last occurrence of the
            * specified character. That is, the index returned is the largest 
            * value <i>k</i> such that:
            * <blockquote><pre>
            * this->charAt(<i>k</i>) == ch
            * </pre></blockquote>
            * is true. 
            * The String is searched backwards starting at the last character. 
            *
            * @param   ch   a character.
            * @return  the index of the last occurrence of the character in the
            *          character sequence represented by this object, or
            *          <code>-1</code> if the character does not occur.
            */
            jint lastIndexOf(jint ch) const;

            /**
            * Returns the index within this string of the last occurrence of the
            * specified character, searching backward starting at the specified 
            * index. That is, the index returned is the largest value <i>k</i> 
            * such that:
            * <blockquote><pre>
            * this->charAt(k) == ch) && (k <= fromIndex)
            * </pre></blockquote>
            * is true.
            *
            * @param   ch          a character.
            * @param   fromIndex   the index to start the search from. There is no 
            *          restriction on the value of <code>fromIndex</code>. If it is 
            *          greater than or equal to the length of this string, it has 
            *          the same effect as if it were equal to one less than the 
            *          length of this string: this entire string may be searched. 
            *          If it is negative, it has the same effect as if it were -1: 
            *          -1 is returned.
            * @return  the index of the last occurrence of the character in the
            *          character sequence represented by this object that is less
            *          than or equal to <code>fromIndex</code>, or <code>-1</code>
            *          if the character does not occur before that point.
            */
            jint lastIndexOf(jint ch, jint fromIndex) const;

            /**
            * Returns the index within this string of the first occurrence of the
            * specified substring. The integer returned is the smallest value 
            * <i>k</i> such that:
            * <blockquote><pre>
            * this->startsWith(str, <i>k</i>)
            * </pre></blockquote>
            * is <code>true</code>.
            *
            * @param   str   any string.
            * @return  if the string argument occurs as a substring within this
            *          object, then the index of the first character of the first
            *          such substring is returned; if it does not occur as a
            *          substring, <code>-1</code> is returned.
            */
            jint indexOf(const String * str) const;

            /**
            * Returns the index within this string of the first occurrence of the
            * specified substring. The integer returned is the smallest value 
            * <i>k</i> such that:
            * <blockquote><pre>
            * this->startsWith(str, <i>k</i>)
            * </pre></blockquote>
            * is <code>true</code>.
            *
            * @param   str   any string.
            * @return  if the string argument occurs as a substring within this
            *          object, then the index of the first character of the first
            *          such substring is returned; if it does not occur as a
            *          substring, <code>-1</code> is returned.
            */
            jint indexOf(const char * str) const;

            /**
            * Returns the index within this string of the first occurrence of the
            * specified substring, starting at the specified index. The integer 
            * returned is the smallest value <i>k</i> such that:
            * <blockquote><pre>
            * this->startsWith(str, <i>k</i>) && (<i>k</i> >= fromIndex)
            * </pre></blockquote>
            * is <code>true</code>.
            * <p>
            * There is no restriction on the value of <code>fromIndex</code>. If 
            * it is negative, it has the same effect as if it were zero: this entire 
            * string may be searched. If it is greater than the length of this 
            * string, it has the same effect as if it were equal to the length of 
            * this string: <code>-1</code> is returned.
            *
            * @param   str         the substring to search for.
            * @param   fromIndex   the index to start the search from.
            * @return  If the string argument occurs as a substring within this
            *          object at a starting index no smaller than
            *          <code>fromIndex</code>, then the index of the first character
            *          of the first such substring is returned. If it does not occur
            *          as a substring starting at <code>fromIndex</code> or beyond,
            *          <code>-1</code> is returned.
            * @exception java.lang.NullPointerException if <code>str</code> is 
            *          <code>null</code>
            */
            jint indexOf(const String * str, jint fromIndex) const;

            /**
            * Returns the index within this string of the first occurrence of the
            * specified substring, starting at the specified index. The integer 
            * returned is the smallest value <i>k</i> such that:
            * <blockquote><pre>
            * this->startsWith(str, <i>k</i>) && (<i>k</i> >= fromIndex)
            * </pre></blockquote>
            * is <code>true</code>.
            * <p>
            * There is no restriction on the value of <code>fromIndex</code>. If 
            * it is negative, it has the same effect as if it were zero: this entire 
            * string may be searched. If it is greater than the length of this 
            * string, it has the same effect as if it were equal to the length of 
            * this string: <code>-1</code> is returned.
            *
            * @param   str         the substring to search for.
            * @param   fromIndex   the index to start the search from.
            * @return  If the string argument occurs as a substring within this
            *          object at a starting index no smaller than
            *          <code>fromIndex</code>, then the index of the first character
            *          of the first such substring is returned. If it does not occur
            *          as a substring starting at <code>fromIndex</code> or beyond,
            *          <code>-1</code> is returned.
            * @exception java.lang.NullPointerException if <code>str</code> is 
            *          <code>null</code>
            */
            jint indexOf(const char* str, jint fromIndex) const;

            /**
            * Returns the index within this string of the rightmost occurrence
            * of the specified substring.  The rightmost empty string "" is
            * considered to occur at the index value <code>this.length()</code>. 
            * The returned index is the largest value <i>k</i> such that 
            * <blockquote><pre>
            * this->startsWith(str, k)
            * </pre></blockquote>
            * is true.
            *
            * @param   str   the substring to search for.
            * @return  if the string argument occurs one or more times as a substring
            *          within this object, then the index of the first character of
            *          the last such substring is returned. If it does not occur as
            *          a substring, <code>-1</code> is returned.
            * @exception java.lang.NullPointerException  if <code>str</code> is 
            *          <code>null</code>.
            */
            jint lastIndexOf(const String * str) const;

            /**
            * Returns the index within this string of the rightmost occurrence
            * of the specified substring.  The rightmost empty string "" is
            * considered to occur at the index value <code>this.length()</code>. 
            * The returned index is the largest value <i>k</i> such that 
            * <blockquote><pre>
            * this->startsWith(str, k)
            * </pre></blockquote>
            * is true.
            *
            * @param   str   the substring to search for.
            * @return  if the string argument occurs one or more times as a substring
            *          within this object, then the index of the first character of
            *          the last such substring is returned. If it does not occur as
            *          a substring, <code>-1</code> is returned.
            * @exception java.lang.NullPointerException  if <code>str</code> is 
            *          <code>null</code>.
            */
            jint lastIndexOf(const char * str) const;

            /**
            * Returns the index within this string of the last occurrence of
            * the specified substring.
            * The returned index indicates the start of the substring, and it
            * must be equal to or less than <code>fromIndex</code>. That is, 
            * the index returned is the largest value <i>k</i> such that:
            * <blockquote><pre>
            * this.startsWith(str, k) && (k <= fromIndex)
            * </pre></blockquote>
            * 
            * @param   str         the substring to search for.
            * @param   fromIndex   the index to start the search from. There is no 
            *          restriction on the value of fromIndex. If it is greater than 
            *          the length of this string, it has the same effect as if it 
            *          were equal to the length of this string: this entire string 
            *          may be searched. If it is negative, it has the same effect 
            *          as if it were -1: -1 is returned.
            * @return  If the string argument occurs one or more times as a substring
            *          within this object at a starting index no greater than
            *          <code>fromIndex</code>, then the index of the first character of
            *          the last such substring is returned. If it does not occur as a
            *          substring starting at <code>fromIndex</code> or earlier,
            *          <code>-1</code> is returned.
            * @exception java.lang.NullPointerException if <code>str</code> is 
            *          <code>null</code>.
            */
            jint lastIndexOf(const String * str, jint fromIndex) const;

            /**
            * Returns a new string that is a substring of this string. The 
            * substring begins with the character at the specified index and 
            * extends to the end of this string. <p>
            * Examples:
            * <blockquote><pre>
            * createString("unhappy")->substring(2) returns "happy"
            * createString("Harbison")->substring(3) returns "bison"
            * createString("emptiness")->substring(9) returns "" (an empty string)
            * </pre></blockquote>
            *
            * @param      beginIndex   the beginning index, inclusive.
            * @return     the specified substring.
            * @exception  IndexOutOfBoundsException  if 
            *             <code>beginIndex</code> is negative or larger than the 
            *             length of this <code>String</code> object.
            */
            String * substring(jint beginIndex) const;

            /**
            * Returns a new string that is a substring of this string. The 
            * substring begins at the specified <code>beginIndex</code> and 
            * extends to the character at index <code>endIndex - 1</code>. 
            * Thus the length of the substring is <code>endIndex-beginIndex</code>.
            * <p>
            * Examples:
            * <blockquote><pre>
            * createString("hamburger")->substring(4, 8) returns "urge"
            * createString("smiles")->substring(1, 5) returns "mile"
            * </pre></blockquote>
            *
            * @param      beginIndex   the beginning index, inclusive.
            * @param      endIndex     the ending index, exclusive.
            * @return     the specified substring.
            * @exception  IndexOutOfBoundsException  if the
            *             <code>beginIndex</code> is negative, or 
            *             <code>endIndex</code> is larger than the length of 
            *             this <code>String</code> object, or 
            *             <code>beginIndex</code> is larger than 
            *             <code>endIndex</code>.
            */
            String * substring(jint beginIndex, jint endIndex) const;

            /**
            * Concatenates the specified string to the end of this string. 
            * <p>
            * If the length of the argument string is <code>0</code>, then this 
            * <code>String</code> object is returned. Otherwise, a new 
            * <code>String</code> object is created, representing a character 
            * sequence that is the concatenation of the character sequence 
            * represented by this <code>String</code> object and the character 
            * sequence represented by the argument string.<p>
            * Examples:
            * <blockquote><pre>
            * createString("cares")->concat(createString("s")) returns "caress"
            * createString("to")->concat(createString("get"))->concat(createString("her")) returns "together"
            * </pre></blockquote>
            *
            * @param   str   the <code>String</code> that is concatenated to the end
            *                of this <code>String</code>.
            * @return  a string that represents the concatenation of this object's
            *          characters followed by the string argument's characters.
            * @exception java.lang.NullPointerException if <code>str</code> is 
            *          <code>null</code>.
            */
            String * concat(const String * str) const;

            /**
            * Returns a new string resulting from replacing all occurrences of 
            * <code>oldChar</code> in this string with <code>newChar</code>. 
            * <p>
            * If the character <code>oldChar</code> does not occur in the 
            * character sequence represented by this <code>String</code> object, 
            * then a reference to this <code>String</code> object is returned. 
            * Otherwise, a new <code>String</code> object is created that 
            * represents a character sequence identical to the character sequence 
            * represented by this <code>String</code> object, except that every 
            * occurrence of <code>oldChar</code> is replaced by an occurrence
            * of <code>newChar</code>. 
            * <p>
            * Examples:
            * <blockquote><pre>
            * createString("mesquite in your cellar")->replace(L'e', L'o')
            *         returns "mosquito in your collar"
            * createString("the war of baronets")->replace(L'r', L'y')
            *         returns "the way of bayonets"
            * createString("sparring with a purple porpoise")->replace(L'p', L't')
            *         returns "starring with a turtle tortoise"
            * createString("JonL")->replace(L'q', L'x') returns "JonL" (no change)
            * </pre></blockquote>
            *
            * @param   oldChar   the old character.
            * @param   newChar   the new character.
            * @return  a string derived from this string by replacing every
            *          occurrence of <code>oldChar</code> with <code>newChar</code>.
            */
            String * replace(jchar oldChar, jchar newChar);

            /**
            * Converts all of the characters in this <code>String</code> to lower
            * case using the rules of the default locale.
            * <p>
            * If no character in the string has a different lowercase version
            * then the original string is returned. 
            * <p>
            * Otherwise, this method creates a new <code>String</code> object that 
            * represents a character sequence identical in length to the character 
            * sequence represented by this String object.
            * <p>Examples:
            * <blockquote><pre>
            * createString("French Fries")->toLowerCase() returns "french fries"
            * </pre></blockquote>
            *
            * @return  the string, converted to lowercase.
            */
            StringRef toLowerCase() const;

            /**
            * Converts all of the characters in this <code>String</code> to lower
            * case using the rules of the default locale.
            * <p>
            * If no character in the string has a different lowercase version,
            * then the original string is returned. 
            * <p>
            * Otherwise, this method creates a new <code>String</code> object that 
            * represents a character sequence identical in length to the character 
            * sequence represented by this String object.
            * <p>Examples:
            * <blockquote><pre>
            * createString("French Fries").toLowerCase() returns "french fries"
            * </pre></blockquote>
            *
            * @return  the string, converted to lowercase.
            */
            StringRef toUpperCase() const;

            /**
            * Removes white space from both ends of this string. 
            * <p>
            * If this <code>String</code> object represents an empty character 
            * sequence, or the first and last characters of character sequence 
            * represented by this <code>String</code> object both have codes 
            * greater than <code>'&#92;u0020'</code> (the space character), then a 
            * reference to this <code>String</code> object is returned. 
            * <p>
            * Otherwise, if there is no character with a code greater than 
            * <code>'&#92;u0020'</code> in the string, then a new 
            * <code>String</code> object representing an empty string is created
            * and returned.
            * <p>
            * Otherwise, let <i>k</i> be the index of the first character in the 
            * string whose code is greater than <code>'&#92;u0020'</code>, and let 
            * <i>m</i> be the index of the last character in the string whose code 
            * is greater than <code>'&#92;u0020'</code>. A new <code>String</code> 
            * object is created, representing the substring of this string that 
            * begins with the character at index <i>k</i> and ends with the 
            * character at index <i>m</i>-that is, the result of 
            * <code>this.substring(<i>k</i>,&nbsp;<i>m</i>+1)</code>.
            * <p>
            * This method may be used to trim whitespace from the beginning and end 
            * of a string; in fact, it trims all ASCII control characters as well.
            *
            * @return  this string, with white space removed from the front and end.
            */
            String * trim();

            /**
            * This object (which is already a string!) is itself returned. 
            *
            * @return  the string itself.
            */
            StringRef toString();

            /**
            * Converts this string to a new character array.
            *
            * @return  a newly allocated character array whose length is the length
            *          of this string and whose contents are initialized to contain
            *          the character sequence represented by this string.
            */
            jcharArray * toCharArray();

            /**
            * Returns the string representation of the <code>Object</code> argument. 
            *
            * @param   obj   an <code>Object</code>.
            * @return  if the argument is <code>null</code>, then a string equal to
            *          <code>"null"</code>; otherwise, the value of
            *          <code>obj->toString()</code> is returned.
            */
            static String * valueOf(const Object * obj);

            /**
            * Returns the string representation of the <code>jchar</code> array
            * argument. The contents of the character array are copied; subsequent 
            * modification of the character array does not affect the newly 
            * created string. 
            *
            * @param   data   a <code>char</code> array.
            * @return  a newly allocated string representing the same sequence of
            *          characters contained in the character array argument.
            */
            static String * valueOf(const jcharArray * data);

            /**
            * Returns the string representation of a specific subarray of the 
            * <code>jchar</code> array argument. 
            * <p>
            * The <code>offset</code> argument is the index of the first 
            * character of the subarray. The <code>count</code> argument 
            * specifies the length of the subarray. The contents of the subarray 
            * are copied; subsequent modification of the character array does not 
            * affect the newly created string. 
            *
            * @param   data     the character array.
            * @param   offset   the initial offset into the value of the
            *                  <code>String</code>.
            * @param   count    the length of the value of the <code>String</code>.
            * @return  a newly allocated string representing the sequence of
            *          characters contained in the subarray of the character array
            *          argument.
            * @exception NullPointerException if <code>data</code> is 
            *          <code>null</code>.
            * @exception IndexOutOfBoundsException if <code>offset</code> is 
            *          negative, or <code>count</code> is negative, or 
            *          <code>offset+count</code> is larger than 
            *          <code>data.length</code>.
            */
            static String * valueOf(const jcharArray * data, jint offset, jint count);

            /**
            * Returns a String that is equivalent to the specified character array.
            * It creates a new array and copies the characters into it.
            *
            * @param   data     the character array.
            * @param   offset   initial offset of the subarray.
            * @param   count    length of the subarray.
            * @return  a <code>String</code> that contains the characters of the
            *          specified subarray of the character array.
            */
            static String * copyValueOf(const jcharArray * data, jint offset, jint count);

            /**
            * Returns a String that is equivalent to the specified character array.
            * It creates a new array and copies the characters into it.
            *
            * @param   data   the character array.
            * @return  a <code>String</code> that contains the characters of the
            *          character array.
            */
            static String * copyValueOf(const jcharArray * data);

            /**
            * Returns the string representation of the <code>jboolean</code> argument. 
            *
            * @param   b   a <code>jboolean</code>.
            * @return  if the argument is <code>jtrue</code>, a string equal to
            *          <code>"true"</code> is returned; otherwise, a string equal to
            *          <code>"false"</code> is returned.
            */
            static String * valueOf(jboolean b);

            /**
            * Returns the string representation of the <code>jbyte</code> 
            * argument. 
            *
            * @param   b   a <code>jbyte</code>.
            * @return  a newly allocated string of length <code>1</code> containing
            *          as its single character the argument <code>c</code> converted 
            *          into a character according to the platform's default character 
            *          encoding.
            */
            static String * valueOf(jbyte b);

            /**
            * Returns the string representation of the <code>jchar</code> 
            * argument. 
            *
            * @param   c   a <code>jchar</code>.
            * @return  a newly allocated string of length <code>1</code> containing
            *          as its single character the argument <code>c</code>.
            */
            static String * valueOf(jchar c);

            /**
            * Returns the string representation of the <code>jshort</code> argument. 
            * <p>
            *
            * @param   s   an <code>jshort</code>.
            * @return  a newly allocated string containing a string representation of
            *          the <code>jshort</code> argument.
            */
            static String * valueOf(jshort s);

            /**
            * Returns the string representation of the <code>jint</code> argument. 
            * <p>
            *
            * @param   i   an <code>jint</code>.
            * @return  a newly allocated string containing a string representation of
            *          the <code>jint</code> argument.
            */
            static String * valueOf(jint i);

            /**
            * Returns the string representation of the <code>jlong</code> argument. 
            * <p>
            *
            * @param   l   a <code>jlong</code>.
            * @return  a newly allocated string containing a string representation of
            *          the <code>jlong</code> argument.
            */
            static String * valueOf(jlong l);

            /**
            * Returns the string representation of the <code>jfloat</code> argument. 
            * <p>
            *
            * @param   f   a <code>jfloat</code>.
            * @return  a newly allocated string containing a string representation of
            *          the <code>jfloat</code> argument.
            */
            static String * valueOf(jfloat f);

            /**
            * Returns the string representation of the <code>jdouble</code> argument. 
            * <p>
            *
            * @param   d   a <code>jdouble</code>.
            * @return  a newly allocated string containing a string representation of
            *          the <code>jdouble</code> argument.
            */
            static String * valueOf(jdouble d) ;

            /**
            * Creates an AString from this String. This method is misnamed as an AString
            * can contain characters with 8-bit values.
            *
            * @return a AStringRef 
            */
            AStringRef toAscii() const;

            /**
            * Creates an WString from this String
            *
            * @return a WStringRef 
            */
            WStringRef toWide() const;

            /**
            * Creates an UTF8String from this String
            *
            * @return a UTF8StringRef 
            */
            UTF8StringRef toUTF8() const;

            /**
            * This method's purpose is to indicate whether an AString can be extracted from this String 
            * without losing data. It is therefore misnamed, but left as is for applications that might
            * already be using it.
            *
            * @return <code>jtrue</code> if all jchars are 8-bit values (0-255); else <code>jfalse</code>
            */
            jboolean isAscii() const;

            /**
            * Returns a canonical representation for the string object. If s and t 
            * are strings such that s->equals(t), it is guaranteed that 
            *
            * s->intern() == t->intern(). 
            *
            * @return a string that has the same contents as this string,
            * but is guaranteed to be from a pool of unique strings. 
            */
            String * intern();

            /**
            * Appends the string representation of a subarray of the charArray str 
            * argument to this string. Characters of the character array str, starting
            * at index offset, are appended, in order, to the contents of this string.
            * The length of this string increases by the value of len.
            *
            * @param str the characters to be appended. 
            * @param offset the index of the first character to append. 
            * @param len the number of characters to append. 
            *
            */
            void append(const jcharArray * str, jint offset, jint len);

            /**
            * Appends the string representation of the jchar argument to this string
            * The argument is appended to the contents of this string. The length of
            * this string increases by 1.
            *
            * @param ch a jchar. 
            *
            */
            void append(jchar c);

            /**
            * Appends the argument string to this string.
            * The characters of the String argument are appended, in order,
            * to the contents of this string, increasing the length of this string
            * by the length of the argument.
            *
            * @param s a string. 
            *
            */
            void append(const String * s);

            /**
            * Appends the argument string to this string.
            * The characters of the String argument are appended, in order,
            * to the contents of this string, increasing the length of this string
            * by the length of the argument.
            *
            * @param s a string. 
            *
            */
            void append(const char* s);

            /**
            * For Sonic Software Internal use only: DO NOT USE.
            */
            ~String();

            /**
            * Returns the type-code for this class' classtype.
            *
            * @return     the type-code for this class.
            */
            static int        type();

            /**
            * Returns the type-code for this object's classtype.
            *
            * @return     the type-code for this object.
            */
            int       getType() const;

            /**
            * Indicates whether this object is an instance of the given classtype. 
            * An object is an instance of the given classtype if it is exactly
            * that classtype or derives from that classtype.
            *
            * @param   classtype   the type-code for the classtype with which to compare.
            * @return  <code>jtrue</code> if this object is the same as or derived from
            *          the given classtype;<code>jfalse</code> otherwise.
            */
            bool  instanceof(int classtype) const;

            String *operator+=(const String *rhs);
            String *operator+=(const char *rhs);
            String *operator+(const String *rhs);
            String *operator+(const char *rhs);

        private: void JLSreserved0();
        private: void JLSreserved1();
        }; // class String

        /**
        * The <code>AString</code> class represents a string of characters with 8-bit values.
        */
        class SMQJ_API AString : public Object
        {
        public:
            /**
            * For Sonic Software Internal use only: DO NOT USE.
            */
            ~AString();

            /**
            * Returns the type-code for this class' classtype.
            *
            * @return     the type-code for this class.
            */
            static int        type();

            /**
            * Returns the type-code for this object's classtype.
            *
            * @return     the type-code for this object.
            */
            int       getType() const;

            /**
            * Indicates whether this object is an instance of the given classtype. 
            * An object is an instance of the given classtype if it is exactly
            * that classtype or derives from that classtype.
            *
            * @param   classtype   the type-code for the classtype with which to compare.
            * @return  <code>jtrue</code> if this object is the same as or derived from
            *          the given classtype;<code>jfalse</code> otherwise.
            */
            bool  instanceof(int classtype) const;

            /**
            * Returns a char * to the string data. Note: the AString owns the
            * memory.
            *
            * @return char * pointer to the string
            */
            const char * getValue(void);

            /**
            * Return the length of a AString
            *
            * @return the length of this AString 
            */
            jint length() const;

            /**
            * Returns a char * to the string data. Note: the AString owns the
            * memory.
            *
            * @return char * pointer to the string
            */
            operator const char *() const;

            /**
            * Returns the character in the string at the given index.
            *
            * @return char the character in the string at the given index.
            * @exception ArrayIndexOutOfBoundsException if <code>index</code> is 
            *          negative, or <code>index</code> is larger than 
            *          <code>this->length()</code>.
            */
            char operator[](jint index) const;

        };

        /**
        * The <code>WString</code> class represents a wide character string.
        */
        class SMQJ_API WString : public Object
        {
        public:
            /**
            * For Sonic Software Internal use only: DO NOT USE.
            */
            ~WString();

            /**
            * Returns the type-code for this class' classtype.
            *
            * @return     the type-code for this class.
            */
            static int        type();

            /**
            * Returns the type-code for this object's classtype.
            *
            * @return     the type-code for this object.
            */
            int       getType() const;

            /**
            * Indicates whether this object is an instance of the given classtype. 
            * An object is an instance of the given classtype if it is exactly
            * that classtype or derives from that classtype.
            *
            * @param   classtype   the type-code for the classtype with which to compare.
            * @return  <code>jtrue</code> if this object is the same as or derived from
            *          the given classtype;<code>jfalse</code> otherwise.
            */
            bool  instanceof(int classtype) const;

            /**
            * Returns a wchar_t * to the Wide Charater string. Note: the WString owns the
            * memory.
            *
            * @return wchar_t * pointer to the string
            */
            const wchar_t * getValue(void);

            /**
            * Return the length of a WString
            *
            * @return the length of this WString 
            */
            jint length() const;

            /**
            * Returns a wchar_t * to the Wide Charater string. Note: the WString owns the
            * memory.
            *
            * @return wchar_t * pointer to the string
            */
            operator const wchar_t *() const;

            /**
            * Returns the character in the string at the given index.
            *
            * @return wchar_t the character in the string at the given index.
            * @exception ArrayIndexOutOfBoundsException if <code>index</code> is 
            *          negative, or <code>index</code> is larger than 
            *          <code>this->length()</code>.
            */
            wchar_t operator[](jint index) const;

        };

        /**
        * The <code>UTF8String</code> class represents a UTF8 character string.
        */
        class SMQJ_API UTF8String : public Object
        {
        public:
            /**
            * For Sonic Software Internal use only: DO NOT USE.
            */
            ~UTF8String();

            /**
            * Returns the type-code for this class' classtype.
            *
            * @return     the type-code for this class.
            */
            static int        type();

            /**
            * Returns the type-code for this object's classtype.
            *
            * @return     the type-code for this object.
            */
            int       getType() const;

            /**
            * Indicates whether this object is an instance of the given classtype. 
            * An object is an instance of the given classtype if it is exactly
            * that classtype or derives from that classtype.
            *
            * @param   classtype   the type-code for the classtype with which to compare.
            * @return  <code>jtrue</code> if this object is the same as or derived from
            *          the given classtype;<code>jfalse</code> otherwise.
            */
            bool  instanceof(int classtype) const;

            /**
            * Returns a unsigned char * to the UTF8 Charater string. Note: the UTF8String owns the
            * memory.
            *
            * @return unsigned char * pointer to the string
            */
            const unsigned char * getValue(void);

            /**
            * Return the length of a UTF8String in bytes 
            *
            * @return the length of this UTF8String in bytes
            */
            jint length() const;

            /**
            * Returns an unsigned char * to the UTF8 string. Note: the UTF8String owns the
            * memory.
            *
            * @return unsigned char * pointer to the string
            */
            operator const unsigned char *() const;

            /**
            * Returns the **byte** (not character!) in the string at the given index.
            *
            * @return unsigned char the byte in the string at the given index.
            * @exception ArrayIndexOutOfBoundsException if <code>index</code> is 
            *          negative, or <code>index</code> is larger than 
            *          <code>this->length()</code>.
            */
            unsigned char operator[](jint index) const;

        };

        // namespace operators

        /**
        * Concatenates the <code>rhs</code> string to the end of the <code>lhs</code> string. 
        * <p>
        * If the length of the <code>rhs</code> string is <code>0</code>, then <code>lhs</code> 
        * <code>String</code> object is returned. Otherwise, a new 
        * <code>String</code> object is created, representing a character 
        * sequence that is the concatenation of the character sequence 
        * represented by <code>lhs</code> <code>String</code> object and the character 
        * sequence represented by the <code>rhs</code> string.<p>
        * Example:
        * <blockquote><pre>
        * createString("cares") += createString("s") returns "caress"
        * </pre></blockquote>
        *
        * @param   lhs   the <code>String</code> that is concatenated to.
        * @param   rhs   the <code>String</code> that is concatenated to the end
        *                of <code>lhs</code> <code>String</code>.
        * @return  a string that represents the concatenation of <code>lhs</code> strings's
        *          characters followed by the <code>rhs</code> string's characters.
        * @exception java.lang.NullPointerException if either argument is 
        *          <code>null</code>.
        */
        SMQJ_API StringRef operator+=(StringRef lhs, const StringRef rhs);

        /**
        * Concatenates the <code>rhs</code> character array to the end of the <code>lhs</code> string. 
        * <p>
        * If the length of the <code>rhs</code> string is <code>0</code>, then <code>lhs</code> 
        * <code>String</code> object is returned. Otherwise, a new 
        * <code>String</code> object is created, representing a character 
        * sequence that is the concatenation of the character sequence 
        * represented by <code>lhs</code> <code>String</code> object and the character 
        * sequence represented by the <code>rhs</code> character array.<p>
        * Example:
        * <blockquote><pre>
        * createString("cares") += "s" returns "caress"
        * </pre></blockquote>
        *
        * @param   lhs   the <code>String</code> that is concatenated to.
        * @param   rhs   the <code>char *</code> that is concatenated to the end
        *                of <code>lhs</code> <code>String</code>.
        * @return  a string that represents the concatenation of <code>lhs</code> strings's
        *          characters followed by the <code>rhs</code> array of characters.
        * @exception java.lang.NullPointerException if either argument is 
        *          <code>null</code>.
        */
        SMQJ_API StringRef operator+=(StringRef lhs, const char *rhs);

        /**
        * Concatenates the <code>rhs</code> wide character array to the end of the <code>lhs</code> string. 
        * <p>
        * If the length of the <code>rhs</code> string is <code>0</code>, then <code>lhs</code> 
        * <code>String</code> object is returned. Otherwise, a new 
        * <code>String</code> object is created, representing a character 
        * sequence that is the concatenation of the character sequence 
        * represented by <code>lhs</code> <code>String</code> object and the character 
        * sequence represented by the <code>rhs</code> wide character array.<p>
        * Example:
        * <blockquote><pre>
        * createString("cares") += L"s" returns "caress"
        * </pre></blockquote>
        *
        * @param   lhs   the <code>String</code> that is concatenated to.
        * @param   rhs   the <code>wchar_t *</code> that is concatenated to the end
        *                of <code>lhs</code> <code>String</code>.
        * @return  a string that represents the concatenation of <code>lhs</code>strings's
        *          characters followed by the <code>rhs</code> array of characters.
        * @exception java.lang.NullPointerException if either argument is 
        *          <code>null</code>.
        */
        SMQJ_API StringRef operator+=(StringRef lhs, const wchar_t *rhs);

        /**
        * Concatenates the <code>rhs</code> string to the end of the <code>lhs</code> string. 
        * <p>
        * If the length of the <code>rhs</code> string is <code>0</code>, then <code>lhs</code> 
        * <code>String</code> object is returned. Otherwise, a new 
        * <code>String</code> object is created, representing a character 
        * sequence that is the concatenation of the character sequence 
        * represented by <code>lhs</code> <code>String</code> object and the character 
        * sequence represented by the <code>rhs</code> string.<p>
        * Example:
        * <blockquote><pre>
        * createString("cares") + createString("s") returns "caress"
        * </pre></blockquote>
        *
        * @param   lhs   the <code>String</code> that is concatenated to.
        * @param   rhs   the <code>String</code> that is concatenated to the end
        *                of <code>lhs</code> <code>String</code>.
        * @return  a string that represents the concatenation of <code>lhs</code> strings's
        *          characters followed by the <code>rhs</code> string's characters.
        * @exception java.lang.NullPointerException if either argument is 
        *          <code>null</code>.
        */
        SMQJ_API StringRef operator+(const StringRef lhs, const StringRef rhs);

        /**
        * Concatenates the <code>rhs</code> character array to the end of the <code>lhs</code> string. 
        * <p>
        * If the length of the <code>rhs</code> string is <code>0</code>, then <code>lhs</code> 
        * <code>String</code> object is returned. Otherwise, a new 
        * <code>String</code> object is created, representing a character 
        * sequence that is the concatenation of the character sequence 
        * represented by <code>lhs</code> <code>String</code> object and the character 
        * sequence represented by the <code>rhs</code> character array.<p>
        * Example:
        * <blockquote><pre>
        * createString("cares") + "s" returns "caress"
        * </pre></blockquote>
        *
        * @param   lhs   the <code>String</code> that is concatenated to.
        * @param   rhs   the <code>char *</code> that is concatenated to the end
        *                of <code>lhs</code> <code>String</code>.
        * @return  a string that represents the concatenation of <code>lhs</code> strings's
        *          characters followed by the <code>rhs</code> array of characters.
        * @exception java.lang.NullPointerException if either argument is 
        *          <code>null</code>.
        */
        SMQJ_API StringRef operator+(const StringRef lhs, const char *rhs);

        /**
        * Concatenates the <code>rhs</code> wide character array to the end of the <code>lhs</code> string. 
        * <p>
        * If the length of the <code>rhs</code> string is <code>0</code>, then <code>lhs</code> 
        * <code>String</code> object is returned. Otherwise, a new 
        * <code>String</code> object is created, representing a character 
        * sequence that is the concatenation of the character sequence 
        * represented by <code>lhs</code> <code>String</code> object and the character 
        * sequence represented by the <code>rhs</code> wide character array.<p>
        * Example:
        * <blockquote><pre>
        * createString("cares") + L"s" returns "caress"
        * </pre></blockquote>
        *
        * @param   lhs   the <code>String</code> that is concatenated to.
        * @param   rhs   the <code>wchar_t *</code> that is concatenated to the end
        *                of <code>lhs</code> <code>String</code>.
        * @return  a string that represents the concatenation of <code>lhs</code>strings's
        *          characters followed by the <code>rhs</code> array of characters.
        * @exception java.lang.NullPointerException if either argument is 
        *          <code>null</code>.
        */
        SMQJ_API StringRef operator+(const StringRef lhs, const wchar_t *rhs);

        /**
        * Concatenates the <code>rhs</code> string to the end of the <code>lhs</code> character array. 
        * <p>
        * If the length of the <code>rhs</code> <code>String</code> is <code>0</code>, then a string representing
        * the <code>lhs</code> character array is returned.Otherwise, a new 
        * <code>String</code> object is created, representing a character 
        * sequence that is the concatenation of the character sequence 
        * represented by <code>lhs</code> character array object and the character 
        * sequence represented by the <code>rhs</code> <code>String</code>.<p>
        * Example:
        * <blockquote><pre>
        * "cares" + createString("s") returns "caress"
        * </pre></blockquote>
        *
        * @param   lhs   the <code>char *</code> that is concatenated to.
        * @param   rhs   the <code>String</code> that is concatenated to the end
        *                of <code>lhs</code> <code>char *</code>.
        * @return  a string that represents the concatenation of <code>lhs</code> array of 
        *          characters followed by the <code>rhs</code> strings's characters.
        * @exception java.lang.NullPointerException if either argument is 
        *          <code>null</code>.
        */
        SMQJ_API StringRef operator+(const char *lhs, const StringRef rhs);

        /**
        * Concatenates the <code>rhs</code> string to the end of the <code>lhs</code> wide character array. 
        * <p>
        * If the length of the <code>rhs</code> <code>String</code> is <code>0</code>, then a string representing
        * the <code>lhs</code> wide character array is returned. Otherwise, a new 
        * <code>String</code> object is created, representing a character 
        * sequence that is the concatenation of the character sequence 
        * represented by <code>lhs</code> wide character array object and the character 
        * sequence represented by the <code>rhs</code> <code>String</code>.<p>
        * Example:
        * <blockquote><pre>
        * L"cares" + createString("s") returns "caress"
        * </pre></blockquote>
        *
        * @param   lhs   the <code>wchar_t *</code> that is concatenated to.
        * @param   rhs   the <code>String</code> that is concatenated to the end
        *                of <code>lhs</code> <code>char *</code>.
        * @return  a string that represents the concatenation of <code>lhs</code> array of 
        *          characters followed by the <code>rhs</code> strings's characters.
        * @exception java.lang.NullPointerException if either argument is 
        *          <code>null</code>.
        */
        SMQJ_API StringRef operator+(const wchar_t *lhs, const StringRef rhs);

    } // namespace lang
} // namespace java

#endif // _JAVA_LANG_STRING_H_
