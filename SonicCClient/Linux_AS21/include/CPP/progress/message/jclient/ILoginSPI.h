#ifndef _PROGRESS_MESSAGE_JCLIENT_ILoginSPI_H_
#define _PROGRESS_MESSAGE_JCLIENT_ILoginSPI_H_
/*
 * Copyright (c) 2002 Sonic Software Corporation. All Rights Reserved.
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


#include <progress/message/jclient/package_decls.h>

namespace progress { namespace message { namespace jclient {

/**
 * JMS TopicSubscriber interface implemented as an abstract base class 
 */
class SMQ_API ILoginSPI : public java::lang::Object
{
public:

	/**
	 * Returns the int corresponding to the ILoginSPI type.
	 *
	 * @return the int corresponding to the ILoginSPI type
	 */
	static  int type();
    /**
     * Returns the type-code for this object's classtype.
     *
     * @return     the type-code for this object.
     */
	virtual int getType() const;
    /**
     * Indicates whether this object is an instance of the given classtype. 
     * An object is an instance of the given classtype if it is exactly
     * that classtype or derives from that classtype.
     *
     * @param   classtype   the type-code for the classtype with which to compare.
     * @return  <code>jtrue</code> if this object is the same as or derived from
     *          the given classtype;<code>jfalse</code> otherwise.
     */
	virtual bool instanceof(int classtype) const;

public:
    ILoginSPI();

	virtual ~ILoginSPI();

	/**
	 * Sets the unauthenticated username.
     * This method will be accessed by the C-Client library
     * to set the username prior to invoking the login method.
	 *
	 */
	virtual void setUsername(StringRef username) = 0;

    /**
     * Returns the unauthenticated user name as a String.
     * This method will be accessed by the C-Client library
     * to set the password prior to invoking the login method.
     *
     * @return     the unauthenticated user name as a String.
     */
	virtual StringRef getUsername() = 0;

	/**
	 * Sets the unauthenticated password.
     * This method wil be accessed by the C-Client library
	 *
	 */
	virtual void setPassword(StringRef password) = 0;

    /**
     * Returns the unauthenticated password as a String.
     *
     * @return     the unauthenticated password as a String.
     */
	virtual StringRef getPassword() = 0;


	/**
	 * Sets the authenticated username.
     * This should be called by the login method to set the username
     * after external authentication has successfully completed.
	 *
	 */
	virtual void setTransformedUsername(StringRef transformedUsername) = 0;

    /**
     * Returns the authenticated user name as a String.
     *
     * @return     the authenticated user name as a String.
     */
	virtual StringRef getTransformedUsername() = 0;

	/**
	 * Sets the authenticated password byte array.
     * This should be called by the login method to set the password
     * after external authentication has successfully completed.
	 *
	 */
	virtual void setTransformedPassword(jbyteArrayRef transformedPassword) = 0;

    /**
     * Returns the authenticated password as a byte array.
     *
     * @return     the authenticated password as a byte array.
     */
	virtual jbyteArrayRef getTransformedPassword() = 0;

  	/**
	 * User defined authentication method.
	 *
	 */
	virtual jboolean login() = 0;

};

}}} // namespace progress::message::jclient

/** C++ interface to register the LoginSPI for use with XA logins.  Users should set
 *  the login SPI prior to invocation of xa_open.
 *  When used with 4.0 brokers only clear text passwords are supported.  Users should
 *  use the setPassword method to set the clear text password.
 *  When used with 5.0 brokers both clear text and encrypted (byte array) 
 *  passwords are supported.  Users should use the setPassword method to set the clear 
 *  text password and/or use setTransformedPassword to set the encrypted byte array
 *  password.
 *
 * @param map - the map that implements the necessary login SPI routines
 */
SMQ_API void xaSetLoginSPI (progress::message::jclient::ILoginSPI *loginSPI);

#endif // _PROGRESS_MESSAGE_JCLIENT_ILoginSPI_H_
