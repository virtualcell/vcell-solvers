#ifndef _PROGRESS_MESSAGE_JCLIENT_ERRORCODES_H_
#define _PROGRESS_MESSAGE_JCLIENT_ERRORCODES_H_

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

#include <java/lang/package.h>
#include <progress/message/jclient/package_decls.h>

namespace progress { namespace message { namespace jclient {

class SMQ_API ErrorCodes
{
public:
	/**
	 * Tests the error code associated with a JMSException against
	 * an integer value associated with a Progress SonicMQ ErrorCode.
	 *
	 * @param anException A JMSException that needs to be tested.
	 * @param aCode An error code specified as an integer.
	 *
	 * @returns True, if the error code of the exception is the one specified.
	 */
	static jboolean testException(JMSExceptionRef anException, jint aCode);
};

}}} // namespace


#endif // _PROGRESS_MESSAGE_JCLIENT_ERRORCODES_H_
