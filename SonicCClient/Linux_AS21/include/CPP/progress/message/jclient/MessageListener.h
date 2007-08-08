#ifndef _PROGRESS_MESSAGE_JCLIENT_MessageListener_H_
#define _PROGRESS_MESSAGE_JCLIENT_MessageListener_H_
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

#include <java/lang/package.h>
#include <progress/message/jclient/package_decls.h>

namespace progress { namespace message { namespace jclient {

class SMQ_API MessageListener
{
public:
    /**
     * Creates a MessageListener
     */
    MessageListener();

	virtual ~MessageListener();
  
    /**
     * virtual method expected to be overridden by user.
     */
	virtual void onMessage(progress::message::jclient::MessageRef msg) = 0;
	/**
	 * Returns the int corresponding to the MessageListener type.
	 *
	 * @return the int corresponding to the MessageListener type
	 */
	static  int type();
    /**
     * Returns the type-code for this object's classtype.
     *
     * @return     the type-code for this object.
     */
	virtual int getType() const;
};

}}} // namespace progress::message::jclient

#endif // _PROGRESS_MESSAGE_JCLIENT_MessageListener_H_
