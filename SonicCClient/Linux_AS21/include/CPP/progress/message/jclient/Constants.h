#ifndef _PROGRESS_MESSAGE_JCLIENT_CONSTANTS_H_
#define _PROGRESS_MESSAGE_JCLIENT_CONSTANTS_H_

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

class SMQ_API Constants
{

public:
    /**
     * Gets name of message property that, if true, causes an administrative event to be 
     * sent to the management tools when the message is declared undeliverable.
     * This is a SonicMQ extenstion to the Java Messaging Specification.
     */
	static const StringRef getNOTIFY_UNDELIVERED();
    /**
     * Gets name of message property that, if true, causes the message to be 
     * enqueued on the dead message queue when it is declared undeliverable.
     * This is a SonicMQ extenstion to the Java Messaging Specification.
     */
	static const StringRef getPRESERVE_UNDELIVERED();
    /**
     * Gets name of message property that describes the reason a message was declared
     * to be undeliverable. This property is set by the SonicMQ broker when a 
     * message is enqueued on the dead message queue.
     */
	static const StringRef getUNDELIVERED_REASON_CODE();
    /**
     * Gets name of message property that states the time when a message was declared
     * to be undeliverable. This property is set by the SonicMQ broker when a 
     * message is enqueued on the dead message queue.
     */
	static const StringRef getUNDELIVERED_TIMESTAMP();
    /**
     * Connection state for an active JMS connection (normal state)
     */
    static const jint ACTIVE;
    /**
     * Connection state for a reconnecting fault-tolerant JMS connection. The connection has detected a failure
     * and is attempting to reconnect automatically.
     */
    static const jint RECONNECTING;
    /**
     * Connection state for a failed JMS connection.
     */
    static const jint FAILED;
    /**
     * Connection state constant for a closed JMS connection
     */
    static const jint CLOSED;
};

}}} // namespace


#endif // _PROGRESS_MESSAGE_JCLIENT_CONSTANTS_H_
