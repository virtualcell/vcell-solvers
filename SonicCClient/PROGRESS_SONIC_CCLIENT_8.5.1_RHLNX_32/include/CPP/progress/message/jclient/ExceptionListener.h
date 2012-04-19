#ifndef _PROGRESS_MESSAGE_JCLIENT_ExceptionListener_H_
#define _PROGRESS_MESSAGE_JCLIENT_ExceptionListener_H_
/*
 * Copyright (c) 2001 - 2008 Progress Software Corporation. All Rights Reserved.
 * This software is the confidential and proprietary information of Progress
 * Software Corporation ("Confidential Information").  You shall not
 * disclose such Confidential Information and shall use it only in
 * accordance with the terms of the license agreement you entered into
 * with Progress Software Corporation.
 */

#include <java/lang/package.h>
#include <progress/message/jclient/package_decls.h>

namespace progress { namespace message { namespace jclient {

class SMQ_API ExceptionListener
{
public:
    /**
     * Creates a ExceptionListener
     */
    ExceptionListener();

	virtual ~ExceptionListener();
  
    /**
     * virtual method expected to be overridden by user.
     */
	virtual void onException(JMSExceptionRef exc) = 0;
	/**
	 * Returns the int corresponding to the ExceptionListener type.
	 *
	 * @return the int corresponding to the ExceptionListener type
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

#endif // _PROGRESS_MESSAGE_JCLIENT_ExceptionListener_H_
