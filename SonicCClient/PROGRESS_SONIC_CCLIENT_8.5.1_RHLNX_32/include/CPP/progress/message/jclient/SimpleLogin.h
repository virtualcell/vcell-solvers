#ifndef _SimpleLogin_H_
#define _SimpleLogin_H_

/*
 * Copyright (c) 2001 - 2008 Progress Software Corporation. All Rights Reserved.
 * This software is the confidential and proprietary information of Progress
 * Software Corporation ("Confidential Information").  You shall not
 * disclose such Confidential Information and shall use it only in
 * accordance with the terms of the license agreement you entered into
 * with Progress Software Corporation.
 */

#include <progress/message/jclient/package.h>

namespace progress { namespace message { namespace jclient {

// This is a default implementation for the ILoginSPI interface.
// A simple implementation which return the raw bytes in the credentials,
// optionally with encoding for e.g "UTF-8"

SMQ_API ILoginSPIRef createSimpleLogin(const char* enc = 0);

class SMQ_API SimpleLogin : public ILoginSPI
{
  public:
    SimpleLogin(const char* enc = 0);
    ~SimpleLogin();

    void setUsername(StringRef);
    StringRef getUsername();

    void setPassword(StringRef);
    StringRef getPassword();

    void setTransformedUsername(StringRef);
    StringRef getTransformedUsername();

    void setTransformedPassword(jbyteArrayRef);
    jbyteArrayRef getTransformedPassword();

    jboolean login();

  private:
    char*       m_enc;
    StringRef   m_user;
    StringRef   m_pw;
};

}}} // namespace progress::message::jclient

#endif // _SimpleLogin_H_

