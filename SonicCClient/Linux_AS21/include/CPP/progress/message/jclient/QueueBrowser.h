#ifndef _PROGRESS_MESSAGE_JCLIENT_QUEUEBROWSER_H_
#define _PROGRESS_MESSAGE_JCLIENT_QUEUEBROWSER_H_
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

#include <progress/message/jclient/package_decls.h>
#include <java/util/Enumeration.h>


namespace progress { namespace message { namespace jclient {


/** A client uses a QueueBrowser to look at messages on a queue without
 * removing them.
 *
 * <P>The browse methods return a java.util.Enumeration that is used to
 * scan the queue's messages. It may be an enumeration of the entire content
 * of a queue or it may only contain the messages matching a message selector.
 *
 * <P>Messages may be arriving and expiring while the scan is done. JMS does
 * not require the content of an enumeration to be a static snapshot of queue
 * content. Whether these changes are visible or not depends on the JMS provider.
 *
 */
class SMQ_API QueueBrowser : public java::lang::Object //implements javax.jms.QueueBrowser
{
public:
    virtual ~QueueBrowser();

    /**
     * Returns the int corresponding to the QueueBrowser type.
     *
     * @return the int corresponding to the QueueBrowser type
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

    /** 
     * Closes the queue browser.
     *
     * Since the C-Client may allocate some resources on behalf of a message consumer,
     * clients should close them when they are not needed.
     *
     * @exception JMSException if JMS fails to close the consumer
     *                         due to some error.
     */
    virtual void close();

    /** Get the queue associated with this queue Browser.
      *
      * @return the queue
      *
      * @exception JMSException if JMS fails to get queue for
      *                         this queue Browser
      *                         due to some internal error.
      */
    virtual QueueRef getQueue();

    /** Get this queue browser's message selector expression.
      *
      * @return this queue browser's message selector
      *
      * @exception JMSException if JMS fails to get the message
      *                         selector for this queue Browser
      *                         due to some internal error.
      */
    virtual StringRef getMessageSelector();

    /** Get an enumeration for browsing the current queue
      * messages in the order they would be received.
      *
      * @return an enumeration for browsing the messages
      *
      * @exception JMSException if JMS fails to get the enumeration
      *                         for this queue Browser
      *                         due to some internal error.
      */
    virtual EnumerationRef getEnumeration();

    private: void PMJQBreserved0();
    private: void PMJQBreserved1();
    private: void PMJQBreserved2();
    private: void PMJQBreserved3();
    private: void PMJQBreserved4();
    private: void PMJQBreserved5();
    private: void PMJQBreserved6();
    private: void PMJQBreserved7();
    private: void PMJQBreserved8();
    private: void PMJQBreserved9();
    private: void PMJQBreserved10();
    private: void PMJQBreserved11();
    private: void PMJQBreserved12();
    private: void PMJQBreserved13();
    private: void PMJQBreserved14();
    private: void PMJQBreserved15();
    private: void PMJQBreserved16();
};

}}} // namespace progress::message::jclient

#endif // _PROGRESS_MESSAGE_JCLIENT_QUEUEBROWSER_H_
