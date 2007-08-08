#ifndef _PROGRESS_MESSAGE_JCLIENT_TOPIC_H_
#define _PROGRESS_MESSAGE_JCLIENT_TOPIC_H_
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

#include <progress/message/jclient/Destination.h>
using java::lang::StringRef;


namespace progress { namespace message { namespace jclient {

SMQ_API TopicRef createTopic(StringRef topicName);

/** A Topic object encapsulates a provider-specific topic name. It is the 
  * way a client specifies the identity of a topic to JMS methods.
  *
  * <P>Many Pub/Sub providers group topics into hierarchies and provide 
  * various options for subscribing to parts of the hierarchy. JMS places 
  * no restriction on what a Topic object represents. It might be a leaf 
  * in a topic hierarchy or it might be a larger part of the hierarchy.
  *
  * <P>The organization of topics and the granularity of subscriptions to 
  * them is an important part of a Pub/Sub application's architecture. JMS 
  * does not specify a policy for how this should be done. If an application 
  * takes advantage of a provider-specific topic grouping mechanism, it 
  * should document this. If the application is installed using a different 
  * provider, it is the job of the administrator to construct an equivalent 
  * topic architecture and create equivalent Topic objects.
  */
class SMQ_API Topic : public Destination
{
public:
    virtual ~Topic();

    /**
     * Returns the int corresponding to the Topic type.
     *
     * @return the int corresponding to the Topic type
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

    /** Return a pretty printed version of the topic name.
      *
      * @return the provider specific identity values for this topic.
      */
    virtual StringRef toString();
    
    /** Get the name of this topic.
      *  
      * <P>Clients that depend upon the name are not portable.
      *  
      * @return the topic name
      *  
      * @exception JMSException if JMS implementation for Topic fails to
      *                         to return topic name due to some internal
      *                         error.
      */ 
    virtual StringRef getTopicName() ;

    private: void PMJTreserved0();
    private: void PMJTreserved1();
    private: void PMJTreserved2();
};

}}} // namespace progress::message::jclient

#endif // _PROGRESS_MESSAGE_JCLIENT_TOPIC_H_
