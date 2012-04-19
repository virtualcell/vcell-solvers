/*
 * Copyright (c) 2001 - 2008 Progress Software Corporation. All Rights Reserved.
 * This software is the confidential and proprietary information of Progress
 * Software Corporation ("Confidential Information").  You shall not
 * disclose such Confidential Information and shall use it only in
 * accordance with the terms of the license agreement you entered into
 * with Progress Software Corporation.
 */

#ifndef _PROGRESS_MESSAGE_JCLIENT_MESSAGEPRODUCER_H_
#define _PROGRESS_MESSAGE_JCLIENT_MESSAGEPRODUCER_H_

#include <java/lang/package.h>
#include <progress/message/jclient/package_decls.h>


namespace progress { namespace message { namespace jclient {

class SMQ_API MessageProducer : public java::lang::Object
{
public:
	virtual ~MessageProducer();

	/**
	 * Returns the int corresponding to the MessageProducer type.
	 *
	 * @return the int corresponding to the MessageProducer type
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
     * Set whether message IDs are disabled.
     *
     * <P>Since message ID's take some effort to create and increase a
     * message's size, some JMS providers may be able to optimize message
     * overhead if they are given a hint that message ID is not used by
     * an application. JMS message Producers provide a hint to disable
     * message ID. When a client sets a Producer to disable message ID
     * they are saying that they do not depend on the value of message
     * ID for the messages it produces. These messages must either have
     * message ID set to null or, if the hint is ignored, messageID must
     * be set to its normal unique value.
     *
     * <P>Message IDs are enabled by default.
     *
     * @param value indicates if message IDs are disabled.
     *
     * @exception JMSException if JMS fails to set disabled message
     *                         Id due to some internal error.
     */
    virtual void setDisableMessageID(jboolean value);

    /**
     * Get an indication of whether message IDs are disabled.
     *
     * @return an indication of whether message IDs are disabled.
     *
     * @exception JMSException if JMS fails to get disabled message
     *                         Id due to some internal error.
     */
    virtual jboolean getDisableMessageID();

    /**
     * Set whether message timestamps are disabled.
     *
     * <P>Since timestamps take some effort to create and increase a
     * message's size, some JMS providers may be able to optimize message
     * overhead if they are given a hint that timestamp is not used by an
     * application. JMS message Producers provide a hint to disable
     * timestamps. When a client sets a producer to disable timestamps
     * they are saying that they do not depend on the value of timestamp
     * for the messages it produces. These messages must either have
     * timestamp set to null or, if the hint is ignored, timestamp must
     * be set to its normal value.
     *
     * <P>Message timestamps are enabled by default.
     *
     * @param value indicates if message timestamps are disabled.
     *
     * @exception JMSException if JMS fails to set disabled message
     *                         timestamp due to some internal error.
     */
    virtual void setDisableMessageTimestamp(jboolean value);

    /**
     * Get an indication of whether message timestamps are disabled.
     *
     * @return an indication of whether message IDs are disabled.
     *
     * @exception JMSException if JMS fails to get disabled message
     *                         timestamp due to some internal error.
     */
    virtual jboolean getDisableMessageTimestamp();

    /**
     * Set the producer's default delivery mode.
     *
     * <P>Delivery mode is set to PERSISTENT by default.
     *
     * @param deliveryMode the message delivery mode for this message
     * producer.
     *
     * @exception JMSException if JMS fails to set delivery mode
     *                         due to some internal error.
     */
    virtual void setDeliveryMode(jint deliveryMode);

    /**
     * Get the producer's default delivery mode.
     *
     * @return the message delivery mode for this message producer.
     *
     * @exception JMSException if JMS fails to get delivery mode
     *                         due to some internal error.
     */
    virtual jint getDeliveryMode();

    /**
     * Set the producer's default priority.
     *
     * <P>Priority is set to 4, by default.
     *
     * @param priority the message priority for this message producer.
     *
     * @exception JMSException if JMS fails to set priority
     *                         due to some internal error.
     */
    virtual void setPriority(jint priority);

    /**
     * Get the producer's default priority.
     *
     * @return the message priority for this message producer.
     *
     * @exception JMSException if JMS fails to get priority
     *                         due to some internal error.
     */
    virtual jint getPriority();

    /**
     * Set the default length of time in milliseconds from its dispatch time
     * that a produced message should be retained by the message system.
     *
     * <P>Time to live is set to zero by default.
     *
     * @param timeToLive the message time to live in milliseconds; zero is
     * unlimited
     *
     * @exception JMSException if JMS fails to set Time to Live
     *                         due to some internal error.
     */
    virtual void setTimeToLive(jlong timeToLive);

    /**
     * Get the default length of time in milliseconds from its dispatch time
     * that a produced message should be retained by the message system.
     *
     * @return the message time to live in milliseconds; zero is unlimited
     *
     * @exception JMSException if JMS fails to get Time to Live
     *                         due to some internal error.
     */
    virtual jlong getTimeToLive();

    /**
     * Closes the message producer.
     *
     * Since the C-Client may allocate some resources on behalf of a message producer,
     * clients should close them when they are not needed.
     *
     * @exception JMSException if JMS fails to close the producer
     *                         due to some error.
     */
    virtual void close();

    private: void PMJMPreserved0();
};

}}} // namespace progress::message::jclient

#endif // _PROGRESS_MESSAGE_JCLIENT_MESSAGEPRODUCER_H_
