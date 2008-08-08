#ifndef _INCLUDE_ARRAY_H_
#define _INCLUDE_ARRAY_H_
/*
 * Copyright (c) 2001 - 2008 Progress Software Corporation. All Rights Reserved.
 * This software is the confidential and proprietary information of Progress
 * Software Corporation ("Confidential Information").  You shall not
 * disclose such Confidential Information and shall use it only in
 * accordance with the terms of the license agreement you entered into
 * with Progress Software Corporation.
 */

#ifdef WIN32 // Windows compiler bug workaround
#define CLONE_RET_TYPE( T ) Object
#else
#define CLONE_RET_TYPE( T ) T##Array
#endif

//------------------------------------------------------------
// DECLARE_ARRAY_MEMBERS
//------------------------------------------------------------
#define DECLARE_ARRAY_MEMBERS(T)\
    private:\
        T##Array();\
    public:\
        virtual ~T##Array();\
        T& operator[](jint index);\
        T operator[](jint index) const;\
        static int type();\
        virtual bool instanceof(int classtype) const;\
        virtual int getType() const;\
        virtual jint getLength();\
        virtual CLONE_RET_TYPE(T) * clone();\
        virtual const T * getArray() const;

//------------------------------------------------------------
// DECLARE_ARRAY_MEMBERS_2
//------------------------------------------------------------
#define DECLARE_ARRAY_MEMBERS_2(T)\
    DECLARE_ARRAY_MEMBERS(T)

//------------------------------------------------------------
// DECLARE_OBJ_ARRAY_MEMBERS
//------------------------------------------------------------
#define DECLARE_OBJ_ARRAY_MEMBERS(T)\
    public:\
        virtual ~T##Array();\
        T##Ref& operator[](jint index);\
        T##Ref operator[](jint index) const;\
        static int type();\
        virtual bool instanceof(int classtype) const;\
        virtual int getType() const;\
        virtual jint getLength();\
        virtual CLONE_RET_TYPE(T) * clone();

//------------------------------------------------------------
// SMQJ_DECLARE_ARRAY
//------------------------------------------------------------
#define SMQJ_DECLARE_ARRAY(T)\
class SMQJ_API T##Array : public Object \
{\
    DECLARE_ARRAY_MEMBERS(T)\
};

//------------------------------------------------------------
// SMQ_DECLARE_ARRAY
//------------------------------------------------------------
#define SMQ_DECLARE_ARRAY(T)\
class SMQ_API T##Array : public Object \
{\
    DECLARE_ARRAY_MEMBERS(T)\
};

//------------------------------------------------------------
// SMQJ_DECLARE_ARRAY_2
//------------------------------------------------------------
#define SMQJ_DECLARE_ARRAY_2(T)\
class SMQJ_API T##Array : public Object \
{\
    DECLARE_ARRAY_MEMBERS_2(T)\
};

//------------------------------------------------------------
// SMQ_DECLARE_ARRAY_2
//------------------------------------------------------------
#define SMQ_DECLARE_ARRAY_2(T)\
class SMQ_API T##Array : public Object \
{\
    DECLARE_ARRAY_MEMBERS_2(T)\
};

//------------------------------------------------------------
// SMQJ_DECLARE_OBJ_ARRAY
//------------------------------------------------------------
// Use this set of array macros for arrays of obj (refs)
// Note: T should be the object and not the obj ref.
#define SMQJ_DECLARE_OBJ_ARRAY(T)\
class SMQJ_API T##Array : public Object \
{\
    DECLARE_OBJ_ARRAY_MEMBERS(T)\
};

//------------------------------------------------------------
// SMQ_DECLARE_OBJ_ARRAY
//------------------------------------------------------------
// Use this set of array macros for arrays of obj (refs)
// Note: T should be the object and not the obj ref.
#define SMQ_DECLARE_OBJ_ARRAY(T)\
class SMQ_API T##Array : public Object \
{\
    DECLARE_OBJ_ARRAY_MEMBERS(T)\
};

//------------------------------------------------------------
// DECLARE_ARRAYREF_METHODS
//------------------------------------------------------------
#define DECLARE_ARRAYREF_METHODS(T)\
public:\
    T##ArrayRef() {}\
    T##ArrayRef(const T##ArrayRef& ptr) : T##ArrayRefBase(ptr) {}\
    T##ArrayRef(T##Array * p_) : T##ArrayRefBase(p_) {}\
    virtual ~T##ArrayRef() {}\
    T##ArrayRef& operator=(const T##ArrayRef &p_)\
    {\
        if (this == &p_) {\
            return * this;\
        }\
        if (p_.pObj) {\
            p_.pObj->addRef();\
        }\
        if (pObj) {\
            pObj->release();\
        }\
        pObj = p_.pObj;\
        return * this;\
    }\
    T##ArrayRef& operator=(const T##Array * p_)\
    {\
        if (pObj == const_cast<T##Array * >(p_)) {\
            return * this;\
        }\
        if (p_) {\
            const_cast<T##Array * >(p_)->addRef();\
        }\
        if (pObj) {\
            pObj->release();\
        }\
        pObj = const_cast<T##Array * >(p_);\
        return * this;\
    }\
    bool instanceof(int classtype) const\
    {\
        if (pObj) {\
            return pObj->instanceof(classtype);\
        }\
        return false;\
    }\
    static T##ArrayRef cast(const Object * p_)\
    {\
        return (T##ArrayRef) T##ArrayRefBase::cast(p_);\
    }

//------------------------------------------------------------
// DECLARE_ARRAYREF_BRACKET_OPERATORS
//------------------------------------------------------------
#define DECLARE_ARRAYREF_BRACKET_OPERATORS(T)\
public:\
    T& operator[](jint index)\
    {\
        return ( * static_cast<T##Array * >(pObj))[index];\
    }\
    T operator[](jint index) const\
    {\
        return ( * static_cast<T##Array * >(pObj))[index];\
    }

//------------------------------------------------------------
// DECLARE_OBJ_ARRAYREF_BRACKET_OPERATORS
//------------------------------------------------------------
#define DECLARE_OBJ_ARRAYREF_BRACKET_OPERATORS(T)\
public:\
    T##Ref& operator[](jint index)\
    {\
        return ( * static_cast<T##Array * >(pObj))[index];\
    }\
    T##Ref operator[](jint index) const\
    {\
        return ( * static_cast<T##Array * >(pObj))[index];\
    }

//------------------------------------------------------------
// SMQJ_DECLARE_ARRAYREF
//------------------------------------------------------------
#define SMQJ_DECLARE_ARRAYREF(T)\
SMQJ_DECLARE_REF_BASE(T##Array,Object)\
class SMQJ_API T##ArrayRef : public T##ArrayRefBase\
{\
    DECLARE_ARRAYREF_METHODS(T)\
    DECLARE_ARRAYREF_BRACKET_OPERATORS(T)\
};\
SMQJ_API T##ArrayRef create##T##Array(int size);\
SMQJ_API T##ArrayRef create##T##Array(T * data, int size);

//------------------------------------------------------------
// SMQJ_DECLARE_ARRAYREF_2
//------------------------------------------------------------
#define SMQJ_DECLARE_ARRAYREF_2(T)\
SMQJ_DECLARE_ARRAYREF(T)\
SMQJ_API T##ArrayRef create##T##Array(T * str);

//------------------------------------------------------------
// SMQJ_DECLARE_OBJ_ARRAYREF
//------------------------------------------------------------
// Use this set of array ref macros for refs to arrays of obj (refs)
// Note that T should be the object and not the obj ref, and that 
// these macros pull in the declaration for the obj base.
#define SMQJ_DECLARE_OBJ_ARRAYREF(T)\
SMQJ_DECLARE_REF_BASE(T##Array, Object) \
class SMQJ_API T##ArrayRef : public T##ArrayRefBase\
{\
    DECLARE_ARRAYREF_METHODS(T)\
    DECLARE_OBJ_ARRAYREF_BRACKET_OPERATORS(T)\
};\
SMQJ_API T##ArrayRef create##T##Array(int size);

//------------------------------------------------------------
// SMQ_DECLARE_ARRAYREF
//------------------------------------------------------------
#define SMQ_DECLARE_ARRAYREF(T)\
SMQ_DECLARE_REF_BASE(T##Array,Object)\
class SMQ_API T##ArrayRef : public T##ArrayRefBase\
{\
    DECLARE_ARRAYREF_METHODS(T)\
    DECLARE_ARRAYREF_BRACKET_OPERATORS(T)\
};\
SMQ_API T##ArrayRef create##T##Array(int size);\
SMQ_API T##ArrayRef create##T##Array(T * data, int size);

//------------------------------------------------------------
// SMQ_DECLARE_ARRAYREF_2
//------------------------------------------------------------
#define SMQ_DECLARE_ARRAYREF_2(T)\
SMQ_DECLARE_ARRAYREF(T)\
SMQ_API T##ArrayRef create##T##Array(T * str);

//------------------------------------------------------------
// SMQ_DECLARE_OBJ_ARRAYREF
//------------------------------------------------------------
// Use this set of array ref macros for refs to arrays of obj (refs)
// Note that T should be the object and not the obj ref, and that 
// these macros pull in the declaration for the obj base.
#define SMQ_DECLARE_OBJ_ARRAYREF(T)\
SMQ_DECLARE_REF_BASE(T##Array, Object) \
class SMQ_API T##ArrayRef : public T##ArrayRefBase\
{\
    DECLARE_ARRAYREF_METHODS(T)\
    DECLARE_OBJ_ARRAYREF_BRACKET_OPERATORS(T)\
};\
SMQ_API T##ArrayRef create##T##Array(int size);

//------------------------------------------------------------
// Primitive Array Definitions.
//------------------------------------------------------------

SMQJ_DECLARE_ARRAY(jbyte);
SMQJ_DECLARE_ARRAYREF(jbyte);

SMQJ_DECLARE_ARRAY_2(jchar);
SMQJ_DECLARE_ARRAYREF(jchar);


#endif // _INCLUDE_ARRAY_H_
