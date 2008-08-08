#ifndef _INCLUDE_REFERENCE_H_
#define _INCLUDE_REFERENCE_H_
/*
 * Copyright (c) 2001 - 2008 Progress Software Corporation. All Rights Reserved.
 * This software is the confidential and proprietary information of Progress
 * Software Corporation ("Confidential Information").  You shall not
 * disclose such Confidential Information and shall use it only in
 * accordance with the terms of the license agreement you entered into
 * with Progress Software Corporation.
 */

#ifndef HPUX
#pragma warning(disable : 4786) // disable warnings for truncation of debug info
#endif

// Forward decl for java.lang.Object
namespace java { namespace lang {
    class Object;
    class ObjectRef;
}}

#if defined(HPUX)
// Pending resolution of linker issues, disable inlining of ref methods
#else
#define USE_INLINE_SMQJ_REFS
#endif

namespace java 
{
    namespace lang
    {
#ifdef USE_INLINE_SMQJ_REFS
        class SMQJ_API ObjectRef
        {
        public:
            ObjectRef() : pObj(0) {}
            ObjectRef(const ObjectRef& ref) : pObj(0) { *this = ref; }
            ObjectRef(Object* p_) : pObj(0)
            { 
                if (p_ != 0) { 
                    pObj = p_; 
                    pObj->addRef(); 
                } 
            } 

            virtual ~ObjectRef() 
            {
                if (pObj) 
                    pObj->release(); 
                pObj = 0;  
            } 

            Object* getObj() const { return pObj; }
            Object& operator*() { return *(pObj); } 
            Object* operator->() { return pObj; } 

            ObjectRef& operator=(const ObjectRef &p_)
            { 
                if (this == &p_) { 
                    return *this; 
                } 
                if (p_.pObj) { 
                    p_.pObj->addRef(); 
                } 
                if (pObj) { 
                    pObj->release(); 
                } 
                pObj = p_.pObj; 
                return *this; 
            } 

            ObjectRef& operator=(const Object* p_)
            { 
                if (pObj == const_cast<Object *>(p_)) { 
                    return *this;
                } 
                if (p_) { 
                    const_cast<Object *>(p_)->addRef(); 
                } 
                if (pObj) { 
                    pObj->release(); 
                }
                pObj = const_cast<Object*>(p_); 
                return *this; 
            } 

            int operator==(Object* p_) const { return (pObj == p_) ? 1 : 0; } 
            int operator!=(Object* p_) const { return (pObj != p_) ? 1 : 0; }
            operator Object*() { return pObj; }
            operator const Object*() const { return pObj; }

            bool instanceof(int classtype) const
            {
                if (pObj) {
                    return pObj->instanceof(classtype);
                }
                return false;
            }

            static ObjectRef cast(const Object *p_);

        protected:
            Object* pObj;
        };
#else
        class SMQJ_API ObjectRef
        {
        public:
            ObjectRef();
            ObjectRef(const ObjectRef& ref);
            ObjectRef(Object* p_);
            virtual ~ObjectRef();
            Object* getObj() const;
            Object& operator*();
            Object* operator->();
            ObjectRef& operator=(const ObjectRef &p_);
            ObjectRef& operator=(const Object* p_);
            int operator==(Object* p_) const;
            int operator!=(Object* p_) const;
            operator Object*();
            operator const Object*() const;
            bool instanceof(int classtype) const;
            static ObjectRef cast(const Object *p_);
        protected:
            Object* pObj;
        };
#endif
    } // namespace lang
} // namespace java

using java::lang::ObjectRef;

/*
 * Reference class common method declarations.
 *
 * T = Class type
 * B = Base class type
 * REF_SUFFIX = string to append to reference class name (usually Ref or RefBase)
 */
#define DECLARE_INLINE_REF_MEMBERS(T,B,REF_SUFFIX,CAST_TYPE) \
public: \
T##REF_SUFFIX() : B##Ref() {} \
T##REF_SUFFIX(const T##REF_SUFFIX& ref) : B##Ref() { *this = ref; } \
T##REF_SUFFIX(T* p_) : B##Ref() \
{ \
    if (p_ != 0) { \
        pObj = p_; \
        pObj->addRef(); \
    } \
} \
\
virtual ~T##REF_SUFFIX() { if (pObj) pObj->release(); pObj = 0; } \
T& operator*() { return *(CAST_TYPE<T*>(pObj)); } \
T* operator->() { return CAST_TYPE<T*>(pObj); } \
\
T##REF_SUFFIX& operator=(const T##REF_SUFFIX &p_) \
{ \
    if (this == &p_) { \
        return *this; \
    } \
    if (p_.pObj) { \
        p_.pObj->addRef(); \
    } \
    if (pObj) { \
        pObj->release(); \
    } \
    pObj = p_.pObj; \
    return *this; \
} \
\
T##REF_SUFFIX& operator=(const T* p_) \
{ \
    if (pObj == const_cast<T*>(p_)) { \
        return *this; \
    } \
    if (p_) { \
        const_cast<T*>(p_)->addRef(); \
    } \
    if (pObj) { \
        pObj->release(); \
    } \
    pObj = const_cast<T*>(p_); \
    return *this; \
} \
\
int operator==(T* p_) const { return (int)(pObj == p_); }\
int operator!=(T* p_) const { return (int)(pObj != p_); }\
operator T*() { return CAST_TYPE<T*>(pObj); }\
operator const T*() const { return CAST_TYPE<T*>(pObj); }\
\
bool instanceof(int classtype) const\
{\
    if (pObj) {\
        return pObj->instanceof(classtype);\
    }\
    return false;\
}\
static T##REF_SUFFIX cast(const Object *p_);


/*
 * Reference class common method declarations.
 *
 * T = Class type
 * B = Base class type
 * REF_SUFFIX = string to append to reference class name (usually Ref or RefBase)
 */
#define DECLARE_REF_MEMBERS(T,B,REF_SUFFIX) \
    public: \
        T##REF_SUFFIX(); \
        T##REF_SUFFIX(const T##REF_SUFFIX& ref); \
        T##REF_SUFFIX(T* p_); \
        virtual ~T##REF_SUFFIX(); \
        T& operator*(); \
        T* operator->(); \
        T##REF_SUFFIX& operator=(const T##REF_SUFFIX &p_); \
        T##REF_SUFFIX& operator=(const T* p_); \
        int operator==(T* p_) const; \
        int operator!=(T* p_) const; \
        operator T*(); \
        operator const T*() const; \
        static T##REF_SUFFIX cast(const java::lang::Object *p_); \
        bool instanceof(int classtype) const;


#ifdef USE_INLINE_SMQJ_REFS
#define SMQJ_DECLARE_REF(T,B) \
class SMQJ_API T##Ref: public B##Ref \
{ \
    DECLARE_INLINE_REF_MEMBERS(T,B,Ref,static_cast) \
};
#else
#define SMQJ_DECLARE_REF(T,B) \
class SMQJ_API T##Ref: public B##Ref \
{ \
    DECLARE_REF_MEMBERS(T,B,Ref) \
};
#endif

#define SMQ_DECLARE_REF(T,B) \
class SMQ_API T##Ref: public B##Ref \
{ \
    DECLARE_REF_MEMBERS(T,B,Ref) \
};


/*
 * Reference class method declarations for array base classes.
 *
 * T = Class type
 * B = Base class type
 */
#ifdef USE_INLINE_SMQJ_REFS
#define SMQJ_DECLARE_REF_BASE(T,B) \
class SMQJ_API T##RefBase: public B##Ref \
{ \
    DECLARE_INLINE_REF_MEMBERS(T,B, RefBase, static_cast) \
};
#else
#define SMQJ_DECLARE_REF_BASE(T,B) \
class SMQJ_API T##RefBase: public B##Ref \
{ \
    DECLARE_REF_MEMBERS(T,B, RefBase) \
};
#endif

#define SMQ_DECLARE_REF_BASE(T,B) \
class SMQ_API T##RefBase: public B##Ref \
{ \
    DECLARE_REF_MEMBERS(T,B, RefBase) \
};

/*
 * Reference class method declarations for array-like classes,
 * such as java::lang::String.
 *
 * T = Class type
 * R = Array element class (e.g. jchar)
 * B = Base class type
 */
#define DECLARE_INLINE_REF2_MEMBERS(T, R, B) \
    DECLARE_INLINE_REF_MEMBERS(T, B, Ref,static_cast) \
    R operator[](jint index) const { return static_cast<T*>(pObj)->operator[](index); } \
    operator const R * () const { return static_cast<const R *>(*static_cast<T*>(pObj)); }

#define DECLARE_REF2_MEMBERS(T, R, B) \
    DECLARE_REF_MEMBERS(T, B, Ref) \
    R operator[](jint index) const;\
    operator const R * () const;


#ifdef USE_INLINE_SMQJ_REFS
#define SMQJ_DECLARE_REF2(T, R, B) \
class SMQJ_API T##Ref : public B##Ref \
{ \
    DECLARE_INLINE_REF2_MEMBERS(T, R, B) \
};
#else
#define SMQJ_DECLARE_REF2(T, R, B) \
class SMQJ_API T##Ref : public B##Ref \
{ \
    DECLARE_REF2_MEMBERS(T, R, B) \
};
#endif

#define SMQ_DECLARE_REF2(T, R, B) \
class SMQ_API T##Ref : public B##Ref \
{ \
    DECLARE_REF2_MEMBERS(T, R, B) \
};


#endif //_INCLUDE_REFERENCE_H_
