/****************************************************************************
** Meta object code from reading C++ file 'qwt_dial.h'
**
** Created: Fri May 6 14:53:31 2011
**      by: The Qt Meta Object Compiler version 62 (Qt 4.7.3)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../qwt_dial.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'qwt_dial.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 62
#error "This file was generated using the moc from 4.7.3. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_QwtDial[] = {

 // content:
       5,       // revision
       0,       // classname
       0,    0, // classinfo
       0,    0, // methods
       7,   14, // properties
       3,   35, // enums/sets
       0,    0, // constructors
       0,       // flags
       0,       // signalCount

 // properties: name, type, flags
      13,    8, 0x01095003,
      35,   31, 0x02095103,
      52,   45, 0x0009510b,
      69,   64, 0x0009510b,
      81,   74, 0x06095103,
      88,    8, 0x01095103,
     107,   97, 0x0009510b,

 // enums: name, flags, count, data
      45, 0x0,    3,   47,
      64, 0x0,    2,   53,
      97, 0x0,    2,   57,

 // enum data: key, value
     117, uint(QwtDial::Plain),
     123, uint(QwtDial::Raised),
     130, uint(QwtDial::Sunken),
     137, uint(QwtDial::RotateNeedle),
     150, uint(QwtDial::RotateScale),
     162, uint(QwtDial::Clockwise),
     172, uint(QwtDial::CounterClockwise),

       0        // eod
};

static const char qt_meta_stringdata_QwtDial[] = {
    "QwtDial\0bool\0visibleBackground\0int\0"
    "lineWidth\0Shadow\0frameShadow\0Mode\0"
    "mode\0double\0origin\0wrapping\0Direction\0"
    "direction\0Plain\0Raised\0Sunken\0"
    "RotateNeedle\0RotateScale\0Clockwise\0"
    "CounterClockwise\0"
};

const QMetaObject QwtDial::staticMetaObject = {
    { &QwtAbstractSlider::staticMetaObject, qt_meta_stringdata_QwtDial,
      qt_meta_data_QwtDial, 0 }
};

#ifdef Q_NO_DATA_RELOCATION
const QMetaObject &QwtDial::getStaticMetaObject() { return staticMetaObject; }
#endif //Q_NO_DATA_RELOCATION

const QMetaObject *QwtDial::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->metaObject : &staticMetaObject;
}

void *QwtDial::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_QwtDial))
        return static_cast<void*>(const_cast< QwtDial*>(this));
    return QwtAbstractSlider::qt_metacast(_clname);
}

int QwtDial::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QwtAbstractSlider::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    
#ifndef QT_NO_PROPERTIES
     if (_c == QMetaObject::ReadProperty) {
        void *_v = _a[0];
        switch (_id) {
        case 0: *reinterpret_cast< bool*>(_v) = hasVisibleBackground(); break;
        case 1: *reinterpret_cast< int*>(_v) = lineWidth(); break;
        case 2: *reinterpret_cast< Shadow*>(_v) = frameShadow(); break;
        case 3: *reinterpret_cast< Mode*>(_v) = mode(); break;
        case 4: *reinterpret_cast< double*>(_v) = origin(); break;
        case 5: *reinterpret_cast< bool*>(_v) = wrapping(); break;
        case 6: *reinterpret_cast< Direction*>(_v) = direction(); break;
        }
        _id -= 7;
    } else if (_c == QMetaObject::WriteProperty) {
        void *_v = _a[0];
        switch (_id) {
        case 0: showBackground(*reinterpret_cast< bool*>(_v)); break;
        case 1: setLineWidth(*reinterpret_cast< int*>(_v)); break;
        case 2: setFrameShadow(*reinterpret_cast< Shadow*>(_v)); break;
        case 3: setMode(*reinterpret_cast< Mode*>(_v)); break;
        case 4: setOrigin(*reinterpret_cast< double*>(_v)); break;
        case 5: setWrapping(*reinterpret_cast< bool*>(_v)); break;
        case 6: setDirection(*reinterpret_cast< Direction*>(_v)); break;
        }
        _id -= 7;
    } else if (_c == QMetaObject::ResetProperty) {
        _id -= 7;
    } else if (_c == QMetaObject::QueryPropertyDesignable) {
        _id -= 7;
    } else if (_c == QMetaObject::QueryPropertyScriptable) {
        _id -= 7;
    } else if (_c == QMetaObject::QueryPropertyStored) {
        _id -= 7;
    } else if (_c == QMetaObject::QueryPropertyEditable) {
        _id -= 7;
    } else if (_c == QMetaObject::QueryPropertyUser) {
        _id -= 7;
    }
#endif // QT_NO_PROPERTIES
    return _id;
}
QT_END_MOC_NAMESPACE
