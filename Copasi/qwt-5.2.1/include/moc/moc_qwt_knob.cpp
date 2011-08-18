/****************************************************************************
** Meta object code from reading C++ file 'qwt_knob.h'
**
** Created: Fri May 6 14:53:31 2011
**      by: The Qt Meta Object Compiler version 62 (Qt 4.7.3)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../qwt_knob.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'qwt_knob.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 62
#error "This file was generated using the moc from 4.7.3. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_QwtKnob[] = {

 // content:
       5,       // revision
       0,       // classname
       0,    0, // classinfo
       0,    0, // methods
       4,   14, // properties
       1,   26, // enums/sets
       0,    0, // constructors
       0,       // flags
       0,       // signalCount

 // properties: name, type, flags
      12,    8, 0x02095103,
      22,    8, 0x02095103,
      41,   34, 0x06095103,
      59,   52, 0x0009510b,

 // enums: name, flags, count, data
      52, 0x0,    2,   30,

 // enum data: key, value
      66, uint(QwtKnob::Line),
      71, uint(QwtKnob::Dot),

       0        // eod
};

static const char qt_meta_stringdata_QwtKnob[] = {
    "QwtKnob\0int\0knobWidth\0borderWidth\0"
    "double\0totalAngle\0Symbol\0symbol\0Line\0"
    "Dot\0"
};

const QMetaObject QwtKnob::staticMetaObject = {
    { &QwtAbstractSlider::staticMetaObject, qt_meta_stringdata_QwtKnob,
      qt_meta_data_QwtKnob, 0 }
};

#ifdef Q_NO_DATA_RELOCATION
const QMetaObject &QwtKnob::getStaticMetaObject() { return staticMetaObject; }
#endif //Q_NO_DATA_RELOCATION

const QMetaObject *QwtKnob::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->metaObject : &staticMetaObject;
}

void *QwtKnob::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_QwtKnob))
        return static_cast<void*>(const_cast< QwtKnob*>(this));
    if (!strcmp(_clname, "QwtAbstractScale"))
        return static_cast< QwtAbstractScale*>(const_cast< QwtKnob*>(this));
    return QwtAbstractSlider::qt_metacast(_clname);
}

int QwtKnob::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QwtAbstractSlider::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    
#ifndef QT_NO_PROPERTIES
     if (_c == QMetaObject::ReadProperty) {
        void *_v = _a[0];
        switch (_id) {
        case 0: *reinterpret_cast< int*>(_v) = knobWidth(); break;
        case 1: *reinterpret_cast< int*>(_v) = borderWidth(); break;
        case 2: *reinterpret_cast< double*>(_v) = totalAngle(); break;
        case 3: *reinterpret_cast< Symbol*>(_v) = symbol(); break;
        }
        _id -= 4;
    } else if (_c == QMetaObject::WriteProperty) {
        void *_v = _a[0];
        switch (_id) {
        case 0: setKnobWidth(*reinterpret_cast< int*>(_v)); break;
        case 1: setBorderWidth(*reinterpret_cast< int*>(_v)); break;
        case 2: setTotalAngle(*reinterpret_cast< double*>(_v)); break;
        case 3: setSymbol(*reinterpret_cast< Symbol*>(_v)); break;
        }
        _id -= 4;
    } else if (_c == QMetaObject::ResetProperty) {
        _id -= 4;
    } else if (_c == QMetaObject::QueryPropertyDesignable) {
        _id -= 4;
    } else if (_c == QMetaObject::QueryPropertyScriptable) {
        _id -= 4;
    } else if (_c == QMetaObject::QueryPropertyStored) {
        _id -= 4;
    } else if (_c == QMetaObject::QueryPropertyEditable) {
        _id -= 4;
    } else if (_c == QMetaObject::QueryPropertyUser) {
        _id -= 4;
    }
#endif // QT_NO_PROPERTIES
    return _id;
}
QT_END_MOC_NAMESPACE
