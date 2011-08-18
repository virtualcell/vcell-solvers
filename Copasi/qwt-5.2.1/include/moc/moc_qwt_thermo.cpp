/****************************************************************************
** Meta object code from reading C++ file 'qwt_thermo.h'
**
** Created: Fri May 6 14:53:32 2011
**      by: The Qt Meta Object Compiler version 62 (Qt 4.7.3)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../qwt_thermo.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'qwt_thermo.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 62
#error "This file was generated using the moc from 4.7.3. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_QwtThermo[] = {

 // content:
       5,       // revision
       0,       // classname
       0,    0, // classinfo
       1,   14, // methods
      12,   19, // properties
       1,   55, // enums/sets
       0,    0, // constructors
       0,       // flags
       0,       // signalCount

 // slots: signature, parameters, type, tag, flags
      15,   11,   10,   10, 0x0a,

 // properties: name, type, flags
      39,   32, 0x42095103,
      57,   50, 0x43095103,
      73,   68, 0x01095103,
      93,   86, 0x06095103,
     113,  104, 0x0009510b,
     131,  127, 0x02095103,
     143,   32, 0x42095103,
     153,   50, 0x43095103,
     163,   86, 0x06095103,
     172,   86, 0x06095103,
     181,  127, 0x02095103,
     191,   86, 0x06095103,

 // enums: name, flags, count, data
     104, 0x0,    5,   59,

 // enum data: key, value
     197, uint(QwtThermo::NoScale),
     205, uint(QwtThermo::LeftScale),
     215, uint(QwtThermo::RightScale),
     226, uint(QwtThermo::TopScale),
     235, uint(QwtThermo::BottomScale),

       0        // eod
};

static const char qt_meta_stringdata_QwtThermo[] = {
    "QwtThermo\0\0val\0setValue(double)\0QBrush\0"
    "alarmBrush\0QColor\0alarmColor\0bool\0"
    "alarmEnabled\0double\0alarmLevel\0ScalePos\0"
    "scalePosition\0int\0borderWidth\0fillBrush\0"
    "fillColor\0maxValue\0minValue\0pipeWidth\0"
    "value\0NoScale\0LeftScale\0RightScale\0"
    "TopScale\0BottomScale\0"
};

const QMetaObject QwtThermo::staticMetaObject = {
    { &QWidget::staticMetaObject, qt_meta_stringdata_QwtThermo,
      qt_meta_data_QwtThermo, 0 }
};

#ifdef Q_NO_DATA_RELOCATION
const QMetaObject &QwtThermo::getStaticMetaObject() { return staticMetaObject; }
#endif //Q_NO_DATA_RELOCATION

const QMetaObject *QwtThermo::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->metaObject : &staticMetaObject;
}

void *QwtThermo::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_QwtThermo))
        return static_cast<void*>(const_cast< QwtThermo*>(this));
    if (!strcmp(_clname, "QwtAbstractScale"))
        return static_cast< QwtAbstractScale*>(const_cast< QwtThermo*>(this));
    return QWidget::qt_metacast(_clname);
}

int QwtThermo::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QWidget::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: setValue((*reinterpret_cast< double(*)>(_a[1]))); break;
        default: ;
        }
        _id -= 1;
    }
#ifndef QT_NO_PROPERTIES
      else if (_c == QMetaObject::ReadProperty) {
        void *_v = _a[0];
        switch (_id) {
        case 0: *reinterpret_cast< QBrush*>(_v) = alarmBrush(); break;
        case 1: *reinterpret_cast< QColor*>(_v) = alarmColor(); break;
        case 2: *reinterpret_cast< bool*>(_v) = alarmEnabled(); break;
        case 3: *reinterpret_cast< double*>(_v) = alarmLevel(); break;
        case 4: *reinterpret_cast< ScalePos*>(_v) = scalePosition(); break;
        case 5: *reinterpret_cast< int*>(_v) = borderWidth(); break;
        case 6: *reinterpret_cast< QBrush*>(_v) = fillBrush(); break;
        case 7: *reinterpret_cast< QColor*>(_v) = fillColor(); break;
        case 8: *reinterpret_cast< double*>(_v) = maxValue(); break;
        case 9: *reinterpret_cast< double*>(_v) = minValue(); break;
        case 10: *reinterpret_cast< int*>(_v) = pipeWidth(); break;
        case 11: *reinterpret_cast< double*>(_v) = value(); break;
        }
        _id -= 12;
    } else if (_c == QMetaObject::WriteProperty) {
        void *_v = _a[0];
        switch (_id) {
        case 0: setAlarmBrush(*reinterpret_cast< QBrush*>(_v)); break;
        case 1: setAlarmColor(*reinterpret_cast< QColor*>(_v)); break;
        case 2: setAlarmEnabled(*reinterpret_cast< bool*>(_v)); break;
        case 3: setAlarmLevel(*reinterpret_cast< double*>(_v)); break;
        case 4: setScalePosition(*reinterpret_cast< ScalePos*>(_v)); break;
        case 5: setBorderWidth(*reinterpret_cast< int*>(_v)); break;
        case 6: setFillBrush(*reinterpret_cast< QBrush*>(_v)); break;
        case 7: setFillColor(*reinterpret_cast< QColor*>(_v)); break;
        case 8: setMaxValue(*reinterpret_cast< double*>(_v)); break;
        case 9: setMinValue(*reinterpret_cast< double*>(_v)); break;
        case 10: setPipeWidth(*reinterpret_cast< int*>(_v)); break;
        case 11: setValue(*reinterpret_cast< double*>(_v)); break;
        }
        _id -= 12;
    } else if (_c == QMetaObject::ResetProperty) {
        _id -= 12;
    } else if (_c == QMetaObject::QueryPropertyDesignable) {
        _id -= 12;
    } else if (_c == QMetaObject::QueryPropertyScriptable) {
        _id -= 12;
    } else if (_c == QMetaObject::QueryPropertyStored) {
        _id -= 12;
    } else if (_c == QMetaObject::QueryPropertyEditable) {
        _id -= 12;
    } else if (_c == QMetaObject::QueryPropertyUser) {
        _id -= 12;
    }
#endif // QT_NO_PROPERTIES
    return _id;
}
QT_END_MOC_NAMESPACE
