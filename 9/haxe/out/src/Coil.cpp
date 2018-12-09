// Generated by Haxe 3.4.4
#include <hxcpp.h>

#ifndef INCLUDED_Coil
#include <Coil.h>
#endif
#ifndef INCLUDED_Std
#include <Std.h>
#endif

HX_DEFINE_STACK_FRAME(_hx_pos_6697e5740b83ac6d_41_new,"Coil","new",0x497d8621,"Coil.new","Nine.hx",41,0x3ffcb0ac)
HX_LOCAL_STACK_FRAME(_hx_pos_6697e5740b83ac6d_54_toString,"Coil","toString",0xddd59f4b,"Coil.toString","Nine.hx",54,0x3ffcb0ac)
HX_LOCAL_STACK_FRAME(_hx_pos_6697e5740b83ac6d_47_toString,"Coil","toString",0xddd59f4b,"Coil.toString","Nine.hx",47,0x3ffcb0ac)
HX_LOCAL_STACK_FRAME(_hx_pos_6697e5740b83ac6d_57_insert,"Coil","insert",0xd784dc18,"Coil.insert","Nine.hx",57,0x3ffcb0ac)
HX_LOCAL_STACK_FRAME(_hx_pos_6697e5740b83ac6d_67_delete,"Coil","delete",0xa480590a,"Coil.delete","Nine.hx",67,0x3ffcb0ac)
HX_LOCAL_STACK_FRAME(_hx_pos_6697e5740b83ac6d_73_move,"Coil","move",0x03b63630,"Coil.move","Nine.hx",73,0x3ffcb0ac)

void Coil_obj::__construct(int val){
            	HX_STACKFRAME(&_hx_pos_6697e5740b83ac6d_41_new)
HXLINE(  42)		this->left = hx::ObjectPtr<OBJ_>(this);
HXLINE(  43)		this->right = hx::ObjectPtr<OBJ_>(this);
HXLINE(  44)		this->val = val;
            	}

Dynamic Coil_obj::__CreateEmpty() { return new Coil_obj; }

void *Coil_obj::_hx_vtable = 0;

Dynamic Coil_obj::__Create(hx::DynamicArray inArgs)
{
	hx::ObjectPtr< Coil_obj > _hx_result = new Coil_obj();
	_hx_result->__construct(inArgs[0]);
	return _hx_result;
}

bool Coil_obj::_hx_isInstanceOf(int inClassId) {
	return inClassId==(int)0x00000001 || inClassId==(int)0x2c9de1af;
}

::String Coil_obj::toString(){
            		HX_BEGIN_LOCAL_FUNC_S0(hx::LocalFunc,_hx_Closure_0) HXARGC(1)
            		int _hx_run( ::Coil c){
            			HX_STACKFRAME(&_hx_pos_6697e5740b83ac6d_54_toString)
HXLINE(  54)			return c->val;
            		}
            		HX_END_LOCAL_FUNC1(return)

            	HX_STACKFRAME(&_hx_pos_6697e5740b83ac6d_47_toString)
HXLINE(  48)		::Array< ::Dynamic> vals = ::Array_obj< ::Dynamic>::__new(1)->init(0,hx::ObjectPtr<OBJ_>(this));
HXLINE(  49)		 ::Coil k = this->right;
HXLINE(  50)		while(hx::IsNotEq( k,hx::ObjectPtr<OBJ_>(this) )){
HXLINE(  51)			vals->push(k);
HXLINE(  52)			k = k->right;
            		}
HXLINE(  54)		return (HX_("",00,00,00,00) + ::Std_obj::string(vals->map( ::Dynamic(new _hx_Closure_0()))));
            	}


HX_DEFINE_DYNAMIC_FUNC0(Coil_obj,toString,return )

 ::Coil Coil_obj::insert(int val){
            	HX_GC_STACKFRAME(&_hx_pos_6697e5740b83ac6d_57_insert)
HXLINE(  58)		 ::Coil c =  ::Coil_obj::__alloc( HX_CTX ,val);
HXLINE(  60)		c->right = this->right;
HXLINE(  61)		this->right->left = c;
HXLINE(  63)		c->left = hx::ObjectPtr<OBJ_>(this);
HXLINE(  64)		this->right = c;
HXLINE(  65)		return c;
            	}


HX_DEFINE_DYNAMIC_FUNC1(Coil_obj,insert,return )

 ::Coil Coil_obj::_hx_delete(){
            	HX_STACKFRAME(&_hx_pos_6697e5740b83ac6d_67_delete)
HXLINE(  68)		this->left->right = this->right;
HXLINE(  69)		this->right->left = this->left;
HXLINE(  70)		return this->right;
            	}


HX_DEFINE_DYNAMIC_FUNC0(Coil_obj,_hx_delete,return )

 ::Coil Coil_obj::move(int n){
            	HX_STACKFRAME(&_hx_pos_6697e5740b83ac6d_73_move)
HXLINE(  74)		if ((n == (int)0)) {
HXLINE(  74)			return hx::ObjectPtr<OBJ_>(this);
            		}
HXLINE(  75)		if ((n < (int)0)) {
HXLINE(  75)			 ::Coil _hx_tmp = this->left;
HXDLIN(  75)			return _hx_tmp->move((n + (int)1));
            		}
HXLINE(  76)		if ((n > (int)0)) {
HXLINE(  76)			 ::Coil _hx_tmp1 = this->right;
HXDLIN(  76)			return _hx_tmp1->move((n - (int)1));
            		}
HXLINE(  77)		return hx::ObjectPtr<OBJ_>(this);
            	}


HX_DEFINE_DYNAMIC_FUNC1(Coil_obj,move,return )


hx::ObjectPtr< Coil_obj > Coil_obj::__new(int val) {
	hx::ObjectPtr< Coil_obj > __this = new Coil_obj();
	__this->__construct(val);
	return __this;
}

hx::ObjectPtr< Coil_obj > Coil_obj::__alloc(hx::Ctx *_hx_ctx,int val) {
	Coil_obj *__this = (Coil_obj*)(hx::Ctx::alloc(_hx_ctx, sizeof(Coil_obj), true, "Coil"));
	*(void **)__this = Coil_obj::_hx_vtable;
	__this->__construct(val);
	return __this;
}

Coil_obj::Coil_obj()
{
}

void Coil_obj::__Mark(HX_MARK_PARAMS)
{
	HX_MARK_BEGIN_CLASS(Coil);
	HX_MARK_MEMBER_NAME(left,"left");
	HX_MARK_MEMBER_NAME(right,"right");
	HX_MARK_MEMBER_NAME(val,"val");
	HX_MARK_END_CLASS();
}

void Coil_obj::__Visit(HX_VISIT_PARAMS)
{
	HX_VISIT_MEMBER_NAME(left,"left");
	HX_VISIT_MEMBER_NAME(right,"right");
	HX_VISIT_MEMBER_NAME(val,"val");
}

hx::Val Coil_obj::__Field(const ::String &inName,hx::PropertyAccess inCallProp)
{
	switch(inName.length) {
	case 3:
		if (HX_FIELD_EQ(inName,"val") ) { return hx::Val( val ); }
		break;
	case 4:
		if (HX_FIELD_EQ(inName,"left") ) { return hx::Val( left ); }
		if (HX_FIELD_EQ(inName,"move") ) { return hx::Val( move_dyn() ); }
		break;
	case 5:
		if (HX_FIELD_EQ(inName,"right") ) { return hx::Val( right ); }
		break;
	case 6:
		if (HX_FIELD_EQ(inName,"insert") ) { return hx::Val( insert_dyn() ); }
		if (HX_FIELD_EQ(inName,"delete") ) { return hx::Val( _hx_delete_dyn() ); }
		break;
	case 8:
		if (HX_FIELD_EQ(inName,"toString") ) { return hx::Val( toString_dyn() ); }
	}
	return super::__Field(inName,inCallProp);
}

hx::Val Coil_obj::__SetField(const ::String &inName,const hx::Val &inValue,hx::PropertyAccess inCallProp)
{
	switch(inName.length) {
	case 3:
		if (HX_FIELD_EQ(inName,"val") ) { val=inValue.Cast< int >(); return inValue; }
		break;
	case 4:
		if (HX_FIELD_EQ(inName,"left") ) { left=inValue.Cast<  ::Coil >(); return inValue; }
		break;
	case 5:
		if (HX_FIELD_EQ(inName,"right") ) { right=inValue.Cast<  ::Coil >(); return inValue; }
	}
	return super::__SetField(inName,inValue,inCallProp);
}

void Coil_obj::__GetFields(Array< ::String> &outFields)
{
	outFields->push(HX_HCSTRING("left","\x07","\x08","\xb0","\x47"));
	outFields->push(HX_HCSTRING("right","\xdc","\x0b","\x64","\xe9"));
	outFields->push(HX_HCSTRING("val","\xe1","\xde","\x59","\x00"));
	super::__GetFields(outFields);
};

#if HXCPP_SCRIPTABLE
static hx::StorageInfo Coil_obj_sMemberStorageInfo[] = {
	{hx::fsObject /*::Coil*/ ,(int)offsetof(Coil_obj,left),HX_HCSTRING("left","\x07","\x08","\xb0","\x47")},
	{hx::fsObject /*::Coil*/ ,(int)offsetof(Coil_obj,right),HX_HCSTRING("right","\xdc","\x0b","\x64","\xe9")},
	{hx::fsInt,(int)offsetof(Coil_obj,val),HX_HCSTRING("val","\xe1","\xde","\x59","\x00")},
	{ hx::fsUnknown, 0, null()}
};
static hx::StaticInfo *Coil_obj_sStaticStorageInfo = 0;
#endif

static ::String Coil_obj_sMemberFields[] = {
	HX_HCSTRING("left","\x07","\x08","\xb0","\x47"),
	HX_HCSTRING("right","\xdc","\x0b","\x64","\xe9"),
	HX_HCSTRING("val","\xe1","\xde","\x59","\x00"),
	HX_HCSTRING("toString","\xac","\xd0","\x6e","\x38"),
	HX_HCSTRING("insert","\x39","\x43","\xdd","\x9d"),
	HX_HCSTRING("delete","\x2b","\xc0","\xd8","\x6a"),
	HX_HCSTRING("move","\x11","\xe3","\x60","\x48"),
	::String(null()) };

static void Coil_obj_sMarkStatics(HX_MARK_PARAMS) {
	HX_MARK_MEMBER_NAME(Coil_obj::__mClass,"__mClass");
};

#ifdef HXCPP_VISIT_ALLOCS
static void Coil_obj_sVisitStatics(HX_VISIT_PARAMS) {
	HX_VISIT_MEMBER_NAME(Coil_obj::__mClass,"__mClass");
};

#endif

hx::Class Coil_obj::__mClass;

void Coil_obj::__register()
{
	hx::Object *dummy = new Coil_obj;
	Coil_obj::_hx_vtable = *(void **)dummy;
	hx::Static(__mClass) = new hx::Class_obj();
	__mClass->mName = HX_HCSTRING("Coil","\xaf","\xe1","\x9d","\x2c");
	__mClass->mSuper = &super::__SGetClass();
	__mClass->mConstructEmpty = &__CreateEmpty;
	__mClass->mConstructArgs = &__Create;
	__mClass->mGetStaticField = &hx::Class_obj::GetNoStaticField;
	__mClass->mSetStaticField = &hx::Class_obj::SetNoStaticField;
	__mClass->mMarkFunc = Coil_obj_sMarkStatics;
	__mClass->mStatics = hx::Class_obj::dupFunctions(0 /* sStaticFields */);
	__mClass->mMembers = hx::Class_obj::dupFunctions(Coil_obj_sMemberFields);
	__mClass->mCanCast = hx::TCanCast< Coil_obj >;
#ifdef HXCPP_VISIT_ALLOCS
	__mClass->mVisitFunc = Coil_obj_sVisitStatics;
#endif
#ifdef HXCPP_SCRIPTABLE
	__mClass->mMemberStorageInfo = Coil_obj_sMemberStorageInfo;
#endif
#ifdef HXCPP_SCRIPTABLE
	__mClass->mStaticStorageInfo = Coil_obj_sStaticStorageInfo;
#endif
	hx::_hx_RegisterClass(__mClass->mName, __mClass);
}
