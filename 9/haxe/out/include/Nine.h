// Generated by Haxe 3.4.4
#ifndef INCLUDED_Nine
#define INCLUDED_Nine

#ifndef HXCPP_H
#include <hxcpp.h>
#endif

HX_DECLARE_CLASS0(Nine)



class HXCPP_CLASS_ATTRIBUTES Nine_obj : public hx::Object
{
	public:
		typedef hx::Object super;
		typedef Nine_obj OBJ_;
		Nine_obj();

	public:
		enum { _hx_ClassId = 0x33deb152 };

		void __construct();
		inline void *operator new(size_t inSize, bool inContainer=false,const char *inName="Nine")
			{ return hx::Object::operator new(inSize,inContainer,inName); }
		inline void *operator new(size_t inSize, int extra)
			{ return hx::Object::operator new(inSize+extra,false,"Nine"); }

		hx::ObjectPtr< Nine_obj > __new() {
			hx::ObjectPtr< Nine_obj > __this = new Nine_obj();
			__this->__construct();
			return __this;
		}

		static hx::ObjectPtr< Nine_obj > __alloc(hx::Ctx *_hx_ctx) {
			Nine_obj *__this = (Nine_obj*)(hx::Ctx::alloc(_hx_ctx, sizeof(Nine_obj), false, "Nine"));
			*(void **)__this = Nine_obj::_hx_vtable;
			return __this;
		}

		static void * _hx_vtable;
		static Dynamic __CreateEmpty();
		static Dynamic __Create(hx::DynamicArray inArgs);
		//~Nine_obj();

		HX_DO_RTTI_ALL;
		static bool __GetStatic(const ::String &inString, Dynamic &outValue, hx::PropertyAccess inCallProp);
		static void __register();
		bool _hx_isInstanceOf(int inClassId);
		::String __ToString() const { return HX_HCSTRING("Nine","\x52","\xb1","\xde","\x33"); }

		static void main();
		static ::Dynamic main_dyn();

};


#endif /* INCLUDED_Nine */ 