project(glut)

set (SRC_FILES	
	lib/glut/glut_8x13.c
	lib/glut/glut_9x15.c
	lib/glut/glut_bitmap.c
	lib/glut/glut_bwidth.c
	lib/glut/glut_cindex.c
	lib/glut/glut_cmap.c
	lib/glut/glut_cursor.c
	lib/glut/glut_dials.c
	lib/glut/glut_dstr.c
	lib/glut/glut_event.c
	lib/glut/glut_ext.c
	lib/glut/glut_fcb.c
	lib/glut/glut_fullscrn.c
	lib/glut/glut_gamemode.c
	lib/glut/glut_get.c
	lib/glut/glut_glxext.c
	lib/glut/glut_hel10.c
	lib/glut/glut_hel12.c
	lib/glut/glut_hel18.c
	lib/glut/glut_init.c
	lib/glut/glut_input.c
	lib/glut/glut_joy.c
	lib/glut/glut_key.c
	lib/glut/glut_keyctrl.c
	lib/glut/glut_keyup.c
	lib/glut/glut_mesa.c
	lib/glut/glut_modifier.c
	lib/glut/glut_mroman.c
	lib/glut/glut_overlay.c
	lib/glut/glut_roman.c
	lib/glut/glut_shapes.c
	lib/glut/glut_space.c
	lib/glut/glut_stroke.c
	lib/glut/glut_swap.c
	lib/glut/glut_swidth.c
	lib/glut/glut_tablet.c
	lib/glut/glut_teapot.c
	lib/glut/glut_tr10.c
	lib/glut/glut_tr24.c
	lib/glut/glut_util.c
	lib/glut/glut_vidresize.c
	lib/glut/glut_warp.c
	lib/glut/glut_win.c
	lib/glut/glut_winmisc.c
	lib/glut/win32_glx.c
	lib/glut/win32_menu.c
	lib/glut/win32_util.c
	lib/glut/win32_winproc.c
	lib/glut/win32_x11.c
)
#set_source_files_properties( ${SRC_FILES} PROPERTIES LANGUAGE CXX )

include_directories(include)

if (ARCH_64bit)
	add_library(glut64 STATIC ${SRC_FILES})
else()
	add_library(glut32 STATIC ${SRC_FILES})
endif()
