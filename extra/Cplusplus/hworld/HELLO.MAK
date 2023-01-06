# Microsoft Visual C++ generated build script - Do not modify

PROJ = HELLO
DEBUG = 1
PROGTYPE = 0
CALLER = 
ARGS = 
DLLS = 
D_RCDEFINES = -d_DEBUG
R_RCDEFINES = -dNDEBUG
ORIGIN = MSVC
ORIGIN_VER = 1.00
PROJPATH = C:\MSVC\BIN\HELLO\
USEMFC = 1
CC = cl
CPP = cl
CXX = cl
CCREATEPCHFLAG = 
CPPCREATEPCHFLAG = 
CUSEPCHFLAG = 
CPPUSEPCHFLAG = 
FIRSTC =             
FIRSTCPP =             
RC = rc
CFLAGS_D_WEXE = /nologo /W3 /FR /G2 /Zi /D_DEBUG /Od /AM /GA /Fd"HELLO.PDB"
CFLAGS_R_WEXE = /nologo /W3 /FR /O1 /DNDEBUG /AM /GA
LFLAGS_D_WEXE = /NOLOGO /ONERROR:NOEXE /NOD /PACKC:61440 /CO /ALIGN:16 /STACK:10240
LFLAGS_R_WEXE = /NOLOGO /ONERROR:NOEXE /NOD /PACKC:61440 /ALIGN:16 /STACK:10240
LIBS_D_WEXE = mafxcwd oldnames libw commdlg shell olecli olesvr mlibcew
LIBS_R_WEXE = mafxcw oldnames libw commdlg shell olecli olesvr mlibcew
RCFLAGS = /nologo
RESFLAGS = /nologo
RUNFLAGS = 
DEFFILE = HELLO.DEF
OBJS_EXT = 
LIBS_EXT = 
!if "$(DEBUG)" == "1"
CFLAGS = $(CFLAGS_D_WEXE)
LFLAGS = $(LFLAGS_D_WEXE)
LIBS = $(LIBS_D_WEXE)
MAPFILE = nul
RCDEFINES = $(D_RCDEFINES)
!else
CFLAGS = $(CFLAGS_R_WEXE)
LFLAGS = $(LFLAGS_R_WEXE)
LIBS = $(LIBS_R_WEXE)
MAPFILE = nul
RCDEFINES = $(R_RCDEFINES)
!endif
!if [if exist MSVC.BND del MSVC.BND]
!endif
SBRS = HELLO.SBR \
		HELLODLG.SBR \
		STDAFX.SBR


HELLO_DEP = c:\msvc\bin\hello\stdafx.h \
	c:\msvc\bin\hello\hello.h \
	c:\msvc\bin\hello\hellodlg.h


HELLODLG_DEP = c:\msvc\bin\hello\stdafx.h \
	c:\msvc\bin\hello\hello.h \
	c:\msvc\bin\hello\hellodlg.h


HELLO_RCDEP = c:\msvc\bin\hello\hello.ico


STDAFX_DEP = c:\msvc\bin\hello\stdafx.h


all:	$(PROJ).EXE $(PROJ).BSC

HELLO.OBJ:	HELLO.CPP $(HELLO_DEP)
	$(CPP) $(CFLAGS) $(CPPUSEPCHFLAG) /c HELLO.CPP

HELLODLG.OBJ:	HELLODLG.CPP $(HELLODLG_DEP)
	$(CPP) $(CFLAGS) $(CPPUSEPCHFLAG) /c HELLODLG.CPP

HELLO.RES:	HELLO.RC $(HELLO_RCDEP)
	$(RC) $(RCFLAGS) $(RCDEFINES) -r HELLO.RC

STDAFX.OBJ:	STDAFX.CPP $(STDAFX_DEP)
	$(CPP) $(CFLAGS) $(CPPUSEPCHFLAG) /c STDAFX.CPP


$(PROJ).EXE::	HELLO.RES

$(PROJ).EXE::	HELLO.OBJ HELLODLG.OBJ STDAFX.OBJ $(OBJS_EXT) $(DEFFILE)
	echo >NUL @<<$(PROJ).CRF
HELLO.OBJ +
HELLODLG.OBJ +
STDAFX.OBJ +
$(OBJS_EXT)
$(PROJ).EXE
$(MAPFILE)
c:\msvc\lib\+
c:\msvc\mfc\lib\+
$(LIBS)
$(DEFFILE);
<<
	link $(LFLAGS) @$(PROJ).CRF
	$(RC) $(RESFLAGS) HELLO.RES $@
	@copy $(PROJ).CRF MSVC.BND

$(PROJ).EXE::	HELLO.RES
	if not exist MSVC.BND 	$(RC) $(RESFLAGS) HELLO.RES $@

run: $(PROJ).EXE
	$(PROJ) $(RUNFLAGS)


$(PROJ).BSC: $(SBRS)
	bscmake @<<
/o$@ $(SBRS)
<<
