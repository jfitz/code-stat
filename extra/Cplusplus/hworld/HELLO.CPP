// hello.cpp : Defines the class behaviors for the application.
//

#include "stdafx.h"
#include "hello.h"
#include "hellodlg.h"

#ifdef _DEBUG
#undef THIS_FILE
static char BASED_CODE THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CHelloApp

BEGIN_MESSAGE_MAP(CHelloApp, CWinApp)
        //{{AFX_MSG_MAP(CHelloApp)
        //}}AFX_MSG
        ON_COMMAND(ID_HELP, CWinApp::OnHelp)
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CHelloApp construction

CHelloApp::CHelloApp()
{
}

/////////////////////////////////////////////////////////////////////////////
// The one and only CHelloApp object

CHelloApp theApp;

/////////////////////////////////////////////////////////////////////////////
// CHelloApp initialization

BOOL CHelloApp::InitInstance()
{
        // Standard initialization

		SetDialogBkColor();        // Set dialog background color to gray
        LoadStdProfileSettings();  // Load standard INI file options (including MRU)

        CHelloDlg dlg;
        m_pMainWnd = &dlg;
        int nResponse = dlg.DoModal();
        if (nResponse == IDOK)
        {
        }
        else if (nResponse == IDCANCEL)
        {
        }

        // Since the dialog has been closed, return FALSE so that we exit the
        //  application, rather than start the application's message pump.
        return FALSE;
}
