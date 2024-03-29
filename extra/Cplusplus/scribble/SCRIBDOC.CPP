// scribdoc.cpp : implementation of the CScribDoc class
//

#include "stdafx.h"
#include "scribble.h"

#include "scribdoc.h"

#ifdef _DEBUG
#undef THIS_FILE
static char BASED_CODE THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CScribDoc

IMPLEMENT_DYNCREATE(CScribDoc, CDocument)

BEGIN_MESSAGE_MAP(CScribDoc, CDocument)
	//{{AFX_MSG_MAP(CScribDoc)
		// NOTE - the ClassWizard will add and remove mapping macros here.
		//    DO NOT EDIT what you see in these blocks of generated code!
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CScribDoc construction/destruction

CScribDoc::CScribDoc()
{
	// TODO: add one-time construction code here
}

CScribDoc::~CScribDoc()
{
}

void CScribDoc::InitDocument()
{
 	m_nPenWidth =2;	// Default 2-pixel pen width
 	// Solid black pen
 	m_penCur.CreatePen( PS_SOLID, m_nPenWidth, RGB( 0,0,0));
}

BOOL CScribDoc::OnNewDocument()
{
	if (!CDocument::OnNewDocument())
		return FALSE;
	InitDocument();
	return TRUE;
}

BOOL CScribDoc::OnOpenDocument( const char* pszPathName)
{
	if( !CDocument::OnOpenDocument( pszPathName))
		return FALSE;
	InitDocument();
	return TRUE;
}

void CScribDoc::DeleteContents()
{
	while( !m_strokeList.IsEmpty())
	{
		delete m_strokeList.RemoveHead();
	}
}
 
CStroke* CScribDoc::NewStroke()
{
	CStroke* pStrokeItem = new CStroke( m_nPenWidth);
	m_strokeList.AddTail( pStrokeItem);
	SetModifiedFlag(); // Mark document as modified to confirm File Close.
	return pStrokeItem;
}

POSITION CScribDoc::GetFirstStrokePos()
{
	return m_strokeList.GetHeadPosition();
}

CStroke* CScribDoc::GetNextStroke( POSITION& pos)
{
	return (CStroke*) m_strokeList.GetNext( pos);
}

/////////////////////////////////////////////////////////////////////////////
// CScribDoc serialization

void CScribDoc::Serialize(CArchive& ar)
{
	if (ar.IsStoring())
	{
		// TODO: add storing code here
	}
	else
	{
		// TODO: add loading code here
	}
	m_strokeList.Serialize( ar);
}

/////////////////////////////////////////////////////////////////////////////
// CScribDoc diagnostics

#ifdef _DEBUG
void CScribDoc::AssertValid() const
{
	CDocument::AssertValid();
}

void CScribDoc::Dump(CDumpContext& dc) const
{
	CDocument::Dump(dc);
}
#endif //_DEBUG

IMPLEMENT_SERIAL( CStroke, CObject, 1);

/////////////////////////////////////////////////////////////////////////////
// CScribDoc commands

/////////////////////////////////////////////////////////////////////////////
// CStroke Implementation

CStroke::CStroke()
{
}


CStroke::CStroke( UINT nPenWidth)
{
	m_nPenWidth = nPenWidth;
}


void CStroke::AddPoint( CPoint pt)
{
	m_pointArray.Add( MAKELONG( pt.x, pt.y));
} 

/////////////////////////////////////////////////////////////////////////////
// CStroke serialization

void CStroke::Serialize( CArchive& ar)
{
	if (ar.IsStoring())
	{
		ar << (WORD)m_nPenWidth;
		m_pointArray.Serialize( ar);
	}
	else
	{
		WORD w;
		ar >> w;
		m_nPenWidth = w;
		m_pointArray.Serialize( ar);
	}
}

BOOL CStroke::DrawStroke( CDC* pDC)
{
	CPen penStroke;
	if( !penStroke.CreatePen( PS_SOLID, m_nPenWidth, RGB( 0,0,0)))
		return FALSE;
	CPen* pOldPen = pDC->SelectObject( &penStroke);
	pDC->MoveTo( GetPoint(0));
	for( int i = 1; i < m_pointArray.GetSize(); i++)
	{
		pDC->LineTo( GetPoint(i));
	}
	pDC->SelectObject( pOldPen);
	return TRUE;
}