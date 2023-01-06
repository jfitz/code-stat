// scribvw.cpp : implementation of the CScribView class
//

#include "stdafx.h"
#include "scribble.h"

#include "scribdoc.h"
#include "scribvw.h"

#ifdef _DEBUG
#undef THIS_FILE
static char BASED_CODE THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CScribView

IMPLEMENT_DYNCREATE(CScribView, CView)

BEGIN_MESSAGE_MAP(CScribView, CView)
	//{{AFX_MSG_MAP(CScribView)
	ON_WM_LBUTTONDOWN()
	ON_WM_LBUTTONUP()
	ON_WM_MOUSEMOVE()
	//}}AFX_MSG_MAP
	// Standard printing commands
	ON_COMMAND(ID_FILE_PRINT, CView::OnFilePrint)
	ON_COMMAND(ID_FILE_PRINT_PREVIEW, CView::OnFilePrintPreview)
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CScribView construction/destruction

CScribView::CScribView()
{
	// TODO: add construction code here
}

CScribView::~CScribView()
{
}

/////////////////////////////////////////////////////////////////////////////
// CScribView drawing

void CScribView::OnDraw(CDC* pDC)
{
	CScribDoc* pDoc = GetDocument();

	for( POSITION pos = pDoc->GetFirstStrokePos(); pos != NULL; )
	{
		CStroke* pStroke = pDoc->GetNextStroke( pos);
		pStroke->DrawStroke( pDC);
	}
}

/////////////////////////////////////////////////////////////////////////////
// CScribView printing

BOOL CScribView::OnPreparePrinting(CPrintInfo* pInfo)
{
	// default preparation
	return DoPreparePrinting(pInfo);
}

void CScribView::OnBeginPrinting(CDC* /*pDC*/, CPrintInfo* /*pInfo*/)
{
	// TODO: add extra initialization before printing
}

void CScribView::OnEndPrinting(CDC* /*pDC*/, CPrintInfo* /*pInfo*/)
{
	// TODO: add cleanup after printing
}

/////////////////////////////////////////////////////////////////////////////
// CScribView diagnostics

#ifdef _DEBUG
void CScribView::AssertValid() const
{
	CView::AssertValid();
}

void CScribView::Dump(CDumpContext& dc) const
{
	CView::Dump(dc);
}

CScribDoc* CScribView::GetDocument() // non-debug version is inline
{
	ASSERT(m_pDocument->IsKindOf(RUNTIME_CLASS(CScribDoc)));
	return (CScribDoc*)m_pDocument;
}
#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////
// CScribView message handlers

void CScribView::OnLButtonDown( UINT, CPoint point)
{
	m_pStrokeCur = GetDocument()->NewStroke();
	// Add new stroke to the new stroke
	m_pStrokeCur->AddPoint( point);
	
	SetCapture();	//Capture the mouse until button up
	m_ptPrev = point;
	
	return;
}

void CScribView::OnLButtonUp(UINT, CPoint point)
{
	if( GetCapture() != this)
		return;
	CScribDoc* pDoc = GetDocument();
	
	CClientDC dc( this);
	
	CPen* pOldPen = dc.SelectObject( pDoc->GetCurrentPen());
	dc.MoveTo( m_ptPrev);
	dc.LineTo( point);
	dc.SelectObject( pOldPen);
	m_pStrokeCur->AddPoint( point);
	
	ReleaseCapture();	// release the mouse capture established at the beginning of the mouse drag.
	
	return;
}

void CScribView::OnMouseMove(UINT, CPoint point)
{
	if( GetCapture() != this)
		return;
		
		CClientDC dc( this);
		
		m_pStrokeCur->AddPoint( point);
		
		CPen* pOldPen = dc.SelectObject( GetDocument()->GetCurrentPen());
		dc.MoveTo( m_ptPrev);
		dc.LineTo( point);
		dc.SelectObject( pOldPen);
		m_ptPrev = point;
		return;
}
