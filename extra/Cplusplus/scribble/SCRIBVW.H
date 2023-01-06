// scribvw.h : interface of the CScribView class
//
/////////////////////////////////////////////////////////////////////////////

class CScribView : public CView
{
protected: // create from serialization only
	CScribView();
	DECLARE_DYNCREATE(CScribView)

// Attributes
public:
	CScribDoc* GetDocument();
protected:
	CStroke*	m_pStrokeCur;	// The stroke in progress
	CPoint		m_ptPrev;		// The last mouse point in the stroke in progress

// Operations
public:

// Implementation
public:
	virtual ~CScribView();
	virtual void OnDraw(CDC* pDC);  // overridden to draw this view
	
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif

protected:

	// Printing support
	virtual BOOL OnPreparePrinting(CPrintInfo* pInfo);
	virtual void OnBeginPrinting(CDC* pDC, CPrintInfo* pInfo);
	virtual void OnEndPrinting(CDC* pDC, CPrintInfo* pInfo);

// Generated message map functions
protected:
	//{{AFX_MSG(CScribView)
	afx_msg void OnLButtonDown(UINT nFlags, CPoint point);
	afx_msg void OnLButtonUp(UINT nFlags, CPoint point);
	afx_msg void OnMouseMove(UINT nFlags, CPoint point);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

#ifndef _DEBUG  // debug version in scribvw.cpp
inline CScribDoc* CScribView::GetDocument()
   { return (CScribDoc*)m_pDocument; }
#endif

/////////////////////////////////////////////////////////////////////////////
