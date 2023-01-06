// scribdoc.h : interface of the CScribDoc class
//
/////////////////////////////////////////////////////////////////////////////

class CStroke;
// Forward declaration of data structure class
class CScribDoc : public CDocument
{
protected: // create from serialization only
	CScribDoc();
	DECLARE_DYNCREATE(CScribDoc)

// Attributes
protected:
	CObList	m_strokeList;	// Each element is a CStroke
	UINT	m_nPenWidth;	// Current user-selected width
	CPen	m_penCur;		// user-selected pen style
public:
	CPen*	GetCurrentPen() {return &m_penCur; }

// Operations
public:
	void 		DeleteContents();
	CStroke*	NewStroke();
	POSITION	GetFirstStrokePos();
	CStroke*	GetNextStroke( POSITION& pos);
	 
// Implementation
public:
	virtual ~CScribDoc();
	virtual void Serialize(CArchive& ar);   // overridden for document i/o
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif
protected:
	void	InitDocument();
	virtual BOOL OnNewDocument();
	virtual BOOL OnOpenDocument( const char* pszPathName );

// Generated message map functions
protected:
	//{{AFX_MSG(CScribDoc)
		// NOTE - the ClassWizard will add and remove member functions here.
		//    DO NOT EDIT what you see in these blocks of generated code !
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////

// Declaration of class CStroke
class CStroke: public CObject
{

public:
	CStroke( UINT nPenWidth );
	
protected:
	CStroke();
	DECLARE_SERIAL( CStroke)
	
// Attributes
	UINT	m_nPenWidth;	// One width applies to entire stroke
	CDWordArray	m_pointArray;	// Series of connected points
	
// Operations
public:
	void AddPoint( CPoint pt );
	BOOL DrawStroke( CDC* pDC);
	
// Helper functions
protected:
	CPoint GetPoint( int i ) const { return CPoint( m_pointArray[i]);; }
public:
	virtual void Serialize( CArchive& ar );
	
};	
	