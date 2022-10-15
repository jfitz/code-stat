PUBLIC XAS
M_FILE=0
xvar=65
do while xvar xas=chr(xvar)
do case
case type('&xas')='C'
m_&xas=space(len(&xas))
case type('&xas')='N'
m_&xas=000000000.00000000
case type('&xas')='L'
m_&xas=' '
case type('&xas')='D'
m_&xas=ctod(' / / ')
ENDcase
xvar=xvar+1
enddo
RETURN
