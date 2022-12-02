#include "hbclass.ch"

   CREATE CLASS Point1D

   VAR Abscissa    // the abscissa of our point

METHOD New( Abscissa )    // Constructor

METHOD Distance( Point )

   ENDCLASS

   CREATE CLASS Point2D INHERIT Point1D

   VAR Ordinate    // the ordinate of our point

METHOD New( Abscissa, Ordinate )    // Constructor

METHOD Distance( Point )

   ENDCLASS

   CREATE CLASS Point3D INHERIT Point2D

   VAR Zcoord    // the Z-coordinate of our point

METHOD New( Zcoord )    // Constructor

METHOD Distance( Point )

   ENDCLASS

&& Constructors Zone
METHOD New( Abscissa ) CLASS Point1D

   ::Abscissa := Abscissa

   RETURN Self

METHOD New( Abscissa, Ordinate ) CLASS Point2D

   ::Abscissa := Abscissa
   ::Ordinate := Ordinate

   RETURN Self

METHOD New( Abscissa, Ordinate, Zcoord ) CLASS Point3D

   ::Abscissa := Abscissa
   ::Ordinate := Ordinate
   ::Zcoord := Zcoord

   RETURN Self

&&Distances Methods

METHOD Distance( Point ) CLASS Point1D

   RETURN Sqrt( ( Self:Abscissa - Point:Abscissa ) ^ 2 )

METHOD Distance( Point ) CLASS Point2D

   RETURN Sqrt( ( Self:Abscissa - Point:Abscissa ) ^ 2 + ( Self:Ordinate - Point:Ordinate ) ^ 2 )

METHOD Distance( Point ) CLASS Point3D

   RETURN Sqrt( ( Self:Abscissa - Point:Abscissa ) ^ 2 + ( Self:Ordinate - Point:Ordinate ) ^ 2 + ( Self:Zcoord - Point:Zcoord ) ^ 2 )

PROCEDURE Main()

   FirstPoint := Point1D():New( 3 )
   SecondPoint := Point1D():New( - 3 )
   ? FirstPoint:Abscissa
   ? FirstPoint:Distance( SecondPoint )
   ThirdPoint := Point2D():New( 2, - 3 )
   FourthPoint := Point2D():New( - 1, - 2 )
   ? ThirdPoint:Distance( FourthPoint )
   FifthPoint := Point3D():New( 1, 1, 1 )
   SixthPoint := Point3D():New( 4, 4, 4 )
   ? FifthPoint:Distance( SixthPoint )

   RETURN
   