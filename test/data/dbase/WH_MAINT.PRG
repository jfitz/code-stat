* WH_MAINT.PRG

module = [WH]

wtitle = [ÄCatÄÄÂÄÄDescriptionÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂTypeÂÄÄRateÄÂTaxÂGovtÂCommÂInhouseÂBill]
wfields = [cat+'³'+SUBS(desc,1,29)+'³ '+type+' ³ '+STR(rate,6,2)+'³ '+IIF(taxable,'Y','N')+' ³ '+IIF(govt_tax,'Y','N')+' ³ '+IIF(commission,'Y','N')+' ³ ' +IIF(inhouse,'Y','N')+' ³ ' +IIF(billable,'Y','N')]
mtitle = [Service Categories]

DO events
DO whom

DO browse

CLOSE DATA
RETURN

PROC wh_gets
PARAM mrow
@ mrow,left_col+4 GET M->whom
RETURN


