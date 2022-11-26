10 print chr$(147)
20 ca=1024:cp=54272:r=25:co=40
30 poke 53280,0:poke 53281,0
40 forw=3to50:fori=1to12:forj=0to12:k=i+j
50 c=j+j+j/(i+3)+i*w/12
60 k1=ca+i+(co*k):i1=ca+k+(co*i):c1=cp+k1:c2=cp+i1
70 k2=ca+(co-i)+(co*(r-k)):i2=ca+(co-k)+(co*(r-i)):c3=cp+k2:c4=cp+i2
80 k3=ca+(co-i)+(co*k):c5=cp+k3:i3=ca+k+(co*(r-i)):c6=cp+i3
90 k4=ca+(co-k)+(co*i):c7=cp+k4:i4=ca+i+(co*(r-k)):c8=cp+i4
100 poke k1,81:poke c1,c:poke i1,81:poke c2,c
110 poke k2,81:poke c3,c:poke i2,81:poke c4,c
120 poke k3,81:poke c5,c:poke i3,81:poke c6,c
130 poke k4,81:poke c7,c:poke i4,81:poke c8,c
140 nextj,i,w
150 goto 10

