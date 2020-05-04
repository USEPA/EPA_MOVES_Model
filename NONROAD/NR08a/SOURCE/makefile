FC = gfortran

FLGS = -O2 -static

INC = ./

LIBS = 

TARGT = nonroad.exe

OBJCTS = \
agedist.o \
alocty.o \
alosta.o \
alosub.o \
blknon.o \
chkasc.o \
chkwrn.o \
chrsrt.o \
clcems.o \
clcevems.o \
clcrtrft.o \
clsnon.o \
cmprrtrft.o \
cnthpcat.o \
dayloop.o \
daymthf.o \
dispit.o \
emfclc.o \
emsadj.o \
evemfclc.o \
fndact.o \
fndasc.o \
fndchr.o \
fnddet.o \
fndefc.o \
fndevefc.o \
fndevtch.o \
fndgxf.o \
fndhpc.o \
fndkey.o \
fndreg.o \
fndrfm.o \
fndrtrft.o \
fndscrp.o \
fndtch.o \
fndtpm.o \
getgrw.o \
getime.o \
getind.o \
getpop.o \
getscrp.o \
getsys.o \
grwfac.o \
hdrbmy.o \
in1fip.o \
in2fip.o \
in3fip.o \
in4fip.o \
in5fip.o \
iniasc.o \
initrtrft.o \
intadj.o \
intams.o \
intnon.o \
lftjst.o \
low2up.o \
modyr.o \
mspinit.o \
nonroad.o \
opnefc.o \
opnnon.o \
prc1st.o \
prccty.o \
prcnat.o \
prcsta.o \
prcsub.o \
prcus.o \
rdact.o \
rdalo.o \
rdalt.o \
rdbsfc.o \
rdday.o \
rddetr.o \
rdefls.o \
rdemfc.o \
rdevemfc.o \
rdevtech.o \
rdevtech_moves.o \
rdfips.o \
rdgrow.o \
rdgxrf.o \
rdind.o \
rdnropt.o \
rdnrper.o \
rdnrreg.o \
rdnrsrc.o \
rdpop.o \
rdrgndf.o \
rdrtrft.o \
rdscrp.o \
rdseas.o \
rdspil.o \
rdstg2.o \
rdsulf.o \
rdtech.o \
rdtech_moves.o \
rgtjst.o \
rtrftengovrlp.o \
scrptime.o \
sitot.o \
spinit.o \
srtrtrft.o \
strlen.o \
strmin.o \
swaprtrft.o \
unitcf.o \
vldrtrfthp.o \
vldrtrftrecs.o \
vldrtrftscc.o \
vldrtrfttchtyp.o \
wadeeq.o \
wrtams.o \
wrtbmy.o \
wrtdat.o \
wrthdr.o \
wrtmsg.o \
wrtsi.o \
wrtsum.o 

nonroad: $(OBJCTS)
	$(FC) -o $(TARGT) $(FLGS) $(OBJCTS) $(LIBS)
.f.o	:
	$(FC) -c -o $@ $(FLGS) $<

clean: 
	del nonroad *.o


agedist.o 	: agedist.f	 $(INC)/nonrdprm.inc
alocty.o 	: alocty.f	 $(INC)/nonrdprm.inc $(INC)/nonrdio.inc $(INC)/nonrdefc.inc $(INC)/nonrdeqp.inc $(INC)/nonrdalo.inc $(INC)/nonrdreg.inc $(INC)/nonrdusr.inc
alosta.o 	: alosta.f	 $(INC)/nonrdprm.inc $(INC)/nonrdio.inc $(INC)/nonrdefc.inc $(INC)/nonrdeqp.inc $(INC)/nonrdalo.inc $(INC)/nonrdreg.inc $(INC)/nonrdusr.inc
alosub.o 	: alosub.f	 $(INC)/nonrdprm.inc $(INC)/nonrdio.inc $(INC)/nonrdefc.inc $(INC)/nonrdeqp.inc $(INC)/nonrdalo.inc $(INC)/nonrdreg.inc $(INC)/nonrdusr.inc
blknon.o 	: blknon.f	 $(INC)/nonrdprm.inc $(INC)/nonrdusr.inc $(INC)/nonrdio.inc $(INC)/nonrdefc.inc $(INC)/nonrdeqp.inc
chkasc.o 	: chkasc.f	 $(INC)/nonrdprm.inc $(INC)/nonrdusr.inc
chkwrn.o 	: chkwrn.f	 $(INC)/nonrdprm.inc $(INC)/nonrdio.inc
chrsrt.o 	: chrsrt.f	 $(INC)/nonrdprm.inc
clcems.o 	: clcems.f	 $(INC)/nonrdprm.inc $(INC)/nonrdio.inc $(INC)/nonrdefc.inc $(INC)/nonrdeqp.inc $(INC)/nonrdact.inc $(INC)/nonrdusr.inc $(INC)/nonrdrtrft.inc
clcevems.o 	: clcevems.f	 $(INC)/nonrdprm.inc $(INC)/nonrdio.inc $(INC)/nonrdefc.inc $(INC)/nonrdeqp.inc $(INC)/nonrdact.inc $(INC)/nonrdusr.inc
clcrtrft.o 	: clcrtrft.f	 $(INC)/nonrdprm.inc $(INC)/nonrdio.inc $(INC)/nonrdusr.inc $(INC)/nonrdrtrft.inc
clsnon.o 	: clsnon.f	 $(INC)/nonrdprm.inc $(INC)/nonrdio.inc
cmprrtrft.o 	: cmprrtrft.f	 $(INC)/nonrdprm.inc $(INC)/nonrdrtrft.inc
cnthpcat.o 	: cnthpcat.f	 $(INC)/nonrdprm.inc $(INC)/nonrdeqp.inc
dayloop.o 	: dayloop.f	 $(INC)/nonrdprm.inc $(INC)/nonrdio.inc $(INC)/nonrdusr.inc
daymthf.o 	: daymthf.f	 $(INC)/nonrdprm.inc $(INC)/nonrdio.inc $(INC)/nonrdreg.inc $(INC)/nonrdusr.inc $(INC)/nonrdefc.inc $(INC)/nonrdeqp.inc $(INC)/nonrdtpl.inc
dispit.o 	: dispit.f	 $(INC)/nonrdprm.inc $(INC)/nonrdio.inc
emfclc.o 	: emfclc.f	 $(INC)/nonrdprm.inc $(INC)/nonrdio.inc $(INC)/nonrdefc.inc $(INC)/nonrdeqp.inc $(INC)/nonrdact.inc $(INC)/nonrdusr.inc
emsadj.o 	: emsadj.f	 $(INC)/nonrdprm.inc $(INC)/nonrdusr.inc $(INC)/nonrdefc.inc $(INC)/nonrdeqp.inc $(INC)/nonrdio.inc
evemfclc.o 	: evemfclc.f	 $(INC)/nonrdprm.inc $(INC)/nonrdio.inc $(INC)/nonrdefc.inc $(INC)/nonrdeqp.inc $(INC)/nonrdact.inc $(INC)/nonrdusr.inc
fndact.o 	: fndact.f	 $(INC)/nonrdprm.inc $(INC)/nonrdeqp.inc $(INC)/nonrdact.inc $(INC)/nonrdreg.inc $(INC)/nonrdtpl.inc
fnddet.o 	: fnddet.f	 $(INC)/nonrdprm.inc $(INC)/nonrdefc.inc
fndefc.o 	: fndefc.f	 $(INC)/nonrdprm.inc $(INC)/nonrdio.inc $(INC)/nonrdeqp.inc $(INC)/nonrdefc.inc
fndevefc.o 	: fndevefc.f	 $(INC)/nonrdprm.inc $(INC)/nonrdio.inc $(INC)/nonrdeqp.inc $(INC)/nonrdefc.inc
fndevtch.o 	: fndevtch.f	 $(INC)/nonrdprm.inc $(INC)/nonrdefc.inc $(INC)/nonrdeqp.inc
fndgxf.o 	: fndgxf.f	 $(INC)/nonrdprm.inc $(INC)/nonrdgrw.inc
fndhpc.o 	: fndhpc.f	 $(INC)/nonrdprm.inc $(INC)/nonrdeqp.inc
fndkey.o 	: fndkey.f	 $(INC)/nonrdprm.inc
fndreg.o 	: fndreg.f	 $(INC)/nonrdprm.inc $(INC)/nonrdreg.inc $(INC)/nonrdtpl.inc
fndrfm.o 	: fndrfm.f	 $(INC)/nonrdprm.inc $(INC)/nonrdefc.inc $(INC)/nonrdeqp.inc
fndrtrft.o 	: fndrtrft.f	 $(INC)/nonrdprm.inc $(INC)/nonrdio.inc $(INC)/nonrdrtrft.inc
fndscrp.o 	: fndscrp.f	 $(INC)/nonrdprm.inc $(INC)/nonrdio.inc $(INC)/nonrdeqp.inc
fndtch.o 	: fndtch.f	 $(INC)/nonrdprm.inc $(INC)/nonrdefc.inc $(INC)/nonrdeqp.inc
fndtpm.o 	: fndtpm.f	 $(INC)/nonrdprm.inc $(INC)/nonrdtpl.inc $(INC)/nonrdreg.inc
getgrw.o 	: getgrw.f	 $(INC)/nonrdprm.inc $(INC)/nonrdio.inc $(INC)/nonrdgrw.inc $(INC)/nonrdeqp.inc
getime.o 	: getime.f	 $(INC)/nonrdprm.inc $(INC)/nonrdio.inc
getind.o 	: getind.f	 $(INC)/nonrdprm.inc $(INC)/nonrdio.inc
getpop.o 	: getpop.f	 $(INC)/nonrdprm.inc $(INC)/nonrdio.inc $(INC)/nonrdefc.inc $(INC)/nonrdeqp.inc $(INC)/nonrdreg.inc $(INC)/nonrdusr.inc
getscrp.o 	: getscrp.f	 $(INC)/nonrdprm.inc $(INC)/nonrdio.inc $(INC)/nonrdeqp.inc
getsys.o 	: getsys.f	 $(INC)/nonrdprm.inc $(INC)/nonrdio.inc
grwfac.o 	: grwfac.f	 $(INC)/nonrdprm.inc $(INC)/nonrdio.inc $(INC)/nonrdgrw.inc $(INC)/nonrdreg.inc
hdrbmy.o 	: hdrbmy.f	 $(INC)/nonrdprm.inc $(INC)/nonrdio.inc
in1fip.o 	: in1fip.f	 $(INC)/nonrdprm.inc $(INC)/nonrdusr.inc $(INC)/nonrdreg.inc
in2fip.o 	: in2fip.f	 $(INC)/nonrdprm.inc $(INC)/nonrdusr.inc $(INC)/nonrdreg.inc
in3fip.o 	: in3fip.f	 $(INC)/nonrdprm.inc $(INC)/nonrdusr.inc $(INC)/nonrdreg.inc
in4fip.o 	: in4fip.f	 $(INC)/nonrdprm.inc $(INC)/nonrdusr.inc $(INC)/nonrdreg.inc
in5fip.o 	: in5fip.f	 $(INC)/nonrdprm.inc $(INC)/nonrdusr.inc $(INC)/nonrdreg.inc
iniasc.o 	: iniasc.f	 $(INC)/nonrdprm.inc $(INC)/nonrdusr.inc $(INC)/nonrdreg.inc
initrtrft.o 	: initrtrft.f	 $(INC)/nonrdprm.inc $(INC)/nonrdrtrft.inc
intadj.o 	: intadj.f	 $(INC)/nonrdprm.inc $(INC)/nonrdusr.inc $(INC)/nonrdefc.inc
intams.o 	: intams.f	 $(INC)/nonrdprm.inc $(INC)/nonrdio.inc $(INC)/nonrdusr.inc $(INC)/nonrdeqp.inc
intnon.o 	: intnon.f	 $(INC)/nonrdprm.inc $(INC)/nonrdio.inc $(INC)/nonrdalo.inc $(INC)/nonrdusr.inc
modyr.o 	: modyr.f	 $(INC)/nonrdprm.inc $(INC)/nonrdio.inc $(INC)/nonrdefc.inc $(INC)/nonrdeqp.inc $(INC)/nonrdact.inc $(INC)/nonrdusr.inc
mspinit.o 	: mspinit.f	 $(INC)/nonrdprm.inc $(INC)/nonrdio.inc
nonroad.o 	: nonroad.f	 $(INC)/nonrdprm.inc $(INC)/nonrdio.inc $(INC)/nonrdreg.inc $(INC)/nonrdusr.inc $(INC)/nonrdefc.inc $(INC)/nonrdeqp.inc
opnefc.o 	: opnefc.f	 $(INC)/nonrdprm.inc $(INC)/nonrdio.inc $(INC)/nonrdusr.inc
opnnon.o 	: opnnon.f	 $(INC)/nonrdprm.inc $(INC)/nonrdio.inc $(INC)/nonrdusr.inc
prc1st.o 	: prc1st.f	 $(INC)/nonrdprm.inc $(INC)/nonrdio.inc $(INC)/nonrdreg.inc $(INC)/nonrdefc.inc $(INC)/nonrdeqp.inc $(INC)/nonrdalo.inc $(INC)/nonrdgrw.inc $(INC)/nonrdact.inc $(INC)/nonrdusr.inc
prccty.o 	: prccty.f	 $(INC)/nonrdprm.inc $(INC)/nonrdio.inc $(INC)/nonrdreg.inc $(INC)/nonrdefc.inc $(INC)/nonrdeqp.inc $(INC)/nonrdalo.inc $(INC)/nonrdgrw.inc $(INC)/nonrdact.inc $(INC)/nonrdusr.inc
prcnat.o 	: prcnat.f	 $(INC)/nonrdprm.inc $(INC)/nonrdio.inc $(INC)/nonrdreg.inc $(INC)/nonrdefc.inc $(INC)/nonrdeqp.inc $(INC)/nonrdalo.inc $(INC)/nonrdgrw.inc $(INC)/nonrdact.inc $(INC)/nonrdusr.inc
prcsta.o 	: prcsta.f	 $(INC)/nonrdprm.inc $(INC)/nonrdio.inc $(INC)/nonrdreg.inc $(INC)/nonrdefc.inc $(INC)/nonrdeqp.inc $(INC)/nonrdalo.inc $(INC)/nonrdgrw.inc $(INC)/nonrdact.inc $(INC)/nonrdusr.inc
prcsub.o 	: prcsub.f	 $(INC)/nonrdprm.inc $(INC)/nonrdio.inc $(INC)/nonrdreg.inc $(INC)/nonrdefc.inc $(INC)/nonrdeqp.inc $(INC)/nonrdalo.inc $(INC)/nonrdgrw.inc $(INC)/nonrdact.inc $(INC)/nonrdusr.inc
prcus.o 	: prcus.f	 $(INC)/nonrdprm.inc $(INC)/nonrdio.inc $(INC)/nonrdreg.inc $(INC)/nonrdefc.inc $(INC)/nonrdeqp.inc $(INC)/nonrdalo.inc $(INC)/nonrdgrw.inc $(INC)/nonrdact.inc $(INC)/nonrdusr.inc
rdact.o 	: rdact.f	 $(INC)/nonrdprm.inc $(INC)/nonrdio.inc $(INC)/nonrdusr.inc $(INC)/nonrdact.inc
rdalo.o 	: rdalo.f	 $(INC)/nonrdprm.inc $(INC)/nonrdio.inc $(INC)/nonrdalo.inc
rdalt.o 	: rdalt.f	 $(INC)/nonrdprm.inc $(INC)/nonrdio.inc $(INC)/nonrdefc.inc $(INC)/nonrdeqp.inc
rdbsfc.o 	: rdbsfc.f	 $(INC)/nonrdprm.inc $(INC)/nonrdio.inc $(INC)/nonrdefc.inc $(INC)/nonrdusr.inc
rdday.o 	: rdday.f	 $(INC)/nonrdprm.inc $(INC)/nonrdio.inc $(INC)/nonrdefc.inc $(INC)/nonrdeqp.inc $(INC)/nonrdusr.inc
rddetr.o 	: rddetr.f	 $(INC)/nonrdprm.inc $(INC)/nonrdio.inc $(INC)/nonrdefc.inc $(INC)/nonrdusr.inc
rdefls.o 	: rdefls.f	 $(INC)/nonrdprm.inc $(INC)/nonrdio.inc $(INC)/nonrdefc.inc $(INC)/nonrdusr.inc
rdemfc.o 	: rdemfc.f	 $(INC)/nonrdprm.inc $(INC)/nonrdio.inc $(INC)/nonrdefc.inc
rdevemfc.o 	: rdevemfc.f	 $(INC)/nonrdprm.inc $(INC)/nonrdio.inc $(INC)/nonrdefc.inc
rdevtech.o 	: rdevtech.f	 $(INC)/nonrdprm.inc $(INC)/nonrdio.inc $(INC)/nonrdefc.inc $(INC)/nonrdeqp.inc
rdevtech_moves.o 	: rdevtech_moves.f	 $(INC)/nonrdprm.inc $(INC)/nonrdio.inc $(INC)/nonrdefc.inc $(INC)/nonrdeqp.inc
rdfips.o 	: rdfips.f	 $(INC)/nonrdprm.inc $(INC)/nonrdio.inc $(INC)/nonrdusr.inc $(INC)/nonrdreg.inc
rdgrow.o 	: rdgrow.f	 $(INC)/nonrdprm.inc $(INC)/nonrdio.inc $(INC)/nonrdgrw.inc $(INC)/nonrdreg.inc
rdgxrf.o 	: rdgxrf.f	 $(INC)/nonrdprm.inc $(INC)/nonrdio.inc $(INC)/nonrdgrw.inc
rdind.o 	: rdind.f	 $(INC)/nonrdprm.inc $(INC)/nonrdio.inc $(INC)/nonrdalo.inc $(INC)/nonrdreg.inc $(INC)/nonrdusr.inc
rdnropt.o 	: rdnropt.f	 $(INC)/nonrdprm.inc $(INC)/nonrdio.inc $(INC)/nonrdusr.inc
rdnrper.o 	: rdnrper.f	 $(INC)/nonrdprm.inc $(INC)/nonrdio.inc $(INC)/nonrdusr.inc
rdnrreg.o 	: rdnrreg.f	 $(INC)/nonrdprm.inc $(INC)/nonrdio.inc $(INC)/nonrdusr.inc $(INC)/nonrdreg.inc
rdnrsrc.o 	: rdnrsrc.f	 $(INC)/nonrdprm.inc $(INC)/nonrdio.inc $(INC)/nonrdreg.inc $(INC)/nonrdusr.inc $(INC)/nonrdtpl.inc
rdpop.o 	: rdpop.f	 $(INC)/nonrdprm.inc $(INC)/nonrdio.inc $(INC)/nonrdefc.inc $(INC)/nonrdeqp.inc $(INC)/nonrdreg.inc $(INC)/nonrdusr.inc
rdrgndf.o 	: rdrgndf.f	 $(INC)/nonrdprm.inc $(INC)/nonrdio.inc $(INC)/nonrdreg.inc $(INC)/nonrdtpl.inc
rdrtrft.o 	: rdrtrft.f	 $(INC)/nonrdprm.inc $(INC)/nonrdio.inc $(INC)/nonrdusr.inc $(INC)/nonrdrtrft.inc
rdscrp.o 	: rdscrp.f	 $(INC)/nonrdprm.inc $(INC)/nonrdio.inc $(INC)/nonrdefc.inc $(INC)/nonrdeqp.inc
rdseas.o 	: rdseas.f	 $(INC)/nonrdprm.inc $(INC)/nonrdio.inc $(INC)/nonrdtpl.inc
rdspil.o 	: rdspil.f	 $(INC)/nonrdprm.inc $(INC)/nonrdio.inc $(INC)/nonrdefc.inc
rdstg2.o 	: rdstg2.f	 $(INC)/nonrdprm.inc $(INC)/nonrdio.inc $(INC)/nonrdusr.inc
rdsulf.o 	: rdsulf.f	 $(INC)/nonrdprm.inc $(INC)/nonrdio.inc $(INC)/nonrdusr.inc
rdtech.o 	: rdtech.f	 $(INC)/nonrdprm.inc $(INC)/nonrdio.inc $(INC)/nonrdefc.inc $(INC)/nonrdeqp.inc
rdtech_moves.o 	: rdtech_moves.f	 $(INC)/nonrdprm.inc $(INC)/nonrdio.inc $(INC)/nonrdefc.inc $(INC)/nonrdeqp.inc
rtrftengovrlp.o 	: rtrftengovrlp.f	 $(INC)/nonrdprm.inc $(INC)/nonrdrtrft.inc
scrptime.o 	: scrptime.f	 $(INC)/nonrdprm.inc $(INC)/nonrdeqp.inc
sitot.o 	: sitot.f	 $(INC)/nonrdprm.inc $(INC)/nonrdio.inc $(INC)/nonrdeqp.inc
spinit.o 	: spinit.f	 $(INC)/nonrdprm.inc $(INC)/nonrdio.inc
srtrtrft.o 	: srtrtrft.f	 $(INC)/nonrdprm.inc $(INC)/nonrdrtrft.inc
swaprtrft.o 	: swaprtrft.f	 $(INC)/nonrdprm.inc $(INC)/nonrdrtrft.inc
unitcf.o 	: unitcf.f	 $(INC)/nonrdprm.inc $(INC)/nonrdefc.inc $(INC)/nonrdact.inc
vldrtrfthp.o 	: vldrtrfthp.f	 $(INC)/nonrdprm.inc $(INC)/nonrdeqp.inc
vldrtrftrecs.o 	: vldrtrftrecs.f	 $(INC)/nonrdprm.inc $(INC)/nonrdio.inc $(INC)/nonrdrtrft.inc
vldrtrftscc.o 	: vldrtrftscc.f	 $(INC)/nonrdprm.inc $(INC)/nonrdrtrft.inc
vldrtrfttchtyp.o 	: vldrtrfttchtyp.f	 $(INC)/nonrdprm.inc $(INC)/nonrdrtrft.inc
wrtams.o 	: wrtams.f	 $(INC)/nonrdprm.inc $(INC)/nonrdio.inc $(INC)/nonrdeqp.inc $(INC)/nonrdreg.inc
wrtbmy.o 	: wrtbmy.f	 $(INC)/nonrdprm.inc $(INC)/nonrdio.inc
wrtdat.o 	: wrtdat.f	 $(INC)/nonrdprm.inc $(INC)/nonrdio.inc
wrthdr.o 	: wrthdr.f	 $(INC)/nonrdprm.inc $(INC)/nonrdio.inc $(INC)/nonrdusr.inc
wrtmsg.o 	: wrtmsg.f	 $(INC)/nonrdprm.inc $(INC)/nonrdio.inc $(INC)/nonrdact.inc $(INC)/nonrdalo.inc $(INC)/nonrdefc.inc $(INC)/nonrdeqp.inc $(INC)/nonrdgrw.inc $(INC)/nonrdreg.inc $(INC)/nonrdtpl.inc $(INC)/nonrdusr.inc
wrtsi.o 	: wrtsi.f	 $(INC)/nonrdprm.inc $(INC)/nonrdio.inc $(INC)/nonrdeqp.inc
wrtsum.o 	: wrtsum.f	 $(INC)/nonrdprm.inc $(INC)/nonrdio.inc $(INC)/nonrdreg.inc $(INC)/nonrdeqp.inc
