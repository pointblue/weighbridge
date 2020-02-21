*to summarize wb2.0 data
*v3 fixes weight calculations vs. v2
*v4 fixes avidstars errors with banddata, helps user find banddata, creates avidsum.dbf, browses avidsum at end
*v5 fixes potential to miss different ID's in same crossing; allows an event to start with an opto reset
*v6 reconciles forked v4a. v4a was created to fix crossing duration calculation. Opto res changed to opto reset
*v7 for wb2.1 Nov 2018
*v8.1 found a case where direction seems to have been wrong due to two penguins on scale at same time - added a trap
*for flagging when opto 12 is happening (both optos) during ID crossing - mark as unk (we only knew it was wrong because
*of observations of nest exchanges...data were ambiguous.
*current version (12/8/18) sets to "unk" if there are any opto 12's during the crossing - but this might be too conservative.
*Trying to get rid of using bandtal - back to banddata now (8.2.prg)
*re-modeled WB data using wb_band_data, wb_nest_checks, wb_nests
*updating links accordingly
*also tracking "bad_avid" for potentially redundant ID's with new encoding

*Feb 2020: working on using avidsum base code to go through all WB events for weight calculation algorithms

CLOSE ALL
CLEAR
SET EXACT ON

SELECT 0
USE wbdata &&run appender program first to populate this

mwbrn=0

SELECT 0
ON ERROR do create_avidsum
	USE avidsum
	CALCULATE MAX(wbrn) TO mwbrn
ON ERROR 

SELECT wbdata
set safe off
copy to wbdata_tares for event="tare" and recno()>(mwbrn-10000)&&creates a smaller file of just the tares for use later
set safe on

IF mwbrn=0
	mwbrn=1
ENDIF

GOTO mwbrn &&this should be the end of the last event that was written to avidsum

DO WHILE NOT EOF()
	DO while ((LEFT(event, 4)!="opto") OR (ALLTRIM(event)="opto reset")) AND ALLTRIM(event)!="ID" AND NOT EOF()
		SKIP 
		
		IF alltrim(event)="opto 1" OR ALLTRIM(event)="opto 2" OR ALLTRIM(event)="opto 12"
			lastopto=ALLTRIM(event)
		ENDIF

	ENDDO
	
	*note that when it gets here we're either at eof or an opto or an ID
	
	DO CASE
	
	CASE ALLTRIM(event)="opto 1" &&outer opto first, so inbound
		mdir="in"
		mend_event="opto 2"
		DO find_end_event
	CASE ALLTRIM(event)="opto 2" &&inner opto first, so outbound
		mdir="out"
		mend_event="opto 1"
		DO find_end_event
	CASE ALLTRIM(event)="opto 12" &&both optos at once - direction unclear
		mdir="unk"
		*don't find end event yet because we don't have a clear direction?
	CASE avid !=0
		*found an ID outside of an opto event
		DO CASE
			CASE lastopto="opto 1"
				mdir="in"
				mend_event="opto 2"
				DO find_end_event
			CASE lastopto="opto 2"
				mdir="out"
				mend_event="opto 1"
				DO find_end_event
			OTHERWISE
				mdir="unk"
		ENDCASE
			
	*CASE ALLTRIM(event)="opto reset" &&reset opto event - direction unclear, but probably learnable?
	*	mdir="unk"
	*	DO find_end_event
	OTHERWISE &&EOF
		mdir=" "
	ENDCASE
	
	IF mdir!=" " AND NOT EOF()
		SKIP
	ELSE
	
	ENDIF
	
ENDDO

*From here to procedures we are adding stuff to avidsum table based on what the procedures calculated (see below)
select avidsum

set uniq on
set safe off
index on ALLTRIM(STR(avid))+TTOC(datetime1)+direction+str(calc_wt) to temp &&re-sorts the list so that it is ordered by avid and datetime1
copy to temp
zap
appe from temp
set uniq off
set index to
set safe on

*do daysbtwn
*do tripsum

CLOSE ALL
ERASE datelist.dbf
ERASE temp.dbf
ERASE temp.idx
ERASE temp_wbdata.dbf
USE avidsum
BROWSE
CLOSE ALL
SET talk ON

************************
PROCEDURE find_end_event
************************
mrec=RECNO()
mtime1=datetime
mt1ds=val(right(alltrim(datetime_1),3))&&this is a counter that in theory can go to 75 (90?) in any given second, but sometimes manages to go higher.
mtime2=mtime1
mt2ds=mt1ds
DIMENSION id_array(10,2)
id_count=0
mid=0
mba=0 &&bad_avid
opto_12_count=0

DO WHILE LEFT(event,12)!="End of event" AND ALLTRIM(event)!=mend_event AND NOT EOF() &&finds all the ID's in an event
		
		IF ALLTRIM(event)="ID" AND ASCAN(id_array,avid)=0 &&only adds new id to array if it is first ID found, or different than previous ID found in this event
			id_count=id_count+1
			STORE avid TO id_array(id_count)
			STORE bad_avid TO id_array(id_count,2)							 
		ENDIF
		
		IF ALLTRIM(event)="opto 12"
			opto_12_count=opto_12_count+1
		ENDIF
		
		IF LEFT(event, 4)="opto" 
			mtime2=datetime
			mt2ds=val(right(alltrim(datetime_1),3))&&this is a counter that in theory can go to 75 in any given second, but sometimes manages to go higher.
            *tracking the last opto of the event for calculating crossing duration
		ENDIF
		
	SKIP
 
ENDDO

IF left(event,12)="End of event" OR ALLTRIM(event)=mend_event &&note: this is a fixed number of seconds from the triggering opto as determined by the event window parameter in the WB code...
	mrec2=RECNO()
ELSE
	mrec2=0
ENDIF

IF id_count=0
	store 9999 to id_array(1)
	STORE 9999 TO id_array(1,2)
	id_count=1
ENDIF

IF id_count>0 &&in this case will process for all birds - even without ID
	*get tare weight
	DIMENSION mtare(1)
	SELECT avg(weight) FROM wbdata_tares WHERE alltrim(event)="tare" AND ABS(datetime-mtime1)<=3600 INTO ARRAY mtare &&all tare weights within an hour
	USE &&need to close wbdata_tares or run into memory issues
	IF _tally=0 &&no tares within an hour? should not happen
		STORE 20 TO mtare(1) &&will produce some obviously wrong weights...
	ENDIF
	
	use wbdata
	
	******	
	*get penguin weights

	*first, create new temporary table that has only the records for the crossing / event
	SET SAFETY off
	COPY for RECNO()>mrec AND RECNO()<mrec2 to temp_wbdata
	SET SAFETY on
	SELECT 0
	USE temp_wbdata
	replace all weight with weight-mtare(1)
	mavgwt=0
	mnavgwt=0
	
	*average weight
	CALCULATE AVG(weight) FOR weight>0 TO mavgwt 
	mavgwt=mavgwt
	mnavgwt=_tally

	*Max weight
	CALCULATE MAX(weight) TO mmaxwt

	*Calculated weight with Standard Deviation (SD)
	SELECT TOP 20 PERCENT weight FROM temp_wbdata WHERE event="weight" AND weight>2.5 ORDER BY weight descending to screen nowait 
	clear
	topwtscount=_tally
	*calculated weight on the basis of top 20% of all weights above 2.5kg (gets averaged below)
	CALCULATE AVG(weight) FOR weight > 2.5 TO mcalcwt 
	CALCULATE STD(weight) FOR weight > 2.5 TO msdcalcwt 
	topwtstot=_tally
	
	*******
	*Find platwts, medwt etc.
	*******
	SELECT temp_wbdata
	DELETE FOR event!="weight"
	PACK
	GO top
	
	wtcount=0

	DECLARE wtarray(400) &&had been 50 in previous versions	
	
	STORE 0 TO wtarray(1)
		
	do while event="weight" .and. .not. eof() AND wtcount<400
			
			mdata=weight
		
			if mdata>=0 &&including zero weights here - might not want to do that
				wtcount=wtcount+1
				mfield="wt"+ltrim(str(wtcount))
				store mdata to wtarray(wtcount)
					
			ELSE &&not storing any zeroes?
			
			ENDIF

		SKIP	
		
	ENDDO

	USE
	
	*So now we have wtarray with all the weights for a given crossing	
	*find the plateaus
	x=1
	mpt=" "
	mpw=0
		
	do while x<=wtcount-9 AND x<=391
		mfield1 = wtarray(x)
		mfield2 = wtarray(x+1)
		mfield3 = wtarray(x+2)
		mfield4 = wtarray(x+3)
		mfield5 = wtarray(x+4)
		mfield6 = wtarray(x+5)
		mfield7 = wtarray(x+6)
		mfield8 = wtarray(x+7)
		mfield9 = wtarray(x+8)
		mfield10= wtarray(x+9)
		
		do case
				
		case abs(mfield1-mfield2)<=.01 .and. abs(mfield1-mfield3)<=.01 .and. abs(mfield1-mfield4)<=.01 .and. ;
			ABS(mfield1-mfield5)<=.01 AND abs(mfield1-mfield6)<=.01 AND abs(mfield1-mfield7)<=0.1 AND abs(mfield1-mfield8)<=0.1 ;
			AND ABS(mfield1-mfield9)<=0.1 AND ABS(mfield1-mfield10)<=0.1 .and. mfield1>2.0 
			x=401
			mpt="10x10g"
			mpw=mfield1
		
		case abs(mfield1-mfield2)<=.01 .and. abs(mfield1-mfield3)<=.01 .and. abs(mfield1-mfield4)<=.01 .and. ;
			ABS(mfield1-mfield5)<=.01 AND abs(mfield1-mfield6)<=.01 AND abs(mfield1-mfield7)<=0.1 AND abs(mfield1-mfield8)<=0.1 ;
			AND ABS(mfield1-mfield9)<=0.1 .and. mfield1>2.0 
			x=401
			mpt="9x10g"
			mpw=mfield1
				
		case abs(mfield1-mfield2)<=.01 .and. abs(mfield1-mfield3)<=.01 .and. abs(mfield1-mfield4)<=.01 .and. ;
			ABS(mfield1-mfield5)<=.01 AND abs(mfield1-mfield6)<=.01 AND abs(mfield1-mfield7)<=0.1 AND abs(mfield1-mfield8)<=0.1 ;
			.and. mfield1>2.0 
			x=401
			mpt="8x10g"
			mpw=mfield1
		
		case abs(mfield1-mfield2)<=.01 .and. abs(mfield1-mfield3)<=.01 .and. abs(mfield1-mfield4)<=.01 .and. ;
			ABS(mfield1-mfield5)<=.01 AND abs(mfield1-mfield6)<=.01 AND abs(mfield1-mfield7)<=0.1 .and. mfield1>2.0 
			x=401
			mpt="7x10g"
			mpw=mfield1
		
		case abs(mfield1-mfield2)<=.01 .and. abs(mfield1-mfield3)<=.01 .and. abs(mfield1-mfield4)<=.01 .and. ;
			ABS(mfield1-mfield5)<=.01 AND abs(mfield1-mfield6)<=.01 .and. mfield1>2.0 
			x=401
			mpt="6x10g"
			mpw=mfield1
		
		case abs(mfield1-mfield2)<=.01 .and. abs(mfield1-mfield3)<=.01 .and. abs(mfield1-mfield4)<=.01 .and. ;
			ABS(mfield1-mfield5)<=.01 .and. mfield1>2.0 
			x=401
			mpt="5x10g"
			mpw=mfield1
		case abs(mfield1-mfield2)<=.01 .and. abs(mfield1-mfield3)<=.01 .and. abs(mfield1-mfield4)<=.01 .and. mfield1>2.0 
			x=401
			mpt="4x10g"
			mpw=mfield1
		case abs(mfield1-mfield2)<=.01 .and. abs(mfield1-mfield3)<=.01 .and. mfield1>2.0
			x=401
			mpt="3x10g"
			mpw=mfield1
		case abs(mfield1-mfield2)<=.02 .and. abs(mfield1-mfield3)<=.02 .and. mfield1>2.0 &&20 grams
			x=401
			mpt="3x20g"
			mpw=mfield1
		otherwise
			x=x+1
		endcase

	ENDDO

	*find the median weight
	IF wtcount>1
			mwt="wtarray("+ALLTRIM(STR(INT(wtcount/2)))+")"
		ELSE
			mwt="wtarray(1)"
	endif
		mmedwt = &mwt
		
	*calculate decimal seconds
	if mt2ds>75
		mt2ds=75
	endif
	if mt1ds>75
		mt1ds=75
	endif
	
	IF mt2ds<1
		mt2ds=1
	ENDIF
	IF mt1ds<1
		mt1ds=1
	ENDIF
	
	mt2ds=mt2ds/75 &&what proportion of a second, if a second is=75? 
	mt1ds=mt1ds/75
	
	mtime2=mtime2+mt2ds
	mtime1=mtime1+mt1ds
	
	mxtime=mtime2-mtime1
	IF mxtime>99999
		mxtime=99999
	ENDIF
 
	******	
	
	*write data to avidsum table
	SELECT avidsum
	
	IF opto_12_count>0
		mdir="unk"
	ENDIF
	
	FOR appno=1 TO id_count
		mid = id_array(appno)
		mba = id_array(appno,2)
				
		APPEND BLANK
  
		REPLACE avid WITH mid, bad_avid WITH mba, direction WITH mdir, datetime1 WITH mtime1, datetime2 WITH mtime2, xingtime WITH mxtime, ;
				avgwt WITH mavgwt, naw WITH mnavgwt, calc_wt WITH mcalcwt, sd_calcwt WITH msdcalcwt, ncw WITH topwtscount, maxwt WITH mmaxwt, method WITH "top20avg", ;
				wbrn WITH mrec2, nw WITH wtcount, platwt WITH mpw, plattype WITH mpt, medwt WITH mmedwt
				
				IF wtcount>9
					FOR i=2 TO 10
						mfield="wt"+ALLTRIM(STR(i*10))+"dwt10"		
						IF wtcount>=i*10
							replace &mfield WITH wtarray(i*10)-wtarray(10)
						ENDIF
					ENDFOR
				ENDIF
				
				IF wtcount>=200
					replace wt200dwt10 WITH wtarray(200)-wtarray(10)
				ENDIF	
	ENDFOR
	
	*reset vars
	id_count=0
	topwtstot=0
	topwtscount=0
	mmaxwt=0
	mavgwt=0
	mdir=" "
	mid=0
	mba=0 &&bad_avid
	mtime1=CTOT(" ")
	mtime2=CTOT(" ")
	mt1ds=0
	mt2ds=0
	mpt=" "
	mpw=0
	wtcount=0
	mmedwt=0
	wtarray=0
	******
	
ENDIF

SELECT wbdata

ON ERROR do nothing
	GOTO mrec2
ON ERROR

****************************
proc daysbtwn
****************************
set safe off
close all
use avidsum
repl all daysbtwn with 0

sort on avid, datetime1 to temp
zap
appe from temp
set safe on
go top
do while .not. eof()
	mavid=avid
	mdate1=datetime1
	mdir=direction
	do while avid=mavid
		skip
		if avid=mavid .and. mdir<>direction
			mdate2=datetime1
			repl daysbtwn with (mdate2-mdate1)/86400			
			mdate1=datetime1
			mdir=direction
		else &&same band but same direction
			if avid=mavid
				mdate1=datetime1
				mdir=direction
			endif
		endif
	enddo
enddo

go top
do while .not. eof()
	
	mavid=avid
	sele c
	
	IF FILE("..\..\wb_band_data.dbf")
		use ..\..\wb_band_data &&note that wb_band_data.dbf has to be here in order for this to work
	ELSE
		CLEAR
		? "Please find and open wb_band_data.dbf: "
		GO top	
	ENDIF
	
	loca for avid=mavid &&relies on the avid field in banddata being an integer
	if found()
		msex=sex
		mband=bandnumb
		mmate=mate
	else
		msex="U"
		mband=99999
		mmate=99999
	endif
	sele a
	repl sex with msex, band with mband, mate with mmate
	skip		
enddo

*average daysbtwn for daysbtwn>=.25 .and. direction="in" .and. sex="F" to fdb
SELECT avg(daysbtwn), count(daysbtwn) FROM avidsum where daysbtwn>=0.25 AND direction="in" AND sex="F" into ARRAY fdb
*average daysbtwn for daysbtwn>=.25 .and. direction="in" .and. sex="M" to mdb
SELECT avg(daysbtwn), count(daysbtwn) FROM avidsum where daysbtwn>=0.25 AND direction="in" AND sex="M" into ARRAY mdb
*average daysbtwn for daysbtwn>=.25 .and. direction="in"  to tdb
SELECT avg(daysbtwn), count(daysbtwn) FROM avidsum where daysbtwn>=0.25 AND direction="in" into ARRAY tdb


clear
? "The average triplength (including unk sex) was "+ALLTRIM(STR(tdb(1),4,2)) +" days (n= "+ALLTRIM(STR(tdb(2)))+")"
? "The average triplength for females was "+ALLTRIM(STR(fdb(1),4,2)) +" days (n="+ALLTRIM(STR(fdb(2)))+")"
? "The average triplength for males was "+ALLTRIM(STR(mdb(1),4,2)) +" days (n="+ALLTRIM(STR(mdb(2)))+")"
wait

*brow fields band, date2, time1, direction, daysbtwn
close all

***************
proc tripsum
***************
*simple version of trip duration calculations - uses all data
*unlike travtime2, which uses only nests with young

close all
set safe off
sele a
use avidsum
set uniq on
index on datetime1 to temp
copy field datetime1 to datelist
set uniq off
set index to

sele b
use datelist

sele c
on error do createtripsum
use tripsum
on error
zap

set safe on
sele b

do while .not. eof()
	mdate=datetime1
	sele a
	average(daysbtwn) for datetime1=mdate .and. daysbtwn>.25 .and. daysbtwn<21 .and. alltrim(direction)="in" to mtrip 
	coun for datetime1=mdate .and. daysbtwn>=.25 .and. alltrim(direction)="in" .and. daysbtwn<21 to mn 
	sele c
	appe blank
	repl date with mdate, triplen with mtrip, n with mn
	sele b
	skip
enddo

close all
*use tripsum
*brow

*************
proc createtripsum
crea table tripsum (date d, triplen n(4,2), n n(3,0), maletr n(4,2), nm n(3,0), femtr n(4,2), nf n(3,0))
************
PROCEDURE create_avidsum 
CREATE TABLE avidsum(avid n(9), bad_avid n(9), datetime1 t, datetime2 t, xingtime n(6,2), ;
	direction c(10), avgwt n(9,3), maxwt n(9,3), naw n(9,0), calc_wt n(9,3), sd_calcwt n(5,2), ;
	ncw n(9,0), method c(10),wbrn n(10),plattype c(5), platwt n(4,2), ;
	nw n(3), medwt n(4,2), wt20dwt10 n(4,2), wt30dwt10 n(4,2), ;
	wt40dwt10 n(4,2), wt50dwt10 n(4,2), wt60dwt10 n(4,2), wt70dwt10 n(4,2), wt80dwt10 n(4,2), ;
	wt90dwt10 n(4,2), wt100dwt10 n(4,2), wt200dwt10 n(4,2))
	*wbrn for wb data record number
***********
PROCEDURE nothing
*does nothing
***********
***********
FUNCTION avidstars
PARAMETERS mavid
*mavid=VAL(mavid)
avidstring=ALLTRIM(STR(mavid))

DO case
	case(mavid)<=9999999 and mavid !=0
		astar="00"+LEFT(avidstring,1)+"*"+SUBSTR(avidstring,2,3)+"*"+RIGHT(avidstring,3)
	CASE LEFT(avidstring,3)="583"
		astar=LEFT(avidstring,3)+"*"+SUBSTR(avidstring,4,3)+"*"+RIGHT(avidstring,3)
	case(mavid)>9999999
		astar="0"+LEFT(avidstring,2)+"*"+SUBSTR(avidstring,3,3)+"*"+RIGHT(avidstring,3)
	OTHERWISE
		astar=" "
ENDCASE
RETURN astar
