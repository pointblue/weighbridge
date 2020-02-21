*sets up multiple subdirs for batch processing wb data

USE wbdata &&run appender program first to populate this
GO bott
total_rec_count=RECNO()
subset_total=10 &&adjust according to how many partitions you want to run
subset_size=total_rec_count/subset_total

subset_counter=1

minrec=0
maxrec=subset_size

SET SAFETY OFF

DO WHILE subset_counter<=subset_total
	mdir="wb_subset_"+ALLTRIM(STR(subset_counter))
	MD &mdir
	wbfilename=mdir+"/wbdata.dbf"
	COPY for RECNO()>minrec AND RECNO()<=maxrec to &wbfilename
	COPY FILE wbdatasum_2.prg TO mdir+"/wbdatasum_2.prg" &&puts a copy of the program in each new subdirectory
	minrec=minrec+subset_size
	maxrec=maxrec+subset_size
	subset_counter=subset_counter+1
ENDDO

SET SAFETY ON


