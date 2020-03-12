       IDENTIFICATION DIVISION.
       PROGRAM-ID. CBLHJB01.
	   AUTHOR. HARRISON BIRKNER.

      **************WHAT NEEDS DONE***************
      *2. DAT FILE
      *3. TESTING
      *4. MAKE COPYBOOKS
      ********************************************

       ENVIRONMENT DIVISION.
		   SELECT CAMPRES-INPUT
			   ASSIGN TO 'C:\COBOLSP20\RESERVE.DAT'
				   ORGANIZATION IS LINE SEQUENTIAL.

		   SELECT PRTOUT
			   ASSIGN TO 'C:\COBOLSP20\CAMPRES.DAT'
				   ORGANIZATION IS LINE SEQUENTIAL.
           
           SELECT ERROUT
			   ASSIGN TO 'C:\COBOLSP20\ERR.PRT'
				   ORGANIZATION IS RECORD SEQUENTIAL.

	   FILE SECTION.
	   FD CAMPRES-INPUT
	   LABEL RECORD IS STANDARD
	   DATA RECORD IS TRAN-REC
	   RECORD CONTAINS 108 CHARACTERS.

	   01 TRAN-REC.
           05	I-CAMPGROUND	PIC X(25).
           05	I-SITE.
               10  I-SITE1      PIC X.
                   88 VAL-CAMPGROUND   VALUE 'A' THRU 'Z'.
               10  I-SITE2      PIC 99.
	       05	I-DATE		    PIC 9(8).
	       05	I-LEN-STAY		PIC 99.	
	       05	I-LNAME		    PIC X(20).	
	       05	I-FNAME		    PIC X(20).	
           05	I-AMT			PIC S9(3)V99.
	       05	I-CCTYPE		PIC X.
               88 VAL-CCTYPE    VALUE 'V', 'M', 'A'.
	       05	I-CCNUM		    PIC 9(16).	
	       05	I-CCEXP		    PIC 9(8).	


	   FD PRTOUT
	   LABEL RECORD IS OMITTED
	   DATA RECORD IS VAL-REC
	   RECORD CONTAINS 136 CHARACTERS.

       01 VAL-REC.
	       05	O-CAMPGROUND	PIC X(25).
           05	O-SITE		    PIC X99.
	       05	O-DATE		    PIC 9(8).	
	       05	O-END-DATE		PIC 9(8).
	       05	O-LEN-STAY		PIC 99.	
	       05	O-NAME		    PIC X(42).	
           05	O-AMT			PIC S9(3)V99.	
	       05	O-CCTYPE		PIC X(16).	
	       05	O-CCNUM		    PIC X(19).	
	       05	O-CCEXP		    PIC 9(8).


	   FD ERROUT
	   LABEL RECORD IS OMITTED
	   DATA RECORD IS ERRLINE
	   RECORD CONTAINS 132 CHARACTERS
	   LINAGE IS 60 WITH FOOTING AT 56.

	   01 ERRLINE               PIC X(132).

       WORKING-STORAGE SECTION.
       01 SWITCHES.
           05 FIRST-ERR-SW      PIC X.
           05 ERR-SW            PIC X.
           05 SITE1-SW          PIC X.
           05 SITE2-SW          PIC X.
           05 AMNT-SW           PIC X.
           05 LEN-STAY-SW       PIC X.
           05 MORE-RECS         PIC X.
       01 CURRENT-DATE-AND-TIME.
           05 CURRENT-DATE.
		       10 CURRENT-YEAR  PIC X(4).	  
		       10 CURRENT-MONTH PIC XX.	  
		       10 CURRENT-DAY   PIC XX.	  
		   05 CURRENT-TIME      PIC X(11).
       01 MISC.
           05 ERR-PAGE-CTR      PIC 99     VALUE 0.
           05 C-REC-ERR-CTR     PIC 99     VALUE 0.
           05 C-TOT-ERR-CTR     PIC 99     VALUE 0.
       01 CALCS.
           05 C-AMT             PIC S9(3)V99.
           05 C-END-DATE        PIC 9(8).
           05 DATE-TYPE         PIC X.
           05 C-LEAP            PIC 9(4).

       01 WK-DATE.
	       05 WK-YYYY	        PIC 9(4).
	       05 WK-MM	            PIC 99.
	       	   88 VAL-MM	VALUE 1 THRU 12.
	       	   88 VAL-30-MM VALUE 4, 6, 9, 11.
	       	   88 VAL-31-MM VALUE 1, 3, 5, 7, 8, 10, 12.
	       05 WK-DD	PIC 99.
	       	   88 VAL-WK-DD-31 VALUE 1 THRU 31.
	       	   88 VAL-WK-DD-30 VALUE 1 THRU 30.
	       	   88 VAL-WK-DD-28 VALUE 1 THRU 28.
	       	   88 VAL-WK-DD-29 VALUE 1 THRU 29.

       01 TBL-WK-DATE-NUM REDEFINES WK-DATE.
           05 WK-DATE-NUM       PIC 9(8).

       01  ERR-TABLE.
           05  FILLER                      PIC X(100)
               VALUE 'CAMPGROUND MUST NOT BE "BUCK CREEK", "ISLAND VIEW"
      -        ', OR "HONEY CREEK".'.
           05  FILLER                      PIC X(100)
               VALUE 'FIRST POSITION OF SITE MUST BE A LETTER'.
           05  FILLER                      PIC X(100)
               VALUE 'SECOND PART OF SITE MUST BE NUMERIC'.
           05  FILLER                      PIC X(100)
               VALUE 'SECOND PART OF SITE MUST BE GREATER THAN ZERO'.
           05  FILLER                      PIC X(100)
               VALUE 'RESERVATION DATE NOT NUMERIC'.
           05  FILLER                      PIC X(100)
               VALUE 'RESERVATION MONTH MUST BE 1-12'.
           05  FILLER                      PIC X(100)
               VALUE 'RESERVATION DAY MUST BE 1-30 FOR MONTH'.
           05  FILLER                      PIC X(100)
               VALUE 'RESERVATION DAY MUST BE 1-31 FOR MONTH'.
           05  FILLER                      PIC X(100)
               VALUE 'RESERVATION DAY MUST BE 1-29 FOR MONTH'.
           05  FILLER                      PIC X(100)
               VALUE 'RESERVATION DAY MUST BE 1-28 FOR MONTH'.
           05  FILLER                      PIC X(100)
               VALUE 'RESERVATION DATE MUST BE AFTER TODAY'.
           05  FILLER                      PIC X(100)
               VALUE 'LENGTH OF STAY MUST BE NUMERIC'.
           05  FILLER                      PIC X(100)
               VALUE 'LENGTH OF STAY MUST BE 2-11'.
           05  FILLER                      PIC X(100)
               VALUE 'LAST NAME MUST NOT BE BLANK'.
           05  FILLER                      PIC X(100)
               VALUE 'FIRST NAME MUST NOT BE BLANK'.
           05  FILLER                      PIC X(100)
               VALUE 'AMOUNT MUST BE NUMERIC'.
           05  FILLER                      PIC X(100)
               VALUE 'AMOUNT MUST BE LENGTH OF STAY * SITE FEE'.
           05  FILLER                      PIC X(100)
               VALUE 'CREDIT CARD TYPE MUST BE "V", "M", OR "A"'.
           05  FILLER                      PIC X(100)
               VALUE 'CREDIT CARD NUMBER MUST BE NUMERIC'.
           05  FILLER                      PIC X(100)
               VALUE 'CREDIT CARD EXPIRATION DATE NOT NUMERIC'.
           05  FILLER                      PIC X(100)
               VALUE 'CREDIT CARD EXPIRATION MONTH MUST BE 1-12'.
           05  FILLER                      PIC X(100)
              VALUE 'CREDIT CARD EXPIRATION DAY MUST BE 1-30 FOR MONTH'.
           05  FILLER                      PIC X(100)
              VALUE 'CREDIT CARD EXPIRATION DAY MUST BE 1-31 FOR MONTH'.
           05  FILLER                      PIC X(100)
              VALUE 'CREDIT CARD EXPIRATION DAY MUST BE 1-29 FOR MONTH'.
           05  FILLER                      PIC X(100)
              VALUE 'CREDIT CARD EXPIRATION DAY MUST BE 1-28 FOR MONTH'.
           05  FILLER                      PIC X(100)
               VALUE 'CREDIT CARD EXPIRATION DATE MUST BE AFTER TODAY'.

       01  TABLE-ERR REDEFINES ERR-TABLE.
           05  ERR-MSG                     PIC X(100)  OCCURS 26 TIMES.

       01 SITES.
           05  FILLER           PIC X(3)   VALUE 'A10'.
           05  FILLER           PIC X(3)   VALUE 'B10'.
           05  FILLER           PIC X(3)   VALUE 'C10'.
           05  FILLER           PIC X(3)   VALUE 'D12'.
           05  FILLER           PIC X(3)   VALUE 'E12'.
           05  FILLER           PIC X(3)   VALUE 'F12'.
           05  FILLER           PIC X(3)   VALUE 'G12'.
           05  FILLER           PIC X(3)   VALUE 'H12'.
           05  FILLER           PIC X(3)   VALUE 'I12'.
           05  FILLER           PIC X(3)   VALUE 'J12'.
           05  FILLER           PIC X(3)   VALUE 'K12'.
           05  FILLER           PIC X(3)   VALUE 'L12'.
           05  FILLER           PIC X(3)   VALUE 'M12'.
           05  FILLER           PIC X(3)   VALUE 'N14'.
           05  FILLER           PIC X(3)   VALUE 'O14'.
           05  FILLER           PIC X(3)   VALUE 'P14'.
           05  FILLER           PIC X(3)   VALUE 'Q14'.
           05  FILLER           PIC X(3)   VALUE 'R14'.
           05  FILLER           PIC X(3)   VALUE 'S14'.
           05  FILLER           PIC X(3)   VALUE 'T14'.
           05  FILLER           PIC X(3)   VALUE 'U14'.
           05  FILLER           PIC X(3)   VALUE 'V14'.
           05  FILLER           PIC X(3)   VALUE 'W14'.
           05  FILLER           PIC X(3)   VALUE 'X14'.
           05  FILLER           PIC X(3)   VALUE 'Y14'.
           05  FILLER           PIC X(3)   VALUE 'Z14'.

       01 TBL-SITE REDEFINES SITES.
           05 TBL-SITE-INFO     OCCURS 26 TIMES   INDEXED BY SITE-INDEX.
               10 TBL-SITE-NAME     PIC X.
               10 TBL-SITE-PRICE    PIC 99.

       01 ERR-TITLE-LINE.
		   05 FILLER                  PIC X(6)    VALUE 'DATE: '.
		   05 ERR-TITLE-DATE.					   
		       10 ERR-TITLE-MONTH     PIC XX.	  
			   10 FILLER              PIC X       VALUE '/'.
			   10 ERR-TITLE-DAY       PIC XX.	  
		       10 FILLER              PIC X       VALUE '/'.
			   10 ERR-TITLE-YEAR      PIC X(4).	  
		   05 FILLER                  PIC X(37)   VALUE SPACES.
		   05 FILLER                  PIC X(25)	  
              VALUE 'RATHBUN LAKE RESERVATIONS'. 
		   05 FILLER                  PIC X(46)   VALUE SPACES.
		   05 FILLER                  PIC X(6)    VALUE 'PAGE: '.
		   05 ERR-TITLE-PAGE          PIC Z9.

	   01 ERR-TITLE-LINE2.
		   05 FILLER                  PIC X(60)   VALUE SPACES.
		   05 FILLER                  PIC X(12)   VALUE 'ERROR REPORT'.

       01 ERR-COL-HEADING.
           05 FILLER                  PIC X(12)   VALUE 
              'ERROR RECORD'.

       01 ERR-COL-HEADING2.
		   05 FILLER                  PIC X(100) VALUE 
              'ERROR DESCRIPTION'.


       01 ERROR-LINE.
           05 O-ERR                   PIC X(108).

       01 ERR-MSG-LINE.
           05 O-ERR-MSG               PIC X(100).

       01 ERR-TOTAL-HEADING.
           05 FILLER                  PIC X(8)    VALUE 'TOTALS: '.
           05 FILLER                  PIC X(17)   VALUE 
           'ERRONEOUS RECORDS'.
           05 FILLER                  PIC XX      VALUE SPACES.
           05 FILLER                  PIC X(6)    VALUE 'ERRORS'.

       01 ERR-TOTALS.
           05 FILLER                  PIC X(23)   VALUE SPACES.
           05 O-REC-ERR-CTR           PIC Z9.
           05 FILLER                  PIC X(6)    VALUE SPACES.
           05 O-TOT-ERR-CTR           PIC Z9.

       PROCEDURE DIVISION.
       L1-MAIN.
           PERFORM L2-INIT.
           PERFORM L2-MAINLINE
               UNTIL MORE-RECS = 'N'.
           PERFORM L2-CLOSING.
           STOP RUN.

       L2-INIT.
           OPEN INPUT CAMPRES-INPUT.
           OPEN OUTPUT PRTOUT.
           OPEN OUTPUT ERROUT.
           MOVE FUNCTION CURRENT-DATE          TO CURRENT-DATE-AND-TIME.
           MOVE CURRENT-MONTH                  TO ERR-TITLE-MONTH.
           MOVE CURRENT-DAY                    TO ERR-TITLE-DAY.
           MOVE CURRENT-YEAR                   TO ERR-TITLE-YEAR.
           MOVE 'Y'                            TO MORE-RECS.
       
           PERFORM L3-INIT-HEADING.
           PERFORM L9-READ-INPUT.

       L2-MAINLINE.
           PERFORM L3-VALIDATION
               THRU L3-VALIDATION-EXIT.
           IF ERR-SW = 'N'
               PERFORM L3-CALCS
               PERFORM L3-MOVE-PRINT
           END-IF.
           PERFORM L9-READ-INPUT.

       L2-CLOSING.
           PERFORM L3-ERR-TOTALS.
           CLOSE CAMPRES-INPUT.
           CLOSE PRTOUT.
           CLOSE ERROUT.

       L3-INIT-HEADING.                                                       
           MOVE 1                               TO ERR-PAGE-CTR.
           MOVE ERR-PAGE-CTR                   TO ERR-TITLE-PAGE.
           WRITE ERRLINE FROM ERR-TITLE-LINE
               AFTER ADVANCING 1 LINE.
           WRITE ERRLINE FROM ERR-TITLE-LINE2
              AFTER ADVANCING 1 LINE.
           WRITE ERRLINE FROM ERR-COL-HEADING
              AFTER ADVANCING 2 LINES.

       L3-VALIDATION.
           MOVE 'N' TO ERR-SW.
           MOVE 'N' TO SITE1-SW.
           MOVE 'N' TO SITE2-SW
           MOVE 'N' TO AMNT-SW.
           MOVE 'N' TO LEN-STAY-SW.
           MOVE 'Y' TO FIRST-ERR-SW.
           IF NOT VAL-CAMPGROUND
           	   MOVE ERR-MSG(1) TO O-ERR-MSG
           	   MOVE 'Y' TO ERR-SW
           	   PERFORM L4-ERROR-PRINT.
           IF I-SITE1 NOT ALPHABETIC
           	   MOVE ERR-MSG(2) TO O-ERR-MSG
           	   MOVE 'Y' TO ERR-SW
           	   MOVE 'Y' TO SITE1-SW
           	   PERFORM L4-ERROR-PRINT.
           IF I-SITE2 NUMERIC
           	   IF I-SITE2 > 0
           		   SET SITE-INDEX TO 1
	               SEARCH TBL-SITE-INFO
                          WHEN I-SITE = TBL-SITE-NAME(SITE-INDEX)
		                  COMPUTE C-AMT = I-LEN-STAY *
                                       TBL-SITE-PRICE(SITE-INDEX)
               ELSE
                   MOVE ERR-MSG(4) TO O-ERR-MSG
                   MOVE 'Y' TO SITE2-SW
                   MOVE 'Y' TO ERR-SW
                   PERFORM L4-ERROR-PRINT
               END-IF
           ELSE
           	   MOVE ERR-MSG(3) TO O-ERR-MSG
           	   MOVE 'Y' TO SITE2-SW
               MOVE 'Y' TO ERR-SW
           	   PERFORM L4-ERROR-PRINT
           END-IF.
           
           MOVE I-DATE TO WK-DATE.
           MOVE 'R' TO DATE-TYPE.
           PERFORM L5-DATE-VAL
           	  THRU L5-DATE-VAL-EXIT.
           	
           IF I-LEN-STAY NUMERIC
           	   IF I-LEN-STAY > 1
           	   	   IF I-LEN-STAY < 12
           	   		   NEXT SENTENCE
           	       ELSE
           	   	       MOVE ERR-MSG(13) TO O-ERR-MSG
           	   	       MOVE 'Y' TO ERR-SW
           	   	       PERFORM L4-ERROR-PRINT
           	       END-IF
               ELSE
           	   	   MOVE ERR-MSG(13) TO O-ERR-MSG
           	   	   MOVE 'Y' TO ERR-SW
           	   	   PERFORM L4-ERROR-PRINT
           	   END-IF
           ELSE
           	   MOVE ERR-MSG(12) TO O-ERR-MSG
           	   MOVE 'Y' TO ERR-SW
           	   MOVE 'Y' TO LEN-STAY-SW
           	   PERFORM L4-ERROR-PRINT
           END-IF.

           IF I-LNAME = SPACES
           	   MOVE ERR-MSG(14) TO O-ERR-MSG
           	   MOVE 'Y' TO ERR-SW
           	   PERFORM L4-ERROR-PRINT.

           IF I-FNAME = SPACES
           	   MOVE ERR-MSG(15) TO O-ERR-MSG
           	   MOVE 'Y' TO ERR-SW
           	   PERFORM L4-ERROR-PRINT.

           IF I-AMT NUMERIC
               SET SITE-INDEX TO 1
	           SEARCH TBL-SITE-INFO
               WHEN I-SITE = TBL-SITE-NAME(SITE-INDEX)
		       COMPUTE C-AMT = I-LEN-STAY *
                               TBL-SITE-PRICE(SITE-INDEX)
               IF C-AMT NOT EQUAL I-AMT
           	       MOVE 'Y' TO ERR-SW
           	       MOVE ERR-MSG(17) TO O-ERR-MSG
           	       PERFORM L4-ERROR-PRINT
           ELSE
               MOVE 'Y' TO ERR-SW
           	   MOVE 'Y' TO AMNT-SW
           	   MOVE ERR-MSG(16) TO O-ERR-MSG
           	   PERFORM L4-ERROR-PRINT
           END-IF.
           
           IF NOT VAL-CCTYPE
           	   MOVE 'Y' TO ERR-SW
           	   MOVE ERR-MSG(18) TO O-ERR-MSG
           	   PERFORM L4-ERROR-PRINT.

           IF I-CCNUM NOT NUMERIC
           	   MOVE 'Y' TO ERR-SW
           	   MOVE ERR-MSG(19) TO O-ERR-MSG
           	   PERFORM L4-ERROR-PRINT.
           	
       MOVE I-CCEXP TO WK-DATE.
       MOVE 'C' TO DATE-TYPE.
       PERFORM L5-DATE-VAL
	      THRU L5-DATE-VAL-EXIT.
           
       L3-VALIDATION-EXIT.
           EXIT.

       L3-CALCS.

           MOVE I-DATE TO WK-DATE.

           IF VAL-31-MM
               ADD I-LEN-STAY TO WK-DD
               IF NOT VAL-WK-DD-31
                   ADD 1 TO WK-MM
                   SUBTRACT 31 FROM WK-DD
                   IF NOT VAL-MM
                       MOVE 1 TO WK-MM
                       ADD 1 TO WK-YYYY
                   END-IF
               END-IF
           ELSE IF VAL-30-MM
               ADD I-LEN-STAY TO WK-DD
               IF NOT VAL-WK-DD-30
                   ADD 1 TO WK-MM
                   SUBTRACT 30 FROM WK-DD
               END-IF
           ELSE IF WK-MM = 2
               ADD I-LEN-STAY TO WK-DD
               IF NOT VAL-WK-DD-29
                   ADD 1 TO WK-MM
                   SUBTRACT 29 FROM WK-DD
                   END-IF
               ELSE
                   ADD 1 TO WK-MM
                   SUBTRACT 28 FROM WK-DD
               END-IF
           END-IF.
	       
           MOVE WK-DATE TO C-END-DATE.

       L3-MOVE-PRINT.
	       EVALUATE I-CCTYPE
		       WHEN 'V'
			       MOVE 'VISA' TO O-CCTYPE
		       WHEN 'M'
			       MOVE 'MASTER CARD' TO O-CCTYPE
		       WHEN 'A'
			       MOVE 'AMERICAN EXPRESS' TO O-CCTYPE
	       END-EVALUATE.
	
	       MOVE I-CAMPGROUND TO O-CAMPGROUND.
	       MOVE I-SITE TO O-SITE.
	       MOVE I-DATE TO O-DATE.
	       MOVE C-END-DATE TO O-END-DATE.
	       MOVE I-LEN-STAY TO O-LEN-STAY.
	       STRING I-LNAME DELIMITED BY "  "
	       	   ', ' DELIMITED BY SIZE
	       	   I-FNAME DELIMITED BY "  "
	       INTO O-NAME.
	       MOVE I-AMT TO O-AMT.
	       MOVE I-CCNUM TO O-CCNUM.
	       MOVE I-CCEXP TO O-CCEXP.
           
           WRITE VAL-REC.

       L3-ERR-TOTALS.
           MOVE C-TOT-ERR-CTR TO O-TOT-ERR-CTR.
           MOVE C-REC-ERR-CTR TO O-REC-ERR-CTR.

           WRITE ERRLINE FROM ERR-TOTAL-HEADING
               AFTER ADVANCING 2 LINES.
           WRITE ERRLINE FROM ERR-TOTALS
               AFTER ADVANCING 1 LINE.

       L4-ERROR-PRINT.
	       IF FIRST-ERR-SW = 'Y'
		       MOVE 'N' TO FIRST-ERR-SW
		       ADD 1 TO C-REC-ERR-CTR
		       MOVE TRAN-REC TO O-ERR
		       WRITE ERRLINE FROM ERROR-LINE
			       AFTER ADVANCING 2 LINES
			          AT EOP
			       PERFORM L4-ERROR-HEADING
               WRITE ERRLINE FROM ERR-COL-HEADING2
		       WRITE ERRLINE FROM ERR-MSG-LINE
	       ELSE
		       WRITE ERRLINE FROM ERR-MSG-LINE
			   AFTER ADVANCING 1 LINE
				   AT EOP
					   PERFORM L4-ERROR-HEADING
		   ADD 1 TO C-TOT-ERR-CTR
	       END-IF.

       L4-ERROR-HEADING.
           ADD 1 TO ERR-PAGE-CTR.
           MOVE ERR-PAGE-CTR TO ERR-TITLE-PAGE.
           WRITE ERRLINE FROM ERR-TITLE-LINE
               AFTER ADVANCING PAGE.
           WRITE ERRLINE FROM ERR-TITLE-LINE2
               AFTER ADVANCING 1 LINE.
           WRITE ERRLINE FROM ERR-COL-HEADING
               AFTER ADVANCING 2 LINES.

       L5-DATE-VAL.
	       IF WK-DATE-NUM NOT NUMERIC
	      	   IF DATE-TYPE = 'R'
	      	       MOVE ERR-MSG(5) TO O-ERR-MSG
	      	   ELSE
	      	   	   MOVE ERR-MSG(20) TO O-ERR-MSG
	      	   END-IF
	      	   PERFORM L4-ERROR-PRINT
           ELSE
               IF NOT VAL-MM
	      	       IF DATE-TYPE = 'R'
	      	           MOVE ERR-MSG(6) TO O-ERR-MSG
	      	       ELSE
	      	   	       MOVE ERR-MSG(21) TO O-ERR-MSG
	      	       END-IF
	      	       PERFORM L4-ERROR-PRINT
	           IF VAL-30-MM AND NOT VAL-WK-DD-30
	       	       IF DATE-TYPE = 'R'
	      	           MOVE ERR-MSG(7) TO O-ERR-MSG
	      	       ELSE
	      	   	       MOVE ERR-MSG(22) TO O-ERR-MSG
	      	       END-IF
	      	       PERFORM L4-ERROR-PRINT
	           IF VAL-31-MM AND NOT VAL-WK-DD-31
	      	       IF DATE-TYPE = 'R'
	      	           MOVE ERR-MSG(8) TO O-ERR-MSG
	      	       ELSE
	      	       	   MOVE ERR-MSG(23) TO O-ERR-MSG
	      	       END-IF
	      	       PERFORM L4-ERROR-PRINT
	           IF WK-MM = 2
	      	       DIVIDE WK-YYYY BY 4 GIVING C-LEAP REMAINDER C-LEAP
	      	       IF C-LEAP = 0
	      	       	   IF NOT VAL-WK-DD-29
	      	               IF DATE-TYPE = 'R'
	      	                   MOVE ERR-MSG(9) TO O-ERR-MSG
	      	               ELSE
	      	               	   MOVE ERR-MSG(24) TO O-ERR-MSG
	      	               END-IF
	      	               PERFORM L4-ERROR-PRINT
	      	       	   ELSE
	      	       		   NEXT SENTENCE
	      	       	   END-IF
	      	       ELSE
	      	       	   IF NOT VAL-WK-DD-28
	      	               IF DATE-TYPE = 'R'
	      	                   MOVE ERR-MSG(10) TO O-ERR-MSG
	      	               ELSE
	      	               	   MOVE ERR-MSG(25) TO O-ERR-MSG
	      	               END-IF
	      	               PERFORM L4-ERROR-PRINT
	           IF WK-DATE-NUM <= CURRENT-DATE
	          	   IF DATE-TYPE = 'R'
	          	       MOVE ERR-MSG(11) TO O-ERR-MSG
	          	   ELSE
	          	   	   MOVE ERR-MSG(26) TO O-ERR-MSG
	          	   END-IF
	          	   PERFORM L4-ERROR-PRINT
           END-IF.

       L5-DATE-VAL-EXIT.
       	   EXIT.

       L9-READ-INPUT.
           READ CAMPRES-INPUT
               AT END
                   MOVE 'N' TO MORE-RECS.

       END PROGRAM CBLHJB01.