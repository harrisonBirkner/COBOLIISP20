      * ERROR 
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