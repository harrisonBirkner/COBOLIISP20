      * TRAN 
	   01 TRAN-REC.
           05	I-CAMPGROUND	PIC X(25).
			   88 VAL-CAMPGROUND  VALUE 'BUCK CREEK', 'HONEY CREEK',
                   'ISLAND VIEW'.
           05	I-SITE.
               10  I-SITE1      PIC X.
               10  I-SITE2      PIC 99.
	       05	I-DATE		    PIC 9(8).
	       05	I-LEN-STAY		PIC 99.	
	       05	I-LNAME		    PIC X(20).	
	       05	I-FNAME		    PIC X(20).	
           05	I-AMT			PIC S9(3)V99.
	       05	I-CCTYPE		PIC X.
               88 VAL-CCTYPE    VALUE 'V', 'M', 'A'.
	       05	I-CCNUM.
		       10 I-CCNUM1    PIC 9(4).
			   10 I-CCNUM2    PIC 9(4).
			   10 I-CCNUM3    PIC 9(4).
			   10 I-CCNUM4    PIC 9(4).
	       05	I-CCEXP		    PIC 9(8).