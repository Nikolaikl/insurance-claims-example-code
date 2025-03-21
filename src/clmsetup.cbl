       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLMSETUP.
      *----------------------------------------------------------------*
      * PROGRAM TO CREATE AND POPULATE THE CLAIM FILE                  *
      *----------------------------------------------------------------*
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "INPUT.txt"
           ORGANIZATION IS LINE SEQUENTIAL.
           
           SELECT OUTPUT-FILE ASSIGN TO "OUTPUT.txt"
           ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-RECORD             PIC X(200).
       
       FD OUTPUT-FILE.
       01 OUTPUT-RECORD            PIC X(200).
       
       WORKING-STORAGE SECTION.
       01 WS-INPUT-FIELDS.
          05 WS-CLAIM-ID           PIC X(12).
          05 FILLER                PIC X VALUE ','.
          05 WS-POLICY-NUMBER      PIC X(10).
          05 FILLER                PIC X VALUE ','.
          05 WS-CLAIM-DATE         PIC 9(8).
          05 FILLER                PIC X VALUE ','.
          05 WS-CLAIM-TYPE         PIC X(2).
          05 FILLER                PIC X VALUE ','.
          05 WS-CLAIM-STATUS       PIC X(1).
          05 FILLER                PIC X VALUE ','.
          05 WS-CLAIM-AMOUNT       PIC 9(8)V99.
          05 FILLER                PIC X VALUE ','.
          05 WS-INSURED-AGE        PIC 9(3).
          05 FILLER                PIC X VALUE ','.
          05 WS-YEARS-EMPLOYED    PIC 9(2).
          05 FILLER                PIC X VALUE ','.
          05 WS-ANNUAL-SALARY      PIC 9(7)V99.
          05 FILLER                PIC X VALUE ','.
          05 WS-OCCUPATION-CODE    PIC X(4).
          05 FILLER                PIC X VALUE ','.
          05 WS-JOB-RISK-LEVEL     PIC 9(1).
          05 FILLER                PIC X VALUE ','.
          05 WS-DISABILITY-PCT     PIC 9(3).
          05 FILLER                PIC X VALUE ','.
          05 WS-ACCIDENT-SEVERITY  PIC X(1).
          05 FILLER                PIC X VALUE ','.
          05 WS-DIRECT-COSTS       PIC 9(7)V99.
          05 FILLER                PIC X VALUE ','.
          05 WS-INDUSTRY-CODE      PIC X(4).
          05 FILLER                PIC X VALUE ','.
          05 WS-GEO-REGION-CODE    PIC X(3).
       
       01 WS-END-OF-FILE          PIC X VALUE 'N'.
           88 EOF                 VALUE 'Y'.
       
       PROCEDURE DIVISION.
           OPEN INPUT INPUT-FILE
                OUTPUT OUTPUT-FILE.
           
           PERFORM UNTIL EOF
               READ INPUT-FILE
                   AT END
                       SET EOF TO TRUE
                   NOT AT END
                       UNSTRING INPUT-RECORD DELIMITED BY ','
                           INTO WS-CLAIM-ID
                                WS-POLICY-NUMBER
                                WS-CLAIM-DATE
                                WS-CLAIM-TYPE
                                WS-CLAIM-STATUS
                                WS-CLAIM-AMOUNT
                                WS-INSURED-AGE
                                WS-YEARS-EMPLOYED
                                WS-ANNUAL-SALARY
                                WS-OCCUPATION-CODE
                                WS-JOB-RISK-LEVEL
                                WS-DISABILITY-PCT
                                WS-ACCIDENT-SEVERITY
                                WS-DIRECT-COSTS
                                WS-INDUSTRY-CODE
                                WS-GEO-REGION-CODE
                       
      *> Perform calculations (you'll need to add this logic)
      *> For now, just write the input to output
           STRING WS-CLAIM-ID ',' WS-POLICY-NUMBER ','
                  WS-CLAIM-DATE ',' WS-CLAIM-TYPE ','
                  WS-CLAIM-STATUS ',' WS-CLAIM-AMOUNT ','
                  WS-INSURED-AGE ',' WS-YEARS-EMPLOYED ','
                  WS-ANNUAL-SALARY ',' WS-OCCUPATION-CODE ','
                  WS-JOB-RISK-LEVEL ',' WS-DISABILITY-PCT ','
                  WS-ACCIDENT-SEVERITY ',' WS-DIRECT-COSTS ','
                  WS-INDUSTRY-CODE ',' WS-GEO-REGION-CODE
                  DELIMITED BY SIZE
                  INTO OUTPUT-RECORD
                       WRITE OUTPUT-RECORD
               END-READ
           END-PERFORM.
           
           CLOSE INPUT-FILE
                 OUTPUT-FILE.
           
           DISPLAY 'CLAIM PROCESSING COMPLETE'.
           STOP RUN.
