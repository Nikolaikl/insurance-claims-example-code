       IDENTIFICATION DIVISION.
       PROGRAM-ID. POLSETUP.
      *----------------------------------------------------------------*
      * PROGRAM TO CREATE AND POPULATE THE POLICY FILE                 *
      *----------------------------------------------------------------*
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT POLICY-FILE ASSIGN TO "POLFILE"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS SEQUENTIAL
           RECORD KEY IS POLICY-NUMBER
           FILE STATUS IS POL-STATUS.
       
       DATA DIVISION.
       FILE SECTION.
       FD POLICY-FILE.
       01 POLICY-RECORD.
          05 POLICY-NUMBER         PIC X(10).
          05 CUSTOMER-ID           PIC X(8).
          05 POLICY-TYPE           PIC X(2).
          05 POLICY-STATUS         PIC X(1).
          05 START-DATE            PIC 9(8).
          05 END-DATE              PIC 9(8).
          05 MONTHLY-PREMIUM       PIC 9(6)V99.
          05 COVERAGE-AMOUNT       PIC 9(8)V99.
          05 INDUSTRY-CODE         PIC X(4).
          05 GEO-REGION-CODE       PIC X(3).
          05 EMR-VALUE             PIC 9V999.
          05 SAFETY-PROG-RATING    PIC 9(1).
       
       WORKING-STORAGE SECTION.
       01 POL-STATUS               PIC X(2).
       01 WS-END-OF-DATA           PIC X(1) VALUE 'N'.
          88 END-OF-DATA           VALUE 'Y'.
       
       PROCEDURE DIVISION.
           OPEN OUTPUT POLICY-FILE.
           
           IF POL-STATUS NOT = '00'
              DISPLAY 'ERROR OPENING POLICY FILE: ' POL-STATUS
              STOP RUN
           END-IF.
           
      * ADD SAMPLE POLICY RECORDS
           
      * Policy 1 - Construction industry with high risk
           MOVE 'POL7890123' TO POLICY-NUMBER.
           MOVE 'CUST1234' TO CUSTOMER-ID.
           MOVE 'WC' TO POLICY-TYPE.
           MOVE 'A' TO POLICY-STATUS.
           MOVE 20230101 TO START-DATE.
           MOVE 20231231 TO END-DATE.
           MOVE 002500.00 TO MONTHLY-PREMIUM.
           MOVE 02000000.00 TO COVERAGE-AMOUNT.
           MOVE 'CONS' TO INDUSTRY-CODE.
           MOVE 'NE1' TO GEO-REGION-CODE.
           MOVE 1.145 TO EMR-VALUE.
           MOVE 3 TO SAFETY-PROG-RATING.
           
           WRITE POLICY-RECORD.
           DISPLAY 'POLICY RECORD 1 WRITTEN: ' POLICY-NUMBER.
           
      * Policy 2 - Manufacturing industry with medium risk
           MOVE 'POL7890124' TO POLICY-NUMBER.
           MOVE 'CUST1235' TO CUSTOMER-ID.
           MOVE 'WC' TO POLICY-TYPE.
           MOVE 'A' TO POLICY-STATUS.
           MOVE 20230201 TO START-DATE.
           MOVE 20240131 TO END-DATE.
           MOVE 001800.00 TO MONTHLY-PREMIUM.
           MOVE 01500000.00 TO COVERAGE-AMOUNT.
           MOVE 'MANU' TO INDUSTRY-CODE.
           MOVE 'MW2' TO GEO-REGION-CODE.
           MOVE 1.050 TO EMR-VALUE.
           MOVE 4 TO SAFETY-PROG-RATING.
           
           WRITE POLICY-RECORD.
           DISPLAY 'POLICY RECORD 2 WRITTEN: ' POLICY-NUMBER.
           
      * Policy 3 - Office work with low risk
           MOVE 'POL7890125' TO POLICY-NUMBER.
           MOVE 'CUST1236' TO CUSTOMER-ID.
           MOVE 'WC' TO POLICY-TYPE.
           MOVE 'A' TO POLICY-STATUS.
           MOVE 20230301 TO START-DATE.
           MOVE 20240229 TO END-DATE.
           MOVE 000850.00 TO MONTHLY-PREMIUM.
           MOVE 00800000.00 TO COVERAGE-AMOUNT.
           MOVE 'OFFI' TO INDUSTRY-CODE.
           MOVE 'SE3' TO GEO-REGION-CODE.
           MOVE 0.925 TO EMR-VALUE.
           MOVE 5 TO SAFETY-PROG-RATING.
           
           WRITE POLICY-RECORD.
           DISPLAY 'POLICY RECORD 3 WRITTEN: ' POLICY-NUMBER.
           
           CLOSE POLICY-FILE.
           DISPLAY 'POLICY FILE CREATED SUCCESSFULLY'.
           STOP RUN.

