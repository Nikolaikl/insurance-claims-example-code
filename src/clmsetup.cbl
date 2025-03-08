       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLMSETUP.
      *----------------------------------------------------------------*
      * PROGRAM TO CREATE AND POPULATE THE CLAIM FILE                  *
      *----------------------------------------------------------------*
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CLAIM-FILE ASSIGN TO "CLMFILE"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS SEQUENTIAL
           RECORD KEY IS CLAIM-ID
           FILE STATUS IS CLM-STATUS.
       
       DATA DIVISION.
       FILE SECTION.
       FD CLAIM-FILE.
       01 CLAIM-RECORD.
          05 CLAIM-ID              PIC X(12).
          05 POLICY-NUMBER         PIC X(10).
          05 CLAIM-DATE            PIC 9(8).
          05 CLAIM-TYPE            PIC X(2).
          05 CLAIM-STATUS          PIC X(1).
          05 CLAIM-AMOUNT          PIC 9(8)V99.
       
       WORKING-STORAGE SECTION.
       01 CLM-STATUS               PIC X(2).
       
       PROCEDURE DIVISION.
           OPEN OUTPUT CLAIM-FILE.
           
           IF CLM-STATUS NOT = '00'
              DISPLAY 'ERROR OPENING CLAIM FILE: ' CLM-STATUS
              STOP RUN
           END-IF.
           
      * ADD SAMPLE CLAIM RECORDS
           
      * Claim 1 - Severe accident for construction policy
           MOVE 'CLM123456789' TO CLAIM-ID.
           MOVE 'POL7890123' TO POLICY-NUMBER.
           MOVE 20230615 TO CLAIM-DATE.
           MOVE 'WA' TO CLAIM-TYPE.
           MOVE 'P' TO CLAIM-STATUS.
           MOVE 00024500.00 TO CLAIM-AMOUNT.
           
           WRITE CLAIM-RECORD.
           DISPLAY 'CLAIM RECORD 1 WRITTEN: ' CLAIM-ID.
           
      * Claim 2 - Minor accident for manufacturing policy  
           MOVE 'CLM123456790' TO CLAIM-ID.
           MOVE 'POL7890124' TO POLICY-NUMBER.
           MOVE 20230720 TO CLAIM-DATE.
           MOVE 'WA' TO CLAIM-TYPE.
           MOVE 'P' TO CLAIM-STATUS.
           MOVE 00012800.00 TO CLAIM-AMOUNT.
           
           WRITE CLAIM-RECORD.
           DISPLAY 'CLAIM RECORD 2 WRITTEN: ' CLAIM-ID.
           
      * Claim 3 - Moderate accident for office policy
           MOVE 'CLM123456791' TO CLAIM-ID.
           MOVE 'POL7890125' TO POLICY-NUMBER.
           MOVE 20230810 TO CLAIM-DATE.
           MOVE 'WA' TO CLAIM-TYPE.
           MOVE 'P' TO CLAIM-STATUS.
           MOVE 00008750.00 TO CLAIM-AMOUNT.
           
           WRITE CLAIM-RECORD.
           DISPLAY 'CLAIM RECORD 3 WRITTEN: ' CLAIM-ID.
           
           CLOSE CLAIM-FILE.
           DISPLAY 'CLAIM FILE CREATED SUCCESSFULLY'.
           STOP RUN.
