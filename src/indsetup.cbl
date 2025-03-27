       IDENTIFICATION DIVISION.
       PROGRAM-ID. INDSETUP.
      *----------------------------------------------------------------*
      * PROGRAM TO CREATE AND POPULATE THE INDUSTRY RISK FILE          *
      *----------------------------------------------------------------*
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INDUSTRY-RISK-FILE ASSIGN TO "data/INDFILE"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS INDUSTRY-CODE
           FILE STATUS IS IND-STATUS.
       
       DATA DIVISION.
       FILE SECTION.
       FD INDUSTRY-RISK-FILE.
       01 INDUSTRY-RISK-RECORD.
          05 INDUSTRY-CODE         PIC X(4).
          05 INDUSTRY-DESCRIPTION  PIC X(30).
          05 INDUSTRY-RISK-FACTOR  PIC 9V999.
          05 FREQUENCY-FACTOR      PIC 9V999.
          05 SEVERITY-FACTOR       PIC 9V999.
          
       WORKING-STORAGE SECTION.
       01 IND-STATUS               PIC X(2).
       
       PROCEDURE DIVISION.
           OPEN OUTPUT INDUSTRY-RISK-FILE.
           
           IF IND-STATUS NOT = '00'
              DISPLAY 'ERROR OPENING INDUSTRY FILE: ' IND-STATUS
              STOP RUN
           END-IF.
           
      * ADD SAMPLE INDUSTRY RECORDS
           
      * Construction Industry - High Risk
           MOVE 'CONS' TO INDUSTRY-CODE.
           MOVE 'CONSTRUCTION                  ' TO INDUSTRY-DESCRIPTION.
           MOVE 1.325 TO INDUSTRY-RISK-FACTOR.
           MOVE 1.450 TO FREQUENCY-FACTOR.
           MOVE 1.680 TO SEVERITY-FACTOR.
           
           WRITE INDUSTRY-RISK-RECORD.
           DISPLAY 'INDUSTRY RECORD 1 WRITTEN: ' INDUSTRY-CODE.
           
      * Manufacturing Industry - Medium-High Risk
           MOVE 'MANU' TO INDUSTRY-CODE.
           MOVE 'MANUFACTURING                 ' TO INDUSTRY-DESCRIPTION.
           MOVE 1.230 TO INDUSTRY-RISK-FACTOR.
           MOVE 1.350 TO FREQUENCY-FACTOR.
           MOVE 1.450 TO SEVERITY-FACTOR.
           
           WRITE INDUSTRY-RISK-RECORD.
           DISPLAY 'INDUSTRY RECORD 2 WRITTEN: ' INDUSTRY-CODE.
           
      * Agriculture Industry - Medium-High Risk
           MOVE 'AGRI' TO INDUSTRY-CODE.
           MOVE 'AGRICULTURE                   ' TO INDUSTRY-DESCRIPTION.
           MOVE 1.265 TO INDUSTRY-RISK-FACTOR.
           MOVE 1.280 TO FREQUENCY-FACTOR.
           MOVE 1.380 TO SEVERITY-FACTOR.
           
           WRITE INDUSTRY-RISK-RECORD.
           DISPLAY 'INDUSTRY RECORD 3 WRITTEN: ' INDUSTRY-CODE.
           
      * Mining Industry - Very High Risk
           MOVE 'MINE' TO INDUSTRY-CODE.
           MOVE 'MINING                        ' TO INDUSTRY-DESCRIPTION.
           MOVE 1.450 TO INDUSTRY-RISK-FACTOR.
           MOVE 1.220 TO FREQUENCY-FACTOR.
           MOVE 1.950 TO SEVERITY-FACTOR.
           
           WRITE INDUSTRY-RISK-RECORD.
           DISPLAY 'INDUSTRY RECORD 4 WRITTEN: ' INDUSTRY-CODE.
           
      * Office Work - Low Risk
           MOVE 'OFFI' TO INDUSTRY-CODE.
           MOVE 'OFFICE WORK                   ' TO INDUSTRY-DESCRIPTION.
           MOVE 1.050 TO INDUSTRY-RISK-FACTOR.
           MOVE 0.920 TO FREQUENCY-FACTOR.
           MOVE 0.880 TO SEVERITY-FACTOR.
           
           WRITE INDUSTRY-RISK-RECORD.
           DISPLAY 'INDUSTRY RECORD 5 WRITTEN: ' INDUSTRY-CODE.
           
           CLOSE INDUSTRY-RISK-FILE.
           DISPLAY 'INDUSTRY RISK FILE CREATED SUCCESSFULLY'.
           STOP RUN.
