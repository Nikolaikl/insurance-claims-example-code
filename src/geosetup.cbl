       IDENTIFICATION DIVISION.
       PROGRAM-ID. GEOSETUP.
      *----------------------------------------------------------------*
      * PROGRAM TO CREATE AND POPULATE THE GEOGRAPHIC FACTOR FILE      *
      *----------------------------------------------------------------*
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT GEO-FACTOR-FILE ASSIGN TO "GEOFILE"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS SEQUENTIAL
           RECORD KEY IS GEO-REGION-CODE
           FILE STATUS IS GEO-STATUS.
       
       DATA DIVISION.
       FILE SECTION.
       FD GEO-FACTOR-FILE.
       01 GEO-FACTOR-RECORD.
          05 GEO-REGION-CODE       PIC X(3).
          05 GEO-REGION-NAME       PIC X(20).
          05 REGIONAL-FACTOR       PIC 9V999.
          05 REGULATORY-FACTOR     PIC 9V999.
          05 WAGE-INDEX            PIC 9V999.
          
       WORKING-STORAGE SECTION.
       01 GEO-STATUS               PIC X(2).
       
       PROCEDURE DIVISION.
           OPEN OUTPUT GEO-FACTOR-FILE.
           
           IF GEO-STATUS NOT = '00'
              DISPLAY 'ERROR OPENING GEO FILE: ' GEO-STATUS
              STOP RUN
           END-IF.
           
      * ADD SAMPLE GEOGRAPHIC REGION RECORDS
           
      * Northeast Region 1 (High cost, strict regulations)
           MOVE 'NE1' TO GEO-REGION-CODE.
           MOVE 'NORTHEAST REGION 1   ' TO GEO-REGION-NAME.
           MOVE 1.280 TO REGIONAL-FACTOR.
           MOVE 1.320 TO REGULATORY-FACTOR.
           MOVE 1.350 TO WAGE-INDEX.
           
           WRITE GEO-FACTOR-RECORD.
           DISPLAY 'GEO RECORD 1 WRITTEN: ' GEO-REGION-CODE.
           
      * Midwest Region 2 (Medium cost, moderate regulations)  
           MOVE 'MW2' TO GEO-REGION-CODE.
           MOVE 'MIDWEST REGION 2' TO GEO-REGION-NAME.
           MOVE 1.080 TO REGIONAL-FACTOR.
           MOVE 1.050 TO REGULATORY-FACTOR.
           MOVE 1.060 TO WAGE-INDEX.
           
           WRITE GEO-FACTOR-RECORD.
           DISPLAY 'GEO RECORD 2 WRITTEN: ' GEO-REGION-CODE.
           
      * Southeast Region 3 (Lower cost, less strict regulations)
           MOVE 'SE3' TO GEO-REGION-CODE.
           MOVE 'SOUTHEAST REGION 3   ' TO GEO-REGION-NAME.
           MOVE 0.950 TO REGIONAL-FACTOR.
           MOVE 0.920 TO REGULATORY-FACTOR.
           MOVE 0.880 TO WAGE-INDEX.
           
           WRITE GEO-FACTOR-RECORD.
           DISPLAY 'GEO RECORD 3 WRITTEN: ' GEO-REGION-CODE.
           
      * West Region 4 (High cost, strict regulations)
           MOVE 'WE4' TO GEO-REGION-CODE.
           MOVE 'WEST REGION 4        ' TO GEO-REGION-NAME.
           MOVE 1.250 TO REGIONAL-FACTOR.
           MOVE 1.280 TO REGULATORY-FACTOR.
           MOVE 1.320 TO WAGE-INDEX.
           
           WRITE GEO-FACTOR-RECORD.
           DISPLAY 'GEO RECORD 4 WRITTEN: ' GEO-REGION-CODE.
           
      * Central Region 5 (Medium-low cost, moderate regulations)
           MOVE 'CE5' TO GEO-REGION-CODE.
           MOVE 'CENTRAL REGION 5     ' TO GEO-REGION-NAME.
           MOVE 1.020 TO REGIONAL-FACTOR.
           MOVE 1.040 TO REGULATORY-FACTOR.
           MOVE 0.980 TO WAGE-INDEX.
           
           WRITE GEO-FACTOR-RECORD.
           DISPLAY 'GEO RECORD 5 WRITTEN: ' GEO-REGION-CODE.
           
           CLOSE GEO-FACTOR-FILE.
           DISPLAY 'GEOGRAPHIC FACTOR FILE CREATED SUCCESSFULLY'.
           STOP RUN.
