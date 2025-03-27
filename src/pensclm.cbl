       IDENTIFICATION DIVISION.
       PROGRAM-ID. PENSCLM.
      *----------------------------------------------------------------*
      * PROGRAM:  PENSCLM                                              *
      * AUTHOR:   Hal Neuntausend                                      *
      * DATE:     2025-03-08                                           *
      * PURPOSE:  CALCULATE PENSION INSURANCE CLAIMS FOR WORK          *
      *           ACCIDENT CASES USING ACTUARIAL MODELS AND            *
      *           CONSIDERING MULTIPLE RISK FACTORS                    *
      *                                                                *
      * PROGRAM STRUCTURE:                                             *
      * 1. FILE DEFINITIONS AND DATA STRUCTURES                        *
      * 2. WORKING STORAGE VARIABLES                                   *
      * 3. MAIN PROCESS FLOW                                           *
      * 4. INITIALIZATION SECTION                                      *
      * 5. CLAIM PROCESSING LOGIC                                      *
      * 6. ACTUARIAL CALCULATIONS                                      *
      * 7. REPORT GENERATION                                           *
      * 8. PROGRAM TERMINATION                                         *
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *  Greetings, young padawan! This here program's been running    *
      *  since the days when punch cards were king and FORTRAN was     *
      *  the new hotness. Treat her with respect, she's got more       *
      *  wisdom in her core than you've got in your whole body!        *
      *----------------------------------------------------------------*

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *----------------------------------------------------------------*
      * FILE DEFINITIONS:                                              *
      * - POLICY-FILE: Contains policy details including coverage      *
      *                amounts, industry codes, and geographic data    *
      * - CLAIM-FILE: Stores claim records with accident details       *
      * - INDUSTRY-RISK-FILE: Contains industry-specific risk factors  *
      * - GEO-FACTOR-FILE: Stores geographic risk adjustment factors   *
      * - CLAIM-REPORT: Output report file for claim calculations      *
      * - INPUT-FILE: Input file containing claim records              *
      *----------------------------------------------------------------*
           SELECT INPUT-FILE ASSIGN TO "data/INPUT.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS WS-INPUT-STATUS.
           SELECT POLICY-FILE ASSIGN TO "data/POLFILE"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS RANDOM
           RECORD KEY IS POLICY-NUMBER OF POLICY-RECORD
           FILE STATUS IS WS-POLICY-STATUS.
           
           SELECT CLAIM-FILE ASSIGN TO "data/CLMFILE"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS RANDOM
           RECORD KEY IS CLAIM-ID OF CLAIM-RECORD
           FILE STATUS IS WS-CLAIM-STATUS.
           
           SELECT INDUSTRY-RISK-FILE ASSIGN TO "data/INDFILE"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS RANDOM
           RECORD KEY IS INDUSTRY-CODE OF INDUSTRY-RISK-RECORD
           FILE STATUS IS WS-INDUSTRY-STATUS.
           
           SELECT GEO-FACTOR-FILE ASSIGN TO "data/GEOFILE"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS RANDOM
           RECORD KEY IS GEO-REGION-CODE OF GEO-FACTOR-RECORD
           FILE STATUS IS WS-GEO-STATUS.
           
           SELECT CLAIM-REPORT ASSIGN TO 'data/OUTPUT.txt'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS WS-REPORT-STATUS.
       
       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 WS-INPUT-RECORD.
          05 CLAIM-ID               PIC X(12).
          05 FILLER                 PIC X VALUE ','.
          05 POLICY-NUMBER          PIC X(10).
          05 FILLER                 PIC X VALUE ','.
          05 CLAIM-DATE             PIC 9(8).
          05 FILLER                 PIC X VALUE ','.
          05 CLAIM-TYPE             PIC X(2).
          05 FILLER                 PIC X VALUE ','.
          05 CLAIM-STATUS           PIC X(1).
          05 FILLER                 PIC X VALUE ','.
          05 CLAIM-AMOUNT           PIC 9(8)V99.
          05 FILLER                 PIC X VALUE ','.
          05 INSURED-AGE            PIC 9(3).
          05 FILLER                 PIC X VALUE ','.
          05 YEARS-EMPLOYED         PIC 9(2).
          05 FILLER                 PIC X VALUE ','.
          05 ANNUAL-SALARY          PIC 9(7)V99.
          05 FILLER                 PIC X VALUE ','.
          05 OCCUPATION-CODE        PIC X(4).
          05 FILLER                 PIC X VALUE ','.
          05 JOB-RISK-LEVEL         PIC 9(1).
          05 FILLER                 PIC X VALUE ','.
          05 DISABILITY-PCT         PIC 9(3).
          05 FILLER                 PIC X VALUE ','.
          05 ACCIDENT-SEVERITY      PIC X(1).
          05 FILLER                 PIC X VALUE ','.
          05 DIRECT-COSTS           PIC 9(7)V99.
          05 FILLER                 PIC X VALUE ','.
          05 INDUSTRY-CODE          PIC X(4).
          05 FILLER                 PIC X VALUE ','.
          05 GEO-REGION-CODE        PIC X(3).

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
       
       FD CLAIM-FILE.
       01 CLAIM-RECORD.
          05 CLAIM-ID             PIC X(12).
          05 FILLER               PIC X(188).
       
       FD INDUSTRY-RISK-FILE.
       01 INDUSTRY-RISK-RECORD.
          05 INDUSTRY-CODE         PIC X(4).
          05 INDUSTRY-DESCRIPTION  PIC X(30).
          05 INDUSTRY-RISK-FACTOR  PIC 9V999.
          05 FREQUENCY-FACTOR      PIC 9V999.
          05 SEVERITY-FACTOR       PIC 9V999.
          
       FD GEO-FACTOR-FILE.
       01 GEO-FACTOR-RECORD.
          05 GEO-REGION-CODE       PIC X(3).
          05 GEO-REGION-NAME       PIC X(20).
          05 REGIONAL-FACTOR       PIC 9V999.
          05 REGULATORY-FACTOR     PIC 9V999.
          05 WAGE-INDEX            PIC 9V999.
       
       FD CLAIM-REPORT.
       01 REPORT-LINE.
          05 FILLER                PIC X(80).
          05 FILLER                PIC X(52).
       
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      * WORKING STORAGE VARIABLES:                                     *
      * - WS-FILE-STATUS: Tracks file operation status codes           *
      * - WS-SWITCHES: Program control flags                          *
      * - WS-INSURED-DETAILS: Insured person information              *
      * - WS-ACCIDENT-DETAILS: Accident-specific data                 *
      * - WS-ACTUARIAL-FACTORS: Various adjustment factors            *
      * - WS-CALCULATION-RESULTS: Calculated pension amounts          *
      * - WS-ACTUARIAL-CONSTANTS: Fixed values for calculations       *
      * - Report line definitions for output formatting               *
      *----------------------------------------------------------------*
       01 INPUT-RECORD.
          05 CLAIM-ID               PIC X(12).
          05 FILLER                 PIC X VALUE ','.
          05 POLICY-NUMBER          PIC X(10).
          05 FILLER                 PIC X VALUE ','.
          05 CLAIM-DATE             PIC 9(8).
          05 FILLER                 PIC X VALUE ','.
          05 CLAIM-TYPE             PIC X(2).
          05 FILLER                 PIC X VALUE ','.
          05 CLAIM-STATUS           PIC X(1).
          05 FILLER                 PIC X VALUE ','.
          05 CLAIM-AMOUNT           PIC 9(8)V99.
          05 FILLER                 PIC X VALUE ','.
          05 INSURED-AGE            PIC 9(3).
          05 FILLER                 PIC X VALUE ','.
          05 YEARS-EMPLOYED         PIC 9(2).
          05 FILLER                 PIC X VALUE ','.
          05 ANNUAL-SALARY          PIC 9(7)V99.
          05 FILLER                 PIC X VALUE ','.
          05 OCCUPATION-CODE        PIC X(4).
          05 FILLER                 PIC X VALUE ','.
          05 JOB-RISK-LEVEL         PIC 9(1).
          05 FILLER                 PIC X VALUE ','.
          05 DISABILITY-PCT         PIC 9(3).
          05 FILLER                 PIC X VALUE ','.
          05 ACCIDENT-SEVERITY      PIC X(1).
          05 FILLER                 PIC X VALUE ','.
          05 DIRECT-COSTS           PIC 9(7)V99.
          05 FILLER                 PIC X VALUE ','.
          05 INDUSTRY-CODE          PIC X(4).
          05 FILLER                 PIC X VALUE ','.
          05 GEO-REGION-CODE        PIC X(3).
       01 WS-FILE-STATUS.
          05 WS-POLICY-STATUS      PIC X(2).
          05 WS-CLAIM-STATUS       PIC X(2).
          05 WS-INDUSTRY-STATUS    PIC X(2).
          05 WS-GEO-STATUS         PIC X(2).
          05 WS-REPORT-STATUS      PIC X(2).
          05 WS-INPUT-STATUS       PIC X(2).
          
       01 WS-SWITCHES.
          05 END-OF-FILE-SW        PIC X(1) VALUE 'N'.
             88 END-OF-FILE        VALUE 'Y'.
          
       01 WS-INSURED-DETAILS.
          05 WS-INSURED-AGE        PIC 9(3).
          05 WS-YEARS-EMPLOYED     PIC 9(2).
          05 WS-ANNUAL-SALARY      PIC 9(7)V99.
          05 WS-OCCUPATION-CODE    PIC X(4).
          05 WS-JOB-RISK-LEVEL     PIC 9(1).
          05 WS-CLAIM-ID           PIC X(12).
          05 WS-POLICY-NUMBER      PIC X(10).
          05 WS-ACC-DATE           PIC 9(8).
          05 WS-CLAIM-TYPE         PIC X(2).
          05 WS-CLAIM-STAT         PIC X(1).
          05 WS-CLAIM-AMT          PIC 9(8)V99.
          05 WS-DISABILITY         PIC 9(3).
          05 WS-SEVERITY           PIC X(1).
          05 WS-DIRECT-COST        PIC 9(7)V99.
          05 WS-INDUSTRY           PIC X(4).
          05 WS-GEO-REGION         PIC X(3).
             88 LOW-RISK           VALUE 1.
             88 MEDIUM-RISK        VALUE 2.
             88 HIGH-RISK          VALUE 3.
             88 VERY-HIGH-RISK     VALUE 4.
          
       01 WS-ACCIDENT-DETAILS.
          05 WS-ACC-DATE           PIC 9(8).
          05 WS-DISABILITY         PIC 9(3).
          05 WS-SEVERITY           PIC X(1).
             88 MINOR              VALUE 'M'.
             88 MODERATE           VALUE 'O'.
             88 SEVERE             VALUE 'S'.
             88 CRITICAL           VALUE 'C'.
          05 WS-DIRECT-COSTS       PIC 9(7)V99.
          05 WS-INDIRECT-COSTS     PIC 9(7)V99.
          05 WS-TOTAL-COSTS        PIC 9(8)V99.
             
       01 WS-ACTUARIAL-FACTORS.
          05 WS-BASE-PCT           PIC 9(3)V99.
          05 WS-AGE-FACTOR         PIC 9(1)V999.
          05 WS-SERVICE-FACTOR     PIC 9(1)V999.
          05 WS-SEVERITY-FACTOR    PIC 9(1)V999.
          05 WS-INDUSTRY-FACTOR    PIC 9(1)V999.
          05 WS-JOB-RISK-FACTOR    PIC 9(1)V999.
          05 WS-EMR-FACTOR         PIC 9(1)V999.
          05 WS-SAFETY-FACTOR      PIC 9(1)V999.
          05 WS-GEO-FACTOR         PIC 9(1)V999.
          05 WS-REG-FACTOR         PIC 9(1)V999.
          05 WS-MARKET-COMP-FACTOR PIC 9(1)V999.
          05 WS-FREQ-TREND-FACTOR  PIC 9(1)V999.
          05 WS-SEV-TREND-FACTOR   PIC 9(1)V999.
          05 WS-FINAL-FACTOR       PIC 9(1)V999.
          
       01 WS-CALCULATION-RESULTS.
          05 WS-BASE-PENSION       PIC 9(7)V99.
          05 WS-INDUSTRY-ADJ       PIC 9(7)V99.
          05 WS-GEO-ADJ            PIC 9(7)V99.
          05 WS-TREND-ADJ          PIC 9(7)V99.
          05 WS-TOTAL-PENSION      PIC 9(7)V99.
          05 WS-MONTHLY-PENSION    PIC 9(7)V99.
          05 WS-PRESENT-VALUE      PIC 9(8)V99.
          
       01 WS-ACTUARIAL-CONSTANTS.
          05 WS-MIN-PENSION-PCT    PIC 9(2)V99 VALUE 30.00.
          05 WS-MAX-PENSION-PCT    PIC 9(2)V99 VALUE 80.00.
          05 WS-MONTHS-IN-YEAR     PIC 9(2)    VALUE 12.
          05 WS-FREQ-DECLINE-RATE  PIC 9V9999  VALUE 0.0510.
          05 WS-SEV-INCREASE-RATE  PIC 9V9999  VALUE 0.0440.
          05 WS-DISCOUNT-RATE      PIC 9V9999  VALUE 0.0350.
          05 WS-LIFE-EXPECTANCY    PIC 99V9    VALUE 20.5.
          05 WS-INDIRECT-COST-MULT PIC 9V99    VALUE 1.50.
          
       01 WS-HEADING-1.
          05 FILLER                PIC X(50) VALUE 
             'PENSION INSURANCE CLAIM CALCULATION REPORT'.
          
       01 WS-HEADING-2.
          05 FILLER                PIC X(12) VALUE 'CLAIM ID: '.
          05 WS-H-CLAIM-ID         PIC X(12).
          05 FILLER                PIC X(13) VALUE ' POLICY NO.: '.
          05 WS-H-POLICY-NO        PIC X(10).
          05 FILLER                PIC X(12) VALUE ' CLAIM DATE:'.
          05 WS-H-CLAIM-DATE       PIC X(10).
          
       01 WS-DETAIL-1.
          05 FILLER                PIC X(24) VALUE 
             'INSURED PERSON DETAILS: '.
          05 FILLER                PIC X(5)  VALUE 'AGE: '.
          05 WS-D-AGE              PIC ZZ9.
          05 FILLER                PIC X(17) VALUE ' YEARS EMPLOYED: '.
          05 WS-D-YEARS            PIC Z9.
          05 FILLER                PIC X(16) VALUE ' ANNUAL SALARY: '.
          05 WS-D-SALARY           PIC ZZ,ZZZ,ZZ9.99.
          
       01 WS-DETAIL-2.
          05 FILLER                PIC X(24) VALUE 
             'INDUSTRY & RISK DATA:  '.
          05 FILLER                PIC X(13) VALUE 'INDUSTRY: '.
          05 WS-D-INDUSTRY         PIC X(30).
          05 FILLER                PIC X(10) VALUE ' EMR: '.
          05 WS-D-EMR              PIC Z.999.
          
       01 WS-DETAIL-3.
          05 FILLER                PIC X(24) VALUE 
             'ACCIDENT DETAILS:      '.
          05 FILLER                PIC X(10) VALUE 'SEVERITY: '.
          05 WS-D-SEVERITY         PIC X(8).
          05 FILLER                PIC X(15) VALUE ' DISABILITY %: '.
          05 WS-D-DISABILITY       PIC ZZ9.
          05 FILLER                PIC X(15) VALUE ' DIRECT COSTS: '.
          05 WS-D-DIRECT-COSTS     PIC Z,ZZZ,ZZ9.99.
          
       01 WS-FACTORS-LINE-1.
          05 FILLER                PIC X(24) VALUE 
             'ACTUARIAL FACTORS:     '.
          05 FILLER                PIC X(13) VALUE 'AGE FACTOR: '.
          05 WS-F-AGE-FACTOR       PIC Z.999.
          05 FILLER                PIC X(20) VALUE ' SEVERITY FACTOR: '.
          05 WS-F-SEVERITY-FACTOR  PIC Z.999.
          05 FILLER                PIC X(15) VALUE ' IND. FACTOR: '.
          05 WS-F-INDUSTRY-FACTOR  PIC Z.999.
      
       01 WS-FACTORS-LINE-2.
          05 FILLER                PIC X(24) VALUE 
             '                       '.
          05 FILLER                PIC X(13) VALUE 'GEO FACTOR: '.
          05 WS-F-GEO-FACTOR       PIC Z.999.
          05 FILLER                PIC X(16) VALUE ' TREND FACTOR: '.
          05 WS-F-TREND-FACTOR     PIC Z.999.
          05 FILLER                PIC X(17) VALUE ' FINAL FACTOR: '.
          05 WS-F-FINAL-FACTOR     PIC Z.999.

       01 WS-RESULT-LINE-1.
           05 FILLER                PIC X(24) VALUE 
              'PENSION CALCULATION:   '.
           05 FILLER                PIC X(14) VALUE 'BASE PENSION: '.
           05 WS-R-BASE-PENSION     PIC ZZ,ZZZ,ZZ9.99.

          
       01 WS-RESULT-LINE-2.
          05 FILLER                PIC X(24) VALUE 
             '                       '.
          05 FILLER                PIC X(16) VALUE 'INDUSTRY ADJ: '.
          05 WS-R-INDUSTRY-ADJ     PIC ZZ,ZZZ,ZZ9.99.
          05 FILLER                PIC X(15) VALUE ' GEO/REG ADJ: '.
          05 WS-R-GEO-ADJ          PIC ZZ,ZZZ,ZZ9.99.
          
       01 WS-FINAL-LINE-1.
          05 FILLER                PIC X(24) VALUE 
             'FINAL PENSION AMOUNT:  '.
          05 FILLER                PIC X(19) VALUE 'ANNUAL PENSION: '.
          05 WS-F-ANNUAL-PENSION   PIC ZZZ,ZZZ,ZZ9.99.
          05 FILLER                PIC X(20) VALUE ' MONTHLY PENSION: '.
          05 WS-F-MONTHLY-PENSION  PIC ZZZ,ZZZ,ZZ9.99.
          
       01 WS-FINAL-LINE-2.
          05 FILLER                PIC X(24) VALUE 
             '                       '.
          05 FILLER                PIC X(19) VALUE 'PRESENT VALUE: '.
          05 WS-F-PRESENT-VALUE    PIC Z,ZZZ,ZZZ,ZZ9.99.
          
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
      * MAIN PROGRAM FLOW:                                             *
      * 1. Initialize program and open files                          *
      * 2. Process claim data and perform calculations                *
      * 3. Generate detailed report                                   *
      * 4. Clean up and terminate program                             *
      *----------------------------------------------------------------*
       000-MAIN-PROCESS.
           PERFORM 100-INITIALIZATION.
           PERFORM 200-PROCESS-CLAIM
           PERFORM 300-GENERATE-REPORT
           PERFORM 900-TERMINATION
           .
           
       100-INITIALIZATION.
      *----------------------------------------------------------------*
      * INITIALIZATION SECTION:                                        *
      * 1. Clear working storage variables                            *
      * 2. Open all required files                                    *
      * 3. Verify successful file opens                               *
      * 4. Exit program if any file open fails                        *
      *----------------------------------------------------------------*
      *  Back in my day, we didn't have these fancy "error handlers"   *
      *  We just let the program crash and burn! But I suppose you     *
      *  young whippersnappers need your hand-holding.                 *
      *----------------------------------------------------------------*
           INITIALIZE WS-INSURED-DETAILS
                      WS-ACCIDENT-DETAILS
                      WS-ACTUARIAL-FACTORS
                      WS-CALCULATION-RESULTS.
           
           OPEN INPUT POLICY-FILE
                      INDUSTRY-RISK-FILE
                      GEO-FACTOR-FILE
                      INPUT-FILE
                OUTPUT CLAIM-REPORT.
           
           *> Initialize output file with header
           MOVE 'PENSION CLAIM CALCULATION REPORT' TO REPORT-LINE
           WRITE REPORT-LINE AFTER ADVANCING PAGE
           MOVE SPACES TO REPORT-LINE
           WRITE REPORT-LINE AFTER ADVANCING 1 LINE
                
      * Try to open CLAIM-FILE as input first
           OPEN INPUT CLAIM-FILE.
           IF WS-CLAIM-STATUS NOT = '00'
              DISPLAY 'CLAIM FILE NOT FOUND, CREATING NEW FILE'
              OPEN OUTPUT CLAIM-FILE
              CLOSE CLAIM-FILE
              OPEN INPUT CLAIM-FILE
           END-IF.
                
           IF WS-POLICY-STATUS NOT = '00'
              DISPLAY 'ERROR OPENING POLICY FILE: ' WS-POLICY-STATUS
              PERFORM 900-TERMINATION
           END-IF.
           
           IF WS-CLAIM-STATUS NOT = '00'
              DISPLAY 'ERROR OPENING CLAIM FILE: ' WS-CLAIM-STATUS
              PERFORM 900-TERMINATION
           END-IF.
           
           IF WS-INDUSTRY-STATUS NOT = '00'
              DISPLAY 'ERROR OPENING INDUSTRY FILE: ' WS-INDUSTRY-STATUS
              PERFORM 900-TERMINATION
           END-IF.
           
           IF WS-GEO-STATUS NOT = '00'
              DISPLAY 'ERROR OPENING GEO FILE: ' WS-GEO-STATUS
              PERFORM 900-TERMINATION
           END-IF.
           
           IF WS-REPORT-STATUS NOT = '00'
              DISPLAY 'ERROR OPENING REPORT FILE: ' WS-REPORT-STATUS
              PERFORM 900-TERMINATION
           END-IF.
           
       200-PROCESS-CLAIM.
      *----------------------------------------------------------------*
      * CLAIM PROCESSING SECTION:                                      *
      * 1. Read each claim from INPUT.txt                             *
      * 2. Process each claim through calculations                    *
      * 3. Generate report for each claim                             *
      *----------------------------------------------------------------*
           PERFORM UNTIL END-OF-FILE
               READ INPUT-FILE
                   AT END
                       SET END-OF-FILE TO TRUE
                   NOT AT END
                       PERFORM 210-PROCESS-CLAIM-RECORD
               END-READ
           END-PERFORM
           DISPLAY 'PROCESSED ALL CLAIMS IN INPUT FILE'.
           
       210-PROCESS-CLAIM-RECORD.
      *----------------------------------------------------------------*
      * PROCESS INDIVIDUAL CLAIM RECORD:                               *
      * 1. Parse input record                                          *
      * 2. Load insured and accident details                          *
      * 3. Calculate direct and indirect costs                        *
      * 4. Load industry and geographic risk factors                  *
      * 5. Perform actuarial calculations                             *
      *----------------------------------------------------------------*
           UNSTRING WS-INPUT-RECORD DELIMITED BY ','
               INTO WS-CLAIM-ID
                    WS-POLICY-NUMBER
                    WS-ACC-DATE OF WS-INSURED-DETAILS
                    WS-CLAIM-TYPE
                    WS-CLAIM-STAT
                    WS-CLAIM-AMT
                    WS-INSURED-AGE
                    WS-YEARS-EMPLOYED
                    WS-ANNUAL-SALARY
                    WS-OCCUPATION-CODE
                    WS-JOB-RISK-LEVEL
                    WS-DISABILITY OF WS-INSURED-DETAILS
                    WS-SEVERITY OF WS-INSURED-DETAILS
                    WS-DIRECT-COST
                    WS-INDUSTRY
                    WS-GEO-REGION
           END-UNSTRING.
           
           PERFORM 220-LOAD-POLICY-DATA.
           PERFORM 230-LOAD-INDUSTRY-DATA.
           PERFORM 240-LOAD-GEO-DATA.
           PERFORM 250-CALCULATE-COSTS.
           PERFORM 260-PERFORM-CALCULATIONS.
           PERFORM 300-GENERATE-REPORT.
      *  This here's where the magic happens, kiddo! Back when I       *
      *  wrote this, we didn't have no fancy "machine learning" or     *
      *  "AI". Just good ol' fashioned math and a slide rule!          *
      *  These calculations have stood the test of time, like a        *
      *  fine whiskey aging in a oak barrel.                          *
      *----------------------------------------------------------------*
           
           
           
           
       210-CALCULATE-BASE-FACTORS.
      *----------------------------------------------------------------*
      * BASE FACTOR CALCULATIONS:                                      *
      * 1. Age Factor: Higher for older workers                       *
      * 2. Service Factor: Higher for longer employment               *
      * 3. Severity Factor: Higher for more severe accidents          *
      * 4. Job Risk Factor: Higher for riskier occupations            *
      * 5. Base Percentage: Calculated from disability percentage     *
      *    using formula:                                             *
      *    base_pct = min_pct + (disability_pct * 0.5) +              *
      *               ((disability_pct^2) / 200)                      *
      *    Capped at maximum percentage                               *
      *----------------------------------------------------------------*
      *  Ah, the base factors! These babies were cooked up during      *
      *  the Nixon administration, and they still hold water today!   *
      *  Like a fine stew, the longer they simmer, the better they    *
      *  taste. Don't go messing with these formulas unless you       *
      *  want to summon the ghost of COBOL past!                      *
      *----------------------------------------------------------------*
           EVALUATE TRUE
               WHEN WS-INSURED-AGE < 30
                   MOVE 1.000 TO WS-AGE-FACTOR
               WHEN WS-INSURED-AGE < 45
                   MOVE 1.250 TO WS-AGE-FACTOR
               WHEN WS-INSURED-AGE < 55
                   MOVE 1.500 TO WS-AGE-FACTOR
               WHEN OTHER
                   MOVE 1.750 TO WS-AGE-FACTOR
           END-EVALUATE.
           
           EVALUATE TRUE
               WHEN WS-YEARS-EMPLOYED < 5
                   MOVE 1.000 TO WS-SERVICE-FACTOR
               WHEN WS-YEARS-EMPLOYED < 10
                   MOVE 1.150 TO WS-SERVICE-FACTOR
               WHEN WS-YEARS-EMPLOYED < 20
                   MOVE 1.300 TO WS-SERVICE-FACTOR
               WHEN OTHER
                   MOVE 1.500 TO WS-SERVICE-FACTOR
           END-EVALUATE.
           
           EVALUATE TRUE
               WHEN MINOR
                   MOVE 1.100 TO WS-SEVERITY-FACTOR
               WHEN MODERATE
                   MOVE 1.250 TO WS-SEVERITY-FACTOR
               WHEN SEVERE
                   MOVE 1.500 TO WS-SEVERITY-FACTOR
               WHEN CRITICAL
                   MOVE 1.750 TO WS-SEVERITY-FACTOR
           END-EVALUATE.
           
      * Calculate job risk factor based on job risk level
           EVALUATE WS-JOB-RISK-LEVEL
               WHEN 1
                   MOVE 1.000 TO WS-JOB-RISK-FACTOR
               WHEN 2
                   MOVE 1.200 TO WS-JOB-RISK-FACTOR
               WHEN 3
                   MOVE 1.350 TO WS-JOB-RISK-FACTOR
               WHEN 4
                   MOVE 1.500 TO WS-JOB-RISK-FACTOR
               WHEN OTHER
                   MOVE 1.000 TO WS-JOB-RISK-FACTOR
           END-EVALUATE.
           
      * Calculate base percentage based on disability percentage
      * using actuarial formula based on GLM principles
           COMPUTE WS-BASE-PCT = WS-MIN-PENSION-PCT 
                    + (WS-DISABILITY OF WS-INSURED-DETAILS * 0.5)
                    + (WS-DISABILITY OF WS-INSURED-DETAILS
                     * (WS-DISABILITY OF WS-INSURED-DETAILS / 200))
           END-COMPUTE
           
      * Ensure the base percentage doesn't exceed maximum
           IF WS-BASE-PCT > WS-MAX-PENSION-PCT
               MOVE WS-MAX-PENSION-PCT TO WS-BASE-PCT
           END-IF.
           
       220-LOAD-POLICY-DATA.
      *----------------------------------------------------------------*
      * LOAD POLICY DATA SECTION:                                      *
      * 1. Read policy record using policy number                     *
      * 2. Store relevant policy data in working storage              *
      *----------------------------------------------------------------*
           MOVE WS-POLICY-NUMBER TO POLICY-NUMBER OF POLICY-RECORD.
           READ POLICY-FILE
               INVALID KEY
                   DISPLAY 'POLICY NOT FOUND: ' WS-POLICY-NUMBER
                   PERFORM 900-TERMINATION
           END-READ.
           
           MOVE INDUSTRY-CODE OF POLICY-RECORD TO WS-INDUSTRY.
           MOVE GEO-REGION-CODE OF POLICY-RECORD TO WS-GEO-REGION.
           MOVE EMR-VALUE OF POLICY-RECORD TO WS-EMR-FACTOR.
           MOVE SAFETY-PROG-RATING OF POLICY-RECORD TO WS-SAFETY-FACTOR.

       230-LOAD-INDUSTRY-DATA.
      *----------------------------------------------------------------*
      * LOAD INDUSTRY DATA SECTION:                                    *
      * 1. Read industry risk record using industry code              *
      * 2. Store industry risk factors in working storage             *
      *----------------------------------------------------------------*
           MOVE WS-INDUSTRY TO INDUSTRY-CODE OF INDUSTRY-RISK-RECORD.
           READ INDUSTRY-RISK-FILE
               INVALID KEY
                   DISPLAY 'INDUSTRY NOT FOUND: ' WS-INDUSTRY
                   PERFORM 900-TERMINATION
           END-READ.
           
           MOVE INDUSTRY-RISK-FACTOR OF INDUSTRY-RISK-RECORD 
               TO WS-INDUSTRY-FACTOR.
           MOVE FREQUENCY-FACTOR OF INDUSTRY-RISK-RECORD 
               TO WS-FREQ-TREND-FACTOR.
           MOVE SEVERITY-FACTOR OF INDUSTRY-RISK-RECORD 
               TO WS-SEV-TREND-FACTOR.

       240-LOAD-GEO-DATA.
      *----------------------------------------------------------------*
      * LOAD GEOGRAPHIC DATA SECTION:                                  *
      * 1. Read geographic factor record using region code            *
      * 2. Store geographic factors in working storage                *
      *----------------------------------------------------------------*
           MOVE WS-GEO-REGION TO GEO-REGION-CODE OF GEO-FACTOR-RECORD.
           READ GEO-FACTOR-FILE
               INVALID KEY
                   DISPLAY 'REGION NOT FOUND: ' WS-GEO-REGION
                   DISPLAY 'SEARCHED IN FILE: data/GEOFILE'
                   DISPLAY 'CURRENT GEO REGIONS: NE1 MW2 SE3 WE4 CE5'
                   DISPLAY 'FILE STATUS: ' WS-GEO-STATUS
                   PERFORM 900-TERMINATION
           END-READ.
           
           MOVE REGIONAL-FACTOR OF GEO-FACTOR-RECORD TO WS-GEO-FACTOR.
           MOVE REGULATORY-FACTOR OF GEO-FACTOR-RECORD TO WS-REG-FACTOR.
           MOVE WAGE-INDEX OF GEO-FACTOR-RECORD TO WS-MARKET-COMP-FACTOR.
           PERFORM 250-CALCULATE-COSTS.

       250-CALCULATE-COSTS.
      *----------------------------------------------------------------*
      * COST CALCULATION SECTION:                                      *
      * 1. Calculate indirect costs (1.5x direct costs)               *
      * 2. Calculate total costs                                      *
      *----------------------------------------------------------------*
           COMPUTE WS-INDIRECT-COSTS = WS-DIRECT-COSTS * 1.5.
           COMPUTE WS-TOTAL-COSTS = WS-DIRECT-COSTS + WS-INDIRECT-COSTS.

       260-PERFORM-CALCULATIONS.
      *----------------------------------------------------------------*
      * MAIN CALCULATION SECTION:                                      *
      * 1. Calculate base factors                                     *
      * 2. Calculate trend factors                                    *
      * 3. Calculate final factor                                     *
      * 4. Calculate pension amounts                                  *
      * 5. Calculate present value                                    *
      *----------------------------------------------------------------*
           PERFORM 210-CALCULATE-BASE-FACTORS.
           PERFORM 220-CALCULATE-TREND-FACTORS.
           PERFORM 230-CALCULATE-FINAL-FACTOR.
           PERFORM 240-CALCULATE-PENSION.
           PERFORM 250-CALCULATE-PRESENT-VALUE.
       220-CALCULATE-TREND-FACTORS.
      *----------------------------------------------------------------*
      * TREND FACTOR CALCULATIONS:                                     *
      * 1. Frequency Trend: Accounts for declining claim frequency    *
      *    (5.1% annual decline)                                      *
      * 2. Severity Trend: Accounts for increasing claim severity     *
      *    (4.4% annual increase)                                     *
      *----------------------------------------------------------------*
      * Factor in the declining frequency trend (5.1% annually)
           COMPUTE WS-FREQ-TREND-FACTOR = 1.000 - WS-FREQ-DECLINE-RATE.
           
      * Factor in the increasing severity trend (4.4% annually)
           COMPUTE WS-SEV-TREND-FACTOR = 1.000 + WS-SEV-INCREASE-RATE.
           
       230-CALCULATE-FINAL-FACTOR.
      *----------------------------------------------------------------*
      * FINAL FACTOR CALCULATION:                                      *
      * Weighted combination of all factors:                          *
      * - Age: 15%                                                    *
      * - Service: 10%                                                *
      * - Severity: 25%                                               *
      * - Industry: 15%                                               *
      * - Job Risk: 10%                                               *
      * - EMR: 5%                                                     *
      * - Safety: 3%                                                  *
      * - Geo: 7%                                                     *
      * - Regulatory: 5%                                              *
      * - Market: 5%                                                  *
      * Adjusted by frequency and severity trends                     *
      *----------------------------------------------------------------*
      * Calculate the final factor as a product of all relevant factors
      * Weighted appropriately based on actuarial principles
           COMPUTE WS-FINAL-FACTOR = (WS-AGE-FACTOR * 0.15) +
                                   (WS-SERVICE-FACTOR * 0.10) +
                                   (WS-SEVERITY-FACTOR * 0.25) +
                                   (WS-INDUSTRY-FACTOR * 0.15) +
                                   (WS-JOB-RISK-FACTOR * 0.10) +
                                   (WS-EMR-FACTOR * 0.05) +
                                   (WS-SAFETY-FACTOR * 0.03) +
                                   (WS-GEO-FACTOR * 0.07) +
                                   (WS-REG-FACTOR * 0.05) +
                                   (WS-MARKET-COMP-FACTOR * 0.05).
           
      * Apply frequency and severity trends to the final factor
           COMPUTE WS-FINAL-FACTOR = WS-FINAL-FACTOR * 
                                   WS-FREQ-TREND-FACTOR *
                                   WS-SEV-TREND-FACTOR.
           
       240-CALCULATE-PENSION.
      *----------------------------------------------------------------*
      * PENSION CALCULATION:                                           *
      * 1. Base Pension: Percentage of annual salary                  *
      * 2. Industry Adjustment: Based on industry and job risk        *
      * 3. Geo Adjustment: Based on geographic and regulatory factors *
      * 4. Trend Adjustment: Accounts for frequency and severity trends*
      * 5. Total Annual Pension: Sum of all components                *
      * 6. Monthly Pension: Annual amount divided by 12               *
      *----------------------------------------------------------------*
      * Calculate base pension (percentage of annual salary)
           COMPUTE WS-BASE-PENSION = (WS-BASE-PCT / 100) * 
                                   WS-ANNUAL-SALARY.
                                    
      * Calculate industry adjustment
           COMPUTE WS-INDUSTRY-ADJ = WS-BASE-PENSION * 
                                   (WS-INDUSTRY-FACTOR - 1) *
                                   (WS-JOB-RISK-FACTOR - 1) *
                                   WS-EMR-FACTOR *
                                   WS-SAFETY-FACTOR.
                                       
      * Calculate geographic and regulatory adjustment
           COMPUTE WS-GEO-ADJ = WS-BASE-PENSION * 
                              (WS-GEO-FACTOR - 1) *
                              (WS-REG-FACTOR - 1) *
                              WS-MARKET-COMP-FACTOR.
                                       
      * Calculate trend adjustment (from both frequency and severity)
           COMPUTE WS-TREND-ADJ = WS-BASE-PENSION * 
                                (WS-SEV-TREND-FACTOR - 1) *
                                (1.00 - (WS-FREQ-TREND-FACTOR - 1)).
                                     
      * Calculate total annual pension
           COMPUTE WS-TOTAL-PENSION = WS-BASE-PENSION + 
                                    WS-INDUSTRY-ADJ +
                                    WS-GEO-ADJ +
                                    WS-TREND-ADJ.
                                     
      * Calculate monthly pension
           COMPUTE WS-MONTHLY-PENSION = WS-TOTAL-PENSION / 
                                      WS-MONTHS-IN-YEAR.
           
       250-CALCULATE-PRESENT-VALUE.
      *----------------------------------------------------------------*
      * PRESENT VALUE CALCULATION:                                     *
      * Uses actuarial present value formula:                         *
      * PV = Annual Payment * [(1 - (1 + r)^-n) / r]                  *
      * Where:                                                        *
      * - r = discount rate (3.5%)                                    *
      * - n = life expectancy (20.5 years)                            *
      *----------------------------------------------------------------*
      * Using actuarial present value calculation
      * PV = Annual Payment * [(1 - (1 + r)^-n) / r]
      * Where r = discount rate, n = expected payment years
           
           COMPUTE WS-PRESENT-VALUE = WS-TOTAL-PENSION *
                                    (1 - (1 / (1 + WS-DISCOUNT-RATE) ** 
                                          WS-LIFE-EXPECTANCY)) /
                                    WS-DISCOUNT-RATE.
           
       300-GENERATE-REPORT.
      *----------------------------------------------------------------*
      * REPORT GENERATION:                                             *
      * 1. Format all calculated values                               *
      * 2. Generate report sections:                                  *
      *    - Header with claim details                                *
      *    - Insured person details                                   *
      *    - Industry and risk data                                   *
      *    - Accident details                                         *
      *    - Actuarial factors                                        *
      *    - Pension calculation results                              *
      *    - Final pension amounts                                    *
      *----------------------------------------------------------------*
           MOVE 'CLM123456789'      TO WS-H-CLAIM-ID.
           MOVE 'POL7890123'        TO WS-H-POLICY-NO.
           MOVE '2023-06-15'        TO WS-H-CLAIM-DATE.
           
           MOVE WS-INSURED-AGE      TO WS-D-AGE.
           MOVE WS-YEARS-EMPLOYED   TO WS-D-YEARS.
           MOVE WS-ANNUAL-SALARY    TO WS-D-SALARY.
           
           MOVE 'CONSTRUCTION'      TO WS-D-INDUSTRY.
           MOVE WS-EMR-FACTOR       TO WS-D-EMR.
           
           EVALUATE WS-SEVERITY OF WS-INSURED-DETAILS
               WHEN 'M'
                   MOVE 'MINOR'     TO WS-D-SEVERITY
               WHEN 'O'
                   MOVE 'MODERATE'  TO WS-D-SEVERITY
               WHEN 'S'
                   MOVE 'SEVERE'    TO WS-D-SEVERITY
               WHEN 'C'
                   MOVE 'CRITICAL'  TO WS-D-SEVERITY
           END-EVALUATE.
           
           MOVE WS-DISABILITY OF WS-INSURED-DETAILS TO WS-D-DISABILITY.
           MOVE WS-DIRECT-COSTS     TO WS-D-DIRECT-COSTS.
           
           MOVE WS-AGE-FACTOR       TO WS-F-AGE-FACTOR.
           MOVE WS-SEVERITY-FACTOR  TO WS-F-SEVERITY-FACTOR.
           MOVE WS-INDUSTRY-FACTOR  TO WS-F-INDUSTRY-FACTOR.
           MOVE WS-GEO-FACTOR       TO WS-F-GEO-FACTOR.
           COMPUTE WS-F-TREND-FACTOR = WS-FREQ-TREND-FACTOR * 
                                      WS-SEV-TREND-FACTOR.
           MOVE WS-FINAL-FACTOR     TO WS-F-FINAL-FACTOR.
           
           MOVE WS-BASE-PENSION     TO WS-R-BASE-PENSION.
           MOVE WS-INDUSTRY-ADJ     TO WS-R-INDUSTRY-ADJ.
           MOVE WS-GEO-ADJ          TO WS-R-GEO-ADJ.
           
           MOVE WS-TOTAL-PENSION    TO WS-F-ANNUAL-PENSION.
           MOVE WS-MONTHLY-PENSION  TO WS-F-MONTHLY-PENSION.
           MOVE WS-PRESENT-VALUE    TO WS-F-PRESENT-VALUE.
           
      * Write report lines
           MOVE WS-HEADING-1        TO REPORT-LINE.
           WRITE REPORT-LINE AFTER ADVANCING 2 LINES.
           MOVE SPACES              TO REPORT-LINE.
           WRITE REPORT-LINE AFTER ADVANCING 1 LINE.
           
           MOVE WS-HEADING-2        TO REPORT-LINE.
           WRITE REPORT-LINE AFTER ADVANCING 1 LINE.
           MOVE SPACES              TO REPORT-LINE.
           WRITE REPORT-LINE AFTER ADVANCING 1 LINE.
           
           MOVE WS-DETAIL-1         TO REPORT-LINE.
           WRITE REPORT-LINE AFTER ADVANCING 1 LINE.
           MOVE WS-DETAIL-2         TO REPORT-LINE.
           WRITE REPORT-LINE AFTER ADVANCING 1 LINE.
           MOVE WS-DETAIL-3         TO REPORT-LINE.
           WRITE REPORT-LINE AFTER ADVANCING 1 LINE.
           MOVE SPACES              TO REPORT-LINE.
           WRITE REPORT-LINE AFTER ADVANCING 1 LINE.
           
           MOVE WS-FACTORS-LINE-1   TO REPORT-LINE.
           WRITE REPORT-LINE AFTER ADVANCING 1 LINE.
           MOVE WS-FACTORS-LINE-2   TO REPORT-LINE.
           WRITE REPORT-LINE AFTER ADVANCING 1 LINE.
           MOVE SPACES              TO REPORT-LINE.
           WRITE REPORT-LINE AFTER ADVANCING 1 LINE.
           
           MOVE WS-RESULT-LINE-1    TO REPORT-LINE.
           WRITE REPORT-LINE AFTER ADVANCING 1 LINE.
           MOVE WS-RESULT-LINE-2    TO REPORT-LINE.
           WRITE REPORT-LINE AFTER ADVANCING 1 LINE.
           MOVE SPACES              TO REPORT-LINE.
           WRITE REPORT-LINE AFTER ADVANCING 1 LINE.
           
           MOVE WS-FINAL-LINE-1     TO REPORT-LINE.
           WRITE REPORT-LINE AFTER ADVANCING 1 LINE.
           MOVE WS-FINAL-LINE-2     TO REPORT-LINE.
           WRITE REPORT-LINE AFTER ADVANCING 1 LINE.
           
       900-TERMINATION.
      *----------------------------------------------------------------*
      * PROGRAM TERMINATION:                                           *
      * 1. Close all open files                                       *
      * 2. Stop program execution                                     *
      *----------------------------------------------------------------*
      *  And so ends another journey through the mainframe jungle!    *
      *  Remember, young coder: The program may end, but the wisdom   *
      *  lives on in the core memory of the universe. Now go forth    *
      *  and may your punch cards never jam!                          *
      *----------------------------------------------------------------*
           CLOSE POLICY-FILE
                 CLAIM-FILE
                 INDUSTRY-RISK-FILE
                 GEO-FACTOR-FILE
                 CLAIM-REPORT
                 INPUT-FILE.
           
           STOP RUN.
