       IDENTIFICATION DIVISION.
       PROGRAM-ID. WORKER.
       AUTHOR. RHABI MENDOZA.
       INSTALLATION. PUP MAIN.
       DATE-WRITTEN. JUNE 29, 2024.
       DATE-COMPILED. JUNE 29, 2024.
       SECURITY. ACCESSIBLE TO ALL.
       REMARKS. THIS PROGRAM WILL OUTPUT WORKER'S REPORT.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. ACER.
       OBJECT-COMPUTER. ACER.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INFILE ASSIGN TO 'INFILE.txt'.
           SELECT OUTFILE ASSIGN TO 'OUTFILE.txt'.

       DATA DIVISION.
       FILE SECTION.

       FD INFILE
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 34 CHARACTERS
           DATA RECORD IS REC-IN.

       01 REC-IN.
           05 PROVCD-IN PIC A.
           05 WNO-IN PIC X(7).
           05 WNAME-IN PIC X(25).
           05 STATCD-IN PIC A.

       FD OUTFILE
           LABEL RECORDS ARE OMITTED
           RECORD CONTAINS 80 CHARACTERS
           DATA RECORD IS OUTREC.

       01 OUTREC.
           05 FILLER PIC X(80).

       WORKING-STORAGE SECTION.

       01 REC-OUT.
           05 FILLER PIC X(9) VALUE SPACES.
           05 WNO-OUT PIC X(7).
           05 FILLER PIC X(10) VALUE SPACES.
           05 WNAME-OUT PIC X(25).
           05 FILLER PIC X(10) VALUE SPACES.
           05 STATCD-OUT PIC X(9).
           05 FILLER PIC X(10) VALUE SPACES.

       01 PNAME.
           05 FILLER PIC X(17) VALUE "PROVINCIAL NAME: ".
           05 PROVCD-OUT PIC X(9).

       01 TWORKERS.
           05 FILLER PIC X(22) VALUE "TOTAL NO. OF WORKERS: ".
           05 TWORKERS-OUT PIC Z,ZZ9.

       01 TNWORKERS.
           05 FILLER PIC X(28) VALUE "TOTAL NO. OF CHIKA WORKERS: ".
           05 TNWORKERS-OUT PIC ZZ,ZZ9.

       01 TNPERMANENT.
           05 FILLER PIC X(24) VALUE "TOTAL NO. OF PERMANENT: ".
           05 TNPERMANENT-OUT PIC Z,ZZ9.

       01 TNTEMPORARY.
           05 FILLER PIC X(24) VALUE "TOTAL NO. OF TEMPORARY: ".
           05 TNTEMPORARY-OUT PIC Z,ZZ9.

       01 TEMP-VALUES.
           05 TPC PIC A.
           05 EOFSW PIC X(3) VALUE "NO ".
           05 TWORKERS-IN PIC 999 VALUE 0.
           05 TNWORKERS-IN PIC 999 VALUE 0.
           05 TNPERMANENT-IN PIC 999 VALUE 0.
           05 TNTEMPORARY-IN PIC 999 VALUE 0.

       01 HD1.
           05 FILLER PIC X(29) VALUE SPACES.
           05 FILLER PIC X(22) VALUE "CHIKA ULIT CORPORATION".
           05 FILLER PIC X(29) VALUE SPACES.

       01 HD2.
           05 FILLER PIC X(33) VALUE SPACES.
           05 FILLER PIC X(14) VALUE "Kalokohan City".
           05 FILLER PIC X(33) VALUE SPACES.

       01 HD3.
           05 FILLER PIC X(32) VALUE SPACES.
           05 FILLER PIC X(15) VALUE "WORKER'S REPORT".
           05 FILLER PIC X(33) VALUE SPACES.

       01 COLHD1.
           05 FILLER PIC X(10) VALUE SPACES.
           05 FILLER PIC X(15) VALUE "WORKER'S NUMBER".
           05 FILLER PIC X(10) VALUE SPACES.
           05 FILLER PIC X(13) VALUE "WORKER'S NAME".
           05 FILLER PIC X(10) VALUE SPACES.
           05 FILLER PIC X(11) VALUE "STATUS NAME".
           05 FILLER PIC X(11) VALUE SPACES.
       
       01 NEWLINE.
           05 FILLER PIC X(80) VALUE SPACES.
       
       PROCEDURE DIVISION.

       MAIN-RTN.
           PERFORM INITIAL-RTN.
           PERFORM UNTIL EOFSW = 'YES'
               PERFORM PROCESS-RTN
           END-PERFORM.
           PERFORM FINISH-RTN.
           STOP RUN.

       INITIAL-RTN. 
           OPEN INPUT INFILE.
           OPEN OUTPUT OUTFILE. 

           WRITE OUTREC FROM HD1. 
           WRITE OUTREC FROM HD2. 
           WRITE OUTREC FROM NEWLINE. 
           WRITE OUTREC FROM HD3. 
           WRITE OUTREC FROM NEWLINE. 
           WRITE OUTREC FROM COLHD1. 
           WRITE OUTREC FROM NEWLINE. 
           
           READ INFILE
               AT END 
                   MOVE "YES" TO EOFSW
               NOT AT END 
                   MOVE PROVCD-IN TO TPC
           END-READ.
       INITIAL-END.

       PROCESS-RTN. 
           IF PROVCD-IN NOT = TPC 
               PERFORM AC-BREAK-RTN
               MOVE PROVCD-IN TO TPC
           END-IF.

           MOVE WNO-IN TO WNO-OUT
           MOVE WNAME-IN TO WNAME-OUT

           IF STATCD-IN EQUAL TO "P"
               MOVE "PERMANENT" TO STATCD-OUT
               ADD 1 TO TNPERMANENT-IN
           ELSE IF STATCD-IN EQUAL TO "T"
               MOVE "TEMPORARY" TO STATCD-OUT
               ADD 1 TO TNTEMPORARY-IN
           END-IF.
              
           IF PROVCD-IN EQUAL TO "A" 
               MOVE "ANTIPOLO" TO PROVCD-OUT
           ELSE IF PROVCD-IN EQUAL TO "B" 
               MOVE "BACOLOD " TO PROVCD-OUT
           ELSE IF PROVCD-IN EQUAL TO "C" 
               MOVE "CEBU " TO PROVCD-OUT
           END-IF.
       
           WRITE OUTREC FROM REC-OUT
           
           ADD 1 TO TWORKERS-IN
           ADD 1 TO TNWORKERS-IN 

           READ INFILE
               AT END
                   MOVE 'YES' TO EOFSW
                   PERFORM AC-BREAK-RTN
               NOT AT END
                   CONTINUE
           END-READ.
       PROCESS-END. 

       AC-BREAK-RTN. 
           MOVE TWORKERS-IN TO TWORKERS-OUT
           MOVE TNWORKERS-IN TO TNWORKERS-OUT 
           MOVE TNPERMANENT-IN TO TNPERMANENT-OUT 
           MOVE TNTEMPORARY-IN TO TNTEMPORARY-OUT 

           WRITE OUTREC FROM NEWLINE
           WRITE OUTREC FROM PNAME
           WRITE OUTREC FROM TWORKERS 
           WRITE OUTREC FROM NEWLINE

           MOVE 0 TO TWORKERS-IN.
       AC-BREAK-END.
       
       FINISH-RTN. 
           WRITE OUTREC FROM TNWORKERS
           WRITE OUTREC FROM TNPERMANENT 
           WRITE OUTREC FROM TNTEMPORARY
           CLOSE INFILE OUTFILE.
       FINISH-END.