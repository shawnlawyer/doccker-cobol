IDENTIFICATION DIVISION.
PROGRAM-ID.  CobolContacts.
AUTHOR.  Shawn Lawyer

ENVIRONMENT DIVISION.

INPUT-OUTPUT SECTION.

FILE-CONTROL.
    SELECT ContactsFile ASSIGN TO "CONTACTS.DAT"
        ORGANIZATION IS LINE SEQUENTIAL
             ACCESS MODE IS SEQUENTIAL.

DATA DIVISION.

FILE SECTION.
FD ContactsFile.
01 Contact.
    02 Fullname.
        03 FirstName                    PIC X(24).
        03 LastName                     PIC X(24).

WORKING-STORAGE SECTION.
01  INDICATORS.
    05 WS-EOF                           PIC XXX         VALUE "YES".
    05 WS-ApplicationArea               PIC 9.
    05 WS-Page                          PIC 9(10)       VALUE 1.
    05 WS-ContactsCount                 PIC 9(10)       VALUE 0.
01  RESPONSE.
    05 RESPONSE-IN-WS                   PIC X           VALUE "C".
01  DATA-FOR-SCREEN.
    05 WS-Contact.
        10 WS-FirstName                 PIC X(24).
        10 WS-LastName                  PIC X(24).
    05 WS-Messages.
        10 WS-Dashboard-Messages        PIC a(256).
        10 WS-New-Contact-Messages      PIC a(256).

01 WS-Person.
    02  WS-Name                         PIC X(48).
SCREEN SECTION.
01  CLEAR-SCREEN BLANK SCREEN BACKGROUND-COLOR 1 FOREGROUND-COLOR 7.
01  DASHBOARD-SCREEN-HEADER BACKGROUND-COLOR 1 FOREGROUND-COLOR 7.
    05  VALUE "Cobol Contacts"                          LINE 1 COL 35.
01  DASHBOARD-MENU BACKGROUND-COLOR 1 FOREGROUND-COLOR 7.
    05  VALUE "1) - Create Contact"                     LINE 17 COL 1.
    05  VALUE "2) - View Contacts"                      LINE 18 COL 1.
    05  VALUE "3) - Quit"                               LINE 19 COL 1.
    05  DASHBOARD-MENU-INPUT                            LINE 20 COL 1
                        PIC X               TO WS-ApplicationArea.
01  NEW-CONTACT-SCREEN-HEADER BACKGROUND-COLOR 1 FOREGROUND-COLOR 7.
    05  VALUE "New Contact"                             LINE 1 COL 35.
01  VIEW-CONTACT-SCREEN-HEADER BACKGROUND-COLOR 1 FOREGROUND-COLOR 7.
    05  VALUE "View Contact"                            LINE 1 COL 35.
01  NAME-SECTION BACKGROUND-COLOR 1 FOREGROUND-COLOR 7.
    05  VALUE "First Name"                              LINE 5 COL 1.
    05  FIRST-NAME-INPUT                                LINE 5 COL 12
                        PIC X(24)           FROM WS-FirstName
                        TO FirstName.
    05  VALUE "Last Name"                               LINE 6 COL 1.
    05  LAST-NAME-INPUT                                 LINE 6 COL 12
                        PIC X(24)           FROM WS-LastName
                        TO LastName.
01  PAGE-SECTION BACKGROUND-COLOR 1 FOREGROUND-COLOR 7.
    05  VALUE "Contact #:"                              LINE 14 COL 1.
    05  PAGE-INPUT      BLANK WHEN ZERO                 LINE 14 COL 11
                        PIC 9(10)           FROM WS-Page.
01  TOTAL-CONTACTS-SECTION BACKGROUND-COLOR 1 FOREGROUND-COLOR 7.
    05  VALUE "Total Contacts:"                         LINE 15 COL 1.
    05  PAGES-INPUT     BLANK WHEN ZERO                 LINE 15 COL 17
                        PIC 9(10)           FROM WS-ContactsCount.
01  VIEW-CONTACT-SCREEN-MENU BACKGROUND-COLOR 1 FOREGROUND-COLOR 7.
    05  VALUE "C - TO CONTINUE"                         LINE 16 COL 1.
    05  VALUE "Q - TO QUIT"                             LINE 17 COL 1.
    05  VALUE "ENTER CHOICE:"                           LINE 19 COL 1.
    05  VIEW-CONTACT-MENU-INPUT                         LINE 19 COL 15
                        PIC X               TO RESPONSE-IN-WS.

01  NEW-CONTACT-SCREEN-MENU BACKGROUND-COLOR 1 FOREGROUND-COLOR 7.
    05  VALUE "S - TO SAVE"                             LINE 16 COL 1.
    05  VALUE "Q - TO QUIT"                             LINE 17 COL 1.
    05  VALUE "ENTER CHOICE:"                           LINE 19 COL 1.
    05  NEW-CONTACT-MENU-INPUT                          LINE 19 COL 15
                        PIC X               TO RESPONSE-IN-WS.

PROCEDURE DIVISION.
Begin.
    PERFORM Main.

EndRun.
    STOP RUN.

Main.
    PERFORM UNTIL WS-ApplicationArea = 3
        PERFORM CountContacts
        DISPLAY CLEAR-SCREEN
        PERFORM DisplayDashboardScreen
        DISPLAY CLEAR-SCREEN
        IF WS-ApplicationArea = 1 THEN
            PERFORM DisplayAddContactScreen
        END-IF
        IF WS-ApplicationArea = 2 THEN
            PERFORM DisplayViewContactsScreen
        END-IF
    END-PERFORM
    PERFORM EndRun.

DisplayAddContactScreen.
    MOVE SPACES TO WS-Contact
    DISPLAY NEW-CONTACT-SCREEN-HEADER.
    DISPLAY NAME-SECTION.
    DISPLAY NEW-CONTACT-SCREEN-MENU.
    ACCEPT NAME-SECTION.
    ACCEPT NEW-CONTACT-MENU-INPUT.
    STRING FirstName OF FullName DELIMITED BY SPACE
    SPACE DELIMITED BY SIZE
        LastName OF FullName DELIMITED BY SPACE
        SPACE DELIMITED BY SIZE
        INTO WS-Name
        ON OVERFLOW
        DISPLAY "The FirstName was truncated"
    END-STRING
    EVALUATE RESPONSE-IN-WS
        WHEN "S"
        WHEN "s"
            IF NOT Contact = SPACES
                OPEN EXTEND ContactsFile
                WRITE Contact
                CLOSE ContactsFile
            END-IF
        WHEN "Q"
        WHEN "q"
            CONTINUE
        WHEN OTHER
            PERFORM DisplayAddContactScreen
    END-EVALUATE.

DisplayDashboardScreen.
    DISPLAY DASHBOARD-SCREEN-HEADER
    DISPLAY DASHBOARD-MENU
    ACCEPT DASHBOARD-MENU-INPUT
    IF NOT (WS-ApplicationArea > 0 AND < 4)
        PERFORM DisplayDashboardScreen
    END-IF.

CountContacts.
    MOVE 0 TO WS-ContactsCount
    MOVE "NO" TO WS-EOF
    OPEN INPUT ContactsFile
    PERFORM UNTIL WS-EOF = "YES"
    READ ContactsFile
        AT END MOVE "YES" TO WS-EOF
        NOT AT END ADD 1 TO WS-ContactsCount
    END-PERFORM
    CLOSE ContactsFile.

DisplayViewContactsScreen.
    MOVE 0 TO WS-Page
    MOVE "NO" TO WS-EOF
    OPEN INPUT ContactsFile
    PERFORM DisplayViewContactsScreenLoop
        UNTIL WS-EOF = "YES" OR RESPONSE-IN-WS = "Q" OR "q"
    CLOSE ContactsFile.

DisplayViewContactsScreenLoop.
    READ ContactsFile INTO WS-Contact
        AT END MOVE "YES" TO WS-EOF
        NOT AT END ADD 1 TO WS-Page
    DISPLAY VIEW-CONTACT-SCREEN-HEADER.
    DISPLAY NAME-SECTION.
    DISPLAY PAGE-SECTION.
    DISPLAY TOTAL-CONTACTS-SECTION.
    DISPLAY VIEW-CONTACT-SCREEN-MENU.
    ACCEPT VIEW-CONTACT-MENU-INPUT.
