# System Architecture Document

Version: 1.1
Last Updated: 2024-10-29

## 1. System Overview

### 1.1 High-Level Architecture

```mermaid
graph TB
    subgraph "Batch Control Layer"
        BC[Batch Control VSAM] --> S[z/OS Scheduler]
        S --> PRF[Process Control File]
        PRF --> |Triggers| BP[Batch Programs]
    end

    subgraph "Batch Processing Layer"
        BP --> TRN[TRNMAIN]
        BP --> POS[POSUPDT]
        BP --> HST[HISTLD00]
        BP --> RPT[RPTGEN00]
    end

    subgraph "Online Layer"
        CIC[CICS] --> INQ[INQONLN]
    end

    subgraph "Data Layer"
        TF[Transaction File] --> TRN
        TRN --> PM[Position Master VSAM]
        POS --> PM
        POS --> TH[Transaction History VSAM]
        HST --> DB[(DB2 Tables)]
        INQ --> PM
        INQ --> TH
        INQ --> DB
    end
```

### 1.2 Component Descriptions

#### 1.2.1 Batch Control Components

- **Batch Control File (BCHCTL)**
  - Controls process execution
  - Maintains process status
  - Supports checkpoint/restart
- **Process Control File (PRCCTL)**
  - Defines process sequence
  - Specifies dependencies
  - Sets completion criteria

#### 1.2.2 Processing Components

- **TRNMAIN (TRNVAL00)**

  - Validates input transactions
  - Performs initial error checking
  - Prepares transactions for processing

- **POSUPDT (POSUPD00)**

  - Updates position records
  - Maintains cost basis
  - Records transaction history

- **HISTLD00**

  - Loads history to DB2
  - Maintains audit trail
  - Supports reporting

- **RPTGEN00**

  - Generates end-of-day reports
  - Produces error summaries
  - Creates audit reports

- **INQONLN**
  - Handles online inquiries
  - Provides position lookups
  - Shows transaction history

## 2. Process Flows

### 2.1 Batch Control Flow

```mermaid
sequenceDiagram
    participant SCH as z/OS Scheduler
    participant BCF as BCHCTL
    participant PCF as PRCCTL
    participant PRG as Programs
    participant CHK as Checkpoints

    SCH->>PCF: Read Process Schedule
    PCF->>BCF: Initialize Control Record
    BCF->>PRG: Start Process
    loop Processing
        PRG->>CHK: Write Checkpoint
        CHK->>BCF: Update Status
    end
    PRG->>BCF: Update Completion
    BCF->>SCH: Signal Next Job
```

### 2.2 Transaction Processing Flow

```mermaid
sequenceDiagram
    participant TF as TRANFILE
    participant TV as TRNVAL00
    participant PU as POSUPD00
    participant HL as HISTLD00
    participant RG as RPTGEN00

    TF->>TV: Read Transactions
    TV->>TV: Validate
    TV->>PU: Valid Transactions
    PU->>HL: Position Updates
    HL->>RG: History Loaded
    RG->>RG: Generate Reports
```

### 2.3 Online Inquiry Flow

```mermaid
sequenceDimap
    participant UI as User
    participant CI as CICS
    participant IN as INQONLN
    participant PM as Position Master
    participant DB as DB2

    UI->>CI: Request
    CI->>IN: Process
    IN->>PM: Read Position
    IN->>DB: Read History
    IN->>CI: Format Response
    CI->>UI: Display
```

## 3. Technical Architecture

### 3.1 File Organization

```
/PROD
├── /CONTROL
│   ├── BCHCTL    (Batch Control VSAM)
│   └── PRCCTL    (Process Control)
├── /DATA
│   ├── TRANFILE  (Input Transactions)
│   ├── POSMSTRE  (Position Master VSAM)
│   └── TRANHIST  (Transaction History VSAM)
├── /LOAD
│   ├── TRNVAL00  (Transaction Validation)
│   ├── POSUPD00  (Position Update)
│   ├── HISTLD00  (History Load)
│   ├── RPTGEN00  (Report Generation)
│   └── INQONLN   (Online Inquiry)
└── /COPY
    ├── TRNREC    (Transaction Record)
    ├── POSREC    (Position Record)
    └── HISTREC   (History Record)
```

### 3.2 Database Organization

```sql
-- Database: POSMVP
TABLESPACE: POSHIST
    Table: POSHIST  (Position History)
    Index: POSHIST_IX1 (ACCOUNT_NO, FUND_ID, TRANS_DATE)

TABLESPACE: ERRLOG
    Table: ERRLOG   (Error Logging)
    Index: ERRLOG_IX1 (ERROR_TIMESTAMP, PROGRAM_ID)
```

## 4. Batch Processing Architecture

### 4.1 Job Flow

```mermaid
graph TD
    A[Start of Day] -->|1800| B[TRNVAL00]
    B -->|RC ≤ 4| C[POSUPD00]
    C -->|RC ≤ 4| D[HISTLD00]
    D -->|RC ≤ 4| E[RPTGEN00]
    E --> F[End of Day]
```

### 4.2 Checkpoint/Restart Framework

```mermaid
graph TD
    A[Program Start] -->|Read| B[BCHCTL]
    B -->|Exists| C{Check Status}
    C -->|New| D[Initialize]
    C -->|Restart| E[Read Checkpoint]
    D --> F[Process]
    E --> F
    F -->|Every N records| G[Write Checkpoint]
    G --> F
    F -->|Complete| H[Update Status]
```

## 5. System Interfaces

### 5.1 Program Dependencies

| Program  | Input            | Output                 | Dependencies |
| -------- | ---------------- | ---------------------- | ------------ |
| TRNVAL00 | TRANFILE, BCHCTL | Validated transactions | None         |
| POSUPD00 | Validated trans  | POSMSTRE, TRANHIST     | TRNVAL00     |
| HISTLD00 | TRANHIST         | POSHIST                | POSUPD00     |
| RPTGEN00 | All files        | Reports                | HISTLD00     |
| INQONLN  | All files        | Screen displays        | None         |

### 5.2 Control Points

- Process initialization validation
- Checkpoint creation frequency
- Restart position identification
- Process completion verification
- Error threshold monitoring

## 6. Recovery Procedures

### 6.1 Batch Recovery

1. Read last checkpoint from BCHCTL
2. Position input files
3. Restore processing state
4. Resume processing
5. Update control records

### 6.2 Error Handling

```mermaid
graph TD
    A[Error Detected] -->|Log| B[ERRLOG]
    B -->|Check| C{Severity}
    C -->|Critical| D[Abort]
    C -->|Warning| E[Continue]
    D --> F[Update BCHCTL]
    E --> G[Process Next]
```

## 7. Performance Considerations

### 7.1 Batch Window Management

- Process timing controls in PRCCTL
- Resource allocation monitoring
- Checkpoint interval optimization
- Parallel processing opportunities

### 7.2 Resource Management

- Buffer allocation strategies
- DB2 commit scopes
- VSAM string settings
- File placement optimization

## 8. Security Architecture

### 8.1 Batch Security

- Program authorization
- File access controls
- DB2 privileges
- Utility access

### 8.2 Online Security

- CICS transaction security
- Resource level security
- User authentication
- Access logging

## 9. Monitoring and Control

### 9.1 Batch Monitoring

- Process status tracking
- Return code checking
- Resource utilization
- Completion verification

### 9.2 Online Monitoring

- Transaction response time
- Resource availability
- Error rate tracking
- User session management
