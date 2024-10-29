# COBOL-Legacy-Benchmark-Suite (CLBS)

## Investment Portfolio Management System Reference Implementation

### Purpose

This project provides a comprehensive COBOL codebase that simulates a real-world investment portfolio management system, specifically designed to serve as a benchmark and testing framework for COBOL modernization and translation solutions. While implementing actual business functionality, its primary purpose is to exercise the full spectrum of traditional mainframe COBOL patterns and complexity.

### Overview

The CLBS-IPMS implementation includes:

- Complex batch processing frameworks
- VSAM file operations
- DB2 database interactions
- CICS online transactions
- Nested copybook hierarchies
- Checkpoint/restart mechanisms
- z/OS batch scheduling integration

### Key Features for Testing Translation Tools

- **Data Complexity**

  - Multiple interrelated file formats
  - Complex data structures with REDEFINES
  - Varying record layouts
  - DB2 table interactions

- **Program Structure**

  - Multi-program dependencies
  - Nested copybook hierarchies
  - Batch control frameworks
  - Online/batch integration

- **Business Logic**

  - Financial calculations
  - Transaction processing
  - Position keeping
  - Cost basis tracking

- **System Integration**
  - Job scheduling dependencies
  - Checkpoint/restart logic
  - Error handling frameworks
  - Status tracking mechanisms

### Repository Structure

```
/documentation
  - data-dictionary.md
  - system-architecture.md
  - program-specifications/
    - trnval00-spec.md
    - posupd00-spec.md
    - histld00-spec.md
    - rptgen00-spec.md
    - inqonln-spec.md

/source
  /cobol
    /batch
      - trnval00.cbl
      - posupd00.cbl
      - histld00.cbl
      - rptgen00.cbl
    /online
      - inqonln.cbl
  /copybook
    - position.cpy
    - transact.cpy
    - history.cpy
    - control.cpy
  /jcl
    - compile.jcl
    - execute.jcl

/test
  /data
    - sample-transactions.dat
    - expected-results/
  /scenarios
    - basic-validation.md
    - error-handling.md
    - restart-recovery.md
```

### Usage as a Benchmark

This implementation is designed to test translation tools against common challenges:

1. **Complex Data Handling**

   - VSAM file operations
   - Multiple record formats
   - Data type conversions

2. **Program Integration**

   - Inter-program communication
   - Shared data structures
   - System dependencies

3. **Business Logic Translation**

   - Calculation accuracy
   - Transaction integrity
   - Error handling fidelity

4. **System Integration**
   - Job control translation
   - Database interactions
   - Online transaction handling

### Getting Started

1. Review the documentation in `/documentation`
2. Examine the system architecture and data dictionary
3. Study the program specifications
4. Use the provided test scenarios and data

### Testing Criteria

The repository includes validation criteria for translation solutions:

- Functional equivalence tests
- Performance benchmarks
- Code structure analysis
- Integration verification

### Contributing

Contributions that enhance the benchmark's coverage or add relevant test scenarios are welcome. Please follow the contribution guidelines in CONTRIBUTING.md.

### License

[Specify chosen license]

### Acknowledgments

- Based on common patterns in financial services mainframe applications
- Designed to represent real-world complexity
- Structured for comprehensive testing of translation solutions

---

**Note**: While this system implements a subset of an investment portfolio management system, its primary purpose is to serve as a reference implementation for testing COBOL modernization tools and approaches.
