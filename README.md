# Investment Portfolio Management System

[![Project Status: Active](https://img.shields.io/badge/Project%20Status-Active-green.svg)]()
[![COBOL](https://img.shields.io/badge/COBOL-Enterprise-blue.svg)]()

A production-grade implementation of an Investment Portfolio Management System created for testing LLM translation of real world legacy applications.

## Project Overview

This project demonstrates a complete z/OS COBOL-based portfolio management system, implementing industry-standard patterns and practices. It serves as a reference implementation for:

- Legacy system architecture and design
- COBOL best practices
- Mainframe data handling patterns
- Production-grade error handling and validation

### Key Features

- Portfolio master file management (VSAM KSDS)
- Transaction processing
- Financial calculations and balance management
- Comprehensive error handling and validation
- DB2 database integration

## Development Approach

This project is being developed through LLM pair programming, specifically using Claude 3.5 Sonnet. The development process:

1. Uses standard mainframe development patterns
2. Implements full error handling and validation
3. Includes comprehensive documentation
4. Follows industry best practices for COBOL development

## Project Structure

The repository is organized following standard mainframe project conventions:

```
project-root/
├── documentation/ # Project documentation
│ ├── assets/ # Shared documentation assets
│ ├── operations/ # Operational guides and specifications
│ ├── technical/ # Technical documentation and architecture
│ └── user/ # User documentation and guides
│
└── src/ # Source code root
  ├── copybook/ # COBOL copybook libraries
  │ ├── batch/ # Batch processing copybooks
  │ ├── common/ # Shared system copybooks
  │ └── db2/ # Database-related copybooks
  │
  ├── database/ # Database definitions
  │ ├── db2/ # DB2 table and index definitions
  │ └── vsam/ # VSAM file definitions
  │
  ├── jcl/ # JCL procedures
  │ ├── batch/ # Batch processing jobs
  │ ├── portfolio/ # Portfolio management jobs
  │ └── utility/ # Utility and maintenance jobs
  │
  ├── programs/ # COBOL programs
  │ ├── batch/ # Batch processing programs
  │ ├── common/ # Shared utility programs
  │ └── portfolio/ # Portfolio management programs
  │
  └── templates/ # Code templates and standards
  │ ├── database/ # Database interaction templates
  │ ├── error/ # Error handling templates
  │ └── program/ # Standard program templates
```

### Key Components

- **COBOL Programs**

  - `PORTMSTR` - Portfolio master file maintenance
  - `PORTTRAN` - Transaction processing
  - `PORTBAL` - Balance and reconciliation
  - `PORTRPT` - Reporting engine

- **Copybooks**

  - `PORTFLIO` - Portfolio record structure
  - `TRANFILE` - Transaction file layout
  - `ERRHANDL` - Standard error handling
  - `SQLCA` - SQL communication area

- **JCL**
  - `PORTDALY` - Daily processing
  - `PORTMTH` - Monthly processing
  - `PORTRSTR` - Restart procedures

## Development Status

Currently in active development:

- Sprint 0: ✅ Complete (Technical Setup)
- Sprint 1: 🚧 In Progress (Core Portfolio Management)

## Purpose

This project serves as a comprehensive example of how Large Language Models can assist in understanding, documenting, and potentially translating legacy mainframe applications. It provides a complete, working example of a production-grade COBOL application with all the complexity and patterns typically found in real-world systems.

## Documentation

Comprehensive documentation is provided:

- [System Architecture Document (SAD)](documentation/system-architecture.md) detailing the technical design
- [Data Dictionary](documentation/data-dictionary.md) defining all data structures
- [Development Backlog](documentation/development-backlog.md)
- [Test Data Specifications](documentation/test-data-specs.md)

## Technical Standards

- Enterprise COBOL for z/OS compliance
- Standard z/OS file handling patterns
- DB2 for z/OS SQL standards
- z/OS batch processing best practices
- Mainframe security standards
- Standard JCL conventions

## Quality Standards

All code in this repository:

- Follows z/OS development best practices
- Includes complete error handling
- Implements standard checkpoint/restart patterns
- Contains production-ready JCL
- Includes comprehensive copybooks
- Follows DB2 for z/OS standards

## License

[Appropriate License]

## Contributing

[Contributing Guidelines]
