# Investment Portfolio Management System

![CLBS Logo](./documentation/images/CLBS-logo.png)

## Overview

This repository contains a production-grade implementation of an Investment Portfolio Management System designed for z/OS environments. The system is written in Enterprise COBOL and follows mainframe development best practices and standards.

## Important Implementation Note

This codebase is designed as a reference architecture that demonstrates production-quality mainframe development patterns. While the code is written to production standards and would be ready for z/OS deployment, it is not intended to be executed within this project's scope. The implementation:

- Uses Enterprise COBOL for z/OS
- Implements standard z/OS batch processing patterns
- Includes complete VSAM and DB2 integration
- Contains production-ready JCL
- Provides comprehensive copybook libraries
- Follows mainframe development best practices

## Repository Structure

```
/
├── src/                    # COBOL source code
├── jcl/                    # JCL procedures
├── copy/                   # Copybook libraries
├── sql/                    # DB2 DDL and DML
└── documentation/
    ├── system-architecture.md    # System design and architecture
    ├── data-dictionary.md        # Data structures and definitions
    └── development-backlog.md    # Implementation plan
```

## Technical Standards

- Enterprise COBOL for z/OS compliance
- Standard z/OS file handling patterns
- DB2 for z/OS SQL standards
- z/OS batch processing best practices
- Mainframe security standards
- Standard JCL conventions

## Documentation

Comprehensive documentation is provided:

- System Architecture Document (SAD) detailing the technical design
- Data Dictionary defining all data structures
- Development Backlog outlining the implementation plan
- Deployment and operational procedures
- Code review standards

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
