# TIMESTAMP: Print a Timestamp
TIMESTAMP is a FORTRAN90 library which prints the current YMDHMS date (year-month-day-hours-minutes-seconds) as a timestamp.

This is useful when documenting the run of a program. By including a timestamp, the output of the program will always contain a clear indication of when it was created. Other indicators, such as the file modification timestamp, may be misleading or subject to unintentional modification.

TIMESTAMP simply gives the current time. If you are trying to do careful timings of the speed of execution of a computer code, this may not be very accurate, especially on a computer that uses timesharing. In that case, there are better solutions that measure just the CPU time associated with your process. See, for instance, the examples in TIMER

## Licensing:
The computer code and data files described and made available on this web page are distributed under the GNU LGPL license.

## Languages:
TIMESTAMP is available in a C version and a C++ version and a FORTRAN77 version and a FORTRAN90 version and a JAVA version and a Mathematica version and a MATLAB version and a PYTHON version.

## Related Data and Programs:
- TIMER, a FORTRAN90 program which shows how to compute the elapsed CPU time inside a program.
- WTIME, a FORTRAN90 library which returns a reading of the wall clock time in seconds.

## Source Code:
- timestamp.f90, the source code.
- timestamp.sh, commands to compile the source code.

## Examples and Tests:
- timestamp_prb.f90, a sample calling program.
- timestamp_prb.sh, commands to compile, link and run the sample calling program.
- timestamp_prb_output.txt, the output file.

## List of Routines:
- HMS_CURRENT_HMS returns the current HMS time as integers.
- HMS_CURRENT_PRINT prints the current HMS time, and a user specified string.
- HMS_CURRENT_STRING writes the current HMS data into a string.
- HMS_DELTA_PRINT prints the change in HMS time, and a user specified string.
- TIMESTAMP prints the current YMDHMS date as a time stamp.
- TIMESTRING writes the current YMDHMS date into a string.

You can go up one level to the FORTRAN90 source codes.

Last revised on 14 December 2007. 
