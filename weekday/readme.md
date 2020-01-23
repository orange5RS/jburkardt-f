# WEEKDAY 
Determine the Day of the Week

WEEKDAY is a FORTRAN90 library which can determine the day of the week corresponding to a given date; for instance, the battle of Hastings, on 14 October 1066 (Julian Calendar!), was a Saturday.


## Licensing:

The computer code and data files described and made available on this web page are distributed under the GNU LGPL license.


## Languages:

WEEKDAY is available in a C version and a C++ version and a FORTRAN77 version and a FORTRAN90 version and a MATLAB version.


## Related Data and Programs:

CALENDAR_NYT, a FORTRAN90 library which shows the correspondence between dates and the New York Times volume and issue number;

CALENDAR_RD, a C++ program which computes the representation of a given date in a number of calendrical systems, by Edward Reingold, Nachum Dershowitz

CALPAK, a FORTRAN90 library which makes various calendar calculations;

DATES, a dataset directory which contains lists of dates in various calendar systems.

DOOMSDAY, a FORTRAN90 library which is given the year, month and day of a date, and uses John Conway's doomsday algorithm to determine the corresponding day of the week.

TEST_VALUES, a FORTRAN90 library which supplies test values of various mathematical functions.


## Reference:

- Lewis Carroll (Charles Dodgson), To Find the Day of the Week for Any Given Date, Nature, 31 March 1887.
- Gary Meisters, Lewis Carroll's Day-of-the-Week Algorithm, Math Horizons, November 2002, pages 24-25.
- Edward Reingold, Nachum Dershowitz, Calendrical Calculations: The Millennium Edition, Cambridge University Press, 2001, ISBN: 0-521-77752-6, LC: CE12.R45.
- Edward Richards, Mapping Time, The Calendar and Its History, Oxford, 1999, ISBN: 0-19-850413-6, LC: CE11.R5.


## Source Code:

- weekday.f90, the source code.
- weekday.sh, commands to compile the source code.


## Examples and Tests:

- weekday_prb.f90, a sample calling program.
- weekday_prb.sh, commands to compile and run the sample program.
- weekday_prb_output.txt, the output file.


## List of Routines:

- DIGIT_TO_CH returns the character representation of a decimal digit.
- I4_MODP returns the positive remainder when I is divided by J.
- I4_TO_S_LEFT converts an integer to a left-justified string.
- I4_TO_S_ZERO converts an integer to a string, with zero padding.
- I4_WRAP forces an integer to lie between given limits by wrapping.
- JED_TO_WEEKDAY computes the day of the week from a JED.
- S_CAT concatenates two strings to make a third string.
- TIMESTAMP prints the current YMDHMS date as a time stamp.
- WEEKDAY_TO_NAME_COMMON returns the name of a Common weekday.
- WEEKDAY_VALUES returns the day of the week for various dates.
- Y_COMMON_TO_ASTRONOMICAL converts a Common year to an Astronomical year.
- YMD_TO_S_COMMON writes a Common YMD date into a string.
- YMD_TO_WEEKDAY_COMMON returns the weekday of a Common YMD date.
- YMDF_COMPARE compares two YMDF dates.
- YMDF_TO_JED_COMMON converts a Common YMDF date to a JED.
- YMDF_TO_JED_GREGORIAN converts a Gregorian YMDF date to a JED.
- YMDF_TO_JED_JULIAN converts a Julian YMDF date to a JED.


You can go up one level to the FORTRAN90 source codes.

Last revised on 25 March 2010.
